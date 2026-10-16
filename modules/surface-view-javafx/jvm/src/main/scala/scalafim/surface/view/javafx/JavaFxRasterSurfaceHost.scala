package scalafim.surface.view.javafx

import java.util.concurrent.{ExecutorService, Executors}
import javafx.application.Platform
import javafx.scene.Group
import javafx.scene.image.{ImageView, WritableImage}
import scala.concurrent.{Future, Promise}
import scala.util.control.NonFatal
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

enum JavaFxRasterHostError:
  case Superseded, Disposed, WrongThread, NoFrame, DecorationReloweringRequired
  case Render(error: SurfaceRasterError)
  case Decoration(reason: String)
  case Fx(reason: String)

final case class JavaFxRasterHostReceipt(provider: String, generation: Long, renderNanos: Long, raster: SurfaceRasterReceipt)
final case class JavaFxRasterPresentedFrame(
  image: WritableImage,
  scientific: SurfaceRasterResult,
  decorations: Option[SurfaceDecoratedRasterResult],
  plan: SurfaceCompositionPlan,
  dimensions: RasterDimensions,
  generation: Long
)

/** Production ImageView host for the checked, portable depth compositor.
  * Submission and disposal are thread-safe; node access and picking require FX.
  * Each submitted future completes, including supersession and disposal. Only
  * the most recently requested frame may replace the coherent displayed image
  * and pick buffers. Caller-owned executors are never shut down by this host.
  */
final class JavaFxRasterSurfaceHost private (executor: ExecutorService, ownsExecutor: Boolean):
  import JavaFxRasterSurfaceHost.*

  val capabilities: SurfaceBackendCapabilities = SurfaceRasterizer.capabilities.copy(
    id = SurfaceBackendId.unsafe("javafx-reference-raster"))

  private final case class Input(plan: SurfaceCompositionPlan, size: RasterDimensions,
    decorations: Option[DecorationSource], style: SurfaceRasterStyle)
  private final case class Request(generation: Long, input: Input,
    promise: Promise[Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]])

  val root = new Group()
  val imageView = new ImageView()
  root.getChildren.add(imageView)
  private var frame = Option.empty[JavaFxRasterPresentedFrame]
  private var generation = 0L
  private var active = Option.empty[Request]
  private var pending = Option.empty[Request]
  private var desired = Option.empty[Input]
  private var closed = false

  def presentedFrame: Option[JavaFxRasterPresentedFrame] = synchronized(frame)

  def submit(plan: SurfaceCompositionPlan, dimensions: RasterDimensions,
      decorations: Option[SurfaceDecorationGeometry] = None,
      style: SurfaceRasterStyle = SurfaceRasterStyle(culling = TriangleCulling.None)): Future[Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]] =
    enqueue(Input(plan, dimensions, decorations.map(DecorationSource.Fixed(_, dimensions)), style))

  /** Retains a lowering function so pixel-sized glyphs can be rebuilt on resize.
    * Lowering executes on the render worker and must not access JavaFX nodes.
    */
  def submitDecorated(plan: SurfaceCompositionPlan, dimensions: RasterDimensions,
      lower: DecorationLowering, style: SurfaceRasterStyle = SurfaceRasterStyle(culling = TriangleCulling.None))
      : Future[Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]] =
    enqueue(Input(plan, dimensions, Some(DecorationSource.Rebuild(lower)), style))

  def resize(size: RasterDimensions): Future[Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]] = synchronized:
    if closed then Future.successful(Left(JavaFxRasterHostError.Disposed))
    else desired match
      case None => Future.successful(Left(JavaFxRasterHostError.NoFrame))
      case Some(input) => input.decorations match
        case Some(DecorationSource.Fixed(_, original)) if original != size =>
          Future.successful(Left(JavaFxRasterHostError.DecorationReloweringRequired))
        case _ => enqueue(input.copy(size = size))

  /** Coordinates are ImageView-local, including its x/y origin, fit scaling,
    * preserved aspect and optional viewport crop. The immutable displayed frame
    * supplies picks; a pending newer request cannot change their geometry.
    */
  def pickDisplay(x: Double, y: Double): Either[JavaFxRasterHostError, Option[SurfaceDisplayHit[SurfacePick]]] = synchronized:
    if closed then Left(JavaFxRasterHostError.Disposed)
    else if !Platform.isFxApplicationThread then Left(JavaFxRasterHostError.WrongThread)
    else if !x.isFinite || !y.isFinite then Left(JavaFxRasterHostError.Fx("pick coordinates must be finite"))
    else frame.toRight(JavaFxRasterHostError.NoFrame).flatMap: current =>
      val bounds = imageView.getBoundsInLocal
      if bounds.getWidth <= 0 || bounds.getHeight <= 0 || x < bounds.getMinX || y < bounds.getMinY ||
          x >= bounds.getMaxX || y >= bounds.getMaxY then Right(None)
      else
        val viewport = Option(imageView.getViewport)
        val left = viewport.fold(0.0)(_.getMinX)
        val top = viewport.fold(0.0)(_.getMinY)
        val width = viewport.fold(current.dimensions.width.toDouble)(_.getWidth)
        val height = viewport.fold(current.dimensions.height.toDouble)(_.getHeight)
        val px = math.floor(left + (x - bounds.getMinX) * width / bounds.getWidth).toInt
        val py = math.floor(top + (y - bounds.getMinY) * height / bounds.getHeight).toInt
        if px < 0 || py < 0 || px >= current.dimensions.width || py >= current.dimensions.height then Right(None)
        else current.decorations.fold(current.scientific.pick(px, py).map(_.map(SurfaceDisplayHit.Scientific.apply)))(_.pick(px, py))
          .left.map(JavaFxRasterHostError.Render.apply)

  def dispose(): Unit = synchronized:
    if !closed then
      closed = true
      active.foreach(_.promise.trySuccess(Left(JavaFxRasterHostError.Disposed)))
      pending.foreach(_.promise.trySuccess(Left(JavaFxRasterHostError.Disposed)))
      pending = None
      desired = None
      frame = None
      if ownsExecutor then executor.shutdown()
      def clear(): Unit =
        imageView.setImage(null)
        root.getChildren.clear()
      if Platform.isFxApplicationThread then clear()
      else
        try Platform.runLater(() => clear())
        catch case NonFatal(_) => () // The disposed host cannot republish even after toolkit shutdown.

  private def enqueue(input: Input): Future[Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]] = synchronized:
    val promise = Promise[Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]]()
    if closed then promise.success(Left(JavaFxRasterHostError.Disposed))
    else
      generation += 1
      active.foreach(_.promise.trySuccess(Left(JavaFxRasterHostError.Superseded)))
      pending.foreach(_.promise.trySuccess(Left(JavaFxRasterHostError.Superseded)))
      desired = Some(input)
      pending = Some(Request(generation, input, promise))
      startNext()
    promise.future

  private def startNext(): Unit =
    if !closed && active.isEmpty then pending.foreach: request =>
      pending = None
      active = Some(request)
      try executor.execute(() => render(request))
      catch case NonFatal(error) => finish(request, Left(fxError(error)))

  private def render(request: Request): Unit =
    val input = request.input
    val result: Either[JavaFxRasterHostError, (SurfaceRasterResult, Option[SurfaceDecoratedRasterResult])] =
      try
        input.decorations match
          case None => SurfaceRasterizer.renderChecked(input.plan, input.size, input.style,
            SurfaceRasterizer.DefaultFragmentCapacity).left.map(JavaFxRasterHostError.Render.apply).map(_ -> None)
          case Some(source) =>
            val geometry = source match
              case DecorationSource.Fixed(value, _) => Right(value)
              case DecorationSource.Rebuild(lower) => lower(input.plan, input.size)
            geometry.left.map(JavaFxRasterHostError.Decoration.apply).flatMap: value =>
              SurfaceDecorationRaster.render(input.plan, value, input.size, input.style)
                .left.map(JavaFxRasterHostError.Render.apply).map(value => value.scientific -> Some(value))
      catch case NonFatal(error) => Left(fxError(error))
    result match
      case Left(error) => finish(request, Left(error))
      case Right((scientific, decorated)) =>
        try Platform.runLater(() => publish(request, scientific, decorated))
        catch case NonFatal(error) => finish(request, Left(fxError(error)))

  private def publish(request: Request, scientific: SurfaceRasterResult, decorated: Option[SurfaceDecoratedRasterResult]): Unit = synchronized:
    try
      if closed then request.promise.trySuccess(Left(JavaFxRasterHostError.Disposed))
      else if request.generation != generation then request.promise.trySuccess(Left(JavaFxRasterHostError.Superseded))
      else
        val size = request.input.size
        val image = new WritableImage(size.width, size.height)
        val source = decorated.fold(scientific.image)(_.image)
        val writer = image.getPixelWriter
        var y = 0
        while y < size.height do
          var x = 0
          while x < size.width do
            val color = source.pixelUnsafe(x, y)
            writer.setArgb(x, y, (color.alpha << 24) | (color.red << 16) | (color.green << 8) | color.blue)
            x += 1
          y += 1
        // setImage invokes application listeners synchronously. Publish the
        // matching pick frame first, and never overwrite reentrant disposal or
        // submission after that callback has run.
        frame = Some(JavaFxRasterPresentedFrame(image, scientific, decorated, request.input.plan, size, request.generation))
        imageView.setImage(image)
        request.promise.trySuccess(Right(JavaFxRasterHostReceipt("JavaFX-hosted raster", request.generation,
          scientific.receipt.renderNanos, scientific.receipt)))
    catch case NonFatal(error) => request.promise.trySuccess(Left(fxError(error)))
    finally release(request)

  private def finish(request: Request, result: Either[JavaFxRasterHostError, JavaFxRasterHostReceipt]): Unit = synchronized:
    request.promise.trySuccess(result)
    release(request)

  private def release(request: Request): Unit =
    if active.contains(request) then active = None
    startNext()

  private def fxError(error: Throwable): JavaFxRasterHostError =
    JavaFxRasterHostError.Fx(Option(error.getMessage).getOrElse(error.getClass.getName))

object JavaFxRasterSurfaceHost:
  type DecorationLowering = (SurfaceCompositionPlan, RasterDimensions) => Either[String, SurfaceDecorationGeometry]
  private enum DecorationSource:
    case Fixed(geometry: SurfaceDecorationGeometry, dimensions: RasterDimensions)
    case Rebuild(lower: DecorationLowering)

  def create(): JavaFxRasterSurfaceHost =
    requireFx()
    new JavaFxRasterSurfaceHost(Executors.newSingleThreadExecutor(), true)

  def create(executor: ExecutorService): JavaFxRasterSurfaceHost =
    requireFx()
    new JavaFxRasterSurfaceHost(executor, false)

  private def requireFx(): Unit =
    require(Platform.isFxApplicationThread, "JavaFxRasterSurfaceHost.create requires the Application Thread")
