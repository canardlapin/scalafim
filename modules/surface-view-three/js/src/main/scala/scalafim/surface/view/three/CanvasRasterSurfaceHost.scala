package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8ClampedArray
import scala.util.control.NonFatal

import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

enum CanvasRasterSurfaceHostError:
  case Disposed, NoFrame, DecorationReloweringRequired, Superseded
  case Render(error: SurfaceRasterError)
  case Decoration(reason: String)
  case Canvas(reason: String)

  def message: String = this match
    case Disposed => "Canvas raster surface host is disposed"
    case Superseded => "Canvas raster request was superseded by a newer request"
    case NoFrame => "Canvas raster surface host has no published frame"
    case DecorationReloweringRequired => "fixed decoration geometry must be lowered again after resize"
    case Render(error) => error.message
    case Decoration(reason) => s"decoration lowering failed: $reason"
    case Canvas(reason) => s"Canvas2D publication failed: $reason"

final case class CanvasRasterSurfaceHostReceipt(
  provider: String,
  generation: Long,
  renderNanos: Long,
  raster: SurfaceRasterReceipt
)

final case class CanvasRasterPresentedFrame(
  scientific: SurfaceRasterResult,
  decorations: Option[SurfaceDecoratedRasterResult],
  plan: SurfaceCompositionPlan,
  dimensions: RasterDimensions,
  generation: Long
)

/** Browser Canvas2D host for the bounded, deterministic raster provider.
  * The caller owns the canvas lifetime; this class never creates or
  * removes a DOM canvas. Publication replaces its bitmap. Its pick buffer is replaced only after the matching
  * pixels have been published successfully.
  */
final class CanvasRasterSurfaceHost private (
  canvas: js.Dynamic,
  context: js.Dynamic,
  fragmentCapacity: Int
):
  import CanvasRasterSurfaceHost.*

  val capabilities: SurfaceBackendCapabilities = SurfaceRasterizer.capabilities.copy(
    id = SurfaceBackendId.unsafe("canvas-reference-raster"))

  private final case class Input(
    plan: SurfaceCompositionPlan,
    dimensions: RasterDimensions,
    decorations: Option[DecorationSource],
    style: SurfaceRasterStyle
  )

  private var latest = Option.empty[CanvasRasterPresentedFrame]
  private var desired = Option.empty[Input]
  private var generation = 0L
  private var request = 0L
  private var closed = false

  def presentedFrame: Option[CanvasRasterPresentedFrame] = latest

  def submit(plan: SurfaceCompositionPlan, dimensions: RasterDimensions,
      style: SurfaceRasterStyle = TransparentStyle): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHostReceipt] =
    render(Input(plan, dimensions, None, style))

  def submitGeometry(plan: SurfaceCompositionPlan, dimensions: RasterDimensions,
      geometry: SurfaceDecorationGeometry,
      style: SurfaceRasterStyle = TransparentStyle): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHostReceipt] =
    render(Input(plan, dimensions, Some(DecorationSource.Fixed(geometry, dimensions)), style))

  /** Retains a pure lowering callback so pixel-sized decorations track resize. */
  def submitDecorated(plan: SurfaceCompositionPlan, dimensions: RasterDimensions,
      lower: DecorationLowering,
      style: SurfaceRasterStyle = TransparentStyle): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHostReceipt] =
    render(Input(plan, dimensions, Some(DecorationSource.Rebuild(lower)), style))

  def resize(dimensions: RasterDimensions): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHostReceipt] =
    if closed then Left(CanvasRasterSurfaceHostError.Disposed)
    else desired match
      case None => Left(CanvasRasterSurfaceHostError.NoFrame)
      case Some(input) => input.decorations match
        case Some(DecorationSource.Fixed(_, original)) if original != dimensions =>
          Left(CanvasRasterSurfaceHostError.DecorationReloweringRequired)
        case _ => render(input.copy(dimensions = dimensions))

  /** Canvas-local CSS coordinates map to the published bitmap through the
    * current bounding rect. A CSS resize does not invalidate raster picks. */
  def pickDisplay(x: Double, y: Double): Either[CanvasRasterSurfaceHostError, Option[SurfaceDisplayHit[SurfacePick]]] =
    if closed then Left(CanvasRasterSurfaceHostError.Disposed)
    else if !x.isFinite || !y.isFinite then Left(CanvasRasterSurfaceHostError.Canvas("pick coordinates must be finite"))
    else latest.toRight(CanvasRasterSurfaceHostError.NoFrame).flatMap: frame =>
      try
        val rect = canvas.applyDynamic("getBoundingClientRect")()
        val (left, top, width, height) = contentBounds(rect)
        val localX = x - left
        val localY = y - top
        if !width.isFinite || !height.isFinite || width <= 0.0 || height <= 0.0 then
          Left(CanvasRasterSurfaceHostError.Canvas("canvas bounds must be finite and positive"))
        else if localX < 0.0 || localY < 0.0 || localX >= width || localY >= height then Right(None)
        else
          pickBitmap(frame, math.floor(localX * frame.dimensions.width / width).toInt,
            math.floor(localY * frame.dimensions.height / height).toInt)
      catch case NonFatal(error) => Left(canvasError(error))

  /** Converts browser client coordinates to Canvas-local coordinates before
    * using the exact same last-published scientific/decoration pick buffers. */
  def pickClient(clientX: Double, clientY: Double): Either[CanvasRasterSurfaceHostError, Option[SurfaceDisplayHit[SurfacePick]]] =
    if !clientX.isFinite || !clientY.isFinite then Left(CanvasRasterSurfaceHostError.Canvas("pick coordinates must be finite"))
    else
      try
        val rect = canvas.applyDynamic("getBoundingClientRect")()
        val left = number(rect.selectDynamic("left"))
        val top = number(rect.selectDynamic("top"))
        if !left.isFinite || !top.isFinite then Left(CanvasRasterSurfaceHostError.Canvas("canvas origin must be finite"))
        else pickDisplay(clientX - left, clientY - top)
      catch case NonFatal(error) => Left(canvasError(error))

  /** Releases only this host's Scala-side plan, image, and pick ownership. */
  def dispose(): Unit =
    if !closed then
      closed = true
      latest = None
      desired = None

  private def render(input: Input): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHostReceipt] =
    if closed then Left(CanvasRasterSurfaceHostError.Disposed)
    else
      request += 1
      val currentRequest = request
      val started = System.nanoTime()
      val result =
        try
          input.decorations match
            case None =>
              SurfaceRasterizer.renderChecked(input.plan, input.dimensions, input.style, fragmentCapacity)
                .left.map(CanvasRasterSurfaceHostError.Render.apply)
                .map(value => value -> Option.empty[SurfaceDecoratedRasterResult])
            case Some(source) =>
              val geometry = source match
                case DecorationSource.Fixed(value, _) => Right(value)
                case DecorationSource.Rebuild(lower) => lower(input.plan, input.dimensions)
              geometry.left.map(CanvasRasterSurfaceHostError.Decoration.apply).flatMap: value =>
                SurfaceDecorationRaster.renderChecked(input.plan, value, input.dimensions, input.style, fragmentCapacity)
                  .left.map(CanvasRasterSurfaceHostError.Render.apply)
                  .map(value => value.scientific -> Some(value))
        catch case NonFatal(error) => Left(canvasError(error))
      result.flatMap: (scientific, decorations) =>
        if closed then Left(CanvasRasterSurfaceHostError.Disposed)
        else if currentRequest != request then Left(CanvasRasterSurfaceHostError.Superseded)
        else publish(input, scientific, decorations, System.nanoTime() - started)

  private def publish(input: Input, scientific: SurfaceRasterResult,
      decorations: Option[SurfaceDecoratedRasterResult], elapsed: Long): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHostReceipt] =
    try
      val image = decorations.fold(scientific.image)(_.image)
      val raw = context.applyDynamic("createImageData")(input.dimensions.width, input.dimensions.height)
      val bytes = raw.selectDynamic("data").asInstanceOf[Uint8ClampedArray]
      var pixel = 0
      while pixel < input.dimensions.pixelCount do
        val color = image.pixelUnsafe(pixel % input.dimensions.width, pixel / input.dimensions.width)
        val offset = pixel * 4
        bytes(offset) = color.red
        bytes(offset + 1) = color.green
        bytes(offset + 2) = color.blue
        bytes(offset + 3) = color.alpha
        pixel += 1
      // Resizing clears the bitmap. From this point any failed publication
      // must expose NoFrame rather than retaining picks for stale pixels.
      latest = None
      desired = None
      canvas.updateDynamic("width")(input.dimensions.width)
      canvas.updateDynamic("height")(input.dimensions.height)
      context.applyDynamic("putImageData")(raw, 0, 0)
      generation += 1
      desired = Some(input)
      latest = Some(CanvasRasterPresentedFrame(scientific, decorations, input.plan, input.dimensions, generation))
      Right(CanvasRasterSurfaceHostReceipt("Canvas2D/reference-raster", generation, elapsed, scientific.receipt))
    catch case NonFatal(error) => Left(canvasError(error))

  private def pickBitmap(frame: CanvasRasterPresentedFrame, x: Int,
      y: Int): Either[CanvasRasterSurfaceHostError, Option[SurfaceDisplayHit[SurfacePick]]] =
    if x < 0 || y < 0 || x >= frame.dimensions.width || y >= frame.dimensions.height then Right(None)
    else frame.decorations.fold(
      frame.scientific.pick(x, y).map(_.map(SurfaceDisplayHit.Scientific.apply))
    )(_.pick(x, y)).left.map(CanvasRasterSurfaceHostError.Render.apply)

  /** Bounding rectangles include borders and padding; the image occupies only
    * the content box. Axis-aligned CSS scaling is supported; rotated/skewed
    * elements require a caller-supplied coordinate transform instead.
    */
  private def contentBounds(rect: js.Dynamic): (Double, Double, Double, Double) =
    val width = number(rect.selectDynamic("width"))
    val height = number(rect.selectDynamic("height"))
    val document = canvas.selectDynamic("ownerDocument")
    if document == null || js.isUndefined(document) then (0.0, 0.0, width, height)
    else
      val style = document.selectDynamic("defaultView").applyDynamic("getComputedStyle")(canvas)
      def px(name: String): Double =
        val value = js.Dynamic.global.parseFloat(style.selectDynamic(name)).asInstanceOf[Double]
        if value.isNaN then 0.0 else value
      val transform = style.selectDynamic("transform").asInstanceOf[String]
      if transform != "none" && transform.nonEmpty then
        val values = transform.stripPrefix("matrix(").stripSuffix(")").split(",").map(_.trim.toDouble)
        require(values.length == 6 && values(1) == 0.0 && values(2) == 0.0 && values(0) > 0.0 && values(3) > 0.0,
          "Canvas picking requires an axis-aligned positive CSS transform")
      val scaleX = width / number(canvas.selectDynamic("offsetWidth"))
      val scaleY = height / number(canvas.selectDynamic("offsetHeight"))
      val left = (px("borderLeftWidth") + px("paddingLeft")) * scaleX
      val right = (px("borderRightWidth") + px("paddingRight")) * scaleX
      val top = (px("borderTopWidth") + px("paddingTop")) * scaleY
      val bottom = (px("borderBottomWidth") + px("paddingBottom")) * scaleY
      (left, top, width - left - right, height - top - bottom)

  private def number(value: js.Dynamic): Double =
    if js.isUndefined(value) || value == null then Double.NaN else value.asInstanceOf[Double]

  private def canvasError(error: Throwable): CanvasRasterSurfaceHostError =
    CanvasRasterSurfaceHostError.Canvas(Option(error.getMessage).getOrElse(error.getClass.getName))

object CanvasRasterSurfaceHost:
  type DecorationLowering = (SurfaceCompositionPlan, RasterDimensions) => Either[String, SurfaceDecorationGeometry]

  val TransparentStyle: SurfaceRasterStyle =
    SurfaceRasterStyle(background = Rgba32.unsafe(0, 0, 0, 0), culling = TriangleCulling.None)

  private enum DecorationSource:
    case Fixed(geometry: SurfaceDecorationGeometry, dimensions: RasterDimensions)
    case Rebuild(lower: DecorationLowering)

  def attach(canvas: js.Dynamic,
      fragmentCapacity: Int = SurfaceRasterizer.DefaultFragmentCapacity): Either[CanvasRasterSurfaceHostError, CanvasRasterSurfaceHost] =
    if fragmentCapacity < 1 then Left(CanvasRasterSurfaceHostError.Render(SurfaceRasterError.FragmentCapacityExceeded(fragmentCapacity)))
    else
      try
        if canvas == null || js.isUndefined(canvas) then Left(CanvasRasterSurfaceHostError.Canvas("host canvas is unavailable"))
        else
          val context = canvas.applyDynamic("getContext")("2d")
          if context == null || js.isUndefined(context) then Left(CanvasRasterSurfaceHostError.Canvas("Canvas2D context is unavailable"))
          else Right(new CanvasRasterSurfaceHost(canvas, context, fragmentCapacity))
      catch case NonFatal(error) => Left(CanvasRasterSurfaceHostError.Canvas(Option(error.getMessage).getOrElse(error.getClass.getName)))
