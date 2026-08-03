package scalafim.image.view.canvas

import scala.scalajs.js
import intaglio.canvas.*
import scalafim.image.AnatomicalPlane
import scalafim.image.view.*

enum CanvasViewerError:
  case View(cause: ImageViewError)
  case Renderer(cause: CanvasRenderError)
  case ControllerClosed
  case InvalidPrefetchOffsets(offsets: Vector[Int])

  def message: String =
    this match
      case View(cause) => cause.message
      case Renderer(cause) => cause.message
      case ControllerClosed => "canvas viewer controller is closed"
      case InvalidPrefetchOffsets(offsets) =>
        s"adjacent-slice prefetch offsets must be a non-empty subset of [-1, 1]; got ${offsets.mkString(", ")}"

final case class CanvasViewerProgram(
  frame: ViewerFrame,
  program: CanvasProgram,
  viewerProfile: ViewerProfile = ViewerProfile.Zero
)

final case class CanvasViewerRender(
  compiled: CanvasViewerProgram,
  canvasProfile: CanvasDrawProfile
)

final case class CanvasViewerSnapshot(session: ViewerSession)

final case class CanvasPrefetchProfile(
  requestedSlices: Int,
  viewerProfile: ViewerProfile
)

trait CanvasTaskScheduler:
  def schedule(task: () => Unit): Unit

object CanvasTaskScheduler:
  val AnimationFrame: CanvasTaskScheduler =
    new CanvasTaskScheduler:
      def schedule(task: () => Unit): Unit =
        js.Dynamic.global.window.requestAnimationFrame(
          ((_: Double) => task()): js.Function1[Double, Unit]
        )

final case class CanvasScrollBatch(
  submittedEvents: Int,
  executedActions: Int,
  netSteps: Map[AnatomicalPlane, Int]
)

/** Stateful browser resource owner. The viewer model and reducer remain pure;
  * only sampled/raster cache state and browser-native Canvas resources live
  * here across animation frames.
  */
final class CanvasViewerRuntime private[canvas] (
  private var viewerCache: ViewerCache,
  val rasterCache: CanvasRasterCache
):
  def compile(
    model: ViewerModel,
    session: ViewerSession
  ): Either[CanvasViewerError, CanvasViewerProgram] =
    CanvasViewerHost.compileCached(model, session, viewerCache).map { case (compiled, nextCache) =>
      viewerCache = nextCache
      compiled
    }

  def render(
    model: ViewerModel,
    session: ViewerSession,
    context: CanvasRenderingContext2D
  )(using CanvasRasterFactory): Either[CanvasViewerError, CanvasViewerRender] =
    compile(model, session).map { compiled =>
      val canvasProfile = CanvasRenderer.drawCached(compiled.program, context, rasterCache)
      CanvasViewerRender(compiled, canvasProfile)
    }

  def sampledSliceCount: Int =
    viewerCache.sampledSliceCount

  def rasterCount: Int =
    viewerCache.size

  /** Populate sampled/raster caches for at most the two immediately adjacent
    * slices. The caller chooses an idle scheduler; this method never changes
    * the visible viewer session or uploads browser-native rasters.
    */
  def prefetchSlices(
    model: ViewerModel,
    session: ViewerSession,
    plane: AnatomicalPlane,
    offsets: Vector[Int] = Vector(-1, 1)
  ): Either[CanvasViewerError, CanvasPrefetchProfile] =
    val valid = offsets.nonEmpty && offsets.distinct == offsets && offsets.forall(step => step == -1 || step == 1)
    if !valid then Left(CanvasViewerError.InvalidPrefetchOffsets(offsets))
    else
      var currentCache = viewerCache
      var aggregate = ViewerProfile.Zero
      var error = Option.empty[CanvasViewerError]
      var index = 0
      while index < offsets.length && error.isEmpty do
        val prefetched =
          ViewerReducer.reduce(model, session, ViewerAction.Scroll(plane, offsets(index)))
            .left.map(CanvasViewerError.View.apply)
            .flatMap { next =>
              next.compileCached(model, currentCache).left.map(CanvasViewerError.View.apply)
            }
        prefetched match
          case Left(value) => error = Some(value)
          case Right(compilation) =>
            currentCache = compilation.cache
            aggregate = aggregate + compilation.profile
        index += 1
      error match
        case Some(value) => Left(value)
        case None =>
          viewerCache = currentCache
          Right(CanvasPrefetchProfile(offsets.length, aggregate))

/** Thin application-facing owner for one model, session, and Canvas runtime.
  * It retains no DOM node or rendering context: applications own listeners and
  * call these typed bindings from their preferred UI framework.
  */
final class CanvasViewerController private[canvas] (
  val model: ViewerModel,
  initialSession: ViewerSession,
  val runtime: CanvasViewerRuntime
):
  private var currentSession = initialSession
  private var active = true

  def isClosed: Boolean =
    !active

  def session: Either[CanvasViewerError, ViewerSession] =
    if active then Right(currentSession) else Left(CanvasViewerError.ControllerClosed)

  def snapshot(): Either[CanvasViewerError, CanvasViewerSnapshot] =
    session.map(CanvasViewerSnapshot.apply)

  def restore(snapshot: CanvasViewerSnapshot): Either[CanvasViewerError, ViewerSession] =
    ensureActive.flatMap { _ =>
      snapshot.session.frame(model).left.map(CanvasViewerError.View.apply).map { _ =>
        currentSession = snapshot.session
        currentSession
      }
    }

  def dispatch(action: ViewerAction): Either[CanvasViewerError, ViewerSession] =
    ensureActive.flatMap { _ =>
      ViewerReducer.reduce(model, currentSession, action).left.map(CanvasViewerError.View.apply).map { next =>
        currentSession = next
        next
      }
    }

  def compile(): Either[CanvasViewerError, CanvasViewerProgram] =
    ensureActive.flatMap(_ => runtime.compile(model, currentSession))

  def render(
    context: CanvasRenderingContext2D
  )(using CanvasRasterFactory): Either[CanvasViewerError, CanvasViewerRender] =
    ensureActive.flatMap(_ => runtime.render(model, currentSession, context))

  def pick(canvasX: Double, canvasY: Double): Either[CanvasViewerError, ViewerSession] =
    compile().flatMap { compiled =>
      CanvasViewerHost.pickAction(compiled, canvasX, canvasY)
        .left.map(CanvasViewerError.View.apply)
        .flatMap(dispatch)
    }

  def scroll(
    canvasX: Double,
    canvasY: Double,
    steps: Int
  ): Either[CanvasViewerError, ViewerSession] =
    compile().flatMap { compiled =>
      CanvasViewerHost.scrollAction(compiled, canvasX, canvasY, steps)
        .left.map(CanvasViewerError.View.apply)
        .flatMap(dispatch)
    }

  def prefetchSlices(
    plane: AnatomicalPlane,
    offsets: Vector[Int] = Vector(-1, 1)
  ): Either[CanvasViewerError, CanvasPrefetchProfile] =
    ensureActive.flatMap(_ => runtime.prefetchSlices(model, currentSession, plane, offsets))

  def scrollCoordinator(
    scheduler: CanvasTaskScheduler
  )(
    onFlush: (Either[CanvasViewerError, ViewerSession], CanvasScrollBatch) => Unit
  ): Either[CanvasViewerError, CanvasScrollCoordinator] =
    ensureActive.map(_ => new CanvasScrollCoordinator(this, scheduler, onFlush))

  def close(): Unit =
    active = false

  private def ensureActive: Either[CanvasViewerError, Unit] =
    if active then Right(()) else Left(CanvasViewerError.ControllerClosed)

/** Coalesces wheel bursts until the application's next scheduled frame.
  * Anatomical scroll vectors commute, so summing steps per plane preserves the
  * reducer result while avoiding obsolete intermediate compilations.
  */
final class CanvasScrollCoordinator private[canvas] (
  controller: CanvasViewerController,
  scheduler: CanvasTaskScheduler,
  onFlush: (Either[CanvasViewerError, ViewerSession], CanvasScrollBatch) => Unit
):
  private var pending = Map.empty[AnatomicalPlane, Int]
  private var eventCount = 0
  private var scheduled = false

  def enqueue(plane: AnatomicalPlane, steps: Int): Unit =
    if steps != 0 then
      pending = pending.updated(plane, pending.getOrElse(plane, 0) + steps)
      eventCount += 1
      if !scheduled then
        scheduled = true
        scheduler.schedule(() => flush())

  private def flush(): Unit =
    val net = pending.filter((_, steps) => steps != 0)
    val submitted = eventCount
    pending = Map.empty
    eventCount = 0
    scheduled = false
    val actions = Vector(
      AnatomicalPlane.Sagittal,
      AnatomicalPlane.Coronal,
      AnatomicalPlane.Axial
    ).flatMap { plane =>
      net.get(plane).map(steps => ViewerAction.Scroll(plane, steps))
    }
    var result = controller.session
    var index = 0
    while index < actions.length && result.isRight do
      result = result.flatMap(_ => controller.dispatch(actions(index)))
      index += 1
    onFlush(result, CanvasScrollBatch(submitted, actions.length, net))

object CanvasViewerHost:
  def controller(
    model: ViewerModel,
    session: ViewerSession,
    viewerCacheCapacity: Int = 48,
    rasterCacheCapacity: Int = 24
  ): Either[CanvasViewerError, CanvasViewerController] =
    for
      _ <- session.frame(model).left.map(CanvasViewerError.View.apply)
      runtime <- runtime(viewerCacheCapacity, rasterCacheCapacity)
    yield new CanvasViewerController(model, session, runtime)

  def runtime(
    viewerCacheCapacity: Int = 48,
    rasterCacheCapacity: Int = 24
  ): Either[CanvasViewerError, CanvasViewerRuntime] =
    for
      viewerCache <- ViewerCache.make(viewerCacheCapacity).left.map(CanvasViewerError.View.apply)
      rasterCache <- CanvasRasterCache.make(rasterCacheCapacity).left.map(CanvasViewerError.Renderer.apply)
    yield new CanvasViewerRuntime(viewerCache, rasterCache)

  def compile(
    model: ViewerModel,
    session: ViewerSession
  ): Either[CanvasViewerError, CanvasViewerProgram] =
    compileCached(model, session, ViewerCache.Disabled).map(_._1)

  private[canvas] def compileCached(
    model: ViewerModel,
    session: ViewerSession,
    cache: ViewerCache
  ): Either[CanvasViewerError, (CanvasViewerProgram, ViewerCache)] =
    for
      compilation <- session.compileCached(model, cache).left.map(CanvasViewerError.View.apply)
      options <- canvasOptions(compilation.frame).left.map(CanvasViewerError.Renderer.apply)
      program <- CanvasRenderer.compile(compilation.frame.scene, options).left.map(CanvasViewerError.Renderer.apply)
    yield CanvasViewerProgram(compilation.frame, program, compilation.profile) -> compilation.cache

  def render(
    model: ViewerModel,
    session: ViewerSession,
    context: CanvasRenderingContext2D
  )(using CanvasRasterFactory): Either[CanvasViewerError, CanvasViewerProgram] =
    compile(model, session).map { compiled =>
      CanvasRenderer.draw(compiled.program, context)
      compiled
    }

  def pickAction(
    compiled: CanvasViewerProgram,
    canvasX: Double,
    canvasY: Double
  ): Either[ImageViewError, ViewerAction] =
    ViewerEvents.pick(compiled.frame, canvasX, canvasY)

  def scrollAction(
    compiled: CanvasViewerProgram,
    canvasX: Double,
    canvasY: Double,
    steps: Int
  ): Either[ImageViewError, ViewerAction] =
    ViewerEvents.scroll(compiled.frame, canvasX, canvasY, steps)

  private def canvasOptions(frame: ViewerFrame): Either[CanvasRenderError, CanvasOptions] =
    val width = math.round(frame.device.width)
    val height = math.round(frame.device.height)
    if width > Int.MaxValue || height > Int.MaxValue then
      Left(CanvasRenderError.InvalidCanvasSize(Int.MaxValue, Int.MaxValue))
    else CanvasOptions(width.toInt, height.toInt)
