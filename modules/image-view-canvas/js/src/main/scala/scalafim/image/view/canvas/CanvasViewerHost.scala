package scalafim.image.view.canvas

import scalafim.graphics.canvas.*
import scalafim.image.view.*

enum CanvasViewerError:
  case View(cause: ImageViewError)
  case Renderer(cause: CanvasRenderError)

  def message: String =
    this match
      case View(cause) => cause.message
      case Renderer(cause) => cause.message

final case class CanvasViewerProgram(
  frame: ViewerFrame,
  program: CanvasProgram,
  viewerProfile: ViewerProfile = ViewerProfile.Zero
)

final case class CanvasViewerRender(
  compiled: CanvasViewerProgram,
  canvasProfile: CanvasDrawProfile
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

object CanvasViewerHost:
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
