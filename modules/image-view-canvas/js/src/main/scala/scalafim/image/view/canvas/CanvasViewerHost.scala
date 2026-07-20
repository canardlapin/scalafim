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
  program: CanvasProgram
)

object CanvasViewerHost:
  def compile(
    model: ViewerModel,
    session: ViewerSession
  ): Either[CanvasViewerError, CanvasViewerProgram] =
    for
      frame <- session.frame(model).left.map(CanvasViewerError.View.apply)
      options <- canvasOptions(frame).left.map(CanvasViewerError.Renderer.apply)
      program <- CanvasRenderer.compile(frame.scene, options).left.map(CanvasViewerError.Renderer.apply)
    yield CanvasViewerProgram(frame, program)

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
