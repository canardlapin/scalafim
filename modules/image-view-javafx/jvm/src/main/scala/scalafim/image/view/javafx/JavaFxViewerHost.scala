package scalafim.image.view.javafx

import javafx.scene.canvas.GraphicsContext

import scalafim.graphics.javafx.*
import scalafim.image.view.*

enum JavaFxViewerError:
  case View(cause: ImageViewError)
  case Renderer(cause: JavaFxRenderError)

  def message: String =
    this match
      case View(cause) => cause.message
      case Renderer(cause) => cause.message

final case class JavaFxViewerProgram(
  frame: ViewerFrame,
  program: JavaFxProgram
)

object JavaFxViewerHost:
  def compile(
    model: ViewerModel,
    session: ViewerSession
  ): Either[JavaFxViewerError, JavaFxViewerProgram] =
    for
      frame <- session.frame(model).left.map(JavaFxViewerError.View.apply)
      options <- javaFxOptions(frame).left.map(JavaFxViewerError.Renderer.apply)
      program <- JavaFxRenderer.compile(frame.scene, options).left.map(JavaFxViewerError.Renderer.apply)
    yield JavaFxViewerProgram(frame, program)

  def render(
    model: ViewerModel,
    session: ViewerSession,
    context: JavaFxGraphicsContext
  ): Either[JavaFxViewerError, JavaFxViewerProgram] =
    compile(model, session).map { compiled =>
      JavaFxRenderer.draw(compiled.program, context)
      compiled
    }

  def renderCanvas(
    model: ViewerModel,
    session: ViewerSession,
    context: GraphicsContext
  ): Either[JavaFxViewerError, JavaFxViewerProgram] =
    render(model, session, new JavaFxCanvasContext(context))

  def pickAction(
    compiled: JavaFxViewerProgram,
    deviceX: Double,
    deviceY: Double
  ): Either[ImageViewError, ViewerAction] =
    ViewerEvents.pick(compiled.frame, deviceX, deviceY)

  def scrollAction(
    compiled: JavaFxViewerProgram,
    deviceX: Double,
    deviceY: Double,
    steps: Int
  ): Either[ImageViewError, ViewerAction] =
    ViewerEvents.scroll(compiled.frame, deviceX, deviceY, steps)

  private def javaFxOptions(frame: ViewerFrame): Either[JavaFxRenderError, JavaFxOptions] =
    val width = math.round(frame.device.width)
    val height = math.round(frame.device.height)
    if width > Int.MaxValue || height > Int.MaxValue then
      Left(JavaFxRenderError.InvalidCanvasSize(Int.MaxValue, Int.MaxValue))
    else JavaFxOptions(width.toInt, height.toInt)
