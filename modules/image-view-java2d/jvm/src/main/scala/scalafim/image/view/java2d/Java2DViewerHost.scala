package scalafim.image.view.java2d

import java.awt.Graphics2D
import java.awt.image.BufferedImage

import intaglio.java2d.*
import scalafim.image.view.*

enum Java2DViewerError:
  case View(cause: ImageViewError)
  case Renderer(cause: Java2DRenderError)

  def message: String =
    this match
      case View(cause) => cause.message
      case Renderer(cause) => cause.message

final case class Java2DViewerProgram(
  frame: ViewerFrame,
  program: Java2DProgram
)

object Java2DViewerHost:
  def compile(
    model: ViewerModel,
    session: ViewerSession
  ): Either[Java2DViewerError, Java2DViewerProgram] =
    for
      frame <- session.frame(model).left.map(Java2DViewerError.View.apply)
      options <- java2DOptions(frame).left.map(Java2DViewerError.Renderer.apply)
      program <- Java2DRenderer.compile(frame.scene, options).left.map(Java2DViewerError.Renderer.apply)
    yield Java2DViewerProgram(frame, program)

  def render(
    model: ViewerModel,
    session: ViewerSession,
    graphics: Graphics2D
  ): Either[Java2DViewerError, Java2DViewerProgram] =
    compile(model, session).map { compiled =>
      Java2DRenderer.draw(compiled.program, graphics)
      compiled
    }

  def renderImage(
    model: ViewerModel,
    session: ViewerSession
  ): Either[Java2DViewerError, (Java2DViewerProgram, BufferedImage)] =
    compile(model, session).map { compiled =>
      val width = math.round(compiled.frame.device.width).toInt
      val height = math.round(compiled.frame.device.height).toInt
      val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
      val graphics = image.createGraphics()
      try Java2DRenderer.draw(compiled.program, graphics)
      finally graphics.dispose()
      compiled -> image
    }

  def pickAction(
    compiled: Java2DViewerProgram,
    deviceX: Double,
    deviceY: Double
  ): Either[ImageViewError, ViewerAction] =
    ViewerEvents.pick(compiled.frame, deviceX, deviceY)

  def scrollAction(
    compiled: Java2DViewerProgram,
    deviceX: Double,
    deviceY: Double,
    steps: Int
  ): Either[ImageViewError, ViewerAction] =
    ViewerEvents.scroll(compiled.frame, deviceX, deviceY, steps)

  private def java2DOptions(frame: ViewerFrame): Either[Java2DRenderError, Java2DOptions] =
    val width = math.round(frame.device.width)
    val height = math.round(frame.device.height)
    if width > Int.MaxValue || height > Int.MaxValue then
      Left(Java2DRenderError.InvalidImageSize(Int.MaxValue, Int.MaxValue))
    else Java2DOptions(width.toInt, height.toInt)
