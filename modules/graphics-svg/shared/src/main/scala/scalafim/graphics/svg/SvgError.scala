package scalafim.graphics.svg

import scalafim.graphics.GraphicsError

enum SvgRenderError:
  case InvalidDocumentSize(width: Int, height: Int)
  case Graphics(error: GraphicsError)

  def message: String =
    this match
      case InvalidDocumentSize(width, height) =>
        s"SVG document size must be positive: ${width}x$height"
      case Graphics(error) =>
        error.message

object SvgRenderError:
  extension [A](either: Either[SvgRenderError, A])
    def orThrow: A =
      either match
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error.message)
