package scalafim.graphics.svg

import scalafim.graphics.LengthUnit

enum SvgRenderError:
  case InvalidDocumentSize(width: Int, height: Int)
  case UnsupportedLengthUnit(unit: LengthUnit)
  case UnsupportedLengthExpression(kind: String)

  def message: String =
    this match
      case InvalidDocumentSize(width, height) =>
        s"SVG document size must be positive: ${width}x$height"
      case UnsupportedLengthUnit(unit) =>
        s"SVG renderer does not support length unit '$unit'"
      case UnsupportedLengthExpression(kind) =>
        s"SVG renderer cannot represent length expression '$kind'"

object SvgRenderError:
  extension [A](either: Either[SvgRenderError, A])
    def orThrow: A =
      either match
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error.message)
