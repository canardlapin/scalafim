package scalafim.graphics

enum GraphicsError:
  case BlankName(kind: String)
  case InvalidInterval(lower: Double, upper: Double)
  case EmptyContinuousRange
  case InvalidTransformDomain(name: String, lower: Double, upper: Double)
  case TransformOutsideDomain(name: String, value: Double)
  case InvalidLength(value: Double)
  case InvalidColorChannel(channel: String, value: Int)
  case InvalidAlpha(value: Double)
  case InvalidLineWidth(value: Double)
  case InvalidBreakCount(value: Int)
  case InvalidBreakWidth(value: Double)
  case EmptyPalette
  case DuplicateLevel(level: String)
  case EmptyGeometry(kind: String)
  case MissingAesthetic(geom: String, aesthetic: String)
  case DuplicateScale(aesthetic: String)
  case InvalidAxisCoordinate(kind: String, value: Double)
  case AxisTickOutsideRange(value: Double, lower: Double, upper: Double)
  case AxisLabelCountMismatch(values: Int, labels: Int)

  def message: String =
    this match
      case BlankName(kind) =>
        s"$kind name must not be blank"
      case InvalidInterval(lower, upper) =>
        s"invalid interval [$lower, $upper]"
      case EmptyContinuousRange =>
        "continuous range has no finite values"
      case InvalidTransformDomain(name, lower, upper) =>
        s"transform '$name' has invalid domain [$lower, $upper]"
      case TransformOutsideDomain(name, value) =>
        s"value $value is outside transform '$name' domain"
      case InvalidLength(value) =>
        s"length value must be finite: $value"
      case InvalidColorChannel(channel, value) =>
        s"color channel '$channel' must be in [0, 255]: $value"
      case InvalidAlpha(value) =>
        s"alpha must be finite and in [0, 1]: $value"
      case InvalidLineWidth(value) =>
        s"line width must be finite and >= 0: $value"
      case InvalidBreakCount(value) =>
        s"break count must be >= 1: $value"
      case InvalidBreakWidth(value) =>
        s"break width must be finite and > 0: $value"
      case EmptyPalette =>
        "palette must contain at least one value"
      case DuplicateLevel(level) =>
        s"duplicate discrete level '$level'"
      case EmptyGeometry(kind) =>
        s"$kind geometry requires at least one element"
      case MissingAesthetic(geom, aesthetic) =>
        s"geom '$geom' requires aesthetic '$aesthetic'"
      case DuplicateScale(aesthetic) =>
        s"duplicate scale for aesthetic '$aesthetic'"
      case InvalidAxisCoordinate(kind, value) =>
        s"axis $kind must be finite and non-negative where applicable: $value"
      case AxisTickOutsideRange(value, lower, upper) =>
        s"axis tick $value is outside range [$lower, $upper]"
      case AxisLabelCountMismatch(values, labels) =>
        s"axis labeler returned $labels labels for $values tick values"

object GraphicsError:
  extension [A](either: Either[GraphicsError, A])
    def orThrow: A =
      either match
        case Right(value) => value
        case Left(error)  => throw new IllegalArgumentException(error.message)
