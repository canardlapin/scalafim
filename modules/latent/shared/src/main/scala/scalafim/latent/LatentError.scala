package scalafim.latent

enum LatentError:
  case EmptyIdentifier(label: String)
  case NonPositiveDimension(label: String, value: Int)
  case NonPowerOfTwoDimension(label: String, value: Int)
  case DimensionMismatch(label: String, expected: Int, actual: Int)
  case MatrixShapeMismatch(label: String, expectedRows: Int, expectedCols: Int, actualRows: Int, actualCols: Int)
  case NonFiniteValue(label: String, index: Int, value: Double)
  case NegativeIndex(axis: String, index: Int)
  case IndexOutOfBounds(axis: String, index: Int, limit: Int)
  case EmptySelection(axis: String)
  case DuplicateSelection(axis: String, index: Int)
  case DomainMismatch(label: String, expected: DomainId, actual: DomainId)
  case InvalidParameter(label: String, value: Double)
  case MissingComponent(label: String)
  case ProjectionFailed(details: String)

  def message: String =
    this match
      case EmptyIdentifier(label) =>
        s"$label identifier must be non-empty"
      case NonPositiveDimension(label, value) =>
        s"$label dimension must be positive, got $value"
      case NonPowerOfTwoDimension(label, value) =>
        s"$label dimension must be a power of two, got $value"
      case DimensionMismatch(label, expected, actual) =>
        s"$label expected $expected but got $actual"
      case MatrixShapeMismatch(label, expectedRows, expectedCols, actualRows, actualCols) =>
        s"$label expected ${expectedRows}x${expectedCols} but got ${actualRows}x${actualCols}"
      case NonFiniteValue(label, index, value) =>
        s"$label value at linear index $index is not finite: $value"
      case NegativeIndex(axis, index) =>
        s"$axis index must be non-negative, got $index"
      case IndexOutOfBounds(axis, index, limit) =>
        s"$axis index $index out of bounds for size $limit"
      case EmptySelection(axis) =>
        s"$axis selection must be non-empty"
      case DuplicateSelection(axis, index) =>
        s"$axis selection contains duplicate index $index"
      case DomainMismatch(label, expected, actual) =>
        s"$label domain mismatch: expected ${expected.value}, got ${actual.value}"
      case InvalidParameter(label, value) =>
        s"$label must be finite and valid, got $value"
      case MissingComponent(label) =>
        s"$label is required but missing"
      case ProjectionFailed(details) =>
        s"basis projection failed: $details"

  def payload: LatentError.Payload =
    this match
      case EmptyIdentifier(label) =>
        LatentError.Payload.Identifier(label)
      case NonPositiveDimension(label, value) =>
        LatentError.Payload.Dimension(label, value)
      case NonPowerOfTwoDimension(label, value) =>
        LatentError.Payload.Dimension(label, value)
      case DimensionMismatch(label, expected, actual) =>
        LatentError.Payload.DimensionMismatch(label, expected, actual)
      case MatrixShapeMismatch(label, expectedRows, expectedCols, actualRows, actualCols) =>
        LatentError.Payload.MatrixShapeMismatch(label, expectedRows, expectedCols, actualRows, actualCols)
      case NonFiniteValue(label, index, value) =>
        LatentError.Payload.NonFiniteValue(label, index, value)
      case NegativeIndex(axis, index) =>
        LatentError.Payload.Index(axis, index, None)
      case IndexOutOfBounds(axis, index, limit) =>
        LatentError.Payload.Index(axis, index, Some(limit))
      case EmptySelection(axis) =>
        LatentError.Payload.Selection(axis, None)
      case DuplicateSelection(axis, index) =>
        LatentError.Payload.Selection(axis, Some(index))
      case DomainMismatch(label, expected, actual) =>
        LatentError.Payload.Domain(label, expected, actual)
      case InvalidParameter(label, value) =>
        LatentError.Payload.Parameter(label, value)
      case MissingComponent(label) =>
        LatentError.Payload.Component(label)
      case ProjectionFailed(details) =>
        LatentError.Payload.Projection(details)

object LatentError:
  enum Payload:
    case Identifier(label: String)
    case Dimension(label: String, value: Int)
    case DimensionMismatch(label: String, expected: Int, actual: Int)
    case MatrixShapeMismatch(label: String, expectedRows: Int, expectedCols: Int, actualRows: Int, actualCols: Int)
    case NonFiniteValue(label: String, index: Int, value: Double)
    case Index(axis: String, index: Int, limit: Option[Int])
    case Selection(axis: String, duplicateIndex: Option[Int])
    case Domain(label: String, expected: DomainId, actual: DomainId)
    case Parameter(label: String, value: Double)
    case Component(label: String)
    case Projection(details: String)
