package scalafim.latent

enum LatentError:
  case EmptyIdentifier(label: String)
  case NonPositiveDimension(label: String, value: Int)
  case DimensionMismatch(label: String, expected: Int, actual: Int)
  case MatrixShapeMismatch(label: String, expectedRows: Int, expectedCols: Int, actualRows: Int, actualCols: Int)
  case NonFiniteValue(label: String, index: Int, value: Double)
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
      case DimensionMismatch(label, expected, actual) =>
        s"$label expected $expected but got $actual"
      case MatrixShapeMismatch(label, expectedRows, expectedCols, actualRows, actualCols) =>
        s"$label expected ${expectedRows}x${expectedCols} but got ${actualRows}x${actualCols}"
      case NonFiniteValue(label, index, value) =>
        s"$label value at linear index $index is not finite: $value"
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
