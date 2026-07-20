package scalafim.connectivity

enum ConnectivityError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidDimension(kind: String, value: Int)
  case InvalidScalar(kind: String, value: Double, reason: String)
  case EmptyAxis(kind: String)
  case DuplicateId(kind: String, value: String)
  case LabelCountMismatch(kind: String, expected: Int, actual: Int)
  case MatrixShapeMismatch(detail: String)
  case NonFiniteValue(role: String, index: Int, value: Double)
  case FrameWeightLengthMismatch(expected: Int, actual: Int)
  case InvalidFrameWeight(index: Int, value: Double)
  case AxisMismatch(detail: String)
  case EdgeVectorLengthMismatch(expected: Int, actual: Int)
  case NonSymmetricMatrix(row: Int, col: Int, left: Double, right: Double)
  case IncompatibleEdgeSpace(detail: String)
  case InvalidPlan(detail: String)
  case InvalidWorkflow(detail: String)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind '$value': $reason"
      case InvalidDimension(kind, value) =>
        s"$kind must be positive, got $value"
      case InvalidScalar(kind, value, reason) =>
        s"invalid $kind $value: $reason"
      case EmptyAxis(kind) =>
        s"$kind axis must be non-empty"
      case DuplicateId(kind, value) =>
        s"duplicate $kind id '$value'"
      case LabelCountMismatch(kind, expected, actual) =>
        s"$kind label count mismatch: expected $expected, got $actual"
      case MatrixShapeMismatch(detail) =>
        detail
      case NonFiniteValue(role, index, value) =>
        s"$role value at linear index $index is not finite: $value"
      case FrameWeightLengthMismatch(expected, actual) =>
        s"frame weights length mismatch: expected $expected, got $actual"
      case InvalidFrameWeight(index, value) =>
        s"frame weight at index $index must be finite and non-negative, got $value"
      case AxisMismatch(detail) =>
        detail
      case EdgeVectorLengthMismatch(expected, actual) =>
        s"edge vector length mismatch: expected $expected, got $actual"
      case NonSymmetricMatrix(row, col, left, right) =>
        s"matrix is not symmetric at ($row, $col): $left vs $right"
      case IncompatibleEdgeSpace(detail) =>
        detail
      case InvalidPlan(detail) =>
        detail
      case InvalidWorkflow(detail) =>
        detail
