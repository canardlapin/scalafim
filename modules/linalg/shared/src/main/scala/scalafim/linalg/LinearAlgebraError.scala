package scalafim.linalg

enum LinearAlgebraError:
  case NonSquareMatrix(rows: Int, cols: Int)
  case NonPositiveDefinite(pivot: Int, value: Double)
  case DimensionMismatch(label: String, expected: Int, actual: Int)
  case InvalidParameter(label: String, value: Double)
  case NonFiniteValue(label: String, index: Int, value: Double)

  def message: String =
    this match
      case NonSquareMatrix(rows, cols) =>
        s"matrix must be square, got ${rows}x${cols}"
      case NonPositiveDefinite(pivot, value) =>
        s"matrix is not positive definite at pivot $pivot: $value"
      case DimensionMismatch(label, expected, actual) =>
        s"$label expected $expected but got $actual"
      case InvalidParameter(label, value) =>
        s"$label is invalid: $value"
      case NonFiniteValue(label, index, value) =>
        s"$label value at linear index $index is not finite: $value"
