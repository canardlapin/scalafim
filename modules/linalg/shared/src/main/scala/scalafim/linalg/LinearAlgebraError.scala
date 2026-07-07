package scalafim.linalg

enum MatrixAxis:
  case Row
  case Col

  def label: String =
    this match
      case Row => "row"
      case Col => "column"

enum MatrixValueRole:
  case Gram
  case RightHandSide
  case Data
  case Basis

  def label: String =
    this match
      case Gram          => "gram"
      case RightHandSide => "right-hand side"
      case Data          => "data"
      case Basis         => "basis"

enum DimensionRole:
  case RightHandSideRows
  case DataRows

  def label: String =
    this match
      case RightHandSideRows => "right-hand side rows"
      case DataRows          => "data rows"

enum NumericParameter:
  case Ridge
  case Tolerance

  def label: String =
    this match
      case Ridge     => "ridge"
      case Tolerance => "tolerance"

enum LinearAlgebraError:
  case InvalidMatrixShape(rows: Int, cols: Int)
  case MatrixTooLarge(rows: Int, cols: Int)
  case MatrixStorageLengthMismatch(shape: MatrixShape, actual: Int)
  case NonSquareMatrix(rows: Int, cols: Int)
  case NonPositiveDefinite(pivot: Int, value: Double)
  case DimensionMismatch(role: DimensionRole, expected: Int, actual: Int)
  case IndexOutOfBounds(axis: MatrixAxis, index: Int, limit: Int)
  case InvalidParameter(parameter: NumericParameter, value: Double)
  case NonFiniteValue(role: MatrixValueRole, index: Int, value: Double)

  def message: String =
    this match
      case InvalidMatrixShape(rows, cols) =>
        s"matrix dimensions must be non-negative, got ${rows}x${cols}"
      case MatrixTooLarge(rows, cols) =>
        s"matrix dimensions are too large for row-major storage: ${rows}x${cols}"
      case MatrixStorageLengthMismatch(shape, actual) =>
        s"data length $actual != rows*cols ${shape.entries}"
      case NonSquareMatrix(rows, cols) =>
        s"matrix must be square, got ${rows}x${cols}"
      case NonPositiveDefinite(pivot, value) =>
        s"matrix is not positive definite at pivot $pivot: $value"
      case DimensionMismatch(role, expected, actual) =>
        s"${role.label} expected $expected but got $actual"
      case IndexOutOfBounds(axis, index, limit) =>
        s"${axis.label} index $index out of bounds for size $limit"
      case InvalidParameter(parameter, value) =>
        s"${parameter.label} is invalid: $value"
      case NonFiniteValue(role, index, value) =>
        s"${role.label} value at linear index $index is not finite: $value"
