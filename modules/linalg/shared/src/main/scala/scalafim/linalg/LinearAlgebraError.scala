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
  case NonSymmetricMatrix(row: Int, col: Int, left: Double, right: Double)
  case NonPositiveDefinite(pivot: Int, value: Double)
  case RankDeficient(requiredRank: Int, actualRank: Int)
  case InvalidDecompositionRank(requested: Int, limit: Int)
  case DimensionMismatch(role: DimensionRole, expected: Int, actual: Int)
  case IndexOutOfBounds(axis: MatrixAxis, index: Int, limit: Int)
  case InvalidParameter(parameter: NumericParameter, value: Double)
  case NonFiniteValue(role: MatrixValueRole, index: Int, value: Double)
  case SolverDidNotConverge(method: String, maxIterations: Int)
  case OperatorOrderUnsupported(method: String, order: Int, maximum: Int)
  case OperatorApplicationFailed(detail: String)
  case BackendFailure(backend: String, detail: String)

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
      case NonSymmetricMatrix(row, col, left, right) =>
        s"matrix is not symmetric at ($row, $col): $left vs $right"
      case NonPositiveDefinite(pivot, value) =>
        s"matrix is not positive definite at pivot $pivot: $value"
      case RankDeficient(requiredRank, actualRank) =>
        s"matrix rank $actualRank is less than required rank $requiredRank"
      case InvalidDecompositionRank(requested, limit) =>
        if limit == Int.MaxValue then s"decomposition rank must be positive, got $requested"
        else s"requested decomposition rank $requested, but at most $limit component(s) are available"
      case DimensionMismatch(role, expected, actual) =>
        s"${role.label} expected $expected but got $actual"
      case IndexOutOfBounds(axis, index, limit) =>
        s"${axis.label} index $index out of bounds for size $limit"
      case InvalidParameter(parameter, value) =>
        s"${parameter.label} is invalid: $value"
      case NonFiniteValue(role, index, value) =>
        s"${role.label} value at linear index $index is not finite: $value"
      case SolverDidNotConverge(method, maxIterations) =>
        s"$method did not converge within $maxIterations iteration(s)"
      case OperatorOrderUnsupported(method, order, maximum) =>
        s"$method supports operator order at most $maximum, got $order"
      case OperatorApplicationFailed(detail) =>
        s"linear operator application failed: $detail"
      case BackendFailure(backend, detail) =>
        s"$backend backend failed: $detail"
