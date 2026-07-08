package scalafim.multivar

enum IndexAxis:
  case Row
  case Column
  case Sample
  case Feature
  case Component
  case Block

  def label: String =
    this match
      case Row       => "row"
      case Column    => "column"
      case Sample    => "sample"
      case Feature   => "feature"
      case Component => "component"
      case Block     => "block"

enum MultivarError:
  case InvalidId(kind: String, value: String, reason: String)
  case InvalidDimension(kind: String, value: Int)
  case DimensionOverflow(rows: Int, cols: Int)
  case EmptyIndexSet(axis: IndexAxis)
  case IndexOutOfBounds(axis: IndexAxis, index: Int, limit: Int)
  case DuplicateIndex(axis: IndexAxis, index: Int)
  case EmptyBlockPartition
  case DuplicateBlock(id: BlockId)
  case MissingBlockColumn(index: Int)
  case InvalidBlockPartition(detail: String)
  case MatrixShapeMismatch(detail: String)
  case InvalidMap(detail: String)
  case InvalidRowGeometry(detail: String)
  case SingularRowMetric(detail: String)
  case InvalidKernelFit(detail: String)
  case NonComposableMaps(left: MvSpace, right: MvSpace)
  case DecoderUnavailable(detail: String)
  case SolverFailed(detail: String)
  case NonSymmetricMatrix(row: Int, col: Int, left: Double, right: Double)
  case InvalidComponentRequest(requested: Int, limit: Int)
  case NonFiniteValue(role: String, index: Int, value: Double)
  case DensificationRejected(operation: String, storage: StorageKind)
  case MetricShapeMismatch(axis: IndexAxis, expected: Int, actual: Int)
  case NonPositiveSemiDefinite(role: String, eigenvalue: Double)
  case IterationLimitExceeded(method: String, maxIterations: Int, residual: Double)

  def message: String =
    this match
      case InvalidId(kind, value, reason) =>
        s"invalid $kind '$value': $reason"
      case InvalidDimension(kind, value) =>
        s"$kind must be positive, got $value"
      case DimensionOverflow(rows, cols) =>
        s"matrix dimensions are too large for row-major storage: ${rows}x${cols}"
      case EmptyIndexSet(axis) =>
        s"${axis.label} index set must be non-empty"
      case IndexOutOfBounds(axis, index, limit) =>
        s"${axis.label} index $index out of bounds for size $limit"
      case DuplicateIndex(axis, index) =>
        s"${axis.label} index set contains duplicate index $index"
      case EmptyBlockPartition =>
        "block partition must contain at least one block"
      case DuplicateBlock(id) =>
        s"block partition contains duplicate block id '${id.value}'"
      case MissingBlockColumn(index) =>
        s"block partition does not cover column $index"
      case InvalidBlockPartition(detail) =>
        detail
      case MatrixShapeMismatch(detail) =>
        detail
      case InvalidMap(detail) =>
        detail
      case InvalidRowGeometry(detail) =>
        detail
      case SingularRowMetric(detail) =>
        detail
      case InvalidKernelFit(detail) =>
        detail
      case NonComposableMaps(left, right) =>
        s"map codomain ${left.id.value}:${left.size} is not composable with domain ${right.id.value}:${right.size}"
      case DecoderUnavailable(detail) =>
        detail
      case SolverFailed(detail) =>
        detail
      case NonSymmetricMatrix(row, col, left, right) =>
        s"matrix is not symmetric at ($row, $col): $left vs $right"
      case InvalidComponentRequest(requested, limit) =>
        s"requested $requested component(s), but at most $limit are available"
      case NonFiniteValue(role, index, value) =>
        s"$role value at linear index $index is not finite: $value"
      case DensificationRejected(operation, storage) =>
        s"$operation would densify ${storage.label} input"
      case MetricShapeMismatch(axis, expected, actual) =>
        s"${axis.label} metric is ${actual}x${actual} but the data ${axis.label} axis has size $expected"
      case NonPositiveSemiDefinite(role, eigenvalue) =>
        s"$role is not positive semi-definite: eigenvalue $eigenvalue is below tolerance"
      case IterationLimitExceeded(method, maxIterations, residual) =>
        s"$method did not converge within $maxIterations iterations (residual $residual)"
