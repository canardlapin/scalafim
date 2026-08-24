package scalafim.fmri.ar

enum CoefficientScopeKind:
  case Global
  case ByRun

enum ArError:
  case RowMismatch(leftRows: Int, rightRows: Int)
  case NonPositiveRows(rows: Int)
  case EmptySegments
  case SegmentGap(index: Int, expectedStart: Int, actualStart: Int)
  case SegmentOutOfBounds(segment: TimeSegment, rows: Int)
  case SegmentCoverageMismatch(coveredRows: Int, matrixRows: Int)
  case NonContiguousRunIndex(segmentIndex: Int, previousRunIndex: Int, actualRunIndex: Int)
  case ExcludedRowOutOfBounds(row: Int, matrixRows: Int)
  case NoEstimableRows
  case NonContiguousRunLabel(firstRow: Int, repeatedRow: Int)
  case MissingRunSegments(runIndex: Int)
  case CoefficientScopeMismatch(scope: CoefficientScopeKind, coefficientSets: Int, runCount: Int)
  case InvalidArOrder(order: Int)
  case InvalidArLag(lag: Int)
  case ArOrderNotEstimable(requested: ArOrderValue, maxEstimable: ArLag)
  case EmptyAutocovariances
  case InsufficientAutocovariances(required: Int, actual: Int)
  case NonFiniteAutocovariance(lag: ArLag, value: Double)
  case NonFiniteResidual(row: Int, column: Int, value: Double)
  case NonFiniteArCoefficient(index: Int, value: Double)
  case NonFiniteMaCoefficient(index: Int, value: Double)
  case NonFinitePartialAutocorrelation(index: Int, value: Double)
  case InvalidStationarityBound(bound: Double)
  case InvalidExactFirstAr1(phi: Double)
  case InvalidInitialScale(scale: Double)
  case NonStationaryArCoefficients(maxRootMagnitude: Double)
  case NonInvertibleMaCoefficients(maxRootMagnitude: Double)
  case StationarityCheckFailed(detail: String)
  case UnableToEstimateArModel

  def message: String =
    this match
      case RowMismatch(leftRows, rightRows) =>
        s"row mismatch: $leftRows vs $rightRows"
      case NonPositiveRows(rows) =>
        s"row count must be positive, got $rows"
      case EmptySegments =>
        "segments must be non-empty"
      case SegmentGap(index, expectedStart, actualStart) =>
        s"expected segment $index to start at $expectedStart, got $actualStart"
      case SegmentOutOfBounds(segment, rows) =>
        s"segment ${segment.start}:${segment.endExclusive} is out of bounds for $rows rows"
      case SegmentCoverageMismatch(coveredRows, matrixRows) =>
        s"segments cover $coveredRows rows but matrix has $matrixRows rows"
      case NonContiguousRunIndex(segmentIndex, previousRunIndex, actualRunIndex) =>
        s"segment $segmentIndex has run index $actualRunIndex after run index $previousRunIndex; run indices must begin at zero and advance contiguously"
      case ExcludedRowOutOfBounds(row, matrixRows) =>
        s"excluded row $row is out of bounds for $matrixRows rows"
      case NoEstimableRows =>
        "noise estimation has no retained rows"
      case NonContiguousRunLabel(firstRow, repeatedRow) =>
        s"run label beginning at row $firstRow reappears non-contiguously at row $repeatedRow"
      case MissingRunSegments(runIndex) =>
        s"run $runIndex has no segments"
      case CoefficientScopeMismatch(scope, coefficientSets, runCount) =>
        val scopeName = scope match
          case CoefficientScopeKind.Global => "global"
          case CoefficientScopeKind.ByRun  => "run"
        s"$scopeName whitening has $coefficientSets coefficient sets for $runCount runs"
      case InvalidArOrder(order) =>
        s"AR order must be non-negative, got $order"
      case InvalidArLag(lag) =>
        s"AR lag must be non-negative, got $lag"
      case ArOrderNotEstimable(requested, maxEstimable) =>
        s"requested AR(${requested.value}) but only ${maxEstimable.value} lags are estimable"
      case EmptyAutocovariances =>
        "autocovariances must include lag zero"
      case InsufficientAutocovariances(required, actual) =>
        s"need $required autocovariance values, got $actual"
      case NonFiniteAutocovariance(lag, value) =>
        s"autocovariance at lag ${lag.value} must be finite, got $value"
      case NonFiniteResidual(row, column, value) =>
        s"residual at row $row, column $column must be finite, got $value"
      case NonFiniteArCoefficient(index, value) =>
        s"AR coefficient at index $index must be finite, got $value"
      case NonFiniteMaCoefficient(index, value) =>
        s"MA coefficient at index $index must be finite, got $value"
      case NonFinitePartialAutocorrelation(index, value) =>
        s"partial autocorrelation at index $index must be finite, got $value"
      case InvalidStationarityBound(bound) =>
        s"stationarity bound must be finite and in (0, 1), got $bound"
      case InvalidExactFirstAr1(phi) =>
        s"exact AR(1) first-row scaling requires abs(phi) < 1, got $phi"
      case InvalidInitialScale(scale) =>
        s"initial-condition scale must be finite and non-negative, got $scale"
      case NonStationaryArCoefficients(maxRootMagnitude) =>
        s"AR coefficients are not stationary; recurrence-root magnitude is $maxRootMagnitude"
      case NonInvertibleMaCoefficients(maxRootMagnitude) =>
        s"MA coefficients are not invertible; innovation-recursion root magnitude is $maxRootMagnitude"
      case StationarityCheckFailed(detail) =>
        s"could not verify AR stationarity: $detail"
      case UnableToEstimateArModel =>
        "unable to estimate AR model"
