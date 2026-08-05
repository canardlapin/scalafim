package scalafim.fmri.fit

import scalafim.fmri.design.{CoefficientAxis, ColumnId, DesignFingerprint, RankPreviewEvidence, RankPreviewMethod, RankToleranceConvention, StructuralColumn}

/** The numerical factorization used to produce a rank report. */
enum RankDiagnosticMethod:
  case PivotedQr
  case CholeskyNormalEquations

/** Why fit-time rank evidence is not expected to equal the compiled preview. */
enum RankPreviewDifference:
  case SelectedRows(previewRows: Int, fittedRows: Int)
  case VolumeWeighting
  case GeneralizedLeastSquares
  case RobustReweighting
  case SolverConvention
  case NumericalMismatch

/** Comparison of authoritative design-preview and fit-time rank evidence. */
final case class RankPreviewComparison(
    preview: RankPreviewEvidence,
    fitted: StructuralRankReport,
    differences: Vector[RankPreviewDifference]
):
  require(differences.distinct.length == differences.length, "rank preview differences must be unique")

  def exact: Boolean = differences.isEmpty

/**
  * Rank and pivot evidence for a design factorization.
  *
  * Predictor indices are numerical lowering details.  They are deliberately
  * retained alongside the pivot order so a caller can compare projections and
  * estimable functions without pretending that an arbitrary coefficient vector
  * is canonical in a rank-deficient parameterization.  When a coefficient axis
  * is available, [[bind]] lifts the same evidence to stable structural columns.
  */
final case class RankDiagnostics(
    method: RankDiagnosticMethod,
    predictorCount: Int,
    numericalRank: Int,
    toleranceConvention: RankToleranceConvention,
    tolerance: Double,
    pivotOrder: Vector[Int],
    independentPredictors: Vector[Int],
    aliasedPredictors: Vector[Int],
    diagonalR: Vector[Double] = Vector.empty,
    conditionEstimate: Option[Double] = None
):
  require(predictorCount > 0, "rank diagnostics require at least one predictor")
  require(numericalRank >= 0 && numericalRank <= predictorCount, "numerical rank must be within predictor count")
  require(tolerance >= 0.0 && tolerance.isFinite, "rank diagnostic tolerance must be finite and non-negative")
  require(pivotOrder.length == predictorCount, "pivot order must cover every predictor")
  require(pivotOrder.distinct.length == predictorCount, "pivot order must be a permutation")
  require(pivotOrder.forall(index => index >= 0 && index < predictorCount), "pivot order contains an out-of-bounds predictor")
  require(independentPredictors.length == numericalRank, "independent predictor count must equal numerical rank")
  require(aliasedPredictors.length == predictorCount - numericalRank, "aliased predictor count must equal rank deficiency")
  require(independentPredictors.distinct.length == independentPredictors.length, "independent predictors must be unique")
  require(aliasedPredictors.distinct.length == aliasedPredictors.length, "aliased predictors must be unique")
  require(independentPredictors.forall(index => index >= 0 && index < predictorCount), "independent predictor out of bounds")
  require(aliasedPredictors.forall(index => index >= 0 && index < predictorCount), "aliased predictor out of bounds")
  require((independentPredictors ++ aliasedPredictors).distinct.length == predictorCount, "rank partition must cover each predictor exactly once")
  require(diagonalR.forall(value => value.isFinite && value >= 0.0), "QR diagonal magnitudes must be finite and non-negative")
  require(conditionEstimate.forall(value => value.isFinite && value >= 1.0), "condition estimate must be finite and at least one")

  def rank: Int = numericalRank

  def deficient: Boolean = numericalRank < predictorCount

  def fullRank: Boolean = !deficient

  /** Compare factorization identity without comparing data-dependent scale details. */
  def structurallyCompatible(other: RankDiagnostics): Boolean =
    method == other.method &&
      predictorCount == other.predictorCount &&
      numericalRank == other.numericalRank &&
      toleranceConvention == other.toleranceConvention &&
      pivotOrder == other.pivotOrder &&
      independentPredictors == other.independentPredictors &&
      aliasedPredictors == other.aliasedPredictors

  /** Lift numerical indices to the immutable structural coefficient axis. */
  def bind(axis: CoefficientAxis): Either[FitError, StructuralRankReport] =
    if axis.predictors != predictorCount then
      Left(
        FitError.InvalidFitAxis(
          "rank diagnostics",
          s"predictor count $predictorCount does not match coefficient axis ${axis.predictors}"
        )
      )
    else
      Right(
        StructuralRankReport(
          designFingerprint = axis.designFingerprint,
          method = method,
          predictorCount = predictorCount,
          numericalRank = numericalRank,
          toleranceConvention = toleranceConvention,
          tolerance = tolerance,
          pivotOrder = pivotOrder.map(axis.columns),
          independentColumns = independentPredictors.map(axis.columns),
          aliasedColumns = aliasedPredictors.map(axis.columns),
          diagonalR = diagonalR,
          conditionEstimate = conditionEstimate
        )
      )

object RankDiagnostics:
  def fromPivotedQr(
      predictorCount: Int,
      rank: Int,
      tolerance: Double,
      pivotOrder: Vector[Int],
      diagonalR: Vector[Double],
      toleranceConvention: RankToleranceConvention = RankToleranceConvention.Absolute
  ): RankDiagnostics =
    require(pivotOrder.length == predictorCount, "QR pivot order must cover every predictor")
    val independent = pivotOrder.take(rank)
    val aliased = pivotOrder.drop(rank)
    val nonZero = diagonalR.take(rank).filter(_ > 0.0)
    val condition =
      if nonZero.isEmpty then None
      else Some(nonZero.max / nonZero.min)
    RankDiagnostics(
      method = RankDiagnosticMethod.PivotedQr,
      predictorCount = predictorCount,
      numericalRank = rank,
      toleranceConvention = toleranceConvention,
      tolerance = tolerance,
      pivotOrder = pivotOrder,
      independentPredictors = independent,
      aliasedPredictors = aliased,
      diagonalR = diagonalR,
      conditionEstimate = condition
    )

  def fullRankNormalEquations(predictorCount: Int, tolerance: Double): RankDiagnostics =
    RankDiagnostics(
      method = RankDiagnosticMethod.CholeskyNormalEquations,
      predictorCount = predictorCount,
      numericalRank = predictorCount,
      toleranceConvention = RankToleranceConvention.Absolute,
      tolerance = tolerance,
      pivotOrder = (0 until predictorCount).toVector,
      independentPredictors = (0 until predictorCount).toVector,
      aliasedPredictors = Vector.empty
    )

/** Rank evidence resolved against structural design columns. */
final case class StructuralRankReport(
    designFingerprint: DesignFingerprint,
    method: RankDiagnosticMethod,
    predictorCount: Int,
    numericalRank: Int,
    toleranceConvention: RankToleranceConvention,
    tolerance: Double,
    pivotOrder: Vector[StructuralColumn],
    independentColumns: Vector[StructuralColumn],
    aliasedColumns: Vector[StructuralColumn],
    diagonalR: Vector[Double] = Vector.empty,
    conditionEstimate: Option[Double] = None
):
  require(predictorCount == pivotOrder.length, "structural pivot order must cover every predictor")
  require(numericalRank == independentColumns.length, "structural independent columns must match rank")
  require(predictorCount - numericalRank == aliasedColumns.length, "structural aliased columns must match rank deficiency")

  def aliasedColumnIds: Vector[ColumnId] = aliasedColumns.map(_.id)

  def independentColumnIds: Vector[ColumnId] = independentColumns.map(_.id)

  def deficient: Boolean = numericalRank < predictorCount

  /**
    * Compare the fit factorization with the design compiler's authoritative
    * preview. Exact agreement is expected only when the fit uses the same rows
    * and no weighting or whitening transform.
    */
  def agreesWith(preview: RankPreviewEvidence): Boolean =
    method == RankDiagnosticMethod.PivotedQr &&
      preview.method == RankPreviewMethod.PivotedQr &&
      predictorCount == preview.columns &&
      numericalRank == preview.numericalRank &&
      toleranceConvention == preview.toleranceConvention &&
      tolerance == preview.tolerance &&
      pivotOrder.map(_.id) == preview.pivotOrder &&
      independentColumnIds == preview.independentColumns &&
      aliasedColumnIds == preview.aliasedColumns &&
      diagonalR == preview.diagonalR &&
      conditionEstimate == preview.conditionEstimate
