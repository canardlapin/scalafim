package scalafim.fmri.fit

import scalafim.fmri.design.CoefficientAxis
import scalafim.fmri.model.{FitEngine, FitSummary}
import gale.linalg.{CholeskyOptions, DMat, DVec, Matrix, Vec}

/** Weighting used when independently fitted runs are combined.
  *
  * The initial implementation deliberately exposes one scientifically
  * explicit policy.  A run contributes the inverse of its coefficient
  * covariance, rather than a sample-count or voxel-count shortcut.
  */
enum FixedEffectsWeighting:
  case InverseCovariance

/** Availability policy for shared estimands.  The initial executable policy
  * intentionally refuses to average a coefficient that is absent or
  * non-estimable in any contributing run.
  */
enum FixedEffectsSharedRunPolicy:
  case RequireAllRuns

/** Policies that affect fixed-effects estimands.  These are retained in the
  * result so a coefficient vector is never detached from its combination
  * convention.
  */
final case class FixedEffectsPolicy(
    weighting: FixedEffectsWeighting = FixedEffectsWeighting.InverseCovariance,
    sharedRunPolicy: FixedEffectsSharedRunPolicy = FixedEffectsSharedRunPolicy.RequireAllRuns
)

/** One run's contribution to a fixed-effects sufficient-statistics fold.
  *
  * `precisionByVoxel(v)` is the inverse covariance of the run's coefficient
  * estimate for voxel `v`; `precisionWeightedCoefficients` contains
  * precision times the coefficient vector for each voxel.  Retaining these
  * values makes the combination auditable and makes pairwise folding
  * associative up to floating-point summation order.
  */
final case class FixedEffectsRunContribution(
    runIndex: Int,
    rowCount: Int,
    timepoints: Vector[Int],
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    precisionByVoxel: Vector[DMat],
    precisionWeightedCoefficients: DMat,
    sourceColumnIndices: Vector[Int] = Vector.empty
):
  require(runIndex >= 0, "fixed-effects run index must be non-negative")
  require(rowCount > 0, "fixed-effects run row count must be positive")
  require(timepoints.nonEmpty, "fixed-effects run timepoints must be non-empty")
  require(rowCount == timepoints.length, "fixed-effects run row count must match timepoints")
  require(precisionByVoxel.nonEmpty, "fixed-effects run must contain at least one voxel precision")
  require(precisionByVoxel.forall(matrix => matrix.rows > 0 && matrix.rows == matrix.cols), "fixed-effects precision matrices must be non-empty and square")
  require(precisionByVoxel.forall(_.rows == precisionByVoxel.head.rows), "fixed-effects precision matrices must share predictor count")
  require(precisionByVoxel.forall(FixedEffectsRunContribution.allFinite), "fixed-effects precision matrices must be finite")
  require(precisionWeightedCoefficients.rows == precisionByVoxel.head.rows, "fixed-effects weighted coefficients must match precision rows")
  require(precisionWeightedCoefficients.cols == precisionByVoxel.length, "fixed-effects weighted coefficients must match voxel count")
  require(FixedEffectsRunContribution.allFinite(precisionWeightedCoefficients), "fixed-effects weighted coefficients must be finite")
  require(
    sourceColumnIndices.isEmpty || sourceColumnIndices.length == precisionByVoxel.head.rows,
    "fixed-effects source-column mapping must match predictor count"
  )
  require(sourceColumnIndices.distinct.length == sourceColumnIndices.length, "fixed-effects source-column mapping must be unique")
  require(sourceColumnIndices.forall(_ >= 0), "fixed-effects source-column mapping must be non-negative")

  def predictors: Int = precisionByVoxel.head.rows
  def voxels: Int = precisionByVoxel.length
  def sourceColumns: Vector[Int] =
    if sourceColumnIndices.nonEmpty then sourceColumnIndices else (0 until predictors).toVector

object FixedEffectsRunContribution:
  private[fit] def allFinite(matrix: DMat): Boolean =
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        if !matrix(row, col).isFinite then return false
        col += 1
      row += 1
    true

/** Sufficient statistics and structural evidence for a fixed-effects fold. */
final case class FixedEffectsSufficientStatistics(
    coefficientAxis: CoefficientAxis,
    voxelIndices: Vector[Int],
    contributions: Vector[FixedEffectsRunContribution],
    policy: FixedEffectsPolicy = FixedEffectsPolicy(),
    sourceColumnIndices: Vector[Int] = Vector.empty
):
  require(voxelIndices.nonEmpty, "fixed-effects statistics must contain at least one voxel")
  require(voxelIndices.distinct.length == voxelIndices.length, "fixed-effects voxel indices must be unique")
  require(contributions.nonEmpty, "fixed-effects statistics must contain at least one run")
  require(contributions.map(_.runIndex).distinct.length == contributions.length, "fixed-effects run contributions must be unique")
  require(contributions.forall(_.voxels == voxelIndices.length), "fixed-effects runs must share the voxel count")
  require(contributions.forall(_.predictors == coefficientAxis.predictors), "fixed-effects runs must share the coefficient axis")
  require(
    sourceColumnIndices.isEmpty || sourceColumnIndices.length == coefficientAxis.predictors,
    "fixed-effects source-column mapping must match the coefficient axis"
  )
  require(sourceColumnIndices.distinct.length == sourceColumnIndices.length, "fixed-effects source-column mapping must be unique")
  require(sourceColumnIndices.forall(_ >= 0), "fixed-effects source-column mapping must be non-negative")
  require(contributions.forall(_.sourceColumns == effectiveSourceColumnIndices), "fixed-effects contributions must share source-column mapping")

  def effectiveSourceColumnIndices: Vector[Int] =
    if sourceColumnIndices.nonEmpty then sourceColumnIndices else coefficientAxis.columns.map(_.ordinal.zeroBased)

  def effectiveResidualDegreesOfFreedom: Either[FitError, ResidualDegreesOfFreedom] =
    val total = contributions.foldLeft(0L)((sum, contribution) => sum + contribution.residualDegreesOfFreedom.value.toLong)
    if total > Int.MaxValue then
      Left(FitError.FixedEffectsIncompatible("effective residual degrees of freedom overflow Int"))
    else ResidualDegreesOfFreedom(total.toInt)

  def runIndices: Vector[Int] = contributions.map(_.runIndex).sorted

  /** Combine disjoint run contributions without losing their provenance. */
  def combine(other: FixedEffectsSufficientStatistics): Either[FitError, FixedEffectsSufficientStatistics] =
    if policy != other.policy then
      Left(FitError.FixedEffectsIncompatible("fixed-effects weighting policies differ"))
    else if !coefficientAxis.structurallyCompatible(other.coefficientAxis) then
      Left(FitError.FixedEffectsIncompatible(
        s"coefficient axes differ (expected ${coefficientAxis.designFingerprint.value}/${coefficientAxis.columnIds.mkString(",")}, " +
          s"got ${other.coefficientAxis.designFingerprint.value}/${other.coefficientAxis.columnIds.mkString(",")})"
      ))
    else if effectiveSourceColumnIndices != other.effectiveSourceColumnIndices then
      Left(FitError.FixedEffectsIncompatible("fixed-effects source-column mappings differ"))
    else if voxelIndices != other.voxelIndices then
      Left(FitError.FixedEffectsIncompatible("voxel identities differ"))
    else if runIndices.intersect(other.runIndices).nonEmpty then
      Left(FitError.FixedEffectsIncompatible("run contributions overlap"))
    else
      Right(copy(contributions = (contributions ++ other.contributions).sortBy(_.runIndex)))

object FixedEffects:
  val DefaultPolicy: FixedEffectsPolicy = FixedEffectsPolicy()

  /** Combine a runwise fit using inverse coefficient covariance weighting. */
  def combine(
      result: RunwiseFmriFitResult,
      policy: FixedEffectsPolicy = DefaultPolicy
  ): Either[FitError, FixedEffectsFmriFitResult] =
    for
      axis <- result.coefficientAxis.toRight(FitError.MissingStructuralIdentity("fixed-effects coefficient axis"))
      statistics <- fromRunwise(result, axis, policy)
      names <- columnNamesFor(result.columnNames, statistics.effectiveSourceColumnIndices)
      fixedSummary = result.summary.copy(
        engine = FitEngine.FixedEffects,
        predictors = statistics.coefficientAxis.predictors,
        coefficientScope = scalafim.fmri.model.CoefficientScope.SeparateRunsThenFixedEffects
      )
      combined <- combine(statistics, names, result.timepoints, fixedSummary, result.preparationProvenance)
    yield combined

  /** Combine two disjoint fixed-effects statistics folds. */
  def combineStatistics(
      left: FixedEffectsSufficientStatistics,
      right: FixedEffectsSufficientStatistics,
      columnNames: Vector[String],
      timepoints: Vector[Int],
      summary: FitSummary,
      preparationProvenance: Option[ResponsePreparationProvenance]
  ): Either[FitError, FixedEffectsFmriFitResult] =
    left.combine(right).flatMap { stats =>
      val normalizedSummary = summary.copy(predictors = stats.coefficientAxis.predictors)
      combine(stats, columnNames, timepoints, normalizedSummary, preparationProvenance)
    }

  private def combine(
      statistics: FixedEffectsSufficientStatistics,
      columnNames: Vector[String],
      timepoints: Vector[Int],
      summary: FitSummary,
      preparationProvenance: Option[ResponsePreparationProvenance]
  ): Either[FitError, FixedEffectsFmriFitResult] =
    if summary.engine != FitEngine.FixedEffects then
      Left(FitError.FixedEffectsIncompatible("fixed-effects result summary must use FitEngine.FixedEffects"))
    else if columnNames.length != statistics.coefficientAxis.predictors then
      Left(FitError.InvalidFitAxis("fixed-effects column names", s"expected ${statistics.coefficientAxis.predictors}, got ${columnNames.length}"))
    else if timepoints.isEmpty then
      Left(FitError.InvalidFitAxis("fixed-effects timepoints", "must be non-empty"))
    else
      materialize(statistics).map { case (coefficients, inference, df) =>
        FixedEffectsFmriFitResult(
          coefficients = coefficients,
          inference = inference,
          residualDegreesOfFreedom = df,
          columnNames = columnNames,
          voxelIndices = statistics.voxelIndices,
          timepoints = timepoints,
          summary = summary,
          sufficientStatistics = statistics,
          policy = statistics.policy,
          coefficientAxis = Some(statistics.coefficientAxis),
          preparationProvenance = preparationProvenance
        )
      }

  private def fromRunwise(
      result: RunwiseFmriFitResult,
      axis: CoefficientAxis,
      policy: FixedEffectsPolicy
  ): Either[FitError, FixedEffectsSufficientStatistics] =
    val projections = result.runs.flatMap(_.projection)
    if projections.nonEmpty && projections.length != result.runs.length then
      return Left(FitError.FixedEffectsIncompatible("all runs must carry a coefficient projection or none may carry one"))

    val sharedColumns =
      if projections.isEmpty then (0 until axis.predictors).toVector
      else
        val perRun = projections.map(_.sharedSourceColumnIndices.toSet)
        val common = perRun.reduce(_.intersect(_)).toVector.sorted
        val sharedSomewhere = perRun.foldLeft(Set.empty[Int])(_ union _)
        val missing = sharedSomewhere.diff(common.toSet).toVector.sorted
        policy.sharedRunPolicy match
          case FixedEffectsSharedRunPolicy.RequireAllRuns if missing.nonEmpty =>
            val names = missing.map(index => axis.columns(index).id.value)
            return Left(FitError.FixedEffectsIncompatible(
              s"shared estimands are not supported in every run: ${names.mkString(", ")}"
            ))
          case FixedEffectsSharedRunPolicy.RequireAllRuns => ()
        common

    if sharedColumns.isEmpty then
      return Left(FitError.FixedEffectsIncompatible("no shared estimand columns remain after run-local projection"))

    val selectedAxis = axis.select(sharedColumns) match
      case Left(error) => return Left(FitError.FixedEffectsIncompatible(error.message))
      case Right(value) => value
    val contributions = Vector.newBuilder[FixedEffectsRunContribution]
    var runPosition = 0
    while runPosition < result.runs.length do
      val run = result.runs(runPosition)
      if run.coefficientAxis.isEmpty then
        return Left(FitError.MissingStructuralIdentity(s"fixed-effects run ${run.runIndex} coefficient axis"))
      val runAxis = run.coefficientAxis.get
      if runAxis.designFingerprint != axis.designFingerprint then
        return Left(FitError.FixedEffectsIncompatible(s"run ${run.runIndex} design fingerprint differs from the shared axis"))
      contribution(run, policy, sharedColumns) match
        case Left(error) => return Left(error)
        case Right(value) => contributions += value
      runPosition += 1
    Right(FixedEffectsSufficientStatistics(selectedAxis, result.voxelIndices, contributions.result(), policy, sharedColumns))

  private def contribution(
      run: RunwiseFmriRunResult,
      policy: FixedEffectsPolicy,
      sourceColumns: Vector[Int]
  ): Either[FitError, FixedEffectsRunContribution] =
    policy.weighting match
      case FixedEffectsWeighting.InverseCovariance =>
        val localSourceColumns = run.sourceColumns
        val localPositions = sourceColumns.map(localSourceColumns.indexOf)
        if localPositions.exists(_ < 0) then
          Left(FitError.FixedEffectsIncompatible(
            s"run ${run.runIndex} does not carry every shared source column (${sourceColumns.mkString(", ")})"
          ))
        else
          val predictors = localPositions.length
          val localCoefficients = selectRows(run.coefficients.value, localPositions)
          val localCovariance = selectRowsCols(run.normalizedCovariance, localPositions)
          if run.residualVariance.length != run.coefficients.voxels then
            Left(FitError.FixedEffectsContributionFailure(run.runIndex, -1, "residual variance count does not match coefficients"))
          else if localCovariance.rows != predictors || localCovariance.cols != predictors then
            Left(FitError.FixedEffectsContributionFailure(run.runIndex, -1, "normalized covariance shape does not match coefficients"))
          else
            inverse(localCovariance, run.runIndex, -1).flatMap { basePrecision =>
              val precision = Vector.newBuilder[DMat]
              var invalid: FitError | Null = null
              var voxel = 0
              while voxel < run.coefficients.voxels && invalid == null do
                val variance = run.residualVariance(voxel)
                if !(variance > 0.0 && variance.isFinite) then
                  invalid = FitError.FixedEffectsContributionFailure(run.runIndex, voxel, s"residual variance must be positive and finite, got $variance")
                else precision += scale(basePrecision, 1.0 / variance)
                voxel += 1
              invalid match
                case error: FitError => Left(error)
                case null =>
                  val baseWeighted = basePrecision * localCoefficients
                  val weighted = Matrix.newBuilder(predictors, run.coefficients.voxels)
                  var row = 0
                  while row < predictors do
                    voxel = 0
                    while voxel < run.coefficients.voxels do
                      weighted(row, voxel) = baseWeighted(row, voxel) / run.residualVariance(voxel)
                      voxel += 1
                    row += 1
                  Right(FixedEffectsRunContribution(
                    runIndex = run.runIndex,
                    rowCount = run.rowIndices.length,
                    timepoints = run.timepoints,
                    residualDegreesOfFreedom = run.residualDegreesOfFreedom,
                    precisionByVoxel = precision.result(),
                    precisionWeightedCoefficients = weighted.result(),
                    sourceColumnIndices = sourceColumns
                  ))
            }

  private def columnNamesFor(
      names: Vector[String],
      sourceColumns: Vector[Int]
  ): Either[FitError, Vector[String]] =
    if sourceColumns.isEmpty then
      Left(FitError.InvalidFitAxis("fixed-effects column names", "source-column mapping must be non-empty"))
    else if sourceColumns.exists(index => index < 0 || index >= names.length) then
      Left(FitError.InvalidFitAxis("fixed-effects column names", "source-column mapping is out of bounds"))
    else Right(sourceColumns.map(names))

  private def selectRows(matrix: DMat, rows: IndexedSeq[Int]): DMat =
    Matrix.tabulate(rows.length, matrix.cols) { (row, col) => matrix(rows(row), col) }

  private def selectRowsCols(matrix: DMat, rows: IndexedSeq[Int]): DMat =
    Matrix.tabulate(rows.length, rows.length) { (row, col) => matrix(rows(row), rows(col)) }

  private def materialize(
      statistics: FixedEffectsSufficientStatistics
  ): Either[FitError, (CoefficientBlock, CoefficientInference, ResidualDegreesOfFreedom)] =
    val predictors = statistics.coefficientAxis.predictors
    val voxels = statistics.voxelIndices.length
    val totalPrecision = Vector.tabulate(voxels)(_ => Matrix.newBuilder(predictors, predictors))
    val totalWeighted = Matrix.newBuilder(predictors, voxels)
    var contributionIndex = 0
    while contributionIndex < statistics.contributions.length do
      val contribution = statistics.contributions(contributionIndex)
      var voxel = 0
      while voxel < voxels do
        val precision = contribution.precisionByVoxel(voxel)
        val builder = totalPrecision(voxel)
        var row = 0
        while row < predictors do
          var col = 0
          while col < predictors do
            builder(row, col) = builder(row, col) + precision(row, col)
            col += 1
          row += 1
        row = 0
        while row < predictors do
          totalWeighted(row, voxel) = totalWeighted(row, voxel) + contribution.precisionWeightedCoefficients(row, voxel)
          row += 1
        voxel += 1
      contributionIndex += 1

    val coefficientMatrix = Matrix.newBuilder(predictors, voxels)
    val covarianceMatrices = Vector.newBuilder[DMat]
    var voxel = 0
    while voxel < voxels do
      val precision = totalPrecision(voxel).result()
      val rhs = Matrix.newBuilder(predictors, 1)
      var row = 0
      while row < predictors do
        rhs(row, 0) = totalWeighted(row, voxel)
        row += 1
      inverse(precision, statistics.contributions.head.runIndex, voxel) match
        case Left(error) => return Left(error)
        case Right(covariance) =>
          precision.cholesky(CholeskyOptions(tolerance(precision))) match
            case Left(error) => return Left(FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, error.getMessage))
            case Right(cholesky) =>
              cholesky.solve(rhs.result()) match
                case Left(error) => return Left(FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, error.getMessage))
                case Right(beta) =>
                  row = 0
                  while row < predictors do
                    coefficientMatrix(row, voxel) = beta(row, 0)
                    row += 1
                  covarianceMatrices += covariance
      voxel += 1

    for
      df <- statistics.effectiveResidualDegreesOfFreedom
      covariance <- CoefficientCovariance.voxelwise(covarianceMatrices.result())
      inference <- CoefficientInference.fromCovariance(
        scope = CoefficientInferenceScope.All,
        covariance = covariance,
        varianceScale = ones(voxels),
        residualDegreesOfFreedom = df,
        method = CoefficientInferenceMethod.Classical
      )
    yield (CoefficientBlock(coefficientMatrix.result()), inference, df)

  private def inverse(matrix: DMat, runIndex: Int, voxelIndex: Int): Either[FitError, DMat] =
    matrix.cholesky(CholeskyOptions(tolerance(matrix)))
      .left
      .map(error => FitError.FixedEffectsContributionFailure(runIndex, voxelIndex, error.getMessage))
      .flatMap(_.solve(Matrix.eye(matrix.rows)).left.map(error => FitError.FixedEffectsContributionFailure(runIndex, voxelIndex, error.getMessage)))

  private def tolerance(matrix: DMat): Double =
    var maximum = 0.0
    var index = 0
    while index < matrix.rows do
      maximum = math.max(maximum, math.abs(matrix(index, index)))
      index += 1
    math.max(1e-12, maximum * 1e-12)

  private def scale(matrix: DMat, factor: Double): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols)((row, col) => matrix(row, col) * factor)

  private[fit] def ones(length: Int): DVec =
    val out = Vec.newBuilder(length)
    var index = 0
    while index < length do
      out(index) = 1.0
      index += 1
    out.result()
