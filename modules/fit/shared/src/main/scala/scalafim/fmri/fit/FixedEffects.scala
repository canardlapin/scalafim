package scalafim.fmri.fit

import scalafim.fmri.design.{CoefficientAxis, RunCoefficientProjection}
import scalafim.fmri.model.{FitEngine, FitSummary}
import gale.backend.Backend
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

/** Availability policy for voxelwise fixed-effects contributions. Requiring
  * every run keeps one shared reference degrees of freedom and prevents a
  * zero variance from being misread as infinite precision.
  */
enum FixedEffectsVoxelPolicy:
  case RequireAllRuns

/** Policies that affect fixed-effects estimands.  These are retained in the
  * result so a coefficient vector is never detached from its combination
  * convention.
  */
final case class FixedEffectsPolicy(
    weighting: FixedEffectsWeighting = FixedEffectsWeighting.InverseCovariance,
    sharedRunPolicy: FixedEffectsSharedRunPolicy = FixedEffectsSharedRunPolicy.RequireAllRuns,
    voxelPolicy: FixedEffectsVoxelPolicy = FixedEffectsVoxelPolicy.RequireAllRuns
)

private final case class FixedEffectsVoxelSelection(
    positions: Vector[Int],
    voxelIndices: Vector[Int],
    exclusions: Vector[VoxelInferenceExclusion]
):
  require(positions.nonEmpty, "fixed-effects voxel selection must retain at least one voxel")
  require(positions.length == voxelIndices.length, "fixed-effects voxel positions must match voxel identities")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, exclusions, "fixed-effects voxel selection")

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
    precisionByVoxel: IndexedSeq[DMat],
    precisionWeightedCoefficients: DMat,
    sourceColumnIndices: Vector[Int] = Vector.empty
):
  require(runIndex >= 0, "fixed-effects run index must be non-negative")
  require(rowCount > 0, "fixed-effects run row count must be positive")
  require(timepoints.nonEmpty, "fixed-effects run timepoints must be non-empty")
  require(rowCount == timepoints.length, "fixed-effects run row count must match timepoints")
  require(precisionByVoxel.nonEmpty, "fixed-effects run must contain at least one voxel precision")
  require(CoefficientMatrixStorage.valid(precisionByVoxel, precisionByVoxel.head.rows), "fixed-effects precision matrices must be finite and share a positive square shape")
  require(precisionWeightedCoefficients.rows == precisionByVoxel.head.rows, "fixed-effects weighted coefficients must match precision rows")
  require(precisionWeightedCoefficients.cols == precisionByVoxel.length, "fixed-effects weighted coefficients must match voxel count")
  require(FixedEffectsRunContribution.allFinite(precisionWeightedCoefficients), "fixed-effects weighted coefficients must be finite")
  require(
    sourceColumnIndices.isEmpty || sourceColumnIndices.length == precisionByVoxel.head.rows,
    "fixed-effects source-column mapping must match predictor count"
  )
  require(sourceColumnIndices.distinct.length == sourceColumnIndices.length, "fixed-effects source-column mapping must be unique")
  require(sourceColumnIndices.forall(_ >= 0), "fixed-effects source-column mapping must be non-negative")

  def retainedPrecisionDoubleCount: Long = CoefficientMatrixStorage.retained(precisionByVoxel)
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

/** Shared preparation retains fit provenance while deferring output materialization. */
private[fit] final case class PreparedFixedEffectsStatistics(
    statistics: FixedEffectsSufficientStatistics,
    columnNames: Vector[String],
    timepoints: Vector[Int],
    summary: FitSummary,
    preparationProvenance: Option[ResponsePreparationProvenance],
    fitExclusions: Vector[VoxelInferenceExclusion]
)

object FixedEffects:
  val DefaultPolicy: FixedEffectsPolicy = FixedEffectsPolicy()

  /** Combine a runwise fit using inverse coefficient covariance weighting. */
  def combine(
      result: RunwiseFmriFitResult,
      policy: FixedEffectsPolicy = DefaultPolicy
  ): Either[FitError, FixedEffectsFmriFitResult] =
    prepareStatistics(result, policy).flatMap { prepared =>
      combine(prepared.statistics, prepared.columnNames, prepared.timepoints, prepared.summary,
        prepared.preparationProvenance, prepared.fitExclusions)
    }

  private[fit] def prepareStatistics(
      result: RunwiseFmriFitResult,
      policy: FixedEffectsPolicy
  )(using Backend): Either[FitError, PreparedFixedEffectsStatistics] =
    for
      axis <- result.coefficientAxis.toRight(FitError.MissingStructuralIdentity("fixed-effects coefficient axis"))
      selection <- selectVoxels(result, policy)
      statistics <- fromRunwise(result, axis, policy, selection.positions, selection.voxelIndices)
      exclusions <- VoxelInferenceExclusions.combine(result.fitExclusions, selection.exclusions)
      names <- columnNamesFor(result.columnNames, statistics.effectiveSourceColumnIndices)
      fixedSummary = result.summary.copy(
        engine = FitEngine.FixedEffects,
        predictors = statistics.coefficientAxis.predictors,
        coefficientScope = scalafim.fmri.model.CoefficientScope.SeparateRunsThenFixedEffects
      )
    yield PreparedFixedEffectsStatistics(statistics, names, result.timepoints, fixedSummary,
      result.preparationProvenance, exclusions)

  /** Combine two disjoint fixed-effects statistics folds. */
  def combineStatistics(
      left: FixedEffectsSufficientStatistics,
      right: FixedEffectsSufficientStatistics,
      columnNames: Vector[String],
      timepoints: Vector[Int],
      summary: FitSummary,
      preparationProvenance: Option[ResponsePreparationProvenance],
      fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
  ): Either[FitError, FixedEffectsFmriFitResult] =
    left.combine(right).flatMap { stats =>
      val normalizedSummary = summary.copy(predictors = stats.coefficientAxis.predictors)
      combine(stats, columnNames, timepoints, normalizedSummary, preparationProvenance, fitExclusions)
    }

  private def combine(
      statistics: FixedEffectsSufficientStatistics,
      columnNames: Vector[String],
      timepoints: Vector[Int],
      summary: FitSummary,
      preparationProvenance: Option[ResponsePreparationProvenance],
      fitExclusions: Vector[VoxelInferenceExclusion]
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
          preparationProvenance = preparationProvenance,
          fitExclusions = fitExclusions
        )
      }

  private def selectVoxels(
      result: RunwiseFmriFitResult,
      policy: FixedEffectsPolicy
  ): Either[FitError, FixedEffectsVoxelSelection] =
    policy.voxelPolicy match
      case FixedEffectsVoxelPolicy.RequireAllRuns =>
        val positions = Vector.newBuilder[Int]
        val voxelIndices = Vector.newBuilder[Int]
        val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
        var voxel = 0
        while voxel < result.voxelIndices.length do
          val statuses = result.runs.map(run => fixedEffectsStatus(run, voxel))
          val status = VoxelFitStatus.aggregate(statuses)
          if status.supportsInference then
            positions += voxel
            voxelIndices += result.voxelIndices(voxel)
          else
            exclusions += VoxelInferenceExclusion(result.voxelIndices(voxel), status)
          voxel += 1

        val retainedPositions = positions.result()
        val retainedVoxelIndices = voxelIndices.result()
        val omitted = exclusions.result()
        if retainedPositions.isEmpty then
          VoxelInferenceExclusions
            .combine(result.fitExclusions, omitted)
            .flatMap(all => Left(FitError.AllVoxelsExcluded(all)))
        else Right(FixedEffectsVoxelSelection(retainedPositions, retainedVoxelIndices, omitted))

  private def fixedEffectsStatus(
      run: RunwiseFmriRunResult,
      voxelPosition: Int
  ): VoxelFitStatus =
    run.resolvedVoxelStatuses(voxelPosition) match
      case VoxelFitStatus.Estimable =>
        val variance = run.residualVariance(voxelPosition)
        if !variance.isFinite then VoxelFitStatus.NonFinite
        else if variance <= 0.0 then VoxelFitStatus.ZeroResidualVariance
        else VoxelFitStatus.Estimable
      case status => status

  private def fromRunwise(
      result: RunwiseFmriFitResult,
      axis: CoefficientAxis,
      policy: FixedEffectsPolicy,
      voxelPositions: Vector[Int],
      voxelIndices: Vector[Int]
  )(using Backend): Either[FitError, FixedEffectsSufficientStatistics] =
    val sharedColumns = sharedSourceColumns(axis, result.runs.map(_.projection), policy) match
      case Left(error) => return Left(error)
      case Right(value) => value

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
      contribution(run, policy, sharedColumns, voxelPositions) match
        case Left(error) => return Left(error)
        case Right(value) => contributions += value
      runPosition += 1
    Right(FixedEffectsSufficientStatistics(selectedAxis, voxelIndices, contributions.result(), policy, sharedColumns))

  private[fit] def sharedSourceColumns(
      axis: CoefficientAxis,
      runProjections: Vector[Option[RunCoefficientProjection]],
      policy: FixedEffectsPolicy
  ): Either[FitError, Vector[Int]] =
    val projections = runProjections.flatten
    if projections.nonEmpty && projections.length != runProjections.length then
      return Left(FitError.FixedEffectsIncompatible("all runs must carry a coefficient projection or none may carry one"))

    val selected =
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

    if selected.isEmpty then
      return Left(FitError.FixedEffectsIncompatible("no shared estimand columns remain after run-local projection"))

    Right(selected)

  private def contribution(
      run: RunwiseFmriRunResult,
      policy: FixedEffectsPolicy,
      sourceColumns: Vector[Int],
      voxelPositions: Vector[Int]
  )(using Backend): Either[FitError, FixedEffectsRunContribution] =
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
          val localCoefficients = selectColumns(selectRows(run.coefficients.value, localPositions), voxelPositions)
          val localCovariance = selectRowsCols(run.normalizedCovariance, localPositions)
          if run.residualVariance.length != run.coefficients.voxels then
            Left(FitError.FixedEffectsContributionFailure(run.runIndex, -1, "residual variance count does not match coefficients"))
          else if localCovariance.rows != predictors || localCovariance.cols != predictors then
            Left(FitError.FixedEffectsContributionFailure(run.runIndex, -1, "normalized covariance shape does not match coefficients"))
          else
            inverse(localCovariance, run.runIndex, -1).flatMap { basePrecision =>
              val scales = Vector.newBuilder[Double]
              var invalid: Option[FitError] = None
              var voxel = 0
              while voxel < voxelPositions.length && invalid.isEmpty do
                val sourceVoxel = voxelPositions(voxel)
                val variance = run.residualVariance(sourceVoxel)
                if !(variance > 0.0 && variance.isFinite) then
                  invalid = Some(FitError.FixedEffectsContributionFailure(run.runIndex, sourceVoxel, s"residual variance must be positive and finite, got $variance"))
                else scales += 1.0 / variance
                voxel += 1
              invalid match
                case Some(error) => Left(error)
                case None =>
                  val baseWeighted = basePrecision * localCoefficients
                  val weighted = Matrix.newBuilder(predictors, voxelPositions.length)
                  var row = 0
                  while row < predictors do
                    voxel = 0
                    while voxel < voxelPositions.length do
                      weighted(row, voxel) = baseWeighted(row, voxel) / run.residualVariance(voxelPositions(voxel))
                      voxel += 1
                    row += 1
                  CoefficientMatrixStorage.scaled(basePrecision, scales.result()).map { precision => FixedEffectsRunContribution(
                    runIndex = run.runIndex,
                    rowCount = run.rowIndices.length,
                    timepoints = run.timepoints,
                    residualDegreesOfFreedom = run.residualDegreesOfFreedom,
                    precisionByVoxel = precision,
                    precisionWeightedCoefficients = weighted.result(),
                    sourceColumnIndices = sourceColumns
                  ) }
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

  private def selectColumns(matrix: DMat, columns: IndexedSeq[Int]): DMat =
    Matrix.tabulate(matrix.rows, columns.length) { (row, col) => matrix(row, columns(col)) }

  private def selectRowsCols(matrix: DMat, rows: IndexedSeq[Int]): DMat =
    Matrix.tabulate(rows.length, rows.length) { (row, col) => matrix(rows(row), rows(col)) }

  private def materialize(
      statistics: FixedEffectsSufficientStatistics
  ): Either[FitError, (CoefficientBlock, CoefficientInference, ResidualDegreesOfFreedom)] =
    val predictors = statistics.coefficientAxis.predictors
    val voxels = statistics.voxelIndices.length
    val fields = statistics.contributions.map(_.precisionByVoxel)
    val covariance = CoefficientCovariance.fromPrecisionSum(fields) match
      case Left(error) => return Left(error)
      case Right(value) => value
    val coefficientMatrix = Matrix.newBuilder(predictors, voxels)
    val standardErrors = Matrix.newBuilder(predictors, voxels)
    var voxel = 0
    while voxel < voxels do
      val precision = CoefficientMatrixStorage.sumAt(fields, voxel)
      val rhs = Matrix.newBuilder(predictors, 1)
      statistics.contributions.foreach { contribution =>
        var row = 0
        while row < predictors do
          rhs(row, 0) = rhs(row, 0) + contribution.precisionWeightedCoefficients(row, voxel)
          row += 1
      }
      val solved = for
        factor <- precision.cholesky(CholeskyOptions(tolerance(precision)))
          .left.map(e => FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, e.getMessage))
        beta <- factor.solve(rhs.result()).left.map(e => FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, e.getMessage))
        inverse <- factor.solve(Matrix.eye(predictors)).left.map(e => FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, e.getMessage))
      yield (beta, inverse)
      solved match
        case Left(error) => return Left(error)
        case Right((beta, inverse)) =>
          var row = 0
          while row < predictors do
            coefficientMatrix(row, voxel) = beta(row, 0)
            val variance = inverse(row, row)
            if variance < -1e-12 || !variance.isFinite then
              return Left(FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, "invalid combined variance"))
            standardErrors(row, voxel) = math.sqrt(math.max(0.0, variance))
            row += 1
      voxel += 1
    for
      df <- statistics.effectiveResidualDegreesOfFreedom
      inference <- CoefficientInference.fromExisting(
        scope = CoefficientInferenceScope.All,
        standardErrors = StandardErrorBlock(standardErrors.result()),
        covariance = covariance,
        varianceScale = ones(voxels),
        residualDegreesOfFreedom = df,
        method = CoefficientInferenceMethod.Classical
      )
    yield (CoefficientBlock(coefficientMatrix.result()), inference, df)

  private[fit] def inverse(matrix: DMat, runIndex: Int, voxelIndex: Int): Either[FitError, DMat] =
    matrix.cholesky(CholeskyOptions(tolerance(matrix)))
      .left
      .map(error => FitError.FixedEffectsContributionFailure(runIndex, voxelIndex, error.getMessage))
      .flatMap(_.solve(Matrix.eye(matrix.rows)).left.map(error => FitError.FixedEffectsContributionFailure(runIndex, voxelIndex, error.getMessage)))

  private[fit] def tolerance(matrix: DMat): Double =
    var maximum = 0.0
    var index = 0
    while index < matrix.rows do
      maximum = math.max(maximum, math.abs(matrix(index, index)))
      index += 1
    math.max(1e-12, maximum * 1e-12)

  private[fit] def ones(length: Int): DVec =
    val out = Vec.newBuilder(length)
    var index = 0
    while index < length do
      out(index) = 1.0
      index += 1
    out.result()
