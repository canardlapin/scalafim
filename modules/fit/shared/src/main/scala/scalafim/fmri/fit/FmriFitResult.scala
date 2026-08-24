package scalafim.fmri.fit

import scalafim.fmri.model.{FitEngine, FitSummary}
import scalafim.fmri.design.{CoefficientAxis, RankPreview, RunCoefficientProjection}
import scalafim.fmri.ar.{InitialConditionPolicy, NoisePooling, WhiteningMethod}
import gale.linalg.{DMat, DVec}

final case class FitDiagnostics(
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    residualVariance: DVec
):
  require(residualVariance.length > 0, "diagnostics must contain at least one residual variance")

final case class ArRunDiagnostic(
    runIndex: Int,
    rho: Double,
    method: String,
    rows: Int,
    coefficients: Vector[Double] = Vector.empty,
    voxelwiseCoefficients: Vector[Vector[Double]] = Vector.empty
):
  require(runIndex >= 0, "run index must be non-negative")
  require(rho.isFinite, "AR rho must be finite")
  require(method.nonEmpty, "AR diagnostic method must be non-empty")
  require(rows > 0, "AR diagnostic rows must be positive")
  require(coefficients.forall(_.isFinite), "AR diagnostic coefficients must be finite")
  require(coefficients.isEmpty || coefficients.head == rho, "AR diagnostic rho must match the first coefficient")
  require(voxelwiseCoefficients.forall(_.forall(_.isFinite)), "voxelwise AR coefficients must be finite")

  def phi: Vector[Double] =
    if coefficients.isEmpty then Vector(rho) else coefficients

/** One contiguous source-time segment represented on the selected-row axis. */
final case class ArWhiteningSegment(
    runIndex: Int,
    startRow: Int,
    endRowExclusive: Int,
    startTimepoint: Int,
    endTimepointExclusive: Int
):
  require(runIndex >= 0, "AR whitening segment run index must be non-negative")
  require(startRow >= 0, "AR whitening segment start row must be non-negative")
  require(endRowExclusive > startRow, "AR whitening segment must contain selected rows")
  require(startTimepoint >= 0, "AR whitening segment start timepoint must be non-negative")
  require(endTimepointExclusive > startTimepoint, "AR whitening segment must contain source timepoints")
  require(
    endRowExclusive - startRow == endTimepointExclusive - startTimepoint,
    "AR whitening segment selected rows and source timepoints must have equal length"
  )

/** A run-local interval of source scans excluded between whitening segments. */
final case class ArCensorGap(
    runIndex: Int,
    startTimepoint: Int,
    endTimepointExclusive: Int
):
  require(runIndex >= 0, "AR censor gap run index must be non-negative")
  require(startTimepoint >= 0, "AR censor gap start must be non-negative")
  require(endTimepointExclusive > startTimepoint, "AR censor gap must contain at least one source scan")

  def timepoints: Vector[Int] =
    (startTimepoint until endTimepointExclusive).toVector

final case class ArWhiteningProvenance(
    method: WhiteningMethod,
    pooling: NoisePooling,
    initialCondition: InitialConditionPolicy,
    segments: Vector[ArWhiteningSegment],
    censorGaps: Vector[ArCensorGap]
):
  require(segments.nonEmpty, "AR whitening provenance must contain at least one segment")
  require(segments.map(segment => segment.startRow until segment.endRowExclusive).flatten.toVector ==
    (0 until segments.last.endRowExclusive).toVector, "AR whitening segments must cover selected rows exactly")
  require(censorGaps.forall(gap => segments.exists(_.runIndex == gap.runIndex)), "AR censor gaps must belong to represented runs")

final case class ArDiagnostics(
    order: Int,
    runs: Vector[ArRunDiagnostic],
    iterations: Int = 1,
    sharedNormalizedCovariance: Boolean = true,
    whitening: ArWhiteningProvenance
):
  require(order >= 1, "AR diagnostics require positive order")
  require(runs.nonEmpty, "AR diagnostics must contain at least one run")
  require(runs.forall(_.phi.length == order), "AR diagnostic coefficient length must match order")
  require(runs.forall(_.voxelwiseCoefficients.forall(_.length == order)), "voxelwise AR coefficient length must match order")
  require(iterations >= 0, "AR diagnostics iterations must be non-negative")
  require(sharedNormalizedCovariance || runs.forall(_.voxelwiseCoefficients.nonEmpty), "voxelwise AR diagnostics must carry per-voxel coefficients")

object ArDiagnostics:
  def merge(blocks: IndexedSeq[ArDiagnostics]): Either[FitError, ArDiagnostics] =
    if blocks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one AR diagnostics block is required"))
    else
      val first = blocks.head
      if blocks.forall(_.sharedNormalizedCovariance) then
        if blocks.forall(_ == first) then Right(first)
        else Left(FitError.IncompatibleFitBlocks("all dense blocks must have identical autocorrelation diagnostics"))
      else if blocks.exists(_.sharedNormalizedCovariance) then
        Left(FitError.IncompatibleFitBlocks("cannot merge shared and voxelwise autocorrelation diagnostics"))
      else validateVoxelwiseCompatible(blocks, first).map { _ =>
        val mergedRuns =
          first.runs.indices.toVector.map { runIndex =>
            val perVoxel =
              blocks.iterator.flatMap(block => block.runs(runIndex).voxelwiseCoefficients).toVector
            val summary = averageCoefficients(perVoxel, first.order)
            first.runs(runIndex).copy(
              rho = summary.head,
              coefficients = summary,
              voxelwiseCoefficients = perVoxel
            )
          }
        first.copy(runs = mergedRuns, sharedNormalizedCovariance = false)
      }

  private def validateVoxelwiseCompatible(
      blocks: IndexedSeq[ArDiagnostics],
      first: ArDiagnostics
  ): Either[FitError, Unit] =
    var blockIndex = 0
    while blockIndex < blocks.length do
      val block = blocks(blockIndex)
      if block.order != first.order then
        return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share order"))
      if block.iterations != first.iterations then
        return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share iteration count"))
      if block.runs.length != first.runs.length then
        return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share run layout"))

      var runIndex = 0
      while runIndex < first.runs.length do
        val left = first.runs(runIndex)
        val right = block.runs(runIndex)
        if left.runIndex != right.runIndex || left.method != right.method || left.rows != right.rows then
          return Left(FitError.IncompatibleFitBlocks("voxelwise AR chunks must share run diagnostics"))
        if right.voxelwiseCoefficients.exists(_.length != first.order) then
          return Left(FitError.IncompatibleFitBlocks("voxelwise AR coefficient length must match order"))
        runIndex += 1
      blockIndex += 1
    Right(())

  private def averageCoefficients(coefficients: Vector[Vector[Double]], order: Int): Vector[Double] =
    val out = new Array[Double](order)
    var row = 0
    while row < coefficients.length do
      var col = 0
      while col < order do
        out(col) += coefficients(row)(col)
        col += 1
      row += 1
    var col = 0
    while col < order do
      out(col) /= coefficients.length.toDouble
      col += 1
    out.toVector

sealed trait FmriFitResult:
  def columnNames: Vector[String]
  def voxelIndices: Vector[Int]
  def timepoints: Vector[Int]
  def engine: FitEngine
  def summary: FitSummary
  def coefficientAxis: Option[CoefficientAxis] = None
  def preparationProvenance: Option[ResponsePreparationProvenance] = None
  def fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
  def voxels: Int = voxelIndices.length

final case class DenseFmriFitResult(
    coefficients: CoefficientBlock,
    inference: CoefficientInference,
    residualVariance: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    columnNames: Vector[String],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary,
    olsDiagnostics: Option[OlsDiagnostics] = None,
    autocorrelation: Option[ArDiagnostics] = None,
    robustDiagnostics: Option[RobustDiagnostics] = None,
    override val coefficientAxis: Option[CoefficientAxis] = None,
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    voxelStatuses: Option[Vector[VoxelFitStatus]] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FmriFitResult:
  require(columnNames.length == coefficients.predictors, "column names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "voxel indices must match coefficient columns")
  require(timepoints.nonEmpty, "fit result must contain at least one timepoint")
  require(residualVariance.length == coefficients.voxels, "residual variances must match coefficient columns")
  require(inference.predictors == coefficients.predictors, "coefficient inference must match coefficient rows")
  require(inference.voxels == coefficients.voxels, "coefficient inference must match coefficient columns")
  require(olsDiagnostics.forall(_.predictors == coefficients.predictors), "OLS diagnostics must match coefficient rows")
  require(coefficientAxis.forall(_.predictors == coefficients.predictors), "coefficient axis must match coefficient rows")
  require(voxelStatuses.forall(_.length == coefficients.voxels), "voxel statuses must match coefficient columns")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "dense fit result")

  def predictors: Int = coefficients.predictors
  override def voxels: Int = coefficients.voxels
  def standardErrors: StandardErrorBlock = inference.standardErrors
  def normalizedCovariance: DMat = inference.normalizedCovariance
  def coefficientCovariance: CoefficientCovariance = inference.covariance
  def inferenceScope: CoefficientInferenceScope = inference.scope
  def rankReport: Option[RankDiagnostics] = olsDiagnostics.map(_.rankReport)
  def resolvedVoxelStatuses: Vector[VoxelFitStatus] =
    voxelStatuses.getOrElse(Vector.fill(coefficients.voxels)(VoxelFitStatus.Estimable))

  def voxelStatus(voxelIndex: Int): Option[VoxelFitStatus] =
    val position = voxelIndices.indexOf(voxelIndex)
    if position >= 0 then Some(resolvedVoxelStatuses(position))
    else fitExclusions.find(_.voxelIndex == voxelIndex).map(_.status)

  /**
    * Resolve numerical rank evidence against the result's structural axis.
    * A missing axis is reported explicitly rather than silently falling back to
    * rendered names or predictor positions.
    */
  def structuralRankReport: Either[FitError, StructuralRankReport] =
    for
      diagnostics <- olsDiagnostics.toRight(FitError.MissingStructuralIdentity("rank diagnostics"))
      axis <- coefficientAxis.toRight(FitError.MissingStructuralIdentity("rank diagnostics structural coefficient axis"))
      report <- diagnostics.rankReport.bind(axis)
    yield report

  /**
    * Relate fit-time factorization evidence to the compiler's full-design
    * preview.  A mismatch is not silently treated as a defect when fitting
    * deliberately selected, weighted, whitened, or robustly reweighted rows;
    * the transformation that changes the numerical factorization is retained
    * as a typed reason.
    */
  def rankPreviewComparison: Either[FitError, RankPreviewComparison] =
    for
      axis <- coefficientAxis.toRight(FitError.MissingStructuralIdentity("rank preview structural coefficient axis"))
      previewState <- axis.audit.rankPreview.toRight(FitError.MissingStructuralIdentity("compiled design rank preview"))
      preview <- previewState match
        case RankPreview.Available(value)         => Right(value)
        case RankPreview.Unavailable(_, _, reason) => Left(FitError.DesignRankPreviewUnavailable(reason))
      fitted <- structuralRankReport
    yield
      val expectedTimepoints = axis.rowLayout.selectedRows.map(_.oneBased - 1)
      val differences = Vector.newBuilder[RankPreviewDifference]
      if timepoints != expectedTimepoints then
        differences += RankPreviewDifference.SelectedRows(preview.rows, timepoints.length)
      val weighted = preparationProvenance.exists(_.records.exists {
        case ResponsePreparationRecord(ResponsePreparationStep.VolumeWeights(_), ResponsePreparationDisposition.Applied(_)) => true
        case _ => false
      })
      if weighted then differences += RankPreviewDifference.VolumeWeighting
      engine match
        case FitEngine.GeneralizedLeastSquares => differences += RankPreviewDifference.GeneralizedLeastSquares
        case FitEngine.RobustLeastSquares      => differences += RankPreviewDifference.RobustReweighting
        case _                                 => ()
      if fitted.method != RankDiagnosticMethod.PivotedQr ||
          fitted.toleranceConvention != preview.toleranceConvention then
        differences += RankPreviewDifference.SolverConvention
      val expectedDifferences = differences.result()
      val finalDifferences =
        if fitted.agreesWith(preview) then expectedDifferences
        else if expectedDifferences.nonEmpty then expectedDifferences
        else Vector(RankPreviewDifference.NumericalMismatch)
      RankPreviewComparison(preview, fitted, finalDifferences)

  def diagnostics: FitDiagnostics = FitDiagnostics(residualDegreesOfFreedom, residualVariance)
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def inferenceReady: Either[FitError, InferenceReadyDenseFit] =
    autocorrelation match
      case Some(diagnostics) if !diagnostics.sharedNormalizedCovariance && !inference.covariance.isVoxelwise =>
        Left(FitError.UnsupportedAutocorrelation("voxelwise AR contrast inference requires per-voxel covariance result bundles"))
      case _ =>
        robustDiagnostics match
          case Some(diagnostics) if !diagnostics.sharedNormalizedCovariance && !inference.covariance.isVoxelwise =>
            Left(FitError.UnsupportedRobust("robust contrast inference requires per-voxel covariance result bundles"))
          case _ =>
            Right(InferenceReadyDenseFit(this, inference.residualDegreesOfFreedom))

  def coefficient(columnName: String, voxelIndex: Int): Option[Double] =
    val row = columnNames.indexOf(columnName)
    val col = voxelIndices.indexOf(voxelIndex)
    if row < 0 || col < 0 then None else Some(coefficients(row, col))

final case class LssFmriFitResult(
    coefficients: CoefficientBlock,
    trialNames: Vector[String],
    lssDiagnostics: LssDiagnostics,
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary,
    override val coefficientAxis: Option[CoefficientAxis] = None,
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FmriFitResult:
  require(trialNames.length == coefficients.predictors, "trial names must match coefficient rows")
  require(voxelIndices.length == coefficients.voxels, "voxel indices must match coefficient columns")
  require(timepoints.nonEmpty, "LSS result must contain at least one timepoint")
  require(engine == FitEngine.LeastSquaresSeparate, "LSS result engine must be LeastSquaresSeparate")
  require(coefficientAxis.forall(_.predictors == coefficients.predictors), "LSS coefficient axis must match coefficient rows")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "LSS fit result")

  def columnNames: Vector[String] = trialNames
  def trials: Int = coefficients.predictors
  override def voxels: Int = coefficients.voxels
  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def coefficient(trialName: String, voxelIndex: Int): Option[Double] =
    val row = trialNames.indexOf(trialName)
    val col = voxelIndices.indexOf(voxelIndex)
    if row < 0 || col < 0 then None else Some(coefficients(row, col))

final case class RunwiseFmriRunResult(
    runIndex: Int,
    rowIndices: Vector[Int],
    timepoints: Vector[Int],
    coefficients: CoefficientBlock,
    standardErrors: StandardErrorBlock,
    normalizedCovariance: DMat,
    residualVariance: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    olsDiagnostics: OlsDiagnostics,
    coefficientAxis: Option[CoefficientAxis] = None,
    sourceColumnIndices: Vector[Int] = Vector.empty,
    projection: Option[RunCoefficientProjection] = None,
    voxelStatuses: Option[Vector[VoxelFitStatus]] = None
):
  require(rowIndices.nonEmpty, "run result must contain at least one selected row")
  require(rowIndices.length == timepoints.length, "run rows and timepoints must align")
  require(residualVariance.length == coefficients.voxels, "run residual variances must match coefficient columns")
  require(standardErrors.predictors == coefficients.predictors, "run standard errors must match coefficient rows")
  require(standardErrors.voxels == coefficients.voxels, "run standard errors must match coefficient columns")
  require(normalizedCovariance.rows == coefficients.predictors, "run covariance rows must match predictors")
  require(normalizedCovariance.cols == coefficients.predictors, "run covariance cols must match predictors")
  require(olsDiagnostics.predictors == coefficients.predictors, "run OLS diagnostics must match coefficient rows")
  require(coefficientAxis.forall(_.predictors == coefficients.predictors), "run coefficient axis must match coefficient rows")
  require(voxelStatuses.forall(_.length == coefficients.voxels), "run voxel statuses must match coefficient columns")
  require(
    sourceColumnIndices.isEmpty || sourceColumnIndices.length == coefficients.predictors,
    "run source column mapping must match coefficient rows"
  )
  require(sourceColumnIndices.distinct.length == sourceColumnIndices.length, "run source column mapping must be unique")
  require(sourceColumnIndices.forall(_ >= 0), "run source column mapping must be non-negative")
  require(
    projection.forall(value => value.sourceColumnIndices == effectiveSourceColumnIndices),
    "run projection must match source column mapping"
  )
  require(
    projection.forall(value => coefficientAxis.exists(_.structurallyCompatible(value.axis))),
    "run projection must match the local coefficient axis"
  )

  private def effectiveSourceColumnIndices: Vector[Int] =
    if sourceColumnIndices.nonEmpty then sourceColumnIndices else (0 until coefficients.predictors).toVector

  def sourceColumns: Vector[Int] = effectiveSourceColumnIndices
  def resolvedVoxelStatuses: Vector[VoxelFitStatus] =
    voxelStatuses.getOrElse(Vector.fill(coefficients.voxels)(VoxelFitStatus.Estimable))

  def localColumnNames(globalColumnNames: Vector[String]): Either[FitError, Vector[String]] =
    if globalColumnNames.isEmpty then
      Left(FitError.InvalidFitAxis("runwise result columns", "global column names must be non-empty"))
    else if effectiveSourceColumnIndices.exists(index => index < 0 || index >= globalColumnNames.length) then
      Left(FitError.InvalidFitAxis("runwise result columns", "source column mapping is out of bounds"))
    else Right(effectiveSourceColumnIndices.map(globalColumnNames))

  def coefficient(columnName: String, voxelIndex: Int, columnNames: Vector[String], voxelIndices: Vector[Int]): Option[Double] =
    for
      names <- localColumnNames(columnNames).toOption
      row = names.indexOf(columnName)
      col = voxelIndices.indexOf(voxelIndex)
      value <- if row < 0 || col < 0 then None else Some(coefficients(row, col))
    yield value

  def diagnostics: FitDiagnostics = FitDiagnostics(residualDegreesOfFreedom, residualVariance)
  def typedRunIndex: RunIndex = RunIndex.unsafe(runIndex)
  def selectedRows: Vector[SelectedRowIndex] = rowIndices.map(SelectedRowIndex.unsafe)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  /** Adapt one run's dense coefficient/inference bundle to the common
    * contrast evaluator.  The result retains the run-local structural axis;
    * callers must still validate a compiled hypothesis against that axis.
    */
  private[fit] def denseResult(
      columnNames: Vector[String],
      voxelIndices: Vector[Int],
      summary: FitSummary,
      fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
  ): Either[FitError, DenseFmriFitResult] =
    if voxelIndices.length != coefficients.voxels then
      Left(FitError.InvalidFitAxis("runwise result voxels", s"expected ${coefficients.voxels}, got ${voxelIndices.length}"))
    else
      localColumnNames(columnNames).flatMap { localNames =>
        for
          covariance <- CoefficientCovariance.shared(normalizedCovariance)
          inference <- CoefficientInference.fromExisting(
            scope = CoefficientInferenceScope.All,
            standardErrors = standardErrors,
            covariance = covariance,
            varianceScale = residualVariance,
            residualDegreesOfFreedom = residualDegreesOfFreedom
          )
        yield DenseFmriFitResult(
          coefficients = coefficients,
          inference = inference,
          residualVariance = residualVariance,
          residualDegreesOfFreedom = residualDegreesOfFreedom,
          columnNames = localNames,
          voxelIndices = voxelIndices,
          timepoints = timepoints,
          engine = FitEngine.RunwiseLeastSquares,
          summary = summary.copy(predictors = coefficients.predictors),
          olsDiagnostics = Some(olsDiagnostics),
          coefficientAxis = coefficientAxis,
          voxelStatuses = Some(resolvedVoxelStatuses),
          fitExclusions = fitExclusions
        )
      }

  /** Structural identity for this run's independently estimated coefficient
    * vector.  It is intentionally separate from the common source-design axis
    * carried by [[RunwiseFmriFitResult]].
    */
  def structuralCoefficientAxis: Option[CoefficientAxis] = coefficientAxis

final case class RunwiseFmriFitResult(
    runs: Vector[RunwiseFmriRunResult],
    columnNames: Vector[String],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    engine: FitEngine,
    summary: FitSummary,
    override val coefficientAxis: Option[CoefficientAxis] = None,
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FmriFitResult:
  require(runs.nonEmpty, "runwise result must contain at least one run")
  require(timepoints.nonEmpty, "runwise result must contain at least one timepoint")
  require(runs.forall(_.coefficients.voxels == voxelIndices.length), "run coefficients must match voxel indices")
  require(coefficientAxis.forall(_.predictors == columnNames.length), "coefficient axis must match column names")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "runwise fit result")
  require(runs.forall(_.sourceColumns.forall(index => index < columnNames.length)), "run source columns must match column names")
  require(
    runs.forall { run =>
      run.projection.forall { projection =>
        coefficientAxis.exists(_.structurallyCompatible(projection.sourceAxis))
      }
    },
    "run projections must match the common source coefficient axis"
  )

  def selectedVoxels: SelectedVoxelIndices = SelectedVoxelIndices.unsafe(voxelIndices)
  def selectedTimepoints: SelectedTimepointIndices = SelectedTimepointIndices.unsafe(timepoints)

  def run(runIndex: Int): Option[RunwiseFmriRunResult] =
    runs.find(_.runIndex == runIndex)

  def coefficientAxisForRun(runIndex: Int): Option[CoefficientAxis] =
    run(runIndex).flatMap(_.coefficientAxis)

  def structuralRankReports: Either[FitError, Vector[StructuralRankReport]] =
    for
      axis <- coefficientAxis.toRight(FitError.MissingStructuralIdentity("runwise rank diagnostics structural coefficient axis"))
      reports <- runs.foldLeft[Either[FitError, Vector[StructuralRankReport]]](Right(Vector.empty)) { (acc, run) =>
        for
          values <- acc
          report <- run.olsDiagnostics.rankReport.bind(run.coefficientAxis.getOrElse(axis))
        yield values :+ report
      }
    yield reports

/** Result of an independently fitted-run fixed-effects combination. */
final case class FixedEffectsFmriFitResult(
    coefficients: CoefficientBlock,
    inference: CoefficientInference,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    columnNames: Vector[String],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    summary: FitSummary,
    sufficientStatistics: FixedEffectsSufficientStatistics,
    policy: FixedEffectsPolicy,
    override val coefficientAxis: Option[CoefficientAxis],
    override val preparationProvenance: Option[ResponsePreparationProvenance] = None,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FmriFitResult:
  require(columnNames.length == coefficients.predictors, "fixed-effects column names must match coefficients")
  require(voxelIndices.length == coefficients.voxels, "fixed-effects voxel indices must match coefficients")
  require(timepoints.nonEmpty, "fixed-effects result must retain selected timepoints")
  require(inference.predictors == coefficients.predictors, "fixed-effects inference must match coefficients")
  require(inference.voxels == coefficients.voxels, "fixed-effects inference must match coefficients")
  require(residualDegreesOfFreedom == inference.residualDegreesOfFreedom, "fixed-effects result degrees of freedom must match inference")
  require(summary.engine == FitEngine.FixedEffects, "fixed-effects result summary must use the fixed-effects engine")
  require(summary.coefficientScope == scalafim.fmri.model.CoefficientScope.SeparateRunsThenFixedEffects, "fixed-effects result summary must expose its coefficient scope")
  require(policy == sufficientStatistics.policy, "fixed-effects result policy must match sufficient-statistics policy")
  require(coefficientAxis.forall(_.structurallyCompatible(sufficientStatistics.coefficientAxis)), "fixed-effects result axis must match sufficient-statistics axis")
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "fixed-effects fit result")

  def predictors: Int = coefficients.predictors
  override def voxels: Int = coefficients.voxels
  val engine: FitEngine = FitEngine.FixedEffects
  def standardErrors: StandardErrorBlock = inference.standardErrors
  def normalizedCovariance: DMat = inference.normalizedCovariance
  def coefficientCovariance: CoefficientCovariance = inference.covariance
  def inferenceScope: CoefficientInferenceScope = inference.scope
  def effectiveResidualDegreesOfFreedom: ResidualDegreesOfFreedom = residualDegreesOfFreedom
  def perRunContributions: Vector[FixedEffectsRunContribution] = sufficientStatistics.contributions

  /** Make the fixed-effects covariance available to the existing contrast
    * evaluator.  The dense view uses unit variance scale because the
    * covariance matrices already contain their complete run-combined scale.
    */
  private[fit] def denseInferenceResult: DenseFmriFitResult =
    DenseFmriFitResult(
      coefficients = coefficients,
      inference = inference,
      residualVariance = FixedEffects.ones(voxelIndices.length),
      residualDegreesOfFreedom = residualDegreesOfFreedom,
      columnNames = columnNames,
      voxelIndices = voxelIndices,
      timepoints = timepoints,
      engine = FitEngine.FixedEffects,
      summary = summary,
      coefficientAxis = coefficientAxis,
      preparationProvenance = preparationProvenance,
      fitExclusions = fitExclusions
    )

  def inferenceReady: Either[FitError, InferenceReadyDenseFit] =
    Right(InferenceReadyDenseFit(denseInferenceResult, residualDegreesOfFreedom))

/** One independently fitted response observation pattern. */
final case class ObservationPatternFitResult(
    pattern: ObservationPattern,
    result: FmriFitResult
):
  require(
    result.voxelIndices.nonEmpty && result.voxelIndices.forall(pattern.sourceVoxels.contains),
    "observation-pattern result voxels must be a non-empty subset of its pattern"
  )
  require(
    result.timepoints == pattern.sourceTimepoints,
    "observation-pattern result timepoints must match its pattern"
  )

/** A scientifically heterogeneous fit whose voxels do not share one temporal
  * design geometry.
  *
  * Each child result retains its own rows, rank diagnostics, normalized
  * covariance, and ordinary residual degrees of freedom. The outer result
  * retains the source selection order without inventing a shared inference
  * geometry.
  */
final case class PatternedFmriFitResult(
    patternResults: Vector[ObservationPatternFitResult],
    voxelIndices: Vector[Int],
    timepoints: Vector[Int],
    summary: FitSummary,
    override val fitExclusions: Vector[VoxelInferenceExclusion] = Vector.empty
) extends FmriFitResult:
  require(patternResults.nonEmpty, "patterned fit result must contain at least one fitted pattern")
  require(timepoints.nonEmpty, "patterned fit result must retain the outer selected timepoints")
  require(voxelIndices.nonEmpty, "patterned fit result must retain at least one fitted voxel")
  require(voxelIndices.distinct.length == voxelIndices.length, "patterned fit result voxels must be unique")
  require(
    patternResults.map(_.pattern.id.value).distinct.length == patternResults.length,
    "patterned fit result ids must be unique"
  )
  require(
    patternResults.flatMap(_.result.voxelIndices).toSet == voxelIndices.toSet,
    "patterned fit result children must cover exactly the retained voxels"
  )
  require(
    patternResults.flatMap(_.result.voxelIndices).distinct.length == voxelIndices.length,
    "patterned fit result child voxel sets must be disjoint"
  )
  require(
    patternResults.forall(_.result.engine == patternResults.head.result.engine),
    "patterned fit result children must use one engine"
  )
  require(
    patternResults.forall(_.result.columnNames == patternResults.head.result.columnNames),
    "patterned fit result children must share structural columns"
  )
  require(
    patternResults.forall(_.result.coefficientAxis == patternResults.head.result.coefficientAxis),
    "patterned fit result children must share one structural coefficient axis"
  )
  VoxelInferenceExclusions.validateDisjoint(voxelIndices, fitExclusions, "patterned fit result")

  val engine: FitEngine = patternResults.head.result.engine
  val columnNames: Vector[String] = patternResults.head.result.columnNames
  override val coefficientAxis: Option[CoefficientAxis] = patternResults.head.result.coefficientAxis

  def patternForVoxel(voxelIndex: Int): Option[ObservationPattern] =
    patternResults.find(_.pattern.sourceVoxels.contains(voxelIndex)).map(_.pattern)

  def resultForVoxel(voxelIndex: Int): Option[FmriFitResult] =
    patternResults.find(_.result.voxelIndices.contains(voxelIndex)).map(_.result)

  def densePatterns: Either[FitError, Vector[(ObservationPattern, DenseFmriFitResult)]] =
    val out = Vector.newBuilder[(ObservationPattern, DenseFmriFitResult)]
    var index = 0
    while index < patternResults.length do
      patternResults(index) match
        case ObservationPatternFitResult(pattern, dense: DenseFmriFitResult) =>
          out += pattern -> dense
        case ObservationPatternFitResult(_, other) =>
          return Left(FitError.NonDenseFitResult(other.engine))
      index += 1
    Right(out.result())

private[fit] object FmriFitResults:
  /** Reconcile a result's exclusions with an ordered outer execution stream.
    * The outer order is authoritative so chunking cannot reorder source voxel
    * identities when some chunks are entirely excluded.
    */
  def mergeFitExclusions(
      result: FmriFitResult,
      ordered: Vector[VoxelInferenceExclusion]
  ): Either[FitError, FmriFitResult] =
    VoxelInferenceExclusions.combine(ordered, result.fitExclusions).map { combined =>
      result match
        case dense: DenseFmriFitResult =>
          dense.copy(fitExclusions = combined)
        case lss: LssFmriFitResult =>
          lss.copy(fitExclusions = combined)
        case runwise: RunwiseFmriFitResult =>
          runwise.copy(fitExclusions = combined)
        case fixed: FixedEffectsFmriFitResult =>
          fixed.copy(fitExclusions = combined)
        case patterned: PatternedFmriFitResult =>
          patterned.copy(fitExclusions = combined)
    }
