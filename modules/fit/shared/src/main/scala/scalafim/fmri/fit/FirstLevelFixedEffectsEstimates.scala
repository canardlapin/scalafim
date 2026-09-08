package scalafim.fmri.fit

import gale.backend.Backend
import gale.linalg.{DMat, DVec, Matrix, QR}
import scalafim.dataset.{DataSelection, DatasetSeriesReader}
import scalafim.fmri.design.{CoefficientAxis, RunIndex as DesignRunIndex, ScanIndex}
import scalafim.fmri.model.{FitEngine, FitPlan}

/** An entirely excluded block still has an explicit, identified outcome. */
enum FixedEffectsEstimateBlockResult:
  case Selected(result: FixedEffectsEstimateResult, exclusions: Vector[VoxelInferenceExclusion])
  case Excluded(exclusions: Vector[VoxelInferenceExclusion])

final case class FirstLevelFixedEffectsEstimateBlock private[fit] (
    ordinal: ChunkOrdinal,
    inputVoxelIndices: Vector[Int],
    result: FixedEffectsEstimateBlockResult,
    runCoefficients: RunCoefficientRetentionResult
)

final case class FixedEffectsRunPreparation private[fit] (
    partition: RunPartition,
    coefficientAxis: CoefficientAxis,
    diagnostics: OlsDiagnostics,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom
)

/** Factors and readouts are prepared once per selected run. Each response
  * block produces only the jointly pooled precision statistics and requested
  * final maps. Additional run coefficients are constructed only on explicit
  * request, without run standard-error maps or another factorization.
  * Variance remains an internal requirement of the pooled model.
  */
final class FirstLevelFixedEffectsEstimatePlan private[fit] (
    val fitPlan: FitPlan,
    val selection: CompiledEstimateSelection,
    val pooledCoefficientAxis: CoefficientAxis,
    val chunks: FitChunkPlan,
    val preparation: ResponsePreparationProvenance,
    val policy: FixedEffectsPolicy,
    private val runs: Vector[PreparedPrecisionRun],
    private val sourceColumns: Vector[Int]
):
  def runPreparations: Vector[FixedEffectsRunPreparation] = runs.map(_.preparation)
  def retainedRunCoefficients: Vector[RunCoefficientRetentionDescription] = runs.flatMap(_.retention)
  def timepoints: Vector[Int] = chunks.timepoints
  def maxBlockVoxels: Int = chunks.iterator.map(_.voxelIndices.length).max
  def computesResidualVariance: Boolean = true

  /** Completion counts input voxels, including explicitly excluded voxels.
    * No previous response or output block is retained by this executor.
    */
  def foreachBlock(
      reader: DatasetSeriesReader,
      consume: FirstLevelFixedEffectsEstimateBlock => Either[FitError, Unit],
      cancelled: () => Boolean = () => false
  )(using Backend): Either[FitError, EstimateExecutionOutcome] =
    EstimateBlockExecutor.foreachBlock(fitPlan, chunks, reader, evaluate, consume, cancelled)

  private def evaluate(chunk: FitChunkSpec, response: ResponseBlock)(using Backend): Either[FitError, FirstLevelFixedEffectsEstimateBlock] =
    val runResults = Vector.newBuilder[PrecisionRunBlock]
    var run = 0
    while run < runs.length do
      runs(run).evaluate(response, chunk.voxelIndices) match
        case Left(error) => return Left(error)
        case Right(value) => runResults += value
      run += 1
    val values = runResults.result()
    val retained = Vector.newBuilder[Int]
    val excluded = Vector.newBuilder[VoxelInferenceExclusion]
    var voxel = 0
    while voxel < response.voxels do
      val status = VoxelFitStatus.aggregate(values.map(_.statuses(voxel)))
      if status.supportsInference then retained += voxel
      else excluded += VoxelInferenceExclusion(chunk.voxelIndices(voxel), status)
      voxel += 1
    val positions = retained.result()
    val exclusions = excluded.result()
    val result =
      if positions.isEmpty then Right(FixedEffectsEstimateBlockResult.Excluded(exclusions))
      else
        val contributions = Vector.newBuilder[FixedEffectsRunContribution]
        run = 0
        while run < runs.length do
          runs(run).contribution(values(run), positions, sourceColumns) match
            case Left(error) => return Left(error)
            case Right(value) => contributions += value
          run += 1
        val statistics = FixedEffectsSufficientStatistics(pooledCoefficientAxis, positions.map(chunk.voxelIndices),
          contributions.result(), policy, sourceColumns)
        FixedEffectsEstimates.estimatePrimary(statistics, selection).map(value => FixedEffectsEstimateBlockResult.Selected(value, exclusions))
    val retainedBlocks = values.flatMap(_.retained)
    val retention = if retainedBlocks.isEmpty then RunCoefficientRetentionResult.NotRequested
      else RunCoefficientRetentionResult.Retained(retainedBlocks)
    result.map(value => FirstLevelFixedEffectsEstimateBlock(chunk.ordinal, chunk.voxelIndices, value, retention))

object FirstLevelFixedEffectsEstimates:
  /** Finite-input OLS within each run, followed by full inverse-covariance
    * pooling. Preparation reads only design and axis metadata. Unsupported
    * response preparation is rejected before reading imaging data.
    */
  def prepare(
      plan: FitPlan,
      request: FirstLevelEstimateRequest,
      blockSize: ChunkSize,
      selection: DataSelection = DataSelection.All,
      solvePolicy: OlsSolvePolicy = OlsSolvePolicy.Default,
      poolingPolicy: FixedEffectsPolicy = FixedEffects.DefaultPolicy
  ): Either[FitError, FirstLevelFixedEffectsEstimatePlan] =
    for
      preparation <- FirstLevelEstimates.admittedPreparation(plan, FitEngine.FixedEffects)
      schema <- plan.model.designSchema.toRight(FitError.MissingStructuralIdentity("selected fixed-effects design"))
      chunks <- FitChunkPlan.fromSelection(plan, selection, FitChunkingStrategy.ByVoxelCount(blockSize))
      // This also checks event support against the exact retained scans.
      _ <- MatrixAdapters.designMatrix(plan.model, chunks.timepoints)
      selected <- request.compile(schema, solvePolicy.rankTolerance)
      partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, chunks.timepoints)
      slices <- partitions.foldLeft[Either[FitError, Vector[scalafim.fmri.design.RunwiseDesignSlice]]](Right(Vector.empty)) { (acc, partition) =>
        for
          prior <- acc
          slice <- schema.runwiseSlice(DesignRunIndex.unsafeOneBased(partition.runIndex + 1),
            chunks.timepoints.map(index => ScanIndex.unsafeOneBased(index + 1)))
            .left.map(error => FitError.InvalidFitAxis("fixed-effects run projection", error.message))
          _ <- if slice.sourceRowIndices == partition.timepoints then Right(())
               else Left(FitError.InvalidFitAxis("fixed-effects runs", "sampling frame and structural run rows disagree"))
        yield prior :+ slice
      }
      retainedSourceColumns = request.retainRunCoefficients.map(schema.coefficientAxis.columnIds.indexOf)
      _ <- if retainedSourceColumns.forall(column => slices.exists(_.sourceColumnIndices.contains(column))) then Right(())
           else Left(FitError.InvalidFitAxis("retained run coefficients", "a requested source coefficient is absent from every selected run"))
      shared <- FixedEffects.sharedSourceColumns(schema.coefficientAxis, slices.map(slice => Some(slice.projection)), poolingPolicy)
      pooledAxis <- schema.coefficientAxis.select(shared).left.map(error => FitError.InvalidFitAxis("pooled coefficient axis", error.message))
      _ <- selected.alignTo(pooledAxis)
      runs <- partitions.zip(slices).foldLeft[Either[FitError, Vector[PreparedPrecisionRun]]](Right(Vector.empty)) { case (acc, (partition, slice)) =>
        for
          prior <- acc
          design <- DesignMatrix.fromMatrix(MatrixAdapters.fromHrfMatrix(slice.matrix))
          readout <- CoefficientReadout.coefficients(design.predictors, shared.map(slice.sourceColumnIndices.indexOf))
          retention <- RunCoefficientRetention.describe(partition, slice, schema.coefficientAxis, retainedSourceColumns)
          prepared <- PreparedPrecisionRun.prepare(partition, slice.axis, design, readout, solvePolicy, retention)
        yield prior :+ prepared
      }
    yield new FirstLevelFixedEffectsEstimatePlan(plan, selected, pooledAxis, chunks, preparation, poolingPolicy, runs, shared)

private[fit] final case class PrecisionRunBlock(
    weighted: DMat,
    variance: DVec,
    statuses: Vector[VoxelFitStatus],
    retained: Option[RetainedRunCoefficientBlock]
)

private[fit] final class PreparedPrecisionRun(
    val preparation: FixedEffectsRunPreparation,
    qr: QR,
    weightedReadout: DMat,
    precision: DMat,
    retainedReadout: Option[(RunCoefficientRetentionDescription, DMat)]
):
  def retention: Option[RunCoefficientRetentionDescription] = retainedReadout.map(_._1)

  def evaluate(response: ResponseBlock, voxelIndices: Vector[Int])(using Backend): Either[FitError, PrecisionRunBlock] =
    val rows = preparation.partition.rowIndices
    val selected = ResponseBlock.unsafe(Matrix.tabulate(rows.length, response.voxels)((row, voxel) => response.value(rows(row), voxel)))
    OlsEstimatePlan.residualCoordinates(qr, selected, preparation.residualDegreesOfFreedom).flatMap { (leading, variance) =>
      // Both readouts reuse these coordinates; only requested run rows exist.
      val statuses = VoxelFitStatus.refine(VoxelFitStatus.classify(selected), variance)
      val retained = retainedReadout.map { (description, readout) =>
        RetainedRunCoefficientBlock(description, readout * leading, voxelIndices, statuses)
      }
      if retained.exists(block => !FixedEffectsRunContribution.allFinite(block.estimates)) then
        Left(FitError.FixedEffectsContributionFailure(preparation.partition.runIndex, -1, "non-finite retained run coefficients"))
      else Right(PrecisionRunBlock(weightedReadout * leading, variance, statuses, retained))
    }

  def contribution(
      block: PrecisionRunBlock,
      positions: Vector[Int],
      sourceColumns: Vector[Int]
  ): Either[FitError, FixedEffectsRunContribution] =
    val scales = positions.map(voxel => 1.0 / block.variance(voxel))
    CoefficientMatrixStorage.scaled(precision, scales).flatMap { field =>
      val weighted = Matrix.tabulate(precision.rows, positions.length)((row, voxel) => block.weighted(row, positions(voxel)) * scales(voxel))
      if !FixedEffectsRunContribution.allFinite(weighted) then
        Left(FitError.FixedEffectsContributionFailure(preparation.partition.runIndex, -1, "non-finite weighted selected-run coefficients"))
      else Right(FixedEffectsRunContribution(preparation.partition.runIndex, preparation.partition.rowIndices.length,
        preparation.partition.timepoints, preparation.residualDegreesOfFreedom, field, weighted, sourceColumns))
    }

private[fit] object PreparedPrecisionRun:
  def prepare(
      partition: RunPartition,
      axis: CoefficientAxis,
      design: DesignMatrix,
      readout: CoefficientReadout,
      policy: OlsSolvePolicy,
      retention: Option[RunCoefficientRetentionDescription]
  ): Either[FitError, PreparedPrecisionRun] =
    (for
      prepared <- Ols.prepareQr(design, policy)
      (qr, diagnostics) = prepared
      df <- ResidualDegreesOfFreedom(design.timepoints - diagnostics.rank)
      dual <- OlsEstimatePlan.solveDual(qr, readout.weights)
      precision <- FixedEffects.inverse(dual.t * dual, partition.runIndex, -1)
      retained <- retention match
        case None => Right(None)
        case Some(description) =>
          for
            readout <- CoefficientReadout.coefficients(design.predictors,
              description.coefficientAxis.columnIds.map(axis.columnIds.indexOf))
            dual <- OlsEstimatePlan.solveDual(qr, readout.weights)
          yield Some(description -> OlsEstimatePlan.packedReadout(dual.t))
    yield new PreparedPrecisionRun(FixedEffectsRunPreparation(partition, axis, diagnostics, df), qr,
      OlsEstimatePlan.packedReadout((dual * precision.t).t), precision, retained))
      .left.map(error => FitError.RunwiseFitFailed(partition.runIndex, FitKernel.bindRankFailure(error, axis)))
