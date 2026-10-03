package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.dataset.{DatasetSeriesReader, FmriSeries}
import scalafim.fmri.model.{ArOptions, FitEngine, FitPlan, MissingDataPolicy, RobustPsi, ScaleScope, VolumeWeighting}

enum FitPreparationTopology:
  case DesignOnly, BlockLocal, GlobalReduction

enum FitPreparationReduction:
  case ObservationPatterns, Dvars, PooledAutocorrelation, RobustRowWeights, RobustScales, SpatialBasis

/** Ordered scientific dependencies, available before any response read.
  * Global reductions may require repeated passes or external order statistics.
  */
final case class FitPreparationRequirements(
    topology: FitPreparationTopology,
    reductions: Vector[FitPreparationReduction]
):
  require((topology == FitPreparationTopology.GlobalReduction) == reductions.nonEmpty,
    "only global preparation has reduction phases")
  require(reductions.distinct == reductions, "preparation phases must be unique")

  /** Admission for the durable executor, which never falls back to dense preparation. */
  def supportsBoundedExecution(engine: FitEngine): Boolean =
    reductions.forall {
      case FitPreparationReduction.ObservationPatterns | FitPreparationReduction.Dvars => true
      case FitPreparationReduction.PooledAutocorrelation => engine == FitEngine.GeneralizedLeastSquares
      case _ => false
    }

object FitPreparation:
  def describe(plan: FitPlan): FitPreparationRequirements =
    import FitPreparationReduction.*
    import FitPreparationTopology.*
    val discovery =
      if plan.config.missingData == MissingDataPolicy.OmitRowsPerVoxel then Vector(ObservationPatterns)
      else Vector.empty
    val weighting = plan.config.volumeWeighting match
      case VolumeWeighting.Estimated(_) => Vector(Dvars)
      case _ => Vector.empty
    val enginePhases = plan.engine match
      case FitEngine.GeneralizedLeastSquares =>
        if needsPooledAr(plan.config.autocorrelation) then Vector(PooledAutocorrelation) else Vector.empty
      case FitEngine.RobustLeastSquares =>
        val weights = if plan.config.robust.psi != RobustPsi.Disabled then Vector(RobustRowWeights)
          else if plan.config.robust.scaleScope != ScaleScope.Voxel then Vector(RobustScales)
          else Vector.empty
        val ar = if plan.config.robust.reestimateAutocorrelation && needsPooledAr(plan.config.autocorrelation)
          then Vector(PooledAutocorrelation) else Vector.empty
        weights ++ ar
      case FitEngine.ReducedRankGls =>
        val ar = if needsPooledAr(plan.config.autocorrelation) then Vector(PooledAutocorrelation) else Vector.empty
        ar :+ SpatialBasis
      case FitEngine.LatentSketch => Vector(SpatialBasis)
      case _ => Vector.empty
    val phases = discovery ++ weighting ++ enginePhases
    val topology =
      if phases.nonEmpty then GlobalReduction
      else plan.engine match
        case FitEngine.GeneralizedLeastSquares if !fixedAr(plan.config.autocorrelation) => BlockLocal
        case FitEngine.RobustLeastSquares => BlockLocal
        case _ => DesignOnly
    FitPreparationRequirements(topology, phases)

  private[fit] def fixedAr(options: ArOptions): Boolean =
    options.rho.nonEmpty || options.phi.nonEmpty

  private def needsPooledAr(options: ArOptions): Boolean =
    !fixedAr(options) && !options.voxelwise

  /** A single zero column supplies only the shape required by temporal preparation.
    * It is never read from a provider or used to estimate response-derived quantities.
    */
  private[fit] def designSeries(plan: FitPlan, timepoints: Vector[Int]): Either[FitError, FmriSeries] =
    FmriSeries.fromIntIndices(
      Matrix.tabulate(timepoints.length, 1)((_, _) => 0.0),
      Vector(0), timepoints, plan.model.dataset.shape
    ).left.map(FitChunkPlan.mapDatasetError)

  private def designInput(plan: FitPlan, timepoints: Vector[Int]): Either[FitError, FitBlockInput] =
    for
      design <- MatrixAdapters.designMatrix(plan.model, timepoints)
    yield FitBlockInput(
      design, ResponseBlock.unsafe(Matrix.tabulate(timepoints.length, 1)((_, _) => 0.0)),
      Vector(0), timepoints,
      partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, timepoints),
      coefficientAxis = plan.coefficientAxis
    )

  private[fit] def ols(
      plan: FitPlan,
      timepoints: Vector[Int],
      dvars: Option[(Vector[Double], Vector[Double])] = None
  ): Either[FitError, OlsExecutionPrepared] =
    for
      input <- designInput(plan, timepoints)
      declared = ResponsePreparationPlan.fromPlan(plan)
      resolved <- (plan.config.volumeWeighting, dvars) match
        case (VolumeWeighting.Estimated(estimator), Some((weights, metric))) =>
          ResolvedVolumeWeighting.fromDvars(estimator, input, weights, metric)
            .map(weighting => ResolvedResponsePreparation(declared, Some(weighting)))
        case (VolumeWeighting.Estimated(_), None) =>
          Left(FitError.UnsupportedVolumeWeighting("DVARS requires a completed global reduction"))
        case _ => declared.resolve(input)
      prepared <- resolved.prepare(input)
      solver <- Ols.prepare(prepared.input.design).left.map { error =>
        plan.coefficientAxis.fold(error)(axis => FitKernel.bindRankFailure(error, axis))
      }
    yield OlsExecutionPrepared(solver, resolved, input.partitions)

  /** One bounded scan, retaining O(selected timepoints) numeric reduction state.
    * Column exclusions are computed before reduction, just as in the dense path.
    */
  private[fit] def dvars(
      reader: DatasetSeriesReader,
      plan: FitPlan,
      chunks: FitChunkPlan
  ): Either[FitError, OlsExecutionPrepared] =
    val estimator = plan.config.volumeWeighting match
      case VolumeWeighting.Estimated(value) => value
      case _ => return Left(FitError.UnsupportedVolumeWeighting("DVARS reduction requires an estimator"))
    val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, chunks.timepoints)
    val sums = Array.fill(chunks.timepoints.length)(0.0)
    var count = 0
    val exclusions = Vector.newBuilder[VoxelInferenceExclusion]
    val iterator = chunks.iterator
    while iterator.hasNext do
      val chunk = iterator.next()
      val input = for
        series <- ChunkedFitExecutor.readChunk(reader, chunk)
        raw <- FitPlanExecutor.rawFitBlockInput(plan, series, partitions)
      yield raw
      input match
        case Left(FitError.AllVoxelsExcluded(values)) => exclusions ++= values
        case Left(error) => return Left(FitError.ChunkFailed(chunk.ordinal.value, error))
        case Right(raw) =>
          count += raw.response.voxels
          partitions.foreach { partition =>
            var i = 1
            while i < partition.rowIndices.length do
              if partition.timepoints(i) == partition.timepoints(i - 1) + 1 then
                val row = partition.rowIndices(i)
                val previous = partition.rowIndices(i - 1)
                var voxel = 0
                while voxel < raw.response.voxels do
                  val difference = raw.response.value(row, voxel) - raw.response.value(previous, voxel)
                  sums(row) += difference * difference
                  voxel += 1
              i += 1
          }
    if count == 0 then Left(FitError.AllVoxelsExcluded(exclusions.result()))
    else
      val metric = Array.fill(chunks.timepoints.length)(Double.NaN)
      partitions.foreach { partition =>
        var i = 1
        while i < partition.rowIndices.length do
          if partition.timepoints(i) == partition.timepoints(i - 1) + 1 then
            val row = partition.rowIndices(i)
            metric(row) = math.sqrt(sums(row) / count.toDouble)
          i += 1
      }
      DvarsVolumeWeightEstimator.weightsFromDvars(metric.toVector, partitions, estimator)
        .flatMap(value => ols(plan, chunks.timepoints, Some(value)))
