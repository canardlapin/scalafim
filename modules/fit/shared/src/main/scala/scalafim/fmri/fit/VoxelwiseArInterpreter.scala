package scalafim.fmri.fit

import scalafim.dataset.FmriSeries
import scalafim.fmri.ar.TimeSegment
import scalafim.fmri.model.{ArBiasCorrection, ArCensorTreatment, ArOptions, FitEngine, FitPlan}

/** The plan route deliberately retains the complete time axis. Censor indicators
  * integrate out missing observations under continuous covariance; arbitrary
  * selected-row gaps must not be silently interpreted as adjacent observations.
  */
private[fit] object VoxelwiseArInterpreter extends FitInterpreter:
  type Prepared = VoxelwiseArSpec
  type Block = VoxelwiseArFitBlockResult
  type Result = VoxelwiseArFmriFitResult

  val engine: FitEngine = FitEngine.GeneralizedLeastSquares

  def prepare(plan: FitPlan, series: FmriSeries): Either[FitError, Prepared] =
    prepareSpec(plan, series.timepoints)

  override private[fit] def prepareFromTimepoints(plan: FitPlan, timepoints: Vector[Int]): Option[Either[FitError, Prepared]] =
    Some(prepareSpec(plan, timepoints))

  private def prepareSpec(plan: FitPlan, timepoints: Vector[Int]): Either[FitError, Prepared] =
    val ar = plan.strategy.arConfig.map(_.toLegacy).getOrElse(ArOptions())
    if timepoints != (0 until plan.nTimepoints).toVector then
      Left(FitError.UnsupportedAutocorrelation("voxelwise FGLS requires the complete model time axis; use explicit censoredTimepoints"))
    else (ar.structure, ar.fgls, ar.biasCorrection) match
      case (scalafim.fmri.model.ArStructure.Ar(order), Some(policy), ArBiasCorrection.OlsDesign(ceiling)) =>
        val censor = ar.censorTreatment match
          case ArCensorTreatment.ContinuousMissing => Right(CensorContinuity.ContinuousMissing)
          case ArCensorTreatment.RestartWhitening => Right(CensorContinuity.RestartAfterCensor)
          case ArCensorTreatment.EstimateOnly => Left(FitError.UnsupportedAutocorrelation("EstimateOnly is not missing-observation FGLS"))
        censor.flatMap(VoxelwiseArSpec.make(order, VoxelArCoefficients.Estimated(policy.scope, ceiling.value), _, policy.uncertainty))
      case _ => Left(FitError.UnsupportedAutocorrelation("missing explicit voxelwise FGLS policy"))

  def fit(plan: FitPlan, series: FmriSeries): Either[FitError, Result] =
    for
      prepared <- prepare(plan, series)
      block <- fitChunk(plan, series, prepared)
      result <- merge(plan, Vector(block))
    yield result

  def fitChunk(plan: FitPlan, series: FmriSeries, prepared: Prepared): Either[FitError, Block] =
    val partitions = RunPartition.fromSamplingFrame(plan.model.dataset.samplingFrame, series.timepoints)
    val ar = plan.strategy.arConfig.map(_.toLegacy).getOrElse(ArOptions())
    for
      expected <- prepare(plan, series)
      _ <- if expected == prepared then Right(()) else Left(FitError.IncompatibleFitBlocks("FGLS preparation differs from plan"))
      input <- FitPlanExecutor.fitBlockInput(plan, series, partitions = partitions)
      runs = partitions.map(p => TimeSegment(p.rowIndices.head, p.rowIndices.last + 1, p.runIndex))
      fitted <- VoxelwiseArFgls.fit(input.design.value, input.response.value, runs, ar.censoredTimepoints, prepared)
      provenance = input.preparationProvenance.map(p => p.copy(records = p.records.map:
        case ResponsePreparationRecord(step: ResponsePreparationStep.Whitening, _) =>
          ResponsePreparationRecord(step, ResponsePreparationDisposition.Applied("voxelwise FGLS with explicit covariance inference policy"))
        case ResponsePreparationRecord(step: ResponsePreparationStep.Censoring, _) if ar.censoredTimepoints.nonEmpty =>
          ResponsePreparationRecord(step, ResponsePreparationDisposition.Applied(s"voxelwise FGLS censor treatment: ${ar.censorTreatment}"))
        case record => record
      ))
    yield VoxelwiseArFitBlockResult(fitted, input.voxelIndices, input.timepoints, input.coefficientAxis, provenance, input.fitExclusions)

  def merge(plan: FitPlan, chunks: IndexedSeq[Block]): Either[FitError, Result] =
    if chunks.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one voxelwise FGLS block is required"))
    else
      val first = chunks.head
      val indices = chunks.iterator.flatMap(_.voxelIndices).toVector
      if indices.distinct != indices || chunks.exists(b => b.fit.spec != first.fit.spec || b.timepoints != first.timepoints ||
          b.coefficientAxis != first.coefficientAxis || b.preparationProvenance != first.preparationProvenance) then
        Left(FitError.IncompatibleFitBlocks("voxelwise FGLS blocks must share policy, time and coefficient axes, and preparation, with disjoint voxels"))
      else VoxelInferenceExclusions.combine(chunks.iterator.flatMap(_.fitExclusions)).map: exclusions =>
        VoxelwiseArFmriFitResult(
          VoxelwiseArFit(first.fit.spec, chunks.iterator.flatMap(_.fit.voxels).toVector),
          plan.model.columnNames, indices, first.timepoints, plan.summary,
          first.coefficientAxis, first.preparationProvenance, exclusions
        )

  protected def collect(chunks: IndexedSeq[FitBlockResult]): Either[FitError, IndexedSeq[Block]] =
    val values = chunks.collect { case value: VoxelwiseArFitBlockResult => value }
    if values.length == chunks.length then Right(values)
    else Left(FitError.IncompatibleFitBlocks("expected voxelwise FGLS blocks"))
