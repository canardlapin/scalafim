package scalafim.fmri.fit

import gale.backend.Backend
import scalafim.dataset.{DataSelection, DatasetSeriesReader}
import scalafim.fmri.design.CoefficientAxis
import scalafim.fmri.model.{FitEngine, FitPlan}

/** Scientific execution topology, independent of output retention. */
enum EstimateTopology:
  case SharedOrdinaryLeastSquares, RunwiseInverseCovarianceFixedEffects

enum EstimateComputation:
  case SelectedTimeReadout, ResponseQrCoordinates, ResidualVariance
  case JointRunPrecision, VoxelwisePrecisionPooling
  case SelectedMarginalVariance, SelectedJointCovariance, RunCoefficientReadout

enum EstimateProduct:
  case Estimates, StandardErrors, JointCovariance, ResidualVariance
  case FittedSeries, ResidualSeries, RunCoefficients

/** Disposition in a prepared plan, not proof of successful execution. */
enum EstimateProductDisposition:
  case Retained, InternalOnly, NotRequested
  case Unavailable(reason: String)

/** Describes actual compiled work. Source revisions and backend-owned buffers
  * remain caller responsibilities. This is not a cache key or a peak-memory
  * estimate: reuse the prepared object only with its verified input binding.
  */
final case class EstimateExecutionDescription private[fit] (
    topology: EstimateTopology,
    computations: Set[EstimateComputation],
    products: Map[EstimateProduct, EstimateProductDisposition],
    outputs: Vector[EstimateOutputMetadata],
    fittedAxes: Vector[CoefficientAxis],
    diagnostics: Vector[OlsDiagnostics],
    timepoints: Vector[Int],
    inputVoxelCount: Int,
    maximumVoxelsPerRead: Int,
    preparation: ResponsePreparationProvenance,
    retainedRunCoefficients: Vector[RunCoefficientRetentionDescription]
)

/** Each case retains the native result, covariance interpretation and exclusion
  * evidence. No full-fit object or absent-product placeholder is constructed.
  */
enum SelectedEstimateBlock:
  case Shared(block: FirstLevelEstimateBlock)
  case Pooled(block: FirstLevelFixedEffectsEstimateBlock)

  def chunkOrdinal: ChunkOrdinal = this match
    case Shared(block) => block.ordinal
    case Pooled(block) => block.ordinal

  def inputVoxelIndices: Vector[Int] = this match
    case Shared(block) => block.voxelIndices
    case Pooled(block) => block.inputVoxelIndices

/** One public entry point for supported scientific routes. Both preparation
  * and execution delegate to the same native implementations exposed directly.
  */
enum PreparedSelectedEstimates:
  case Shared(plan: FirstLevelEstimatePlan)
  case Pooled(plan: FirstLevelFixedEffectsEstimatePlan)

  def fitPlan: FitPlan = this match
    case Shared(plan) => plan.fitPlan
    case Pooled(plan) => plan.fitPlan

  def request: FirstLevelEstimateRequest = this match
    case Shared(plan) => plan.request
    case Pooled(plan) => plan.selection.request

  def chunks: FitChunkPlan = this match
    case Shared(plan) => plan.chunks
    case Pooled(plan) => plan.chunks

  lazy val description: EstimateExecutionDescription =
    val uncertainty = request.uncertainty
    val (topology, computations, outputs, axes, diagnostics, preparation) = this match
      case Shared(plan) =>
        val work = if uncertainty == EstimateUncertaintyRequest.None then Set(EstimateComputation.SelectedTimeReadout)
          else Set(EstimateComputation.ResponseQrCoordinates, EstimateComputation.ResidualVariance)
        (EstimateTopology.SharedOrdinaryLeastSquares, work, plan.outputs, Vector(plan.coefficientAxis), Vector(plan.diagnostics), plan.preparation)
      case Pooled(plan) =>
        (EstimateTopology.RunwiseInverseCovarianceFixedEffects,
          Set(EstimateComputation.ResponseQrCoordinates, EstimateComputation.ResidualVariance,
            EstimateComputation.JointRunPrecision, EstimateComputation.VoxelwisePrecisionPooling),
          plan.selection.outputs, plan.runPreparations.map(_.coefficientAxis), plan.runPreparations.map(_.diagnostics), plan.preparation)
    val optionalWork = uncertainty match
      case EstimateUncertaintyRequest.None => Set.empty[EstimateComputation]
      case EstimateUncertaintyRequest.Marginal => Set(EstimateComputation.SelectedMarginalVariance)
      case EstimateUncertaintyRequest.Joint => Set(EstimateComputation.SelectedMarginalVariance, EstimateComputation.SelectedJointCovariance)
    def retained(yes: Boolean): EstimateProductDisposition =
      if yes then EstimateProductDisposition.Retained else EstimateProductDisposition.NotRequested
    val variance = this match
      case Shared(_) => retained(uncertainty != EstimateUncertaintyRequest.None)
      case Pooled(_) => EstimateProductDisposition.InternalOnly
    val runRetention = this match
      case Shared(_) => Vector.empty
      case Pooled(plan) => plan.retainedRunCoefficients
    val retentionWork = if runRetention.isEmpty then Set.empty[EstimateComputation]
      else Set(EstimateComputation.RunCoefficientReadout)
    val retentionProduct = this match
      case Shared(_) => EstimateProductDisposition.Unavailable("Shared OLS does not produce independently fitted run coefficients")
      case Pooled(_) => retained(runRetention.nonEmpty)
    val trace = EstimateProductDisposition.Unavailable("This executor does not retain fitted or residual series; use a separately verified bounded inspection request")
    EstimateExecutionDescription(topology, computations ++ optionalWork ++ retentionWork, Map(
      EstimateProduct.Estimates -> EstimateProductDisposition.Retained,
      EstimateProduct.StandardErrors -> retained(uncertainty != EstimateUncertaintyRequest.None),
      EstimateProduct.JointCovariance -> retained(uncertainty == EstimateUncertaintyRequest.Joint),
      EstimateProduct.ResidualVariance -> variance,
      EstimateProduct.FittedSeries -> trace, EstimateProduct.ResidualSeries -> trace,
      EstimateProduct.RunCoefficients -> retentionProduct),
      outputs, axes, diagnostics, chunks.timepoints,
      chunks.iterator.map(_.voxelIndices.length).sum, chunks.iterator.map(_.voxelIndices.length).max, preparation, runRetention)

  /** Checks cancellation before each read and delivery. The caller owns reader,
    * backend and sink lifetimes, and must discard partial output on failure.
    */
  def foreachBlock(
      reader: DatasetSeriesReader,
      consume: SelectedEstimateBlock => Either[FitError, Unit],
      cancelled: () => Boolean = () => false
  )(using Backend): Either[FitError, EstimateExecutionOutcome] = this match
    case Shared(plan) => plan.foreachBlock(reader, block => consume(SelectedEstimateBlock.Shared(block)), cancelled)
    case Pooled(plan) => plan.foreachBlock(reader, block => consume(SelectedEstimateBlock.Pooled(block)), cancelled)

object SelectedEstimates:
  /** Does not read responses, choose a different noise model, or change run
    * combination. Unsupported engines fail with the original typed fit error.
    */
  def prepare(
      plan: FitPlan,
      request: FirstLevelEstimateRequest,
      blockSize: ChunkSize,
      selection: DataSelection = DataSelection.All,
      solvePolicy: OlsSolvePolicy = OlsSolvePolicy.Default,
      poolingPolicy: FixedEffectsPolicy = FixedEffects.DefaultPolicy
  ): Either[FitError, PreparedSelectedEstimates] = plan.engine match
    case FitEngine.OrdinaryLeastSquares =>
      FirstLevelEstimates.prepare(plan, request, blockSize, selection, solvePolicy).map(PreparedSelectedEstimates.Shared.apply)
    case FitEngine.FixedEffects =>
      FirstLevelFixedEffectsEstimates.prepare(plan, request, blockSize, selection, solvePolicy, poolingPolicy).map(PreparedSelectedEstimates.Pooled.apply)
    case other => Left(FitError.UnsupportedEngine(s"$other: compiled selected estimates support shared OLS or runwise inverse-covariance fixed effects"))
