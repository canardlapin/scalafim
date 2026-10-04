package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, Matrix}
import scalafim.fmri.ar.WhiteningPlan
import scalafim.fmri.model.{AutocorrelationConfig, ReducedRankSolverConfig, VoxelwiseReducedRankBootstrapConfig}

enum ReducedRankSolverStatus:
  case Converged
  case IterationLimit
  case FinalStationarityRejected

final case class ReducedRankStartDiagnostic(
    start: Int,
    status: ReducedRankSolverStatus,
    iterations: Int,
    objective: Double,
    projectedGradientResidual: Double
):
  require(start >= 0 && iterations >= 0, "solver indices must be non-negative")
  require(objective.isFinite && objective >= 0.0, "solver loss must be finite and non-negative")
  require(projectedGradientResidual.isFinite && projectedGradientResidual >= 0.0, "solver residual must be finite and non-negative")

/** Full-selection preparation evidence is shared by every decoded voxel chunk. */
final case class VoxelwiseReducedRankDiagnostics(
    requestedRank: Int,
    achievedRank: Int,
    objective: Double,
    selectedStart: Int,
    starts: Vector[ReducedRankStartDiagnostic],
    solver: ReducedRankSolverConfig,
    targetColumns: Vector[Int],
    preparedVoxelIndices: Vector[Int],
    whiteningPlans: Vector[WhiteningPlan],
    preparationFingerprint: String,
    autocorrelation: AutocorrelationConfig
):
  require(requestedRank > 0 && achievedRank >= 0 && achievedRank <= requestedRank, "invalid reduced rank")
  require(objective.isFinite && objective >= 0.0, "objective must be finite and non-negative")
  require(targetColumns.nonEmpty && targetColumns.distinct == targetColumns && targetColumns.forall(_ >= 0), "target columns must be unique and non-negative")
  require(preparedVoxelIndices.nonEmpty && preparedVoxelIndices.distinct == preparedVoxelIndices, "prepared voxels must be unique")
  require(whiteningPlans.length == preparedVoxelIndices.length, "whitening plans must match prepared voxels")
  require(starts.exists(d => d.start == selectedStart && d.status == ReducedRankSolverStatus.Converged), "selected solver start must have converged")
  require(preparationFingerprint.nonEmpty, "preparation fingerprint is required")

  val method: String = "voxelwise_gls_global_fixed_rank_projected_gradient_v1"
  val objectiveConvention: String = "sum_of_voxelwise_whitened_residual_squares_unit_voxel_weights"
  val coordinateConvention: String = "target_columns_scaled_by_pooled_residualized_norm_and_response_by_residualized_rms"
  val rankToleranceConvention: String = "gale_scale_relative_pivoted_qr_and_normalized_svd_2_maxdim_epsilon_maxsingular"
  val optimality: String = "converged_projected_gradient_candidate_not_certified_global_optimum"
  val residualVarianceConvention: String = "reduced_whitened_rss_over_unrestricted_residual_df_descriptive_not_innovation_variance"

final case class VoxelwiseReducedRankBootstrapDiagnostics(
    config: VoxelwiseReducedRankBootstrapConfig,
    donorBlocksByRun: Vector[(Int, Int)],
    excludedNoiseRows: Vector[Int],
    minimumResidualLeverage: Double,
    replicateObjectives: Vector[Double],
    refittedWhiteningReplicates: Int,
    donorTraceFingerprint: String
):
  require(donorBlocksByRun.nonEmpty && donorBlocksByRun.forall(_._2 > 0), "each run must have eligible donor blocks")
  require(minimumResidualLeverage.isFinite && minimumResidualLeverage > 0.0, "residual leverage must be positive")
  require(replicateObjectives.length == config.resampling.replicates.value && replicateObjectives.forall(v => v.isFinite && v >= 0.0), "all bootstrap replicates must succeed")
  require(refittedWhiteningReplicates >= 0 && refittedWhiteningReplicates <= replicateObjectives.length, "invalid whitening refit count")

  val method: String = "synchronized_run_local_moving_residual_blocks_hc2_v1"
  val intervalConvention: String = "pointwise_percentile_linear_interpolation_r_type_7"
  val noiseModel: String = "inverse_whitening_segment_reset_residual_approximation"
  val calibration: String = "model_conditional_approximation_no_generic_t_or_f_inference"
  val randomStream: String = "park_miller_48271_mod_2147483647_seed_zero_or_modulus_maps_to_one"

sealed trait VoxelwiseReducedRankUncertainty:
  def label: String
  def bootstrap: Option[VoxelwiseReducedRankUncertainty.Bootstrap]

object VoxelwiseReducedRankUncertainty:
  case object Unavailable extends VoxelwiseReducedRankUncertainty:
    val label: String = "estimates_only_inference_not_requested"
    val bootstrap: Option[Bootstrap] = None

  /** Covariance and intervals contain target rows only, in targetColumns order. */
  final case class Bootstrap(
      targetColumns: Vector[Int],
      covariance: CoefficientCovariance,
      standardErrors: StandardErrorBlock,
      lower: CoefficientBlock,
      upper: CoefficientBlock,
      diagnostics: VoxelwiseReducedRankBootstrapDiagnostics
  ) extends VoxelwiseReducedRankUncertainty:
    require(targetColumns.nonEmpty && targetColumns.distinct == targetColumns && targetColumns.forall(_ >= 0), "bootstrap target columns must be unique")
    require(covariance.isVoxelwise && covariance.predictors == targetColumns.length, "bootstrap covariance must contain target rows per voxel")
    require(standardErrors.predictors == targetColumns.length && standardErrors.voxels == covariance.matrixCount, "bootstrap SE shape mismatch")
    require(lower.predictors == targetColumns.length && upper.predictors == targetColumns.length && lower.voxels == covariance.matrixCount && upper.voxels == covariance.matrixCount, "bootstrap interval shape mismatch")
    require((0 until lower.predictors).forall(r => (0 until lower.voxels).forall(v => lower(r, v) <= upper(r, v))), "bootstrap lower bounds must not exceed upper bounds")
    val label: String = "voxelwise_reduced_rank_bootstrap_uncertainty"
    def bootstrap: Option[Bootstrap] = Some(this)

final case class VoxelwiseReducedRankEstimate(
    coefficients: CoefficientBlock,
    uncertainty: VoxelwiseReducedRankUncertainty,
    residualVariance: DVec,
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    diagnostics: VoxelwiseReducedRankDiagnostics
):
  require(residualVariance.length == coefficients.voxels, "residual variances must match voxels")
  require(residualVariance.toSeq.forall(v => v.isFinite && v >= 0.0), "residual variances must be finite and non-negative")
  require(diagnostics.targetColumns.forall(_ < coefficients.predictors), "target column out of bounds")
  require(uncertainty.bootstrap.forall(b => b.targetColumns == diagnostics.targetColumns && b.standardErrors.voxels == coefficients.voxels), "uncertainty must match estimate axes")

  def selectVoxelPositions(positions: Vector[Int]): Either[FitError, VoxelwiseReducedRankEstimate] =
    if positions.isEmpty || positions.distinct != positions || positions.exists(p => p < 0 || p >= coefficients.voxels) then
      Left(FitError.InvalidFitAxis("voxelwise reduced-rank selection", "requires unique in-range voxel positions"))
    else
      val selected = uncertainty match
        case VoxelwiseReducedRankUncertainty.Unavailable => Right(VoxelwiseReducedRankUncertainty.Unavailable)
        case b: VoxelwiseReducedRankUncertainty.Bootstrap =>
          b.covariance.selectVoxelPositions(positions).map { covariance =>
            b.copy(
              covariance = covariance,
              standardErrors = StandardErrorBlock(select(b.standardErrors.value, positions)),
              lower = CoefficientBlock(select(b.lower.value, positions)),
              upper = CoefficientBlock(select(b.upper.value, positions))
            )
          }
      selected.map(u => copy(
        coefficients = CoefficientBlock(select(coefficients.value, positions)),
        uncertainty = u,
        residualVariance = DVec.fromSeq(positions.map(residualVariance.apply))
      ))

  private def select(matrix: DMat, positions: Vector[Int]): DMat =
    Matrix.tabulate(matrix.rows, positions.length)((r, c) => matrix(r, positions(c)))

object VoxelwiseReducedRankEstimate:
  def merge(values: IndexedSeq[VoxelwiseReducedRankEstimate]): Either[FitError, VoxelwiseReducedRankEstimate] =
    if values.isEmpty then Left(FitError.IncompatibleFitBlocks("at least one voxelwise reduced-rank estimate is required"))
    else
      val first = values.head
      if values.exists(v => v.diagnostics != first.diagnostics || v.residualDegreesOfFreedom != first.residualDegreesOfFreedom || v.coefficients.predictors != first.coefficients.predictors) then
        Left(FitError.IncompatibleFitBlocks("voxelwise reduced-rank chunks must share the complete prepared geometry"))
      else
        val uncertainty: Either[FitError, VoxelwiseReducedRankUncertainty] = first.uncertainty match
          case VoxelwiseReducedRankUncertainty.Unavailable =>
            if values.forall(_.uncertainty == VoxelwiseReducedRankUncertainty.Unavailable) then Right(VoxelwiseReducedRankUncertainty.Unavailable)
            else Left(FitError.IncompatibleFitBlocks("reduced-rank uncertainty modes differ"))
          case b: VoxelwiseReducedRankUncertainty.Bootstrap =>
            val bs = values.flatMap(_.uncertainty.bootstrap)
            if bs.length != values.length || bs.exists(v => v.diagnostics != b.diagnostics || v.targetColumns != b.targetColumns) then
              Left(FitError.IncompatibleFitBlocks("reduced-rank bootstrap preparations differ"))
            else CoefficientCovariance.voxelwise(bs.flatMap(_.covariance.matrices).toVector).map { covariance =>
              b.copy(
                covariance = covariance,
                standardErrors = StandardErrorBlock(bind(bs.map(_.standardErrors.value))),
                lower = CoefficientBlock(bind(bs.map(_.lower.value))),
                upper = CoefficientBlock(bind(bs.map(_.upper.value)))
              )
            }
        uncertainty.map(u => first.copy(
          coefficients = CoefficientBlock(bind(values.map(_.coefficients.value))),
          uncertainty = u,
          residualVariance = DVec.fromSeq(values.flatMap(_.residualVariance.toSeq))
        ))

  private def bind(values: IndexedSeq[DMat]): DMat =
    val out = Matrix.newBuilder(values.head.rows, values.map(_.cols).sum)
    var offset = 0
    values.foreach { value =>
      var row = 0
      while row < value.rows do
        var col = 0
        while col < value.cols do
          out(row, offset + col) = value(row, col)
          col += 1
        row += 1
      offset += value.cols
    }
    out.result()
