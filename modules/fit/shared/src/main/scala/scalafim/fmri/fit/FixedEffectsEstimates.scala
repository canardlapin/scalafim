package scalafim.fmri.fit

import gale.backend.Backend
import gale.linalg.{CholeskyOptions, DMat, Matrix}
import scalafim.fmri.design.CoefficientAxis
import scalafim.fmri.model.FitSummary

enum FixedEffectsEstimateUncertainty:
  case NotRequested
  case Marginal(standardErrors: StandardErrorBlock)
  case Joint(standardErrors: StandardErrorBlock, covariance: CoefficientCovariance)

/** Selected pooled outputs retain the scientific receipt, not the input
  * precision-weighted coefficient maps or unrequested covariance matrices.
  */
final case class FixedEffectsEstimateResult private[fit] (
    estimates: DMat,
    selection: CompiledEstimateSelection,
    pooledCoefficientAxis: CoefficientAxis,
    uncertainty: FixedEffectsEstimateUncertainty,
    voxelIndices: Vector[Int],
    runIndices: Vector[Int],
    residualDegreesOfFreedom: ResidualDegreesOfFreedom,
    policy: FixedEffectsPolicy
)

/** Selected outputs combined from a runwise fit preserve the same exclusions,
  * selected timepoints and response preparation as the full native result.
  * `summary.predictors` counts the jointly pooled model coefficients, rather
  * than the number of requested output rows.
  */
final case class FixedEffectsEstimateFit private[fit] (
    result: FixedEffectsEstimateResult,
    timepoints: Vector[Int],
    summary: FitSummary,
    preparationProvenance: Option[ResponsePreparationProvenance],
    fitExclusions: Vector[VoxelInferenceExclusion]
)

object FixedEffectsEstimates:
  /** Combine existing runwise fits without materializing all pooled coefficient
    * and uncertainty maps. This entry point shares the full fitter's admission,
    * run projection and voxel exclusion rules. The input runwise fits still
    * own their original maps; use a bounded source for bounded total memory.
    */
  def combine(
      source: RunwiseFmriFitResult,
      selection: CompiledEstimateSelection,
      policy: FixedEffectsPolicy = FixedEffects.DefaultPolicy
  )(using Backend): Either[FitError, FixedEffectsEstimateFit] =
    for
      _ <- requiresPrimaryOnly(selection)
      prepared <- FixedEffects.prepareStatistics(source, policy)
      selected <- estimate(prepared.statistics, selection)
    yield FixedEffectsEstimateFit(selected, prepared.timepoints, prepared.summary,
      prepared.preparationProvenance, prepared.fitExclusions)

  /** Apply a selection AFTER multivariate precision pooling. All jointly
    * pooled columns remain in each precision solve, even for a one-row readout.
    * No full coefficient map or inverse precision is constructed for the
    * estimates-only request. Input sufficient statistics must already contain
    * the required run variance information; suppressing output uncertainty
    * never removes that scientific requirement.
    */
  def estimate(
      statistics: FixedEffectsSufficientStatistics,
      selection: CompiledEstimateSelection
  )(using Backend): Either[FitError, FixedEffectsEstimateResult] =
    requiresPrimaryOnly(selection).flatMap(_ => estimatePrimary(statistics, selection))

  private def requiresPrimaryOnly(selection: CompiledEstimateSelection): Either[FitError, Unit] =
    if selection.request.retainRunCoefficients.isEmpty then Right(())
    else Left(FitError.UnsupportedEngine("Pooling-only results cannot retain run coefficients; use the prepared first-level selected executor"))

  /** The combined executor owns the separate retained run product. */
  private[fit] def estimatePrimary(
      statistics: FixedEffectsSufficientStatistics,
      selection: CompiledEstimateSelection
  )(using Backend): Either[FitError, FixedEffectsEstimateResult] =
    for
      readout <- selection.alignTo(statistics.coefficientAxis)
      df <- statistics.effectiveResidualDegreesOfFreedom
      result <- execute(statistics, selection, readout, df)
    yield result

  private def execute(
      statistics: FixedEffectsSufficientStatistics,
      selection: CompiledEstimateSelection,
      readout: CoefficientReadout,
      df: ResidualDegreesOfFreedom
  )(using Backend): Either[FitError, FixedEffectsEstimateResult] =
    val predictors = statistics.coefficientAxis.predictors
    val voxels = statistics.voxelIndices.length
    val fields = statistics.contributions.map(_.precisionByVoxel)
    val estimates = Matrix.newBuilder(readout.outputs, voxels)
    val errors = selection.request.uncertainty match
      case EstimateUncertaintyRequest.None => None
      case _ => Some(Matrix.newBuilder(readout.outputs, voxels))
    val covariances = selection.request.uncertainty match
      case EstimateUncertaintyRequest.Joint => Some(Vector.newBuilder[DMat])
      case _ => None
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
      val fitted = for
        factor <- precision.cholesky(CholeskyOptions(FixedEffects.tolerance(precision)))
          .left.map(error => failure(statistics, voxel, error.getMessage))
        beta <- factor.solve(rhs.result()).left.map(error => failure(statistics, voxel, error.getMessage))
        dual <- selection.request.uncertainty match
          case EstimateUncertaintyRequest.None => Right(None)
          case _ => factor.solve(readout.weights.t).left.map(error => failure(statistics, voxel, error.getMessage)).map(Some(_))
      yield (readout.weights * beta, dual)
      fitted match
        case Left(error) => return Left(error)
        case Right((selected, dual)) =>
          var row = 0
          while row < readout.outputs do
            val value = selected(row, 0)
            if !value.isFinite then return Left(failure(statistics, voxel, "non-finite pooled estimate"))
            estimates(row, voxel) = value
            row += 1
          dual match
            case None => ()
            case Some(solved) =>
              // Each diagonal is a dot product. Marginal-only requests never
              // form the full selected covariance matrix.
              val destination = errors.get
              var output = 0
              while output < readout.outputs do
                val variance = readout.weights.row(output).dot(solved.col(output))
                if variance < -1e-12 || !variance.isFinite then
                  return Left(failure(statistics, voxel, "invalid selected pooled variance"))
                destination(output, voxel) = math.sqrt(math.max(0.0, variance))
                output += 1
              covariances.foreach(_ += readout.weights * solved)
      voxel += 1
    val uncertainty = errors match
      case None => Right(FixedEffectsEstimateUncertainty.NotRequested)
      case Some(values) =>
        val matrix = values.result()
        if !FixedEffectsRunContribution.allFinite(matrix) then
          Left(FitError.FixedEffectsIncompatible("non-finite selected pooled standard errors"))
        else
          val se = StandardErrorBlock(matrix)
          covariances match
            case None => Right(FixedEffectsEstimateUncertainty.Marginal(se))
            case Some(values) => CoefficientCovariance.voxelwise(values.result()).map(cov => FixedEffectsEstimateUncertainty.Joint(se, cov))
    uncertainty.map(value => FixedEffectsEstimateResult(estimates.result(), selection, statistics.coefficientAxis, value,
      statistics.voxelIndices, statistics.runIndices, df, statistics.policy))

  private def failure(statistics: FixedEffectsSufficientStatistics, voxel: Int, detail: String): FitError =
    FitError.FixedEffectsContributionFailure(statistics.contributions.head.runIndex, voxel, detail)
