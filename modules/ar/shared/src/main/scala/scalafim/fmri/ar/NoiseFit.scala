package scalafim.fmri.ar

import gale.linalg.DMat

/** An estimated whitening plan together with the noise scale and shape that `fmriAR`'s plan also carries.
  *
  * `acvf` and `innovationVariance` are aligned with `plan.coefficients`: one entry for global pooling, one per run
  * for run pooling. An entry is empty (`None` for the variance) when a run has no usable autocovariance.
  *
  * @param corrections the conditioning outcome per run; a run reported as [[RunCorrection.IllConditioned]] was
  *                    estimated without the correction
  * @param biasMatrices the bias matrices behind the correction, when one was requested
  */
final case class NoiseFit(
    plan: WhiteningPlan,
    acvf: Vector[Vector[Double]],
    innovationVariance: Vector[Option[Double]],
    corrections: Vector[RunCorrection],
    biasMatrices: Option[AcvfBiasMatrices]
)

object NoiseFit:

  def estimate(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      options: ArFitOptions,
      policy: EstimationPolicy = EstimationPolicy.Raw
  ): Either[ArError, NoiseFit] =
    for
      _ <- layout.coveredSegments.validateRows(residuals.rows)
      _ <- ArEstimation.validateFinite(residuals)
      _ <- if layout.retainedRows > 0 then Right(()) else Left(ArError.NoEstimableRows)
      prepared <- ArEstimation.resolveCorrection(residuals, layout, options.order.maxRequested, policy)
      plan <- ArEstimation.fitNoisePrepared(residuals, layout, options, prepared)
      acvf <- NoiseAcvf.estimateWith(residuals, layout, options.order.maxRequested, options.pooling, prepared)
    yield
      val gamma: Vector[Vector[Double]] =
        options.pooling match
          case NoisePooling.Global => Vector(acvf.units.headOption.fold(Vector.empty[Double])(_.acvf))
          case NoisePooling.Run =>
            Vector.tabulate(layout.runCount)(run => acvf.units.find(_.runIndex.contains(run)).fold(Vector.empty[Double])(_.acvf))
      val variance = gamma.zip(plan.coefficients).map { case (g, coefficients) =>
        innovationVariance(g, coefficients.phi)
      }
      NoiseFit(
        plan,
        gamma,
        variance,
        prepared.runs,
        policy match
          case EstimationPolicy.Raw                 => None
          case EstimationPolicy.DesignCorrected(_, _) => Some(prepared.matrices)
      )

  /** `sigma2 = gamma_0 - sum_k phi_k gamma_k`, clamped to `[1e-12, gamma_0]`, mirroring
    * `.sigma2_from_gamma_phi()`. Derived from the coefficients actually stored on the plan, which differ from the
    * ones estimation produced after stationarity clamping or pooling. Absent when the autocovariance does not
    * reach lag `p`: a partial sum would overstate the innovation variance.
    */
  private[ar] def innovationVariance(gamma: Vector[Double], phi: Vector[Double]): Option[Double] =
    if gamma.isEmpty || !gamma.head.isFinite || gamma.head <= 0.0 then None
    else if phi.isEmpty then Some(gamma.head)
    else if !phi.forall(_.isFinite) then None
    else if gamma.length - 1 < phi.length then None
    else
      var variance = gamma.head
      var lag = 0
      while lag < phi.length do
        variance -= phi(lag) * gamma(lag + 1)
        lag += 1
      Some(math.min(math.max(variance, 1e-12), gamma.head))
