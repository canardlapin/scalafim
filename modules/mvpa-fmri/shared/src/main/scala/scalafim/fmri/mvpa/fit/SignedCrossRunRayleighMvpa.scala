package scalafim.fmri.mvpa.fit



/** A finite signed Rayleigh quotient. Unlike [[multivar.family.canonical.CanonicalRoot]],
  * negative values are scientifically meaningful: they indicate that the
  * held-out contrast projection opposes the aggregate training projection.
  */
opaque type SignedCrossRunRayleigh = Double

object SignedCrossRunRayleigh:
  def apply(value: Double): Either[OneShotMvpaError, SignedCrossRunRayleigh] =
    if value.isFinite then Right(value)
    else Left(OneShotMvpaError.InvalidSignedCrossRunValue(value))

  private[fit] def unsafe(value: Double): SignedCrossRunRayleigh =
    require(value.isFinite, "signed cross-run Rayleigh statistic must be finite")
    value

  extension (statistic: SignedCrossRunRayleigh)
    inline def value: Double = statistic

enum SignedCrossRunEstimator:
  /** Learn the direction and residual regularization from training runs, then
    * evaluate the rank-one cross-run numerator without re-optimizing on the
    * held-out response.
    */
  case FrozenTrainingDirection

enum SignedCrossRunOrientation:
  /** The direction's presentation sign cancels from the quadratic cross-run
    * product; the statistic sign comes only from train/held-out agreement.
    */
  case AgreementSign

enum SignedCrossRunExchangeability:
  /** Under the null, independently flip each run's contrast score while
    * leaving residual moments unchanged.
    */
  case RunWiseContrastSignFlip

final case class SignedCrossRunReceipt(
    canonical: CanonicalFoldReceipt,
    estimator: SignedCrossRunEstimator,
    orientation: SignedCrossRunOrientation,
    exchangeability: SignedCrossRunExchangeability
)
