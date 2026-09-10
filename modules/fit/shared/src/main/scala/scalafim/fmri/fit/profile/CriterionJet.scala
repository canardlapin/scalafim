package scalafim.fmri.fit.profile

import scalafim.fmri.model.ProfileCriterion

/** The criterion a decoder maximises, with its jet: `-E / (2 sigma2)` plus,
  * for trial random effects, `-log det K / 2`. Assembled once from the energy
  * jet and an optional determinant jet; the prior is added by the decoder.
  */
final case class CriterionJet(score: Double, gradient: Vector[Double], hessian: Vector[Double])

object CriterionJet:

  enum Error:
    case MissingDeterminant
    case NonPositiveSigma2(value: Double)

    def message: String =
      this match
        case MissingDeterminant => "TrialRandomEffectsML needs a log-determinant jet"
        case NonPositiveSigma2(value) => s"sigma2 must be finite and > 0, got $value"

  /** `logDet` is the `(value, gradient, hessian)` jet of `log det K`, or `None`. */
  def assemble(criterion: ProfileCriterion, energy: ProfileJet, logDet: Option[ProfileJet]): Either[Error, CriterionJet] =
    val sigma2 = criterion.noiseVariance
    if !(sigma2.isFinite && sigma2 > 0.0) then Left(Error.NonPositiveSigma2(sigma2))
    else if criterion.usesDeterminant && logDet.isEmpty then Left(Error.MissingDeterminant)
    else
      val scale = -0.5 / sigma2
      val det = if criterion.usesDeterminant then logDet else None
      val score = scale * energy.energy - det.map(_.energy * 0.5).getOrElse(0.0)
      val gradient = energy.gradient.indices.map(i => scale * energy.gradient(i) - det.map(_.gradient(i) * 0.5).getOrElse(0.0)).toVector
      val hessian = energy.hessian.indices.map(i => scale * energy.hessian(i) - det.map(_.hessian(i) * 0.5).getOrElse(0.0)).toVector
      Right(CriterionJet(score, gradient, hessian))
