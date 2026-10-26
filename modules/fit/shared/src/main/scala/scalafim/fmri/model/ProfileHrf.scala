package scalafim.fmri.model

enum AmplitudeStructureError:
  case NonPositiveAlpha(value: Double)

  def message: String =
    this match
      case NonPositiveAlpha(value) => s"alpha must be finite and > 0, got $value; condition-only fitting is ConditionMeans, not a limit"

/** `alpha = 1 / lambda`, the trial-deviation variance ratio; finite and positive. */
opaque type PositiveAlpha = Double

object PositiveAlpha:
  def apply(value: Double): Either[AmplitudeStructureError, PositiveAlpha] =
    if value.isFinite && value > 0.0 then Right(value) else Left(AmplitudeStructureError.NonPositiveAlpha(value))

  extension (alpha: PositiveAlpha)
    inline def value: Double = alpha
    inline def lambda: Double = 1.0 / alpha

/** How amplitudes are structured under one shared shape per voxel.
  *
  * `ConditionMeans` is the exact `alpha = 0, u = 0` specialisation: amplitudes
  * are the condition coefficients and nothing trial-sized exists.
  * `ConditionCenteredTrials` adds zero-sum trial deviations `u` within each
  * condition, penalised by `lambda = 1 / alpha`. Neither is a limit of the
  * other in the implementation; each compiles its own backend.
  */
enum AmplitudeStructure:
  case ConditionMeans
  case ConditionCenteredTrials(alpha: PositiveAlpha)

  def label: String =
    this match
      case ConditionMeans => "condition-means"
      case ConditionCenteredTrials(_) => "condition-centered-trials"

  def hasTrialDeviations: Boolean =
    this match
      case ConditionMeans => false
      case ConditionCenteredTrials(_) => true

/** The scalar criterion a shape decoder maximises. `sigma2` is frozen from an
  * independent preparation step: it sets the prior weight, so estimating it
  * from the profiled residual would feed the estimate back into itself.
  */
enum ProfileCriterion:
  /** `-E / (2 sigma2)`, the penalised profile energy; no determinant. */
  case PenalizedProfile(sigma2: Double)
  /** `-E / (2 sigma2) - log det K / 2` for constrained trial random effects. */
  case TrialRandomEffectsML(sigma2: Double)

  def noiseVariance: Double =
    this match
      case PenalizedProfile(s) => s
      case TrialRandomEffectsML(s) => s

  def usesDeterminant: Boolean =
    this match
      case PenalizedProfile(_) => false
      case TrialRandomEffectsML(_) => true
