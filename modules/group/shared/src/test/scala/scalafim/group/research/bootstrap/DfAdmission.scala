package scalafim.group.research.bootstrap

/** What a first-level fit claims about the law of its variance estimate v_i (declaration §3). */
enum FirstLevelDfClaim:
  /** v is the true sigma^2 (nu = infinity). */
  case Known
  /** OLS contrast with iid Gaussian errors: nu v / sigma^2 ~ chi^2_nu exactly. */
  case ExactOls(nu: Int)
  /** GLS with a fixed, known, correct Sigma_T: after whitening it is OLS, same law. */
  case ExactFixedGls(nu: Int)
  /** The adapter's nominal `Estimated(Residual)` tag without an exact-model declaration. */
  case NominalResidual(nu: Int)
  /** Estimated AR / GLS fit: its nominal residual df is not a chi-square df (O3). */
  case EstimatedAutoregressive(nominalNu: Int)
  /** A moment-matched Satterthwaite df: stress only, never adoptable. */
  case Satterthwaite(nu: Double)
  case Unknown

enum DfRefusal(val message: String):
  case UnknownDf extends DfRefusal("first-level df is unknown; it is never imputed")
  case NominalTagIsNotALaw extends DfRefusal("Estimated(Residual) is a nominal tag and certifies no chi-square law")
  case EstimatedAutoregressiveDf extends DfRefusal("estimated AR/GLS nominal df is refused (owner decision O3)")
  case SatterthwaiteStressOnly extends DfRefusal("Satterthwaite df is stress-only and cannot be adopted")
  case BelowFloor(nu: Int, floor: Int) extends DfRefusal(s"nu = $nu is below the declared floor $floor (owner decision O4)")
  case NonPositive(nu: Int) extends DfRefusal(s"nu must be positive, got $nu")

/** An admitted chi-square df: +infinity (known v) or an exact-model integer df >= the floor. */
opaque type AdmittedDf = Double

object AdmittedDf:
  private[bootstrap] def unsafe(value: Double): AdmittedDf = value

  extension (df: AdmittedDf)
    def value: Double = df
    def isKnown: Boolean = df.isInfinite

object DfAdmission:
  /** Declared df floor (O4). */
  val Floor = 8

  def admit(claim: FirstLevelDfClaim): Either[DfRefusal, AdmittedDf] = claim match
    case FirstLevelDfClaim.Known => Right(AdmittedDf.unsafe(Double.PositiveInfinity))
    case FirstLevelDfClaim.ExactOls(nu) => exact(nu)
    case FirstLevelDfClaim.ExactFixedGls(nu) => exact(nu)
    case FirstLevelDfClaim.NominalResidual(_) => Left(DfRefusal.NominalTagIsNotALaw)
    case FirstLevelDfClaim.EstimatedAutoregressive(_) => Left(DfRefusal.EstimatedAutoregressiveDf)
    case FirstLevelDfClaim.Satterthwaite(_) => Left(DfRefusal.SatterthwaiteStressOnly)
    case FirstLevelDfClaim.Unknown => Left(DfRefusal.UnknownDf)

  private def exact(nu: Int): Either[DfRefusal, AdmittedDf] =
    if nu <= 0 then Left(DfRefusal.NonPositive(nu))
    else if nu < Floor then Left(DfRefusal.BelowFloor(nu, Floor))
    else Right(AdmittedDf.unsafe(nu.toDouble))

  /** Whether a cell's declared nu would be admitted as an exact-model df (research runs use simulation truth regardless). */
  def floorAdmits(nu: NuLevel): Boolean = nu match
    case NuLevel.Infinite => true
    case NuLevel.Finite(v) => v >= Floor
