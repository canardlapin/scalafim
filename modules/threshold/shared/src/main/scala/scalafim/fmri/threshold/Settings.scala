package scalafim.fmri.threshold

opaque type Alpha = Double
object Alpha:
  def apply(value: Double): Either[ThresholdError, Alpha] =
    if value.isFinite && value > 0.0 && value < 1.0 then Right(value)
    else Left(ThresholdError.InvalidAlpha(value))

  def unsafe(value: Double): Alpha =
    require(value.isFinite && value > 0.0 && value < 1.0, "alpha must be finite and in (0, 1)")
    value

  extension (alpha: Alpha)
    inline def value: Double = alpha

opaque type QValue = Double
object QValue:
  def apply(value: Double): Either[ThresholdError, QValue] =
    if value.isFinite && value > 0.0 && value < 1.0 then Right(value)
    else Left(ThresholdError.InvalidQValue(value))

  def unsafe(value: Double): QValue =
    require(value.isFinite && value > 0.0 && value < 1.0, "q must be finite and in (0, 1)")
    value

  extension (q: QValue)
    inline def value: Double = q

opaque type Kappa = Double
object Kappa:
  def apply(value: Double): Either[ThresholdError, Kappa] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ThresholdError.InvalidKappa(value))

  def unsafe(value: Double): Kappa =
    require(value.isFinite && value > 0.0, "kappa must be finite and positive")
    value

  extension (kappa: Kappa)
    inline def value: Double = kappa

opaque type PermutationCount = Int
object PermutationCount:
  def apply(value: Int): Either[ThresholdError, PermutationCount] =
    if value > 0 then Right(value)
    else Left(ThresholdError.InvalidPermutationCount(value))

  def unsafe(value: Int): PermutationCount =
    require(value > 0, "permutation count must be positive")
    value

  extension (n: PermutationCount)
    inline def value: Int = n

enum Tail:
  case Positive, Negative, TwoSided

  def applyTo(value: Double): Double =
    this match
      case Positive => value
      case Negative => -value
      case TwoSided => math.abs(value)

enum EvidenceScore:
  case SoftMax(kappa: Kappa)
  case Diffuse
  case Omnibus(kappas: Vector[Kappa])

enum StatKind:
  case Z
  case T(df: Int)
  case NegLog10P(pSide: PSide)

enum PSide:
  case OneSided, TwoSided
