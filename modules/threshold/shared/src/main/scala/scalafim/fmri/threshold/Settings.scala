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

opaque type DegreesOfFreedom = Double
object DegreesOfFreedom:
  def apply(value: Double): Either[ThresholdError, DegreesOfFreedom] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(ThresholdError.InvalidDegreesOfFreedom(value))

  def unsafe(value: Double): DegreesOfFreedom =
    require(value.isFinite && value > 0.0, "degrees of freedom must be finite and positive")
    value

  extension (df: DegreesOfFreedom)
    inline def value: Double = df

opaque type AdjustedP = Double
object AdjustedP:
  def apply(value: Double): Either[ThresholdError, AdjustedP] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(ThresholdError.InvalidAdjustedPValue(value))

  def unsafe(value: Double): AdjustedP =
    require(value.isFinite && value >= 0.0 && value <= 1.0, "adjusted p-value must be finite and in [0, 1]")
    value

  extension (p: AdjustedP)
    inline def value: Double = p

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

enum EvidenceOrientation:
  case Signed, Unsigned

enum ThresholdAlternative:
  case Greater, Less, TwoSided

  def compatibleWith(orientation: EvidenceOrientation): Boolean =
    orientation match
      case EvidenceOrientation.Signed =>
        true
      case EvidenceOrientation.Unsigned =>
        this == Greater

  def validate(orientation: EvidenceOrientation): Either[ThresholdError, Unit] =
    if compatibleWith(orientation) then Right(())
    else Left(ThresholdError.IncompatibleAlternative(this, orientation))

  def applyTo(value: Double): Double =
    this match
      case Greater  => value
      case Less     => -value
      case TwoSided => math.abs(value)

enum Tail:
  case Positive, Negative, TwoSided

  def alternative: ThresholdAlternative =
    this match
      case Positive => ThresholdAlternative.Greater
      case Negative => ThresholdAlternative.Less
      case TwoSided => ThresholdAlternative.TwoSided

  def applyTo(value: Double): Double =
    alternative.applyTo(value)

object Tail:
  def fromAlternative(alternative: ThresholdAlternative): Tail =
    alternative match
      case ThresholdAlternative.Greater  => Positive
      case ThresholdAlternative.Less     => Negative
      case ThresholdAlternative.TwoSided => TwoSided

enum EvidenceScore:
  case SoftMax(kappa: Kappa)
  case Diffuse
  case Omnibus(kappas: Vector[Kappa])

enum StatKind:
  case Z
  case T(df: DegreesOfFreedom)
  case NegLog10P(pSide: PSide)

  def orientation: EvidenceOrientation =
    this match
      case Z | T(_) =>
        EvidenceOrientation.Signed
      case NegLog10P(_) =>
        EvidenceOrientation.Unsigned

enum PSide:
  case OneSided, TwoSided
