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

/** How a null draw set relates to the observed labelling.
  *
  * `MonteCarlo`: B null actions sampled at random, the observed (identity)
  * action not among them. The observed statistic is counted once more:
  * p = (1 + #{null >= t}) / (B + 1).
  *
  * `ExactEnumeration`: the complete set of B equally likely null actions,
  * including the identity action, so the observed statistic is already among
  * the draws: p = #{null >= t} / B. Adding one again would double-count it.
  * Matrix procedures, which see every null row, require a row equal to the
  * oriented observed statistics. Max-null procedures see only per-draw maxima
  * and can check only the necessary condition that some maximum reaches each
  * observed statistic; declaring a Monte Carlo sample as exact there is the
  * caller's error and yields p-values that are too small.
  */
enum NullReference:
  case MonteCarlo, ExactEnumeration

  /** Smallest exceedance count a valid draw set can produce for an observed
    * statistic.
    */
  private[threshold] def minimumCount: Int =
    this match
      case MonteCarlo       => 0
      case ExactEnumeration => 1

  private[threshold] def pValue(count: Int, draws: Int): Double =
    this match
      case MonteCarlo       => (count.toDouble + 1.0) / (draws.toDouble + 1.0)
      case ExactEnumeration => count.toDouble / draws.toDouble

/** What the values of a statistic map are.
  *
  * Every admitted procedure (`MaxT`, `WestfallYoung`, `HierScan`) is a
  * permutation procedure: it judges the observed map only against null draws
  * of the same statistic, oriented by one `ThresholdAlternative`. The kind
  * therefore decides only the evidence orientation, which fixes the
  * admissible alternatives. It carries no degrees of freedom and no p-value
  * sidedness because nothing here converts between scales: a t map is
  * thresholded as t values, and the sidedness of the p-values behind a
  * `NegLog10P` map is part of the caller's hypothesis, which the null draws
  * must share.
  */
enum StatKind:
  case Z, T, NegLog10P

  def orientation: EvidenceOrientation =
    this match
      case Z | T =>
        EvidenceOrientation.Signed
      case NegLog10P =>
        EvidenceOrientation.Unsigned
