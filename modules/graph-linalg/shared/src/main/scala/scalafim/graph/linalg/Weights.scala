package scalafim.graph.linalg

trait AdjacencyWeight[-E]:
  def weight(edge: E): Double

trait NonNegativeAdjacencyWeight[-E] extends AdjacencyWeight[E]

trait StrictlyPositiveAdjacencyWeight[-E] extends NonNegativeAdjacencyWeight[E]

enum AffinityError:
  case NonFinite(value: Double)
  case Negative(value: Double)
  case NonPositive(value: Double)

  def message: String =
    this match
      case NonFinite(value)  => s"affinity must be finite, got $value"
      case Negative(value)   => s"affinity must be non-negative, got $value"
      case NonPositive(value) => s"affinity must be strictly positive, got $value"

opaque type SignedAffinity = Double

object SignedAffinity:
  def from(value: Double): Either[AffinityError, SignedAffinity] =
    if value.isFinite then Right(value) else Left(AffinityError.NonFinite(value))

  def unsafe(value: Double): SignedAffinity =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (affinity: SignedAffinity)
    inline def value: Double = affinity

  given AdjacencyWeight[SignedAffinity] with
    def weight(edge: SignedAffinity): Double = edge.value

opaque type NonNegativeAffinity = Double

object NonNegativeAffinity:
  def from(value: Double): Either[AffinityError, NonNegativeAffinity] =
    if !value.isFinite then Left(AffinityError.NonFinite(value))
    else if value < 0.0 then Left(AffinityError.Negative(value))
    else Right(value)

  def unsafe(value: Double): NonNegativeAffinity =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (affinity: NonNegativeAffinity)
    inline def value: Double = affinity

  given NonNegativeAdjacencyWeight[NonNegativeAffinity] with
    def weight(edge: NonNegativeAffinity): Double = edge.value

opaque type PositiveAffinity = Double

object PositiveAffinity:
  def from(value: Double): Either[AffinityError, PositiveAffinity] =
    if !value.isFinite then Left(AffinityError.NonFinite(value))
    else if value <= 0.0 then Left(AffinityError.NonPositive(value))
    else Right(value)

  def unsafe(value: Double): PositiveAffinity =
    from(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (affinity: PositiveAffinity)
    inline def value: Double = affinity

  given StrictlyPositiveAdjacencyWeight[PositiveAffinity] with
    def weight(edge: PositiveAffinity): Double = edge.value
