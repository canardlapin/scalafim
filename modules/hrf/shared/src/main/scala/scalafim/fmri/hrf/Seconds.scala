package scalafim.fmri.hrf

enum TimeError:
  case NonFinite(label: String, value: Double)
  case Negative(label: String, value: Double)
  case NonPositive(label: String, value: Double)

  def message: String =
    this match
      case NonFinite(label, value)  => s"$label must be finite, got $value"
      case Negative(label, value)   => s"$label must be non-negative, got $value"
      case NonPositive(label, value) => s"$label must be > 0, got $value"

opaque type Seconds = Double

object Seconds:
  def fromDouble(value: Double, label: String = "seconds"): Either[TimeError, Seconds] =
    if value.isFinite then Right(value) else Left(TimeError.NonFinite(label, value))

  def apply(value: Double): Seconds =
    fromDouble(value).fold(err => throw new IllegalArgumentException(err.message), identity)

  inline def unsafe(value: Double): Seconds = value

  extension (s: Seconds)
    inline def value: Double = s
    inline def +(o: Seconds): Seconds = s + o
    inline def -(o: Seconds): Seconds = s - o
    inline def *(k: Double): Seconds = s * k
    inline def /(k: Double): Seconds = s / k

  given Ordering[Seconds] with
    def compare(a: Seconds, b: Seconds): Int =
      java.lang.Double.compare(a.value, b.value)

extension (d: Double)
  inline def s: Seconds = Seconds(d)
  inline def value: Double = d

extension (i: Int)
  inline def s: Seconds = Seconds(i.toDouble)

extension (l: Long)
  inline def s: Seconds = Seconds(l.toDouble)

opaque type NonNegativeSeconds = Double

object NonNegativeSeconds:
  def apply(value: Double, label: String = "seconds"): Either[TimeError, NonNegativeSeconds] =
    Seconds.fromDouble(value, label).flatMap(fromSeconds(_, label))

  def fromSeconds(value: Seconds, label: String = "seconds"): Either[TimeError, NonNegativeSeconds] =
    if value.value >= 0.0 then Right(value.value) else Left(TimeError.Negative(label, value.value))

  inline def unsafe(value: Seconds): NonNegativeSeconds = value.value

  extension (s: NonNegativeSeconds)
    inline def value: Double = s
    inline def seconds: Seconds = Seconds.unsafe(s)

opaque type PositiveSeconds = Double

object PositiveSeconds:
  def apply(value: Double, label: String = "seconds"): Either[TimeError, PositiveSeconds] =
    Seconds.fromDouble(value, label).flatMap(fromSeconds(_, label))

  def fromSeconds(value: Seconds, label: String = "seconds"): Either[TimeError, PositiveSeconds] =
    if value.value > 0.0 then Right(value.value) else Left(TimeError.NonPositive(label, value.value))

  inline def unsafe(value: Seconds): PositiveSeconds = value.value

  extension (s: PositiveSeconds)
    inline def value: Double = s
    inline def seconds: Seconds = Seconds.unsafe(s)
