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

/** A displacement from an event onset — the argument every kernel takes.
  *
  * `Seconds` names three different quantities in this module: a reading on a
  * run clock, a width, and a displacement. A kernel accepts only the third.
  * While all three share one type, `hrf(onset)` typechecks and is silently
  * wrong — it asks what the response looks like `onset` seconds *after* the
  * event instead of at the event — and `Regressor` computing `g - onset` is
  * correct only by convention, with nothing in the types to say so.
  *
  * `Lag` is that third quantity. The only way to reach one from clock readings
  * is [[Lag.between]], which has to be told which onset it is measured from, so
  * the conversion is confined to the boundary where an event schedule meets a
  * kernel.
  *
  * Widths and horizons deliberately stay [[Seconds]]: `span`, `duration` and
  * `precision` are not displacements, and `Seconds` subtraction still yields
  * `Seconds` because subtracting two widths gives a width.
  */
opaque type Lag = Double

object Lag:
  def fromDouble(value: Double, label: String = "lag"): Either[TimeError, Lag] =
    if value.isFinite then Right(value) else Left(TimeError.NonFinite(label, value))

  def apply(value: Double): Lag =
    fromDouble(value).fold(err => throw new IllegalArgumentException(err.message), identity)

  inline def unsafe(value: Double): Lag = value

  /** The moment of the event itself. */
  val zero: Lag = 0.0

  /** The lag at which a clock reading `at` observes an event begun at `onset`.
    *
    * This is the boundary conversion: absolute time in, displacement out.
    */
  inline def between(onset: Seconds, at: Seconds): Lag = at.value - onset.value

  /** A displacement stated directly as a width or offset. */
  inline def ofSeconds(width: Seconds): Lag = width.value

  extension (l: Lag)
    inline def value: Double = l
    inline def +(o: Lag): Lag = l + o
    inline def -(o: Lag): Lag = l - o
    inline def *(k: Double): Lag = l * k

    /** This lag as seen by a kernel delayed by `amount`. */
    inline def rewound(amount: Seconds): Lag = l - amount.value

    /** This lag moved later by `amount`. */
    inline def advanced(amount: Seconds): Lag = l + amount.value

    inline def isCausal: Boolean = l >= 0.0

  given Ordering[Lag] with
    def compare(a: Lag, b: Lag): Int =
      java.lang.Double.compare(a.value, b.value)

/** Where a causal kernel is permitted to be non-zero.
  *
  * The distinction is not decoration. A Fourier, sine, FIR, B-spline, tent or
  * boxcar kernel is *defined* only on `[0, horizon]` — outside it the formula is
  * meaningless (a Fourier basis simply wraps). A gamma, SPM or Gaussian kernel
  * decays but is non-zero for every positive lag, so its `span` is a
  * computational horizon chosen for pruning, not a statement about the
  * function. Conflating the two makes `span` a lie in one direction or a
  * silent truncation in the other.
  *
  * Causality (`h(lag) = 0` for `lag < 0`) is not part of this enum: it holds
  * for every kernel and is enforced unconditionally by [[Hrf.apply]].
  */
enum Support:
  /** Zero outside `[0, horizon]` by definition. */
  case Compact(horizon: Seconds)

  /** Non-zero for all positive lags; `span` is advisory. */
  case Unbounded

  /** Whether a non-negative `lag` lies inside the support. */
  def containsNonNegative(lag: Lag): Boolean =
    this match
      case Compact(horizon) => lag.value <= horizon.value
      case Unbounded        => true

  def horizonOption: Option[Seconds] =
    this match
      case Compact(horizon) => Some(horizon)
      case Unbounded        => None

  /** Support of a kernel translated later in time by `amount`. */
  def shifted(amount: Seconds): Support =
    this match
      case Compact(horizon) => Compact(horizon + amount)
      case Unbounded        => Unbounded

  /** Support of a kernel convolved with a pulse of width `width`. */
  def widened(width: Seconds): Support =
    shifted(width)

object Support:
  /** Support of a basis formed by stacking `supports`; compact only if all are. */
  def union(supports: Seq[Support]): Support =
    if supports.isEmpty then Compact(Seconds.unsafe(0.0))
    else
      val horizons = supports.map(_.horizonOption)
      if horizons.forall(_.isDefined) then Compact(horizons.flatten.max)
      else Unbounded

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
