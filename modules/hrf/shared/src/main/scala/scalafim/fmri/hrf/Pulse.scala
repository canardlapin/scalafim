package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Vec

/** The shape of neural drive entering the system at one event onset.
  *
  * Duration lives here, in the drive, and not in the kernel. An epoch is not a
  * different HRF; it is the same HRF driven by a wider pulse. Keeping the two
  * apart is what lets a single kernel serve fixed-duration, variable-duration
  * and impulse designs without a family of near-duplicate constructors.
  *
  * The two box conventions are genuinely different models and therefore
  * different constructors rather than a boolean:
  *
  *   - [[BoxHeight]] is a unit-*height* box, `1` on `[0, d)`. Its total mass
  *     grows with duration, so a longer epoch produces a larger response.
  *   - [[BoxMass]] is a unit-*mass* box, `1/d` on `[0, d)`. Its integral is `1`
  *     regardless of duration, so a longer epoch produces a flatter, not
  *     larger, response.
  */
enum Pulse:
  case Impulse
  case BoxHeight(duration: NonNegativeSeconds)
  case BoxMass(duration: NonNegativeSeconds)

  def durationSeconds: Seconds =
    this match
      case Impulse            => Seconds.unsafe(0.0)
      case BoxHeight(d)       => d.seconds
      case BoxMass(d)         => d.seconds

  /** Total mass of the pulse: `d` for a unit-height box, `1` otherwise. */
  def mass: Double =
    this match
      case Impulse      => 1.0
      case BoxHeight(d) => d.value
      case BoxMass(_)   => 1.0

  def isImpulse: Boolean =
    this match
      case Impulse      => true
      case BoxHeight(d) => d.value <= 0.0
      case BoxMass(d)   => d.value <= 0.0

object Pulse:
  /** A unit-height box, degenerating to [[Impulse]] at zero duration. */
  def box(duration: Seconds): Either[TimeError, Pulse] =
    NonNegativeSeconds.fromSeconds(duration, "duration").map { d =>
      if d.value <= 0.0 then Impulse else BoxHeight(d)
    }

  /** A unit-mass box, degenerating to [[Impulse]] at zero duration. */
  def boxMass(duration: Seconds): Either[TimeError, Pulse] =
    NonNegativeSeconds.fromSeconds(duration, "duration").map { d =>
      if d.value <= 0.0 then Impulse else BoxMass(d)
    }

  /** R `fmrihrf`'s `summate` flag, named for what it selects. */
  def fromSummate(duration: Seconds, summate: Boolean): Either[TimeError, Pulse] =
    if summate then box(duration) else boxMass(duration)

/** Quadrature for integrating a causal kernel against a box.
  *
  * The box response is a genuine integral,
  * `(q_d * h)(l) = ∫₀^d h(l - u) du`, so it must converge as the step shrinks.
  * Summing kernel samples without weights — which is what this module used to
  * do, and what R `fmrihrf`'s regressor path still does — instead produces a
  * quantity proportional to `d / step`, so the amplitude of every epoch
  * regressor became a function of the `precision` argument.
  *
  * The rule is the trapezoid, matching R's `evaluate.HRF`. `precision` is
  * therefore a tolerance, not a scale factor.
  */
object Quadrature:

  /** Offsets into `[0, width]` and their trapezoid weights.
    *
    * The final offset is snapped to `width` so the interval is covered exactly
    * even when `width` is not a multiple of `step`; weights then sum to
    * `width`.
    */
  def boxOffsets(width: Double, step: Double): (Array[Double], Array[Double]) =
    require(step > 0.0, "quadrature step must be > 0")
    if width <= 0.0 then (Array(0.0), Array(1.0))
    else
      val n = math.floor(width / step).toInt + 1
      val base = Array.tabulate(n)(i => i * step)
      val offsets =
        if base(n - 1) < width then base :+ width else base
      val m = offsets.length
      if m == 1 then (offsets, Array(1.0))
      else
        val weights = new Array[Double](m)
        var i = 0
        while i < m do
          val left = if i == 0 then 0.0 else offsets(i) - offsets(i - 1)
          val right = if i == m - 1 then 0.0 else offsets(i + 1) - offsets(i)
          weights(i) = (left + right) / 2.0
          i += 1
        (offsets, weights)

  /** Weights normalized to sum to one — the unit-mass box. */
  def normalized(weights: Array[Double]): Array[Double] =
    var total = 0.0
    var i = 0
    while i < weights.length do
      total += weights(i)
      i += 1
    if math.abs(total) <= 1e-12 then weights
    else
      val out = new Array[Double](weights.length)
      i = 0
      while i < weights.length do
        out(i) = weights(i) / total
        i += 1
      out

/** Response of a single pulse driven through a causal kernel. */
object PulseResponse:

  /** `(q * h)(lag) = ∫_{lag-d}^{lag} h(τ) dτ`, scaled by the pulse convention.
    *
    * For [[Pulse.Impulse]] this is exactly `h(lag)` with no quadrature at all,
    * so impulse designs are unaffected by `precision`.
    *
    * Under [[Integration.Exact]] a family with a known primitive is integrated
    * in closed form, so `precision` does not enter the answer there either.
    * Everything else — and all of [[Integration.Trapezoid]] — falls back to the
    * trapezoid rule at `precision`, which is what R `evaluate.HRF` does.
    */
  def at(
      pulse: Pulse,
      kernel: Hrf,
      lag: Lag,
      precision: Seconds,
      integration: Integration = Integration.Exact
  ): Vec =
    if pulse.isImpulse then kernel(lag)
    else
      val exact =
        if integration == Integration.Exact then exactly(pulse, kernel, lag) else None
      exact.getOrElse(byTrapezoid(pulse, kernel, lag, precision))

  /** Closed-form box response, when the kernel's family provides a primitive. */
  private def exactly(pulse: Pulse, kernel: Hrf, lag: Lag): Option[Vec] =
    val width = pulse.durationSeconds.value
    Primitive.definiteIntegral(kernel, Lag.unsafe(lag.value - width), lag).map { integral =>
      // A unit-height box has mass `d`; a unit-mass box is the duration average.
      val scale =
        pulse match
          case Pulse.BoxMass(_) => if width > 0.0 then 1.0 / width else 1.0
          case _                => 1.0
      if scale == 1.0 then Vec.unsafe(integral)
      else
        var i = 0
        while i < integral.length do
          integral(i) *= scale
          i += 1
        Vec.unsafe(integral)
    }

  private def byTrapezoid(pulse: Pulse, kernel: Hrf, lag: Lag, precision: Seconds): Vec =
    val width = pulse.durationSeconds.value
    val (offsets, rawWeights) = Quadrature.boxOffsets(width, precision.value)
    val weights =
      pulse match
        case Pulse.BoxMass(_) => Quadrature.normalized(rawWeights)
        case _                => rawWeights
    val nb = kernel.nbasis
    val out = new Array[Double](nb)
    var k = 0
    while k < offsets.length do
      val w = weights(k)
      if w != 0.0 then
        val v = kernel(Lag.unsafe(lag.value - offsets(k))).data
        var j = 0
        while j < nb do
          out(j) += w * v(j)
          j += 1
      k += 1
    Vec.unsafe(out)
