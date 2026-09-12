package scalafim.fmri.hrf

enum Cascade34Error:
  case InvalidRates(kappaP: Double, kappaU: Double)
  case InvalidAreaRatio(rho: Double)

  def message: String =
    this match
      case InvalidRates(p, u) => s"Cascade34 requires finite rates kappaP > kappaU > 0 (per second), got $p and $u"
      case InvalidAreaRatio(r) => s"Cascade34 requires finite rho >= 0, got $r"

/** Ordered rates in inverse seconds and the undershoot-to-positive component
  * area ratio. Rho is neither the attained height ratio nor a signed-area
  * normalizer. Construction (including `copy`) preserves the domain.
  */
final case class Cascade34Params(kappaP: Double, kappaU: Double, rho: Double):
  require(kappaP.isFinite && kappaU.isFinite && kappaP > kappaU && kappaU > 0.0,
    Cascade34Error.InvalidRates(kappaP, kappaU).message)
  require(rho.isFinite && rho >= 0.0, Cascade34Error.InvalidAreaRatio(rho).message)

object Cascade34Params:
  def make(kappaP: Double, kappaU: Double, rho: Double): Either[Cascade34Error, Cascade34Params] =
    if !(kappaP.isFinite && kappaU.isFinite && kappaP > kappaU && kappaU > 0.0) then
      Left(Cascade34Error.InvalidRates(kappaP, kappaU))
    else if !(rho.isFinite && rho >= 0.0) then Left(Cascade34Error.InvalidAreaRatio(rho))
    else Right(Cascade34Params(kappaP, kappaU, rho))

  val Default: Cascade34Params = Cascade34Params(0.5, 0.2, 0.3)

final case class Cascade34Undershoot(latency: Seconds, height: Double, ratioToPeak: Double)

/** Attained continuous-time summaries, independent of the evaluation horizon. */
final case class Cascade34Summary(
    peakLatency: Seconds,
    peakHeight: Double,
    fwhm: Seconds,
    undershoot: Option[Cascade34Undershoot]
)

/** `h(t) = g3(t; kappaP) - rho g4(t; kappaU)`, causal unit-area Erlang
  * components. Its signed integral is `1 - rho`, including zero at rho = 1.
  * The horizon on [[toHrf]] is an evaluation horizon, not a hard tail cutoff.
  */
object Cascade34:
  private val LogTwo = math.log(2.0)
  private val LogSix = math.log(6.0)

  /** A unit-area Erlang density, with log-domain evaluation at extreme lags
    * to avoid `polynomial * exp` overflow in a vanishing tail.
    */
  private[hrf] def erlang(order: Int, rate: Double, t: Double): Double =
    if t <= 0.0 then 0.0
    else
      val x = rate * t
      if x == Double.PositiveInfinity then 0.0
      else if x >= 1e-100 && x <= 128.0 then
        val polynomial = if order == 3 then x * x / 2.0 else x * x * x / 6.0
        rate * (polynomial * math.exp(-x))
      else
        val logFactorial = if order == 3 then LogTwo else LogSix
        math.exp(math.log(rate) + (order - 1) * (math.log(rate) + math.log(t)) - x - logFactorial)

  def value(params: Cascade34Params, lag: Lag): Double =
    erlang(3, params.kappaP, lag.value) - params.rho * erlang(4, params.kappaU, lag.value)

  private def survival(order: Int, x: Double): Double =
    if x == Double.PositiveInfinity then 0.0
    else if x > 1e100 then 0.0
    else
      val polynomial = if order == 3 then 1.0 + x * (1.0 + x / 2.0)
        else 1.0 + x * (1.0 + x * (0.5 + x / 6.0))
      math.exp(-x) * polynomial

  /** Erlang CDF. The small-x positive series avoids cancellation of `1 - Q`;
    * for larger x, Q is the finite Erlang survival polynomial.
    */
  private def cdf(order: Int, x: Double): Double =
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0 - survival(order, x)
    else
      var term = if order == 3 then x * x * x / 6.0 else x * x * x * x / 24.0
      var sum = term
      var j = order + 1
      var more = true
      while more && j < 200 do
        term *= x / j
        sum += term
        more = term > 1e-16 * sum
        j += 1
      math.exp(-x) * sum

  def primitive(params: Cascade34Params, lag: Lag): Double =
    cdf(3, params.kappaP * lag.value) - params.rho * cdf(4, params.kappaU * lag.value)

  private def componentIntegral(order: Int, rate: Double, lo: Double, hi: Double): Double =
    val a = rate * math.max(0.0, lo)
    val b = rate * math.max(0.0, hi)
    // Difference survival probabilities in the tail; subtracting two CDFs
    // rounded to one would lose a nonzero box response.
    if a >= 1.0 then survival(order, a) - survival(order, b)
    else cdf(order, b) - cdf(order, a)

  def integral(params: Cascade34Params, from: Lag, to: Lag): Double =
    if to.value < from.value then -integral(params, to, from)
    else
      componentIntegral(3, params.kappaP, from.value, to.value) -
        params.rho * componentIntegral(4, params.kappaU, from.value, to.value)

  def descriptor(params: Cascade34Params, span: Seconds): HrfDescriptor =
    HrfDescriptor.scalar(HrfKind.Cascade34, span, HrfParams.Cascade34(params),
      integration = IntegrationPolicy.Cascade34(params))

  def toHrf(params: Cascade34Params = Cascade34Params.Default, span: Seconds = 32.0.s): Hrf =
    Hrf.of("cascade34", nbasis = 1, span = span, descriptor = Some(descriptor(params, span))): lag =>
      scalafim.fmri.hrf.linalg.Vec.unsafe(Array(value(params, lag)))

  /** Bracketed roots in dimensionless x = kappaP * t. No sampled maxima or
    * horizon clipping enter the summaries. For rho > 0 there is one maximum
    * before x = 2 and one minimum after x = 3 / q, q = kappaU / kappaP.
    */
  def summaries(params: Cascade34Params): Cascade34Summary =
    val q = params.kappaU / params.kappaP
    require(q > 0.0 && (3.0 / params.kappaU).isFinite,
      "Cascade34 summaries require a representable rate ratio and finite component peak times")
    val rho = params.rho
    val logWeight = math.log(rho) + 4.0 * math.log(q) - math.log(3.0)
    def h(x: Double): Double = erlang(3, 1.0, x) - rho * erlang(4, q, x)
    val peak = if rho == 0.0 then 2.0 else bisect(0.0, 2.0): x =>
      logWeight + math.log(x) + (1.0 - q) * x + math.log(3.0 - q * x) - math.log(2.0 - x) > 0.0
    val height = h(peak)
    require(height > 0.0 && height.isFinite, "Cascade34 peak height is not representable at these parameters")
    val left = bisect(0.0, peak)(x => h(x) >= height / 2.0)
    var rightBound = 2.0
    while h(rightBound) > height / 2.0 do rightBound *= 2.0
    val right = bisect(peak, rightBound)(x => h(x) <= height / 2.0)
    val undershoot =
      if rho == 0.0 then None
      else
        val start = 3.0 / q
        require(start.isFinite, "Cascade34 dimensionless trough bound is not representable")
        def rising(x: Double): Boolean =
          logWeight + math.log(x) + (1.0 - q) * x + math.log(q * x - 3.0) - math.log(x - 2.0) > 0.0
        var end = 2.0 * start
        while end.isFinite && !rising(end) do end *= 2.0
        require(end.isFinite, "Cascade34 trough bracket is not representable")
        val trough = bisect(start, end)(rising)
        val depth = -h(trough)
        Some(Cascade34Undershoot(Seconds(trough / params.kappaP), -depth * params.kappaP, depth / height))
    Cascade34Summary(Seconds(peak / params.kappaP), height * params.kappaP,
      Seconds((right - left) / params.kappaP), undershoot)

  private def bisect(lo: Double, hi: Double)(above: Double => Boolean): Double =
    var a = lo
    var b = hi
    var i = 0
    var resolved = false
    while i < 1100 && !resolved do
      val mid = a + (b - a) / 2.0
      if mid == a || mid == b then resolved = true
      else if above(mid) then b = mid else a = mid
      i += 1
    a + (b - a) / 2.0
