package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfFunctions, HrfKind, HrfParams, Hrfs, LwuParams, PositiveSeconds, Seconds}

/** Causal Lag-Width-Undershoot family
  * `h(t) = exp(-(t - tau)^2 / (2 sigma^2)) - rho exp(-(t - tau - 2 sigma)^2 / (2 (1.6 sigma)^2))`,
  * `t >= 0`, in the chart `(tau, log sigma, rho)` with one shared `rho`.
  *
  * `tau` is the positive component's centre, not the attained peak; `sigma`
  * is not the FWHM; `rho` is the component amplitude ratio, not the attained
  * trough-to-peak ratio. [[summaries]] therefore report attained values by
  * a fine evaluation. The raw formula is the library convention
  * (`HrfFunctions.lwu` with `LwuNormalize.None`), so
  * [[NormalizationRule.Unnormalised]] is the library normalisation; a grid
  * peak normalisation is nonsmooth and is not admitted.
  */
final class LwuFamily private (val chart: ShapeChart, val horizon: PositiveSeconds) extends ParametricHrfFamily:
  def name: String = "lwu"
  def kind: HrfKind = HrfKind.Lwu

  def supports(rule: NormalizationRule): Boolean =
    rule == NormalizationRule.Unnormalised

  def libraryNormalization: NormalizationRule = NormalizationRule.Unnormalised

  private val K = 1.6

  def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
    val n = lags.length
    val tau = point(0)
    val s = math.exp(point(1))
    val rho = point(2)
    val inv1 = 1.0 / (s * s)
    val inv2 = 1.0 / (K * K * s * s)
    var i = 0
    while i < n do
      val t = lags(i)
      if t < 0.0 then out(i) = 0.0
      else
        val u = t - tau
        val w = u - 2.0 * s
        out(i) = math.exp(-0.5 * u * u * inv1) - rho * math.exp(-0.5 * w * w * inv2)
      i += 1

  def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
    val n = lags.length
    val d = 3
    val tau = point(0)
    val s = math.exp(point(1))
    val rho = point(2)
    val inv1 = 1.0 / (s * s)
    val ks = K * s
    val iv = JetLayout.Value
    val it = JetLayout.first(0)
    val ivv = JetLayout.first(1)
    val ir = JetLayout.first(2)
    val itt = JetLayout.second(d, 0, 0)
    val itv = JetLayout.second(d, 0, 1)
    val itr = JetLayout.second(d, 0, 2)
    val ivv2 = JetLayout.second(d, 1, 1)
    val ivr = JetLayout.second(d, 1, 2)
    val irr = JetLayout.second(d, 2, 2)
    var i = 0
    while i < n do
      val t = lags(i)
      if t < 0.0 then
        var comp = 0
        while comp < 10 do
          out(comp * n + i) = 0.0
          comp += 1
      else
        val u = t - tau
        // positive Gaussian and its (tau, v) jet
        val g1 = math.exp(-0.5 * u * u * inv1)
        val a = u * inv1
        val a2 = u * a
        val g1t = g1 * a
        val g1v = g1 * a2
        val g1tt = g1 * (a * a - inv1)
        val g1tv = g1 * (a2 * a - 2.0 * a)
        val g1vv = g1 * (a2 * a2 - 2.0 * a2)
        // undershoot Gaussian in q = (u - 2 s) / (k s): q_tau = -1/(k s), q_v = -u/(k s)
        val q = (u - 2.0 * s) / ks
        val g2 = math.exp(-0.5 * q * q)
        val qt = -1.0 / ks
        val qv = -u / ks
        val qtv = 1.0 / ks
        val qvv = u / ks
        val g2t = -g2 * q * qt
        val g2v = -g2 * q * qv
        val g2tt = g2 * (q * q - 1.0) * qt * qt
        val g2tv = g2 * ((q * q - 1.0) * qt * qv - q * qtv)
        val g2vv = g2 * ((q * q - 1.0) * qv * qv - q * qvv)
        out(iv * n + i) = g1 - rho * g2
        out(it * n + i) = g1t - rho * g2t
        out(ivv * n + i) = g1v - rho * g2v
        out(ir * n + i) = -g2
        out(itt * n + i) = g1tt - rho * g2tt
        out(itv * n + i) = g1tv - rho * g2tv
        out(itr * n + i) = -g2t
        out(ivv2 * n + i) = g1vv - rho * g2vv
        out(ivr * n + i) = -g2v
        out(irr * n + i) = 0.0
      i += 1

  def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
    require(supports(rule), s"$name does not support ${rule.label} normalisation")
    java.util.Arrays.fill(out, 0, jetComponents, 0.0)
    out(JetLayout.Value) = 1.0

  /** Attained peak, FWHM of the positive lobe and trough-to-peak ratio on a 0.01 s grid. */
  def summaries(point: ShapePoint): ShapeSummary =
    val dt = 0.01
    val n = math.floor(horizon.value / dt).toInt + 1
    val lags = Array.tabulate(n)(i => i * dt)
    val values = new Array[Double](n)
    evalInto(lags, point, values)
    var peak = 0
    var trough = 0
    var i = 1
    while i < n do
      if values(i) > values(peak) then peak = i
      if values(i) < values(trough) then trough = i
      i += 1
    val half = 0.5 * values(peak)
    var left = peak
    while left > 0 && values(left) > half do left -= 1
    var right = peak
    while right < n - 1 && values(right) > half do right += 1
    val undershoot = if values(peak) > 0.0 && values(trough) < 0.0 then Some(-values(trough) / values(peak)) else None
    ShapeSummary(Seconds(lags(peak)), Seconds(lags(right) - lags(left)), undershoot)

  def descriptor(point: ShapePoint): HrfDescriptor =
    HrfDescriptor.scalar(
      HrfKind.Lwu,
      horizon.seconds,
      HrfParams.Lwu(LwuParams(point(0), math.exp(point(1)), point(2)), HrfFunctions.LwuNormalize.None)
    )

  def toHrf(point: ShapePoint): Hrf =
    Hrfs.lwu(tau = point(0), sigma = math.exp(point(1)), rho = point(2), normalize = HrfFunctions.LwuNormalize.None, span = horizon.seconds)

object LwuFamily:
  val CoordinateNames: Vector[String] = Vector("tau", "logSd", "rho")

  /** `tau in [3, 8] s`, `sigma in [0.8, 3] s`, `rho in [0, 0.8]`. */
  val DefaultChart: ShapeChart = ShapeChart(("tau", 3.0, 8.0), ("logSd", math.log(0.8), math.log(3.0)), ("rho", 0.0, 0.8))

  def make(chart: ShapeChart = DefaultChart, horizon: Seconds = Seconds(32.0)): Either[FamilyError, LwuFamily] =
    if chart.names != CoordinateNames then Left(FamilyError.WrongCoordinates(CoordinateNames, chart.names))
    else if chart.lower(0) < 0.0 then Left(FamilyError.NegativeLatency(chart.lower(0)))
    else if chart.lower(2) < 0.0 then Left(FamilyError.Chart(ShapeChartError.InvalidBound("rho", chart.lower(2), chart.upper(2))))
    else
      PositiveSeconds.fromSeconds(horizon, "horizon") match
        case Left(_) => Left(FamilyError.Chart(ShapeChartError.InvalidBound("horizon", 0.0, horizon.value)))
        case Right(h) => Right(new LwuFamily(chart, h))

  val Default: LwuFamily =
    make().fold(err => throw new IllegalStateException(err.message), identity)
