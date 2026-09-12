package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{Cascade34, Cascade34Params, Cascade34Summary, Hrf, HrfDescriptor, HrfKind, PositiveSeconds, Seconds}

/** Continuous Cascade34 with chart `(log kappaP, logit(kappaU/kappaP), rho)`.
  * Both rates move with coordinate 0; only the slower rate moves with
  * coordinate 1. The intrinsic normalization keeps each Erlang component at
  * unit area and never divides by the full signed integral `1 - rho`.
  */
final class Cascade34Family private (val chart: ShapeChart, val horizon: PositiveSeconds) extends ParametricHrfFamily:
  def name: String = "cascade34"
  def kind: HrfKind = HrfKind.Cascade34
  override def realization: RealizationSupport = RealizationSupport.Exact(7)

  def supports(rule: NormalizationRule): Boolean =
    rule match
      case NormalizationRule.Unnormalised | NormalizationRule.PositiveComponentArea => true
      case NormalizationRule.UnitPeak | NormalizationRule.UnitIntegral | NormalizationRule.Density => false

  def libraryNormalization: NormalizationRule = NormalizationRule.PositiveComponentArea

  def parameters(point: ShapePoint): Cascade34Params =
    val p = math.exp(point(0))
    Cascade34Params(p, p * Cascade34Family.logistic(point(1)), point(2))

  def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
    val p = math.exp(point(0))
    val u = p * Cascade34Family.logistic(point(1))
    val rho = point(2)
    var i = 0
    while i < lags.length do
      out(i) = Cascade34.erlang(3, p, lags(i)) - rho * Cascade34.erlang(4, u, lags(i))
      i += 1

  def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
    val n = lags.length
    val p = math.exp(point(0))
    val q = Cascade34Family.logistic(point(1))
    val u = p * q
    val rho = point(2)
    val v = 1.0 - q
    var i = 0
    while i < n do
      val t = lags(i)
      if t <= 0.0 then
        var component = 0
        while component < jetComponents do
          out(component * n + i) = 0.0
          component += 1
      else
        val gp = Cascade34.erlang(3, p, t)
        val gu = Cascade34.erlang(4, u, t)
        // For g_n, d/d log(k) g = (n - k t) g and
        // d2/d log(k)^2 g = ((n - k t)^2 - k t) g.
        // log(ku) = a + log(logistic(b)); its b jet is (v, -q v).
        val xp = p * t
        val xu = u * t
        val ap = 3.0 - xp
        val au = 4.0 - xu
        val gpA = if gp == 0.0 then 0.0 else gp * ap
        val guA = if gu == 0.0 then 0.0 else gu * au
        val gpAA = if gp == 0.0 then 0.0 else gp * (ap * ap - xp)
        val guAA = if gu == 0.0 then 0.0 else gu * (au * au - xu)
        out(i) = gp - rho * gu
        out(n + i) = gpA - rho * guA
        out(2 * n + i) = -rho * v * guA
        out(3 * n + i) = -gu
        out(4 * n + i) = gpAA - rho * guAA
        out(5 * n + i) = -rho * v * guAA
        out(6 * n + i) = -guA
        out(7 * n + i) = -rho * (v * v * guAA - q * v * guA)
        out(8 * n + i) = -v * guA
        out(9 * n + i) = 0.0
      i += 1

  def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
    require(supports(rule), s"$name does not support ${rule.label} normalisation")
    java.util.Arrays.fill(out, 0, jetComponents, 0.0)
    out(JetLayout.Value) = 1.0

  override def unidentifiedCoordinates(point: ShapePoint): Vector[String] =
    if point(2) == 0.0 then Vector(Cascade34Family.CoordinateNames(1)) else Vector.empty

  def attainedSummaries(point: ShapePoint): Cascade34Summary = Cascade34.summaries(parameters(point))

  def summaries(point: ShapePoint): ShapeSummary =
    val s = attainedSummaries(point)
    ShapeSummary(s.peakLatency, s.fwhm, s.undershoot.map(_.ratioToPeak))

  def descriptor(point: ShapePoint): HrfDescriptor = Cascade34.descriptor(parameters(point), horizon.seconds)

  def toHrf(point: ShapePoint): Hrf = Cascade34.toHrf(parameters(point), horizon.seconds)

object Cascade34Family:
  val CoordinateNames: Vector[String] = Vector("logKappaP", "logitRateRatio", "rho")

  /** Positive rate 0.25..1 /s, slower-to-faster rate ratio 0.1..0.8,
    * undershoot area ratio 0..0.8. A modeling domain, not a calibrated prior.
    */
  val DefaultChart: ShapeChart = ShapeChart(
    ("logKappaP", math.log(0.25), math.log(1.0)),
    ("logitRateRatio", math.log(0.1 / 0.9), math.log(0.8 / 0.2)),
    ("rho", 0.0, 0.8)
  )

  private[family] def logistic(x: Double): Double =
    if x >= 0.0 then 1.0 / (1.0 + math.exp(-x))
    else
      val e = math.exp(x)
      e / (1.0 + e)

  def make(chart: ShapeChart = DefaultChart, horizon: Seconds = Seconds(48.0)): Either[FamilyError, Cascade34Family] =
    def invalid(axis: Int): Left[FamilyError, Nothing] =
      Left(FamilyError.Chart(ShapeChartError.InvalidBound(chart.names(axis), chart.lower(axis), chart.upper(axis))))
    if chart.names != CoordinateNames then Left(FamilyError.WrongCoordinates(CoordinateNames, chart.names))
    else if !(math.exp(chart.lower(0)) > 0.0 && math.exp(chart.upper(0)).isFinite) then invalid(0)
    else if !(logistic(chart.lower(1)) > 0.0 && logistic(chart.upper(1)) < 1.0) then invalid(1)
    else if !(3.0 / (math.exp(chart.lower(0)) * logistic(chart.lower(1)))).isFinite then invalid(0)
    else if chart.lower(2) < 0.0 then invalid(2)
    else
      PositiveSeconds.fromSeconds(horizon, "horizon") match
        case Left(_) => Left(FamilyError.Chart(ShapeChartError.InvalidBound("horizon", 0.0, horizon.value)))
        case Right(h) => Right(new Cascade34Family(chart, h))

  val Default: Cascade34Family = make().fold(err => throw new IllegalStateException(err.message), identity)
