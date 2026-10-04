package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfKind, HrfParams, Hrfs, IntegrationPolicy, PositiveSeconds, Seconds}

enum FamilyError:
  case Chart(error: ShapeChartError)
  case WrongCoordinates(expected: Vector[String], actual: Vector[String])
  case NegativeLatency(lower: Double)

  def message: String =
    this match
      case Chart(error) => error.message
      case WrongCoordinates(expected, actual) => s"expected coordinates ${expected.mkString(", ")}, got ${actual.mkString(", ")}"
      case NegativeLatency(lower) => s"the latency coordinate must be non-negative, got lower bound $lower"

/** Causal Gaussian family `h(t) = exp(-(t - tau)^2 / (2 sigma^2))`, `t >= 0`,
  * in the chart `(tau, log sigma)`.
  *
  * The unnormalised kernel has unit peak at `tau`, so [[NormalizationRule.UnitPeak]]
  * is intrinsic. [[NormalizationRule.Density]] divides by `sigma sqrt(2 pi)` and
  * reproduces the existing `Hrfs.gaussian` kernel exactly. [[NormalizationRule.UnitIntegral]]
  * needs the normal CDF at `tau / sigma` and is not admitted here.
  *
  * Each lag is evaluated directly. A multiplicative grid recurrence introduces
  * shape-dependent rounding error that can reverse the tiny energy decrease
  * of a terminal profile-Newton step; values and jets must also be independent
  * of the other lags supplied in the same call.
  */
final class GaussianFamily private (val chart: ShapeChart, val horizon: PositiveSeconds) extends ParametricHrfFamily:
  def name: String = "gaussian"
  def kind: HrfKind = HrfKind.Gaussian

  def supports(rule: NormalizationRule): Boolean =
    rule match
      case NormalizationRule.Unnormalised | NormalizationRule.UnitPeak | NormalizationRule.Density => true
      case NormalizationRule.UnitIntegral | NormalizationRule.PositiveComponentArea => false

  def libraryNormalization: NormalizationRule = NormalizationRule.Density

  private inline def tau(point: ShapePoint): Double = point(0)
  private inline def logSd(point: ShapePoint): Double = point(1)

  def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
    val n = lags.length
    val t0 = tau(point)
    val inv2 = math.exp(-2.0 * logSd(point))
    var i = 0
    while i < n do
      val t = lags(i)
      if t < 0.0 then out(i) = 0.0
      else
        val u = t - t0
        out(i) = math.exp(-0.5 * u * u * inv2)
      i += 1

  def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
    val n = lags.length
    val t0 = tau(point)
    val inv2 = math.exp(-2.0 * logSd(point))
    evalInto(lags, point, out)
    var i = 0
    while i < n do
      val h = out(i)
      val u = lags(i) - t0
      val a = u * inv2
      val a2 = u * a
      out(n + i) = h * a
      out(2 * n + i) = h * a2
      out(3 * n + i) = h * (a * a - inv2)
      out(4 * n + i) = h * (a2 * a - 2.0 * a)
      out(5 * n + i) = h * (a2 * a2 - 2.0 * a2)
      i += 1

  def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit =
    require(supports(rule), s"$name does not support ${rule.label} normalisation")
    java.util.Arrays.fill(out, 0, jetComponents, 0.0)
    rule match
      case NormalizationRule.Density =>
        // s = exp(-v) / sqrt(2 pi): d/dv s = -s, d2/dv2 s = s, no tau dependence.
        val s = math.exp(-logSd(point)) / math.sqrt(2.0 * math.Pi)
        out(JetLayout.Value) = s
        out(JetLayout.first(1)) = -s
        out(JetLayout.second(2, 1, 1)) = s
      case _ =>
        out(JetLayout.Value) = 1.0

  def summaries(point: ShapePoint): ShapeSummary =
    val sd = math.exp(logSd(point))
    ShapeSummary(Seconds(tau(point)), Seconds(2.0 * math.sqrt(2.0 * math.log(2.0)) * sd), None)

  def descriptor(point: ShapePoint): HrfDescriptor =
    val sd = math.exp(logSd(point))
    HrfDescriptor.scalar(
      HrfKind.Gaussian,
      horizon.seconds,
      HrfParams.Gaussian(tau(point), sd),
      integration = IntegrationPolicy.Gaussian(tau(point), sd)
    )

  def toHrf(point: ShapePoint): Hrf =
    Hrfs.gaussian(mean = tau(point), sd = math.exp(logSd(point)), span = horizon.seconds)

object GaussianFamily:
  val CoordinateNames: Vector[String] = Vector("tau", "logSd")

  /** `tau in [3, 8] s`, `sigma in [0.8, 3] s`. */
  val DefaultChart: ShapeChart = ShapeChart(("tau", 3.0, 8.0), ("logSd", math.log(0.8), math.log(3.0)))

  def make(chart: ShapeChart = DefaultChart, horizon: Seconds = Seconds(24.0)): Either[FamilyError, GaussianFamily] =
    if chart.names != CoordinateNames then Left(FamilyError.WrongCoordinates(CoordinateNames, chart.names))
    else if chart.lower(0) < 0.0 then Left(FamilyError.NegativeLatency(chart.lower(0)))
    else
      PositiveSeconds.fromSeconds(horizon, "horizon") match
        case Left(_) => Left(FamilyError.Chart(ShapeChartError.InvalidBound("horizon", 0.0, horizon.value)))
        case Right(h) => Right(new GaussianFamily(chart, h))

  val Default: GaussianFamily =
    make().fold(err => throw new IllegalStateException(err.message), identity)
