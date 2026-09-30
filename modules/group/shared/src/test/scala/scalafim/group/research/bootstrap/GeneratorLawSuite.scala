package scalafim.group.research.bootstrap

import scalafim.fmri.group.Distributions
import ResearchTestSupport.*

/** Laws of the Model-J generator (declaration §1 and §5), on harness seeds only.
  * Tolerances are pre-set: means at 4 standard errors, KS at the 0.999 Kolmogorov
  * quantile 1.9495/sqrt(N).
  */
class GeneratorLawSuite extends munit.FunSuite:
  private def lane(tag: Int): SplitMix64 =
    StreamKey(Phase.Harness.root(StreamKind.FirstLevel), CellId.unsafe("C-n8-DI-Vflat-T0-N8"), 900 + tag, StreamKind.FirstLevel)
      .lane(Lane.FirstLevelSeries)

  private def ks(samples: Array[Double], cdf: Double => Double): Double =
    val sorted = samples.sorted
    val n = sorted.length.toDouble
    var d = 0.0
    var i = 0
    while i < sorted.length do
      val f = cdf(sorted(i))
      d = math.max(d, math.max(math.abs((i + 1) / n - f), math.abs(f - i / n)))
      i += 1
    d

  private def normalCdf(x: Double): Double = 0.5 * Distributions.erfc(-x / math.sqrt(2.0))

  /** Chi-square CDF for even df: 1 - exp(-x/2) sum_{j < df/2} (x/2)^j / j!. */
  private def chiSquareCdfEven(df: Int)(x: Double): Double =
    require(df % 2 == 0)
    if x <= 0.0 then 0.0
    else
      val h = x / 2.0
      var term = 1.0
      var sum = 1.0
      var j = 1
      while j < df / 2 do
        term *= h / j
        sum += term
        j += 1
      1.0 - math.exp(-h) * sum

  /** Student t3 CDF: 1/2 + (1/pi)(u/(1+u^2) + atan u), u = t/sqrt(3). */
  private def t3Cdf(t: Double): Double =
    val u = t / math.sqrt(3.0)
    0.5 + (u / (1.0 + u * u) + math.atan(u)) / math.Pi

  private def moments(x: Array[Double]): (Double, Double) =
    val mean = x.sum / x.length
    (mean, x.map(a => (a - mean) * (a - mean)).sum / (x.length - 1))

  private def assertKs(label: String, samples: Array[Double], cdf: Double => Double): Unit =
    val d = ks(samples, cdf)
    println(f"GENERATOR_LAW,$label,N=${samples.length},ks=$d%.5f,tol=${ksTolerance(samples.length)}%.5f")
    assert(d <= ksTolerance(samples.length), s"$label: KS $d exceeds ${ksTolerance(samples.length)}")

  test("family H: sigma^2 ~ scaled-inv-chi^2(10, .52): E[1/sigma^2] = 1/.52 and 10 * .52 / sigma^2 ~ chi^2_10"):
    val c = cell("H-n80-DI-Vsichi-T2-N8")
    val d = c.researchDesign
    val sigma2 = (0 until 60).toArray.flatMap(s => ModelJ.draw(c, Phase.Harness, StudyPurpose.Null, s, d).truth.sigma2)
    val precision = sigma2.map(1.0 / _)
    val (mean, _) = moments(precision)
    val se = math.sqrt(20.0) / 5.2 / math.sqrt(precision.length.toDouble)
    assertEqualsDouble(mean, 1.0 / 0.52, 4.0 * se)
    assertKs("H-scaled-inv-chi2", sigma2.map(s => 10.0 * 0.52 / s), chiSquareCdfEven(10))

  test("t3 u/e law: mean 0, and x sqrt(3) ~ t3 (so Var = 1; t3 has no fourth moment, so the law carries the variance check)"):
    val g = lane(1)
    val x = Array.fill(100000)(ModelJ.standard(ErrorLaw.StudentT3, g))
    val (mean, _) = moments(x)
    assertEqualsDouble(mean, 0.0, 4.0 / math.sqrt(x.length.toDouble))
    assertKs("t3-scaled", x.map(_ * math.sqrt(3.0)), t3Cdf)

  test("lognormal u/e law: mean 0, variance 1, and log(x sd + sqrt(e)) ~ N(0, 1)"):
    val g = lane(2)
    val x = Array.fill(100000)(ModelJ.standard(ErrorLaw.Lognormal, g))
    val (mean, variance) = moments(x)
    val e = math.E
    val kurtosis = math.pow(e, 4) + 2 * math.pow(e, 3) + 3 * e * e - 3.0
    assertEqualsDouble(mean, 0.0, 4.0 / math.sqrt(x.length.toDouble))
    assertEqualsDouble(variance, 1.0, 5.0 * math.sqrt((kurtosis - 1.0) / x.length))
    val sd = math.sqrt((e - 1.0) * e)
    assertKs("lognormal-log", x.map(a => math.log(a * sd + math.sqrt(e))), normalCdf)

  test("Gaussian u/e law: mean 0, variance 1"):
    val g = lane(3)
    val x = Array.fill(50000)(ModelJ.standard(ErrorLaw.Gaussian, g))
    val (mean, variance) = moments(x)
    assertEqualsDouble(mean, 0.0, 4.0 / math.sqrt(x.length.toDouble))
    assertEqualsDouble(variance, 1.0, 4.0 * math.sqrt(2.0 / x.length))

  test("AR(1) generator at rho = 0: slope z ~ N(0,1) and (T - rank) s^2 ~ chi^2_{T - rank} with s^2 = RSS/(T - 2)"):
    val series = FirstLevelSeries.Declared.copy(rho = 0.0)
    val rank = 2
    assertEquals(series.nominalDf, series.length - rank)
    val g = lane(4)
    val draws = Array.fill(20000)(ModelJ.arSlope(series, g))
    val z = draws.map(_._1)
    val q = draws.map((_, s2) => series.nominalDf * s2)
    val (zMean, zVar) = moments(z)
    assertEqualsDouble(zMean, 0.0, 4.0 / math.sqrt(z.length.toDouble))
    assertEqualsDouble(zVar, 1.0, 4.0 * math.sqrt(2.0 / z.length))
    assertKs("ar0-slope", z, normalCdf)
    val (qMean, _) = moments(q)
    val df = series.nominalDf
    assertEqualsDouble(qMean, df.toDouble, 4.0 * math.sqrt(2.0 * df / q.length))
    assertKs("ar0-rss", q, chiSquareCdfEven(df))
