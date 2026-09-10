package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{HrfFunctions, Hrfs, Lag, Seconds}

class LwuFamilySuite extends munit.FunSuite:

  private val family = LwuFamily.Default
  private val lags = Array.tabulate(320)(i => i * 0.1)

  private def pointAt(tau: Double, logSd: Double, rho: Double): ShapePoint =
    family.chart.point(tau, logSd, rho).fold(e => fail(e.message), identity)

  test("the raw LWU formula is the library kernel and reduces to the Gaussian at rho = 0"):
    val point = pointAt(5.5, math.log(1.7), 0.35)
    val values = new Array[Double](lags.length)
    family.evalInto(lags, point, values)
    val library = Hrfs.lwu(tau = 5.5, sigma = 1.7, rho = 0.35, normalize = HrfFunctions.LwuNormalize.None, span = Seconds(32.0))
    var i = 0
    while i < lags.length do
      assertEqualsDouble(values(i), library(Lag(lags(i))).data(0), 1e-13, s"lag ${lags(i)}")
      i += 1
    val gauss = GaussianFamily.Default
    val g = new Array[Double](lags.length)
    gauss.evalInto(lags, gauss.chart.point(5.5, math.log(1.7)).fold(e => fail(e.message), identity), g)
    family.evalInto(lags, pointAt(5.5, math.log(1.7), 0.0), values)
    i = 0
    while i < lags.length do
      assertEqualsDouble(values(i), g(i), 1e-13)
      i += 1

  test("jets agree with central finite differences in all three coordinates"):
    val h = 1e-4
    val n = lags.length
    for (t, v, r) <- Seq((4.5, math.log(1.2), 0.3), (6.0, math.log(2.5), 0.7), (3.2, math.log(0.9), 0.05)) do
      val jet = new Array[Double](10 * n)
      family.jetInto(lags, ShapePoint.unsafe(Vector(t, v, r)), jet)
      def shifted(dx: Vector[Double]): Array[Double] =
        val out = new Array[Double](10 * n)
        family.jetInto(lags, ShapePoint.unsafe(Vector(t + dx(0), v + dx(1), r + dx(2))), out)
        out
      var p = 0
      while p < 3 do
        val step = Vector.tabulate(3)(i => if i == p then h else 0.0)
        val plus = shifted(step)
        val minus = shifted(step.map(-_))
        var i = 0
        var worstFirst = 0.0
        while i < n do
          worstFirst = math.max(worstFirst, math.abs((plus(i) - minus(i)) / (2 * h) - jet(JetLayout.first(p) * n + i)))
          i += 1
        assert(worstFirst < 1e-6, s"first derivative $p at ($t,$v,$r): $worstFirst")
        var q = 0
        while q < 3 do
          i = 0
          var worstSecond = 0.0
          while i < n do
            val fd = (plus(JetLayout.first(q) * n + i) - minus(JetLayout.first(q) * n + i)) / (2 * h)
            worstSecond = math.max(worstSecond, math.abs(fd - jet(JetLayout.second(3, p, q) * n + i)))
            i += 1
          assert(worstSecond < 1e-6, s"second derivative ($p,$q) at ($t,$v,$r): $worstSecond")
          q += 1
        p += 1

  test("summaries report attained peak, width and undershoot; normalisation is unnormalised only"):
    val point = pointAt(6.0, math.log(2.0), 0.4)
    val summary = family.summaries(point)
    assert(summary.peakLatency.value > 5.0 && summary.peakLatency.value < 6.5, s"peak ${summary.peakLatency.value}")
    assert(summary.fwhm.value > 2.0 && summary.fwhm.value < 6.0, s"fwhm ${summary.fwhm.value}")
    assert(summary.undershootRatio.exists(u => u > 0.2 && u < 0.6), s"undershoot ${summary.undershootRatio}")
    assertEquals(family.summaries(pointAt(6.0, math.log(2.0), 0.0)).undershootRatio, None)
    assert(family.supports(NormalizationRule.Unnormalised))
    assert(!family.supports(NormalizationRule.UnitPeak))
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(NormalizationRule.Unnormalised, point, scale)
    assertEquals(scale.toVector, Vector.fill(10)(0.0).updated(0, 1.0))
    assert(LwuFamily.make(ShapeChart(("tau", 3.0, 8.0), ("logSd", 0.0, 1.0), ("rho", -0.1, 0.5))).isLeft)
    assertEquals(family.descriptor(point).span.value, 32.0)
