package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{HrfParams, Hrfs, Lag, PositiveSeconds, Seconds}

class GaussianFamilySuite extends munit.FunSuite:

  private val family = GaussianFamily.Default
  private val lags = Array.tabulate(240)(i => i * 0.1)

  private def pointAt(tau: Double, logSd: Double): ShapePoint =
    family.chart.point(tau, logSd).fold(e => fail(e.message), identity)

  test("charts validate names, bounds and points"):
    assert(ShapeChart.make().isLeft)
    assert(ShapeChart.make(("a", 0.0, 1.0), ("a", 0.0, 1.0)).isLeft)
    assert(ShapeChart.make(("a", 1.0, 1.0)).isLeft)
    assert(ShapeChart.make(("a", 0.0, 1.0), ("b", 0.0, 1.0), ("c", 0.0, 1.0), ("d", 0.0, 1.0)).isLeft)
    val chart = family.chart
    assert(chart.point(4.0, 0.0).isRight)
    assert(chart.point(2.0, 0.0).isLeft, "tau below the chart")
    assert(chart.point(4.0, 5.0).isLeft, "log sigma above the chart")
    assert(chart.point(4.0).isLeft, "wrong dimension")
    assert(chart.point(Double.NaN, 0.0).isLeft)
    assert(GaussianFamily.make(ShapeChart(("tau", -1.0, 8.0), ("logSd", 0.0, 1.0))).isLeft)
    assert(GaussianFamily.make(ShapeChart(("mean", 3.0, 8.0), ("logSd", 0.0, 1.0))).isLeft)
    assert(GaussianFamily.make(horizon = Seconds(0.0)).isLeft)

  test("jet layout indexes the upper triangle without gaps"):
    assertEquals(JetLayout.components(2), 6)
    assertEquals(JetLayout.components(3), 10)
    assertEquals(JetLayout.second(2, 0, 0), 3)
    assertEquals(JetLayout.second(2, 0, 1), 4)
    assertEquals(JetLayout.second(2, 1, 0), 4)
    assertEquals(JetLayout.second(2, 1, 1), 5)
    val seen = (for p <- 0 until 3; q <- p until 3 yield JetLayout.second(3, p, q)).toSet
    assertEquals(seen, (4 until 10).toSet)

  test("the density convention reproduces Hrfs.gaussian exactly"):
    val point = pointAt(5.5, math.log(1.7))
    val values = new Array[Double](lags.length)
    family.evalInto(lags, point, values)
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(NormalizationRule.Density, point, scale)
    val library = Hrfs.gaussian(mean = 5.5, sd = 1.7, span = Seconds(24.0))
    var i = 0
    while i < lags.length do
      assertEqualsDouble(values(i) * scale(0), library(Lag(lags(i))).data(0), 1e-13, s"lag ${lags(i)}")
      i += 1
    assertEqualsDouble(family.toHrf(point)(Lag(5.5)).data(0), library(Lag(5.5)).data(0), 1e-15)

  test("uniform-grid recurrence agrees with direct evaluation"):
    val point = pointAt(7.9, math.log(0.85))
    val uniform = new Array[Double](lags.length)
    family.evalInto(lags, point, uniform)
    // A non-uniform grid forces the direct path: append one extra lag.
    val irregular = lags :+ 24.05
    val direct = new Array[Double](irregular.length)
    family.evalInto(irregular, point, direct)
    var i = 0
    while i < lags.length do
      assertEqualsDouble(uniform(i), direct(i), 1e-13 * math.max(1e-6, direct(i)), s"lag ${lags(i)}")
      i += 1

  test("negative lags are causal zeros in values and jets"):
    val point = pointAt(3.0, 0.0)
    val negative = Array(-3.0, -0.5, -1e-9)
    val jet = new Array[Double](family.jetComponents * negative.length)
    family.jetInto(negative, point, jet)
    assert(jet.forall(_ == 0.0))

  test("jets agree with central finite differences"):
    val h = 1e-4
    for (t, v) <- Seq((4.5, math.log(1.2)), (6.0, math.log(2.5)), (3.2, math.log(0.9))) do
      val point = pointAt(t, v)
      val n = lags.length
      val jet = new Array[Double](6 * n)
      family.jetInto(lags, point, jet)
      def fd(comp: Int, source: Int, dTau: Double, dV: Double): Unit =
        val plus = new Array[Double](6 * n)
        val minus = new Array[Double](6 * n)
        family.jetInto(lags, ShapePoint.unsafe(Vector(t + dTau, v + dV)), plus)
        family.jetInto(lags, ShapePoint.unsafe(Vector(t - dTau, v - dV)), minus)
        var worst = 0.0
        var i = 0
        while i < n do
          val estimate = (plus(source * n + i) - minus(source * n + i)) / (2.0 * math.hypot(dTau, dV))
          worst = math.max(worst, math.abs(estimate - jet(comp * n + i)))
          i += 1
        assert(worst < 1e-6, s"component $comp from $source at ($t, $v): $worst")
      fd(1, 0, h, 0.0)
      fd(2, 0, 0.0, h)
      fd(3, 1, h, 0.0)
      fd(4, 1, 0.0, h)
      fd(5, 2, 0.0, h)

  test("scale jets, summaries and descriptor follow the chart"):
    val point = pointAt(6.0, math.log(2.0))
    val scale = new Array[Double](family.jetComponents)
    family.scaleJetInto(NormalizationRule.UnitPeak, point, scale)
    assertEquals(scale.toVector, Vector(1.0, 0.0, 0.0, 0.0, 0.0, 0.0))
    family.scaleJetInto(NormalizationRule.Density, point, scale)
    val s = 1.0 / (2.0 * math.sqrt(2.0 * math.Pi))
    assertEqualsDouble(scale(0), s, 1e-15)
    assertEqualsDouble(scale(JetLayout.first(1)), -s, 1e-15)
    assertEqualsDouble(scale(JetLayout.second(2, 1, 1)), s, 1e-15)
    assert(!family.supports(NormalizationRule.UnitIntegral))
    intercept[IllegalArgumentException](family.scaleJetInto(NormalizationRule.UnitIntegral, point, scale))
    val summary = family.summaries(point)
    assertEqualsDouble(summary.peakLatency.value, 6.0, 1e-15)
    assertEqualsDouble(summary.fwhm.value, 2.0 * 2.0 * math.sqrt(2.0 * math.log(2.0)), 1e-12)
    assertEquals(summary.undershootRatio, None)
    val descriptor = family.descriptor(point)
    assertEquals(descriptor.params, HrfParams.Gaussian(6.0, 2.0))
    assertEqualsDouble(descriptor.span.value, 24.0, 0.0)

  test("tail energy beyond the horizon grows with width and latency"):
    val precision = PositiveSeconds(0.05).fold(e => fail(e.message), identity)
    val narrow = pointAt(4.0, math.log(1.0))
    val wide = pointAt(8.0, math.log(3.0))
    val tailNarrow = family.tailRelativeEnergy(narrow, precision)
    val tailWide = family.tailRelativeEnergy(wide, precision)
    assert(tailNarrow < 1e-12, s"narrow tail $tailNarrow")
    assert(tailWide > tailNarrow)
    assert(tailWide < 1e-5, s"wide tail $tailWide")
