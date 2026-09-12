package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{HrfParams, Seconds}
import scalafim.fmri.hrf.fixtures.Cascade34Reference

class Cascade34FamilySuite extends munit.FunSuite:
  private val family = Cascade34Family.Default

  test("all jet components agree with independent 80-digit differentiation of the Erlang formula"):
    for c <- Cascade34Reference.jets do
      val point = ShapePoint.unsafe(c.point)
      val jet = Array.fill(10)(Double.NaN)
      val value = Array(Double.NaN)
      family.jetInto(Array(c.lag), point, jet)
      family.evalInto(Array(c.lag), point, value)
      assertEqualsDouble(value(0), jet(0), 0.0)
      for component <- 0 until 10 do
        val expected = c.jet(component)
        assertEqualsDouble(jet(component), expected, 1e-14 * math.abs(c.jet(0)) + 3e-12 * math.abs(expected) + 1e-300,
          s"point ${c.point}, lag ${c.lag}, component $component")

  test("jets use component-major layout, overwrite caller buffers and remain finite in the zero tail"):
    val point = family.chart.point(math.log(0.5), math.log(0.4 / 0.6), 0.3).toOption.get
    val lags = Array(-10.0, 0.0, 0.1, 4.0, 12.0, 100.0, 1e100, Double.MaxValue)
    val all = Array.fill(10 * lags.length + 1)(Double.NaN)
    family.jetInto(lags, point, all)
    for i <- lags.indices do
      val one = new Array[Double](10)
      family.jetInto(Array(lags(i)), point, one)
      for c <- 0 until 10 do
        assert(all(c * lags.length + i).isFinite)
        assertEqualsDouble(all(c * lags.length + i), one(c), 0.0)
    assert(all.last.isNaN, "caller storage beyond the jet is not workspace")

  test("rho zero reports tail nonidentification, invariant values and vanishing pure tail derivatives"):
    val lags = Array(0.2, 1.0, 5.0, 12.0, 48.0)
    val a = family.chart.point(math.log(0.5), -1.5, 0.0).toOption.get
    val b = family.chart.point(math.log(0.5), 0.5, 0.0).toOption.get
    val ja = new Array[Double](10 * lags.length)
    val jb = new Array[Double](ja.length)
    family.jetInto(lags, a, ja)
    family.jetInto(lags, b, jb)
    assertEquals(family.unidentifiedCoordinates(a), Vector("logitRateRatio"))
    for i <- lags.indices do
      assertEqualsDouble(ja(i), jb(i), 0.0)
      for c <- Seq(JetLayout.first(1), JetLayout.second(3, 0, 1), JetLayout.second(3, 1, 1)) do
        assertEqualsDouble(ja(c * lags.length + i), 0.0, 0.0)
      // Mixed rho/tail derivatives need not vanish: turning on rho restores the tail.
      assert(ja(JetLayout.first(2) * lags.length + i) < 0.0)
    assertEquals(family.summaries(a).undershootRatio, None)
    assertEquals(family.unidentifiedCoordinates(ShapePoint.unsafe(a.coordinates.updated(2, 0.1))), Vector.empty)

  test("positive-component-area normalization stays smooth at signed-area cancellation"):
    val point = ShapePoint.unsafe(Vector(math.log(0.5), math.log(0.4 / 0.6), 1.0))
    for rule <- Seq(NormalizationRule.PositiveComponentArea, NormalizationRule.Unnormalised) do
      val scale = Array.fill(10)(Double.NaN)
      family.scaleJetInto(rule, point, scale)
      assertEqualsDouble(scale(0), 1.0, 0.0)
      scale.drop(1).foreach(v => assertEqualsDouble(v, 0.0, 0.0))
    assertEquals(family.libraryNormalization, NormalizationRule.PositiveComponentArea)
    for rule <- Seq(NormalizationRule.UnitPeak, NormalizationRule.UnitIntegral, NormalizationRule.Density) do
      assert(!family.supports(rule))
      intercept[IllegalArgumentException](family.scaleJetInto(rule, point, new Array[Double](10)))

  test("chart admission enforces coordinate names, representable ordered rates, rho and horizon"):
    assert(Cascade34Family.make(GaussianFamily.DefaultChart).isLeft)
    def chart(a: (Double, Double), b: (Double, Double), r: (Double, Double)): ShapeChart =
      ShapeChart(("logKappaP", a._1, a._2), ("logitRateRatio", b._1, b._2), ("rho", r._1, r._2))
    for bad <- Seq(
        chart((-1000.0, -900.0), (-1.0, 1.0), (0.0, 0.8)),
        chart((700.0, 800.0), (-1.0, 1.0), (0.0, 0.8)),
        chart((-2.0, 0.0), (40.0, 50.0), (0.0, 0.8)),
        chart((-2.0, 0.0), (-1000.0, -900.0), (0.0, 0.8)),
        chart((-2.0, 0.0), (-1.0, 1.0), (-0.1, 0.8))) do
      assert(Cascade34Family.make(bad).isLeft)
    assert(Cascade34Family.make(horizon = Seconds(0.0)).isLeft)
    assert(Cascade34Family.make(chart((-2.0, 0.0), (-1.0, 1.0), (0.0, 1.5))).isRight)

  test("summaries and provenance report actual peaks and an untruncated slow undershoot"):
    val point = family.chart.point(math.log(0.26), math.log(0.027 / (0.26 - 0.027)), 0.75).toOption.get
    val short = Cascade34Family.make(horizon = Seconds(1.0)).toOption.get
    val s = family.attainedSummaries(point)
    assertEquals(short.attainedSummaries(point), s)
    assert(s.undershoot.get.latency.value > family.horizon.value)
    assertEquals(family.summaries(point).peakLatency, s.peakLatency)
    assertEquals(family.descriptor(point), family.toHrf(point).descriptor)
    assertEquals(family.descriptor(point).params, HrfParams.Cascade34(family.parameters(point)))
    assertEquals(family.realization, RealizationSupport.Exact(7))
    assert(family.secondOrderSmooth)
