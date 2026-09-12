package scalafim.fmri.hrf

import scalafim.fmri.hrf.fixtures.Cascade34Reference
import scalafim.fmri.hrf.HrfCombinators.*

class Cascade34Suite extends munit.FunSuite:
  private def params(values: Vector[Double]): Cascade34Params =
    Cascade34Params.make(values(0), values(1), values(2)).fold(e => fail(e.message), identity)

  private def close(actual: Double, expected: Double, relative: Double = 2e-12): Unit =
    assert(actual.isFinite, s"nonfinite value $actual")
    assertEqualsDouble(actual, expected, math.max(1e-300, relative * math.abs(expected)))

  test("parameters reject invalid rates and area ratios, including nonfinite values and invalid copies"):
    for (p, u) <- Seq((0.0, 0.2), (0.2, 0.2), (0.1, 0.2), (0.5, -0.1),
        (Double.NaN, 0.2), (Double.PositiveInfinity, 0.2), (0.5, Double.NaN)) do
      assert(Cascade34Params.make(p, u, 0.3).isLeft)
    for r <- Seq(-0.1, Double.NaN, Double.PositiveInfinity) do
      assert(Cascade34Params.make(0.5, 0.2, r).isLeft)
    intercept[IllegalArgumentException](Cascade34Params.Default.copy(kappaU = 0.6))
    assert(Cascade34Params.make(0.5, 0.2, 1.0).isRight)

  test("the scalar kernel is causal, matches the closed Erlang polynomial and keeps an unbounded tail"):
    for r <- Seq(0.0, 0.3, 1.0, 1.5) do
      val p = Cascade34Params(0.5, 0.2, r)
      val h = Hrfs.cascade34(p, span = 8.0.s)
      assertEquals(h.support, Support.Unbounded)
      for t <- Seq(-100.0, -0.001, 0.0) do
        assertEqualsDouble(h(Lag(t)).data(0), 0.0, 0.0)
      for t <- Seq(0.001, 0.3, 1.0, 4.0, 8.0, 32.0, 80.0) do
        val expected = math.pow(0.5, 3) * t * t * math.exp(-0.5 * t) / 2.0 -
          r * math.pow(0.2, 4) * t * t * t * math.exp(-0.2 * t) / 6.0
        close(h(Lag(t)).data(0), expected)

  test("exact box integrals match 80-digit quadrature near onset and in the far tail"):
    for c <- Cascade34Reference.integrals do
      val p = params(c.params)
      val h = Hrfs.cascade34(p)
      val got = Primitive.definiteIntegral(h, Lag(c.lo), Lag(c.hi)).get(0)
      close(got, c.value, 3e-12)
      close(Cascade34.integral(p, Lag(c.hi), Lag(c.lo)), -c.value, 3e-12)
      val width = Seconds(c.hi - c.lo)
      val height = Pulse.box(width).fold(e => fail(e.message), identity)
      val mass = Pulse.boxMass(width).fold(e => fail(e.message), identity)
      for precision <- Seq(0.001.s, 2.0.s) do
        close(PulseResponse.at(height, h, Lag(c.hi), precision).data(0), c.value, 3e-12)
        close(PulseResponse.at(mass, h, Lag(c.hi), precision).data(0), c.value / width.value, 3e-12)

  test("components have unit area and the full signed area remains 1 - rho at rho = 1"):
    for r <- Seq(0.0, 0.3, 1.0, 1.5) do
      val p = Cascade34Params(0.5, 0.2, r)
      assertEqualsDouble(Cascade34.primitive(p, Lag(1000.0)), 1.0 - r, 1e-14)
      // Independent midpoint quadrature; O(dt^2), with an exponentially negligible tail.
      val dt = 0.002
      var sum = 0.0
      var i = 0
      while i < 100000 do
        sum += Cascade34.value(p, Lag((i + 0.5) * dt)) * dt
        i += 1
      assertEqualsDouble(sum, 1.0 - r, 2e-11)

  test("binding Cascade34 into a basis preserves its exact far-tail box response"):
    val c = Cascade34Reference.integrals.find(c => c.params(2) == 0.3 && c.lo == 240.0).get
    val h = Hrfs.cascade34(params(c.params))
    val nested = bindBasis(Seq(Hrfs.gaussian(), bindBasis(Seq(h, Hrfs.gamma()))))
    val values = Primitive.definiteIntegral(nested, Lag(c.lo), Lag(c.hi)).get
    assertEquals(values.length, 3)
    close(values(1), c.value, 3e-12)

  test("attained peak, FWHM and trough match the independent continuous-time oracle"):
    for c <- Cascade34Reference.summaries do
      val p = params(c.params)
      val s = Cascade34.summaries(p)
      close(s.peakLatency.value, c.peak)
      close(s.peakHeight, c.height)
      close(s.fwhm.value, c.width)
      (s.undershoot, c.trough) match
        case (None, None) => ()
        case (Some(actual), Some((t, h))) =>
          close(actual.latency.value, t)
          close(actual.height, h, 2e-10)
          close(actual.ratioToPeak, -h / c.height, 2e-10)
        case _ => fail(s"undershoot disagreement: $s vs $c")
      assert(s.peakHeight > 0.0)

  test("changing both rates dilates time and rescales heights, preserving areas and trough ratios"):
    val p = Cascade34Params.Default
    val a = Cascade34.summaries(p)
    for factor <- Seq(0.1, 2.0, 10.0) do
      val scaled = Cascade34Params(p.kappaP * factor, p.kappaU * factor, p.rho)
      val b = Cascade34.summaries(scaled)
      close(b.peakLatency.value, a.peakLatency.value / factor)
      close(b.fwhm.value, a.fwhm.value / factor)
      close(b.peakHeight, a.peakHeight * factor)
      close(b.undershoot.get.ratioToPeak, a.undershoot.get.ratioToPeak)
      for t <- Seq(0.1, 4.0, 12.0, 40.0) do
        close(Cascade34.value(scaled, Lag(t / factor)), factor * Cascade34.value(p, Lag(t)))

  test("typed and named dispatch retain Cascade34 parameters and its primitive; decorators clear it"):
    val h = Hrfs.cascade34()
    assertEquals(HrfKind.fromString("cascade34"), Right(HrfKind.Cascade34))
    assert(HrfKind.Cascade34.isScalarByDefault)
    assert(Registry.listAvailable.contains("cascade34"))
    assertEquals(h.descriptor.params, HrfParams.Cascade34(Cascade34Params.Default))
    assertEquals(h.descriptor.integration, IntegrationPolicy.Cascade34(Cascade34Params.Default))
    val fromSpec = HrfSpec(HrfKind.Cascade34).flatMap(_.toHrf).fold(e => fail(e.message), identity)
    val fromRegistry = Registry.getEither("cascade34").fold(e => fail(e.message), identity)
    for other <- Seq(fromSpec, fromRegistry) do
      close(other(Lag(4.0)).data(0), h(Lag(4.0)).data(0))
    assert(Primitive.definiteIntegral(h.lag(2.0.s), Lag(0.0), Lag(8.0)).isEmpty)

  test("extreme finite lags produce a zero tail without polynomial overflow"):
    val p = Cascade34Params.Default
    for t <- Seq(1e5, 1e100, Double.MaxValue) do
      assertEqualsDouble(Cascade34.value(p, Lag(t)), 0.0, 0.0)
      assertEqualsDouble(Cascade34.primitive(p, Lag(t)), 0.7, 1e-15)
    assertEqualsDouble(Cascade34.value(p, Lag(1e-200)), 0.0, 0.0)
