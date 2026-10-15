package scalafim.fmri.hrf.family

import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfKind, PositiveSeconds, Seconds}

class FamilySummaryGridSuite extends munit.FunSuite:
  private def positive(value: Double): PositiveSeconds = PositiveSeconds(value).fold(e => fail(e.message), identity)
  private val point = GaussianFamily.Default.chart.point(5.5, math.log(1.7)).fold(e => fail(e.message), identity)

  private class Probe(val horizon: PositiveSeconds, value: Double = 1.0, throws: Boolean = false) extends ParametricHrfFamily:
    var evaluations = 0
    var lastLag = 0.0
    val chart = GaussianFamily.Default.chart
    def name: String = "summary-probe"
    def kind: HrfKind = HrfKind.Gaussian
    def supports(rule: NormalizationRule): Boolean = true
    def libraryNormalization: NormalizationRule = NormalizationRule.Unnormalised
    def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      evaluations += 1
      lastLag = lags.last
      if throws then throw new IllegalArgumentException("probe validation failure")
      java.util.Arrays.fill(out, value)
    def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit = fail("summaries must not allocate/evaluate jets")
    def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit = fail("not needed")
    def summaries(point: ShapePoint): ShapeSummary = fail("not needed")
    def descriptor(point: ShapePoint): HrfDescriptor = GaussianFamily.Default.descriptor(point)
    def toHrf(point: ShapePoint): Hrf = GaussianFamily.Default.toHrf(point)

  test("tail admission accepts the sample threshold and refuses the next sample before evaluation"):
    val at = new Probe(positive(249999.75))
    assertEquals(at.validateTailRelativeEnergyGrid(positive(1.0)), Right(()))
    assertEquals(FamilySummaryGrid.tail(at.horizon, positive(1.0), 4.0).map(_.samples), Right(FamilySummaryGrid.MaxSamples))
    val above = new Probe(positive(250000.0), throws = true)
    assertEquals(above.tailRelativeEnergyEither(point, positive(1.0)), Left(FamilySummaryError.SampleLimitExceeded(1000001.0, FamilySummaryGrid.MaxSamples)))
    assertEquals(above.evaluations, 0)
    intercept[IllegalArgumentException](above.tailRelativeEnergy(point, positive(1.0)))
    assertEquals(above.evaluations, 0)

  test("LWU floor admission accepts threshold and refuses one more sample"):
    assertEquals(FamilySummaryGrid.summary(positive(9999.99)).map(_.samples), Right(FamilySummaryGrid.MaxSamples))
    val family = LwuFamily.make(horizon = Seconds(10000.0)).fold(e => fail(e.message), identity)
    val p = family.chart.point(6.0, math.log(2.0), 0.4).fold(e => fail(e.message), identity)
    assertEquals(family.summariesEither(p), Left(FamilySummaryError.SampleLimitExceeded(1000001.0, FamilySummaryGrid.MaxSamples)))
    intercept[IllegalArgumentException](family.summaries(p))

  test("invalid extent precision horizon and derived grids are refused without evaluation"):
    val family = new Probe(positive(1.0))
    for extent <- Seq(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity, -1.0, 0.0, 0.999) do
      assert(family.tailRelativeEnergyEither(point, positive(0.1), extent).swap.exists(_.isInstanceOf[FamilySummaryError.InvalidExtent]))
    for dt <- Seq(Double.NaN, Double.PositiveInfinity, 0.0, -1.0) do
      assert(family.tailRelativeEnergyEither(point, PositiveSeconds.unsafe(Seconds.unsafe(dt))).swap.exists(_.isInstanceOf[FamilySummaryError.InvalidPrecision]))
    for h <- Seq(Double.NaN, Double.PositiveInfinity, 0.0, -1.0) do
      val invalid = new Probe(PositiveSeconds.unsafe(Seconds.unsafe(h)))
      assert(invalid.tailRelativeEnergyEither(point, positive(0.1)).swap.exists(_.isInstanceOf[FamilySummaryError.InvalidHorizon]))
      assertEquals(invalid.evaluations, 0)
    assert(family.tailRelativeEnergyEither(point, positive(Double.MinPositiveValue)).swap.exists(_.isInstanceOf[FamilySummaryError.SampleLimitExceeded]))
    assert(family.tailRelativeEnergyEither(point, positive(1e-10)).swap.exists(_.isInstanceOf[FamilySummaryError.SampleLimitExceeded]))
    val huge = new Probe(positive(1e308))
    assert(huge.tailRelativeEnergyEither(point, positive(1e308), 4.0).swap.exists(_.isInstanceOf[FamilySummaryError.NonFiniteRange]))
    assert(huge.tailRelativeEnergyEither(point, positive(1e308), 1.1).swap.exists(_.isInstanceOf[FamilySummaryError.NonFiniteLastTime]))
    assertEquals(huge.evaluations, 0)
    assertEquals(family.evaluations, 0)

  test("admitted tail grids preserve ceil overshoot minimum two samples and extent one"):
    val family = new Probe(positive(1.0))
    assertEqualsDouble(family.tailRelativeEnergyEither(point, positive(0.3), 1.0).fold(e => fail(e.message), identity), 0.125, 1e-15)
    assertEqualsDouble(family.lastLag, 1.2, 1e-15)
    assertEqualsDouble(family.tailRelativeEnergyEither(point, positive(5.0), 1.0).fold(e => fail(e.message), identity), 0.5, 1e-15)
    assertEqualsDouble(family.lastLag, 5.0, 0.0)

  test("typed boundaries revalidate points and represent callback validation and nonfinite numerics"):
    val family = new Probe(positive(1.0))
    for coordinates <- Seq(Vector(5.5), Vector(5.5, Double.NaN), Vector(20.0, math.log(1.7))) do
      assert(family.tailRelativeEnergyEither(ShapePoint.unsafe(coordinates), positive(0.1)).swap.exists(_.isInstanceOf[FamilySummaryError.Chart]))
    assertEquals(family.evaluations, 0)
    val lwu = LwuFamily.Default
    assert(lwu.summariesEither(point).swap.exists(_.isInstanceOf[FamilySummaryError.Chart]))
    assert(new Probe(positive(1.0), throws = true).tailRelativeEnergyEither(point, positive(1.0)).swap.exists(_.isInstanceOf[FamilySummaryError.EvaluationFailed]))
    assert(new Probe(positive(1.0), Double.NaN).tailRelativeEnergyEither(point, positive(1.0)).swap.exists(_.isInstanceOf[FamilySummaryError.NonFiniteValue]))
    assertEquals(new Probe(positive(1.0), 1e308).tailRelativeEnergyEither(point, positive(1.0)), Left(FamilySummaryError.NonFiniteEnergy))

  test("tail quadrature matches independent scalar Gaussian golden fixtures"):
    // Generated by docs/verification/family-summary-budget-20261005/fixtures.py.
    for (tau, sd, horizon, dt, extent, expected) <- Seq(
      (5.5, 1.7, 8.0, 0.3, 4.0, 0.02050460553659128),
      (6.0, 2.0, 6.0, 0.5, 4.0, 0.42948195197451383),
      (3.0, 0.8, 3.0, 0.7, 1.0, 0.21434628313951998)
    ) do
      val family = GaussianFamily.make(horizon = Seconds(horizon)).fold(e => fail(e.message), identity)
      val p = family.chart.point(tau, math.log(sd)).fold(e => fail(e.message), identity)
      assertEqualsDouble(family.tailRelativeEnergyEither(p, positive(dt), extent).fold(e => fail(e.message), identity), expected, 1e-14)

  test("LWU attained summaries match independent formula goldens including floor horizon"):
    for (tau, sd, rho, horizon, peak, width, undershoot) <- Seq(
      (6.0, 2.0, 0.4, 32.0, 5.72, 4.11, Some(0.40980877128414617)),
      (5.5, 1.7, 0.35, 32.0, 5.29, 3.56, Some(0.34511941256400946)),
      (6.0, 2.0, 0.0, 32.0, 6.0, 4.72, None),
      (6.0, 2.0, 0.4, 7.005, 5.72, 3.48, None)
    ) do
      val family = LwuFamily.make(horizon = Seconds(horizon)).fold(e => fail(e.message), identity)
      val p = family.chart.point(tau, math.log(sd), rho).fold(e => fail(e.message), identity)
      val actual = family.summariesEither(p).fold(e => fail(e.message), identity)
      assertEqualsDouble(actual.peakLatency.value, peak, 1e-14)
      assertEqualsDouble(actual.fwhm.value, width, 1e-14)
      undershoot match
        case Some(expected) => assertEqualsDouble(actual.undershootRatio.getOrElse(fail("missing undershoot")), expected, 1e-13)
        case None => assertEquals(actual.undershootRatio, None)

  test("summary structural admission is independent of coarse tail diagnostic precision"):
    val family = LwuFamily.make(horizon = Seconds(10000.0)).fold(e => fail(e.message), identity)
    assertEquals(family.validateTailRelativeEnergyGrid(positive(500.0)), Right(()))
    assertEquals(family.validateSummaryGrid, Left(FamilySummaryError.SampleLimitExceeded(1000001.0, FamilySummaryGrid.MaxSamples)))
    assertEquals(GaussianFamily.Default.validateSummaryGrid, Right(()))

  test("typed analytic summaries validate points and report decoded parameter overflow"):
    val chart = ShapeChart(("tau", 3.0, 8.0), ("logSd", 799.0, 801.0))
    val family = GaussianFamily.make(chart).fold(e => fail(e.message), identity)
    val point = chart.point(5.0, 800.0).fold(e => fail(e.message), identity)
    assert(family.summariesEither(point).swap.exists(_.isInstanceOf[FamilySummaryError.EvaluationFailed]))
    assert(GaussianFamily.Default.summariesEither(ShapePoint.unsafe(Vector(5.0))).swap.exists(_.isInstanceOf[FamilySummaryError.Chart]))

  test("typed summary validation rejects nonfinite returned scalars"):
    val invalid = Seq(
      (ShapeSummary(Seconds.unsafe(Double.NaN), Seconds(1.0), None), ShapeSummaryField.PeakLatency),
      (ShapeSummary(Seconds(1.0), Seconds.unsafe(Double.PositiveInfinity), None), ShapeSummaryField.Fwhm),
      (ShapeSummary(Seconds(1.0), Seconds(1.0), Some(Double.PositiveInfinity)), ShapeSummaryField.UndershootRatio)
    )
    for (summary, field) <- invalid do
      val family = new Probe(positive(1.0)):
        override def summaries(point: ShapePoint): ShapeSummary = summary
      family.summariesEither(point) match
        case Left(FamilySummaryError.NonFiniteSummary(actualField, _)) => assertEquals(actualField, field)
        case other => fail(s"expected finite summary scalar error, got $other")
