package scalafim.fmri.hrf

class EndpointAnchoredBsplineSuite extends munit.FunSuite:
  private val convention = Hrfs.BsplineConvention.EndpointAnchored

  test("endpoint-anchored basis matches pinned fmrihrf 18d418f fixtures"):
    fixtures.EndpointAnchoredBsplineRFixtures.all.foreach { fixture =>
      val span = Seconds(fixture.span)
      val hrf = Hrfs.bspline(fixture.nBasis, span, fixture.degree, convention)
      assertEquals(hrf.nbasis, fixture.nBasis, fixture.name)
      assertEquals(hrf.descriptor.nbasis, fixture.nBasis, fixture.name)
      fixture.times.zipWithIndex.foreach { case (time, row) =>
        val raw = HrfFunctions.bsplineBasis(Lag(time), span, fixture.nBasis, fixture.degree, convention)
        val actual = hrf(Lag(time)).data
        assertEquals(raw.length, fixture.nBasis, fixture.name)
        assertEquals(actual.length, fixture.nBasis, fixture.name)
        for column <- 0 until fixture.nBasis do
          // Serialized at 13 significant digits (absolute rounding <= 5e-14
          // for values in [0, 1]); allow evaluation roundoff on both platforms.
          val expected = fixture.at(row, column)
          assertEqualsDouble(raw(column), expected, 1e-12, s"${fixture.name} raw t=$time column=$column")
          assertEqualsDouble(actual(column), expected, 1e-12, s"${fixture.name} t=$time column=$column")
      }
    }

  test("minimum cubic basis is the two interior Bernstein polynomials"):
    val hrf = Hrfs.bspline(2, 24.5.s, 3, convention)
    for i <- 0 to 100 do
      val u = i / 100.0
      val row = hrf(Lag(24.5 * u)).data
      assertEquals(row.length, 2)
      assertEqualsDouble(row(0), 3 * u * math.pow(1 - u, 2), 2e-14)
      assertEqualsDouble(row(1), 3 * u * u * (1 - u), 2e-14)

  test("endpoints and outside support are exactly zero for every column"):
    for degree <- 1 to 12 do
      val count = math.max(1, degree - 1) + 3
      val hrf = Hrfs.bspline(count, 0.5.s, degree, convention)
      for time <- Vector(-1.0, 0.0, 0.5, 1.0) do
        val raw = HrfFunctions.bsplineBasis(Lag(time), 0.5.s, count, degree, convention)
        assertEquals(raw.length, count)
        assertEquals(hrf(Lag(time)).data.length, count)
        raw.foreach(value => assertEqualsDouble(value, 0.0, 0.0))
        hrf(Lag(time)).data.foreach(value => assertEqualsDouble(value, 0.0, 0.0))

  test("invalid degree, basis count and span fail at construction and raw evaluation"):
    for (count, degree) <- Vector((1, 0), (1, -1), (1, 3), (2, 4), (0, 2), (Int.MaxValue, 3)) do
      intercept[IllegalArgumentException](Hrfs.bspline(count, 24.s, degree, convention))
      intercept[IllegalArgumentException](HrfFunctions.bsplineBasis(Lag.zero, 24.s, count, degree, convention))
    for span <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      intercept[IllegalArgumentException](Hrfs.bspline(5, Seconds.unsafe(span), 3, convention))
      intercept[IllegalArgumentException](HrfFunctions.bsplineBasis(Lag.zero, Seconds.unsafe(span), 5, 3, convention))

  test("uniform fractional-span knots give analytic whole-window mean weights"):
    val basis = ResponseBasis.of(Hrfs.bspline(5, 24.5.s, 3, convention))
    val mean = basis.responseFunctional(ResponseFunctional.WindowMean(0.s, 24.5.s),
      FunctionalDiscretization.Exact).fold(error => fail(error.message), identity)
    // Full cubic knot vector: [0,0,0,0, span/4,span/2,3span/4,span,span,span,span].
    // Integral of N_i,3 / span = (knot(i+4)-knot(i))/(4*span), retaining i=1..5.
    assertEquals(mean.values.length, 5)
    mean.values.zip(Vector(2, 3, 4, 3, 2)).foreach { case (actual, numerator) =>
      assertEqualsDouble(actual, numerator / 16.0, 1e-12)
    }

  test("default legacy and complete identities stay distinct from endpoint anchoring"):
    val legacy = Hrfs.bspline(5, 24.5.s, 3)
    val explicitLegacy = Hrfs.bspline(5, 24.5.s, 3, Hrfs.BsplineConvention.LegacyR)
    val complete = Hrfs.bspline(5, 24.5.s, 3, Hrfs.BsplineConvention.Complete)
    val anchored = Hrfs.bspline(5, 24.5.s, 3, convention)
    assertEquals(legacy.descriptor.canonicalId, explicitLegacy.descriptor.canonicalId)
    assertEquals(Vector(legacy, complete, anchored).map(_.descriptor.canonicalId).distinct.length, 3)
    assert(anchored.descriptor.canonicalId.contains("17:endpoint-anchored"))
