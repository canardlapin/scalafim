package scalafim.fmri.hrf

class CompleteBsplineSuite extends munit.FunSuite:
  test("complete cubic basis agrees with independently generated R splines fixture"):
    // R: splines::bs(c(0,2.45,6.125,12.25,22.05,24.5), knots=12.25,
    //   Boundary.knots=c(0,24.5), degree=3, intercept=TRUE)
    val times = Vector(0.0, 2.45, 6.125, 12.25, 22.05, 24.5)
    val expected = Vector(
      Vector(1.0, 0.0, 0.0, 0.0, 0.0),
      Vector(0.512, 0.434, 0.052, 0.002, 0.0),
      Vector(0.125, 0.59375, 0.25, 0.03125, 0.0),
      Vector(0.0, 0.25, 0.5, 0.25, 0.0),
      Vector(0.0, 0.002, 0.052, 0.434, 0.512),
      Vector(0.0, 0.0, 0.0, 0.0, 1.0)
    )
    val hrf = Hrfs.bspline(5, 24.5.s, convention = Hrfs.BsplineConvention.Complete)
    times.zip(expected).foreach { case (time, row) =>
      hrf(Lag(time)).data.zip(row).foreach { case (actual, value) =>
        assertEqualsDouble(actual, value, 2e-14)
      }
    }

  test("complete basis matches the generated splines::bs(intercept = TRUE) fixtures"):
    // 13-significant-digit serialization of values in [0, 1] bounds the
    // rounding error by 5e-14; times are serialized exactly as evaluated.
    fixtures.CompleteBsplineRFixtures.all.foreach { fixture =>
      val hrf = Hrfs.bspline(fixture.nBasis, Seconds(fixture.span), fixture.degree, convention = Hrfs.BsplineConvention.Complete)
      assertEquals(hrf.descriptor.nbasis, fixture.width, fixture.name)
      fixture.times.zipWithIndex.foreach { case (time, row) =>
        val actual = hrf(Lag(time)).data
        assertEquals(actual.length, fixture.width, fixture.name)
        for basis <- 0 until fixture.width do
          assertEqualsDouble(actual(basis), fixture.at(row, basis), 1e-12,
            s"${fixture.name} t=$time column ${basis + 1}")
      }
    }

  test("complete bases contain constants across degrees, widths and fractional spans"):
    for
      degree <- 0 to 4
      requested <- Vector(1, 5, 8)
      span <- Vector(0.5, 24.0, 24.5)
    do
      val hrf = Hrfs.bspline(requested, Seconds(span), degree, convention = Hrfs.BsplineConvention.Complete)
      val width = math.max(requested, degree + 1)
      assertEquals(hrf.descriptor.nbasis, width)
      for i <- 0 to 100 do
        val row = hrf(Lag(span * i / 100.0)).data
        assertEquals(row.length, width)
        assert(row.forall(_ >= -1e-14))
        assertEqualsDouble(row.sum, 1.0, 2e-14)
      for time <- Vector(-0.1, span + 0.1) do
        assertEqualsDouble(hrf(Lag(time)).data.sum, 0.0, 0.0)
        assertEqualsDouble(HrfFunctions.bsplineBasis(Lag(time), Seconds(span), requested,
          degree, convention = Hrfs.BsplineConvention.Complete).sum, 0.0, 0.0)

  test("minimum cubic basis is the analytic Bernstein basis"):
    val hrf = Hrfs.bspline(1, 0.5.s, convention = Hrfs.BsplineConvention.Complete)
    for i <- 0 to 20 do
      val u = i / 20.0
      val expected = Vector(math.pow(1 - u, 3), 3 * u * math.pow(1 - u, 2),
        3 * u * u * (1 - u), u * u * u)
      hrf(Lag(u * 0.5)).data.zip(expected).foreach { case (actual, value) =>
        assertEqualsDouble(actual, value, 2e-14)
      }

  test("whole-window weights integrate the complete basis and preserve a flat response"):
    val basis = ResponseBasis.of(Hrfs.bspline(5, 24.5.s, convention = Hrfs.BsplineConvention.Complete))
    val mean = basis.responseFunctional(ResponseFunctional.WindowMean(0.s, 24.5.s),
      FunctionalDiscretization.Exact).fold(error => fail(error.message), identity)
    // Integral of N_i,3 is (knot(i+4) - knot(i)) / 4.
    mean.values.zip(Vector(0.125, 0.25, 0.25, 0.25, 0.125)).foreach { case (actual, value) =>
      assertEqualsDouble(actual, value, 1e-12)
    }
    val fitted = basis.reconstruct(basis.coefficients(Vector.fill(5)(2.5)).toOption.get)
    for time <- Vector(0.0, 1.0, 12.25, 24.5) do
      assertEqualsDouble(fitted(Lag(time)).data(0), 2.5, 1e-12)

  test("complete and legacy descriptors distinguish different response spaces"):
    assertNotEquals(Hrfs.bspline().descriptor, Hrfs.bspline(convention = Hrfs.BsplineConvention.Complete).descriptor)
    assertEqualsDouble(Hrfs.bspline()(Lag(0)).data.sum, 0.0, 0.0)
    intercept[IllegalArgumentException](Hrfs.bspline(degree = -1, convention = Hrfs.BsplineConvention.Complete))

  test("legacy convention is onset-anchored only when it has interior knots"):
    // Why the legacy case is named for its R convention rather than for an
    // onset-zero property: at the minimum width it is the full Bernstein basis.
    val minimum = Hrfs.bspline(nBasis = 2, span = 8.s)
    assertEquals(minimum.descriptor.nbasis, 4)
    assertEqualsDouble(minimum(Lag(0)).data.sum, 1.0, 1e-14)
    assertEqualsDouble(minimum(Lag(3.0)).data.sum, 1.0, 1e-14)
    assertEqualsDouble(Hrfs.bspline(nBasis = 5, span = 8.s)(Lag(0)).data.sum, 0.0, 0.0)
