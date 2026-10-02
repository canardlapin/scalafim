package scalafim.fmri.hrf

class TemporalDerivativeConventionSuite extends munit.FunSuite:
  test("SPM one-second convention is the declared canonical backward difference"):
    val finite = TemporalDerivativeConvention
      .derive(Hrfs.SPMG1, TemporalDerivativeConvention.SpmOneSecondBackwardDifference)
      .toOption
      .getOrElse(fail("finite-difference convention should derive"))
    Vector(0.0, 0.5, 1.0, 3.0, 7.0, 16.0).foreach { time =>
      val expected = Hrfs.SPMG1(Lag(time)).data(0) - Hrfs.SPMG1(Lag(time - 1.0)).data(0)
      assertEqualsDouble(finite(Lag(time)).data(0), expected, 0.0, s"difference at $time seconds")
    }
    assertEquals(finite.basisElements.head.role, BasisRole.TemporalDerivative)

  test("analytic convention remains the continuous SPMG derivative"):
    val analytic = TemporalDerivativeConvention
      .derive(Hrfs.SPMG1, TemporalDerivativeConvention.AnalyticSpmg)
      .toOption
      .getOrElse(fail("analytic convention should derive"))
    Vector(0.5, 3.0, 5.0, 8.0, 16.0).foreach { time =>
      assertEqualsDouble(analytic(Lag(time)).data(0), HrfFunctions.spmg1Deriv(Lag(time)), 2e-14)
    }
    assertEquals(analytic.basisElements.head.role, BasisRole.TemporalDerivative)

  test("analytic convention refuses a transformed canonical response"):
    import HrfCombinators.*
    val lagged = Hrfs.SPMG1.lag(Seconds(1.0))
    assert(TemporalDerivativeConvention.derive(lagged, TemporalDerivativeConvention.AnalyticSpmg).isLeft)

  test("temporal convention leaves the genuine SPMG dispersion component unchanged"):
    Vector(1.0, 3.0, 5.0, 10.0, 16.0).foreach { time =>
      assertEqualsDouble(
        Hrfs.SPMG3(Lag(time)).data(2),
        HrfFunctions.spmg1DispersionDeriv(Lag(time)),
        2e-14,
        s"dispersion at $time seconds"
      )
    }
