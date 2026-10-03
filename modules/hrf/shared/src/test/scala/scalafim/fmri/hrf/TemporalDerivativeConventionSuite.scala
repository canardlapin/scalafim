package scalafim.fmri.hrf

import scalafim.fmri.hrf.fixtures.SpmInformedBasisFixture

class TemporalDerivativeConventionSuite extends munit.FunSuite:
  private def grid(tr: Double): SpmKernelGrid =
    SpmKernelGrid(Seconds(tr)).fold(error => fail(error.message), identity)

  /** Compare our kernels on SPM's grid with spm_get_bf's columns. SPM columns
    * are sum-normalized; ours are in fmrihrf units, so one common factor taken
    * from the canonical column relates every column. */
  private def assertSpmParity(tr: Double, columns: Int, rows: Int, expected: Array[Double]): Unit =
    val basis = TemporalDerivativeConvention
      .spmInformedBasis(Hrfs.SPMG1, columns, grid(tr))
      .fold(error => fail(error.message), identity)
    val g = grid(tr)
    assertEquals(g.sampleCount, rows)
    val ours = g.times.map(t => basis(Lag(t)).data)
    val factor = ours.map(_(0)).sum / (0 until rows).map(r => expected(r * columns)).sum
    val peak = (0 until columns).map(c => (0 until rows).map(r => math.abs(expected(r * columns + c))).max)
    (0 until rows).foreach { r =>
      (0 until columns).foreach { c =>
        // Relative to each SPM column's peak; both sides are double-precision
        // evaluations of the same closed forms, so agreement is near rounding.
        assertEqualsDouble(ours(r)(c) / factor, expected(r * columns + c), 1e-11 * peak(c), s"TR=$tr row=$r column=$c")
      }
    }

  test("SPM-named temporal convention matches spm_get_bf + spm_orth on SPM's grid (TR 2 s, spmg3)"):
    assertSpmParity(SpmInformedBasisFixture.tr2Tr, SpmInformedBasisFixture.tr2Columns, SpmInformedBasisFixture.tr2Rows, SpmInformedBasisFixture.tr2)

  test("SPM-named temporal convention matches spm_get_bf at a non-dyadic TR (0.72 s, spmg2)"):
    assertSpmParity(SpmInformedBasisFixture.tr072Tr, SpmInformedBasisFixture.tr072Columns, SpmInformedBasisFixture.tr072Rows, SpmInformedBasisFixture.tr072)

  test("SPM informed columns are serially orthogonal on the kernel grid, not raw differences"):
    val g = grid(2.0)
    val basis = TemporalDerivativeConvention.spmInformedBasis(Hrfs.SPMG1, 3, g).fold(error => fail(error.message), identity)
    val values = g.times.map(t => basis(Lag(t)).data)
    def dot(a: Int, b: Int) = values.map(v => v(a) * v(b)).sum
    assertEqualsDouble(dot(0, 1) / math.sqrt(dot(0, 0) * dot(1, 1)), 0.0, 1e-13)
    assertEqualsDouble(dot(0, 2) / math.sqrt(dot(0, 0) * dot(2, 2)), 0.0, 1e-13)
    assertEqualsDouble(dot(1, 2) / math.sqrt(dot(1, 1) * dot(2, 2)), 0.0, 1e-13)
    assertEquals(basis.basisElements.map(_.role), Vector(BasisRole.Canonical, BasisRole.TemporalDerivative, BasisRole.DispersionDerivative))
    val raw = TemporalDerivativeConvention.rawOneSecondDifference(Hrfs.SPMG1).fold(error => fail(error.message), identity)
    // The raw difference is not orthogonal to the canonical, so the two differ.
    val rawDot = g.times.map(t => raw(Lag(t)).data(0) * Hrfs.SPMG1(Lag(t)).data(0)).sum
    assert(math.abs(rawDot) > 1e-3 * math.sqrt(dot(0, 0)) * math.sqrt(g.times.map(t => raw(Lag(t)).data(0) * raw(Lag(t)).data(0)).sum))

  test("derive returns the SPM temporal column and requires a kernel grid"):
    val g = grid(2.0)
    val temporal = TemporalDerivativeConvention
      .derive(Hrfs.SPMG1, TemporalDerivativeConvention.SpmOneSecondBackwardDifference, Some(g))
      .fold(error => fail(error.message), identity)
    val basis = TemporalDerivativeConvention.spmInformedBasis(Hrfs.SPMG1, 2, g).fold(error => fail(error.message), identity)
    Vector(0.0, 0.5, 1.0, 3.0, 7.0, 16.0).foreach { t =>
      assertEqualsDouble(temporal(Lag(t)).data(0), basis(Lag(t)).data(1), 0.0)
    }
    assertEquals(temporal.basisElements.head.role, BasisRole.TemporalDerivative)
    assertEquals(
      TemporalDerivativeConvention.derive(Hrfs.SPMG1, TemporalDerivativeConvention.SpmOneSecondBackwardDifference),
      Left(TemporalDerivativeConventionError.RequiresKernelGrid)
    )

  test("raw one-second difference stays available under an explicitly raw name"):
    val raw = TemporalDerivativeConvention.rawOneSecondDifference(Hrfs.SPMG1).fold(error => fail(error.message), identity)
    Vector(0.0, 0.5, 1.0, 3.0, 7.0, 16.0).foreach { time =>
      val expected = Hrfs.SPMG1(Lag(time)).data(0) - Hrfs.SPMG1(Lag(time - 1.0)).data(0)
      assertEqualsDouble(raw(Lag(time)).data(0), expected, 0.0, s"difference at $time seconds")
    }

  test("SPM kernel grids and informed widths are validated with typed errors"):
    assert(SpmKernelGrid(Seconds(0.0)).isLeft)
    assert(SpmKernelGrid(Seconds(2.0), microtimeResolution = 0).isLeft)
    assertEquals(
      TemporalDerivativeConvention.spmInformedBasis(Hrfs.SPMG1, 4, grid(2.0)),
      Left(TemporalDerivativeConventionError.UnsupportedInformedColumns(4))
    )
    assert(TemporalDerivativeConvention.spmInformedBasis(Hrfs.SPMG2, 2, grid(2.0)).isLeft)

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

  test("a point response readout records exact evaluation even under a declared window rule"):
    val step = PositiveSeconds(0.5).fold(error => fail(error.toString), identity)
    val weights = ResponseBasis.of(Hrfs.SPMG2)
      .responseFunctional(ResponseFunctional.At(6.0.s), FunctionalDiscretization.Trapezoid(step))
      .fold(error => fail(error.toString), identity)
    assertEquals(weights.receipt, FunctionalDiscretizationReceipt(FunctionalDiscretization.Exact, samples = 1, effectiveStep = None))
