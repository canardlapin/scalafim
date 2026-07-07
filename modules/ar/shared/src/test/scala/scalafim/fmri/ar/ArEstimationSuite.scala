package scalafim.fmri.ar

import scalafim.linalg.DoubleMatrix

class ArEstimationSuite extends munit.FunSuite:

  private def ar1Series(phi: Double, n: Int): Vector[Double] =
    val out = Array.ofDim[Double](n)
    var i = 0
    while i < n do
      val raw = math.sin((i + 1).toDouble * 12.9898 + 78.233) * 43758.5453
      val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
      out(i) = innovation + (if i == 0 then 0.0 else phi * out(i - 1))
      i += 1
    out.toVector

  private def matrix(values: Vector[Double]): DoubleMatrix =
    DoubleMatrix.fromRows(values.map(v => Vector(v)))

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-10): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  test("PACF and AR coefficients round-trip") {
    val kappa = Vector(0.2, -0.1, 0.3)
    val phi = Pacf.pacfToAr(kappa)
    val back = Pacf.arToPacf(phi)

    assertEquals(phi.length, 3)
    kappa.zip(back).foreach { case (a, b) =>
      assertEqualsDouble(a, b, 1e-10)
    }
  }

  test("PACF and AR coefficients round-trip across deterministic orders") {
    val cases = Vector(
      Vector.empty[Double],
      Vector(0.25),
      Vector(0.2, -0.15),
      Vector(0.3, -0.2, 0.1),
      Vector(0.35, -0.25, 0.15, -0.05),
      Vector(0.4, -0.3, 0.2, -0.1, 0.05)
    )

    cases.foreach { kappa =>
      val phi = Pacf.pacfToAr(kappa)
      val back = Pacf.arToPacf(phi)

      assertEquals(phi.length, kappa.length)
      assert(phi.forall(_.isFinite), clues(kappa, phi))
      assertClose(back, kappa)
    }
  }

  test("stationarity enforcement clips partial autocorrelations at the requested bound") {
    val unstable = Pacf.pacfToAr(Vector(1.4, -1.2, 0.7))
    val stable = Pacf.enforceStationary(unstable, bound = 0.8)
    val pacf = Pacf.arToPacf(stable)

    assert(stable.forall(_.isFinite), clues(stable))
    assert(pacf.forall(k => k.isFinite && math.abs(k) <= 0.8000000001), clues(pacf))
  }

  test("typed AR order, lag, and stationarity constructors reject invalid bounds") {
    assert(ArOrderValue(-1).left.toOption.contains(ArError.InvalidArOrder(-1)))
    assert(ArLag(-1).left.toOption.contains(ArError.InvalidArLag(-1)))
    assert(StationarityBound(1.0).left.toOption.contains(ArError.InvalidStationarityBound(1.0)))
  }

  test("Yule-Walker recovers a fixed-order AR(1) coefficient") {
    val values = ar1Series(0.6, 500)
    val residuals = matrix(values)
    val segments = TimeSegments.continuous(values.length)

    val plan = ArEstimation
      .fitNoise(residuals, segments, ArFitOptions(order = ArOrder.Fixed(1)))
      .toOption
      .get

    assertEquals(plan.method, WhiteningMethod.Estimated)
    assertEquals(plan.coefficients.head.arOrder, 1)
    assert(math.abs(plan.coefficients.head.phi.head - 0.6) < 0.08, clues(plan.coefficients.head.phi.head))
  }

  test("Yule-Walker clips near-unit estimates through the stationarity bound") {
    val values = ar1Series(0.98, 1400)
    val estimate = ArEstimation
      .fitNoise(
        matrix(values),
        TimeSegments.continuous(values.length),
        ArFitOptions(order = ArOrder.Fixed(1), stationarityBound = 0.9)
      )
      .toOption
      .get

    val phi = estimate.coefficients.head.phi
    assertEquals(phi.length, 1)
    assert(phi.head.isFinite)
    assert(math.abs(phi.head) <= 0.9000000001, clues(phi))
  }

  test("fixed order requests preserve coefficient length") {
    val values = ar1Series(0.5, 400)
    val estimate = ArEstimation
      .fitNoise(matrix(values), TimeSegments.continuous(values.length), ArFitOptions(order = ArOrder.Fixed(3)))
      .toOption
      .get

    assertEquals(estimate.coefficients.head.arOrder, 3)
  }

  test("fixed order requests fail when segments are too short to estimate the lag") {
    val result = ArEstimation.fitNoise(
      matrix(Vector(1.0, 2.0)),
      TimeSegments.continuous(2),
      ArFitOptions(order = ArOrder.Fixed(2))
    )

    assert(result.left.toOption.exists {
      case ArError.ArOrderNotEstimable(requested, maxLag) =>
        requested.value == 2 && maxLag.value == 1
      case _ =>
        false
    })
  }

  test("auto order selects a non-iid model for autocorrelated data") {
    val values = ar1Series(0.7, 600)
    val estimate = ArEstimation
      .fitNoise(matrix(values), TimeSegments.continuous(values.length), ArFitOptions(order = ArOrder.Auto(4)))
      .toOption
      .get

    assert(estimate.coefficients.head.arOrder >= 1)
    assert(estimate.coefficients.head.phi.forall(_.isFinite))
  }

  test("run pooling estimates one coefficient set per run") {
    val run1 = ar1Series(0.55, 600)
    val run2 = ar1Series(-0.3, 600)
    val values = run1 ++ run2
    val segments = TimeSegments.fromRunLengths(Vector(run1.length, run2.length))

    val plan = ArEstimation
      .fitNoise(
        matrix(values),
        segments,
        ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run)
      )
      .toOption
      .get

    assertEquals(plan.pooling, NoisePooling.Run)
    assertEquals(plan.coefficients.length, 2)
    assert(plan.coefficients(0).phi.head > 0.3)
    assert(plan.coefficients(1).phi.head < -0.1)
  }

  test("autocovariance is sign invariant and scales quadratically") {
    val values = Vector(-2.0, 0.5, 3.0, 1.0, -1.5, 4.0, 2.0)
    val segments = TimeSegments.continuous(values.length)
    val gamma = ArEstimation.autocovariance(matrix(values), segments, maxLag = 3)
    val typedGamma = ArEstimation.autocovariances(matrix(values), segments, ArLag.unsafe(3))
    val negated = ArEstimation.autocovariance(matrix(values.map(-_)), segments, maxLag = 3)
    val scaled = ArEstimation.autocovariance(matrix(values.map(_ * 3.0)), segments, maxLag = 3)

    assertEquals(typedGamma.maxLag, ArLag.unsafe(3))
    assertClose(typedGamma.toVector, gamma)
    assertClose(negated, gamma)
    assertClose(scaled, gamma.map(_ * 9.0))
  }

  test("constant residuals produce finite zero AR estimates") {
    val residuals = matrix(Vector.fill(12)(4.0))
    val estimate = ArEstimation
      .estimateForSegments(
        residuals,
        TimeSegments.continuous(residuals.rows),
        ArFitOptions(order = ArOrder.Fixed(3))
      )
      .toOption
      .get

    assertEquals(estimate.coefficients.phi, Vector(0.0, 0.0, 0.0))
    assertEqualsDouble(estimate.innovationVariance, 0.0, 1e-12)
    assert(estimate.coefficients.phi.forall(_.isFinite))
  }

  test("ACF diagnostics match the manual lag-one correlation") {
    val residuals = matrix(Vector(1.0, 2.0, 3.0, 4.0))
    val diag = AcorrDiagnostics.compute(residuals, maxLag = 2, aggregation = AcfAggregation.None)

    assertEquals(diag.lags, Vector(1, 2))
    assertEquals(diag.acf.rows, 2)
    assertEquals(diag.acf.cols, 1)
    assertEqualsDouble(diag.acf(0, 0), 0.25, 1e-12)
    assertEqualsDouble(diag.confidenceInterval, 1.96 / 2.0, 1e-12)
  }

  test("ACF diagnostics stay finite for constant multivoxel residuals") {
    val residuals = DoubleMatrix.fromRows(Vector.fill(6)(Vector(2.0, 2.0, 2.0)))

    Vector(AcfAggregation.None, AcfAggregation.Mean, AcfAggregation.Median).foreach { aggregation =>
      val diag = AcorrDiagnostics.compute(residuals, maxLag = 3, aggregation = aggregation)
      val values = diag.acf.copyData.toVector

      assertEquals(diag.lags, Vector(1, 2, 3))
      assert(values.forall(_.isFinite), clues(aggregation, values))
      assert(values.forall(_ == 0.0), clues(aggregation, values))
    }
  }

  test("whitening with an estimated AR plan reduces lag-one residual autocorrelation") {
    val values = ar1Series(0.65, 700)
    val residuals = matrix(values)
    val segments = TimeSegments.continuous(values.length)
    val before = AcorrDiagnostics.compute(residuals, maxLag = 1, aggregation = AcfAggregation.None).acf(0, 0)

    val plan = ArEstimation
      .fitNoise(residuals, segments, ArFitOptions(order = ArOrder.Fixed(1), exactFirstAr1 = false))
      .toOption
      .get
    val whitened = WhiteningTransform.matrix(plan, residuals).toOption.get
    val after = AcorrDiagnostics.compute(whitened, maxLag = 1, aggregation = AcfAggregation.None).acf(0, 0)

    assert(math.abs(after) < math.abs(before), clues(before, after, plan.coefficients.head.phi))
  }
