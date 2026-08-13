package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

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

  private def matrix(values: Vector[Double]): DMat =
    Matrix.tabulate(values.length, 1)((row, _) => values(row))

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-10): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def valueOrFail[A](result: Either[ArError, A]): A =
    result.fold(error => fail(error.message), identity)

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

  test("checked stationarity enforcement contains overflow as a typed error") {
    val result = Pacf.enforceStationaryChecked(
      Vector(Double.MaxValue, Double.MaxValue),
      StationarityBound.Default
    )

    assert(result.left.toOption.exists {
      case ArError.NonFinitePartialAutocorrelation(_, value) => !value.isFinite
      case _                                                  => false
    })
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

  test("fixed order requests fail when censoring leaves no contributing lag pairs") {
    val residuals = matrix(Vector(1.0, -1.0, 2.0, -2.0))
    val segments = Vector(
      TimeSegment(0, 1, 0),
      TimeSegment(1, 2, 0),
      TimeSegment(2, 3, 0),
      TimeSegment(3, 4, 0)
    )
    val result = ArEstimation.fitNoise(
      residuals,
      segments,
      ArFitOptions(order = ArOrder.Fixed(1))
    )

    assert(result.left.toOption.exists {
      case ArError.ArOrderNotEstimable(requested, maxLag) =>
        requested.value == 1 && maxLag.value == 0
      case _ => false
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
    val gamma = valueOrFail(ArEstimation.autocovariance(matrix(values), segments, maxLag = 3))
    val typedGamma = valueOrFail(ArEstimation.autocovariances(matrix(values), segments, ArLag.unsafe(3)))
    val negated = valueOrFail(ArEstimation.autocovariance(matrix(values.map(-_)), segments, maxLag = 3))
    val scaled = valueOrFail(ArEstimation.autocovariance(matrix(values.map(_ * 3.0)), segments, maxLag = 3))

    assertEquals(typedGamma.maxLag, ArLag.unsafe(3))
    assertClose(typedGamma.toVector, gamma)
    assertClose(negated, gamma)
    assertClose(scaled, gamma.map(_ * 9.0))
  }

  test("censored rows cannot leak into AR estimation") {
    val values = ar1Series(0.6, 300)
    val excluded = (9 until 290 by 10).toSet
    val segments = TimeSegments.withCensorResets(TimeSegments.continuous(values.length), excluded)
    val layout = valueOrFail(NoiseEstimationLayout.excludingRows(segments, values.length, excluded))
    val spoiled = values.zipWithIndex.map { case (value, row) =>
      if excluded.contains(row) then value + 500.0 else value
    }
    val options = ArFitOptions(order = ArOrder.Fixed(2), exactFirstAr1 = false)

    val clean = valueOrFail(ArEstimation.fitNoise(matrix(values), layout, options))
    val contaminated = valueOrFail(ArEstimation.fitNoise(matrix(spoiled), layout, options))

    assertClose(clean.coefficients.head.phi, contaminated.coefficients.head.phi, tol = 1e-12)
    assertEquals(layout.excludedRows, excluded.toVector.sorted)
    assertEquals(layout.retainedRows, values.length - excluded.size)
    assertEquals(clean.nTimepoints, values.length)
  }

  test("short censor fragments retain a run-level mean") {
    val values = ar1Series(0.7, 400)
    val excluded = values.indices.filter(_ % 3 == 2).toSet
    val segments = TimeSegments.withCensorResets(TimeSegments.continuous(values.length), excluded)
    val layout = valueOrFail(NoiseEstimationLayout.excludingRows(segments, values.length, excluded))
    val plan = valueOrFail(
      ArEstimation.fitNoise(
        matrix(values),
        layout,
        ArFitOptions(order = ArOrder.Fixed(1), exactFirstAr1 = false)
      )
    )

    assert(plan.coefficients.head.phi.head > 0.4, clues(plan.coefficients.head.phi))
  }

  test("run offsets do not enter autocovariances or cross run boundaries") {
    val run = Vector(1.0, -1.0, 1.0, -1.0, 1.0)
    val centered = run ++ run
    val shifted = run.map(_ - 100.0) ++ run.map(_ + 250.0)
    val segments = TimeSegments.fromRunLengths(Vector(run.length, run.length))

    val reference = valueOrFail(ArEstimation.autocovariance(matrix(centered), segments, maxLag = 2))
    val withOffsets = valueOrFail(ArEstimation.autocovariance(matrix(shifted), segments, maxLag = 2))

    assertClose(withOffsets, reference, tol = 1e-12)

    val constantRuns = matrix(Vector.fill(5)(-20.0) ++ Vector.fill(5)(30.0))
    val constantGamma = valueOrFail(ArEstimation.autocovariance(constantRuns, segments, maxLag = 1))
    assertClose(constantGamma, Vector(0.0, 0.0), tol = 1e-12)
  }

  test("non-finite residuals are rejected with their matrix coordinate") {
    val residuals = Matrix.tabulate(4, 2) { (row, col) =>
      if row == 2 && col == 1 then Double.NaN else row.toDouble + col.toDouble
    }
    val result = ArEstimation.fitNoise(
      residuals,
      TimeSegments.continuous(residuals.rows),
      ArFitOptions(order = ArOrder.Fixed(1))
    )

    assert(result.left.toOption.exists {
      case ArError.NonFiniteResidual(2, 1, value) => value.isNaN
      case _                                      => false
    })
  }

  test("non-positive-definite autocovariance is repaired before Yule-Walker") {
    val original = Autocovariances.unsafe(Vector(1.0, 1.4, -0.8, 1.2))
    val repaired = ArEstimation.stabilizeAutocovariances(original)
    val estimate = valueOrFail(
      ArEstimation.yuleWalker(repaired, ArOrderValue.unsafe(3), StationarityBound.Default)
    )

    assert(!ArEstimation.isStrictlyPositiveDefinite(original.toVector))
    assert(ArEstimation.isStrictlyPositiveDefinite(repaired.toVector), clues(repaired.toVector))
    assertEqualsDouble(repaired.lagZero, original.lagZero, 1e-12)
    assert(repaired.toVector.tail.zip(original.toVector.tail).exists { case (a, b) => math.abs(a) < math.abs(b) })
    assert(Pacf.validateStationary(estimate.coefficients.phi).isRight, clues(estimate.coefficients.phi))
  }

  test("high-order stationarity enforcement leaves a strict root margin") {
    val unstable = Pacf.pacfToAr(Vector.fill(10)(0.99))
    val stable = valueOrFail(Pacf.enforceStationaryChecked(unstable, StationarityBound.Default))
    val recurrenceRadius = valueOrFail(Pacf.recurrenceRootMagnitude(stable))

    assert(stable.forall(_.isFinite))
    assert(recurrenceRadius < 1.0 / (1.0 + 1e-6), clues(recurrenceRadius, stable))
  }

  test("automatic order is sample-size bounded while fixed order is preserved") {
    val short = ar1Series(0.0, 11)
    val automatic = valueOrFail(
      ArEstimation.fitNoise(
        matrix(short),
        TimeSegments.continuous(short.length),
        ArFitOptions(order = ArOrder.Auto(8), exactFirstAr1 = false)
      )
    )
    val fixedValues = ar1Series(0.4, 25)
    val fixed = valueOrFail(
      ArEstimation.fitNoise(
        matrix(fixedValues),
        TimeSegments.continuous(fixedValues.length),
        ArFitOptions(order = ArOrder.Fixed(4), exactFirstAr1 = false)
      )
    )

    assert(automatic.arOrder <= short.length / 5, clues(automatic.arOrder))
    assertEquals(fixed.arOrder, 4)
  }

  test("global pooling weights run estimates by surviving observations") {
    val run1 = ar1Series(0.3, 400)
    val run2 = ar1Series(0.7, 600)
    val residuals = matrix(run1 ++ run2)
    val segments = TimeSegments.fromRunLengths(Vector(run1.length, run2.length))
    val perRun = valueOrFail(
      ArEstimation.fitNoise(
        residuals,
        segments,
        ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run, exactFirstAr1 = false)
      )
    )
    val global = valueOrFail(
      ArEstimation.fitNoise(
        residuals,
        segments,
        ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Global, exactFirstAr1 = false)
      )
    )
    val expected =
      (run1.length.toDouble * perRun.coefficients(0).phi.head +
        run2.length.toDouble * perRun.coefficients(1).phi.head) /
        (run1.length + run2.length).toDouble

    assertEqualsDouble(global.coefficients.head.phi.head, expected, 1e-12)
  }

  test("a fully censored run does not erase a global estimate") {
    val run1 = ar1Series(0.55, 120)
    val run2 = ar1Series(-0.4, 120)
    val values = run1 ++ run2
    val excluded = (run1.length until values.length).toSet
    val segments = TimeSegments.withCensorResets(
      TimeSegments.fromRunLengths(Vector(run1.length, run2.length)),
      excluded
    )
    val layout = valueOrFail(NoiseEstimationLayout.excludingRows(segments, values.length, excluded))
    val plan = valueOrFail(
      ArEstimation.fitNoise(
        matrix(values),
        layout,
        ArFitOptions(order = ArOrder.Fixed(2), pooling = NoisePooling.Global, exactFirstAr1 = false)
      )
    )

    assertEquals(plan.arOrder, 2)
    assert(plan.coefficients.head.phi.forall(_.isFinite))
    assert(Pacf.validateStationary(plan.coefficients.head.phi).isRight)
  }

  test("excluding every row produces a typed no-estimable-rows failure") {
    val values = ar1Series(0.4, 12)
    val excluded = values.indices.toSet
    val segments = TimeSegments.withCensorResets(TimeSegments.continuous(values.length), excluded)
    val layout = valueOrFail(NoiseEstimationLayout.excludingRows(segments, values.length, excluded))
    val result = ArEstimation.fitNoise(
      matrix(values),
      layout,
      ArFitOptions(order = ArOrder.Fixed(1))
    )

    assertEquals(layout.retainedRows, 0)
    assert(result.left.toOption.contains(ArError.NoEstimableRows))
  }

  test("run labels preserve time order and reject a reappearing run") {
    val descending = valueOrFail(TimeSegments.fromRunLabels(Vector("run-b", "run-b", "run-a", "run-a")))
    val repeated = TimeSegments.fromRunLabels(Vector("run-a", "run-b", "run-a"))

    assertEquals(descending, Vector(TimeSegment(0, 2, 0), TimeSegment(2, 4, 1)))
    assert(repeated.left.toOption.contains(ArError.NonContiguousRunLabel(0, 2)))
  }

  test("segment layouts reject skipped or returning canonical run indices") {
    val skipped = SegmentLayout.fromSegments(
      Vector(TimeSegment(0, 2, 0), TimeSegment(2, 4, 2))
    )
    val returning = SegmentLayout.fromSegments(
      Vector(TimeSegment(0, 1, 0), TimeSegment(1, 2, 1), TimeSegment(2, 3, 0))
    )

    assert(skipped.left.toOption.contains(ArError.NonContiguousRunIndex(1, 0, 2)))
    assert(returning.left.toOption.contains(ArError.NonContiguousRunIndex(2, 1, 0)))
  }

  test("run-aware diagnostics remove artificial between-run offsets") {
    val runLength = 100
    val offsets = Vector(-20.0, 8.0, 30.0, -12.0)
    val white = ar1Series(0.0, runLength * offsets.length)
    val values = offsets.zipWithIndex.flatMap { case (offset, run) =>
      white.slice(run * runLength, (run + 1) * runLength).map(_ + offset)
    }
    val residuals = matrix(values)
    val segments = TimeSegments.fromRunLengths(Vector.fill(offsets.length)(runLength))
    val unaware = AcorrDiagnostics.compute(residuals, maxLag = 5, aggregation = AcfAggregation.None)
    val aware = valueOrFail(
      AcorrDiagnostics.compute(residuals, segments, maxLag = 5, aggregation = AcfAggregation.None)
    )

    assert(unaware.acf(0, 0) > 0.5, clues(unaware.acf(0, 0)))
    assert(math.abs(aware.acf(0, 0)) < 0.15, clues(aware.acf(0, 0)))
  }

  test("run-aware diagnostics preserve the requested lag axis across short segments") {
    val residuals = matrix(Vector(1.0, -1.0, 2.0, -2.0))
    val segments = Vector(
      TimeSegment(0, 1, 0),
      TimeSegment(1, 2, 0),
      TimeSegment(2, 3, 0),
      TimeSegment(3, 4, 0)
    )
    val diagnostics = valueOrFail(
      AcorrDiagnostics.compute(residuals, segments, maxLag = 3, aggregation = AcfAggregation.None)
    )

    assertEquals(diagnostics.lags, Vector(1, 2, 3))
    assertClose(diagnostics.acf.col(0).toSeq.toVector, Vector(0.0, 0.0, 0.0))
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
    val residuals = Matrix.tabulate(6, 3)((_, _) => 2.0)

    Vector(AcfAggregation.None, AcfAggregation.Mean, AcfAggregation.Median).foreach { aggregation =>
      val diag = AcorrDiagnostics.compute(residuals, maxLag = 3, aggregation = aggregation)
      val values = diag.acf.valuesRowMajor.toVector

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
