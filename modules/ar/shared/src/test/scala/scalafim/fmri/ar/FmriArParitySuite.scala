package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.fixtures.FmriArRFixture

class FmriArParitySuite extends munit.FunSuite:

  private val estimationSeries = FmriArRFixture.estimationSeries

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double = 1e-12): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    actual.valuesRowMajor.zip(expected.valuesRowMajor).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    require(rows.nonEmpty && rows.forall(_.length == rows.head.length))
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def valueOrFail[A](result: Either[ArError, A]): A =
    result.fold(error => fail(error.message), identity)

  test("PACF conversion matches fmriAR helper output") {
    val kappa = Vector(0.25, -0.1, 0.2)
    val expectedPhi = FmriArRFixture.pacfPhi
    val expectedBack = FmriArRFixture.pacfBack
    val unstable = Pacf.pacfToAr(Vector(1.4, -1.2, 0.7))
    val expectedStable = FmriArRFixture.stablePhi

    val phi = Pacf.pacfToAr(kappa)

    assertClose(phi, expectedPhi)
    assertClose(Pacf.arToPacf(phi), expectedBack)
    assertClose(Pacf.enforceStationary(unstable, bound = 0.8), expectedStable)
  }

  test("ARMA whitening with runs and censor resets matches fmriAR whiten_apply") {
    val x = matrix(
      Vector(
        Vector(1.0, 1.0, 0.0),
        Vector(1.0, 2.0, 1.0),
        Vector(1.0, 3.0, 0.0),
        Vector(1.0, 4.0, 1.0),
        Vector(1.0, 5.0, 1.0),
        Vector(1.0, 6.0, 0.0),
        Vector(1.0, 7.0, 1.0),
        Vector(1.0, 8.0, 0.0)
      )
    )
    val y = matrix(
      Vector(
        Vector(1.0, 0.0),
        Vector(2.0, 1.0),
        Vector(4.0, 1.0),
        Vector(7.0, 2.0),
        Vector(1.0, 1.0),
        Vector(3.0, 2.0),
        Vector(5.0, 3.0),
        Vector(8.0, 5.0)
      )
    )
    val coefficients = ArmaCoefficients.arma(phi = Vector(0.35, -0.1), theta = Vector(0.2))
    val segments = TimeSegments.withCensorResets(TimeSegments.fromRunLengths(Vector(4, 4)), Set(1, 6))
    val plan = WhiteningPlan.byRun(
      coefficients = Vector(coefficients, coefficients),
      segments = segments,
      exactFirstAr1 = false
    )

    val out = WhiteningTransform(plan, x, y).toOption.get

    val expectedX = matrix(FmriArRFixture.expectedXRows)
    val expectedY = matrix(FmriArRFixture.expectedYRows)

    assertMatrixClose(out.design, expectedX)
    assertMatrixClose(out.response, expectedY)
  }

  test("ACF diagnostics match fmriAR exported helper fixtures") {
    val noneResiduals = matrix(
      Vector(
        Vector(0.0, 1.0),
        Vector(1.0, 0.0),
        Vector(0.0, -1.0),
        Vector(-1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 0.0)
      )
    )
    val none = AcorrDiagnostics.compute(noneResiduals, maxLag = 3, aggregation = AcfAggregation.None)
    val expectedNone = matrix(FmriArRFixture.acfNoneRows)

    assertEquals(none.lags, Vector(1, 2, 3))
    assertMatrixClose(none.acf, expectedNone)
    assertEqualsDouble(none.confidenceInterval, FmriArRFixture.acfNoneCi, 1e-12)

    val medianResiduals = matrix(
      Vector(
        Vector(0.0, 1.0, -1.0),
        Vector(2.0, 1.0, 0.0),
        Vector(0.0, -1.0, 1.0),
        Vector(-2.0, -1.0, 0.0),
        Vector(0.0, 1.0, -1.0),
        Vector(2.0, 1.0, 0.0)
      )
    )
    val median = AcorrDiagnostics.compute(medianResiduals, maxLag = 2, aggregation = AcfAggregation.Median)

    assertClose(median.acf.col(0).toSeq.toVector, FmriArRFixture.acfMedian)
    assertEqualsDouble(median.confidenceInterval, FmriArRFixture.acfMedianCi, 1e-12)
  }

  test("fixed AR(1)-AR(4) and automatic-order estimation match fmriAR 0.3.3") {
    val residuals = matrix(estimationSeries.map(v => Vector(v)))
    val segments = TimeSegments.continuous(estimationSeries.length)

    val gamma = valueOrFail(ArEstimation.autocovariance(residuals, segments, maxLag = 4))
    val expectedByOrder = FmriArRFixture.fitNoisePhiByOrder

    assertClose(
      gamma,
      FmriArRFixture.fitNoiseGamma
    )
    expectedByOrder.zipWithIndex.foreach { case (expected, index) =>
      val order = index + 1
      val plan = valueOrFail(
        ArEstimation.fitNoise(
          residuals,
          segments,
          ArFitOptions(order = ArOrder.Fixed(order), exactFirstAr1 = false)
        )
      )

      assertEquals(plan.method, WhiteningMethod.Estimated)
      assertEquals(plan.coefficients.head.arOrder, order)
      assertClose(plan.coefficients.head.phi, expected)
    }

    val automatic = valueOrFail(
      ArEstimation.fitNoise(
        residuals,
        segments,
        ArFitOptions(order = ArOrder.Auto(4), exactFirstAr1 = false)
      )
    )
    assertEquals(automatic.coefficients.head.arOrder, FmriArRFixture.fitNoiseAutoOrder)
    assertClose(automatic.coefficients.head.phi, FmriArRFixture.fitNoiseAutoPhi)
  }

  test("run-weighted censored AR estimates match fmriAR 0.3.3") {
    val residuals = matrix(estimationSeries.map(v => Vector(v)))
    val excluded = Set(3, 8, 13, 19)
    val segments = TimeSegments.withCensorResets(
      TimeSegments.fromRunLengths(Vector(12, 12)),
      excluded
    )
    val layout = valueOrFail(
      NoiseEstimationLayout.excludingRows(segments, estimationSeries.length, excluded)
    )
    val perRun = valueOrFail(
      ArEstimation.fitNoise(
        residuals,
        layout,
        ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run, exactFirstAr1 = false)
      )
    )
    val global = valueOrFail(
      ArEstimation.fitNoise(
        residuals,
        layout,
        ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Global, exactFirstAr1 = false)
      )
    )

    assertClose(perRun.coefficients.map(_.phi.head), FmriArRFixture.censoredRunPhi)
    assertClose(global.coefficients.head.phi, FmriArRFixture.censoredGlobalPhi)
  }
