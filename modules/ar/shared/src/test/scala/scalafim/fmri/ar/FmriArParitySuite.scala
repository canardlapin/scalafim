package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

class FmriArParitySuite extends munit.FunSuite:

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

  test("PACF conversion matches fmriAR helper output") {
    val kappa = Vector(0.25, -0.1, 0.2)
    val expectedPhi = Vector(
      0.29500000000000004,
      -0.15500000000000003,
      0.20000000000000001
    )
    val expectedBack = Vector(
      0.25000000000000006,
      -0.10000000000000002,
      0.20000000000000001
    )
    val unstable = Pacf.pacfToAr(Vector(1.4, -1.2, 0.7))
    val expectedStable = Vector(
      -0.88000000000000023,
      0.20799999999999996,
      0.69999999999999996
    )

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

    val expectedX = matrix(
      Vector(
        Vector(1.0, 1.0, 0.0),
        Vector(0.45000000000000001, 1.45, 1.0),
        Vector(1.0, 3.0, 0.0),
        Vector(0.45000000000000001, 2.3500000000000001, 1.0),
        Vector(1.0, 5.0, 1.0),
        Vector(0.45000000000000001, 3.25, -0.55000000000000004),
        Vector(0.66000000000000003, 4.75, 1.2100000000000002),
        Vector(1.0, 8.0, 0.0)
      )
    )
    val expectedY = matrix(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.45, 1.0),
        Vector(4.0, 1.0),
        Vector(4.7999999999999998, 1.45),
        Vector(1.0, 1.0),
        Vector(2.4499999999999997, 1.45),
        Vector(3.5600000000000001, 2.1099999999999999),
        Vector(8.0, 5.0)
      )
    )

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
    val expectedNone = matrix(
      Vector(
        Vector(-0.0098039215686274491, -0.0098039215686274491),
        Vector(-0.66666666666666652, -0.66666666666666685),
        Vector(-0.029411764705882342, -0.029411764705882356)
      )
    )

    assertEquals(none.lags, Vector(1, 2, 3))
    assertMatrixClose(none.acf, expectedNone)
    assertEqualsDouble(none.confidenceInterval, 0.80016664930917158, 1e-12)

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

    assertClose(median.acf.col(0).toSeq.toVector, Vector(-0.0098039215686274491, -0.66666666666666652))
    assertEqualsDouble(median.confidenceInterval, 0.80016664930917158, 1e-12)
  }

  test("fixed AR(1) estimation matches fmriAR fit_noise on a stable deterministic series") {
    val values = Vector(
      0.4801696483973501,
      0.39288356195620511,
      0.12558448074114043,
      -0.38612706117446371,
      0.43452753754339596,
      0.74467469542360953,
      0.41708286304387504,
      0.27627162953246176,
      0.19550347314594618,
      -0.85268289486611282,
      -0.28429628811595231,
      0.065872502651131981,
      -0.16112459073465857,
      0.60950890905917421,
      0.016836331954432016,
      -0.14592877149866851,
      -0.60464975504093599,
      0.64206680041053854,
      0.77310149031943698,
      1.1563170236945068,
      1.3398933301660652,
      -0.17614675302192817,
      0.15924145658477434,
      0.95462884101509238
    )
    val residuals = matrix(values.map(v => Vector(v)))
    val segments = TimeSegments.continuous(values.length)

    val gamma = ArEstimation.autocovariance(residuals, segments, maxLag = 1)
    val plan = ArEstimation
      .fitNoise(residuals, segments, ArFitOptions(order = ArOrder.Fixed(1), exactFirstAr1 = false))
      .toOption
      .get

    assertClose(gamma, Vector(0.27919527280158629, 0.087990088623818852))
    assertEquals(plan.method, WhiteningMethod.Estimated)
    assertEquals(plan.coefficients.head.arOrder, 1)
    assertEqualsDouble(plan.coefficients.head.phi.head, 0.31515608319898075, 1e-12)
  }
