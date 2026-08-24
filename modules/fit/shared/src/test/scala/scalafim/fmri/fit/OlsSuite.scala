package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.design.RankToleranceConvention
import scalafim.fmri.hrf.BasisTransform
import scalafim.fmri.hrf.linalg.Mat

import gale.linalg.DMat

class OlsSuite extends munit.FunSuite:

  private def assertAllFinite(matrix: DMat): Unit =
    assert(matrix.copyData.forall(_.isFinite), clues(matrix.toRows))

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  private def linearEstimate(fit: OlsFit, weights: Vector[Double]): Double =
    weights.indices.map(index => weights(index) * fit.coefficients(index, 0)).sum

  private def linearVariance(fit: OlsFit, weights: Vector[Double]): Double =
    weights.indices.flatMap { row =>
      weights.indices.map(col => weights(row) * fit.normalizedCovariance(row, col) * weights(col))
    }.sum

  private def fitted(row: Vector[Double], fit: OlsFit): Double =
    row.indices.map(index => row(index) * fit.coefficients(index, 0)).sum

  test("OLS fits multiple response columns with one prepared factorization") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, 1.0),
          Vector(5.0, 0.0),
          Vector(7.0, -1.0)
        )
      )
    )

    val fit = Ols.unsafePrepare(design).unsafeFit(response)

    assertEquals(fit.predictors, 2)
    assertEquals(fit.voxels, 2)
    assertEquals(fit.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEquals(fit.diagnostics.solveMethod, OlsSolveMethod.QrRankRevealing)
    assertEquals(fit.diagnostics.policy.rankPolicy, OlsRankPolicy.StrictFullRank)
    assertEquals(fit.diagnostics.predictors, 2)
    assertEquals(fit.diagnostics.rank, 2)
    assert(fit.diagnostics.fullRank)
    assertEquals(fit.diagnostics.rankReport.method, RankDiagnosticMethod.PivotedQr)
    assertEquals(fit.diagnostics.rankReport.toleranceConvention, RankToleranceConvention.ScaleAware)
    assertEquals(fit.diagnostics.rankReport.numericalRank, 2)
    assertEquals(fit.diagnostics.rankReport.pivotOrder.toSet, Set(0, 1))
    assertEquals(fit.diagnostics.rankReport.aliasedPredictors, Vector.empty)
    assert(fit.diagnostics.rankReport.conditionEstimate.exists(_.isFinite))
    assertEqualsDouble(fit.coefficients(0, 0), 1.0, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 0), 2.0, 1e-10)
    assertEqualsDouble(fit.coefficients(0, 1), 2.0, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 1), -1.0, 1e-10)
    assertEqualsDouble(fit.residualVariance(0), 0.0, 1e-10)
    assertEqualsDouble(fit.residualVariance(1), 0.0, 1e-10)
  }

  test("basis reparameterization transports fit covariance and linear hypotheses") {
    val rows = Vector(
      Vector(1.0, -2.0),
      Vector(1.0, -1.0),
      Vector(1.0, 0.5),
      Vector(1.0, 2.0),
      Vector(1.0, 3.0),
      Vector(1.0, 4.0)
    )
    val residuals = Vector(0.20, -0.10, 0.15, -0.05, 0.08, -0.12)
    val responseRows = rows.zip(residuals).map { case (row, residual) =>
      Vector(1.25 * row(0) - 0.75 * row(1) + residual)
    }
    val transform = BasisTransform.Diagonal(Vector(2.0, 0.5))
    val transformedRows = rows.map(row => Vector(row(0) / 2.0, row(1) / 0.5))
    val original = Ols.unsafeFit(
      DesignMatrix.unsafe(GaleTestMatrix.fromRows(rows)),
      ResponseBlock.unsafe(GaleTestMatrix.fromRows(responseRows))
    )
    val transformed = Ols.unsafeFit(
      DesignMatrix.unsafe(GaleTestMatrix.fromRows(transformedRows)),
      ResponseBlock.unsafe(GaleTestMatrix.fromRows(responseRows))
    )

    assertEqualsDouble(transformed.coefficients(0, 0), original.coefficients(0, 0) * 2.0, 1e-10)
    assertEqualsDouble(transformed.coefficients(1, 0), original.coefficients(1, 0) * 0.5, 1e-10)

    val transportedCovariance = transform
      .transportCovariance(Mat.unsafe(2, 2, original.normalizedCovariance.copyData))
      .toOption
      .getOrElse(fail("fit covariance should transport through an invertible basis change"))
    var row = 0
    while row < 2 do
      var col = 0
      while col < 2 do
        assertEqualsDouble(transformed.normalizedCovariance(row, col), transportedCovariance(row, col), 1e-10)
        col += 1
      row += 1

    val weights = Vector(1.0, -0.25)
    val transportedWeights = transform
      .transportLinearWeights(weights)
      .toOption
      .getOrElse(fail("linear hypothesis should transport through an invertible basis change"))
    assertEqualsDouble(
      linearEstimate(original, weights),
      linearEstimate(transformed, transportedWeights),
      1e-10
    )
    assertEqualsDouble(
      linearVariance(original, weights),
      linearVariance(transformed, transportedWeights),
      1e-10
    )
    rows.indices.foreach { timepoint =>
      assertEqualsDouble(
        fitted(rows(timepoint), original),
        fitted(transformedRows(timepoint), transformed),
        1e-10
      )
    }
  }

  test("OLS reports residual variance per voxel") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(2.0), Vector(4.0)))
    )

    val fit = Ols.unsafeFit(design, response)

    assertEquals(fit.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(fit.coefficients(0, 0), 0.9, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 0), 0.9, 1e-10)
    assertEqualsDouble(fit.residualVariance(0), 0.35, 1e-10)
  }

  test("OLS matches independent Gaussian oracle on overdetermined multivoxel data") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, -2.0, 0.25),
          Vector(1.0, -1.0, 1.5),
          Vector(1.0, 0.0, -0.5),
          Vector(1.0, 1.0, 0.75),
          Vector(1.0, 2.0, -1.25),
          Vector(1.0, 3.0, 2.25)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(3.0, -1.5),
          Vector(2.2, 0.25),
          Vector(0.8, 1.75),
          Vector(1.6, 2.5),
          Vector(3.4, 1.0),
          Vector(5.1, 5.5)
        )
      )
    )

    val fit = Ols.unsafeFit(design, response)
    val oracle = LeastSquaresOracle.coefficients(design.value, response.value)

    assertMatrixClose(fit.coefficients.value, oracle, tol = 1e-10)
  }

  test("OLS matches independent oracle for scaled full-rank predictors") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, -0.003, 4.0),
          Vector(1.0, -0.002, 1.0),
          Vector(1.0, -0.001, 0.25),
          Vector(1.0, 0.0, 0.0),
          Vector(1.0, 0.001, 0.25),
          Vector(1.0, 0.002, 1.0),
          Vector(1.0, 0.003, 4.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(-2.0, 7.5),
          Vector(-0.5, 3.0),
          Vector(0.2, 1.2),
          Vector(0.1, 0.3),
          Vector(0.4, 1.0),
          Vector(1.5, 2.5),
          Vector(3.2, 8.0)
        )
      )
    )

    val fit = Ols.unsafeFit(design, response)
    val oracle = LeastSquaresOracle.coefficients(design.value, response.value)

    assertMatrixClose(fit.coefficients.value, oracle, tol = 1e-8)
  }

  test("scale-aware rank detection is invariant to uniform predictor scaling") {
    val ordinaryRows = Vector(
      Vector(1.0, -2.0),
      Vector(1.0, -1.0),
      Vector(1.0, 0.0),
      Vector(1.0, 1.0),
      Vector(1.0, 2.0)
    )
    val tinyRows = ordinaryRows.map(_.map(_ * 1.0e-12))
    val ordinary = Ols.prepare(DesignMatrix.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(ordinaryRows)))
    val tiny = Ols.prepare(DesignMatrix.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(tinyRows)))

    assertEquals(ordinary.toOption.map(_.diagnostics.rank), Some(2))
    assertEquals(tiny.toOption.map(_.diagnostics.rank), Some(2))
    assertEquals(
      tiny.toOption.map(_.diagnostics.rankReport.toleranceConvention),
      Some(RankToleranceConvention.ScaleAware)
    )
    assert(tiny.toOption.exists(_.diagnostics.rankReport.tolerance < 1.0e-20))

    val explicitAbsolute = Ols.prepare(
      DesignMatrix.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(tinyRows)),
      OlsSolvePolicy(rankTolerance = OlsRankTolerance.Absolute(1.0e-7))
    )
    assert(explicitAbsolute.left.toOption.exists {
      case FitError.RankDeficientDesign(report) =>
        report.toleranceConvention == RankToleranceConvention.Absolute && report.numericalRank == 0
      case _ => false
    })
  }

  test("OLS rejects non-finite dense inputs at construction") {
    val badDesign = DesignMatrix.fromMatrix(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(1.0, Double.NaN), Vector(1.0, 2.0)))
    )
    val badResponse = ResponseBlock.fromMatrix(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(Double.PositiveInfinity), Vector(3.0)))
    )

    assertEquals(badDesign.left.toOption, Some(FitError.NonFiniteInput("design matrix")))
    assertEquals(badResponse.left.toOption, Some(FitError.NonFiniteInput("response block")))
  }

  test("OLS default QR solver agrees with normal equations on well-conditioned designs") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, -2.0, 4.0),
          Vector(1.0, -1.0, 1.0),
          Vector(1.0, 0.0, 0.0),
          Vector(1.0, 1.0, 1.0),
          Vector(1.0, 2.0, 4.0),
          Vector(1.0, 3.0, 9.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(3.1, -7.0),
          Vector(2.2, -4.1),
          Vector(1.0, -1.0),
          Vector(1.1, 1.9),
          Vector(2.0, 5.2),
          Vector(4.1, 8.1)
        )
      )
    )

    val qr = Ols.unsafeFit(design, response)
    val normal = Ols.unsafeFit(design, response, OlsSolvePolicy.NormalEquations)

    assertEquals(qr.residualDegreesOfFreedom, normal.residualDegreesOfFreedom)
    assertEquals(qr.diagnostics.solveMethod, OlsSolveMethod.QrRankRevealing)
    assertEquals(normal.diagnostics.solveMethod, OlsSolveMethod.CholeskyNormalEquations)
    assertEquals(qr.diagnostics.rank, design.predictors)
    assertEquals(normal.diagnostics.rank, design.predictors)
    assertMatrixClose(qr.coefficients.value, normal.coefficients.value, tol = 1e-11)
    assertMatrixClose(qr.normalizedCovariance, normal.normalizedCovariance, tol = 1e-11)
    assertMatrixClose(qr.standardErrors.value, normal.standardErrors.value, tol = 1e-11)
  }

  test("OLS coefficients are invariant to design column permutation") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, -2.0, 0.5),
          Vector(1.0, -1.0, -1.0),
          Vector(1.0, 0.0, 0.0),
          Vector(1.0, 1.0, 1.0),
          Vector(1.0, 2.0, -0.5),
          Vector(1.0, 3.0, 2.0)
        )
      )
    )
    val beta = scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(2.0, -3.0),
        Vector(0.5, 1.5),
        Vector(-1.0, 0.25)
      )
    )
    val response = ResponseBlock.unsafe(design.value * beta)
    val permuted = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(design.value.toRows.map(row => Vector(row(2), row(0), row(1))))
    )

    val originalFit = Ols.unsafeFit(design, response)
    val permutedFit = Ols.unsafeFit(permuted, response)

    assertEqualsDouble(permutedFit.coefficients(0, 0), originalFit.coefficients(2, 0), 1e-10)
    assertEqualsDouble(permutedFit.coefficients(1, 0), originalFit.coefficients(0, 0), 1e-10)
    assertEqualsDouble(permutedFit.coefficients(2, 0), originalFit.coefficients(1, 0), 1e-10)
    assertEqualsDouble(permutedFit.coefficients(0, 1), originalFit.coefficients(2, 1), 1e-10)
    assertEqualsDouble(permutedFit.coefficients(1, 1), originalFit.coefficients(0, 1), 1e-10)
    assertEqualsDouble(permutedFit.coefficients(2, 1), originalFit.coefficients(1, 1), 1e-10)
  }

  test("OLS rejects singular designs") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(1.0, 1.0),
          Vector(1.0, 1.0)
        )
      )
    )

    val result = Ols.prepare(design)
    assert(result.left.toOption.exists(_.isInstanceOf[FitError.RankDeficientDesign]))
    val report = result.left.toOption.collect { case FitError.RankDeficientDesign(value) => value }.getOrElse(fail("rank report missing"))
    assertEquals(report.numericalRank, 1)
    assertEquals(report.predictorCount, 2)
    assertEquals(report.pivotOrder.toSet, Set(0, 1))
    assertEquals(report.aliasedPredictors.length, 1)
    assertEquals(report.independentPredictors.length, 1)
  }

  test("OLS rank policy makes strict full-rank rejection explicit") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(2.0, 4.0),
          Vector(3.0, 6.0),
          Vector(4.0, 8.0)
        )
      )
    )

    val result = Ols.prepare(design, OlsSolvePolicy(rankPolicy = OlsRankPolicy.StrictFullRank))

    assert(result.left.toOption.exists {
      case FitError.RankDeficientDesign(report) =>
        report.numericalRank == 1 && report.predictorCount == 2 && report.aliasedPredictors.length == 1
      case _ => false
    })
  }

  test("OLS reports unsupported rank policies explicitly") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0)
        )
      )
    )

    val ridgePolicy = OlsSolvePolicy(rankPolicy = OlsRankPolicy.RidgeRegularized(1e-6))
    val ridge = Ols.prepare(design, ridgePolicy)
    assert(ridge.left.toOption.exists {
      case FitError.UnsupportedLeastSquaresPolicy(message) => message.contains("ridge regularized")
      case _                                               => false
    })

    val minimumNorm = Ols.prepare(design, OlsSolvePolicy(rankPolicy = OlsRankPolicy.MinimumNorm))
    assert(minimumNorm.left.toOption.exists {
      case FitError.UnsupportedLeastSquaresPolicy(message) => message.contains("minimum norm")
      case _                                               => false
    })
  }

  test("OLS rejects nearly collinear designs at the QR rank tolerance boundary") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, -2.0, -2.0 - 2e-14),
          Vector(1.0, -1.0, -1.0 - 1e-14),
          Vector(1.0, 0.0, 0.0),
          Vector(1.0, 1.0, 1.0 + 1e-14),
          Vector(1.0, 2.0, 2.0 + 2e-14)
        )
      )
    )

    assert(Ols.prepare(design).isLeft)
  }

  test("OLS rejects saturated fits before residual inference is represented") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0))))

    val result = Ols.fit(design, response)

    assertEquals(result.left.toOption, Some(FitError.NonPositiveResidualDegreesOfFreedom(0)))
  }

  test("OLS remains finite for extreme but well-conditioned predictor scales") {
    val x = Vector(-2.0e6, -1.0e6, 0.0, 1.0e6, 2.0e6)
    val design = DesignMatrix.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(x.map(value => Vector(1.0, value))))
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        x.map { value =>
          Vector(
            3.0 + 2.0e-6 * value,
            -4.0 - 1.5e-6 * value
          )
        }
      )
    )

    val fit = Ols.unsafeFit(design, response)

    assertEquals(fit.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(3))
    assertEqualsDouble(fit.coefficients(0, 0), 3.0, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 0), 2.0e-6, 1e-18)
    assertEqualsDouble(fit.coefficients(0, 1), -4.0, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 1), -1.5e-6, 1e-18)
    assertAllFinite(fit.coefficients.value)
    assertAllFinite(fit.normalizedCovariance)
    assertAllFinite(fit.standardErrors.value)
    assert(fit.residualVariance.toVector.forall(_.isFinite))
  }

  test("OLS rejects row mismatches at fit time") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(1.0), Vector(1.0)))
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0)))
    )

    val result = Ols.unsafePrepare(design).fit(response)
    assertEquals(result.left.toOption, Some(FitError.RowMismatch(3, 2)))
  }
