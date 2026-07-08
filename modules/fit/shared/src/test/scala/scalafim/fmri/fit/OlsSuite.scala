package scalafim.fmri.fit

import scalafim.linalg.{DoubleMatrix, Ridge}

class OlsSuite extends munit.FunSuite:

  private def assertAllFinite(matrix: DoubleMatrix): Unit =
    assert(matrix.copyData.forall(_.isFinite), clues(matrix.toRows))

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  test("OLS fits multiple response columns with one prepared factorization") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(
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
    assertEqualsDouble(fit.coefficients(0, 0), 1.0, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 0), 2.0, 1e-10)
    assertEqualsDouble(fit.coefficients(0, 1), 2.0, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 1), -1.0, 1e-10)
    assertEqualsDouble(fit.residualVariance(0), 0.0, 1e-10)
    assertEqualsDouble(fit.residualVariance(1), 0.0, 1e-10)
  }

  test("OLS reports residual variance per voxel") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(2.0), Vector(4.0)))
    )

    val fit = Ols.unsafeFit(design, response)

    assertEquals(fit.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(fit.coefficients(0, 0), 0.9, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 0), 0.9, 1e-10)
    assertEqualsDouble(fit.residualVariance(0), 0.35, 1e-10)
  }

  test("OLS rejects non-finite dense inputs at construction") {
    val badDesign = DesignMatrix.fromMatrix(
      DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(1.0, Double.NaN), Vector(1.0, 2.0)))
    )
    val badResponse = ResponseBlock.fromMatrix(
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(Double.PositiveInfinity), Vector(3.0)))
    )

    assertEquals(badDesign.left.toOption, Some(FitError.NonFiniteInput("design matrix")))
    assertEquals(badResponse.left.toOption, Some(FitError.NonFiniteInput("response block")))
  }

  test("OLS default QR solver agrees with normal equations on well-conditioned designs") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
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
      DoubleMatrix.fromRows(
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
      DoubleMatrix.fromRows(
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
    val beta = DoubleMatrix.fromRows(
      Vector(
        Vector(2.0, -3.0),
        Vector(0.5, 1.5),
        Vector(-1.0, 0.25)
      )
    )
    val response = ResponseBlock.unsafe(DoubleMatrix.multiply(design.value, beta))
    val permuted = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(design.value.toRows.map(row => Vector(row(2), row(0), row(1))))
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
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(1.0, 1.0),
          Vector(1.0, 1.0)
        )
      )
    )

    assert(Ols.prepare(design).isLeft)
  }

  test("OLS rank policy makes strict full-rank rejection explicit") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
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
      case FitError.SingularDesign(cause) => cause.message.contains("rank 1") && cause.message.contains("required rank 2")
      case _                              => false
    })
  }

  test("OLS reports unsupported rank policies explicitly") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0)
        )
      )
    )

    val ridgePolicy = OlsSolvePolicy(rankPolicy = OlsRankPolicy.RidgeRegularized(Ridge(1e-6).toOption.get))
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
      DoubleMatrix.fromRows(
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
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0))))

    val result = Ols.fit(design, response)

    assertEquals(result.left.toOption, Some(FitError.NonPositiveResidualDegreesOfFreedom(0)))
  }

  test("OLS remains finite for extreme but well-conditioned predictor scales") {
    val x = Vector(-2.0e6, -1.0e6, 0.0, 1.0e6, 2.0e6)
    val design = DesignMatrix.unsafe(DoubleMatrix.fromRows(x.map(value => Vector(1.0, value))))
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(
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
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(1.0), Vector(1.0)))
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0)))
    )

    val result = Ols.unsafePrepare(design).fit(response)
    assertEquals(result.left.toOption, Some(FitError.RowMismatch(3, 2)))
  }
