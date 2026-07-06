package scalafim.fmri.fit

import scalafim.linalg.DoubleMatrix

class OlsSuite extends munit.FunSuite:

  private def assertAllFinite(matrix: DoubleMatrix): Unit =
    assert(matrix.copyData.forall(_.isFinite), clues(matrix.toRows))

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
    assertEquals(fit.residualDegreesOfFreedom, 2)
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

    assertEquals(fit.residualDegreesOfFreedom, 2)
    assertEqualsDouble(fit.coefficients(0, 0), 0.9, 1e-10)
    assertEqualsDouble(fit.coefficients(1, 0), 0.9, 1e-10)
    assertEqualsDouble(fit.residualVariance(0), 0.35, 1e-10)
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

  test("OLS rejects nearly collinear designs at the Cholesky tolerance boundary") {
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

    assertEquals(fit.residualDegreesOfFreedom, 3)
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
