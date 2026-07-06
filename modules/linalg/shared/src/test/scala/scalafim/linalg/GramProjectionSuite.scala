package scalafim.linalg

class GramProjectionSuite extends munit.FunSuite:

  test("coefficients use the orthonormal shortcut result when basis columns are orthonormal") {
    val basis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(0.0, 0.0)
        )
      )
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0, 10.0),
          Vector(4.0, 20.0),
          Vector(6.0, 30.0)
        )
      )

    val coef =
      GramProjection.coefficients(data, basis).fold(err => fail(err.message), identity)
    val shortcut = DoubleMatrix.transposeMultiply(basis, data)

    assertEquals(coef.toRows, shortcut.toRows)
    assertEquals(coef.toRows, Vector(Vector(2.0, 10.0), Vector(4.0, 20.0)))
  }

  test("non-orthonormal basis recovers least-squares coefficients") {
    val basis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val coefficients =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0, 10.0),
          Vector(4.0, 20.0)
        )
      )
    val data = DoubleMatrix.multiply(basis, coefficients)

    val actual =
      GramProjection.coefficients(data, basis).fold(err => fail(err.message), identity)

    actual.toRows.zip(coefficients.toRows).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, 1e-12)
      }
    }
  }

  test("ridge-capable Gram solve returns finite coefficients for collinear bases") {
    val basis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0)
        )
      )
    val data =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0),
          Vector(2.0),
          Vector(3.0)
        )
      )

    val withoutRidge =
      GramProjection.coefficients(data, basis)
    assert(withoutRidge.isLeft)

    val withRidge =
      GramProjection.coefficients(data, basis, ridge = 1e-6).fold(err => fail(err.message), identity)
    assertEquals(withRidge.rows, 2)
    assertEquals(withRidge.cols, 1)
    assert(withRidge.toRows.flatten.forall(_.isFinite))
  }

  test("project returns fitted data in original observation space") {
    val basis =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(0.0, 1.0)
        )
      )
    val coefficients =
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0),
          Vector(4.0)
        )
      )
    val data = DoubleMatrix.multiply(basis, coefficients)

    val projection =
      GramProjection.project(data, basis).fold(err => fail(err.message), identity)

    projection.fitted.toRows.zip(data.toRows).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, 1e-12)
      }
    }
  }
