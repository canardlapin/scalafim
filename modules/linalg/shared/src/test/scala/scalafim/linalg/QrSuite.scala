package scalafim.linalg

class QrSuite extends munit.FunSuite:

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

  test("QR residualizer removes full-rank design columns") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 1.0),
        Vector(1.0, 2.0),
        Vector(1.0, 3.0),
        Vector(1.0, 4.0)
      )
    )
    val data = DoubleMatrix.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(4.0), Vector(9.0), Vector(16.0)))

    val residualized = MatrixResidualizer.residualize(design, data)

    assertEquals(residualized.designRank, 2)
    assertMatrixClose(
      residualized.value,
      DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(-1.0), Vector(-2.0), Vector(-1.0), Vector(2.0))),
      tol = 1e-12
    )
  }

  test("QR residualizer uses numerical rank for rank-deficient designs") {
    val design = DoubleMatrix.fromRows(Vector(Vector(1.0, 1.0), Vector(1.0, 1.0), Vector(1.0, 1.0)))
    val data = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))

    val residualized = MatrixResidualizer.residualize(design, data)

    assertEquals(residualized.designRank, 1)
    assertMatrixClose(
      residualized.value,
      DoubleMatrix.fromRows(Vector(Vector(-1.0), Vector(0.0), Vector(1.0))),
      tol = 1e-12
    )
  }

  test("QR residualizer leaves data unchanged for empty designs") {
    val design = DoubleMatrix.zeros(4, 0)
    val data = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(-2.0, 3.0), Vector(3.0, 4.0), Vector(-4.0, 5.0)))

    val residualized = MatrixResidualizer.residualize(design, data)

    assertEquals(residualized.designRank, 0)
    assertMatrixClose(residualized.value, data, tol = 0.0)
    val copy = residualized.value.copyData
    copy(0) = 99.0
    assertEquals(data(0, 0), 1.0)
  }
