package scalafim.linalg

class DoubleMatrixSuite extends munit.FunSuite:

  test("DoubleMatrix stores row-major values and exposes immutable copies") {
    val matrix = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
    assertEquals(matrix(1, 0), 3.0)

    val copy = matrix.copyData
    copy(0) = 99.0
    assertEquals(matrix(0, 0), 1.0)
  }

  test("transposeMultiply computes X transpose Y") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 1.0),
        Vector(1.0, 2.0)
      )
    )
    val y = DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(4.0), Vector(6.0)))

    val xty = DoubleMatrix.transposeMultiply(x, y)
    assertEquals(xty.rows, 2)
    assertEquals(xty.cols, 1)
    assertEquals(xty(0, 0), 12.0)
    assertEquals(xty(1, 0), 16.0)
  }

  test("transpose and crossProduct expose reusable row-major helpers") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )
    )

    assertEquals(x.transpose.toRows, Vector(Vector(1.0, 4.0), Vector(2.0, 5.0), Vector(3.0, 6.0)))

    val xtx = DoubleMatrix.crossProduct(x)
    assertEquals(xtx.toRows, Vector(Vector(17.0, 22.0, 27.0), Vector(22.0, 29.0, 36.0), Vector(27.0, 36.0, 45.0)))
  }

  test("Cholesky solves symmetric positive-definite systems") {
    val a = DoubleMatrix.fromRows(Vector(Vector(4.0, 2.0), Vector(2.0, 3.0)))
    val b = DoubleMatrix.fromRows(Vector(Vector(6.0), Vector(5.0)))

    val solved = Cholesky.decompose(a).map(_.solve(b))
    assert(solved.isRight)
    val x = solved.toOption.get
    assertEqualsDouble(x(0, 0), 1.0, 1e-10)
    assertEqualsDouble(x(1, 0), 1.0, 1e-10)
  }

  test("Cholesky rejects singular systems") {
    val a = DoubleMatrix.fromRows(Vector(Vector(1.0, 1.0), Vector(1.0, 1.0)))
    assert(Cholesky.decompose(a).isLeft)
  }
