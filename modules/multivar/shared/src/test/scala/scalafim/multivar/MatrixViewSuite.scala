package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class MatrixViewSuite extends munit.FunSuite:

  private def matrix: DoubleMatrix =
    DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )
    )

  private def assertMatrixClose(actual: DoubleMatrix, expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.headOption.map(_.length).getOrElse(0))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row)(col), tol)
        col += 1
      row += 1

  test("dense MatrixView computes column stats without changing storage") {
    val view = MatrixView.dense(matrix)
    val stats = view.columnStats.toOption.get
    val means = stats.means.toOption.get

    assertEquals(view.storage, StorageKind.Dense)
    assertEquals(stats.count, 2)
    assertEqualsDouble(stats.sums(0), 5.0, 1e-12)
    assertEqualsDouble(stats.sums(1), 7.0, 1e-12)
    assertEqualsDouble(stats.sumSquares(2), 45.0, 1e-12)
    assertEqualsDouble(means(0), 2.5, 1e-12)
    assertEqualsDouble(means(2), 4.5, 1e-12)
  }

  test("dense MatrixView exposes right multiplication and cross products") {
    val view = MatrixView.dense(matrix)
    val weights = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )

    assertMatrixClose(
      view.rightMultiply(weights).toOption.get,
      Vector(Vector(4.0, 5.0), Vector(10.0, 11.0)),
      1e-12
    )

    assertMatrixClose(
      view.crossProduct.toOption.get,
      Vector(
        Vector(17.0, 22.0, 27.0),
        Vector(22.0, 29.0, 36.0),
        Vector(27.0, 36.0, 45.0)
      ),
      1e-12
    )
  }

  test("dense MatrixView selects ordered feature columns") {
    val view = MatrixView.dense(matrix)
    val selected = view.selectColumns(IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get).toOption.get

    assertEquals(selected.rows, 2)
    assertEquals(selected.cols, 2)
    assertEquals(selected.storage, StorageKind.Dense)
    assertMatrixClose(
      selected.toDense().toOption.get,
      Vector(Vector(3.0, 1.0), Vector(6.0, 4.0)),
      1e-12
    )
  }

  test("dense MatrixView reports shape and non-finite errors") {
    val view = MatrixView.dense(matrix)
    val badWeights = DoubleMatrix.fromRows(Vector(Vector(1.0)))
    val nonFinite = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, Double.NaN))))

    assert(view.rightMultiply(badWeights).swap.toOption.exists(_.message.contains("expected 3 weight rows")))
    assert(view.selectColumns(IndexSet.from(Vector(3), IndexAxis.Feature).toOption.get).isLeft)
    assert(nonFinite.columnStats.swap.toOption.exists(_.message.contains("not finite")))
  }

