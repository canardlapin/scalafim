package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class RowGeometrySuite extends munit.FunSuite:

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

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertMatrixClose(actual, expected.toRows, tol)

  private def pass(cols: Int): FittedPreprocessor =
    FittedColumnAffine(cols, MatrixView.ones(cols), MatrixView.zeros(cols))

  private def cross(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    DoubleMatrix.transposeMultiply(left, right)

  test("block Cholesky row metric exposes whitening, unwhitening, and solve algebra") {
    val blocks = Vector(
      IndexSet.from(Vector(0, 1), IndexAxis.Row).toOption.get,
      IndexSet.from(Vector(2), IndexAxis.Row).toOption.get
    )
    val metric = RowMetric.blockCholesky(
      rows = 3,
      blocks = blocks,
      upperCholesky = Vector(
        DoubleMatrix.fromRows(Vector(Vector(2.0, 1.0), Vector(0.0, 3.0))),
        DoubleMatrix.fromRows(Vector(Vector(4.0)))
      )
    ).toOption.get
    val a = DoubleMatrix.fromRows(Vector(Vector(2.0, 1.0), Vector(3.0, 5.0), Vector(8.0, 4.0)))
    val b = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))

    val whitened = metric.whiten(a).toOption.get
    val unwhitened = metric.unwhiten(whitened).toOption.get
    val solved = metric.solve(b).toOption.get

    assertMatrixClose(unwhitened, a, 1e-10)
    assertMatrixClose(cross(metric.whiten(a).toOption.get, metric.whiten(b).toOption.get), cross(a, solved), 1e-10)
  }

  test("singular row metric Cholesky factors return a typed error") {
    val blocks = Vector(IndexSet.from(Vector(0), IndexAxis.Row).toOption.get)
    val result = RowMetric.blockCholesky(
      rows = 1,
      blocks = blocks,
      upperCholesky = Vector(DoubleMatrix.fromRows(Vector(Vector(0.0))))
    )

    assert(result.swap.toOption.exists {
      case MultivarError.SingularRowMetric(_) => true
      case _                                  => false
    })
  }

  test("orthogonal row projectors are symmetric idempotent and expose rank") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, -1.0),
        Vector(1.0, 0.0),
        Vector(1.0, 1.0)
      )
    )

    val projector = RowProjector.orthogonal(design).toOption.get
    val squared = DoubleMatrix.multiply(projector.matrix, projector.matrix)

    assertEquals(projector.rank, 2)
    assertMatrixClose(projector.matrix.transpose, projector.matrix, 1e-9)
    assertMatrixClose(squared, projector.matrix, 1e-9)
  }

  test("effect term fit reconstructs the fixed-effect projector algebra used by multivarious") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0, 1.0)
      )
    )
    val y = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 1.0),
        Vector(2.0, 2.0),
        Vector(3.0, 1.0),
        Vector(6.0, 2.0)
      )
    )
    val metric = RowMetric.identity(4).toOption.get
    val fit = EffectTermFit.fromDesign("group.level", design, Vector(3), metric).toOption.get
    val effectMatrix = fit.effectMatrix(y).toOption.get

    assertEquals(fit.term.df, 1)
    assertEquals(fit.termProjector.rank, 1)
    assertMatrixClose(
      effectMatrix,
      Vector(
        Vector(0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(0.5, 0.0)
      ),
      1e-9
    )
    assertMatrixClose(DoubleMatrix.multiply(fit.fullProjector.matrix, fit.nuisanceProjector.matrix), fit.nuisanceProjector.matrix, 1e-9)
  }

  test("effect operator reconstructs whitened and processed contributions") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 0.0),
        Vector(1.0, 0.0, 1.0, 0.0),
        Vector(1.0, 1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0, 1.0)
      )
    )
    val y = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 1.0),
          Vector(6.0, 2.0)
        )
      )
    )
    val metric = RowMetric.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("group.level", design, Vector(3), metric).toOption.get
    val basis = DoubleMatrix.eye(2)
    val operator = EffectOperator.fit(termFit, y, pass(2), basis).toOption.get

    assertEquals(operator.rank, 1)
    assertEqualsDouble(operator.singularValues(0), 1.0, 1e-9)
    assertMatrixClose(
      operator.reconstruct(EffectScale.Whitened).toOption.get,
      Vector(
        Vector(0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(-0.5, 0.0),
        Vector(0.5, 0.0)
      ),
      1e-9
    )
    assertMatrixClose(
      operator.reconstruct(EffectScale.Processed).toOption.get,
      operator.reconstruct(EffectScale.Whitened).toOption.get,
      1e-9
    )
  }

  test("aliased effect terms produce empty valid effect operators") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, 0.0, 0.0),
        Vector(1.0, 1.0, 1.0),
        Vector(1.0, 1.0, 1.0)
      )
    )
    val y = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 2.0),
          Vector(3.0, 4.0),
          Vector(5.0, 6.0),
          Vector(7.0, 8.0)
        )
      )
    )
    val metric = RowMetric.identity(4).toOption.get
    val termFit = EffectTermFit.fromDesign("aliased", design, Vector(2), metric).toOption.get
    val operator = EffectOperator.fit(termFit, y, pass(2), DoubleMatrix.eye(2)).toOption.get

    assertEquals(termFit.term.df, 0)
    assertEquals(operator.rank, 0)
    assertEquals(operator.scores.cols, 0)
    assertEquals(operator.loadings.cols, 0)
    assertMatrixClose(
      operator.reconstruct(EffectScale.Processed).toOption.get,
      Vector(
        Vector(0.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      ),
      1e-12
    )
  }
