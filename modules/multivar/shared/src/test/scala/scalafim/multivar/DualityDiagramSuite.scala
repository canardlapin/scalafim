package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

class DualityDiagramSuite extends munit.FunSuite:

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

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  private def dense(metric: MvMetric): DoubleMatrix =
    metric.toDense(StoragePolicy.AllowDense).toOption.get

  private def rowGramReference(x: DoubleMatrix, rowMetric: MvMetric): DoubleMatrix =
    DoubleMatrix.transposeMultiply(x, DoubleMatrix.multiply(dense(rowMetric), x))

  private def columnGramReference(x: DoubleMatrix, columnMetric: MvMetric): DoubleMatrix =
    DoubleMatrix.multiply(DoubleMatrix.multiply(x, dense(columnMetric)), x.transpose)

  private def rightMultiplyMetric(matrix: DoubleMatrix, metric: MvMetric): DoubleMatrix =
    DoubleMatrix.multiply(matrix, dense(metric))

  private def trace(matrix: DoubleMatrix): Double =
    assertEquals(matrix.rows, matrix.cols)
    var acc = 0.0
    var i = 0
    while i < matrix.rows do
      acc += matrix(i, i)
      i += 1
    acc

  private def positiveEigenvalues(matrix: DoubleMatrix, tol: Double): Vector[Double] =
    val eigen = DenseSolvers.symmetricEigen.decompose(matrix).toOption.get
    val out = Vector.newBuilder[Double]
    var i = 0
    while i < eigen.values.length do
      val value = eigen.values(i)
      if value > tol then out += value
      i += 1
    out.result()

  private val x = DoubleMatrix.fromRows(
    Vector(
      Vector(1.0, 2.0),
      Vector(0.0, 3.0),
      Vector(4.0, 1.0)
    )
  )

  test("duality diagram validates table spaces and metric tags") {
    val rowSpace = MvSpace.of("samples", SpaceRole.Samples, 3).toOption.get
    val columnSpace = MvSpace.of("features", SpaceRole.Observed, 2).toOption.get
    val rowMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0)), Some(rowSpace)).toOption.get
    val columnMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(3.0, 0.25)), Some(columnSpace)).toOption.get
    val diagram = DualityDiagram
      .from(MatrixView.dense(x), Some(rowMetric), Some(columnMetric), Some(rowSpace), Some(columnSpace))
      .toOption
      .get

    assertEquals(diagram.rows, 3)
    assertEquals(diagram.cols, 2)
    assertEquals(diagram.rowSpace, rowSpace)
    assertEquals(diagram.columnSpace, columnSpace)
    assertEquals(diagram.rowMetric.space, Some(rowSpace))
    assertEquals(diagram.columnMetric.space, Some(columnSpace))
  }

  test("duality diagram rejects metric dimension and space mismatches") {
    val rowSpace = MvSpace.of("samples", SpaceRole.Samples, 3).toOption.get
    val otherRowSpace = MvSpace.of("other-samples", SpaceRole.Samples, 3).toOption.get
    val wrongDim = MvMetric.identity(2).toOption.get
    val taggedElsewhere = MvMetric.identity(3, Some(otherRowSpace)).toOption.get

    DualityDiagram.from(MatrixView.dense(x), rowMetric = Some(wrongDim)) match
      case Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, expected, actual)) =>
        assertEquals(expected, 3)
        assertEquals(actual, 2)
      case other =>
        fail(s"expected row metric shape mismatch, got $other")

    DualityDiagram.from(MatrixView.dense(x), rowMetric = Some(taggedElsewhere), rowSpace = Some(rowSpace)) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("row metric space"), detail)
      case other =>
        fail(s"expected row metric space mismatch, got $other")
  }

  test("row and column Gram forms plus total inertia match direct dense algebra") {
    val rowMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0))).toOption.get
    val columnMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(3.0, 0.25))).toOption.get
    val diagram = DualityDiagram.from(MatrixView.dense(x), Some(rowMetric), Some(columnMetric)).toOption.get
    val rowGram = diagram.rowGram().toOption.get
    val columnGram = diagram.columnGram().toOption.get
    val rowOperator = diagram.rowOperator().toOption.get
    val columnOperator = diagram.columnOperator().toOption.get

    assertMatrixClose(rowGram, rowGramReference(x, rowMetric), 1e-12)
    assertMatrixClose(columnGram, columnGramReference(x, columnMetric), 1e-12)
    assertMatrixClose(rowOperator, rightMultiplyMetric(columnGram, rowMetric), 1e-12)
    assertMatrixClose(columnOperator, rightMultiplyMetric(rowGram, columnMetric), 1e-12)
    assertEqualsDouble(diagram.totalInertia().toOption.get, trace(columnOperator), 1e-12)
    assertEqualsDouble(trace(rowOperator), trace(columnOperator), 1e-12)
    assertEqualsDouble(trace(DoubleMatrix.multiply(rowOperator, rowOperator)), trace(DoubleMatrix.multiply(columnOperator, columnOperator)), 1e-9)
  }

  test("transposed duality diagram swaps the row and column forms") {
    val rowMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 0.5, 2.0))).toOption.get
    val columnMetric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(3.0, 0.25))).toOption.get
    val diagram = DualityDiagram.from(MatrixView.dense(x), Some(rowMetric), Some(columnMetric)).toOption.get
    val transposed = diagram.transpose().toOption.get

    assertEquals(transposed.rows, diagram.cols)
    assertEquals(transposed.cols, diagram.rows)
    assertMatrixClose(transposed.rowGram().toOption.get, diagram.columnGram().toOption.get, 1e-12)
    assertMatrixClose(transposed.columnGram().toOption.get, diagram.rowGram().toOption.get, 1e-12)
    assertEqualsDouble(transposed.totalInertia().toOption.get, diagram.totalInertia().toOption.get, 1e-12)
  }

  test("transposed duality diagram is lazy over sparse tables") {
    val sparse = SparseMatrixView.fromRows(x.toRows).toOption.get
    val diagram = DualityDiagram.from(sparse).toOption.get
    val transposed = diagram.transpose().toOption.get

    assertEquals(transposed.rows, diagram.cols)
    assertEquals(transposed.cols, diagram.rows)
    assertEquals(transposed.table.storage, StorageKind.Sparse)
    assertMatrixClose(transposed.rowGram().toOption.get, diagram.columnGram().toOption.get, 1e-12)
  }

  test("row and column dual operators share non-zero eigenvalues in identity geometry") {
    val diagram = DualityDiagram.from(MatrixView.dense(x)).toOption.get
    val rowEigen = positiveEigenvalues(diagram.rowOperator().toOption.get, 1e-10)
    val columnEigen = positiveEigenvalues(diagram.columnOperator().toOption.get, 1e-10)

    assertVectorClose(rowEigen, columnEigen, 1e-9)
  }
