package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

class GenPcaKernelsSuite extends munit.FunSuite:

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

  private def metricDense(metric: MvMetric): DoubleMatrix =
    metric.toDense(StoragePolicy.AllowDense).toOption.get

  private def bruteRowGram(xd: DoubleMatrix, metric: MvMetric): DoubleMatrix =
    DoubleMatrix.transposeMultiply(xd, DoubleMatrix.multiply(metricDense(metric), xd))

  private def bruteColGram(xd: DoubleMatrix, metric: MvMetric): DoubleMatrix =
    DoubleMatrix.multiply(DoubleMatrix.multiply(xd, metricDense(metric)), xd.transpose)

  private def bruteTrace(xd: DoubleMatrix, rowMetric: MvMetric, colMetric: MvMetric): Double =
    val gram = bruteRowGram(xd, rowMetric)
    val weighted = DoubleMatrix.multiply(gram, metricDense(colMetric))
    var acc = 0.0
    var i = 0
    while i < weighted.rows do
      acc += weighted(i, i)
      i += 1
    acc

  private val xDense = DoubleMatrix.fromRows(
    Vector(
      Vector(1.0, 0.0, 2.0),
      Vector(0.0, 3.0, 0.0),
      Vector(4.0, 0.0, 5.0),
      Vector(0.0, 6.0, 1.0)
    )
  )

  private val xSparse = SparseMatrixView.fromRows(xDense.toRows).toOption.get

  private def rowMetrics: Vector[(String, MvMetric)] =
    val spd = DoubleMatrix.fromRows(
      Vector(
        Vector(2.0, 0.5, 0.0, 0.0),
        Vector(0.5, 3.0, 0.0, 1.0),
        Vector(0.0, 0.0, 1.5, 0.0),
        Vector(0.0, 1.0, 0.0, 2.5)
      )
    )
    val sparse = SparseMatrixView.fromRows(spd.toRows).toOption.get
    Vector(
      "identity" -> MvMetric.identity(4).toOption.get,
      "diagonal" -> MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 2.0, 0.5, 1.5))).toOption.get,
      "dense" -> MvMetric.denseSymmetric(spd).toOption.get,
      "sparse" -> MvMetric.sparseSymmetric(sparse).toOption.get
    )

  private def colMetrics: Vector[(String, MvMetric)] =
    val spd = DoubleMatrix.fromRows(
      Vector(
        Vector(1.5, 0.0, 0.5),
        Vector(0.0, 2.0, 0.0),
        Vector(0.5, 0.0, 3.0)
      )
    )
    val sparse = SparseMatrixView.fromRows(spd.toRows).toOption.get
    Vector(
      "identity" -> MvMetric.identity(3).toOption.get,
      "diagonal" -> MvMetric.diagonal(DoubleVector.fromSeq(Vector(0.5, 2.0, 1.0))).toOption.get,
      "dense" -> MvMetric.denseSymmetric(spd).toOption.get,
      "sparse" -> MvMetric.sparseSymmetric(sparse).toOption.get
    )

  test("dense row Gram matches the brute-force reference for every metric kind") {
    for (label, metric) <- rowMetrics do
      val gram = GenPcaKernels.rowGram(MatrixView.dense(xDense), metric).toOption.get
      assertMatrixClose(gram, bruteRowGram(xDense, metric), 1e-10)
  }

  test("sparse row Gram stays sparse-driven for diagonal-like metrics and matches dense") {
    for (label, metric) <- rowMetrics.take(2) do
      val gram = GenPcaKernels.rowGram(xSparse, metric, StoragePolicy.PreserveSparse).toOption.get
      assertMatrixClose(gram, bruteRowGram(xDense, metric), 1e-10)
  }

  test("sparse row Gram with a non-diagonal metric is policy gated") {
    val metric = rowMetrics(2)._2
    assertEquals(
      GenPcaKernels.rowGram(xSparse, metric, StoragePolicy.PreserveSparse),
      Left(MultivarError.DensificationRejected("row Gram with a non-diagonal metric", StorageKind.Sparse))
    )
    val gram = GenPcaKernels.rowGram(xSparse, metric, StoragePolicy.AllowDense).toOption.get
    assertMatrixClose(gram, bruteRowGram(xDense, metric), 1e-10)
  }

  test("affine row Gram expands the lazy shift analytically for every metric kind") {
    val scale = DoubleVector.fromSeq(Vector(2.0, 0.5, -1.0))
    val shift = DoubleVector.fromSeq(Vector(1.0, -2.0, 0.5))
    val affine = MatrixView.affine(xSparse, scale, shift, StoragePolicy.Operator).toOption.get
    assertEquals(affine.storage, StorageKind.LazyAffine)
    val materialized = MatrixView.materializeAffine(xDense, scale, shift)

    for (label, metric) <- rowMetrics.take(2) do
      val gram = GenPcaKernels.rowGram(affine, metric, StoragePolicy.PreserveSparse).toOption.get
      assertMatrixClose(gram, bruteRowGram(materialized, metric), 1e-10)

    for (label, metric) <- rowMetrics.drop(2) do
      val rejected = GenPcaKernels.rowGram(affine, metric, StoragePolicy.PreserveSparse)
      assert(rejected.isLeft, s"affine over sparse base with $label metric must be policy gated")
      val gram = GenPcaKernels.rowGram(affine, metric, StoragePolicy.AllowDense).toOption.get
      assertMatrixClose(gram, bruteRowGram(materialized, metric), 1e-10)
  }

  test("dense column Gram matches the brute-force reference for every metric kind") {
    for (label, metric) <- colMetrics do
      val gram = GenPcaKernels.colGram(MatrixView.dense(xDense), metric).toOption.get
      assertMatrixClose(gram, bruteColGram(xDense, metric), 1e-10)
  }

  test("column Gram on sparse data is policy gated") {
    val metric = colMetrics(0)._2
    GenPcaKernels.colGram(xSparse, metric, StoragePolicy.PreserveSparse) match
      case Left(MultivarError.DensificationRejected("column Gram", StorageKind.Sparse)) => ()
      case other => fail(s"expected a densification rejection, got $other")
    val gram = GenPcaKernels.colGram(xSparse, metric, StoragePolicy.AllowDense).toOption.get
    assertMatrixClose(gram, bruteColGram(xDense, metric), 1e-10)
  }

  test("total variance matches the brute-force trace for dense data across metric combinations") {
    for
      (rowLabel, rowMetric) <- rowMetrics
      (colLabel, colMetric) <- colMetrics
    do
      val tv = GenPcaKernels.totalVariance(MatrixView.dense(xDense), rowMetric, colMetric).toOption.get
      assertEqualsDouble(tv, bruteTrace(xDense, rowMetric, colMetric), 1e-9)
  }

  test("total variance uses the wide orientation for wide dense data") {
    val wide = xDense.transpose
    val rowMetric = colMetrics(2)._2
    val colMetric = rowMetrics(2)._2
    val tv = GenPcaKernels.totalVariance(MatrixView.dense(wide), rowMetric, colMetric).toOption.get
    assertEqualsDouble(tv, bruteTrace(wide, rowMetric, colMetric), 1e-9)
  }

  test("total variance stays matrix-free on sparse data for every metric combination") {
    for
      (rowLabel, rowMetric) <- rowMetrics
      (colLabel, colMetric) <- colMetrics
    do
      val tv = GenPcaKernels.totalVariance(xSparse, rowMetric, colMetric, StoragePolicy.PreserveSparse).toOption.get
      assertEqualsDouble(tv, bruteTrace(xDense, rowMetric, colMetric), 1e-9)
      val tvDense = GenPcaKernels.totalVariance(xSparse, rowMetric, colMetric, StoragePolicy.AllowDense).toOption.get
      assertEqualsDouble(tvDense, bruteTrace(xDense, rowMetric, colMetric), 1e-9)
  }

  test("metric shape mismatches are reported with the offending axis") {
    val wrongRow = rowMetrics(0)._2
    GenPcaKernels.rowGram(MatrixView.dense(xDense.transpose), wrongRow) match
      case Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, expected, actual)) =>
        assertEquals(expected, 3)
        assertEquals(actual, 4)
      case other => fail(s"expected a row metric shape mismatch, got $other")

    val wrongCol = colMetrics(0)._2
    GenPcaKernels.colGram(MatrixView.dense(xDense.transpose), wrongCol) match
      case Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, expected, actual)) =>
        assertEquals(expected, 4)
        assertEquals(actual, 3)
      case other => fail(s"expected a column metric shape mismatch, got $other")
  }
