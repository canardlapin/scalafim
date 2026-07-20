package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

class DualityKernelsSuite extends munit.FunSuite:

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

  private def metricDense(metric: MvMetric): DMat =
    metric.toDense(StoragePolicy.AllowDense).toOption.get

  private def bruteRowGram(xd: DMat, metric: MvMetric): DMat =
    GaleNumerics.transposeMultiply(xd, GaleNumerics.multiply(metricDense(metric), xd))

  private def bruteColGram(xd: DMat, metric: MvMetric): DMat =
    GaleNumerics.multiply(GaleNumerics.multiply(xd, metricDense(metric)), xd.transpose)

  private def bruteCrossGram(xd: DMat, yd: DMat, metric: MvMetric): DMat =
    GaleNumerics.transposeMultiply(xd, GaleNumerics.multiply(metricDense(metric), yd))

  private def bruteTrace(xd: DMat, rowMetric: MvMetric, colMetric: MvMetric): Double =
    val gram = bruteRowGram(xd, rowMetric)
    val weighted = GaleNumerics.multiply(gram, metricDense(colMetric))
    var acc = 0.0
    var i = 0
    while i < weighted.rows do
      acc += weighted(i, i)
      i += 1
    acc

  private val xDense = GaleNumerics.matrixFromRows(
    Vector(
      Vector(1.0, 0.0, 2.0),
      Vector(0.0, 3.0, 0.0),
      Vector(4.0, 0.0, 5.0),
      Vector(0.0, 6.0, 1.0)
    )
  )

  private val xSparse = SparseMatrixView.fromRows(xDense.toRows).toOption.get

  private val yDense = GaleNumerics.matrixFromRows(
    Vector(
      Vector(2.0, 1.0),
      Vector(0.0, 4.0),
      Vector(3.0, 0.0),
      Vector(1.0, 5.0)
    )
  )

  private val ySparse = SparseMatrixView.fromRows(yDense.toRows).toOption.get

  private def rowMetrics: Vector[(String, MvMetric)] =
    val spd = GaleNumerics.matrixFromRows(
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
      "diagonal" -> MvMetric.diagonal(DVec.fromSeq(Vector(1.0, 2.0, 0.5, 1.5))).toOption.get,
      "dense" -> MvMetric.denseSymmetric(spd).toOption.get,
      "sparse" -> MvMetric.sparseSymmetric(sparse).toOption.get
    )

  private def colMetrics: Vector[(String, MvMetric)] =
    val spd = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.5, 0.0, 0.5),
        Vector(0.0, 2.0, 0.0),
        Vector(0.5, 0.0, 3.0)
      )
    )
    val sparse = SparseMatrixView.fromRows(spd.toRows).toOption.get
    Vector(
      "identity" -> MvMetric.identity(3).toOption.get,
      "diagonal" -> MvMetric.diagonal(DVec.fromSeq(Vector(0.5, 2.0, 1.0))).toOption.get,
      "dense" -> MvMetric.denseSymmetric(spd).toOption.get,
      "sparse" -> MvMetric.sparseSymmetric(sparse).toOption.get
    )

  test("dense row Gram matches the brute-force reference for every metric kind") {
    for (label, metric) <- rowMetrics do
      val gram = DualityKernels.rowGram(MatrixView.dense(xDense), metric).toOption.get
      assertMatrixClose(gram, bruteRowGram(xDense, metric), 1e-10)
  }

  test("sparse row Gram stays sparse-driven for diagonal-like metrics and matches dense") {
    for (label, metric) <- rowMetrics.take(2) do
      val gram = DualityKernels.rowGram(xSparse, metric, StoragePolicy.PreserveSparse).toOption.get
      assertMatrixClose(gram, bruteRowGram(xDense, metric), 1e-10)
  }

  test("sparse row Gram with a non-diagonal metric is policy gated") {
    val metric = rowMetrics(2)._2
    assertEquals(
      DualityKernels.rowGram(xSparse, metric, StoragePolicy.PreserveSparse),
      Left(MultivarError.DensificationRejected("row Gram", StorageKind.Sparse))
    )
    val gram = DualityKernels.rowGram(xSparse, metric, StoragePolicy.AllowDense).toOption.get
    assertMatrixClose(gram, bruteRowGram(xDense, metric), 1e-10)
  }

  test("affine row Gram expands the lazy shift analytically for every metric kind") {
    val scale = DVec.fromSeq(Vector(2.0, 0.5, -1.0))
    val shift = DVec.fromSeq(Vector(1.0, -2.0, 0.5))
    val affine = MatrixView.affine(xSparse, scale, shift, StoragePolicy.Operator).toOption.get
    assertEquals(affine.storage, StorageKind.LazyAffine)
    val materialized = MatrixView.materializeAffine(xDense, scale, shift)

    for (label, metric) <- rowMetrics.take(2) do
      val gram = DualityKernels.rowGram(affine, metric, StoragePolicy.PreserveSparse).toOption.get
      assertMatrixClose(gram, bruteRowGram(materialized, metric), 1e-10)

    for (label, metric) <- rowMetrics.drop(2) do
      val rejected = DualityKernels.rowGram(affine, metric, StoragePolicy.PreserveSparse)
      assert(rejected.isLeft, s"affine over sparse base with $label metric must be policy gated")
      val gram = DualityKernels.rowGram(affine, metric, StoragePolicy.AllowDense).toOption.get
      assertMatrixClose(gram, bruteRowGram(materialized, metric), 1e-10)
  }

  test("affine row Gram over a dense base matches the materialized reference for every metric kind") {
    val scale = DVec.fromSeq(Vector(2.0, 0.5, -1.0))
    val shift = DVec.fromSeq(Vector(1.0, -2.0, 0.5))
    // The public affine constructor materializes dense bases eagerly, so build the
    // lazy view directly to exercise the dense-base branch of the affine row Gram.
    val affine = AffineMatrixView.unsafe(MatrixView.dense(xDense), scale, shift)
    val materialized = MatrixView.materializeAffine(xDense, scale, shift)

    for (label, metric) <- rowMetrics do
      val gram = DualityKernels.rowGram(affine, metric, StoragePolicy.AllowDense).toOption.get
      assertMatrixClose(gram, bruteRowGram(materialized, metric), 1e-10)
  }

  test("total variance over a lazy affine view matches the materialized brute-force trace") {
    val scale = DVec.fromSeq(Vector(2.0, 0.5, -1.0))
    val shift = DVec.fromSeq(Vector(1.0, -2.0, 0.5))
    val affine = MatrixView.affine(xSparse, scale, shift, StoragePolicy.Operator).toOption.get
    assertEquals(affine.storage, StorageKind.LazyAffine)
    val materialized = MatrixView.materializeAffine(xDense, scale, shift)

    for
      (rowLabel, rowMetric) <- rowMetrics
      (colLabel, colMetric) <- colMetrics
    do
      val tv = DualityKernels.totalVariance(affine, rowMetric, colMetric, StoragePolicy.AllowDense).toOption.get
      assertEqualsDouble(tv, bruteTrace(materialized, rowMetric, colMetric), 1e-9)

    for (colLabel, colMetric) <- colMetrics do
      val tv = DualityKernels
        .totalVariance(affine, rowMetrics(1)._2, colMetric, StoragePolicy.PreserveSparse)
        .toOption
        .get
      assertEqualsDouble(tv, bruteTrace(materialized, rowMetrics(1)._2, colMetric), 1e-9)
  }

  test("dense column Gram matches the brute-force reference for every metric kind") {
    for (label, metric) <- colMetrics do
      val gram = DualityKernels.colGram(MatrixView.dense(xDense), metric).toOption.get
      assertMatrixClose(gram, bruteColGram(xDense, metric), 1e-10)
  }

  test("dense paired cross Gram matches the brute-force row-metric reference") {
    for (label, metric) <- rowMetrics do
      val paired = Unsafe
        .pairedDiagramFromArrays(
          MatrixView.dense(xDense),
          MatrixView.dense(yDense),
          "test fixtures share row order",
          rowMetric = Some(metric)
        )
        .toOption
        .get
      val gram = DualityKernels.crossGram(paired, StoragePolicy.AllowDense).toOption.get
      assertMatrixClose(gram, bruteCrossGram(xDense, yDense, metric), 1e-10)
  }

  test("cross Gram accepts a shared row metric built as two identical instances") {
    val xObserved = MvSpace.of("x-observed", SpaceRole.Observed, 3).toOption.get
    val yObserved = MvSpace.of("y-observed", SpaceRole.Observed, 2).toOption.get
    val weights = Vector(1.0, 2.0, 0.5, 1.5)
    val first = MvMetric.diagonal(DVec.fromSeq(weights)).toOption.get
    val second = MvMetric.diagonal(DVec.fromSeq(weights)).toOption.get
    val xDiagram = DualityDiagram
      .from(MatrixView.dense(xDense), rowMetric = Some(first), columnSpace = Some(xObserved))
      .toOption
      .get
    val yDiagram = DualityDiagram
      .from(MatrixView.dense(yDense), rowMetric = Some(second), columnSpace = Some(yObserved))
      .toOption
      .get
    val paired = PairedDualityDiagram.fromDiagrams(xDiagram, yDiagram).toOption.get

    DualityKernels.crossGram(paired, StoragePolicy.AllowDense) match
      case Right(gram) =>
        assertMatrixClose(gram, bruteCrossGram(xDense, yDense, first), 1e-10)
      case Left(error) =>
        fail(s"expected identical-valued row metrics to be accepted, got $error")
  }

  test("sparse paired cross Gram stays sparse-driven for diagonal-like row metrics") {
    for (label, metric) <- rowMetrics.take(2) do
      val paired = Unsafe
        .pairedDiagramFromArrays(xSparse, ySparse, "test fixtures share row order", rowMetric = Some(metric))
        .toOption
        .get
      val gram = DualityKernels.crossGram(paired, StoragePolicy.PreserveSparse).toOption.get
      assertMatrixClose(gram, bruteCrossGram(xDense, yDense, metric), 1e-10)
  }

  test("sparse paired cross Gram with a non-diagonal row metric is policy gated") {
    val metric = rowMetrics(2)._2
    val paired = Unsafe
      .pairedDiagramFromArrays(xSparse, ySparse, "test fixtures share row order", rowMetric = Some(metric))
      .toOption
      .get

    assertEquals(
      DualityKernels.crossGram(paired, StoragePolicy.PreserveSparse),
      Left(MultivarError.DensificationRejected("cross Gram", StorageKind.Sparse))
    )
    assertMatrixClose(
      DualityKernels.crossGram(paired, StoragePolicy.AllowDense).toOption.get,
      bruteCrossGram(xDense, yDense, metric),
      1e-10
    )
  }

  test("column Gram on sparse data is policy gated") {
    val metric = colMetrics(0)._2
    DualityKernels.colGram(xSparse, metric, StoragePolicy.PreserveSparse) match
      case Left(MultivarError.DensificationRejected("column Gram", StorageKind.Sparse)) => ()
      case other => fail(s"expected a densification rejection, got $other")
    val gram = DualityKernels.colGram(xSparse, metric, StoragePolicy.AllowDense).toOption.get
    assertMatrixClose(gram, bruteColGram(xDense, metric), 1e-10)
  }

  test("transposed sparse Grams honor PreserveSparse for diagonal-like metrics") {
    val transposed = xSparse.transposeView
    val denseTransposed = xDense.transpose
    val rowMetric = colMetrics(1)._2
    val colMetric = rowMetrics(1)._2

    val rowGram = DualityKernels.rowGram(transposed, rowMetric, StoragePolicy.PreserveSparse).toOption.get
    val colGram = DualityKernels.colGram(transposed, colMetric, StoragePolicy.PreserveSparse).toOption.get

    assertMatrixClose(rowGram, bruteRowGram(denseTransposed, rowMetric), 1e-10)
    assertMatrixClose(colGram, bruteColGram(denseTransposed, colMetric), 1e-10)
  }

  test("total variance on a transposed sparse table stays matrix-free under PreserveSparse") {
    val transposed = xSparse.transposeView
    val denseTransposed = xDense.transpose

    for
      (rowLabel, rowMetric) <- colMetrics
      (colLabel, colMetric) <- rowMetrics
    do
      DualityKernels.totalVariance(transposed, rowMetric, colMetric, StoragePolicy.PreserveSparse) match
        case Right(tv) =>
          assertEqualsDouble(tv, bruteTrace(denseTransposed, rowMetric, colMetric), 1e-9)
        case Left(error) =>
          fail(s"expected a matrix-free transposed-sparse trace for $rowLabel x $colLabel, got $error")
  }

  test("total variance matches the brute-force trace for dense data across metric combinations") {
    for
      (rowLabel, rowMetric) <- rowMetrics
      (colLabel, colMetric) <- colMetrics
    do
      val tv = DualityKernels.totalVariance(MatrixView.dense(xDense), rowMetric, colMetric).toOption.get
      assertEqualsDouble(tv, bruteTrace(xDense, rowMetric, colMetric), 1e-9)
  }

  test("total variance uses the wide orientation for wide dense data") {
    val wide = xDense.transpose
    val rowMetric = colMetrics(2)._2
    val colMetric = rowMetrics(2)._2
    val tv = DualityKernels.totalVariance(MatrixView.dense(wide), rowMetric, colMetric).toOption.get
    assertEqualsDouble(tv, bruteTrace(wide, rowMetric, colMetric), 1e-9)
  }

  test("total variance stays matrix-free on sparse data for every metric combination") {
    for
      (rowLabel, rowMetric) <- rowMetrics
      (colLabel, colMetric) <- colMetrics
    do
      val tv = DualityKernels.totalVariance(xSparse, rowMetric, colMetric, StoragePolicy.PreserveSparse).toOption.get
      assertEqualsDouble(tv, bruteTrace(xDense, rowMetric, colMetric), 1e-9)
      val tvDense = DualityKernels.totalVariance(xSparse, rowMetric, colMetric, StoragePolicy.AllowDense).toOption.get
      assertEqualsDouble(tvDense, bruteTrace(xDense, rowMetric, colMetric), 1e-9)
  }

  test("metric shape mismatches are reported with the offending axis") {
    val wrongRow = rowMetrics(0)._2
    DualityKernels.rowGram(MatrixView.dense(xDense.transpose), wrongRow) match
      case Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, expected, actual)) =>
        assertEquals(expected, 3)
        assertEquals(actual, 4)
      case other => fail(s"expected a row metric shape mismatch, got $other")

    val wrongCol = colMetrics(0)._2
    DualityKernels.colGram(MatrixView.dense(xDense.transpose), wrongCol) match
      case Left(MultivarError.MetricShapeMismatch(IndexAxis.Column, expected, actual)) =>
        assertEquals(expected, 4)
        assertEquals(actual, 3)
      case other => fail(s"expected a column metric shape mismatch, got $other")
  }
