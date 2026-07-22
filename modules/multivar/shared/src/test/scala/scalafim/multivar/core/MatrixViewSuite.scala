package scalafim.multivar
package core

import scalafim.multivar.core.*

import gale.linalg.DMat
import gale.linalg.DVec

class MatrixViewSuite extends munit.FunSuite:

  private def matrix: DMat =
    GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0)
      )
    )

  private def sparseMatrix: SparseMatrixView =
    SparseMatrixView
      .fromRows(
        Vector(
          Vector(1.0, 0.0, 2.0),
          Vector(0.0, 3.0, 0.0),
          Vector(4.0, 0.0, 5.0),
          Vector(0.0, 6.0, 1.0)
        )
      )
      .toOption
      .get

  private def assertMatrixClose(actual: DMat, expected: Vector[Vector[Double]], tol: Double): Unit =
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
    val weights = GaleNumerics.matrixFromRows(
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
    val badWeights = GaleNumerics.matrixFromRows(Vector(Vector(1.0)))
    val nonFinite = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0, Double.NaN))))

    assert(view.rightMultiply(badWeights).swap.toOption.exists(_.message.contains("expected 3 weight rows")))
    assert(view.selectColumns(IndexSet.from(Vector(3), IndexAxis.Feature).toOption.get).isLeft)
    assert(nonFinite.columnStats.swap.toOption.exists(_.message.contains("not finite")))
  }

  test("transposed sparse MatrixView computes column stats without materializing") {
    val transposed = sparseMatrix.transposeView
    val stats = transposed.columnStats.toOption.get

    assertEquals(transposed.rows, 3)
    assertEquals(transposed.cols, 4)
    assertEquals(transposed.storage, StorageKind.Sparse)
    assertEquals(stats.count, 3)
    assertEqualsDouble(stats.sums(0), 3.0, 1e-12)
    assertEqualsDouble(stats.sums(1), 3.0, 1e-12)
    assertEqualsDouble(stats.sums(2), 9.0, 1e-12)
    assertEqualsDouble(stats.sums(3), 7.0, 1e-12)
    assertEqualsDouble(stats.sumSquares(2), 41.0, 1e-12)
    assertEqualsDouble(stats.means.toOption.get(3), 7.0 / 3.0, 1e-12)
  }

  test("transposed sparse MatrixView selects columns as a sparse view") {
    val transposed = sparseMatrix.transposeView
    val selected = transposed.selectColumns(IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get).toOption.get

    assertEquals(selected.rows, 3)
    assertEquals(selected.cols, 2)
    assertEquals(selected.storage, StorageKind.Sparse)
    assertMatrixClose(
      selected.toDense(StoragePolicy.AllowDense).toOption.get,
      Vector(
        Vector(4.0, 1.0),
        Vector(0.0, 0.0),
        Vector(5.0, 2.0)
      ),
      1e-12
    )
  }

  test("transposed sparse MatrixView multiplies sparse operands without densifying them first") {
    val transposed = sparseMatrix.transposeView
    val weights = SparseMatrixView
      .fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 2.0),
          Vector(3.0, 0.0)
        )
      )
      .toOption
      .get
    val transposedWeights = SparseMatrixView
      .fromRows(
        Vector(
          Vector(1.0, 0.0, 3.0),
          Vector(0.0, 2.0, 0.0)
        )
      )
      .toOption
      .get
      .transposeView

    val expected = Vector(
      Vector(7.0, 0.0),
      Vector(0.0, 6.0),
      Vector(19.0, 0.0),
      Vector(3.0, 12.0)
    )

    assertMatrixClose(transposed.transposeMultiply(weights).toOption.get, expected, 1e-12)
    assertMatrixClose(transposed.transposeMultiply(transposedWeights).toOption.get, expected, 1e-12)
  }

  test("column stats compute large-mean sample standard deviations without cancellation") {
    val rows = Vector(Vector(1e8), Vector(1e8 + 1.0))
    val dense = MatrixView.dense(GaleNumerics.matrixFromRows(rows))
    val denseSds = dense.columnStats.flatMap(_.sampleStandardDeviations).toOption.get
    assertEqualsDouble(denseSds(0), Math.sqrt(0.5), 1e-9)

    val sparse = SparseMatrixView.fromRows(rows).toOption.get
    val sparseSds = sparse.columnStats.flatMap(_.sampleStandardDeviations).toOption.get
    assertEqualsDouble(sparseSds(0), Math.sqrt(0.5), 1e-9)
  }

  test("tiny nonzero affine shifts are applied identically on dense and sparse paths") {
    val rows = Vector(
      Vector(1.0, 0.0, 2.0),
      Vector(0.0, 3.0, 0.0),
      Vector(4.0, 0.0, 5.0),
      Vector(0.0, 6.0, 1.0)
    )
    val scale = DVec.fromSeq(Vector(1.0, 1.0, 1.0))
    val shift = DVec.fromSeq(Vector(5e-13, 5e-13, 5e-13))
    val denseOut = MatrixView
      .affine(MatrixView.dense(GaleNumerics.matrixFromRows(rows)), scale, shift)
      .toOption
      .get
      .toDense(StoragePolicy.AllowDense)
      .toOption
      .get
    val sparseOut = MatrixView
      .affine(SparseMatrixView.fromRows(rows).toOption.get, scale, shift)
      .toOption
      .get
      .toDense(StoragePolicy.AllowDense)
      .toOption
      .get

    assertMatrixClose(sparseOut, denseOut.toRows, 0.0)
  }

  test("transposed operator views refuse densifying column selection") {
    val scale = DVec.fromSeq(Vector(2.0, 0.5, -1.5))
    val shift = DVec.fromSeq(Vector(0.25, -1.0, 3.0))
    val affine = MatrixView.affine(sparseMatrix, scale, shift, StoragePolicy.Operator).toOption.get
    val transposed = affine.transposeView

    transposed.selectColumns(IndexSet.from(Vector(1, 0), IndexAxis.Feature).toOption.get) match
      case Left(MultivarError.DensificationRejected(_, _)) => ()
      case other                                           => fail(s"expected DensificationRejected, got $other")
  }

  test("affine row stats over sparse bases match the densified reference without materializing") {
    val scale = DVec.fromSeq(Vector(2.0, 0.5, -1.5))
    val shift = DVec.fromSeq(Vector(0.25, -1.0, 3.0))
    val affine = MatrixView.affine(sparseMatrix, scale, shift, StoragePolicy.Operator).toOption.get
    assertEquals(affine.storage, StorageKind.LazyAffine)

    val stats = affine.transposeView.columnStats.toOption.get
    val reference = ColumnStats
      .fromDenseRows(affine.toDense(StoragePolicy.AllowDense).toOption.get)
      .toOption
      .get

    assertEquals(stats.count, reference.count)
    var row = 0
    while row < sparseMatrix.rows do
      assertEqualsDouble(stats.sums(row), reference.sums(row), 1e-10)
      assertEqualsDouble(stats.sumSquares(row), reference.sumSquares(row), 1e-10)
      row += 1
  }

  test("operator affine views reject dense materialization under strict policies") {
    val scale = DVec.fromSeq(Vector(1.0, 1.0, 1.0))
    val shift = DVec.fromSeq(Vector(1.0, -1.0, 2.0))
    val affine = MatrixView.affine(sparseMatrix, scale, shift, StoragePolicy.Operator).toOption.get

    assertEquals(affine.storage, StorageKind.LazyAffine)
    assert(affine.toDense(StoragePolicy.PreserveSparse).isLeft)
    affine.toDense(StoragePolicy.Operator) match
      case Left(MultivarError.DensificationRejected("toDense", StorageKind.LazyAffine)) => ()
      case other => fail(s"expected DensificationRejected, got $other")

    MatrixView.affine(sparseMatrix, scale, shift, StoragePolicy.PreserveSparse, "strict affine") match
      case Left(MultivarError.DensificationRejected("strict affine", StorageKind.Sparse)) => ()
      case other => fail(s"expected DensificationRejected, got $other")
  }

  test("sparse MatrixView computes column stats directly") {
    val stats = sparseMatrix.columnStats.toOption.get

    assertEquals(stats.count, 4)
    assertEqualsDouble(stats.sums(0), 5.0, 1e-12)
    assertEqualsDouble(stats.sums(1), 9.0, 1e-12)
    assertEqualsDouble(stats.sums(2), 8.0, 1e-12)
    assertEqualsDouble(stats.sumSquares(2), 30.0, 1e-12)
    assertEqualsDouble(stats.means.toOption.get(0), 1.25, 1e-12)

    val denseTwin = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0, 2.0),
        Vector(0.0, 3.0, 0.0),
        Vector(4.0, 0.0, 5.0),
        Vector(0.0, 6.0, 1.0)
      )
    )
    val referenceSds = ColumnStats.fromDense(denseTwin).flatMap(_.sampleStandardDeviations).toOption.get
    val sparseSds = stats.sampleStandardDeviations.toOption.get
    var col = 0
    while col < 3 do
      assertEqualsDouble(sparseSds(col), referenceSds(col), 1e-12)
      col += 1
  }

  test("fromTriplets canonicalizes unsorted duplicated triplets and drops cancelled sums") {
    val view = SparseMatrixView
      .fromTriplets(
        2,
        3,
        Array(1, 0, 1, 0, 1),
        Array(2, 1, 2, 1, 0),
        Array(4.0, 2.0, -1.0, -2.0, 5.0)
      )
      .toOption
      .get

    assertEquals(view.nnz, 2)
    assertMatrixClose(
      view.toDense(StoragePolicy.AllowDense).toOption.get,
      Vector(Vector(0.0, 0.0, 0.0), Vector(5.0, 0.0, 3.0)),
      1e-12
    )
  }

  test("fromTriplets rejects out-of-bounds indices") {
    SparseMatrixView.fromTriplets(2, 3, Array(2), Array(0), Array(1.0)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Row, 2, 2)) => ()
      case other => fail(s"expected row bound rejection, got $other")
    SparseMatrixView.fromTriplets(2, 3, Array(0), Array(3), Array(1.0)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Column, 3, 3)) => ()
      case other => fail(s"expected column bound rejection, got $other")
    SparseMatrixView.fromTriplets(2, 3, Array(0), Array(-1), Array(1.0)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Column, -1, 3)) => ()
      case other => fail(s"expected column bound rejection, got $other")
  }

  test("mixed sparse and dense right multiplication dispatch matches dense arithmetic") {
    val denseView = MatrixView.dense(matrix)
    val sparseDense = sparseMatrix.toDense(StoragePolicy.AllowDense).toOption.get
    val weights = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, -2.0),
        Vector(0.0, 3.0),
        Vector(4.0, 0.5)
      )
    )
    val sparseWeights = SparseMatrixView
      .fromRows(Vector(Vector(1.0, -2.0), Vector(0.0, 3.0), Vector(4.0, 0.5)))
      .toOption
      .get

    // sparse x dense
    assertMatrixClose(
      MatrixView.rightMultiplyView(sparseMatrix, MatrixView.dense(weights)).toOption.get,
      GaleNumerics.multiply(sparseDense, weights).toRows,
      1e-12
    )
    // dense x sparse
    assertMatrixClose(
      MatrixView.rightMultiplyView(denseView, sparseWeights).toOption.get,
      GaleNumerics.multiply(matrix, weights).toRows,
      1e-12
    )
    // dense x sparse-transpose
    val transposedWeights = SparseMatrixView
      .fromRows(Vector(Vector(1.0, 0.0, 4.0), Vector(-2.0, 3.0, 0.5)))
      .toOption
      .get
      .transposeView
    assertMatrixClose(
      MatrixView.rightMultiplyView(denseView, transposedWeights).toOption.get,
      GaleNumerics.multiply(matrix, weights).toRows,
      1e-12
    )
    // shape mismatch is reported once from the dispatch path
    assert(
      MatrixView
        .rightMultiplyView(denseView, sparseMatrix)
        .swap
        .toOption
        .exists(_.message.contains("expected 3 weight rows"))
    )
  }

  test("empty matrices: dense views allow zero rows while sparse construction rejects them") {
    val empty = MatrixView.dense(DMat.zeros(0, 3))
    assertEquals(empty.rows, 0)
    assertEquals(empty.cols, 3)
    assertEquals(empty.columnStats.toOption.get.count, 0)

    SparseMatrixView.fromRows(Vector.empty) match
      case Left(MultivarError.InvalidDimension("sparse matrix rows", 0)) => ()
      case other => fail(s"expected sparse zero-row rejection, got $other")
    SparseMatrixView.fromRows(Vector(Vector.empty[Double])) match
      case Left(MultivarError.InvalidDimension("sparse matrix columns", 0)) => ()
      case other => fail(s"expected sparse zero-column rejection, got $other")
  }
