package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

class SparsePreprocessingSuite extends munit.FunSuite:

  private val denseRows: Vector[Vector[Double]] =
    Vector(
      Vector(1.0, 0.0, 2.0),
      Vector(0.0, 3.0, 0.0),
      Vector(4.0, 0.0, 5.0),
      Vector(0.0, 6.0, 0.0)
    )

  private def denseMatrix: DMat =
    GaleNumerics.matrixFromRows(denseRows)

  private def sparseView: MatrixView =
    SparseMatrixView.fromRows(denseRows).toOption.get

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

  private def denseTransform(scale: Vector[Double], shift: Vector[Double]): Vector[Vector[Double]] =
    denseRows.map { row =>
      row.zipWithIndex.map { case (value, col) => value * scale(col) + shift(col) }
    }

  test("pass and column scaling preserve sparse storage") {
    val pass = PreprocessSpec.Pass.fit(sparseView).toOption.get
    val passOut = pass.transform(sparseView).toOption.get
    assertEquals(passOut.storage, StorageKind.Sparse)

    val scaleSpec = PreprocessSpec.scale(Vector(2.0, 0.5, -1.0)).toOption.get
    val scale = scaleSpec.fit(sparseView).toOption.get
    val scaled = scale.transform(sparseView).toOption.get

    assertEquals(scaled.storage, StorageKind.Sparse)
    assertMatrixClose(
      scaled.toDense(StoragePolicy.AllowDense).toOption.get,
      denseTransform(Vector(2.0, 0.5, -1.0), Vector(0.0, 0.0, 0.0)),
      1e-12
    )
  }

  test("sparse centering is lazy by default and explicit about densification policy") {
    val center = PreprocessSpec.Center.fit(sparseView).toOption.get
    val centered = center.transform(sparseView).toOption.get

    assertEquals(centered.storage, StorageKind.LazyAffine)
    assert(center.transform(sparseView, policy = StoragePolicy.PreserveSparse).isLeft)

    val means = Vector(1.25, 2.25, 1.75)
    assertMatrixClose(
      centered.toDense(StoragePolicy.AllowDense).toOption.get,
      denseTransform(Vector(1.0, 1.0, 1.0), means.map(-_)),
      1e-12
    )

    val denseCentered = center.transform(sparseView, policy = StoragePolicy.AllowDense).toOption.get
    assertEquals(denseCentered.storage, StorageKind.Dense)
  }

  test("standardizing sparse matrices stays lazy and matches dense arithmetic") {
    val fitted = PreprocessSpec.Standardize.fit(sparseView).toOption.get
    val standardized = fitted.transform(sparseView).toOption.get
    val means = Vector(1.25, 2.25, 1.75)
    val sds = Vector(1.8929694486000912, 2.8722813232690143, 2.362907813126304)
    val expected = denseRows.map { row =>
      row.zipWithIndex.map { case (value, col) => (value - means(col)) / sds(col) }
    }

    assertEquals(standardized.storage, StorageKind.LazyAffine)
    assertMatrixClose(standardized.toDense(StoragePolicy.AllowDense).toOption.get, expected, 1e-12)
  }

  test("affine MatrixView algebra matches dense arithmetic") {
    val fitted = PreprocessSpec.Center.fit(sparseView).toOption.get
    val centered = fitted.transform(sparseView).toOption.get
    val denseCentered = centered.toDense(StoragePolicy.AllowDense).toOption.get
    val weights = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.0, 1.0)
      )
    )

    assertMatrixClose(
      centered.rightMultiply(weights).toOption.get,
      GaleNumerics.multiply(denseCentered, weights).toRows,
      1e-12
    )
    assertMatrixClose(centered.crossProduct.toOption.get, GaleNumerics.crossProduct(denseCentered).toRows, 1e-12)

    val selected = centered.selectColumns(IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get).toOption.get
    assertEquals(selected.storage, StorageKind.LazyAffine)
    assertMatrixClose(
      selected.toDense(StoragePolicy.AllowDense).toOption.get,
      denseCentered.toRows.map(row => Vector(row(2), row(0))),
      1e-12
    )
  }

  test("standardize scales genuinely varying tiny-magnitude columns to unit variance") {
    val rows = Vector(
      Vector(1e-10 + 5e-13),
      Vector(1e-10 - 5e-13),
      Vector(1e-10 + 5e-13),
      Vector(1e-10 - 5e-13)
    )
    val view = MatrixView.dense(GaleNumerics.matrixFromRows(rows))
    val fitted = PreprocessSpec.Standardize.fit(view).toOption.get
    val out = fitted.transform(view).toOption.get.toDense(StoragePolicy.AllowDense).toOption.get
    val sds = ColumnStats.fromDense(out).flatMap(_.sampleStandardDeviations).toOption.get

    assertEqualsDouble(sds(0), 1.0, 1e-6)
  }

  test("standardize treats constant columns as degenerate and centers them only") {
    val rows = Vector.fill(4)(Vector(3.5, 1e8))
    val view = MatrixView.dense(GaleNumerics.matrixFromRows(rows))
    val fitted = PreprocessSpec.Standardize.fit(view).toOption.get
    val out = fitted.transform(view).toOption.get.toDense(StoragePolicy.AllowDense).toOption.get

    var row = 0
    while row < out.rows do
      assertEqualsDouble(out(row, 0), 0.0, 1e-12)
      assertEqualsDouble(out(row, 1), 0.0, 1e-12)
      row += 1
  }

  test("standardize keeps a unit scale for numerically constant huge-magnitude columns") {
    val rows = Vector.fill(3)(Vector(1e8 + 0.1))
    val view = MatrixView.dense(GaleNumerics.matrixFromRows(rows))
    val fitted = PreprocessSpec.Standardize.fit(view).toOption.get

    fitted match
      case affine: FittedColumnAffine =>
        assertEqualsDouble(affine.scale(0), 1.0, 0.0)
      case other =>
        fail(s"expected FittedColumnAffine, got $other")
  }

  test("fitted preprocessor supports column-subset transform and inverse transform") {
    val fitted = PreprocessSpec.Center.fit(sparseView).toOption.get
    val columnSet = IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get
    val subset = sparseView.selectColumns(columnSet).toOption.get

    val transformed = fitted.transform(subset, columns = Some(columnSet)).toOption.get
    val restored = fitted.inverseTransform(transformed, columns = Some(columnSet)).toOption.get

    assertMatrixClose(
      transformed.toDense(StoragePolicy.AllowDense).toOption.get,
      denseTransform(Vector(1.0, 1.0, 1.0), Vector(-1.25, -2.25, -1.75)).map(row => Vector(row(2), row(0))),
      1e-12
    )
    assertMatrixClose(
      restored.toDense(StoragePolicy.AllowDense).toOption.get,
      denseRows.map(row => Vector(row(2), row(0))),
      1e-12
    )
  }

  test("standardizing a single-row matrix reports insufficient rows") {
    val view = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0, 2.0))))

    PreprocessSpec.Standardize.fit(view) match
      case Left(MultivarError.InsufficientRows("sample standard deviations", 2, 1)) => ()
      case other => fail(s"expected InsufficientRows, got $other")
  }

  test("preprocessing fit rejects inputs without columns") {
    val view = MatrixView.dense(DMat.zeros(3, 0))

    PreprocessSpec.Pass.fit(view) match
      case Left(MultivarError.InvalidDimension("preprocessing input columns", 0)) => ()
      case other => fail(s"expected InvalidDimension, got $other")
    assert(PreprocessSpec.Standardize.fit(view).isLeft)
  }

  test("scale preprocessing rejects wrong-length weights") {
    val spec = PreprocessSpec.scale(Vector(1.0, 2.0)).toOption.get

    assert(spec.fit(sparseView).swap.toOption.exists(_.message.contains("length 2 != expected 3")))
    assert(PreprocessSpec.scale(Vector(1.0, Double.NaN)).isLeft)
  }

  test("inverse transform reports non-invertible scale weights precisely") {
    val fitted = FittedColumnAffine(
      inputCols = 3,
      scale = DVec.fromSeq(Vector(1.0, 0.0, 2.0)),
      shift = MatrixView.zeros(3)
    )

    fitted.inverseTransform(sparseView) match
      case Left(MultivarError.NonInvertibleValue("affine inverse scale", 1, 0.0)) => ()
      case other => fail(s"expected NonInvertibleValue, got $other")
  }

  test("restrict narrows a fitted preprocessor to selected columns") {
    val fitted = PreprocessSpec.Center.fit(sparseView).toOption.get
    val columnSet = IndexSet.from(Vector(2, 0), IndexAxis.Feature).toOption.get
    val restricted = fitted.restrict(columnSet).toOption.get
    val subset = sparseView.selectColumns(columnSet).toOption.get

    assertEquals(restricted.inputCols, 2)
    assertMatrixClose(
      restricted.transform(subset).toOption.get.toDense(StoragePolicy.AllowDense).toOption.get,
      denseTransform(Vector(1.0, 1.0, 1.0), Vector(-1.25, -2.25, -1.75)).map(row => Vector(row(2), row(0))),
      1e-12
    )
    assert(fitted.restrict(IndexSet.from(Vector(3), IndexAxis.Feature).toOption.get).isLeft)
    assert(fitted.restrict(IndexSet.from(Vector(0), IndexAxis.Row).toOption.get).isLeft)
  }

