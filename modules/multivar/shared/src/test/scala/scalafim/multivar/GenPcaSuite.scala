package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

class GenPcaSuite extends munit.FunSuite:

  import GenPcaRReferenceFixtures as R

  private def k(value: Int): ComponentCount =
    ComponentCount.unsafe(value)

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

  private def assertVectorClose(actual: DoubleVector, expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  /** Flip (ou_j, ov_j) pairs so the largest-magnitude ov entry is positive — the same
    * canonicalization applied to the R reference values.
    */
  private def canonicalize(ou: DoubleMatrix, ov: DoubleMatrix): (DoubleMatrix, DoubleMatrix) =
    val ouData = ou.copyData
    val ovData = ov.copyData
    var col = 0
    while col < ov.cols do
      var best = 0
      var row = 1
      while row < ov.rows do
        if Math.abs(ovData(row * ov.cols + col)) > Math.abs(ovData(best * ov.cols + col)) then best = row
        row += 1
      if ovData(best * ov.cols + col) < 0.0 then
        row = 0
        while row < ov.rows do
          ovData(row * ov.cols + col) = -ovData(row * ov.cols + col)
          row += 1
        row = 0
        while row < ou.rows do
          ouData(row * ou.cols + col) = -ouData(row * ou.cols + col)
          row += 1
      col += 1
    (DoubleMatrix.unsafe(ou.rows, ou.cols, ouData), DoubleMatrix.unsafe(ov.rows, ov.cols, ovData))

  private def assertGolden(
      fit: GenPcaFit,
      sdev: Vector[Double],
      ou: Vector[Vector[Double]],
      ov: Vector[Vector[Double]],
      propv: Vector[Double],
      tol: Double
  ): Unit =
    assertVectorClose(fit.d, sdev, tol)
    assertVectorClose(fit.propV, propv, tol)
    val (canonOu, canonOv) = canonicalize(fit.ou, fit.ov)
    assertMatrixClose(canonOu, DoubleMatrix.fromRows(ou), tol)
    assertMatrixClose(canonOv, DoubleMatrix.fromRows(ov), tol)

  private def assertMetricOrthonormal(factors: DoubleMatrix, metric: MvMetric, tol: Double): Unit =
    val weighted = metric.matvec(factors).toOption.get
    val gram = DoubleMatrix.transposeMultiply(factors, weighted)
    var row = 0
    while row < gram.rows do
      var col = 0
      while col < gram.cols do
        assertEqualsDouble(gram(row, col), if row == col then 1.0 else 0.0, tol)
        col += 1
      row += 1

  /** Subspace agreement in a metric: all singular values of U1' M U2 must be 1. */
  private def assertSameSubspace(
      left: DoubleMatrix,
      right: DoubleMatrix,
      metric: MvMetric,
      tol: Double
  ): Unit =
    val cross = DoubleMatrix.transposeMultiply(left, metric.matvec(right).toOption.get)
    val eigen = DenseSolvers.symmetricEigen.decompose(DoubleMatrix.crossProduct(cross)).toOption.get
    var i = 0
    while i < eigen.values.length do
      assertEqualsDouble(Math.sqrt(Math.max(eigen.values(i), 0.0)), 1.0, tol)
      i += 1

  private def diagonal(values: Vector[Double]): MvMetric =
    MvMetric.diagonal(DoubleVector.fromSeq(values)).toOption.get

  private def denseMetric(rows: Vector[Vector[Double]]): MvMetric =
    MvMetric.denseSymmetric(DoubleMatrix.fromRows(rows)).toOption.get

  // ---------------------------------------------------------------- properties

  test("identity metrics reproduce the plain PCA fit exactly") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g2X))
    val gen = GenPca.fit(x, k(2)).toOption.get
    val pca = Pca.fit(x, k(2)).toOption.get

    assertVectorClose(gen.d, Vector.tabulate(2)(pca.result.singularValues(_)), 1e-12)
    assertMatrixClose(gen.projection.scores, pca.projection.scores, 1e-12)
    assertMatrixClose(gen.v, pca.result.v, 1e-12)
    assertEquals(gen.projection.diagnostics.map(_.method), Some("genpca"))
  }

  test("a diagonal column metric equals the SVD of column-scaled data") {
    val x = DoubleMatrix.fromRows(R.g2X)
    val weights = R.g2ColWeights
    val gen = GenPca
      .fit(
        MatrixView.dense(x),
        k(3),
        colMetric = Some(diagonal(weights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get

    val scaled = MetricOperator.scaleColumnsDense(x, DoubleVector.fromSeq(weights.map(Math.sqrt)))
    val svd = Svd.fit(MatrixView.dense(scaled), k(3)).toOption.get

    var j = 0
    while j < 3 do
      assertEqualsDouble(gen.d(j), svd.result.singularValues(j), 1e-9)
      j += 1
    var row = 0
    while row < x.cols do
      var col = 0
      while col < 3 do
        assertEqualsDouble(
          Math.abs(gen.ov(row, col)) * Math.sqrt(weights(row)),
          Math.abs(svd.result.v(row, col)),
          1e-9
        )
        col += 1
      row += 1
  }

  test("factors are orthonormal in their metrics for dense SPD row and column metrics") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g5X))
    val rowMetric = denseMetric(R.g5RowMetric)
    val colMetric = denseMetric(R.g5ColMetric)
    val fit = GenPca
      .fit(x, k(3), Some(rowMetric), Some(colMetric), preproc = PreprocessSpec.Pass, backend = GmdBackend.Eigen())
      .toOption
      .get

    assertMetricOrthonormal(fit.ou, rowMetric, 1e-8)
    assertMetricOrthonormal(fit.ov, colMetric, 1e-8)
  }

  test("full-rank reconstruction recovers the data and variance proportions sum to one") {
    val x = DoubleMatrix.fromRows(R.g3X)
    val fit = GenPca
      .fit(
        MatrixView.dense(x),
        k(4),
        Some(diagonal(R.g3RowWeights)),
        Some(diagonal(R.g3ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get

    assertEquals(fit.componentCount, 4)
    assertMatrixClose(fit.reconstruct().toOption.get, x, 1e-7)

    var total = 0.0
    var i = 0
    while i < fit.propV.length do
      total += fit.propV(i)
      i += 1
    assertEqualsDouble(total, 1.0, 1e-9)
  }

  test("centered reconstruction inverts the preprocessing back to the original scale") {
    val x = DoubleMatrix.fromRows(R.g5X)
    val fit = GenPca
      .fit(
        MatrixView.dense(x),
        k(5),
        Some(denseMetric(R.g5RowMetric)),
        Some(denseMetric(R.g5ColMetric)),
        preproc = PreprocessSpec.Center,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get

    assertMatrixClose(fit.reconstruct().toOption.get, x, 1e-6)
  }

  test("truncate keeps leading factors and project matches the training scores") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g3X))
    val fit = GenPca
      .fit(
        x,
        k(3),
        Some(diagonal(R.g3RowWeights)),
        Some(diagonal(R.g3ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get

    val truncated = fit.truncate(k(2)).toOption.get
    assertEquals(truncated.componentCount, 2)
    assertVectorClose(truncated.d, Vector.tabulate(2)(fit.d(_)), 1e-12)
    assertMatrixClose(truncated.projection.scores, MatrixOps.takeColumns(fit.projection.scores, 2), 1e-12)
    assertMatrixClose(fit.project(x).toOption.get, fit.projection.scores, 1e-12)
    assert(fit.truncate(k(4)).isLeft)
  }

  test("eigen and deflation backends agree on singular values and metric subspaces") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g3X))
    val rowMetric = diagonal(R.g3RowWeights)
    val colMetric = diagonal(R.g3ColWeights)

    val eigen = GenPca
      .fit(x, k(2), Some(rowMetric), Some(colMetric), preproc = PreprocessSpec.Pass, backend = GmdBackend.Eigen())
      .toOption
      .get
    val deflation = GenPca
      .fit(x, k(2), Some(rowMetric), Some(colMetric), preproc = PreprocessSpec.Pass, backend = GmdBackend.Deflation())
      .toOption
      .get

    var j = 0
    while j < 2 do
      assertEqualsDouble(deflation.d(j), eigen.d(j), 1e-4 * (1.0 + eigen.d(j)))
      j += 1
    assertSameSubspace(eigen.ou, deflation.ou, rowMetric, 1e-3)
    assertSameSubspace(eigen.ov, deflation.ov, colMetric, 1e-3)
  }

  test("sparse data with sparse metrics fits through deflation and eigen is policy gated") {
    val xSparse = SparseMatrixView.fromRows(R.g7X).toOption.get
    val sparseMetric = MvMetric
      .sparseSymmetric(
        SparseMatrixView.fromRows(
          Vector(
            Vector(2.0, 0.0, 0.5, 0.0),
            Vector(0.0, 1.5, 0.0, 0.0),
            Vector(0.5, 0.0, 1.0, 0.0),
            Vector(0.0, 0.0, 0.0, 2.5)
          )
        ).toOption.get
      )
      .toOption
      .get
    val colMetric = diagonal(R.g7ColWeights)

    val resolved = GmdBackend.resolve(GmdBackend.Auto, xSparse, sparseMetric, colMetric, StoragePolicy.PreserveSparse)
    assertEquals(resolved, GmdBackend.Deflation())

    val eigenRejected = GenPca.fit(
      xSparse,
      k(2),
      Some(sparseMetric),
      Some(colMetric),
      preproc = PreprocessSpec.Pass,
      backend = GmdBackend.Eigen(),
      policy = StoragePolicy.PreserveSparse
    )
    assert(eigenRejected.isLeft)

    val deflationFit = GenPca
      .fit(
        xSparse,
        k(2),
        Some(sparseMetric),
        Some(colMetric),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Auto,
        policy = StoragePolicy.PreserveSparse
      )
      .toOption
      .get
    val denseFit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g7X)),
        k(2),
        Some(sparseMetric),
        Some(colMetric),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    var j = 0
    while j < 2 do
      assertEqualsDouble(deflationFit.d(j), denseFit.d(j), 1e-4 * (1.0 + denseFit.d(j)))
      j += 1
  }

  test("backend auto resolution prefers eigen for small dense problems") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g2X))
    val identity = MvMetric.identity(6).toOption.get
    val colMetric = diagonal(R.g2ColWeights)
    assertEquals(
      GmdBackend.resolve(GmdBackend.Auto, x, identity, colMetric, StoragePolicy.AllowDense),
      GmdBackend.Eigen()
    )
  }

  test("metric shape mismatches are rejected with the offending axis") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g2X))
    GenPca.fit(x, k(2), rowMetric = Some(diagonal(R.g2ColWeights))) match
      case Left(MultivarError.MetricShapeMismatch(IndexAxis.Row, expected, actual)) =>
        assertEquals(expected, 6)
        assertEquals(actual, 4)
      case other => fail(s"expected a row metric shape mismatch, got $other")
  }

  // ------------------------------------------------------------------ goldens

  test("G1: identity metrics match the R genpca reference") {
    val fit = GenPca
      .fit(MatrixView.dense(DoubleMatrix.fromRows(R.g1X)), k(2), preproc = PreprocessSpec.Pass)
      .toOption
      .get
    assertGolden(fit, R.g1Sdev, R.g1Ou, R.g1Ov, R.g1Propv, 1e-6)
  }

  test("G2: diagonal column metric matches the R genpca reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g2X)),
        k(3),
        colMetric = Some(diagonal(R.g2ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    assertGolden(fit, R.g2Sdev, R.g2Ou, R.g2Ov, R.g2Propv, 1e-6)
  }

  test("G3: diagonal row and column metrics match the R genpca reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g3X)),
        k(3),
        Some(diagonal(R.g3RowWeights)),
        Some(diagonal(R.g3ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    assertGolden(fit, R.g3Sdev, R.g3Ou, R.g3Ov, R.g3Propv, 1e-6)
  }

  test("G4: dense SPD column metric matches the R genpca reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g4X)),
        k(3),
        colMetric = Some(denseMetric(R.g4ColMetric)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    assertGolden(fit, R.g4Sdev, R.g4Ou, R.g4Ov, R.g4Propv, 1e-5)
  }

  test("G5: dense SPD metrics with centering match the R genpca reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g5X)),
        k(3),
        Some(denseMetric(R.g5RowMetric)),
        Some(denseMetric(R.g5ColMetric)),
        preproc = PreprocessSpec.Center,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    assertGolden(fit, R.g5Sdev, R.g5Ou, R.g5Ov, R.g5Propv, 1e-5)
  }

  test("G6: wide data exercises the dual path and matches the R genpca reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g6X)),
        k(2),
        Some(diagonal(R.g6RowWeights)),
        Some(diagonal(R.g6ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    assertGolden(fit, R.g6Sdev, R.g6Ou, R.g6Ov, R.g6Propv, 1e-6)
  }

  test("G7: sparse data on the sparse-preserving eigen path matches the R genpca reference") {
    val xSparse = SparseMatrixView.fromRows(R.g7X).toOption.get
    val fit = GenPca
      .fit(
        xSparse,
        k(2),
        Some(diagonal(R.g7RowWeights)),
        Some(diagonal(R.g7ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen(),
        policy = StoragePolicy.PreserveSparse
      )
      .toOption
      .get
    assertGolden(fit, R.g7Sdev, R.g7Ou, R.g7Ov, R.g7Propv, 1e-6)
  }

  test("G8: the deflation backend matches the R genpca deflation reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g8X)),
        k(2),
        Some(diagonal(R.g8RowWeights)),
        Some(diagonal(R.g8ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Deflation(threshold = 1e-12, maxIterations = 10000)
      )
      .toOption
      .get
    var j = 0
    while j < 2 do
      assertEqualsDouble(fit.d(j), R.g8Sdev(j), 1e-4 * (1.0 + R.g8Sdev(j)))
      j += 1
    assertVectorClose(fit.propV, R.g8Propv, 1e-4)
    assertMetricOrthonormal(fit.ou, diagonal(R.g8RowWeights), 1e-6)
    assertMetricOrthonormal(fit.ov, diagonal(R.g8ColWeights), 1e-6)
  }

  test("G9: a rank-deficient PSD column metric drops null directions like the R reference") {
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(R.g9X)),
        k(3),
        colMetric = Some(denseMetric(R.g9ColMetric)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    assertEquals(fit.componentCount, R.g9Sdev.length)
    assertGolden(fit, R.g9Sdev, R.g9Ou, R.g9Ov, R.g9Propv, 1e-5)
  }
