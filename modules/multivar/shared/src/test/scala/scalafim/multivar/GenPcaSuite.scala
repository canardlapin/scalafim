package scalafim.multivar

import scala.annotation.nowarn

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

/** The legacy raw-array overload is deliberately exercised here for compatibility.
  * Safe-boundary behavior is covered independently through `Unsafe` and the semantic
  * diagram suites.
  */
@nowarn("cat=deprecation")
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

  test("raw-array generalized PCA requires an explicit unsafe reason") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g2X))
    Unsafe.genPcaFromArrays(x, k(2), reason = "") match
      case Left(MultivarError.InvalidMap(detail)) =>
        assert(detail.contains("non-empty reason"))
      case other => fail(s"expected an unsafe-boundary rejection, got $other")

    assert(
      Unsafe
        .genPcaFromArrays(x, k(2), reason = "GenPcaSuite compatibility fixture")
        .isRight
    )
  }

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

  test("fitting a DualityDiagram is equivalent to metric arguments and preserves its column space") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g5X))
    val rowSpace = MvSpace.of("trial.rows", SpaceRole.Samples, x.rows).toOption.get
    val columnSpace = MvSpace.of("voxel.features", SpaceRole.Observed, x.cols).toOption.get
    val rowMetric = MvMetric.denseSymmetric(DoubleMatrix.fromRows(R.g5RowMetric), space = Some(rowSpace)).toOption.get
    val colMetric = MvMetric.denseSymmetric(DoubleMatrix.fromRows(R.g5ColMetric), space = Some(columnSpace)).toOption.get
    val diagram = DualityDiagram
      .from(x, rowMetric = Some(rowMetric), columnMetric = Some(colMetric), rowSpace = Some(rowSpace), columnSpace = Some(columnSpace))
      .toOption
      .get
    val fromDiagram = GenPca
      .fit(
        diagram,
        k(3),
        PreprocessSpec.Pass,
        GmdBackend.Eigen(),
        StoragePolicy.AllowDense,
        DenseSolvers.symmetricEigen,
        DenseSolvers.svd
      )
      .toOption
      .get
    val fromArguments = GenPca
      .fit(x, k(3), Some(rowMetric), Some(colMetric), preproc = PreprocessSpec.Pass, backend = GmdBackend.Eigen())
      .toOption
      .get
    val (diagramOu, diagramOv) = canonicalize(fromDiagram.ou, fromDiagram.ov)
    val (argumentOu, argumentOv) = canonicalize(fromArguments.ou, fromArguments.ov)

    assertEquals(fromDiagram.projection.map.domain, columnSpace)
    assertVectorClose(fromDiagram.d, Vector.tabulate(3)(fromArguments.d(_)), 1e-9)
    assertMatrixClose(diagramOu, argumentOu, 1e-8)
    assertMatrixClose(diagramOv, argumentOv, 1e-8)
    assertVectorClose(fromDiagram.propV, Vector.tabulate(3)(fromArguments.propV(_)), 1e-9)
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

    val diagnostics = deflationFit.projection.diagnostics.get
    assertEquals(diagnostics.backend, Some("deflation"))
    assertEquals(diagnostics.storagePolicy, Some(StoragePolicy.PreserveSparse))
    assertEquals(diagnostics.components, k(2))
    assertEquals(diagnostics.effectiveComponents, 2)
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

  // ------------------------------------------------------------- hardening

  test("deflation honors the metric scale: c*I metrics rescale the identity GMD analytically") {
    // From X = U D V' with U' (cI) U = I and V' (cI) V = I, substituting the plain
    // SVD X = U0 D0 V0' gives U = U0 / sqrt(c), V = V0 / sqrt(c), D = c * D0.
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g8X))
    val c = 1e12
    val rowScaled = diagonal(Vector.fill(x.rows)(c))
    val colScaled = diagonal(Vector.fill(x.cols)(c))
    val baseline = GenPca
      .fit(
        x,
        k(2),
        Some(diagonal(Vector.fill(x.rows)(1.0))),
        Some(diagonal(Vector.fill(x.cols)(1.0))),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    val scaled = GenPca
      .fit(
        x,
        k(2),
        Some(rowScaled),
        Some(colScaled),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Deflation(threshold = 1e-12, maxIterations = 10000)
      )
      .toOption
      .get

    var j = 0
    while j < 2 do
      assertEqualsDouble(scaled.d(j), c * baseline.d(j), 1e-4 * c * baseline.d(j))
      var uDot = 0.0
      var row = 0
      while row < x.rows do
        uDot += Math.sqrt(c) * scaled.ou(row, j) * baseline.ou(row, j)
        row += 1
      var vDot = 0.0
      row = 0
      while row < x.cols do
        vDot += Math.sqrt(c) * scaled.ov(row, j) * baseline.ov(row, j)
        row += 1
      assertEqualsDouble(Math.abs(uDot), 1.0, 1e-6)
      assertEqualsDouble(Math.abs(vDot), 1.0, 1e-6)
      j += 1
    assertMetricOrthonormal(scaled.ou, rowScaled, 1e-6)
    assertMetricOrthonormal(scaled.ov, colScaled, 1e-6)
  }

  test("requesting more than the rank returns the honest component count on the identity path") {
    // 8x5 data whose fifth column is the sum of the first two: rank 4 after centering.
    val rows = Vector.tabulate(8, 5) { (i, j) =>
      val base = Math.sin((i + 1) * (j + 2) * 0.7) + 0.1 * (i + 1) * (j + 1)
      if j < 4 then base
      else
        val c0 = Math.sin((i + 1) * 2 * 0.7) + 0.1 * (i + 1) * 1
        val c1 = Math.sin((i + 1) * 3 * 0.7) + 0.1 * (i + 1) * 2
        c0 + c1
    }
    val x = MatrixView.dense(DoubleMatrix.fromRows(rows))
    val fit = GenPca.fit(x, k(5), preproc = PreprocessSpec.Center).toOption.get

    assertEquals(fit.componentCount, 4)
    val diagnostics = fit.projection.diagnostics.get
    assertEquals(diagnostics.components, k(5))
    assertEquals(diagnostics.effectiveComponents, 4)
    assertMetricOrthonormal(fit.ou, MvMetric.identity(8).toOption.get, 1e-8)
  }

  test("non-finite input is rejected with a typed error on the identity SVD path") {
    val rows = Vector(Vector(1.0, 2.0), Vector(3.0, Double.NaN), Vector(4.0, 5.0))
    val x = MatrixView.dense(DoubleMatrix.fromRows(rows))
    GenPca.fit(x, k(1), preproc = PreprocessSpec.Pass) match
      case Left(MultivarError.NonFiniteValue(_, _, _)) => ()
      case other => fail(s"expected a typed non-finite rejection, got $other")
  }

  test("tiny-scale full-rank data keeps its full spectrum under the eigen backend") {
    val s = 1e-7
    val rows = R.g3X.map(_.map(_ * s))
    val fit = GenPca
      .fit(
        MatrixView.dense(DoubleMatrix.fromRows(rows)),
        k(4),
        Some(diagonal(R.g3RowWeights)),
        Some(diagonal(R.g3ColWeights)),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get

    assertEquals(fit.componentCount, 4)
    // The rank cutoff no longer rejects the spectrum outright. Eigenpair precision
    // at this scale is capped by the Jacobi solver's absolute convergence floor
    // (tolerance * max(1, maxDiag)), so assert the exactly-preserved invariants —
    // a positive full spectrum whose generalized variance sums to the total — plus
    // coarse proportionality to the reference spectrum.
    var propTotal = 0.0
    var j = 0
    while j < 4 do
      assert(fit.d(j) > 0.0)
      if j < R.g3Sdev.length then assertEqualsDouble(fit.d(j), s * R.g3Sdev(j), 0.5 * s * R.g3Sdev(j))
      propTotal += fit.propV(j)
      j += 1
    assertEqualsDouble(propTotal, 1.0, 1e-6)
  }

  test("an explicit deflation backend is honored under identity metrics") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g3X))
    val fit = GenPca
      .fit(x, k(2), preproc = PreprocessSpec.Pass, backend = GmdBackend.Deflation())
      .toOption
      .get
    assertEquals(fit.projection.diagnostics.flatMap(_.backend), Some("deflation"))

    val reference = GenPca.fit(x, k(2), preproc = PreprocessSpec.Pass).toOption.get
    var j = 0
    while j < 2 do
      assertEqualsDouble(fit.d(j), reference.d(j), 1e-4 * (1.0 + reference.d(j)))
      j += 1
  }

  test("wide identity-metric data takes the n x n dual eigen path and matches the SVD") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g6X))
    assert(x.rows < x.cols)
    val fit = GenPca
      .fit(x, k(2), preproc = PreprocessSpec.Pass, backend = GmdBackend.Eigen())
      .toOption
      .get
    assertEquals(fit.projection.diagnostics.flatMap(_.backend), Some("eigen"))

    val svd = Svd.fit(x, k(2)).toOption.get
    var j = 0
    while j < 2 do
      assertEqualsDouble(fit.d(j), svd.result.singularValues(j), 1e-8 * (1.0 + svd.result.singularValues(j)))
      j += 1
  }

  test("deflation reports iteration exhaustion as a typed error") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(R.g3X))
    GenPca.fit(
      x,
      k(2),
      Some(diagonal(R.g3RowWeights)),
      Some(diagonal(R.g3ColWeights)),
      preproc = PreprocessSpec.Pass,
      backend = GmdBackend.Deflation(threshold = 1e-12, maxIterations = 1)
    ) match
      case Left(MultivarError.IterationLimitExceeded(method, maxIterations, residual)) =>
        assertEquals(method, "gmd deflation")
        assertEquals(maxIterations, 1)
        assert(residual > 1e-12)
      case other => fail(s"expected iteration exhaustion, got $other")
  }

  test("a zero matrix is rejected with a typed error on every backend") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector.fill(4)(Vector.fill(3)(0.0))))
    GenPca.fit(x, k(1), preproc = PreprocessSpec.Pass) match
      case Left(MultivarError.SolverFailed(_)) => ()
      case other => fail(s"expected a typed zero-spectrum failure on the identity path, got $other")
    GenPca.fit(
      x,
      k(1),
      Some(diagonal(Vector.fill(4)(2.0))),
      preproc = PreprocessSpec.Pass,
      backend = GmdBackend.Eigen()
    ) match
      case Left(MultivarError.SolverFailed(_)) => ()
      case other => fail(s"expected a typed zero-spectrum failure on the eigen path, got $other")
    GenPca.fit(
      x,
      k(1),
      Some(diagonal(Vector.fill(4)(2.0))),
      preproc = PreprocessSpec.Pass,
      backend = GmdBackend.Deflation()
    ) match
      case Left(MultivarError.SolverFailed(_)) => ()
      case other => fail(s"expected a typed zero-spectrum failure on the deflation path, got $other")
  }

  test("deflation truncation below the request is reflected in the diagnostics") {
    // Rank-one data: the second requested component must be dropped, not fabricated.
    val rows = Vector.tabulate(4, 3)((i, j) => (i + 1.0) * (j + 1.0))
    val x = MatrixView.dense(DoubleMatrix.fromRows(rows))
    val fit = GenPca
      .fit(
        x,
        k(2),
        Some(diagonal(Vector(1.0, 2.0, 0.5, 1.5))),
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Deflation()
      )
      .toOption
      .get

    assertEquals(fit.componentCount, 1)
    val diagnostics = fit.projection.diagnostics.get
    assertEquals(diagnostics.components, k(2))
    assertEquals(diagnostics.effectiveComponents, 1)
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

  test("wide identity-metric data keeps its rank at large scale and scales linearly") {
    // Regression: the dual-path column renormalization compares the dimensionless
    // quad norm (~1 for every true component) against an eigenvalue-scaled cutoff,
    // so large-magnitude wide data used to lose every component to SolverFailed.
    val rows = 6
    val cols = 50
    val base = Vector.tabulate(rows)(i => Vector.tabulate(cols)(j => Math.sin(1.7 * j + 0.9 * i) + 0.2 * ((i + j) % 3)))
    val factor = 1.0e6
    val scaled = base.map(_.map(_ * factor))
    val small = GenPca
      .fit(MatrixView.dense(DoubleMatrix.fromRows(base)), k(3), preproc = PreprocessSpec.Pass)
      .toOption
      .get

    GenPca.fit(MatrixView.dense(DoubleMatrix.fromRows(scaled)), k(3), preproc = PreprocessSpec.Pass) match
      case Right(big) =>
        assertEquals(big.componentCount, small.componentCount)
        var i = 0
        while i < small.d.length do
          assertEqualsDouble(big.d(i), factor * small.d(i), 1e-6 * factor * small.d(i))
          i += 1
        val (_, smallOv) = canonicalize(small.ou, small.ov)
        val (_, bigOv) = canonicalize(big.ou, big.ov)
        assertMatrixClose(bigOv, smallOv, 1e-6)
      case Left(error) =>
        fail(s"expected the large-scale wide identity fit to succeed, got $error")

    assert(Pca.fit(MatrixView.dense(DoubleMatrix.fromRows(scaled)), k(2)).isRight)
  }

  test("identity-path total variance is stable for lazily centered huge-mean data") {
    // Regression: sparse input keeps centering lazy, and summing the affine view's
    // raw column sumSquares cancels catastrophically when column means dwarf the
    // spread; the centered sums must be used instead.
    val means = Vector(1.1e7, -0.9e7, 1.3e7)
    val raw = Vector.tabulate(8)(i => Vector.tabulate(3)(j => means(j) + 10.0 * Math.sin(1.3 * i + 0.7 * j + 0.5)))
    val sparse = SparseMatrixView.fromRows(raw).toOption.get

    val fitMeans = Vector.tabulate(3)(j => raw.map(_(j)).sum / raw.length)
    val centered = raw.map(row => row.indices.toVector.map(j => row(j) - fitMeans(j)))
    val reference = GenPca
      .fit(MatrixView.dense(DoubleMatrix.fromRows(centered)), k(2), preproc = PreprocessSpec.Pass)
      .toOption
      .get

    val fit = GenPca.fit(sparse, k(2)).toOption.get
    assertEqualsDouble(fit.totalVariance, reference.totalVariance, 1e-6 * reference.totalVariance)
    var i = 0
    while i < reference.propV.length do
      assert(fit.propV(i) >= 0.0 && fit.propV(i) <= 1.0, s"propV($i) = ${fit.propV(i)} is not a proportion")
      assertEqualsDouble(fit.propV(i), reference.propV(i), 1e-2)
      i += 1
  }
