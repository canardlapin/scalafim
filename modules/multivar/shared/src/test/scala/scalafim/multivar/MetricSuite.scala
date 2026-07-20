package scalafim.multivar

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector

class MetricSuite extends munit.FunSuite:

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

  private def denseReference(metric: MvMetric): DoubleMatrix =
    metric.toDense(StoragePolicy.AllowDense).toOption.get

  private def contractReference(metric: DoubleMatrix, g: DoubleMatrix): Double =
    var acc = 0.0
    var row = 0
    while row < metric.rows do
      var col = 0
      while col < metric.cols do
        acc += metric(row, col) * g(row, col)
        col += 1
      row += 1
    acc

  private val block = DoubleMatrix.fromRows(
    Vector(
      Vector(1.0, 4.0),
      Vector(2.0, 5.0),
      Vector(3.0, 6.0)
    )
  )

  private val gram = DoubleMatrix.fromRows(
    Vector(
      Vector(2.0, 1.0, 0.0),
      Vector(1.0, 3.0, 1.0),
      Vector(0.0, 1.0, 4.0)
    )
  )

  private val x = DoubleVector.fromSeq(Vector(1.0, -2.0, 3.0))
  private val y = DoubleVector.fromSeq(Vector(0.5, 1.0, -1.0))

  private def allMetrics: Vector[(String, MvMetric)] =
    val spd = DoubleMatrix.fromRows(
      Vector(
        Vector(4.0, 1.0, 0.0),
        Vector(1.0, 3.0, 1.0),
        Vector(0.0, 1.0, 2.0)
      )
    )
    val sparse = SparseMatrixView.fromRows(
      Vector(
        Vector(2.0, 0.0, 1.0),
        Vector(0.0, 3.0, 0.0),
        Vector(1.0, 0.0, 4.0)
      )
    ).toOption.get
    Vector(
      "identity" -> MvMetric.identity(3).toOption.get,
      "diagonal" -> MvMetric.diagonal(DoubleVector.fromSeq(Vector(2.0, 0.5, 1.5))).toOption.get,
      "dense" -> MvMetric.denseSymmetric(spd).toOption.get,
      "sparse" -> MvMetric.sparseSymmetric(sparse).toOption.get
    )

  test("all metric kinds agree with their dense reference on matvec, inner products, and contraction") {
    for (label, metric) <- allMetrics do
      val dense = denseReference(metric)

      val viaMetric = metric.matvec(block).toOption.get
      val viaDense = DoubleMatrix.multiply(dense, block)
      assertMatrixClose(viaMetric, viaDense, 1e-12)

      val vector = metric.applyVector(x).toOption.get
      var row = 0
      while row < 3 do
        var acc = 0.0
        var col = 0
        while col < 3 do
          acc += dense(row, col) * x(col)
          col += 1
        assertEqualsDouble(vector(row), acc, 1e-12)
        row += 1

      val inner = metric.innerProduct(x, y).toOption.get
      var expectedInner = 0.0
      row = 0
      while row < 3 do
        var col = 0
        while col < 3 do
          expectedInner += x(row) * dense(row, col) * y(col)
          col += 1
        row += 1
      assertEqualsDouble(inner, expectedInner, 1e-12)

      val quad = metric.quadNorm(x).toOption.get
      assert(quad >= 0.0, s"$label quadNorm must be non-negative, got $quad")

      val contracted = metric.contract(gram).toOption.get
      assertEqualsDouble(contracted, contractReference(dense, gram), 1e-12)
  }

  test("metric constructors reject invalid shapes, asymmetry, and negative diagonals") {
    assertEquals(MvMetric.identity(0), Left(MultivarError.InvalidDimension("metric dimension", 0)))

    val negative = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, -0.5)))
    assertEquals(negative, Left(MultivarError.NonPositiveSemiDefinite("diagonal metric", -0.5)))

    MvMetric.denseSymmetric(block) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("square"), detail)
      case other =>
        fail(s"expected MatrixShapeMismatch for a rectangular metric, got $other")

    val asymmetric = MvMetric.denseSymmetric(
      DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(0.0, 1.0)))
    )
    assertEquals(asymmetric, Left(MultivarError.NonSymmetricMatrix(0, 1, 2.0, 0.0)))

    val asymmetricSparse = SparseMatrixView.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(0.0, 1.0)
      )
    ).toOption.get
    MvMetric.sparseSymmetric(asymmetricSparse) match
      case Left(MultivarError.MetricMismatch(detail)) =>
        assert(detail.contains("no stored mirror"), detail)
        assert(detail.contains("both triangles"), detail)
      case other =>
        fail(s"expected a missing-mirror metric mismatch, got $other")

    val unbalancedSparse = SparseMatrixView.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 1.0)
      )
    ).toOption.get
    assertEquals(
      MvMetric.sparseSymmetric(unbalancedSparse),
      Left(MultivarError.NonSymmetricMatrix(0, 1, 2.0, 3.0))
    )
  }

  test("sameValues identifies separately built metrics by kind, dimension, and entries") {
    val identityA = MvMetric.identity(3).toOption.get
    val identityB = MvMetric.identity(3).toOption.get
    val identityOther = MvMetric.identity(4).toOption.get
    assert(identityA.sameValues(identityB))
    assert(!identityA.sameValues(identityOther))

    val diagonalA = MvMetric.diagonal(DoubleVector.fromSeq(Vector(2.0, 0.5, 1.5))).toOption.get
    val diagonalB = MvMetric.diagonal(DoubleVector.fromSeq(Vector(2.0, 0.5, 1.5))).toOption.get
    val diagonalOther = MvMetric.diagonal(DoubleVector.fromSeq(Vector(2.0, 0.5, 1.0))).toOption.get
    assert(diagonalA.sameValues(diagonalB), "separately built identical diagonal metrics must compare equal by value")
    assert(!diagonalA.sameValues(diagonalOther))

    val spdRows = Vector(
      Vector(4.0, 1.0, 0.0),
      Vector(1.0, 3.0, 1.0),
      Vector(0.0, 1.0, 2.0)
    )
    val denseA = MvMetric.denseSymmetric(DoubleMatrix.fromRows(spdRows)).toOption.get
    val denseB = MvMetric.denseSymmetric(DoubleMatrix.fromRows(spdRows)).toOption.get
    assert(denseA.sameValues(denseB))

    val sparseRows = Vector(
      Vector(2.0, 0.0, 1.0),
      Vector(0.0, 3.0, 0.0),
      Vector(1.0, 0.0, 4.0)
    )
    val sparseA = MvMetric.sparseSymmetric(SparseMatrixView.fromRows(sparseRows).toOption.get).toOption.get
    val sparseB = MvMetric.sparseSymmetric(SparseMatrixView.fromRows(sparseRows).toOption.get).toOption.get
    assert(sparseA.sameValues(sparseB))

    val onesDiagonal = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, 1.0, 1.0))).toOption.get
    assert(!identityA.sameValues(onesDiagonal), "sameValues is kind-sensitive, not just numerically equivalent")
    assert(!diagonalA.sameValues(denseA))
  }

  test("diagonal metrics clamp roundoff negatives to zero and stay factorizable") {
    val metric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(1.0, -5e-11))).toOption.get

    metric match
      case MvMetric.Diagonal(weights, _) =>
        assertEqualsDouble(weights(1), 0.0, 0.0)
      case other =>
        fail(s"expected a diagonal metric, got $other")

    MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.AllowDense) match
      case Right(roots) =>
        assertEquals(roots.rank, 1)
        val half = roots.half.applyLeft(DoubleMatrix.eye(2))
        assertEqualsDouble(half(0, 0), 1.0, 1e-12)
        assertEqualsDouble(half(1, 1), 0.0, 1e-12)
      case Left(error) =>
        fail(s"expected a construction-accepted diagonal metric to be factorizable, got $error")
  }

  test("strict PSD validation rejects an indefinite matrix that structural checks accept") {
    val indefinite = DoubleMatrix.fromRows(Vector(Vector(0.0, 1.0), Vector(1.0, 0.0)))

    assert(MvMetric.denseSymmetric(indefinite, MetricValidation.Structural).isRight)
    assert(MvMetric.denseSymmetric(indefinite, MetricValidation.Trusted).isRight)

    MvMetric.denseSymmetric(indefinite, MetricValidation.StrictPsd()) match
      case Left(MultivarError.NonPositiveSemiDefinite(role, eigenvalue)) =>
        assertEquals(role, "dense metric")
        assertEqualsDouble(eigenvalue, -1.0, 1e-9)
      case other =>
        fail(s"expected NonPositiveSemiDefinite, got $other")
  }

  test("metric space tags must match the metric dimension") {
    val space = MvSpace.of("rows", SpaceRole.Samples, 4).toOption.get
    val result = MvMetric.identity(3, Some(space))
    assert(result.isLeft)
  }

  test("diagonal square roots pseudo-invert on the range and report rank") {
    val metric = MvMetric.diagonal(DoubleVector.fromSeq(Vector(4.0, 0.0, 9.0))).toOption.get
    val roots = MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.AllowDense).toOption.get

    assertEquals(roots.rank, 2)
    val eye3 = DoubleMatrix.eye(3)
    val half = roots.half.applyLeft(eye3)
    val pinv = roots.pinvHalf.applyLeft(eye3)
    assertEqualsDouble(half(0, 0), 2.0, 1e-12)
    assertEqualsDouble(half(1, 1), 0.0, 1e-12)
    assertEqualsDouble(half(2, 2), 3.0, 1e-12)
    assertEqualsDouble(pinv(0, 0), 0.5, 1e-12)
    assertEqualsDouble(pinv(1, 1), 0.0, 1e-12)
    assertEqualsDouble(pinv(2, 2), 1.0 / 3.0, 1e-12)
  }

  test("dense SPD square roots reconstruct the metric and invert it on the range") {
    val spd = DoubleMatrix.fromRows(
      Vector(
        Vector(4.0, 1.0, 0.0),
        Vector(1.0, 3.0, 1.0),
        Vector(0.0, 1.0, 2.0)
      )
    )
    val metric = MvMetric.denseSymmetric(spd).toOption.get
    val roots = MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.AllowDense).toOption.get

    assertEquals(roots.rank, 3)
    val half = roots.half.applyLeft(DoubleMatrix.eye(3))
    assertMatrixClose(DoubleMatrix.multiply(half, half), spd, 1e-9)

    val pinv = roots.pinvHalf.applyLeft(DoubleMatrix.eye(3))
    val whitened = DoubleMatrix.multiply(pinv, DoubleMatrix.multiply(spd, pinv))
    assertMatrixClose(whitened, DoubleMatrix.eye(3), 1e-9)
  }

  test("rank-deficient PSD square roots drop the null space instead of failing") {
    val psd = DoubleMatrix.fromRows(Vector(Vector(1.0, 1.0), Vector(1.0, 1.0)))
    val metric = MvMetric.denseSymmetric(psd).toOption.get
    val roots = MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.AllowDense).toOption.get

    assertEquals(roots.rank, 1)
    val half = roots.half.applyLeft(DoubleMatrix.eye(2))
    assertMatrixClose(DoubleMatrix.multiply(half, half), psd, 1e-9)

    val pinv = roots.pinvHalf.applyLeft(DoubleMatrix.eye(2))
    val projector = DoubleMatrix.multiply(pinv, DoubleMatrix.multiply(psd, pinv))
    assertMatrixClose(projector, MatrixOps.scale(psd, 0.5), 1e-9)
  }

  test("indefinite metrics fail at square-root time even when constructed as trusted") {
    val indefinite = DoubleMatrix.fromRows(Vector(Vector(0.0, 1.0), Vector(1.0, 0.0)))
    val metric = MvMetric.denseSymmetric(indefinite, MetricValidation.Trusted).toOption.get

    MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.AllowDense) match
      case Left(MultivarError.NonPositiveSemiDefinite(_, eigenvalue)) =>
        assertEqualsDouble(eigenvalue, -1.0, 1e-9)
      case other =>
        fail(s"expected NonPositiveSemiDefinite, got $other")
  }

  test("sparse metric square roots respect the densification policy") {
    val sparse = SparseMatrixView.fromRows(
      Vector(
        Vector(2.0, 0.0, 1.0),
        Vector(0.0, 3.0, 0.0),
        Vector(1.0, 0.0, 4.0)
      )
    ).toOption.get
    val metric = MvMetric.sparseSymmetric(sparse).toOption.get

    assertEquals(
      MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.PreserveSparse),
      Left(MultivarError.DensificationRejected("metric square root", StorageKind.Sparse))
    )

    val roots = MetricSqrt.factor(metric, DenseSolvers.symmetricEigen, 1e-12, StoragePolicy.AllowDense).toOption.get
    val half = roots.half.applyLeft(DoubleMatrix.eye(3))
    assertMatrixClose(DoubleMatrix.multiply(half, half), denseReference(metric), 1e-9)
  }

  test("sparse row scaling matches the dense equivalent") {
    val sparse = SparseMatrixView.fromRows(
      Vector(
        Vector(1.0, 0.0, 2.0),
        Vector(0.0, 3.0, 0.0),
        Vector(4.0, 0.0, 5.0)
      )
    ).toOption.get
    val scale = DoubleVector.fromSeq(Vector(2.0, -1.0, 0.5))

    val scaled = sparse.scaleRows(scale).toOption.get.toDense(StoragePolicy.AllowDense).toOption.get
    val expected = MatrixView.scaleRows(sparse.toDense(StoragePolicy.AllowDense).toOption.get, scale)
    assertMatrixClose(scaled, expected, 1e-12)
  }
