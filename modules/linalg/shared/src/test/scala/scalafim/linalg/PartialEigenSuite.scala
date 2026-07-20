package scalafim.linalg

class PartialEigenSuite extends munit.FunSuite:
  private val solver = DenseReferencePartialEigenSolver(maximumOrder = 32)
  private val tolerance = 1e-8

  test("smallest and largest requests have explicit opposite ordering"):
    val operator = denseOperator(diagonal(Vector(4.0, -2.0, 7.0, 1.0)))
    val rank = DecompositionRank.bounded(3, operator.order).toOption.get
    val smallest = solver.smallestK(operator, rank).toOption.get
    val largest = solver.largestK(operator, rank).toOption.get

    assertEquals(smallest.values.toVector, Vector(-2.0, 1.0, 4.0))
    assertEquals(largest.values.toVector, Vector(7.0, 4.0, 1.0))
    assertEquals(smallest.spectrum, PartialSpectrum.Smallest)
    assertEquals(largest.spectrum, PartialSpectrum.Largest)
    assertEquals(smallest.vectors.rows, 4)
    assertEquals(smallest.vectors.cols, 3)

  test("returned pairs satisfy residual and orthonormality contracts"):
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(4.0, 1.0, 0.0, 0.0),
        Vector(1.0, 3.0, 0.5, 0.0),
        Vector(0.0, 0.5, 2.0, 0.25),
        Vector(0.0, 0.0, 0.25, 1.0)
      )
    )
    val result = solver.smallestK(denseOperator(matrix), DecompositionRank.unsafe(3)).toOption.get

    result.residualNorms.toVector.foreach(residual => assert(residual < tolerance))
    assertMatrixClose(
      DoubleMatrix.transposeMultiply(result.vectors, result.vectors),
      DoubleMatrix.eye(3),
      tolerance
    )

  test("declared sparse operators materialize through LinearMap rather than CSR-specific APIs"):
    val indices = Array(0, 1, 2, 3)
    val csr = CsrMatrix.fromTriplets(4, 4, indices, indices, Array(9.0, 4.0, 1.0, -3.0)).toOption.get
    val operator = SymmetricOperator.declared(csr).toOption.get
    val result = solver.smallestK(operator, DecompositionRank.unsafe(2)).toOption.get

    assertEquals(result.values.toVector, Vector(-3.0, 1.0))
    assert(result.residualNorms.toVector.forall(_ < tolerance))

  test("permutation similarity preserves eigenvalues and repeated eigenspaces"):
    val original = diagonal(Vector(5.0, 5.0, 2.0, 1.0))
    val permutation = Vector(2, 0, 3, 1)
    val permuted = permuteSymmetric(original, permutation)
    val rank = DecompositionRank.unsafe(2)
    val first = solver.largestK(denseOperator(original), rank).toOption.get
    val second = solver.largestK(denseOperator(permuted), rank).toOption.get
    val secondBack = unpermuteRows(second.vectors, permutation)

    assertVectorClose(first.values.toVector, second.values.toVector, tolerance)
    assertMatrixClose(projector(first.vectors), projector(secondBack), tolerance)

  test("near-repeated spectra preserve the requested invariant subspace"):
    val angle = 0.37
    val cosine = Math.cos(angle)
    val sine = Math.sin(angle)
    val q = DoubleMatrix.fromRows(
      Vector(
        Vector(cosine, -sine, 0.0),
        Vector(sine, cosine, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val values = diagonal(Vector(3.0, 3.0 + 1e-9, 0.5))
    val matrix = DoubleMatrix.multiply(DoubleMatrix.multiply(q, values), q.transpose)
    val result = solver.largestK(denseOperator(matrix), DecompositionRank.unsafe(2)).toOption.get
    val expectedProjector = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0)
      )
    )

    assertMatrixClose(projector(result.vectors), expectedProjector, 1e-7)

  test("scale changes preserve selected eigenspaces and relative residual quality"):
    val base = DoubleMatrix.fromRows(
      Vector(
        Vector(3.0, 1.0, 0.0),
        Vector(1.0, 2.0, 0.5),
        Vector(0.0, 0.5, 1.0)
      )
    )
    val reference = solver.largestK(denseOperator(base), DecompositionRank.unsafe(2)).toOption.get

    Vector(1e-8, 1e8).foreach: scale =>
      val scaled = scaleMatrix(base, scale)
      val result = solver.largestK(denseOperator(scaled), DecompositionRank.unsafe(2)).toOption.get
      result.values.toVector.zip(reference.values.toVector).foreach: (actual, expected) =>
        assertEqualsDouble(actual / scale, expected, 1e-7)
      assertMatrixClose(projector(result.vectors), projector(reference.vectors), 1e-7)
      result.residualNorms.toVector.zip(result.values.toVector).foreach: (residual, eigenvalue) =>
        assert(residual / Math.max(1.0, Math.abs(eigenvalue)) < 1e-7)

  test("rank, symmetry, finiteness, and square-shape boundaries fail explicitly"):
    val operator = denseOperator(diagonal(Vector(2.0, 1.0)))
    assertEquals(
      solver.largestK(operator, DecompositionRank.unsafe(3)).left.toOption,
      Some(LinearAlgebraError.InvalidDecompositionRank(3, 2))
    )
    assert(SymmetricOperator.fromDense(DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(0.0, 1.0)))).isLeft)
    assert(SymmetricOperator.fromDense(DoubleMatrix.fromRows(Vector(Vector(1.0, Double.NaN), Vector(Double.NaN, 1.0)))).isLeft)
    assert(SymmetricOperator.declared(RectangularMap(2, 3)).isLeft)

  test("dense reference size boundary rejects before applying the operator"):
    val map = CountingIdentityMap(5)
    val operator = SymmetricOperator.declared(map).toOption.get
    val bounded = DenseReferencePartialEigenSolver(maximumOrder = 4)
    val result = bounded.smallestK(operator, DecompositionRank.unsafe(1))

    assertEquals(
      result.left.toOption,
      Some(LinearAlgebraError.OperatorOrderUnsupported("dense reference partial eigensolver", 5, 4))
    )
    assertEquals(map.applications, 0)

  test("operator application failures remain typed"):
    val operator = SymmetricOperator.declared(FailingSquareMap(3)).toOption.get
    val result = solver.smallestK(operator, DecompositionRank.unsafe(1))

    assert(result.left.toOption.exists(_.message.contains("linear operator application failed")))

  private def denseOperator(matrix: DoubleMatrix): SymmetricOperator =
    SymmetricOperator.fromDense(matrix).toOption.get

  private def diagonal(values: Vector[Double]): DoubleMatrix =
    val out = Array.fill(values.length * values.length)(0.0)
    values.indices.foreach(index => out(index * values.length + index) = values(index))
    DoubleMatrix.fromRowMajor(MatrixShape.from(values.length, values.length).toOption.get, out).toOption.get

  private def projector(vectors: DoubleMatrix): DoubleMatrix =
    DoubleMatrix.multiply(vectors, vectors.transpose)

  private def scaleMatrix(matrix: DoubleMatrix, scale: Double): DoubleMatrix =
    DoubleMatrix.fromRowMajor(matrix.shape, matrix.copyData.map(_ * scale)).toOption.get

  private def permuteSymmetric(matrix: DoubleMatrix, order: Vector[Int]): DoubleMatrix =
    DoubleMatrix.fromRows(order.map(row => order.map(col => matrix(row, col))))

  private def unpermuteRows(matrix: DoubleMatrix, permutation: Vector[Int]): DoubleMatrix =
    val sourceRowByOriginal = permutation.zipWithIndex.toMap
    DoubleMatrix.fromRows(Vector.tabulate(matrix.rows)(row => matrix.row(sourceRowByOriginal(row)).toVector))

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach: (left, right) =>
      assertEqualsDouble(left, right, tolerance)

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private final case class RectangularMap(rows: Int, cols: Int) extends LinearMap:
    def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
      Left(LinearMapError.DimensionMismatch(cols, input.rows))
    def adjoint: LinearMap = RectangularMap(cols, rows)

  private final class CountingIdentityMap(val rows: Int) extends LinearMap:
    val cols: Int = rows
    var applications: Int = 0
    def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
      applications += 1
      Right(input)
    def adjoint: LinearMap = this

  private object CountingIdentityMap:
    def apply(order: Int): CountingIdentityMap = new CountingIdentityMap(order)

  private final case class FailingSquareMap(rows: Int) extends LinearMap:
    val cols: Int = rows
    def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
      Left(LinearMapError.DimensionMismatch(rows + 1, input.rows))
    def adjoint: LinearMap = this
