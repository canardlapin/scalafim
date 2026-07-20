package scalafim.linalg

class DecompositionSuite extends munit.FunSuite:

  test("Jacobi symmetric eigen solver returns descending eigenpairs with deterministic signs") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(2.0, 1.0),
        Vector(1.0, 2.0)
      )
    )

    val eigen = LinalgSolvers.symmetricEigen.decompose(matrix).toOption.get

    assertEqualsDouble(eigen.values(0), 3.0, 1e-10)
    assertEqualsDouble(eigen.values(1), 1.0, 1e-10)
    assert(eigen.vectors(0, 0) > 0.0)
    assert(eigen.vectors(0, 1) > 0.0)
    assertMatrixClose(
      DoubleMatrix.multiply(matrix, eigen.vectors),
      DoubleMatrix.multiply(eigen.vectors, diagonal(eigen.values)),
      1e-10
    )
    assertMatrixClose(
      DoubleMatrix.transposeMultiply(eigen.vectors, eigen.vectors),
      DoubleMatrix.eye(2),
      1e-10
    )
  }

  test("symmetric eigen solver rejects non-symmetric inputs at the linalg boundary") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(1.0, 1.0)
      )
    )

    LinalgSolvers.symmetricEigen.decompose(matrix) match
      case Left(LinearAlgebraError.NonSymmetricMatrix(0, 1, 2.0, 1.0)) => ()
      case other => fail(s"expected non-symmetric matrix error, got $other")
  }

  test("Gram dense SVD reconstructs the input at full rank") {
    val input = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 2.0),
        Vector(0.0, 0.0)
      )
    )

    val svd = LinalgSolvers.denseSvd.decompose(input, DecompositionRank.unsafe(2)).toOption.get
    val reconstruction = DoubleMatrix.multiply(DoubleMatrix.multiply(svd.u, diagonal(svd.singularValues)), svd.v.transpose)

    assertEqualsDouble(svd.singularValues(0), 2.0, 1e-10)
    assertEqualsDouble(svd.singularValues(1), 1.0, 1e-10)
    assertMatrixClose(reconstruction, input, 1e-10)
    assertMatrixClose(DoubleMatrix.transposeMultiply(svd.u, svd.u), DoubleMatrix.eye(2), 1e-10)
    assertMatrixClose(DoubleMatrix.transposeMultiply(svd.v, svd.v), DoubleMatrix.eye(2), 1e-10)
  }

  test("generalized eigen solver satisfies A v = lambda B v") {
    val a = DoubleMatrix.fromRows(
      Vector(
        Vector(6.0, 0.0),
        Vector(0.0, 4.0)
      )
    )
    val b = DoubleMatrix.fromRows(
      Vector(
        Vector(3.0, 0.0),
        Vector(0.0, 1.0)
      )
    )

    val eigen = LinalgSolvers.generalizedEigen.decompose(a, b, DecompositionRank.unsafe(2)).toOption.get

    assertEqualsDouble(eigen.values(0), 4.0, 1e-10)
    assertEqualsDouble(eigen.values(1), 2.0, 1e-10)
    assertMatrixClose(
      DoubleMatrix.multiply(a, eigen.vectors),
      DoubleMatrix.multiply(DoubleMatrix.multiply(b, eigen.vectors), diagonal(eigen.values)),
      1e-10
    )
  }

  test("SPD inverse solver inverts positive-definite matrices and rejects non-PD input") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(4.0, 1.0),
        Vector(1.0, 3.0)
      )
    )

    val inverse = LinalgSolvers.spdInverse.invert(matrix).toOption.get

    assertMatrixClose(DoubleMatrix.multiply(matrix, inverse), DoubleMatrix.eye(2), 1e-10)
    assertMatrixClose(inverse, inverse.transpose, 1e-12)

    val singular = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 1.0),
        Vector(1.0, 1.0)
      )
    )
    assert(LinalgSolvers.spdInverse.invert(singular).isLeft)
  }

  test("Jacobi symmetric eigensolver recovers a diagonal eigensystem in descending order") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(2.0, 0.0),
        Vector(0.0, 5.0)
      )
    )

    val eigen = LinalgSolvers.symmetricEigen.decompose(matrix).toOption.get

    assertEqualsDouble(eigen.values(0), 5.0, 1e-10)
    assertEqualsDouble(eigen.values(1), 2.0, 1e-10)
    assertEqualsDouble(Math.abs(eigen.vectors(1, 0)), 1.0, 1e-10)
    assertEqualsDouble(Math.abs(eigen.vectors(0, 1)), 1.0, 1e-10)
  }

  test("Jacobi symmetric eigensolver converges on larger dense SPD matrices") {
    for (n, seed) <- Vector((10, 17L), (20, 91L)) do
      val matrix = deterministicSpd(n, seed)
      val eigen = LinalgSolvers.symmetricEigen.decompose(matrix) match
        case Right(result) => result
        case Left(error)   => fail(s"decompose failed for n=$n: ${error.message}")

      var trace = 0.0
      var valueSum = 0.0
      var i = 0
      while i < n do
        trace += matrix(i, i)
        valueSum += eigen.values(i)
        if i > 0 then assert(eigen.values(i) <= eigen.values(i - 1) + 1e-9, "eigenvalues must be descending")
        i += 1
      assertEqualsDouble(valueSum, trace, 1e-7)

      val av = DoubleMatrix.multiply(matrix, eigen.vectors)
      var col = 0
      while col < n do
        var row = 0
        while row < n do
          assertEqualsDouble(av(row, col), eigen.values(col) * eigen.vectors(row, col), 1e-7)
          row += 1
        col += 1

      assertMatrixClose(DoubleMatrix.crossProduct(eigen.vectors), DoubleMatrix.eye(n), 1e-9)
  }

  private def deterministicSpd(n: Int, seed: Long): DoubleMatrix =
    var state = seed
    def next(): Double =
      state = state * 6364136223846793005L + 1442695040888963407L
      ((state >>> 11).toDouble / (1L << 53).toDouble) * 2.0 - 1.0
    val rows = Vector.tabulate(n, n)((_, _) => next())
    val gram = DoubleMatrix.crossProduct(DoubleMatrix.fromRows(rows))
    val out = gram.copyData
    var i = 0
    while i < n do
      out(i * n + i) += 0.5
      i += 1
    DoubleMatrix.unsafe(n, n, out)

  test("decomposition rank is a positive bounded scalar") {
    assertEquals(DecompositionRank(2).toOption.map(_.value), Some(2))
    assert(DecompositionRank(0).isLeft)
    assert(DecompositionRank.bounded(3, 2).isLeft)
  }

  private def diagonal(values: DoubleVector): DoubleMatrix =
    val out = new Array[Double](values.length * values.length)
    var i = 0
    while i < values.length do
      out(i * values.length + i) = values(i)
      i += 1
    DoubleMatrix.unsafe(values.length, values.length, out)

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
