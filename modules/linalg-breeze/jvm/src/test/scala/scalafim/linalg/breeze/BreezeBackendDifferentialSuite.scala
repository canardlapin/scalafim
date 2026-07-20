package scalafim.linalg.breeze

import scalafim.linalg.*

class BreezeBackendDifferentialSuite extends munit.FunSuite:

  test("Breeze symmetric eigensolver matches the portable eigen contract") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(5.0, 1.0, 0.5),
        Vector(1.0, 3.0, 0.25),
        Vector(0.5, 0.25, 2.0)
      )
    )

    val portable = LinalgSolvers.symmetricEigen.decompose(matrix).toOption.get
    val breeze = BreezeSolvers.symmetricEigen.decompose(matrix).toOption.get

    assertVectorClose(breeze.values, portable.values, 1e-9)
    assertMatrixClose(
      DoubleMatrix.multiply(matrix, breeze.vectors),
      DoubleMatrix.multiply(breeze.vectors, diagonal(breeze.values)),
      1e-9
    )
    assertMatrixClose(DoubleMatrix.transposeMultiply(breeze.vectors, breeze.vectors), DoubleMatrix.eye(3), 1e-9)
  }

  test("Breeze dense SVD matches portable singular values and reconstructs input") {
    val input = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 0.0),
        Vector(0.0, 1.0, 3.0),
        Vector(2.0, 0.0, 1.0),
        Vector(1.0, 1.0, 1.0)
      )
    )

    val rank = DecompositionRank.unsafe(3)
    val portable = LinalgSolvers.denseSvd.decompose(input, rank).toOption.get
    val breeze = BreezeSolvers.denseSvd.decompose(input, rank).toOption.get
    val reconstruction = DoubleMatrix.multiply(DoubleMatrix.multiply(breeze.u, diagonal(breeze.singularValues)), breeze.v.transpose)

    assertVectorClose(breeze.singularValues, portable.singularValues, 1e-8)
    assertMatrixClose(reconstruction, input, 1e-8)
    assertMatrixClose(DoubleMatrix.transposeMultiply(breeze.u, breeze.u), DoubleMatrix.eye(3), 1e-8)
    assertMatrixClose(DoubleMatrix.transposeMultiply(breeze.v, breeze.v), DoubleMatrix.eye(3), 1e-8)
  }

  test("Breeze partial operator eigensolver matches portable values and repeated eigenspaces") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(5.0, 0.0, 0.0, 0.0, 0.0),
        Vector(0.0, 5.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 2.0, 0.5, 0.0),
        Vector(0.0, 0.0, 0.5, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0, -2.0)
      )
    )
    val operator = SymmetricOperator.fromDense(matrix).toOption.get
    val rank = DecompositionRank.unsafe(2)
    val portableSolver = DenseReferencePartialEigenSolver(maximumOrder = 16)

    Vector(PartialSpectrum.Smallest, PartialSpectrum.Largest).foreach { spectrum =>
      val portable = portableSolver.decompose(operator, rank, spectrum).toOption.get
      val breeze = BreezeSolvers.partialSymmetricEigen.decompose(operator, rank, spectrum).toOption.get

      assertVectorClose(breeze.values, portable.values, 1e-9)
      assertMatrixClose(projector(breeze.vectors), projector(portable.vectors), 1e-8)
      assert(breeze.residualNorms.toVector.forall(_ < 1e-9))
      assertEquals(breeze.backend, "breeze-dense")
    }
  }

  test("Breeze partial eigensolver consumes declared sparse LinearMap operators") {
    val indices = Array(0, 1, 2, 3)
    val csr = CsrMatrix.fromTriplets(4, 4, indices, indices, Array(8.0, 3.0, 1.0, -1.0)).toOption.get
    val operator = SymmetricOperator.declared(csr).toOption.get
    val result = BreezeSolvers.partialSymmetricEigen.smallestK(operator, DecompositionRank.unsafe(2)).toOption.get

    assertEquals(result.values.toVector, Vector(-1.0, 1.0))
    assert(result.residualNorms.toVector.forall(_ < 1e-10))
  }

  test("Breeze generalized eigensolver matches portable values and residual equation") {
    val a = DoubleMatrix.fromRows(
      Vector(
        Vector(6.0, 1.0),
        Vector(1.0, 4.0)
      )
    )
    val b = DoubleMatrix.fromRows(
      Vector(
        Vector(3.0, 0.5),
        Vector(0.5, 2.0)
      )
    )

    val rank = DecompositionRank.unsafe(2)
    val portable = LinalgSolvers.generalizedEigen.decompose(a, b, rank).toOption.get
    val breeze = BreezeSolvers.generalizedEigen.decompose(a, b, rank).toOption.get

    assertVectorClose(breeze.values, portable.values, 1e-8)
    assertMatrixClose(
      DoubleMatrix.multiply(a, breeze.vectors),
      DoubleMatrix.multiply(DoubleMatrix.multiply(b, breeze.vectors), diagonal(breeze.values)),
      1e-8
    )
  }

  test("Breeze SPD inverse and Cholesky agree with portable linalg") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(4.0, 1.0, 0.5),
        Vector(1.0, 3.0, 0.25),
        Vector(0.5, 0.25, 2.5)
      )
    )

    val portableInverse = LinalgSolvers.spdInverse.invert(matrix).toOption.get
    val breezeInverse = BreezeSolvers.spdInverse.invert(matrix).toOption.get
    val portableLower = Cholesky.decompose(matrix).toOption.get.lowerMatrix
    val breezeLower = BreezeSolvers.choleskyLower(matrix).toOption.get

    assertMatrixClose(breezeInverse, portableInverse, 1e-9)
    assertMatrixClose(DoubleMatrix.multiply(matrix, breezeInverse), DoubleMatrix.eye(3), 1e-9)
    assertMatrixClose(breezeLower, portableLower, 1e-9)
  }

  test("Breeze full-rank least squares agrees with portable QR solution") {
    val design = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 2.0),
        Vector(1.0, 1.0, 1.0),
        Vector(1.0, 2.0, 0.0),
        Vector(1.0, 3.0, 1.0),
        Vector(1.0, 4.0, 3.0)
      )
    )
    val rhs = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(2.0, 1.0),
        Vector(3.0, 0.0),
        Vector(5.0, 1.0),
        Vector(8.0, 2.0)
      )
    )

    val portable = QrDecomposition.decompose(design).solveFullRank(rhs).toOption.get
    val breeze = BreezeSolvers.solveLeastSquaresFullRank(design, rhs).toOption.get

    assertEquals(breeze.rank, portable.rank)
    assertMatrixClose(breeze.coefficients, portable.coefficients, 1e-8)
    assertMatrixClose(breeze.normalizedCovariance, portable.normalizedCovariance, 1e-8)
  }

  test("Breeze adapters preserve linalg failure boundaries") {
    val nonSymmetric = DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(1.0, 1.0)))
    val singular = DoubleMatrix.fromRows(Vector(Vector(1.0, 1.0), Vector(1.0, 1.0)))
    val rankDeficientDesign = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(1.0, 2.0),
        Vector(1.0, 2.0)
      )
    )
    val rhs = DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))

    assert(BreezeSolvers.symmetricEigen.decompose(nonSymmetric).isLeft)
    val operator = SymmetricOperator.fromDense(singular).toOption.get
    assert(BreezeSolvers.partialSymmetricEigen.largestK(operator, DecompositionRank.unsafe(3)).isLeft)
    assert(BreezeSolvers.spdInverse.invert(singular).isLeft)
    assert(BreezeSolvers.choleskyLower(singular).isLeft)
    assert(BreezeSolvers.solveLeastSquaresFullRank(rankDeficientDesign, rhs).isLeft)
    assert(BreezeSolvers.denseSvd.decompose(rankDeficientDesign, DecompositionRank.unsafe(3)).isLeft)
  }

  private def diagonal(values: DoubleVector): DoubleMatrix =
    val out = new Array[Double](values.length * values.length)
    var i = 0
    while i < values.length do
      out(i * values.length + i) = values(i)
      i += 1
    DoubleMatrix.unsafe(values.length, values.length, out)

  private def projector(vectors: DoubleMatrix): DoubleMatrix =
    DoubleMatrix.multiply(vectors, vectors.transpose)

  private def assertVectorClose(actual: DoubleVector, expected: DoubleVector, tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tolerance)
      i += 1

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
