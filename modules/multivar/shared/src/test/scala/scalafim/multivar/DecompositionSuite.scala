package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class DecompositionSuite extends munit.FunSuite:

  private def assertMatrixClose(actual: DoubleMatrix, expected: Vector[Vector[Double]], tol: Double): Unit =
    assertEquals(actual.rows, expected.length)
    assertEquals(actual.cols, expected.headOption.map(_.length).getOrElse(0))
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row)(col), tol)
        col += 1
      row += 1

  private def assertAbsEquals(actual: Double, expected: Double, tol: Double): Unit =
    assertEqualsDouble(Math.abs(actual), Math.abs(expected), tol)

  test("Jacobi symmetric eigensolver recovers a diagonal eigensystem in descending order") {
    val matrix = DoubleMatrix.fromRows(
      Vector(
        Vector(2.0, 0.0),
        Vector(0.0, 5.0)
      )
    )

    val eigen = DenseSolvers.symmetricEigen.decompose(matrix).toOption.get

    assertEqualsDouble(eigen.values(0), 5.0, 1e-10)
    assertEqualsDouble(eigen.values(1), 2.0, 1e-10)
    assertAbsEquals(eigen.vectors(1, 0), 1.0, 1e-10)
    assertAbsEquals(eigen.vectors(0, 1), 1.0, 1e-10)
  }

  test("Jacobi symmetric eigensolver converges on larger dense SPD matrices") {
    for (n, seed) <- Vector((10, 17L), (20, 91L)) do
      val matrix = deterministicSpd(n, seed)
      val eigen = DenseSolvers.symmetricEigen.decompose(matrix) match
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

      val gram = DoubleMatrix.crossProduct(eigen.vectors)
      var p = 0
      while p < n do
        var q = 0
        while q < n do
          assertEqualsDouble(gram(p, q), if p == q then 1.0 else 0.0, 1e-9)
          q += 1
        p += 1
  }

  private def deterministicSpd(n: Int, seed: Long): DoubleMatrix =
    var state = seed
    def next(): Double =
      state = state * 6364136223846793005L + 1442695040888963407L
      ((state >>> 11).toDouble / (1L << 53).toDouble) * 2.0 - 1.0
    val rows = Vector.tabulate(n, n)((_, _) => next())
    val b = DoubleMatrix.fromRows(rows)
    MatrixOps.addRidge(DoubleMatrix.crossProduct(b), 0.5)

  test("Gram SVD reconstructs a rank-one matrix through scores and loadings") {
    val input = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0)
        )
      )
    )

    val svd = DenseSolvers.svd.decompose(input, ComponentCount(1).toOption.get).toOption.get
    val scores = input.rightMultiply(svd.v).toOption.get
    val reconstructed = DoubleMatrix.multiply(scores, svd.v.transpose)

    assertEqualsDouble(svd.singularValues(0), Math.sqrt(28.0), 1e-9)
    assertMatrixClose(reconstructed, input.toDense().toOption.get.toRows, 1e-9)
  }

  test("PCA centers data and returns an inspectable BiProjection artifact") {
    val input = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0)
        )
      )
    )

    val fit = Pca.fit(input, ComponentCount(1).toOption.get).toOption.get
    val scores = fit.project(input).toOption.get

    assertEquals(fit.projection.map.domain.size, 2)
    assertEquals(fit.projection.map.codomain.size, 1)
    assertEquals(fit.projection.diagnostics.map(_.method), Some("pca"))
    assertEqualsDouble(fit.result.singularValues(0), 2.0, 1e-9)
    assertAbsEquals(scores(0, 0), Math.sqrt(2.0), 1e-9)
    assertEqualsDouble(scores(1, 0), 0.0, 1e-9)
    assertAbsEquals(scores(2, 0), Math.sqrt(2.0), 1e-9)
  }

  test("sparse-centered PCA keeps preprocessing lazy and projects through MatrixView algebra") {
    val sparse = SparseMatrixView.fromRows(
      Vector(
        Vector(1.0, 0.0, 2.0),
        Vector(0.0, 3.0, 0.0),
        Vector(4.0, 0.0, 5.0),
        Vector(0.0, 6.0, 0.0)
      )
    ).toOption.get

    val fit = Pca.fit(sparse, ComponentCount(2).toOption.get).toOption.get
    val transformed = fit.projection.map.preprocessor.transform(sparse).toOption.get
    val projected = fit.project(sparse).toOption.get
    val expected = transformed.rightMultiply(fit.result.v).toOption.get

    assertEquals(transformed.storage, StorageKind.LazyAffine)
    assertMatrixClose(projected, expected.toRows, 1e-9)
  }

  test("PLSC returns two maps into a shared latent space from cross-covariance SVD") {
    val x = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(-1.0, 0.0),
          Vector(0.0, -1.0)
        )
      )
    )
    val y = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(2.0),
          Vector(0.0),
          Vector(-2.0),
          Vector(0.0)
        )
      )
    )

    val fit = Plsc.fit(x, y, ComponentCount(1).toOption.get).toOption.get

    assertEquals(fit.projection.latent.size, 1)
    assertEquals(fit.projection.x.codomain, fit.projection.latent)
    assertEquals(fit.projection.y.codomain, fit.projection.latent)
    assertEqualsDouble(fit.result.singularValues(0), 4.0 / 3.0, 1e-9)
    assertAbsEquals(fit.projection.x.weights(0, 0), 1.0, 1e-9)
    assertAbsEquals(fit.projection.y.weights(0, 0), 1.0, 1e-9)
  }

  test("CCA recovers a one-dimensional perfect canonical correlation with ridge regularization") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0))))
    val y = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(2.0), Vector(4.0), Vector(6.0), Vector(8.0))))

    val fit = Cca.fit(x, y, ComponentCount(1).toOption.get, ridge = 1e-10).toOption.get

    assertEquals(fit.projection.latent.size, 1)
    assertEqualsDouble(fit.result.singularValues(0), 1.0, 1e-8)
    assertAbsEquals(fit.projection.xScores(0, 0), fit.projection.yScores(0, 0), 1e-5)
  }

  test("generalized eigensolver solves A v = lambda B v for diagonal SPD B") {
    val a = DoubleMatrix.fromRows(Vector(Vector(4.0, 0.0), Vector(0.0, 9.0)))
    val b = DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 3.0)))

    val result = DenseSolvers.generalizedEigen.decompose(a, b, ComponentCount(2).toOption.get).toOption.get

    assertEqualsDouble(result.values(0), 4.0, 1e-9)
    assertEqualsDouble(result.values(1), 3.0, 1e-9)
  }

