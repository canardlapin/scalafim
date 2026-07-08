package scalafim.multivar

import scalafim.linalg.DoubleMatrix

class KernelSuite extends munit.FunSuite:

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

  private def tcross(matrix: DoubleMatrix): DoubleMatrix =
    DoubleMatrix.multiply(matrix, matrix.transpose)

  private def diag(values: scalafim.linalg.DoubleVector): DoubleMatrix =
    MatrixOps.diagonal(values)

  private def explicitNystromKernel(input: DoubleMatrix, landmarks: Vector[Int]): DoubleMatrix =
    val landmarkData = RowGeometryOps.selectRows(input, landmarks)
    val c = DoubleMatrix.multiply(input, landmarkData.transpose)
    val w = RowGeometryOps.selectRows(c, landmarks)
    val eigen = DenseSolvers.symmetricEigen.decompose(w).toOption.get
    val keep = landmarks.length
    val u = MatrixOps.takeColumns(eigen.vectors, keep)
    val lambda = MatrixOps.takeVector(eigen.values, keep)
    val inv = new Array[Double](keep)
    var i = 0
    while i < keep do
      inv(i) = 1.0 / lambda(i)
      i += 1
    val middle = DoubleMatrix.multiply(DoubleMatrix.multiply(u, MatrixOps.diagonal(scalafim.linalg.DoubleVector.unsafe(inv))), u.transpose)
    DoubleMatrix.multiply(DoubleMatrix.multiply(c, middle), c.transpose)

  test("standard Nyström all-landmark linear fit matches exact kernel eigensystem") {
    val input = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(-1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 0.0),
          Vector(0.0, -1.0)
        )
      )
    )

    val fit = Nystrom.fit(
      input,
      ComponentCount(2).toOption.get,
      landmarks = Vector(0, 1, 2, 3),
      preproc = PreprocessSpec.Pass
    ).toOption.get
    val kernel = input.toDense().toOption.get
    val k = DoubleMatrix.multiply(kernel, kernel.transpose)
    val residual = RowGeometryOps.subtract(
      DoubleMatrix.multiply(k, fit.eigen.eigenvectors),
      DoubleMatrix.multiply(fit.eigen.eigenvectors, diag(fit.eigen.eigenvalues))
    )

    assertEquals(fit.method, NystromMethod.Standard)
    assertEquals(fit.eigen.components, 2)
    assertEqualsDouble(fit.eigen.eigenvalues(0), 2.0, 1e-9)
    assertEqualsDouble(fit.eigen.eigenvalues(1), 2.0, 1e-9)
    assertMatrixClose(residual, DoubleMatrix.zeros(4, 2), 1e-8)
    assertMatrixClose(fit.transform(input).toOption.get, fit.eigen.scores, 1e-9)
  }

  test("partial-landmark standard Nyström reconstructs the explicit kernel approximation") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 1.0),
        Vector(0.0, 1.0, 1.0),
        Vector(1.0, 1.0, 0.0),
        Vector(2.0, 0.0, 1.0),
        Vector(0.0, 2.0, 1.0)
      )
    )
    val landmarks = Vector(0, 2, 4)
    val fit = Nystrom.fit(
      MatrixView.dense(x),
      ComponentCount(3).toOption.get,
      landmarks = landmarks,
      preproc = PreprocessSpec.Pass
    ).toOption.get
    val explicit = explicitNystromKernel(x, landmarks)

    assertMatrixClose(tcross(fit.eigen.scores), explicit, 1e-8)
    assertMatrixClose(fit.transform(MatrixView.dense(x)).toOption.get, fit.eigen.scores, 1e-8)
  }

  test("all-landmark standard Nyström matches exact RBF kernel eigenvalues") {
    val x = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(-1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 0.0),
          Vector(0.0, -1.0)
        )
      )
    )
    val kernel = RbfKernel(gamma = 0.3)
    val fit = Nystrom.fit(
      x,
      ComponentCount(3).toOption.get,
      landmarks = Vector(0, 1, 2, 3),
      kernel = kernel,
      preproc = PreprocessSpec.Pass
    ).toOption.get
    val k = kernel.compute(x, x).toOption.get
    val exact = DenseSolvers.symmetricEigen.decompose(k).toOption.get
    val residual = RowGeometryOps.subtract(
      DoubleMatrix.multiply(k, fit.eigen.eigenvectors),
      DoubleMatrix.multiply(fit.eigen.eigenvectors, diag(fit.eigen.eigenvalues))
    )

    assertEqualsDouble(fit.eigen.eigenvalues(0), exact.values(0), 1e-9)
    assertEqualsDouble(fit.eigen.eigenvalues(1), exact.values(1), 1e-9)
    assertEqualsDouble(fit.eigen.eigenvalues(2), exact.values(2), 1e-9)
    assertMatrixClose(residual, DoubleMatrix.zeros(4, 3), 1e-8)
  }

  test("double Nyström with full intermediate rank reconstructs the standard approximation") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 1.0),
        Vector(0.0, 1.0, 1.0),
        Vector(1.0, 1.0, 0.0),
        Vector(2.0, 0.0, 1.0),
        Vector(0.0, 2.0, 1.0)
      )
    )
    val landmarks = Vector(0, 2, 4)
    val fit = Nystrom.fit(
      MatrixView.dense(x),
      ComponentCount(3).toOption.get,
      landmarks = landmarks,
      preproc = PreprocessSpec.Pass,
      method = NystromMethod.DoubleNystrom(ComponentCount(3).toOption.get)
    ).toOption.get
    val explicit = explicitNystromKernel(x, landmarks)

    assertEquals(fit.method.label, "double")
    assertMatrixClose(tcross(fit.eigen.scores), explicit, 1e-8)
    assertMatrixClose(fit.transform(MatrixView.dense(x)).toOption.get, fit.eigen.scores, 1e-8)
  }

  test("out-of-sample projection follows the stored standard Nyström weights") {
    val x = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 1.0),
          Vector(2.0, 1.0)
        )
      )
    )
    val newData = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 0.0))))
    val fit = Nystrom.fit(x, ComponentCount(2).toOption.get, landmarks = Vector(0, 2)).toOption.get
    val kNew = LinearKernel().compute(newData, MatrixView.dense(fit.landmarkData)).toOption.get
    val expected = DoubleMatrix.multiply(kNew, fit.state.scoreWeights)

    assertMatrixClose(fit.transform(newData).toOption.get, expected, 1e-10)
  }

  test("landmarks are canonicalized and rank deficient kernels degrade to estimable rank") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector.fill(5)(Vector(1.0, 1.0))))
    val fit = Nystrom.fit(
      x,
      ComponentCount(3).toOption.get,
      landmarks = Vector(4, 0, 2, 0, 4),
      preproc = PreprocessSpec.Pass
    ).toOption.get

    assertEquals(fit.landmarks.indices, Vector(0, 2, 4))
    assertEquals(fit.eigen.components, 1)
    assert(fit.eigen.standardDeviations(0).isFinite)
  }

  test("non-finite kernel outputs fail before eigendecomposition") {
    val badKernel = new Kernel:
      override def spec: KernelSpec =
        KernelSpec("bad")

      override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DoubleMatrix] =
        Right(DoubleMatrix.fromRows(Vector.fill(left.rows)(Vector.fill(right.rows)(Double.NaN))))

    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))))
    val result = Nystrom.fit(x, ComponentCount(1).toOption.get, landmarks = Vector(0, 1), kernel = badKernel)

    assert(result.swap.toOption.exists {
      case MultivarError.NonFiniteValue(_, _, _) => true
      case _                                     => false
    })
  }

  test("new-sample projection rejects wrong feature counts") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0))))
    val fit = Nystrom.fit(x, ComponentCount(2).toOption.get, landmarks = Vector(0, 1)).toOption.get
    val bad = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0))))

    assert(fit.transform(bad).swap.toOption.exists(_.message.contains("expected 2 columns")))
  }
