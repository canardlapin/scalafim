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

  test("double Nyström with truncated intermediate rank matches an explicit truncated reference") {
    val x = DoubleMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, 0.5),
        Vector(-1.0, 1.0, 1.5),
        Vector(2.0, -1.0, 1.0),
        Vector(0.5, 0.5, -2.0),
        Vector(-1.5, 2.0, 0.5),
        Vector(1.0, 1.0, 1.0)
      )
    )
    val landmarks = Vector(0, 1, 3, 5)
    val intermediateRank = 2
    val components = 2
    val fit = Nystrom.fit(
      MatrixView.dense(x),
      ComponentCount(components).toOption.get,
      landmarks = landmarks,
      preproc = PreprocessSpec.Pass,
      method = NystromMethod.DoubleNystrom(ComponentCount(intermediateRank).toOption.get)
    ).toOption.get

    // Independent reference: explicit truncated first stage, then the second-stage
    // eigensystem of W'W, using dense linear algebra only.
    val landmarkData = RowGeometryOps.selectRows(x, landmarks)
    val c = DoubleMatrix.multiply(x, landmarkData.transpose)
    val kMm = RowGeometryOps.selectRows(c, landmarks)
    val first = DenseSolvers.symmetricEigen.decompose(kMm).toOption.get
    val vSL = MatrixOps.takeColumns(first.vectors, intermediateRank)
    val lambdaL = MatrixOps.takeVector(first.values, intermediateRank)
    val invSqrtL = new Array[Double](intermediateRank)
    var i = 0
    while i < intermediateRank do
      invSqrtL(i) = 1.0 / Math.sqrt(lambdaL(i))
      i += 1
    val firstWeights = DoubleMatrix.multiply(vSL, MatrixOps.diagonal(scalafim.linalg.DoubleVector.unsafe(invSqrtL)))
    val w = DoubleMatrix.multiply(c, firstWeights)
    val second = DenseSolvers.symmetricEigen.decompose(DoubleMatrix.crossProduct(w)).toOption.get
    val lambdaK = MatrixOps.takeVector(second.values, components)
    val vK = MatrixOps.takeColumns(second.vectors, components)
    val sqrtInvK = new Array[Double](components)
    i = 0
    while i < components do
      sqrtInvK(i) = 1.0 / Math.sqrt(lambdaK(i))
      i += 1
    val eigenWeights = DoubleMatrix.multiply(
      firstWeights,
      DoubleMatrix.multiply(vK, MatrixOps.diagonal(scalafim.linalg.DoubleVector.unsafe(sqrtInvK)))
    )
    val sqrtK = new Array[Double](components)
    i = 0
    while i < components do
      sqrtK(i) = Math.sqrt(lambdaK(i))
      i += 1
    val expectedScores = DoubleMatrix.multiply(
      DoubleMatrix.multiply(c, eigenWeights),
      MatrixOps.diagonal(scalafim.linalg.DoubleVector.unsafe(sqrtK))
    )

    fit.state match
      case state: DoubleNystromState =>
        assertEquals(state.firstStageEigenvectors.cols, intermediateRank, "first stage must be genuinely truncated")
      case other =>
        fail(s"expected a double Nyström state, got $other")
    assertEquals(fit.eigen.components, components)
    i = 0
    while i < components do
      assertEqualsDouble(fit.eigen.eigenvalues(i), lambdaK(i), 1e-9)
      i += 1
    assertMatrixClose(tcross(fit.eigen.scores), tcross(expectedScores), 1e-8)
    assertMatrixClose(fit.transform(MatrixView.dense(x)).toOption.get, fit.eigen.scores, 1e-8)
  }

  test("Nyström rejects component and intermediate-rank requests beyond the landmark count") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0))))

    Nystrom.fit(x, ComponentCount(3).toOption.get, landmarks = Vector(0, 1)) match
      case Left(MultivarError.InvalidComponentRequest(requested, limit)) =>
        assertEquals(requested, 3)
        assertEquals(limit, 2)
      case other =>
        fail(s"expected component request rejection, got $other")

    Nystrom.fit(
      x,
      ComponentCount(2).toOption.get,
      landmarks = Vector(0, 1),
      method = NystromMethod.DoubleNystrom(ComponentCount(3).toOption.get)
    ) match
      case Left(MultivarError.InvalidComponentRequest(requested, limit)) =>
        assertEquals(requested, 3)
        assertEquals(limit, 2)
      case other =>
        fail(s"expected intermediate rank rejection, got $other")
  }

  test("Nyström rejects out-of-bounds landmark indices") {
    val x = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0))))

    Nystrom.fit(x, ComponentCount(1).toOption.get, landmarks = Vector(0, 7)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Row, 7, 3)) => ()
      case other => fail(s"expected landmark bounds rejection, got $other")

    Nystrom.fit(x, ComponentCount(1).toOption.get, landmarks = Vector(-1)) match
      case Left(MultivarError.IndexOutOfBounds(IndexAxis.Row, -1, 3)) => ()
      case other => fail(s"expected negative landmark rejection, got $other")
  }

  test("linear kernel rejects mismatched feature counts") {
    val left = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0))))
    val right = MatrixView.dense(DoubleMatrix.fromRows(Vector(Vector(1.0, 2.0, 3.0))))

    LinearKernel().compute(left, right) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("feature counts"), detail)
      case other =>
        fail(s"expected linear kernel feature mismatch, got $other")
  }

  test("RBF kernels require a finite positive gamma") {
    intercept[IllegalArgumentException](RbfKernel(0.0))
    intercept[IllegalArgumentException](RbfKernel(-1.0))
    intercept[IllegalArgumentException](RbfKernel(Double.NaN))
    intercept[IllegalArgumentException](RbfKernel(Double.PositiveInfinity))
  }

  test("a user kernel with roundoff asymmetry on the landmark block still fits") {
    val asymmetric = new Kernel:
      override def spec: KernelSpec =
        KernelSpec("asymmetric-linear")

      override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DoubleMatrix] =
        LinearKernel().compute(left, right).map { out =>
          if out.rows == out.cols && out.cols > 1 then
            val data = out.copyData
            data(1) += 1e-8
            DoubleMatrix.unsafe(out.rows, out.cols, data)
          else out
        }

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

    val standard = Nystrom.fit(x, ComponentCount(2).toOption.get, landmarks = Vector(0, 2, 3), kernel = asymmetric)
    assert(standard.isRight, s"expected a slightly asymmetric kernel to fit, got $standard")
    assert(standard.toOption.get.eigen.eigenvalues(0).isFinite)

    val double = Nystrom.fit(
      x,
      ComponentCount(2).toOption.get,
      landmarks = Vector(0, 2, 3),
      kernel = asymmetric,
      method = NystromMethod.DoubleNystrom(ComponentCount(2).toOption.get)
    )
    assert(double.isRight, s"expected a slightly asymmetric kernel to double-fit, got $double")
  }

  test("centering diagnostics are derived from the fitted preprocessor") {
    val x = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 1.0),
          Vector(2.0, 3.0)
        )
      )
    )

    val raw = Nystrom.fit(x, ComponentCount(1).toOption.get, landmarks = Vector(0, 2), preproc = PreprocessSpec.Pass).toOption.get
    assertEquals(raw.centering, KernelCentering.Uncentered)
    assertEquals(raw.diagnostics.centering, KernelCentering.Uncentered)

    val centered = Nystrom.fit(x, ComponentCount(1).toOption.get, landmarks = Vector(0, 2), preproc = PreprocessSpec.Center).toOption.get
    assertEquals(centered.centering, KernelCentering.InputPreprocessed)
    assertEquals(centered.diagnostics.centering, KernelCentering.InputPreprocessed)
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

  private final class CountingKernel extends Kernel:
    var invocations = 0

    override def spec: KernelSpec =
      KernelSpec("counting-linear")

    override def compute(left: MatrixView, right: MatrixView): Either[MultivarError, DoubleMatrix] =
      invocations += 1
      LinearKernel().compute(left, right)

  test("standard Nyström computes the landmark and all-landmark kernels exactly once each") {
    val x = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0, 1.0),
          Vector(0.0, 1.0, 1.0),
          Vector(1.0, 1.0, 0.0),
          Vector(2.0, 0.0, 1.0),
          Vector(0.0, 2.0, 1.0)
        )
      )
    )
    val kernel = new CountingKernel

    val fit = Nystrom.fit(x, ComponentCount(2).toOption.get, landmarks = Vector(0, 2, 4), kernel = kernel)

    assert(fit.isRight, s"expected counting-kernel fit to succeed, got $fit")
    assertEquals(kernel.invocations, 2)
  }

  test("double Nyström computes the n x m kernel once, not once per stage") {
    val x = MatrixView.dense(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0, 1.0),
          Vector(0.0, 1.0, 1.0),
          Vector(1.0, 1.0, 0.0),
          Vector(2.0, 0.0, 1.0),
          Vector(0.0, 2.0, 1.0)
        )
      )
    )
    val kernel = new CountingKernel

    val fit = Nystrom.fit(
      x,
      ComponentCount(2).toOption.get,
      landmarks = Vector(0, 2, 4),
      kernel = kernel,
      method = NystromMethod.DoubleNystrom(ComponentCount(2).toOption.get)
    )

    assert(fit.isRight, s"expected counting-kernel double fit to succeed, got $fit")
    assertEquals(kernel.invocations, 2)
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
