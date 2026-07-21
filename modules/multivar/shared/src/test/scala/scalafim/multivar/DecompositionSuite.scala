package scalafim.multivar

import gale.linalg.DMat

class DecompositionSuite extends munit.FunSuite:

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

  private def assertAbsEquals(actual: Double, expected: Double, tol: Double): Unit =
    assertEqualsDouble(Math.abs(actual), Math.abs(expected), tol)

  // The Jacobi eigensolver's own contract (ordering, orthonormality, trace) is
  // covered in linalg's DecompositionSuite, next to the solver.

  test("Gram SVD reconstructs a rank-one matrix through scores and loadings") {
    val input = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0)
        )
      )
    )

    val svd = DenseSolvers.svd.decompose(input, ComponentCount(1).toOption.get).toOption.get
    val scores = input.rightMultiply(svd.v).toOption.get
    val reconstructed = GaleNumerics.multiply(scores, svd.v.transpose)

    assertEqualsDouble(svd.singularValues(0), Math.sqrt(28.0), 1e-9)
    assertMatrixClose(reconstructed, input.toDense().toOption.get.toRows, 1e-9)
  }

  test("PCA centers data and returns an inspectable typed frame transform") {
    val input = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0)
        )
      )
    )

    val fit = Pca.fit(input, ComponentCount(1).toOption.get).toOption.get
    val scores = fit.project(input).toOption.get

    assertEquals(fit.transform.featureSpace.descriptor.size, 2)
    assertEquals(fit.transform.componentSpace.descriptor.size, 1)
    assertEquals(fit.transform.featureSpace.descriptor.id.value, "pca.features")
    assertEquals(fit.transform.componentSpace.descriptor.id.value, "pca.components")
    assertEquals(fit.transform.diagnostics.method, "pca")
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
    val transformed = fit.transform.preprocessor.transform(sparse).toOption.get
    val projected = fit.project(sparse).toOption.get
    val expected = transformed.rightMultiply(fit.result.v).toOption.get

    assertEquals(transformed.storage, StorageKind.LazyAffine)
    assertMatrixClose(projected, expected.toRows, 1e-9)
  }

  test("PLSC returns two typed frames into compatible component spaces") {
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(-1.0, 0.0),
          Vector(0.0, -1.0)
        )
      )
    )
    val y = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(2.0),
          Vector(0.0),
          Vector(-2.0),
          Vector(0.0)
        )
      )
    )

    val fit = Plsc.fit(x, y, ComponentCount(1).toOption.get).toOption.get

    assertEquals(fit.sourceTransform.componentSpace.descriptor.size, 1)
    assertEquals(
      fit.sourceTransform.componentSpace.descriptor.size,
      fit.targetTransform.componentSpace.descriptor.size
    )
    assertEquals(fit.operator.diagnostics.kind, PairedProgramKind.Plsc)
    assertEquals(fit.operator.result.singularValues, fit.result.singularValues)
    assertEqualsDouble(fit.result.singularValues(0), 4.0 / 3.0, 1e-9)
    assertAbsEquals(fit.operator.sourceWeights.toOption.get(0, 0), 1.0, 1e-9)
    assertAbsEquals(fit.operator.targetWeights.toOption.get(0, 0), 1.0, 1e-9)
  }

  test("CCA recovers a one-dimensional perfect canonical correlation with ridge regularization") {
    val x = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0))))
    val y = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(2.0), Vector(4.0), Vector(6.0), Vector(8.0))))

    val fit = Cca.fit(x, y, ComponentCount(1).toOption.get, ridge = 1e-10).toOption.get

    assertEquals(fit.sourceTransform.componentSpace.descriptor.size, 1)
    fit.operator.diagnostics.kind match
      case PairedProgramKind.Cca(_) =>
      case other =>
        fail(s"expected CCA method, got $other")
    assertEqualsDouble(fit.result.singularValues(0), 1.0, 1e-8)
    assertEquals(fit.operator.result.singularValues, fit.result.singularValues)
    assertAbsEquals(fit.xScores(0, 0), fit.yScores(0, 0), 1e-5)
  }

  test("CCA exposes typed asymmetric regularization and rejects invalid raw ridge") {
    val x = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0))))
    val y = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.5), Vector(3.0), Vector(4.5), Vector(6.0))))
    val regularization = CcaRegularization.asymmetric(1e-4, 1e-3).toOption.get

    val fit = Cca.fitRegularized(x, y, ComponentCount(1).toOption.get, regularization).toOption.get

    fit.operator.diagnostics.kind match
      case PairedProgramKind.Cca(value) =>
        assertEqualsDouble(value.x.value, 1e-4, 0.0)
        assertEqualsDouble(value.y.value, 1e-3, 0.0)
      case other =>
        fail(s"expected CCA method, got $other")
    assert(fit.result.singularValues(0).isFinite)

    Cca.fit(x, y, ComponentCount(1).toOption.get, ridge = -1.0) match
      case Left(MultivarError.InvalidTolerance(kind, value)) =>
        assertEquals(kind, "ridge")
        assertEqualsDouble(value, -1.0, 0.0)
      case other =>
        fail(s"expected typed ridge rejection, got $other")
  }

  test("RRR recovers an exact rank-one directed prediction") {
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 1.0),
          Vector(2.0, -1.0)
        )
      )
    )
    val y = GaleNumerics.matrixFromRows(
      Vector(
        Vector(2.0, 4.0),
        Vector(-1.0, -2.0),
        Vector(1.0, 2.0),
        Vector(5.0, 10.0)
      )
    )

    val fit = ReducedRankRegression
      .fit(x, MatrixView.dense(y), ComponentCount(1).toOption.get, xPreproc = PreprocessSpec.Pass, yPreproc = PreprocessSpec.Pass)
      .toOption
      .get

    fit.operator.diagnostics.kind match
      case PairedProgramKind.ReducedRankRegression(RegressionRegularization.Ols) =>
      case other =>
        fail(s"expected XToY OLS RRR method, got $other")
    assertEquals(fit.coefficientTransform.sourceFeatureSpace.descriptor.size, 2)
    assertEquals(fit.coefficientTransform.targetFeatureSpace.descriptor.size, 2)
    assertEquals(fit.operator.sourceWeights.toOption.get.rows, 2)
    assertEquals(fit.operator.targetWeights.toOption.get.rows, 2)
    assertEquals(fit.operator.result.singularValues.length, 1)
    assertMatrixClose(fit.predictWorking(x).toOption.get, y.toRows, 1e-8)
    assertMatrixClose(fit.predict(x).toOption.get, y.toRows, 1e-8)
  }

  test("full-rank RRR equals the OLS coefficient map") {
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(1.0, 1.0),
          Vector(2.0, -1.0)
        )
      )
    )
    val y = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.5),
        Vector(-2.0, 3.0),
        Vector(-1.0, 3.5),
        Vector(4.0, -2.0)
      )
    )
    val expectedCoefficient = Vector(Vector(1.0, 0.5), Vector(-2.0, 3.0))

    val fit = ReducedRankRegression
      .fit(x, MatrixView.dense(y), ComponentCount(2).toOption.get, xPreproc = PreprocessSpec.Pass, yPreproc = PreprocessSpec.Pass)
      .toOption
      .get

    assertMatrixClose(fit.fullCoefficient, expectedCoefficient, 1e-8)
    assertMatrixClose(fit.coefficientTransform.coefficient.toDense.toOption.get, expectedCoefficient, 1e-8)
    assertMatrixClose(fit.predict(x).toOption.get, y.toRows, 1e-8)
  }

  test("RRR validates rank and row alignment before fitting") {
    val x = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))))
    val y = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0, 2.0), Vector(2.0, 4.0), Vector(3.0, 6.0))))
    val shortY = MatrixView.dense(GaleNumerics.matrixFromRows(Vector(Vector(1.0), Vector(2.0))))

    ReducedRankRegression.fit(x, y, ComponentCount(2).toOption.get) match
      case Left(MultivarError.InvalidComponentRequest(requested, limit)) =>
        assertEquals(requested, 2)
        assertEquals(limit, 1)
      case other =>
        fail(s"expected RRR rank rejection, got $other")

    ReducedRankRegression.fit(x, shortY, ComponentCount(1).toOption.get) match
      case Left(MultivarError.MatrixShapeMismatch(detail)) =>
        assert(detail.contains("equal rows"), detail)
      case other =>
        fail(s"expected RRR row mismatch, got $other")
  }

  test("ridge RRR matches the closed-form covariance-scaled ridge solution at full rank") {
    val xData = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.5),
        Vector(-1.0, 2.0),
        Vector(2.0, -1.0),
        Vector(0.5, 1.5),
        Vector(-2.0, -0.5),
        Vector(1.5, 1.0)
      )
    )
    val yData = GaleNumerics.matrixFromRows(
      Vector(
        Vector(2.0, 1.0),
        Vector(1.0, -1.0),
        Vector(0.0, 3.0),
        Vector(3.0, 2.0),
        Vector(-1.0, 0.5),
        Vector(2.5, -0.5)
      )
    )
    val lambda = 0.3
    val n = xData.rows
    val regularization = RegressionRegularization.ridge(lambda).toOption.get

    val fit = ReducedRankRegression
      .fit(
        MatrixView.dense(xData),
        MatrixView.dense(yData),
        ComponentCount(2).toOption.get,
        regularization = regularization,
        xPreproc = PreprocessSpec.Pass,
        yPreproc = PreprocessSpec.Pass
      )
      .toOption
      .get

    // Independent closed form per the documented covariance-scale convention:
    // B = (X'X/(n-1) + lambda I)^-1 (X'Y/(n-1)), built here from dense products and
    // an eigendecomposition-based inverse, not the production metric path.
    def inverseSpd(matrix: DMat): DMat =
      val eigen = DenseSolvers.symmetricEigen.decompose(matrix).toOption.get
      val k = eigen.values.length
      val scaled = eigen.vectors.copyData
      var col = 0
      while col < k do
        val inv = 1.0 / eigen.values(col)
        var row = 0
        while row < k do
          scaled(row * k + col) *= inv
          row += 1
        col += 1
      GaleNumerics.multiply(GaleNumerics.matrixFromRowMajor(k, k, scaled), eigen.vectors.transpose)

    val denom = 1.0 / (n - 1)
    val cxx = MatrixOps.scale(GaleNumerics.crossProduct(xData), denom)
    val cxy = MatrixOps.scale(GaleNumerics.transposeMultiply(xData, yData), denom)
    val expected = GaleNumerics.multiply(inverseSpd(MatrixOps.addRidge(cxx, lambda)), cxy)
    val expectedPrediction = GaleNumerics.multiply(xData, expected)

    assertMatrixClose(fit.fullCoefficient, expected.toRows, 1e-8)
    assertMatrixClose(fit.coefficientTransform.coefficient.toDense.toOption.get, expected.toRows, 1e-8)
    assertMatrixClose(fit.predictWorking(MatrixView.dense(xData)).toOption.get, expectedPrediction.toRows, 1e-8)
  }

  test("RRR predict restores the response preprocessor to original scale") {
    val x = MatrixView.dense(
      GaleNumerics.matrixFromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(-1.0, 0.0),
          Vector(0.0, 1.0),
          Vector(0.0, -1.0)
        )
      )
    )
    val y = GaleNumerics.matrixFromRows(
      Vector(
        Vector(12.0, 24.0),
        Vector(8.0, 16.0),
        Vector(9.0, 18.0),
        Vector(11.0, 22.0)
      )
    )

    val fit = ReducedRankRegression.fit(x, MatrixView.dense(y), ComponentCount(1).toOption.get).toOption.get

    assertMatrixClose(fit.predict(x).toOption.get, y.toRows, 1e-8)
  }

  test("generalized eigensolver solves A v = lambda B v for diagonal SPD B") {
    val a = GaleNumerics.matrixFromRows(Vector(Vector(4.0, 0.0), Vector(0.0, 9.0)))
    val b = GaleNumerics.matrixFromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 3.0)))

    val result = DenseSolvers.generalizedEigen.decompose(a, b, ComponentCount.unsafe(2)).toOption.get

    assertEqualsDouble(result.values(0), 4.0, 1e-9)
    assertEqualsDouble(result.values(1), 3.0, 1e-9)
  }
