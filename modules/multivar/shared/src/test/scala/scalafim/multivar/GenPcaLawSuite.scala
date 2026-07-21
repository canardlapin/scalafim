package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

class GenPcaLawSuite extends munit.FunSuite:

  private val absoluteTolerance = 1e-8
  private val relativeTolerance = 1e-7

  private def k(value: Int): ComponentCount =
    ComponentCount.unsafe(value)

  private def assertMatrixClose(
      actual: DMat,
      expected: DMat,
      absolute: Double = absoluteTolerance,
      relative: Double = relativeTolerance
  ): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    val actualData = actual.copyData
    val expectedData = expected.copyData
    var squaredResidual = 0.0
    var squaredReference = 0.0
    var i = 0
    while i < actualData.length do
      val difference = actualData(i) - expectedData(i)
      squaredResidual += difference * difference
      squaredReference += expectedData(i) * expectedData(i)
      i += 1
    val residual = Math.sqrt(squaredResidual)
    val threshold = absolute + relative * Math.sqrt(squaredReference)
    assert(
      residual <= threshold,
      s"Frobenius residual $residual exceeded absolute-relative threshold $threshold"
    )

  private def assertVectorClose(
      actual: DVec,
      expected: DVec,
      absolute: Double = absoluteTolerance,
      relative: Double = relativeTolerance
  ): Unit =
    assertEquals(actual.length, expected.length)
    var squaredResidual = 0.0
    var squaredReference = 0.0
    var i = 0
    while i < actual.length do
      val difference = actual(i) - expected(i)
      squaredResidual += difference * difference
      squaredReference += expected(i) * expected(i)
      i += 1
    val residual = Math.sqrt(squaredResidual)
    val threshold = absolute + relative * Math.sqrt(squaredReference)
    assert(
      residual <= threshold,
      s"Euclidean residual $residual exceeded absolute-relative threshold $threshold"
    )

  /** Subspace agreement in a metric: all singular values of U1' M U2 are one. */
  private def assertSameMetricSubspace(
      left: DMat,
      right: DMat,
      metric: MetricSpec,
      tolerance: Double = 1e-6
  ): Unit =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    val cross = GaleNumerics.transposeMultiply(left, metric.matvec(right).toOption.get)
    val gram = GaleNumerics.crossProduct(cross)
    val eigen = DenseSolvers.symmetricEigen.decompose(gram).toOption.get
    var i = 0
    while i < eigen.values.length do
      assertEqualsDouble(Math.sqrt(Math.max(eigen.values(i), 0.0)), 1.0, tolerance)
      i += 1

  private def scaleColumns(matrix: DMat, values: DVec): DMat =
    MetricOperator.scaleColumnsDense(matrix, values)

  private def squared(values: DVec): DVec =
    val out = new Array[Double](values.length)
    var i = 0
    while i < values.length do
      out(i) = values(i) * values(i)
      i += 1
    GaleNumerics.vectorFromArray(out)

  private val data = GaleNumerics.matrixFromRows(
    Vector(
      Vector(1.0, 2.0, 0.0),
      Vector(0.0, 1.0, 3.0),
      Vector(2.0, -1.0, 1.0),
      Vector(4.0, 0.0, 2.0),
      Vector(3.0, 2.0, -2.0)
    )
  )

  private val rowSpace =
    MvSpace.of("law.rows", SpaceRole.Samples, data.rows).toOption.get

  private val columnSpace =
    MvSpace.of("law.columns", SpaceRole.Observed, data.cols).toOption.get

  private val rowWeights =
    DVec.fromSeq(Vector(1.0, 2.0, 0.5, 1.5, 0.75))

  private val rowMetric =
    MetricSpec.diagonal(rowWeights, Some(rowSpace)).toOption.get

  private val columnMetricMatrix = GaleNumerics.matrixFromRows(
    Vector(
      Vector(2.0, 0.2, 0.0),
      Vector(0.2, 1.5, 0.1),
      Vector(0.0, 0.1, 1.0)
    )
  )

  private val columnMetric =
    MetricSpec
      .denseSymmetric(
        columnMetricMatrix,
        MetricValidation.StrictPsd(),
        Some(columnSpace)
      )
      .toOption
      .get

  private def diagram(
      table: DMat = data,
      rows: MetricSpec = rowMetric,
      columns: MetricSpec = columnMetric
  ): DualityDiagram =
    DualityDiagram
      .from(
        MatrixView.dense(table),
        rowMetric = Some(rows),
        columnMetric = Some(columns),
        rowSpace = Some(rowSpace),
        columnSpace = Some(columnSpace)
      )
      .toOption
      .get

  private def fit(value: DualityDiagram = diagram()): GenPcaFit =
    GenPca
      .fit(
        value,
        k(3),
        PreprocessSpec.Pass,
        GmdBackend.Eigen(),
        StoragePolicy.AllowDense,
        DenseSolvers.symmetricEigen,
        DenseSolvers.svd
      )
      .toOption
      .get

  test("semantic result names every primal and dual side of the generalized singular system") {
    val fitted = fit()
    val semantic = fitted.semanticResult

    assertEquals(semantic.componentCount, fitted.componentCount)
    assertEquals(semantic.standardRowScores.space, Some(rowSpace))
    assertEquals(semantic.columnAxes.space, columnSpace)
    assertMatrixClose(semantic.standardRowScores.values, fitted.ou)
    assertMatrixClose(semantic.principalRowScores.values, fitted.projection.scores)
    assertMatrixClose(semantic.columnAxes.values, fitted.ov)
    assertMatrixClose(semantic.columnMetricLoadings.values, fitted.v)
    assertMatrixClose(semantic.rowMetricLoadings.values, fitted.u)
    assertMatrixClose(semantic.rowDualPrincipalScores.values, fitted.metricScores)
    assertVectorClose(semantic.spectrum.singularValues, fitted.d)
    assertVectorClose(semantic.spectrum.generalizedEigenvalues, squared(fitted.d))
  }

  test("dual transport and metric orthonormality laws return explicit residual diagnostics") {
    val fitted = fit()
    val tolerance = NumericalLawTolerance.from(1e-9, 1e-8).toOption.get
    val diagnostics = GenPcaLaws
      .evaluate(MatrixView.dense(data), fitted, tolerance)
      .toOption
      .get

    assert(diagnostics.satisfied)
    assertEquals(diagnostics.residuals.map(_.norm).distinct, Vector(NumericalResidualNorm.Frobenius))
    diagnostics.residuals.foreach { value =>
      assert(value.residual.isFinite)
      assert(value.referenceScale.isFinite)
      assert(value.residual <= value.threshold, s"${value.law}: ${value.residual} > ${value.threshold}")
    }
    assert(NumericalLawTolerance.from(-1.0, 0.0).isLeft)
    assert(NumericalLawTolerance.from(0.0, Double.NaN).isLeft)
  }

  test("row and column operators share the fitted nonzero spectrum through transported axes") {
    val source = diagram()
    val fitted = fit(source)
    val eigenvalues = fitted.semanticResult.spectrum.generalizedEigenvalues
    val rowApplied = GaleNumerics.multiply(source.rowOperator().toOption.get, fitted.ou)
    val columnApplied = GaleNumerics.multiply(source.columnOperator().toOption.get, fitted.ov)

    assertMatrixClose(rowApplied, scaleColumns(fitted.ou, eigenvalues))
    assertMatrixClose(columnApplied, scaleColumns(fitted.ov, eigenvalues))
  }

  test("rank-k reconstruction minimizes the SPD weighted loss and discarded spectrum gives the error") {
    val fitted = fit()
    val rankTwo = fitted.reconstruct(Some(k(2))).toOption.get
    val full = fitted.reconstruct().toOption.get
    val optimalError = GenPcaLaws
      .weightedSquaredError(data, rankTwo, rowMetric, columnMetric)
      .toOption
      .get
    val discarded = fitted.d(2) * fitted.d(2)
    val competitor = GaleNumerics.matrixFromRows(data.toRows.map(row => row.updated(2, 0.0)))
    val competitorError = GenPcaLaws
      .weightedSquaredError(data, competitor, rowMetric, columnMetric)
      .toOption
      .get
    val fullError = GenPcaLaws
      .weightedSquaredError(data, full, rowMetric, columnMetric)
      .toOption
      .get

    assertEqualsDouble(optimalError, discarded, 1e-7 * (1.0 + discarded))
    assert(optimalError <= competitorError + 1e-9)
    assertEqualsDouble(fullError, 0.0, 1e-10)
    assert(
      GenPcaLaws
        .weightedSquaredError(data, DMat.zeros(2, 2), rowMetric, columnMetric)
        .isLeft
    )
  }

  test("basis changes preserve eigenvalues and map row and column subspaces covariantly") {
    val original = fit()
    val rowBasis = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.15, 0.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, -0.2, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0, 1.0)
      )
    )
    val rowBasisInverse = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, -0.15, 0.0, 0.0, 0.0),
        Vector(0.0, 1.0, 0.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.2, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 0.0, 1.0)
      )
    )
    val columnBasis = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.2, 0.0),
        Vector(0.0, 1.0, -0.1),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val columnBasisInverse = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, -0.2, -0.02),
        Vector(0.0, 1.0, 0.1),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val transformedData = GaleNumerics.multiply(
      GaleNumerics.multiply(rowBasisInverse, data),
      columnBasisInverse.transpose
    )
    val transformedRowMetricMatrix = GaleNumerics.multiply(
      GaleNumerics.multiply(rowBasis.transpose, rowMetric.toDense().toOption.get),
      rowBasis
    )
    val transformedColumnMetricMatrix = GaleNumerics.multiply(
      GaleNumerics.multiply(columnBasis.transpose, columnMetricMatrix),
      columnBasis
    )
    val transformedRowMetric = MetricSpec
      .denseSymmetric(
        transformedRowMetricMatrix,
        MetricValidation.StrictPsd(),
        Some(rowSpace)
      )
      .toOption
      .get
    val transformedColumnMetric = MetricSpec
      .denseSymmetric(
        transformedColumnMetricMatrix,
        MetricValidation.StrictPsd(),
        Some(columnSpace)
      )
      .toOption
      .get
    val transformed = fit(diagram(transformedData, transformedRowMetric, transformedColumnMetric))
    val expectedRows = GaleNumerics.multiply(rowBasisInverse, original.ou)
    val expectedColumns = GaleNumerics.multiply(columnBasisInverse, original.ov)

    assertVectorClose(transformed.d, original.d)
    assertSameMetricSubspace(expectedRows, transformed.ou, transformedRowMetric)
    assertSameMetricSubspace(expectedColumns, transformed.ov, transformedColumnMetric)
  }

  test("transpose is involutive and GenPCA exchanges the row and column singular systems") {
    val source = diagram()
    val twice = source.transpose().toOption.get.transpose().toOption.get
    val original = fit(source)
    val transposedDiagram = source.transpose().toOption.get
    val transposed = fit(transposedDiagram)

    assertEquals(twice.rowSpace, source.rowSpace)
    assertEquals(twice.columnSpace, source.columnSpace)
    assertMatrixClose(twice.table.toDense().toOption.get, data)
    assertVectorClose(transposed.d, original.d)
    assertSameMetricSubspace(original.ou, transposed.ov, rowMetric)
    assertSameMetricSubspace(original.ov, transposed.ou, columnMetric)
  }

  test("repeated eigenvalues are identified as a subspace cluster") {
    val repeated = GaleNumerics.matrixFromRows(
      Vector(
        Vector(3.0, 0.0, 0.0),
        Vector(0.0, 3.0, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val rotation = GaleNumerics.matrixFromRows(
      Vector(
        Vector(Math.sqrt(0.5), -Math.sqrt(0.5), 0.0),
        Vector(Math.sqrt(0.5), Math.sqrt(0.5), 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val first = Unsafe
      .genPcaFromArrays(
        MatrixView.dense(repeated),
        k(3),
        reason = "repeated-eigenvalue law fixture",
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    val rotated = Unsafe
      .genPcaFromArrays(
        MatrixView.dense(GaleNumerics.multiply(repeated, rotation)),
        k(3),
        reason = "rotated repeated-eigenvalue law fixture",
        preproc = PreprocessSpec.Pass,
        backend = GmdBackend.Eigen()
      )
      .toOption
      .get
    val clusters = first.semanticResult.spectrum.clusters()
    val identity = MetricSpec.identity(3).toOption.get

    assertEquals(clusters.map(_.componentCount), Vector(2, 1))
    assert(!clusters.head.individuallyIdentifiable)
    assert(clusters(1).individuallyIdentifiable)
    assertSameMetricSubspace(
      MatrixOps.takeColumns(first.ou, 2),
      MatrixOps.takeColumns(rotated.ou, 2),
      identity
    )
    assert(SpectralClusteringTolerance.from(-1.0, 0.0).isLeft)
    assert(SpectralClusteringTolerance.from(0.0, Double.PositiveInfinity).isLeft)
  }
