package scalafim.multivar

import gale.linalg.DMat
import gale.linalg.DVec

class SupplementaryProjectionSuite extends munit.FunSuite:

  import ProjectionParityReferenceFixtures as R

  private val trainingWorking: DMat =
    GaleNumerics.matrixFromRows(
      Vector(
        Vector(-1.0, 0.5, 0.2, 1.2),
        Vector(0.4, -0.8, 1.1, -0.3),
        Vector(0.7, 0.6, -0.5, 0.2),
        Vector(-0.2, -0.1, 0.9, -1.0),
        Vector(0.1, -0.2, -1.7, -0.1)
      )
    )

  private val rowIds: Vector[RowId] =
    Vector.tabulate(trainingWorking.rows)(index => RowId.unsafe(s"subject-$index"))

  private val variableIds: Vector[FeatureId] =
    Vector(FeatureId.unsafe("outcome"), FeatureId.unsafe("covariate"))

  private def transform(
      weights: DMat = R.weights,
      spectrum: DVec = DVec.fromSeq(R.singularValues)
  ): FittedFrameTransform =
    val input = MatrixView.dense(trainingWorking)
    val preprocessor = PreprocessSpec.Pass.fit(input).toOption.get
    FittedFrameTransform
      .fromTraining(
        input,
        weights,
        preprocessor,
        "supplementary-fixture",
        ComponentCount.unsafe(weights.cols),
        spectrum = Some(spectrum),
        featureIds = Some(Vector.tabulate(weights.rows)(index => FeatureId.unsafe(s"training-feature-$index"))),
        rowIds = Some(rowIds)
      )
      .toOption
      .get

  private def rowMeasure(value: FittedFrameTransform): RowMeasure[value.trainingRowSpace.Id] =
    RowMeasure
      .fromWeights(
        value.trainingRowSpace.evidence,
        DVec.fromSeq(R.rowWeights),
        ValueIdentity.source(ValueId.unsafe("supplementary-row-measure"))
      )
      .toOption
      .get

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def assertFinite(input: DMat): Unit =
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        assert(input(row, col).isFinite)
        col += 1
      row += 1

  private def weightedCenter(input: DMat, weights: Vector[Double]): DMat =
    val means = Array.ofDim[Double](input.cols)
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        means(col) += weights(row) * input(row, col)
        col += 1
      row += 1
    val values = input.copyData
    row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        values(row * input.cols + col) -= means(col)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def scaleRows(input: DMat, weights: Vector[Double]): DMat =
    val values = input.copyData
    var row = 0
    while row < input.rows do
      var col = 0
      while col < input.cols do
        values(row * input.cols + col) *= weights(row)
        col += 1
      row += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def addRidge(input: DMat, ridge: Double): DMat =
    val values = input.copyData
    var index = 0
    while index < input.rows do
      values(index * input.cols + index) += ridge
      index += 1
    GaleNumerics.matrixFromRowMajor(input.rows, input.cols, values)

  private def rightSolveTwoByTwo(input: DMat, system: DMat): DMat =
    val determinant = system(0, 0) * system(1, 1) - system(0, 1) * system(1, 0)
    val inverse = GaleNumerics.matrixFromRows(
      Vector(
        Vector(system(1, 1) / determinant, -system(0, 1) / determinant),
        Vector(-system(1, 0) / determinant, system(0, 0) / determinant)
      )
    )
    GaleNumerics.multiply(input, inverse)

  test("Euclidean compatibility projection returns a typed frame matching multivarious"):
    val fitted = transform()
    val projector = fitted.supplementary
    val table = projector
      .bind(
        MatrixView.dense(R.supplementaryRaw),
        fitted.trainingRowSchema,
        variableIds,
        ValueId.unsafe("supplementary-r-compat")
      )
      .toOption
      .get
    val result = projector
      .multivarious(table, NullComponentPolicy.reject().toOption.get)
      .toOption
      .get

    assertMatrixClose(result.coefficients, R.supplementaryCompatibility)
    assertMatrixClose(result.frame.weights.toDense.toOption.get, R.supplementaryCompatibility)
    assertEquals(result.table.featureSchema.identities, variableIds)
    assertEquals(result.sourceComponents.indices, Vector(0, 1))
    assertEquals(result.componentSpace.dimension, 2)
    assertEquals(result.projectionProvenance.supplementaryTable, table.valueIdentity)
    assertEquals(result.projectionProvenance.fittedScores, fitted.trainingScores.valueIdentity)

  test("generalized row-measure least squares matches the independent dense fixture"):
    val fitted = transform()
    val projector = fitted.supplementary
    val table = projector
      .bind(
        MatrixView.dense(R.supplementaryRaw),
        fitted.trainingRowSchema,
        variableIds,
        ValueId.unsafe("supplementary-metric")
      )
      .toOption
      .get
    val result = projector
      .metricLeastSquares(
        table,
        rowMeasure(fitted),
        SupplementaryCentering.ArithmeticMean,
        NullComponentPolicy.regularize(R.ridge).toOption.get
      )
      .toOption
      .get

    assertMatrixClose(result.coefficients, R.supplementaryMetricLeastSquares)
    result.convention match
      case SupplementaryConvention.MetricLeastSquares(measure, centering, _) =>
        assertEquals(measure, ValueIdentity.source(ValueId.unsafe("supplementary-row-measure")))
        assertEquals(centering, SupplementaryCentering.ArithmeticMean)
      case other => fail(s"expected metric least-squares convention, got $other")

  test("row-measure centering is explicit and agrees with a separate closed-form oracle"):
    val fitted = transform()
    val projector = fitted.supplementary
    val table = projector
      .bind(
        MatrixView.dense(R.supplementaryRaw),
        fitted.trainingRowSchema,
        variableIds,
        ValueId.unsafe("supplementary-weighted-center")
      )
      .toOption
      .get
    val result = projector
      .metricLeastSquares(
        table,
        rowMeasure(fitted),
        SupplementaryCentering.RowMeasureMean,
        NullComponentPolicy.regularize(R.ridge).toOption.get
      )
      .toOption
      .get
    val centered = weightedCenter(R.supplementaryRaw, R.rowWeights)
    val weightedScores = scaleRows(R.trainingScores, R.rowWeights)
    val cross = GaleNumerics.transposeMultiply(centered, weightedScores)
    val gram = addRidge(GaleNumerics.transposeMultiply(R.trainingScores, weightedScores), R.ridge)
    val expected = rightSolveTwoByTwo(cross, gram)

    assertMatrixClose(result.coefficients, expected)
    assert(Math.abs(result.coefficients(0, 0) - R.supplementaryMetricLeastSquares(0, 0)) > 1e-3)

  test("a consistent named-row permutation is realigned and leaves both conventions invariant"):
    val fitted = transform()
    val projector = fitted.supplementary
    val permutation = Vector(3, 0, 4, 1, 2)
    val permutedValues = MatrixView.dense(R.supplementaryRaw.selectRows(permutation))
    val permutedSchema = RowSchema
      .reordered(fitted.trainingRowSchema, permutation.map(fitted.trainingRowSchema.identities))
      .toOption
      .get
    val table = projector
      .bind(
        permutedValues,
        permutedSchema,
        variableIds,
        ValueId.unsafe("supplementary-permuted")
      )
      .toOption
      .get
    val compatibility = projector
      .multivarious(table, NullComponentPolicy.reject().toOption.get)
      .toOption
      .get
    val metric = projector
      .metricLeastSquares(
        table,
        rowMeasure(fitted),
        SupplementaryCentering.ArithmeticMean,
        NullComponentPolicy.regularize(R.ridge).toOption.get
      )
      .toOption
      .get

    assertMatrixClose(table.values.toDense(StoragePolicy.AllowDense).toOption.get, R.supplementaryRaw)
    assertMatrixClose(compatibility.coefficients, R.supplementaryCompatibility)
    assertMatrixClose(metric.coefficients, R.supplementaryMetricLeastSquares)

  test("row count and foreign row identities fail at the supplementary-table boundary"):
    val fitted = transform()
    val projector = fitted.supplementary
    val foreign = RowSchema
      .from(
        fitted.trainingRowSpace.descriptor,
        fitted.trainingRowSchema.identities,
        ValueIdentity.source(ValueId.unsafe("foreign-row-schema"))
      )
      .toOption
      .get
    val short = MatrixView.dense(R.supplementaryRaw.selectRows(Vector(0, 1, 2, 3)))

    assert(
      projector
        .bind(short, fitted.trainingRowSchema, variableIds, ValueId.unsafe("supplementary-short"))
        .isLeft
    )
    assert(
      projector
        .bind(MatrixView.dense(R.supplementaryRaw), foreign, variableIds, ValueId.unsafe("supplementary-foreign"))
        .isLeft
    )
    assert(
      projector
        .bind(MatrixView.dense(R.supplementaryRaw), variableIds, ValueId.unsafe("supplementary-unstamped"))
        .isLeft
    )

  test("null component policy explicitly rejects, drops, or regularizes"):
    val nullWeights = GaleNumerics.matrixFromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      )
    )
    val fitted = transform(nullWeights, DVec.fromSeq(Vector(1.0, 0.0)))
    val projector = fitted.supplementary
    val table = projector
      .bind(
        MatrixView.dense(R.supplementaryRaw),
        fitted.trainingRowSchema,
        variableIds,
        ValueId.unsafe("supplementary-null")
      )
      .toOption
      .get
    val reject = NullComponentPolicy.reject().toOption.get
    val drop = NullComponentPolicy.drop().toOption.get
    val regularize = NullComponentPolicy.regularize(0.2).toOption.get

    assert(projector.multivarious(table, reject).isLeft)
    val droppedCompatibility = projector.multivarious(table, drop).toOption.get
    val regularizedCompatibility = projector.multivarious(table, regularize).toOption.get
    assertEquals(droppedCompatibility.sourceComponents.indices, Vector(0))
    assertEquals(droppedCompatibility.coefficients.cols, 1)
    assertEquals(regularizedCompatibility.coefficients.cols, 2)
    assertFinite(regularizedCompatibility.coefficients)

    val measure = rowMeasure(fitted)
    assert(
      projector
        .metricLeastSquares(table, measure, SupplementaryCentering.ArithmeticMean, reject)
        .isLeft
    )
    val droppedMetric = projector
      .metricLeastSquares(table, measure, SupplementaryCentering.ArithmeticMean, drop)
      .toOption
      .get
    val regularizedMetric = projector
      .metricLeastSquares(table, measure, SupplementaryCentering.ArithmeticMean, regularize)
      .toOption
      .get
    assertEquals(droppedMetric.sourceComponents.indices, Vector(0))
    assertEquals(regularizedMetric.coefficients.cols, 2)
    assertFinite(regularizedMetric.coefficients)
