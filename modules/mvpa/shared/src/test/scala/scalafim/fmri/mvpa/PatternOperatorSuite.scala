package scalafim.fmri.mvpa

import gale.linalg.{DMat, LinearOperator, Matrix}

class PatternOperatorSuite extends munit.FunSuite:

  test("dense pattern operator preserves the matrix and satisfies the adjoint law") {
    val values = Vector(
      Vector(1.0, 2.0, -1.0),
      Vector(0.5, -2.0, 3.0),
      Vector(4.0, 1.5, 0.0),
      Vector(-1.0, 2.5, 1.0)
    )
    val base = patternMatrix(values)
    val patterns = PatternMatrix(
      base.value,
      Vector(4, 7, 8, 10).map(SampleIndex.apply),
      base.featureIndices
    )
    val operator = PatternOperator.fromMatrix(patterns).toOption.get
    val weights = GaleTestMatrix.fromRows(Vector(Vector(0.5, -1.0), Vector(2.0, 0.25), Vector(-0.75, 1.5)))
    val sampleProbe = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, -0.5),
        Vector(0.25, 2.0),
        Vector(-1.5, 0.75),
        Vector(0.4, -1.0)
      )
    )

    assertMatrixClose(operator.materialize.toOption.get.value, patterns.value)
    assertEquals(operator.materialize.toOption.get.sampleIndices.map(_.value), Vector(4, 7, 8, 10))
    val forward = operator.applyTo(weights).toOption.get
    val adjoint = operator.transposeApplyTo(sampleProbe).toOption.get
    assertMatrixClose(forward, multiply(patterns.value, weights))
    assertMatrixClose(adjoint, multiply(patterns.value.t, sampleProbe))
    assertEqualsDouble(frobeniusDot(forward, sampleProbe), frobeniusDot(weights, adjoint), 1e-12)
  }

  test("row and feature restrictions match an explicit dense reference") {
    val canonical = patternMatrix(
      Vector(
        Vector(1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0),
        Vector(7.0, 8.0, 9.0),
        Vector(10.0, 11.0, 12.0)
      ),
      featureIds = Vector(10, 20, 30)
    )
    val patterns = canonical.copy(
      sampleIndices = Vector(100, 101, 102, 103).map(SampleIndex.apply)
    )
    val operator = PatternOperator.fromMatrix(patterns).toOption.get
    val featureSet = FeatureSet.unsafe(RoiId(4), Vector(30, 10))
    val restricted = operator
      .selectFeatures(featureSet)
      .flatMap(_.selectRows(Vector(SampleIndex(2), SampleIndex(0))))
      .toOption
      .get
    val expected = GaleTestMatrix.fromRows(Vector(Vector(9.0, 7.0), Vector(3.0, 1.0)))

    assertEquals(restricted.featureIndices.map(_.value), Vector(30, 10))
    assertEquals(restricted.sampleIndices.map(_.value), Vector(102, 100))
    assertEquals(restricted.samples, 2)
    assertEquals(restricted.provenance.featureSelections, 1)
    assertEquals(restricted.provenance.rowSelections, 1)
    assertMatrixClose(restricted.materialize.toOption.get.value, expected)
  }

  test("row stacking and composed operators match explicit block matrices") {
    val first = patternMatrix(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val second = patternMatrix(Vector(Vector(-1.0, 0.5), Vector(2.5, -3.0), Vector(0.0, 1.0)))
    val stacked = PatternOperator
      .stackRows(
        Vector(
          PatternOperator.fromMatrix(first).toOption.get,
          PatternOperator.fromMatrix(second).toOption.get
        )
      )
      .toOption
      .get
    val expectedStack = GaleTestMatrix.fromRows(first.value.toRows ++ second.value.toRows)

    assertEquals(stacked.samples, 5)
    assertEquals(stacked.provenance.stackedParts, 2)
    assertMatrixClose(stacked.materialize.toOption.get.value, expectedStack)

    val readout = GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.0, 0.5), Vector(0.0, -1.0, 1.0)))
    val timeSeries = GaleTestMatrix.fromRows(
      Vector(
        Vector(2.0, 1.0),
        Vector(-1.0, 3.0),
        Vector(4.0, 0.5)
      )
    )
    val composedLinear = readout.compose(timeSeries).toOption.get
    val composed = PatternOperator
      .fromOperator(
        SampleAxis.unsafe(2),
        Vector(FeatureIndex(0), FeatureIndex(1)),
        composedLinear,
        PatternOperatorProvenance.composed
      )
      .toOption
      .get

    assertMatrixClose(composed.materialize.toOption.get.value, multiply(readout, timeSeries))
  }

  test("generated operators preserve adjoints and feature-permutation laws") {
    var samples = 2
    while samples <= 6 do
      var features = 2
      while features <= 5 do
        val values = Matrix.tabulate(samples, features): (row, col) =>
          (row + 1) * 0.7 - (col + 1) * 0.4 + row * col * 0.05
        val patterns = PatternMatrix(
          value = values,
          sampleIndices = Vector.tabulate(samples)(SampleIndex.apply),
          featureIndices = Vector.tabulate(features)(FeatureIndex.apply)
        )
        val operator = PatternOperator.fromMatrix(patterns).toOption.get
        val weights = Matrix.tabulate(features, 3): (row, col) =>
          (row + 1) * (col + 2) * 0.13
        val probe = Matrix.tabulate(samples, 3): (row, col) =>
          (row - col) * 0.19 + 0.3
        val forward = operator.applyTo(weights).toOption.get
        val adjoint = operator.transposeApplyTo(probe).toOption.get
        val permutation = (0 until features).reverse.toVector
        val featureSet = FeatureSet.unsafe(RoiId(samples * 10 + features), permutation)
        val permuted = operator.selectFeatures(featureSet).toOption.get.materialize.toOption.get
        val expected = patterns.selectFeatures(featureSet).toOption.get

        assertEqualsDouble(frobeniusDot(forward, probe), frobeniusDot(weights, adjoint), 1e-10)
        assertMatrixClose(permuted.value, expected.value)
        features += 1
      samples += 1
  }

  test("pattern operator reports malformed axes, shapes, and non-finite inputs") {
    val matrix = GaleTestMatrix.fromRows(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))
    val wrongRows = PatternOperator.fromOperator(
      SampleAxis.unsafe(3),
      Vector(FeatureIndex(0), FeatureIndex(1)),
      matrix,
      PatternOperatorProvenance.dense
    )
    val duplicateFeatures = PatternOperator.fromOperator(
      SampleAxis.unsafe(2),
      Vector(FeatureIndex(0), FeatureIndex(0)),
      matrix,
      PatternOperatorProvenance.dense
    )
    val nonFinite = PatternOperator.fromMatrix(
      PatternMatrix(
        GaleTestMatrix.fromRows(Vector(Vector(1.0, Double.NaN))),
        Vector(SampleIndex(0)),
        Vector(FeatureIndex(0), FeatureIndex(1))
      )
    )
    val valid = PatternOperator.fromMatrix(patternMatrix(Vector(Vector(1.0, 2.0), Vector(3.0, 4.0)))).toOption.get
    val poison = LinearOperator.fromFunctions(2, 2)(
      forward = (_, into) =>
        into(0) = Double.NaN
        into(1) = 0.0,
      transpose = (_, into) =>
        into(0) = 0.0
        into(1) = 0.0
    )
    val poisonPatterns = PatternOperator
      .fromOperator(
        SampleAxis.unsafe(2),
        Vector(FeatureIndex(0), FeatureIndex(1)),
        poison,
        PatternOperatorProvenance.composed
      )
      .toOption
      .get

    assertEquals(
      wrongRows.left.toOption,
      Some(MvpaError.MatrixShapeMismatch("operator rows 2 do not match sample axis 3"))
    )
    assertEquals(
      duplicateFeatures.left.toOption,
      Some(MvpaError.MatrixShapeMismatch("pattern operator feature indices must be unique"))
    )
    assertEquals(
      nonFinite.left.toOption,
      Some(MvpaError.InvalidPatternOperatorInput("pattern matrix contains non-finite values"))
    )
    assertEquals(
      valid.applyTo(GaleTestMatrix.fromRows(Vector(Vector(1.0)))).left.toOption,
      Some(MvpaError.MatrixShapeMismatch("feature-weight rows 1 do not match operator features 2"))
    )
    assertEquals(
      poisonPatterns.applyTo(DMat.eye(2)).left.toOption,
      Some(MvpaError.InvalidPatternOperatorInput("pattern-score output contains non-finite values"))
    )
  }

  private def patternMatrix(rows: Vector[Vector[Double]], featureIds: Vector[Int] = Vector.empty): PatternMatrix =
    val value = GaleTestMatrix.fromRows(rows)
    val features =
      if featureIds.isEmpty then Vector.tabulate(value.cols)(FeatureIndex.apply)
      else featureIds.map(FeatureIndex.apply)
    PatternMatrix(value, Vector.tabulate(value.rows)(SampleIndex.apply), features)

  private def multiply(left: DMat, right: DMat): DMat =
    require(left.cols == right.rows, "matrix-product shape mismatch")
    Matrix.tabulate(left.rows, right.cols): (row, col) =>
      var total = 0.0
      var inner = 0
      while inner < left.cols do
        total += left(row, inner) * right(inner, col)
        inner += 1
      total

  private def frobeniusDot(left: DMat, right: DMat): Double =
    require(left.rows == right.rows && left.cols == right.cols, "dot-product shape mismatch")
    var total = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        total += left(row, col) * right(row, col)
        col += 1
      row += 1
    total

  private def assertMatrixClose(actual: DMat, expected: DMat, absTol: Double = 1e-12, relTol: Double = 1e-12): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        val difference = math.abs(actual(row, col) - expected(row, col))
        val scale = math.max(math.abs(actual(row, col)), math.abs(expected(row, col)))
        assert(difference <= absTol + relTol * scale, s"matrix mismatch at ($row, $col)")
        col += 1
      row += 1
