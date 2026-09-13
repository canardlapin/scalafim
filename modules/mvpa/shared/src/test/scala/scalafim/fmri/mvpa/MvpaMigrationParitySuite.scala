package scalafim.fmri.mvpa

import gale.linalg.DMat

class MvpaMigrationParitySuite extends munit.FunSuite:
  private val Tolerance = MvpaMigrationParityFixtures.tolerance

  test("Swift centroid freezes training Z-score, class order, priors, scores, and probabilities"):
    val fixture = MvpaMigrationParityFixtures.SwiftFit
    val model = SwiftCentroidClassifier()
      .fit(fixture.trainingPatterns, fixture.trainingResponse)
      .toOption
      .get match
      case value: SwiftCentroidModel => value
      case other                     => fail(s"unexpected Swift model: $other")

    assertEquals(model.classes.map(_.value), fixture.classes)
    assertVectorClose(model.priors, fixture.priors)
    assertVectorClose(model.scaler.means.toVector, fixture.zscoreCenter)
    assertVectorClose(model.scaler.scales.toVector, fixture.zscoreSampleSd)
    assertMatrixClose(model.centroids, fixture.scaledCentroids)

    val scaledTest = model.scaler.transform(fixture.testPatterns.value)
    val scores = Classification.linearCentroidScores(scaledTest, model.centroids, model.priors)
    val prediction = model.predict(fixture.testPatterns).toOption.get

    assertMatrixClose(scaledTest, fixture.scaledTest)
    assertMatrixClose(scores, fixture.scores)
    assertEquals(prediction.classes.map(_.value), fixture.classes)
    assertMatrixClose(prediction.probabilities, fixture.probabilities)
    assertEquals(prediction.predicted.map(_.value), fixture.predicted)

    val scaledTwice = model.scaler.transform(scaledTest)
    val doubleScaledProbabilities = Classification.softmax(
      Classification.linearCentroidScores(scaledTwice, model.centroids, model.priors)
    )
    assert(maxAbsoluteDifference(prediction.probabilities, doubleScaledProbabilities) > 0.1)

  test("Swift cross-validation freezes pooled-sample reduction and unequal-fold denominators"):
    val fixture = MvpaMigrationParityFixtures.SwiftCrossValidation
    val labels = Classification
      .categorical(fixture.response, fixture.labels.length)
      .toOption
      .get
    val folds = FoldPlan.leaveOneBlockOut(fixture.blocks).toOption.get
    val prediction = Classification
      .crossValidate(SwiftCentroidClassifier(), fixture.patterns, labels, folds)
      .toOption
      .get

    assertEquals(folds.folds.map(_.test.length), Vector(2, 3, 4))
    assertEquals(prediction.classes.map(_.value), fixture.classes)
    assertEquals(prediction.sampleIndices.map(_.value), Vector.range(0, fixture.testedSamples))
    assertMatrixClose(prediction.probabilities, fixture.probabilities)
    assertEquals(prediction.predicted.map(_.value), fixture.predicted)
    val correct = prediction.predicted
      .zip(fixture.labels)
      .count: (predicted, observed) =>
        predicted.value == observed
    assertEquals(correct, fixture.correct)

    val accuracy = Classification.accuracy(prediction, labels).toOption.get
    assertEqualsDouble(accuracy, fixture.pooledAccuracy, Tolerance)
    assertEqualsDouble(fixture.unweightedMeanFoldAccuracy, 13.0 / 18.0, Tolerance)
    assert(math.abs(accuracy - fixture.unweightedMeanFoldAccuracy) > 0.05)

    val featureSet = FeatureSet.unsafe(RoiId(904), Vector(0, 1, 2))
    val result = MvpaEngine
      .run(
        fixture.patterns,
        Vector(featureSet),
        fixture.response,
        CrossValidatedClassifierAnalysis(SwiftCentroidClassifier(), storePredictions = true),
        Some(folds)
      )
      .toOption
      .get
    val success = result.successes.head
    assertEqualsDouble(success.metrics("Accuracy").get, fixture.pooledAccuracy, Tolerance)
    assertEqualsDouble(success.metrics("TestedSamples").get, fixture.testedSamples.toDouble, Tolerance)

  test("identity-metric RDM uses every ordered distinct partition pair and retains signed values"):
    val fixture = MvpaMigrationParityFixtures.IdentityMetricRdm
    val expectedPairIndices = Vector((1, 0), (2, 0), (2, 1))

    assertEquals(Rdm.pairIndices(fixture.means.conditions), expectedPairIndices)
    assertEquals(
      expectedPairIndices.map((left, right) => (fixture.conditionNames(left), fixture.conditionNames(right))),
      fixture.pairOrder
    )
    assertEquals(
      orderedPartitionPairs(fixture.means.folds),
      fixture.orderedPartitionPairs
    )

    val independent = orderedPartitionOracle(fixture.means)
    val raw = Rdm.crossnobisDistances(fixture.means, normalizeByFeatures = false)
    val normalized = Rdm.crossnobisDistances(fixture.means, normalizeByFeatures = true)

    assertVectorClose(independent, fixture.raw)
    assertVectorClose(raw.values, fixture.raw)
    assertVectorClose(normalized.values, fixture.featureNormalized)
    assert(raw.values.head < 0.0)

  test("migration baseline failure policy remains typed"):
    val nonFinite = Rdm.squaredEuclidean(
      GaleTestMatrix.fromRows(Vector(Vector(0.0), Vector(Double.NaN)))
    )
    assertEquals(
      nonFinite.left.toOption,
      Some(MvpaError.InvalidRdmInput("RDM pattern matrix contains non-finite values"))
    )

    val fixture = MvpaMigrationParityFixtures.SwiftCrossValidation
    val labels = Classification
      .categorical(fixture.response, fixture.labels.length)
      .toOption
      .get
    val missingClass = FoldPlan.unsafe(
      Vector(Fold.unsafe("missing-class", Seq(1, 2, 4), Seq(0, 3))),
      samples = fixture.labels.length
    )
    assertEquals(
      Classification
        .crossValidate(SwiftCentroidClassifier(), fixture.patterns, labels, missingClass)
        .left
        .toOption,
      Some(MvpaError.InvalidClassifierInput("every training fold must contain every class"))
    )

  private def orderedPartitionOracle(means: PartitionMeans): Vector[Double] =
    Rdm
      .pairIndices(means.conditions)
      .map: (left, right) =>
        var total = 0.0
        var leftFold = 0
        while leftFold < means.folds do
          var rightFold = 0
          while rightFold < means.folds do
            if leftFold != rightFold then
              var dot = 0.0
              var feature = 0
              while feature < means.features do
                val leftDelta = means(left, feature, leftFold) - means(right, feature, leftFold)
                val rightDelta = means(left, feature, rightFold) - means(right, feature, rightFold)
                dot += leftDelta * rightDelta
                feature += 1
              total += dot
            rightFold += 1
          leftFold += 1
        total / (means.folds * (means.folds - 1))

  private def orderedPartitionPairs(folds: Int): Vector[(Int, Int)] =
    (for
      left <- Vector.range(0, folds)
      right <- Vector.range(0, folds)
      if left != right
    yield (left, right))

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.length, expected.length)
    actual
      .zip(expected)
      .foreach: (observed, reference) =>
        assertEqualsDouble(observed, reference, Tolerance + Tolerance * math.abs(reference))

  private def assertMatrixClose(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        val reference = expected(row, col)
        assertEqualsDouble(actual(row, col), reference, Tolerance + Tolerance * math.abs(reference))
        col += 1
      row += 1

  private def maxAbsoluteDifference(left: DMat, right: DMat): Double =
    require(left.rows == right.rows && left.cols == right.cols)
    var result = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        result = math.max(result, math.abs(left(row, col) - right(row, col)))
        col += 1
      row += 1
    result
