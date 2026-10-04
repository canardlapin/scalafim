package scalafim.fmri.mvpa

import gale.linalg.DMat

class ClassificationSuite extends munit.FunSuite:
  private def labels(values: String*): Vector[ClassLabel] = values.map(ClassLabel.unsafe).toVector

  private val training = DMat.dense(8, 3, Vector(
    2.0, 2.0, 0.0,
    -2.0, -2.0, 0.0,
    2.2, 1.8, 0.1,
    -2.1, -1.9, -0.1,
    1.9, 2.1, 0.0,
    -1.8, -2.2, 0.1,
    2.1, 2.0, -0.1,
    -2.0, -2.1, 0.0
  ))
  private val trainingLabels = labels("a", "b", "a", "b", "a", "b", "a", "b")

  private def assertNormalized(prediction: CategoricalProbabilities): Unit =
    var row = 0
    while row < prediction.probabilities.rows do
      var sum = 0.0
      var column = 0
      while column < prediction.probabilities.cols do
        val value = prediction.probabilities(row, column)
        assert(value.isFinite)
        sum += value
        column += 1
      assertEqualsDouble(sum, 1.0, 1e-12)
      row += 1

  test("correlation centroid consumes dense matrices and has row-offset invariant scores") {
    val model = CorrelationCentroidClassifier().fit(training, trainingLabels).toOption.get
    val predicted = model.predict(training).toOption.get
    val shifted = DMat.dense(training.rows, training.cols,
      Vector.tabulate(training.rows * training.cols)(index => training(index / training.cols, index % training.cols) + 100.0))
    val shiftedPredicted = CorrelationCentroidClassifier().fit(shifted, trainingLabels).toOption.get.predict(shifted).toOption.get

    assertEquals(predicted.classes.map(_.value), Vector("a", "b"))
    assertEquals(predicted.predicted.map(_.value), Vector("a", "b", "a", "b", "a", "b", "a", "b"))
    var row = 0
    while row < predicted.probabilities.rows do
      var column = 0
      while column < predicted.probabilities.cols do
        assertEqualsDouble(shiftedPredicted.probabilities(row, column), predicted.probabilities(row, column), 1e-12)
        column += 1
      row += 1
    assertNormalized(predicted)
  }

  test("swift centroid keeps sample-standard-deviation scaling and prior linear scores") {
    val data = DMat.dense(4, 1, Vector(0.0, 2.0, 4.0, 6.0))
    val model = SwiftCentroidClassifier(FeatureScaling.ZScore).fit(data, labels("a", "a", "b", "b")).toOption.get
    val prediction = model.predict(data).toOption.get
    val sampleSd = math.sqrt(20.0 / 3.0)

    assertEqualsDouble(model.scaler.means(0), 3.0, 1e-12)
    assertEqualsDouble(model.scaler.scales(0), sampleSd, 1e-12)
    assertEquals(prediction.predicted.map(_.value), Vector("a", "a", "b", "b"))
    assertNormalized(prediction)
  }

  test("ridge LDA matches a one-dimensional analytic probability oracle") {
    val data = DMat.dense(4, 1, Vector(0.0, 2.0, 4.0, 6.0))
    val prediction = RidgeLdaClassifier(1.0).fit(data, labels("a", "a", "b", "b")).toOption.get.predict(data).toOption.get
    val expectedHigh = 1.0 / (1.0 + math.exp(-2.4))

    assertEquals(prediction.predicted.map(_.value), Vector("a", "a", "b", "b"))
    assertEqualsDouble(prediction.probabilities(0, 0), expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(3, 1), expectedHigh, 1e-12)
    assertNormalized(prediction)
  }

  test("class summary and explicit probability reordering preserve the declared class columns") {
    val summary = Classification.classSummary(DMat.dense(4, 2, Vector(1.0, 3.0, 3.0, 5.0, 7.0, 11.0, 9.0, 13.0)), labels("x", "x", "y", "y")).toOption.get
    assertEquals(summary.counts, Vector(2, 2))
    assertEqualsDouble(summary.means(0, 0), 2.0, 1e-12)
    assertEqualsDouble(summary.means(1, 1), 12.0, 1e-12)

    val reordered = Classification.reorderProbabilities(labels("b", "a"), DMat.dense(2, 2, Vector(0.2, 0.8, 0.7, 0.3)), labels("a", "b")).toOption.get
    assertEqualsDouble(reordered(0, 0), 0.8, 1e-12)
    assertEqualsDouble(reordered(1, 1), 0.7, 1e-12)
  }

  test("matrix kernels reject malformed labels, non-finite values, and incompatible prediction width") {
    val badLabels = CorrelationCentroidClassifier().fit(training, labels("a", "b"))
    assert(badLabels.isLeft)
    val badTrain = SwiftCentroidClassifier().fit(DMat.dense(2, 1, Vector(0.0, Double.NaN)), labels("a", "b"))
    assert(badTrain.isLeft)
    val model = RidgeLdaClassifier().fit(training, trainingLabels).toOption.get
    assert(model.predict(DMat.dense(1, 2, Vector(1.0, 2.0))).isLeft)
    assert(Classification.reorderProbabilities(labels("a", "a"), DMat.dense(1, 2, Vector(0.5, 0.5)), labels("a", "b")).isLeft)
  }

  test("finite Swift inputs that overflow scores return a typed refusal"):
    val data = DMat.dense(4, 1, Vector(1e200, 1e200, -1e200, -1e200))
    val model = SwiftCentroidClassifier(FeatureScaling.None).fit(data, labels("a", "a", "b", "b")).toOption.get
    val result = model.predict(DMat.dense(1, 1, Vector(0.0)))
    assert(result.left.toOption.exists(_.message.contains("non-finite")))

  test("class probability construction validates rows and derives first-maximum labels"):
    val classes = labels("a", "b")
    val probabilities = CategoricalProbabilities.from(classes, DMat.dense(2, 2, Vector(0.9, 0.1, 0.5, 0.5))).toOption.get
    assertEquals(probabilities.predicted, labels("a", "a"))
    assert(CategoricalProbabilities.from(classes, DMat.dense(1, 2, Vector(Double.NaN, 0.1))).isLeft)
    assert(CategoricalProbabilities.from(classes, DMat.dense(1, 2, Vector(0.9, 0.9))).isLeft)
    assert(CategoricalProbabilities.from(classes, DMat.dense(1, 2, Vector(-0.1, 1.1))).isLeft)
    assert(CorrelationCentroidClassifier().fit(DMat.dense(4, 1, Vector(1.0, 2.0, 3.0, 4.0)), labels("a", "a", "b", "b")).isLeft)
