package scalafim.fmri.mvpa

import gale.linalg.DMat

class ClassificationSuite extends munit.FunSuite:

  private val labels: Response =
    Response.categorical(Vector("a", "b", "a", "b", "a", "b", "a", "b")).toOption.get

  private val labelVector: Vector[ClassLabel] =
    Classification.categorical(labels, labels.length).toOption.get

  private val data: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(2.0, 2.0, 0.0),
        Vector(-2.0, -2.0, 0.0),
        Vector(2.2, 1.8, 0.1),
        Vector(-2.1, -1.9, -0.1),
        Vector(1.9, 2.1, 0.0),
        Vector(-1.8, -2.2, 0.1),
        Vector(2.1, 2.0, -0.1),
        Vector(-2.0, -2.1, 0.0)
      )
    )

  private val folds: FoldPlan =
    FoldPlan.leaveOneBlockOut(Vector(1, 1, 2, 2, 3, 3, 4, 4)).toOption.get

  private def makeLabels(values: String*): Vector[ClassLabel] =
    values.map(ClassLabel.unsafe).toVector

  private def response(values: String*): Response =
    Response.categorical(values).toOption.get

  private def leftMessage[A](result: Either[MvpaError, A]): String =
    result match
      case Left(error) => error.message
      case Right(_) => fail("expected failure")

  private def assertFiniteNormalized(prediction: ClassificationPrediction): Unit =
    var row = 0
    while row < prediction.probabilities.rows do
      var sum = 0.0
      var col = 0
      while col < prediction.probabilities.cols do
        val value = prediction.probabilities(row, col)
        assert(value.isFinite)
        sum += value
        col += 1
      assertEqualsDouble(sum, 1.0, 1e-12)
      row += 1

  private final case class FixedOrderClassifier(predictionClasses: Vector[ClassLabel]) extends Classifier:
    override val name: String = "fixed_order"

    override def fit(train: PatternMatrix, response: Response): Either[MvpaError, ClassifierModel] =
      Right(FixedOrderModel(predictionClasses))

  private final case class FixedOrderModel(classes: Vector[ClassLabel]) extends ClassifierModel:
    override val classifierName: String = "fixed_order"

    override def predict(test: PatternMatrix): Either[MvpaError, ClassificationPrediction] =
      val out = new Array[Double](test.samples * classes.length)
      var row = 0
      while row < test.samples do
        out(row * classes.length) = 0.2
        out(row * classes.length + 1) = 0.8
        row += 1
      Right(ClassificationPrediction(classes, GaleTestMatrix.fromArray(test.samples, classes.length, out), test.sampleIndices))

  private final case class TrainMeanProbabilityClassifier() extends Classifier:
    override val name: String = "train_mean_probability"

    override def fit(train: PatternMatrix, response: Response): Either[MvpaError, ClassifierModel] =
      if train.features < 1 then Left(MvpaError.InvalidClassifierInput("test classifier requires at least one feature"))
      else
        for
          _ <- Classification.validateFinite(train.value, "training data")
          labels <- Classification.categorical(response, train.samples)
        yield
          var sum = 0.0
          var row = 0
          while row < train.samples do
            sum += train.value(row, 0)
            row += 1
          TrainMeanProbabilityModel(labels.distinct, sum / train.samples)

  private final case class TrainMeanProbabilityModel(
      classes: Vector[ClassLabel],
      firstClassProbability: Double
  ) extends ClassifierModel:
    override val classifierName: String = "train_mean_probability"

    override def predict(test: PatternMatrix): Either[MvpaError, ClassificationPrediction] =
      if classes.length != 2 then Left(MvpaError.InvalidClassifierInput("test classifier expects two classes"))
      else
        val out = new Array[Double](test.samples * classes.length)
        var row = 0
        while row < test.samples do
          out(row * classes.length) = firstClassProbability
          out(row * classes.length + 1) = 1.0 - firstClassProbability
          row += 1
        Right(ClassificationPrediction(classes, GaleTestMatrix.fromArray(test.samples, classes.length, out), test.sampleIndices))

  test("correlation centroid classifier predicts by class templates") {
    val classifier = CorrelationCentroidClassifier()
    val fit = classifier.fit(data, labels).toOption.get
    val prediction = fit.predict(data).toOption.get

    assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
    assertEquals(prediction.predicted.map(_.value), Vector("a", "b", "a", "b", "a", "b", "a", "b"))
  }

  test("correlation centroid predictions are invariant to a common row offset") {
    val shifted = PatternMatrix.fromRows(data.value.toRows.map(row => row.map(_ + 100.0)))
    val prediction = CorrelationCentroidClassifier().fit(data, labels).toOption.get.predict(data).toOption.get
    val shiftedPrediction = CorrelationCentroidClassifier().fit(shifted, labels).toOption.get.predict(shifted).toOption.get

    var row = 0
    while row < prediction.probabilities.rows do
      var col = 0
      while col < prediction.probabilities.cols do
        assertEqualsDouble(shiftedPrediction.probabilities(row, col), prediction.probabilities(row, col), 1e-12)
        col += 1
      row += 1
  }

  test("swift centroid classifier supports cross-validated ROI analysis") {
    val analysis = CrossValidatedClassifierAnalysis(SwiftCentroidClassifier())
    val featureSet = FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2))
    val result = MvpaEngine.run(data, Vector(featureSet), labels, analysis, Some(folds)).toOption.get

    assertEquals(result.failures.length, 0)
    assertEquals(result.successes.length, 1)
    assertEqualsDouble(result.successes.head.metrics("Accuracy").get, 1.0, 1e-12)
  }

  test("cross-validated classifier analysis can store prediction payloads") {
    val analysis = CrossValidatedClassifierAnalysis(SwiftCentroidClassifier(), storePredictions = true)
    val featureSet = FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2))
    val result = MvpaEngine.run(data, Vector(featureSet), labels, analysis, Some(folds)).toOption.get

    result.successes.head.payload match
      case Some(RoiPayload.Classification(prediction)) =>
        assertEquals(prediction.probabilities.rows, data.samples)
        assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("ridge LDA classifier returns normalized probabilities") {
    val classifier = RidgeLdaClassifier(gamma = 0.01)
    val fit = classifier.fit(data, labels).toOption.get
    val prediction = fit.predict(data).toOption.get

    assertEquals(prediction.predicted.map(_.value), Vector("a", "b", "a", "b", "a", "b", "a", "b"))
    assertFiniteNormalized(prediction)
  }

  test("ridge LDA matches a one-dimensional analytic oracle") {
    val oracleData = PatternMatrix.fromRows(
      Vector(
        Vector(0.0),
        Vector(2.0),
        Vector(4.0),
        Vector(6.0)
      )
    )
    val classifier = RidgeLdaClassifier(gamma = 1.0)
    val fit = classifier.fit(oracleData, response("a", "a", "b", "b")).toOption.get
    val prediction = fit.predict(oracleData).toOption.get
    val expectedHigh = 1.0 / (1.0 + math.exp(-2.4))

    assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(prediction.probabilities(0, 0), expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(0, 1), 1.0 - expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(3, 0), 1.0 - expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(3, 1), expectedHigh, 1e-12)
    assertFiniteNormalized(prediction)
  }

  test("swift centroid keeps finite normalized probabilities with a zero-variance feature") {
    val classifier = SwiftCentroidClassifier()
    val fit = classifier.fit(data, labels).toOption.get
    val prediction = fit.predict(data).toOption.get

    assertFiniteNormalized(prediction)
  }

  test("swift diagonal shrinkage validates alpha through typed errors") {
    val result = SwiftCentroidClassifier(FeatureScaling.unsafeDiagonalShrinkage(1.5)).fit(data, labels)

    assert(result.isLeft)
    assert(leftMessage(result).contains("diagonal shrinkage"))
  }

  test("classifiers reject non-finite training and prediction matrices") {
    val classifiers = Vector(
      CorrelationCentroidClassifier(),
      SwiftCentroidClassifier(),
      RidgeLdaClassifier()
    )
    val badTrain = PatternMatrix.fromRows(
      Vector(
        Vector(2.0, 2.0, Double.NaN),
        Vector(-2.0, -2.0, 0.0),
        Vector(2.2, 1.8, 0.1),
        Vector(-2.1, -1.9, -0.1)
      )
    )
    val badTest = PatternMatrix.fromRows(
      Vector(
        Vector(2.0, 2.0, Double.PositiveInfinity),
        Vector(-2.0, -2.0, 0.0)
      )
    )
    classifiers.foreach { classifier =>
      val fitError = classifier.fit(badTrain, response("a", "b", "a", "b"))
      assert(fitError.isLeft)
      assert(leftMessage(fitError).contains("training data"))

      val fit = classifier.fit(data, labels).toOption.get
      val predictError = fit.predict(badTest)
      assert(predictError.isLeft)
      assert(leftMessage(predictError).contains("test data"))
    }
  }

  test("cross-validation aligns classifier probability columns by class label") {
    val smallData = PatternMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(1.1, 0.0),
        Vector(0.0, 1.1)
      )
    )
    val smallLabels = makeLabels("a", "b", "a", "b")
    val plan = FoldPlan.unsafe(
      Vector(
        Fold.unsafe("one", Seq(0, 1), Seq(2, 3)),
        Fold.unsafe("two", Seq(2, 3), Seq(0, 1))
      ),
      samples = 4
    )
    val classifier = FixedOrderClassifier(makeLabels("b", "a"))
    val prediction = Classification.crossValidate(classifier, smallData, smallLabels, plan).toOption.get

    assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
    var row = 0
    while row < prediction.probabilities.rows do
      assertEqualsDouble(prediction.probabilities(row, 0), 0.8, 1e-12)
      assertEqualsDouble(prediction.probabilities(row, 1), 0.2, 1e-12)
      row += 1
  }

  test("cross-validation averages repeated test rows and sorts sample outputs") {
    val smallData = PatternMatrix.fromRows(
      Vector(
        Vector(0.2),
        Vector(0.0),
        Vector(0.4),
        Vector(0.6),
        Vector(0.8)
      )
    )
    val smallLabels = makeLabels("a", "b", "a", "b", "a")
    val plan = FoldPlan.unsafe(
      Vector(
        Fold.unsafe("one", Seq(2, 3, 4), Seq(0, 1)),
        Fold.unsafe("two", Seq(0, 2, 3), Seq(1, 4))
      ),
      samples = 5
    )

    val prediction = Classification.crossValidate(TrainMeanProbabilityClassifier(), smallData, smallLabels, plan).toOption.get

    assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
    assertEquals(prediction.sampleIndices.map(_.value), Vector(0, 1, 4))
    assertEqualsDouble(prediction.probabilities(0, 0), 0.6, 1e-12)
    assertEqualsDouble(prediction.probabilities(0, 1), 0.4, 1e-12)
    assertEqualsDouble(prediction.probabilities(1, 0), 0.5, 1e-12)
    assertEqualsDouble(prediction.probabilities(1, 1), 0.5, 1e-12)
    assertEqualsDouble(prediction.probabilities(2, 0), 0.4, 1e-12)
    assertEqualsDouble(prediction.probabilities(2, 1), 0.6, 1e-12)
  }

  test("cross-validation validates fold sample counts when called directly") {
    val badPlan = FoldPlan.unsafe(Vector(Fold.unsafe("small", Seq(0, 1), Seq(2, 3))), samples = 4)
    val result = Classification.crossValidate(SwiftCentroidClassifier(), data, labelVector, badPlan)

    assert(result.isLeft)
    assert(leftMessage(result).contains("fold plan sample count"))
  }

  test("accuracy rejects empty predictions") {
    val prediction = ClassificationPrediction(
      makeLabels("a", "b"),
      GaleTestMatrix.fromArray(0, 2, Array.empty[Double]),
      Vector.empty
    )

    val result = Classification.accuracy(prediction, labelVector)
    assert(result.isLeft)
    assert(leftMessage(result).contains("at least one prediction"))
  }

  test("cross-validated classifier analysis requires every class in every training fold") {
    val badFolds = FoldPlan.leaveOneBlockOut(Vector(1, 2, 1, 2, 1, 2, 1, 2)).toOption.get
    val analysis = CrossValidatedClassifierAnalysis(RidgeLdaClassifier())
    val featureSet = FeatureSet.unsafe(RoiId(1), Vector(0, 1))

    val result = MvpaEngine.run(data, Vector(featureSet), labels, analysis, Some(badFolds)).toOption.get
    assertEquals(result.failures.length, 1)
    assert(result.failures.head.error.message.contains("every training fold"))
  }
