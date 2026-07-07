package scalafim.fmri.mvpa

class SearchlightClassificationSuite extends munit.FunSuite:

  private val labels: Response =
    Response.categorical(Vector("a", "b", "a", "b", "a", "b", "a", "b")).toOption.get

  private val data: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(2.0, 1.8, 0.2, 0.0, 1.0, 0.5),
        Vector(-2.0, -1.9, -0.1, 0.2, -1.0, -0.4),
        Vector(2.2, 1.7, 0.1, 0.1, 1.1, 0.6),
        Vector(-2.1, -1.8, -0.2, 0.0, -1.1, -0.5),
        Vector(1.9, 2.1, 0.0, -0.1, 0.9, 0.4),
        Vector(-1.8, -2.2, 0.2, 0.1, -0.8, -0.6),
        Vector(2.1, 2.0, -0.1, 0.2, 1.0, 0.7),
        Vector(-2.0, -2.1, 0.1, -0.2, -1.0, -0.7)
      )
    )

  private val folds: FoldPlan =
    FoldPlan.leaveOneBlockOut(Vector(1, 1, 2, 2, 3, 3, 4, 4)).toOption.get

  private val overlappingFolds: FoldPlan =
    FoldPlan.unsafe(
      Vector(
        Fold.unsafe("one", Seq(3, 4, 5, 6, 7), Seq(0, 1, 2)),
        Fold.unsafe("two", Seq(0, 1, 5, 6, 7), Seq(2, 3, 4)),
        Fold.unsafe("three", Seq(0, 1, 2, 3, 7), Seq(4, 5, 6)),
        Fold.unsafe("four", Seq(1, 2, 3, 4, 5), Seq(0, 6, 7))
      ),
      samples = 8
    )

  private val searchlights: FeatureSetPlan =
    FeatureSetPlan
      .searchlight(
        "toy-search",
        Vector(
          FeatureSet.unsafe(RoiId(10), Vector(0, 1), center = Some(0), label = Some("left")),
          FeatureSet.unsafe(RoiId(11), Vector(1, 2, 3), center = Some(2), label = Some("middle")),
          FeatureSet.unsafe(RoiId(12), Vector(0, 4, 5), center = Some(4), label = Some("right"))
        )
      )
      .toOption
      .get

  test("SWIFT searchlight scanner matches the reference classifier analysis") {
    val classifier = SwiftCentroidClassifier(FeatureScaling.unsafeDiagonalShrinkage(0.2))
    val reference =
      MvpaEngine
        .run(data, searchlights, labels, CrossValidatedClassifierAnalysis(classifier, storePredictions = true), Some(folds))
        .toOption
        .get
    val fast =
      SearchlightClassifierScanner(classifier, storePredictions = true)
        .run(data, searchlights, labels, folds)
        .toOption
        .get

    assertEquivalent(reference, fast)
  }

  test("SWIFT searchlight scanner matches reference classification with overlapping folds") {
    val classifier = SwiftCentroidClassifier(FeatureScaling.unsafeDiagonalShrinkage(0.2))
    val reference =
      MvpaEngine
        .run(data, searchlights, labels, CrossValidatedClassifierAnalysis(classifier, storePredictions = true), Some(overlappingFolds))
        .toOption
        .get
    val fast =
      SearchlightClassifierScanner(classifier, storePredictions = true)
        .run(data, searchlights, labels, overlappingFolds)
        .toOption
        .get

    assertEquivalent(reference, fast)
  }

  test("ridge LDA searchlight scanner matches the reference classifier analysis") {
    val classifier = RidgeLdaClassifier(gamma = 0.5)
    val reference =
      MvpaEngine
        .run(data, searchlights, labels, CrossValidatedClassifierAnalysis(classifier, storePredictions = true), Some(folds))
        .toOption
        .get
    val scanned =
      SearchlightClassifierScanner(classifier, storePredictions = true)
        .run(data, searchlights, labels, folds)
        .toOption
        .get

    assertEquivalent(reference, scanned)
  }

  test("searchlight scanner falls back to the classifier contract for non-specialized classifiers") {
    val classifier = CorrelationCentroidClassifier()
    val reference =
      MvpaEngine
        .run(data, searchlights, labels, CrossValidatedClassifierAnalysis(classifier, storePredictions = true), Some(folds))
        .toOption
        .get
    val scanned =
      SearchlightClassifierScanner(classifier, storePredictions = true)
        .run(data, searchlights, labels, folds)
        .toOption
        .get

    assertEquivalent(reference, scanned)
  }

  test("SWIFT searchlight scanner preserves reference failures") {
    val plan =
      FeatureSetPlan
        .searchlight("bad-search", Vector(FeatureSet.unsafe(RoiId(99), Vector(0, 99), center = Some(0))))
        .toOption
        .get
    val reference =
      MvpaEngine
        .run(data, plan, labels, CrossValidatedClassifierAnalysis(SwiftCentroidClassifier()), Some(folds))
        .toOption
        .get
    val fast =
      SearchlightClassifierScanner(SwiftCentroidClassifier())
        .run(data, plan, labels, folds)
        .toOption
        .get

    assertEquals(reference.successes.length, 0)
    assertEquals(fast.successes.length, 0)
    assertEquals(reference.failures.length, 1)
    assertEquals(fast.failures.length, 1)
    assertEquals(fast.failures.head.error.message, reference.failures.head.error.message)
  }

  private def assertEquivalent(reference: MvpaResult, scanned: MvpaResult): Unit =
    assertEquals(scanned.analysisName, reference.analysisName)
    assertEquals(scanned.featureSetPlan, reference.featureSetPlan)
    assertEquals(scanned.outcomes.length, reference.outcomes.length)
    reference.outcomes.zip(scanned.outcomes).foreach { case (expected, actual) =>
      (expected, actual) match
        case (e: RoiOutcome.Success, a: RoiOutcome.Success) =>
          assertEquals(a.roiId, e.roiId)
          assertEquals(a.features, e.features)
          assertEqualsDouble(a.metrics("Accuracy").get, e.metrics("Accuracy").get, 1e-12)
          assertEqualsDouble(a.metrics("TestedSamples").get, e.metrics("TestedSamples").get, 1e-12)
          assertPredictionPayload(e.payload, a.payload)
        case (e: RoiOutcome.Failure, a: RoiOutcome.Failure) =>
          assertEquals(a.roiId, e.roiId)
          assertEquals(a.features, e.features)
          assertEquals(a.error.message, e.error.message)
        case other =>
          fail(s"outcome mismatch: $other")
    }

  private def assertPredictionPayload(expected: Option[RoiPayload], actual: Option[RoiPayload]): Unit =
    (expected, actual) match
      case (Some(RoiPayload.Classification(e)), Some(RoiPayload.Classification(a))) =>
        assertEquals(a.classes, e.classes)
        assertEquals(a.sampleIndices, e.sampleIndices)
        assertEquals(a.probabilities.rows, e.probabilities.rows)
        assertEquals(a.probabilities.cols, e.probabilities.cols)
        var row = 0
        while row < e.probabilities.rows do
          var col = 0
          while col < e.probabilities.cols do
            assertEqualsDouble(a.probabilities(row, col), e.probabilities(row, col), 1e-12)
            col += 1
          row += 1
      case (None, None) =>
      case other =>
        fail(s"payload mismatch: $other")
