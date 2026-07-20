package scalafim.fmri.mvpa

import gale.linalg.DMat

class MvpaAdversarialSuite extends munit.FunSuite:
  private val Tolerance = 1e-10

  private def patternMatrix(matrix: DMat): PatternMatrix =
    PatternMatrix(
      matrix,
      (0 until matrix.rows).map(SampleIndex.unsafe).toVector,
      (0 until matrix.cols).map(FeatureIndex.unsafe).toVector
    )

  private def assertFiniteMatrix(matrix: DMat): Unit =
    val data = matrix.copyData
    var i = 0
    while i < data.length do
      assert(data(i).isFinite)
      i += 1

  private def assertFiniteMetrics(metrics: MetricVector, names: String*): Unit =
    names.foreach { name =>
      assert(metrics(name).exists(_.isFinite), s"metric $name should be finite")
    }

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
      assertEqualsDouble(sum, 1.0, Tolerance)
      row += 1

  private def classificationPrediction(success: RoiOutcome.Success): ClassificationPrediction =
    success.payload match
      case Some(RoiPayload.Classification(prediction)) => prediction
      case other => fail(s"unexpected classification payload: $other")

  private def featurePrediction(success: RoiOutcome.Success): FeatureModelPrediction =
    success.payload match
      case Some(RoiPayload.FeatureModel(prediction)) => prediction
      case other => fail(s"unexpected feature model payload: $other")

  private def assertEquivalentXdec(reference: MvpaResult, scanned: MvpaResult): Unit =
    assertEquals(scanned.analysisName, reference.analysisName)
    assertEquals(scanned.outcomes.length, reference.outcomes.length)
    reference.outcomes.zip(scanned.outcomes).foreach { case (expected, actual) =>
      (expected, actual) match
        case (e: RoiOutcome.Success, a: RoiOutcome.Success) =>
          assertEquals(a.roiId, e.roiId)
          assertEquals(a.features, e.features)
          assertEquals(a.metrics.names, e.metrics.names)
          var metric = 0
          while metric < e.metrics.values.length do
            assertEqualsDouble(a.metrics.values(metric), e.metrics.values(metric), Tolerance)
            metric += 1
          val ep = classificationPrediction(e)
          val ap = classificationPrediction(a)
          assertEquals(ap.classes, ep.classes)
          assertEquals(ap.predicted, ep.predicted)
          assertEquals(ap.sampleIndices, ep.sampleIndices)
          var row = 0
          while row < ep.probabilities.rows do
            var col = 0
            while col < ep.probabilities.cols do
              assertEqualsDouble(ap.probabilities(row, col), ep.probabilities(row, col), Tolerance)
              col += 1
            row += 1
        case (e: RoiOutcome.Failure, a: RoiOutcome.Failure) =>
          assertEquals(a.roiId, e.roiId)
          assertEquals(a.features, e.features)
          assertEquals(a.error.message, e.error.message)
        case other =>
          fail(s"outcome mismatch: $other")
    }

  private val featureItems: Vector[String] =
    (0 until 8).map(index => s"item_$index").toVector

  private val featureFolds: FoldPlan =
    FoldPlan.unsafe(
      Vector(
        Fold.unsafe("fold_0", Seq(2, 3, 4, 5, 6, 7), Seq(0, 1)),
        Fold.unsafe("fold_1", Seq(0, 1, 4, 5, 6, 7), Seq(2, 3)),
        Fold.unsafe("fold_2", Seq(0, 1, 2, 3, 6, 7), Seq(4, 5)),
        Fold.unsafe("fold_3", Seq(0, 1, 2, 3, 4, 5), Seq(6, 7))
      ),
      samples = featureItems.length
    )

  private val featureResponse: Response =
    Response.categorical(Vector("a", "b", "c", "a", "b", "c", "a", "b")).toOption.get

  test("feature ridge fit stays finite for near-collinear high-dynamic-range predictors") {
    val featureRows =
      GaleTestMatrix.fromRows(
        Vector.tabulate(featureItems.length) { row =>
          val x = (row.toDouble - 3.5) * 1000000.0
          Vector(
            x,
            2.0 * x + 0.001 * (row + 1).toDouble,
            -0.5 * x + 0.0007 * (row % 3).toDouble,
            1e-6 * (row + 1).toDouble
          )
        }
      )
    val patternRows =
      GaleTestMatrix.fromRows(
        Vector.tabulate(featureItems.length) { row =>
          val x = (row.toDouble - 3.5) * 1000000.0
          Vector(
            0.5 + 2e-6 * x + 0.01 * math.sin(row.toDouble),
            -1.0 - 3e-6 * x + 0.02 * math.cos(row.toDouble),
            2.0 + 1e-6 * x + 0.03 * ((row % 2).toDouble - 0.5)
          )
        }
      )
    val design =
      FeatureModelDesign.unsafe(featureItems, featureRows, Vector("x", "x2", "minus_half_x", "tiny"))
    val patterns =
      patternMatrix(patternRows)
    val plan =
      FeatureSetPlan
        .regional("near-collinear", Vector(FeatureSet.unsafe(RoiId(801), Vector(0, 1, 2))))
        .toOption
        .get
    val result =
      MvpaEngine
        .run(
          patterns,
          plan,
          featureResponse,
          FeatureModelAnalysis(design, FeaturePredictionDirection.FeaturesToPatterns, FeatureRidgeEstimator(lambda = 0.25), storePrediction = true),
          Some(featureFolds)
        )
        .toOption
        .get
    val success = result.successes.head
    val prediction = featurePrediction(success)

    assertEquals(result.failures.length, 0)
    assertFiniteMatrix(prediction.predicted)
    assertFiniteMatrix(prediction.observed)
    assertFiniteMetrics(success.metrics, "TargetCorrelation", "Mse", "RSquared", "MeanTargetwiseCorrelation")
  }

  test("feature model non-finite pattern values fail only selected local ROIs") {
    val design =
      FeatureModelDesign.unsafe(
        MvpaRReferenceFixtures.FeatureRsa.items,
        MvpaRReferenceFixtures.FeatureRsa.featureRows,
        MvpaRReferenceFixtures.FeatureRsa.featureNames
      )
    val poisonedRows =
      MvpaRReferenceFixtures.FeatureRsa.patternRows
        .toRows
        .updated(2, MvpaRReferenceFixtures.FeatureRsa.patternRows.toRows(2).updated(3, Double.NaN))
    val patterns =
      patternMatrix(GaleTestMatrix.fromRows(poisonedRows))
    val plan =
      FeatureSetPlan
        .regional(
          "mixed-feature-finiteness",
          Vector(
            FeatureSet.unsafe(RoiId(901), Vector(0, 1, 2), label = Some("clean")),
            FeatureSet.unsafe(RoiId(902), Vector(1, 3), label = Some("poisoned"))
          )
        )
        .toOption
        .get
    val result =
      MvpaEngine
        .run(
          patterns,
          plan,
          Response.categorical(Vector("a", "b", "c", "a", "b", "c")).toOption.get,
          FeatureModelAnalysis(design, FeaturePredictionDirection.FeaturesToPatterns, FeatureRidgeEstimator(lambda = 0.75), storePrediction = true),
          Some(
            FoldPlan.unsafe(
              Vector(
                Fold.unsafe("fold_a", Seq(2, 3, 4, 5), Seq(0, 1)),
                Fold.unsafe("fold_b", Seq(0, 1, 4, 5), Seq(2, 3)),
                Fold.unsafe("fold_c", Seq(0, 1, 2, 3), Seq(4, 5))
              ),
              samples = 6
            )
          )
        )
        .toOption
        .get

    assertEquals(result.successes.map(_.roiId.value), Vector(901))
    assertEquals(result.failures.map(_.roiId.value), Vector(902))
    assert(result.failures.head.error.message.contains("non-finite"))
    assertFiniteMatrix(featurePrediction(result.successes.head).predicted)
  }

  test("naive cross-decoding source non-finites are local and scanner matches reference") {
    val sourceRows =
      Vector(
        Vector(2.0, 1.0, -1.0, 0.0, 0.5, -0.5),
        Vector(-1.0, 2.0, 1.0, -0.5, 0.0, 1.0),
        Vector(0.0, -1.0, 2.0, 1.0, Double.NaN, 0.5),
        Vector(2.2, 1.1, -0.9, 0.1, 0.4, -0.4),
        Vector(-1.1, 2.1, 0.9, -0.4, 0.1, 0.9),
        Vector(0.1, -0.9, 2.1, 0.9, -0.9, 0.4)
      )
    val targetRows =
      Vector(
        Vector(3.0, 1.5, -1.5, 0.0, 0.7, -0.7),
        Vector(-1.5, 3.0, 1.5, -0.7, 0.0, 1.4),
        Vector(0.0, -1.5, 3.0, 1.4, -1.4, 0.7),
        Vector(2.8, 1.4, -1.4, 0.1, 0.8, -0.8)
      )
    val source =
      PatternMatrix.fromRows(sourceRows)
    val target =
      PatternMatrix.fromRows(targetRows)
    val design =
      CrossDecodingDesign.unsafe(
        Vector("a", "b", "c", "a", "b", "c"),
        Vector("a", "b", "c", "a")
      )
    val plan =
      FeatureSetPlan
        .searchlight(
          "mixed-xdec-finiteness",
          Vector(
            FeatureSet.unsafe(RoiId(911), Vector(0, 1, 2), center = Some(1)),
            FeatureSet.unsafe(RoiId(912), Vector(2, 4, 5), center = Some(4)),
            FeatureSet.unsafe(RoiId(913), Vector(0, 3, 5), center = Some(3))
          )
        )
        .toOption
        .get
    val reference =
      CrossDomainMvpaEngine
        .run(source, target, plan, design, CrossDecoding.naive(storePredictions = true))
        .toOption
        .get
    val scanned =
      NaiveCrossDecodingScanner(storePredictions = true)
        .run(source, target, plan, design)
        .toOption
        .get

    assertEquivalentXdec(reference, scanned)
    assertEquals(scanned.successes.map(_.roiId.value), Vector(911, 913))
    assertEquals(scanned.failures.map(_.roiId.value), Vector(912))
    assert(scanned.failures.head.error.message.contains("training data"))
  }

  test("degenerate prototype correlations stay finite and normalized") {
    val source =
      PatternMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0, 1.0),
          Vector(1.0, 1.0, 1.0),
          Vector(2.0, 2.0, 2.0),
          Vector(2.0, 2.0, 2.0)
        )
      )
    val target =
      PatternMatrix.fromRows(
        Vector(
          Vector(3.0, 3.0, 3.0),
          Vector(4.0, 4.0, 4.0)
        )
      )
    val design =
      CrossDecodingDesign.unsafe(Vector("a", "a", "b", "b"), Vector("a", "b"))
    val plan =
      FeatureSetPlan
        .regional("degenerate-xdec", Vector(FeatureSet.unsafe(RoiId(921), Vector(0, 1, 2))))
        .toOption
        .get
    val result =
      NaiveCrossDecodingScanner(storePredictions = true)
        .run(source, target, plan, design)
        .toOption
        .get
    val success = result.successes.head
    val prediction = classificationPrediction(success)

    assertEquals(result.failures.length, 0)
    assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
    assertEquals(prediction.predicted.map(_.value), Vector("a", "a"))
    assertEqualsDouble(success.metrics("Accuracy").get, 0.5, Tolerance)
    assertFiniteNormalized(prediction)
    assertEqualsDouble(prediction.probabilities(0, 0), 0.5, Tolerance)
    assertEqualsDouble(prediction.probabilities(0, 1), 0.5, Tolerance)
    assertEqualsDouble(prediction.probabilities(1, 0), 0.5, Tolerance)
    assertEqualsDouble(prediction.probabilities(1, 1), 0.5, Tolerance)
  }
