package scalafim.fmri.mvpa

import gale.linalg.DMat

class FeatureModelSuite extends munit.FunSuite:
  private val itemLabels: Vector[String] =
    (0 until 6).map(index => s"trial_$index").toVector

  private val featureRows: Vector[Vector[Double]] =
    Vector(
      Vector(0.0, 1.0),
      Vector(1.0, 0.0),
      Vector(0.0, 2.0),
      Vector(2.0, 0.0),
      Vector(1.0, 3.0),
      Vector(3.0, 1.0)
    )

  private val patternRows: Vector[Vector[Double]] =
    featureRows.map { row =>
      val f1 = row(0)
      val f2 = row(1)
      Vector(
        1.0 + 2.0 * f1 + 0.5 * f2,
        -2.0 - f1 + 3.0 * f2,
        0.5 + f1 - f2
      )
    }

  private val design: FeatureModelDesign =
    FeatureModelDesign
      .unsafe(
        itemLabels,
        GaleTestMatrix.fromRows(featureRows),
        Vector("semantic", "visual")
      )

  private val patterns: PatternMatrix =
    PatternMatrix.fromRows(patternRows)

  private val response: Response =
    Response.categorical(Vector("a", "b", "a", "b", "a", "b")).toOption.get

  private val folds: FoldPlan =
    FoldPlan.unsafe(
      Vector(
        Fold.unsafe("one", Seq(2, 3, 4, 5), Seq(0, 1)),
        Fold.unsafe("two", Seq(0, 1, 4, 5), Seq(2, 3)),
        Fold.unsafe("three", Seq(0, 1, 2, 3), Seq(4, 5))
      ),
      samples = 6
    )

  private val allFeatures: FeatureSetPlan =
    FeatureSetPlan.regional("roi", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2)))).toOption.get

  test("feature model encodes design features into held-out neural patterns") {
    val analysis = FeatureModelAnalysis(
      design,
      FeaturePredictionDirection.FeaturesToPatterns,
      estimator = FeatureRidgeEstimator(lambda = 1e-6),
      storePrediction = true
    )
    val result = MvpaEngine.run(patterns, allFeatures, response, analysis, Some(folds)).toOption.get
    val success = result.successes.head

    assertEquals(result.failures.length, 0)
    assert(success.metrics("TargetCorrelation").exists(_ > 0.999))
    assert(success.metrics("RdmCorrelation").exists(_ > 0.999))
    assert(success.metrics("Mse").exists(_ < 1e-6))
    success.payload match
      case Some(RoiPayload.FeatureModel(prediction)) =>
        assertEquals(prediction.direction, FeaturePredictionDirection.FeaturesToPatterns)
        assertEquals(prediction.items, itemLabels)
        assertEquals(prediction.targetNames, Vector("pattern_0", "pattern_1", "pattern_2"))
        assertEquals(prediction.predicted.cols, patterns.features)
        assertEquals(prediction.observed.rows, patterns.samples)
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("feature model decodes held-out neural patterns into design features") {
    val analysis = FeatureModelAnalysis(
      design,
      FeaturePredictionDirection.PatternsToFeatures,
      estimator = FeatureRidgeEstimator(lambda = 1e-6),
      storePrediction = true
    )
    val result = MvpaEngine.run(patterns, allFeatures, response, analysis, Some(folds)).toOption.get
    val success = result.successes.head

    assertEquals(result.failures.length, 0)
    assert(success.metrics("TargetCorrelation").exists(_ > 0.999))
    assert(success.metrics("Mse").exists(_ < 1e-6))
    success.payload match
      case Some(RoiPayload.FeatureModel(prediction)) =>
        assertEquals(prediction.direction, FeaturePredictionDirection.PatternsToFeatures)
        assertEquals(prediction.targetNames, Vector("semantic", "visual"))
        assertEquals(prediction.predicted.cols, 2)
        assertEquals(prediction.observed.cols, 2)
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("feature model averages repeated held-out samples with a zero-variance source") {
    val constantDesign =
      FeatureModelDesign
        .unsafe(
          Vector("item_0", "item_1", "item_2", "item_3", "item_4"),
          GaleTestMatrix.fromRows(Vector.fill(5)(Vector(0.0))),
          Vector("constant")
        )
    val targetPatterns =
      PatternMatrix.fromRows(
        Vector(
          Vector(0.0),
          Vector(10.0),
          Vector(20.0),
          Vector(30.0),
          Vector(40.0)
        )
      )
    val overlappingFolds =
      FoldPlan.unsafe(
        Vector(
          Fold.unsafe("one", Seq(0, 2, 3), Seq(1, 4)),
          Fold.unsafe("two", Seq(0, 3, 4), Seq(1, 2))
        ),
        samples = 5
      )
    val analysis = FeatureModelAnalysis(
      constantDesign,
      FeaturePredictionDirection.FeaturesToPatterns,
      estimator = FeatureRidgeEstimator(lambda = 1.0),
      storePrediction = true
    )
    val plan = FeatureSetPlan.regional("single-feature", Vector(FeatureSet.unsafe(RoiId(1), Vector(0)))).toOption.get
    val result =
      MvpaEngine
        .run(targetPatterns, plan, Response.categorical(Vector("a", "b", "a", "b", "a")).toOption.get, analysis, Some(overlappingFolds))
        .toOption
        .get

    assertEquals(result.failures.length, 0)
    assertEquals(result.successes.length, 1)
    assertEqualsDouble(result.successes.head.metrics("Observations").get, 3.0, 1e-12)
    result.successes.head.payload match
      case Some(RoiPayload.FeatureModel(prediction)) =>
        assertEquals(prediction.items, Vector("item_1", "item_2", "item_4"))
        assertEquals(prediction.targetNames, Vector("pattern_0"))
        assertEqualsDouble(prediction.predicted(0, 0), 20.0, 1e-12)
        assertEqualsDouble(prediction.predicted(1, 0), 70.0 / 3.0, 1e-12)
        assertEqualsDouble(prediction.predicted(2, 0), 50.0 / 3.0, 1e-12)
        assertEqualsDouble(prediction.observed(0, 0), 10.0, 1e-12)
        assertEqualsDouble(prediction.observed(1, 0), 20.0, 1e-12)
        assertEqualsDouble(prediction.observed(2, 0), 40.0, 1e-12)
      case other =>
        fail(s"unexpected payload: $other")
  }

  test("feature model design validates labels, dimensions, and finite values") {
    val duplicate = FeatureModelDesign(
      Vector("a", "a"),
      GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0)))
    )
    val nonFinite = FeatureModelDesign(
      Vector("a", "b"),
      GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(Double.NaN)))
    )

    assert(duplicate.swap.toOption.get.message.contains("unique"))
    assert(nonFinite.swap.toOption.get.message.contains("non-finite"))
  }

  test("feature model analysis reports typed shape errors through ROI failures") {
    val badDesign = FeatureModelDesign
      .unsafe(
        Vector("a", "b", "c"),
        GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0)))
      )
    val analysis = FeatureModelAnalysis(badDesign, FeaturePredictionDirection.FeaturesToPatterns)
    val result = MvpaEngine.run(patterns, allFeatures, response, analysis, Some(folds)).toOption.get

    assertEquals(result.successes.length, 0)
    assertEquals(result.failures.length, 1)
    assert(result.failures.head.error.message.contains("feature design rows"))
  }
