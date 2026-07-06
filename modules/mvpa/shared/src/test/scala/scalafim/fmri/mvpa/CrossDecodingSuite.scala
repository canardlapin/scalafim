package scalafim.fmri.mvpa

class CrossDecodingSuite extends munit.FunSuite:

  private val source: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(1.0, -1.0, 0.0),
        Vector(2.0, -2.0, 1.0),
        Vector(-1.0, 1.0, 0.0),
        Vector(-2.0, 2.0, -1.0)
      )
    )

  private val target: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(3.0, -3.0, 1.0),
        Vector(-3.0, 3.0, -1.0),
        Vector(2.0, -2.0, 0.5),
        Vector(-2.0, 2.0, -0.5)
      )
    )

  private val design: CrossDecodingDesign =
    CrossDecodingDesign.unsafe(
      Vector("a", "a", "b", "b"),
      Vector("a", "b", "a", "b")
    )

  private val allFeatures: FeatureSet =
    FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2))

  private def predictionFrom(outcome: RoiOutcome.Success): ClassificationPrediction =
    outcome.payload match
      case Some(RoiPayload.Classification(prediction)) => prediction
      case other => fail(s"unexpected payload: $other")

  private def runSingle(
      sourceData: PatternMatrix = source,
      targetData: PatternMatrix = target,
      featureSet: FeatureSet = allFeatures
  ): RoiOutcome.Success =
    val result =
      CrossDomainMvpaEngine
        .run(sourceData, targetData, Vector(featureSet), design, CrossDecoding.naive(storePredictions = true))
        .toOption
        .get

    assertEquals(result.failures.length, 0)
    assertEquals(result.successes.length, 1)
    result.successes.head

  test("naive cross-decoding matches analytic source prototypes") {
    val success = runSingle(featureSet = FeatureSet.unsafe(RoiId(1), Vector(0, 1)))
    val prediction = predictionFrom(success)
    val expectedHigh = 1.0 / (1.0 + math.exp(-2.0))

    assertEquals(success.roiId.value, 1)
    assertEqualsDouble(success.metrics("Accuracy").get, 1.0, 1e-12)
    assertEqualsDouble(success.metrics("TestedSamples").get, 4.0, 1e-12)
    assertEqualsDouble(success.metrics("SourceSamples").get, 4.0, 1e-12)
    assertEqualsDouble(success.metrics("Classes").get, 2.0, 1e-12)
    assertEquals(prediction.classes.map(_.value), Vector("a", "b"))
    assertEquals(prediction.sampleIndices.map(_.value), Vector(0, 1, 2, 3))
    assertEquals(prediction.predicted.map(_.value), Vector("a", "b", "a", "b"))
    assertEqualsDouble(prediction.probabilities(0, 0), expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(0, 1), 1.0 - expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(1, 0), 1.0 - expectedHigh, 1e-12)
    assertEqualsDouble(prediction.probabilities(1, 1), expectedHigh, 1e-12)
  }

  test("cross-domain engine preserves searchlight feature-set plans") {
    val plan =
      FeatureSetPlan
        .searchlight(
          "xdec-search",
          Vector(
            FeatureSet.unsafe(RoiId(10), Vector(0, 1), center = Some(0), label = Some("front")),
            FeatureSet.unsafe(RoiId(11), Vector(1, 2), center = Some(2), label = Some("back"))
          )
        )
        .toOption
        .get

    val result =
      CrossDomainMvpaEngine
        .run(source, target, plan, design, CrossDecoding.naive())
        .toOption
        .get

    assertEquals(result.analysisName, "xdec_correlation_centroid")
    assertEquals(result.featureSetPlan.map(_.name), Some("xdec-search"))
    assertEquals(result.featureSetPlan.map(_.kind), Some(FeatureSetKind.Searchlight))
    assertEquals(result.successes.map(_.roiId.value), Vector(10, 11))
    assertEquals(result.failures.length, 0)
    result.successes.foreach { success =>
      assertEqualsDouble(success.metrics("Accuracy").get, 1.0, 1e-12)
      assertEquals(success.payload, None)
    }
  }

  test("cross-domain single-ROI task is a distributed execution boundary") {
    val outcome =
      CrossDomainMvpaTask.evaluate(
        CrossDomainPatternSource(PatternSource.fromMatrix(source), PatternSource.fromMatrix(target)),
        FeatureSet.unsafe(RoiId(3), Vector(0, 2)),
        design,
        CrossDecoding.naive()
      )

    outcome match
      case success: RoiOutcome.Success =>
        assertEquals(success.roiId.value, 3)
        assertEqualsDouble(success.metrics("Accuracy").get, 1.0, 1e-12)
      case failure: RoiOutcome.Failure =>
        fail(s"unexpected failure: ${failure.error.message}")
  }

  test("cross-domain accuracy is target-row aligned, not sample-id indexed") {
    val shiftedTarget =
      PatternMatrix(
        target.value,
        Vector(10, 11, 12, 13).map(SampleIndex.unsafe),
        target.featureIndices
      )
    val success = runSingle(targetData = shiftedTarget, featureSet = FeatureSet.unsafe(RoiId(1), Vector(0, 1)))
    val prediction = predictionFrom(success)

    assertEqualsDouble(success.metrics("Accuracy").get, 1.0, 1e-12)
    assertEquals(prediction.sampleIndices.map(_.value), Vector(10, 11, 12, 13))
  }

  test("naive cross-decoding is invariant to common feature permutation") {
    val reference = predictionFrom(runSingle())
    val permutation = Vector(2, 0, 1)
    val permutedSource = PatternMatrix.fromRows(source.value.toRows.map(row => permutation.map(row)))
    val permutedTarget = PatternMatrix.fromRows(target.value.toRows.map(row => permutation.map(row)))
    val permuted = predictionFrom(runSingle(permutedSource, permutedTarget))

    assertEquals(permuted.predicted.map(_.value), reference.predicted.map(_.value))
    assertEquals(permuted.classes.map(_.value), reference.classes.map(_.value))
    var row = 0
    while row < reference.probabilities.rows do
      var col = 0
      while col < reference.probabilities.cols do
        assertEqualsDouble(permuted.probabilities(row, col), reference.probabilities(row, col), 1e-12)
        col += 1
      row += 1
  }

  test("cross-decoding design rejects unknown target labels") {
    val error = CrossDecodingDesign(Vector("a", "b"), Vector("a", "c")).swap.toOption.get

    assert(error.message.contains("absent from source labels"))
  }

  test("cross-domain engine validates sample counts before visiting feature sets") {
    val shortTargetDesign =
      CrossDecodingDesign.unsafe(
        Vector("a", "a", "b", "b"),
        Vector("a", "b")
      )
    val result =
      CrossDomainMvpaEngine.run(source, target, Vector(allFeatures), shortTargetDesign, CrossDecoding.naive())

    assert(result.isLeft)
    assert(result.swap.toOption.get.message.contains("response length mismatch"))
  }

  test("cross-domain ROI failures stay local to the offending feature set") {
    val plan =
      FeatureSetPlan
        .regional(
          "mixed",
          Vector(
            FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
            FeatureSet.unsafe(RoiId(2), Vector(0, 99))
          )
        )
        .toOption
        .get

    val result =
      CrossDomainMvpaEngine
        .run(source, target, plan, design, CrossDecoding.naive())
        .toOption
        .get

    assertEquals(result.successes.map(_.roiId.value), Vector(1))
    assertEquals(result.failures.map(_.roiId.value), Vector(2))
    assert(result.failures.head.error.message.contains("missing feature"))
  }
