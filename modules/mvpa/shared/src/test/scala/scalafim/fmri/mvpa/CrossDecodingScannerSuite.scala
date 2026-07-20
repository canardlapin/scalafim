package scalafim.fmri.mvpa

import gale.linalg.DMat

class CrossDecodingScannerSuite extends munit.FunSuite:

  private val source: PatternMatrix =
    PatternMatrix.fromRows(
      Vector(
        Vector(2.0, 1.0, -1.0, 0.0, 0.5, -0.5),
        Vector(-1.0, 2.0, 1.0, -0.5, 0.0, 1.0),
        Vector(0.0, -1.0, 2.0, 1.0, -1.0, 0.5),
        Vector(2.2, 1.1, -0.9, 0.1, 0.4, -0.4),
        Vector(-1.1, 2.1, 0.9, -0.4, 0.1, 0.9),
        Vector(0.1, -0.9, 2.1, 0.9, -0.9, 0.4)
      )
    )

  private val targetRows: Vector[Vector[Double]] =
    Vector(
      Vector(3.0, 1.5, -1.5, 0.0, 0.7, -0.7),
      Vector(-1.5, 3.0, 1.5, -0.7, 0.0, 1.4),
      Vector(0.0, -1.5, 3.0, 1.4, -1.4, 0.7),
      Vector(0.1, -1.4, 2.8, 1.2, -1.3, 0.8),
      Vector(-1.4, 2.8, 1.4, -0.8, 0.1, 1.2),
      Vector(2.8, 1.4, -1.4, 0.1, 0.8, -0.8)
    )

  private val target: PatternMatrix =
    PatternMatrix.fromRows(targetRows)

  private val targetShuffled: PatternMatrix =
    val order = Vector(4, 0, 2, 1, 3, 5)
    PatternMatrix(
      GaleTestMatrix.fromRows(targetRows.map(row => order.map(row))),
      (10 until 16).map(SampleIndex.unsafe).toVector,
      order.map(FeatureIndex.unsafe)
    )

  private val design: CrossDecodingDesign =
    CrossDecodingDesign.unsafe(
      Vector("a", "b", "c", "a", "b", "c"),
      Vector("a", "b", "c", "c", "b", "a")
    )

  private val searchlights: FeatureSetPlan =
    FeatureSetPlan
      .searchlight(
        "xdec-fast-search",
        Vector(
          FeatureSet.unsafe(RoiId(10), Vector(0, 1, 2), center = Some(1), label = Some("anterior")),
          FeatureSet.unsafe(RoiId(11), Vector(2, 3, 4), center = Some(3), label = Some("middle")),
          FeatureSet.unsafe(RoiId(12), Vector(0, 4, 5), center = Some(4), label = Some("posterior"))
        )
      )
      .toOption
      .get

  test("naive cross-decoding scanner matches the reference engine with shuffled target storage") {
    val reference =
      CrossDomainMvpaEngine
        .run(source, targetShuffled, searchlights, design, CrossDecoding.naive(storePredictions = true))
        .toOption
        .get
    val scanned =
      NaiveCrossDecodingScanner(storePredictions = true)
        .run(source, targetShuffled, searchlights, design)
        .toOption
        .get

    assertEquivalent(reference, scanned)
  }

  test("naive cross-decoding scanner omits prediction payloads when disabled") {
    val regional =
      FeatureSetPlan
        .regional(
          "xdec-regions",
          Vector(
            FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2, 3)),
            FeatureSet.unsafe(RoiId(2), Vector(2, 3, 4, 5))
          )
        )
        .toOption
        .get
    val reference =
      CrossDomainMvpaEngine
        .run(source, target, regional, design, CrossDecoding.naive())
        .toOption
        .get
    val scanned =
      NaiveCrossDecodingScanner()
        .run(source, target, regional, design)
        .toOption
        .get

    assertEquivalent(reference, scanned)
    assert(scanned.successes.forall(_.payload.isEmpty))
  }

  test("naive cross-decoding scanner preserves local ROI failures") {
    val targetMissing =
      PatternMatrix(
        GaleTestMatrix.fromRows(targetRows.map(_.take(5))),
        target.sampleIndices,
        Vector(0, 1, 2, 3, 4).map(FeatureIndex.unsafe)
      )
    val plan =
      FeatureSetPlan
        .searchlight(
          "bad-xdec-search",
          Vector(
            FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2), center = Some(1)),
            FeatureSet.unsafe(RoiId(2), Vector(0, 5), center = Some(5))
          )
        )
        .toOption
        .get
    val reference =
      CrossDomainMvpaEngine
        .run(source, targetMissing, plan, design, CrossDecoding.naive())
        .toOption
        .get
    val scanned =
      NaiveCrossDecodingScanner()
        .run(source, targetMissing, plan, design)
        .toOption
        .get

    assertEquivalent(reference, scanned)
    assertEquals(scanned.successes.map(_.roiId.value), Vector(1))
    assertEquals(scanned.failures.map(_.roiId.value), Vector(2))
    assert(scanned.failures.head.error.message.contains("missing feature"))
  }

  test("naive cross-decoding scanner reports non-finite target data like the reference engine") {
    val badRows = targetRows.updated(0, targetRows(0).updated(1, Double.NaN))
    val badTarget = PatternMatrix.fromRows(badRows)
    val plan =
      FeatureSetPlan
        .regional("bad-target", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2))))
        .toOption
        .get
    val reference =
      CrossDomainMvpaEngine
        .run(source, badTarget, plan, design, CrossDecoding.naive())
        .toOption
        .get
    val scanned =
      NaiveCrossDecodingScanner(storePredictions = true)
        .run(source, badTarget, plan, design)
        .toOption
        .get

    assertEquivalent(reference, scanned)
    assertEquals(scanned.successes.length, 0)
    assert(scanned.failures.head.error.message.contains("test data"))
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
          assertEqualsDouble(a.metrics("SourceSamples").get, e.metrics("SourceSamples").get, 1e-12)
          assertEqualsDouble(a.metrics("Classes").get, e.metrics("Classes").get, 1e-12)
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
