package scalafim.fmri.mvpa

import scalafim.linalg.DoubleMatrix

final case class MvpaBenchmarkCase(
    name: String,
    iterations: Int,
    runOnce: () => Either[MvpaError, Double]
)

final case class MvpaBenchmarkResult(
    name: String,
    iterations: Int,
    checksum: Double,
    elapsedNanos: Long
)

object MvpaBenchmarkHarness:
  def run(
      cases: Vector[MvpaBenchmarkCase],
      nowNanos: () => Long
  ): Either[MvpaError, Vector[MvpaBenchmarkResult]] =
    val out = Vector.newBuilder[MvpaBenchmarkResult]
    var caseIndex = 0
    while caseIndex < cases.length do
      val benchmark = cases(caseIndex)
      val trimmedName = benchmark.name.trim
      if trimmedName.isEmpty then
        return Left(MvpaError.InvalidClassifierInput("benchmark case name must be non-empty"))
      if benchmark.iterations <= 0 then
        return Left(MvpaError.InvalidClassifierInput("benchmark iterations must be positive"))

      val start = nowNanos()
      var checksum = 0.0
      var iteration = 0
      while iteration < benchmark.iterations do
        benchmark.runOnce() match
          case Right(value) if value.isFinite =>
            checksum += value
          case Right(_) =>
            return Left(MvpaError.InvalidClassifierInput(s"benchmark '$trimmedName' produced a non-finite checksum"))
          case Left(error) =>
            return Left(error)
        iteration += 1
      val elapsed = nowNanos() - start
      out += MvpaBenchmarkResult(trimmedName, benchmark.iterations, checksum, elapsed)
      caseIndex += 1
    Right(out.result())

class MvpaParitySuite extends munit.FunSuite:
  private val RReferenceTolerance = 1e-10

  private def assertVectorEquals(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tolerance)
      i += 1

  private def assertMatrixEquals(actual: DoubleMatrix, expected: DoubleMatrix, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def assertMetricVectorEquals(actual: MetricVector, expected: MetricVector, tolerance: Double): Unit =
    assertEquals(actual.names, expected.names)
    assertEquals(actual.values.length, expected.values.length)
    var i = 0
    while i < actual.values.length do
      // The generated feature decode fixture contains exact tied RDM distances; keep rank-score tolerance local.
      val metricTolerance =
        if expected.names(i) == "RdmCorrelation" then math.max(tolerance, 5e-3)
        else tolerance
      assertEqualsDouble(actual.values(i), expected.values(i), metricTolerance)
      i += 1

  private def patternMatrix(matrix: DoubleMatrix): PatternMatrix =
    PatternMatrix(
      matrix,
      (0 until matrix.rows).map(SampleIndex.unsafe).toVector,
      (0 until matrix.cols).map(FeatureIndex.unsafe).toVector
    )

  private def featureFolds: FoldPlan =
    FoldPlan.unsafe(
      Vector(
        Fold.unsafe("fold_a", Seq(2, 3, 4, 5), Seq(0, 1)),
        Fold.unsafe("fold_b", Seq(0, 1, 4, 5), Seq(2, 3)),
        Fold.unsafe("fold_c", Seq(0, 1, 2, 3), Seq(4, 5))
      ),
      samples = MvpaRReferenceFixtures.FeatureRsa.items.length
    )

  private def featureDesign: FeatureModelDesign =
    FeatureModelDesign.unsafe(
      MvpaRReferenceFixtures.FeatureRsa.items,
      MvpaRReferenceFixtures.FeatureRsa.featureRows,
      MvpaRReferenceFixtures.FeatureRsa.featureNames
    )

  private def featureResponse: Response =
    Response.categorical(Vector("a", "b", "c", "a", "b", "c")).toOption.get

  private def featurePrediction(success: RoiOutcome.Success): FeatureModelPrediction =
    success.payload match
      case Some(RoiPayload.FeatureModel(prediction)) => prediction
      case other => fail(s"unexpected feature model payload: $other")

  private def classificationPrediction(success: RoiOutcome.Success): ClassificationPrediction =
    success.payload match
      case Some(RoiPayload.Classification(prediction)) => prediction
      case other => fail(s"unexpected classification payload: $other")

  private def xdecSource: PatternMatrix =
    patternMatrix(MvpaRReferenceFixtures.NaiveXdec.sourceRows)

  private def xdecTarget: PatternMatrix =
    patternMatrix(MvpaRReferenceFixtures.NaiveXdec.targetRows)

  private def xdecDesign: CrossDecodingDesign =
    CrossDecodingDesign.unsafe(
      MvpaRReferenceFixtures.NaiveXdec.sourceLabels,
      MvpaRReferenceFixtures.NaiveXdec.targetLabels
    )

  private def assertNaiveXdecCases(
      result: MvpaResult,
      expectedCases: Vector[MvpaRReferenceFixtures.NaiveXdecCase]
  ): Unit =
    assertEquals(result.failures.length, 0)
    assertEquals(result.successes.length, expectedCases.length)
    result.successes.zip(expectedCases).foreach { case (success, expected) =>
      assertEquals(success.roiId.value, expected.roiId)
      assertEquals(success.features.map(_.value), expected.featureIndices)
      assertEqualsDouble(success.metrics("Accuracy").get, expected.expectedAccuracy, RReferenceTolerance)
      assertEqualsDouble(success.metrics("TestedSamples").get, MvpaRReferenceFixtures.NaiveXdec.targetLabels.length.toDouble, RReferenceTolerance)
      assertEqualsDouble(success.metrics("SourceSamples").get, MvpaRReferenceFixtures.NaiveXdec.sourceLabels.length.toDouble, RReferenceTolerance)
      assertEqualsDouble(success.metrics("Classes").get, expected.expectedClasses.length.toDouble, RReferenceTolerance)

      val prediction = classificationPrediction(success)
      assertEquals(prediction.classes.map(_.value), expected.expectedClasses)
      assertEquals(prediction.predicted.map(_.value), expected.expectedPredicted)
      assertMatrixEquals(prediction.probabilities, expected.expectedProbabilities, RReferenceTolerance)
    }

  test("RDM parity fixtures keep squared, normalized, and Euclidean estimands explicit") {
    val squared = Rdm.squaredEuclidean(MvpaParityFixtures.Rdm.patterns).toOption.get
    val normalized = Rdm
      .squaredEuclidean(MvpaParityFixtures.Rdm.patterns, normalizeByFeatures = true)
      .toOption
      .get
    val euclidean = Rdm.euclidean(MvpaParityFixtures.Rdm.patterns).toOption.get
    val correlation = Rdm.correlation(MvpaParityFixtures.Correlation.patterns).toOption.get

    assertVectorEquals(squared.values, MvpaParityFixtures.Rdm.squaredEuclidean, 1e-12)
    assertVectorEquals(normalized.values, MvpaParityFixtures.Rdm.squaredEuclideanNormalized, 1e-12)
    assertVectorEquals(euclidean.values, MvpaParityFixtures.Rdm.euclidean, 1e-12)
    assertVectorEquals(correlation.values, MvpaParityFixtures.Correlation.distances, 1e-12)
  }

  test("crossnobis parity fixture keeps feature normalization explicit") {
    val raw = Rdm.crossnobisDistances(MvpaParityFixtures.Crossnobis.means, normalizeByFeatures = false)
    val normalized = Rdm.crossnobisDistances(MvpaParityFixtures.Crossnobis.means, normalizeByFeatures = true)

    assertEqualsDouble(raw.values.head, MvpaParityFixtures.Crossnobis.rawDistance, 1e-12)
    assertEqualsDouble(normalized.values.head, MvpaParityFixtures.Crossnobis.normalizedDistance, 1e-12)
  }

  test("RSA scorer parity fixture residualizes labeled nuisance RDMs") {
    val scorer = RdmScorer.PartialPearson.unsafe(Vector(MvpaParityFixtures.Rsa.trendControlReversed))
    val score = scorer
      .score(MvpaParityFixtures.Rsa.items, MvpaParityFixtures.Rsa.observed, MvpaParityFixtures.Rsa.target)
      .toOption
      .get

    assertEqualsDouble(score, 1.0, 1e-12)
  }

  test("classifier parity fixtures anchor centroid and ridge probability oracles") {
    val correlationFit = CorrelationCentroidClassifier()
      .fit(MvpaParityFixtures.Classifiers.centroidPatterns, MvpaParityFixtures.Classifiers.centroidResponse)
      .toOption
      .get
    val correlationPrediction = correlationFit.predict(MvpaParityFixtures.Classifiers.centroidPatterns).toOption.get

    assertEquals(correlationPrediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(correlationPrediction.probabilities(0, 0), MvpaParityFixtures.Classifiers.centroidHighProbability, 1e-12)
    assertEqualsDouble(correlationPrediction.probabilities(1, 1), MvpaParityFixtures.Classifiers.centroidHighProbability, 1e-12)

    val swiftFit = SwiftCentroidClassifier(FeatureScaling.None)
      .fit(MvpaParityFixtures.Classifiers.swiftPatterns, MvpaParityFixtures.Classifiers.swiftResponse)
      .toOption
      .get
    val swiftPrediction = swiftFit.predict(MvpaParityFixtures.Classifiers.swiftPatterns).toOption.get

    assertEquals(swiftPrediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(swiftPrediction.probabilities(0, 0), MvpaParityFixtures.Classifiers.swiftHighProbability, 1e-12)
    assertEqualsDouble(swiftPrediction.probabilities(1, 1), MvpaParityFixtures.Classifiers.swiftHighProbability, 1e-12)

    val ridgeFit = RidgeLdaClassifier(gamma = 1.0)
      .fit(MvpaParityFixtures.Classifiers.ridgePatterns, MvpaParityFixtures.Classifiers.ridgeResponse)
      .toOption
      .get
    val ridgePrediction = ridgeFit.predict(MvpaParityFixtures.Classifiers.ridgePatterns).toOption.get

    assertEquals(ridgePrediction.classes.map(_.value), Vector("a", "b"))
    assertEqualsDouble(ridgePrediction.probabilities(0, 0), MvpaParityFixtures.Classifiers.ridgeHighProbability, 1e-12)
    assertEqualsDouble(ridgePrediction.probabilities(3, 1), MvpaParityFixtures.Classifiers.ridgeHighProbability, 1e-12)
  }

  test("rMVPA feature_rsa reference fixture anchors bidirectional regional ridge fits") {
    val patterns = patternMatrix(MvpaRReferenceFixtures.FeatureRsa.patternRows)
    val regional =
      FeatureSetPlan
        .regional(
          "r-feature-rsa-regional",
          Vector(FeatureSet.unsafe(RoiId(301), 0 until patterns.features, label = Some("all_patterns")))
        )
        .toOption
        .get
    val estimator = FeatureRidgeEstimator(MvpaRReferenceFixtures.FeatureRsa.lambda)

    val encode =
      MvpaEngine
        .run(
          patterns,
          regional,
          featureResponse,
          FeatureModelAnalysis(featureDesign, FeaturePredictionDirection.FeaturesToPatterns, estimator, storePrediction = true),
          Some(featureFolds)
        )
        .toOption
        .get
        .successes
        .head
    val encodePrediction = featurePrediction(encode)

    assertMetricVectorEquals(encode.metrics, MvpaRReferenceFixtures.FeatureRsa.EncodeRegional.metrics, RReferenceTolerance)
    assertMatrixEquals(encodePrediction.predicted, MvpaRReferenceFixtures.FeatureRsa.EncodeRegional.predicted, RReferenceTolerance)
    assertMatrixEquals(encodePrediction.observed, MvpaRReferenceFixtures.FeatureRsa.EncodeRegional.observed, RReferenceTolerance)
    assertEquals(encodePrediction.items, MvpaRReferenceFixtures.FeatureRsa.items)

    val decode =
      MvpaEngine
        .run(
          patterns,
          regional,
          featureResponse,
          FeatureModelAnalysis(featureDesign, FeaturePredictionDirection.PatternsToFeatures, estimator, storePrediction = true),
          Some(featureFolds)
        )
        .toOption
        .get
        .successes
        .head
    val decodePrediction = featurePrediction(decode)

    assertMetricVectorEquals(decode.metrics, MvpaRReferenceFixtures.FeatureRsa.DecodeRegional.metrics, RReferenceTolerance)
    assertMatrixEquals(decodePrediction.predicted, MvpaRReferenceFixtures.FeatureRsa.DecodeRegional.predicted, RReferenceTolerance)
    assertMatrixEquals(decodePrediction.observed, MvpaRReferenceFixtures.FeatureRsa.DecodeRegional.observed, RReferenceTolerance)
    assertEquals(decodePrediction.targetNames, MvpaRReferenceFixtures.FeatureRsa.featureNames)
  }

  test("rMVPA feature_rsa reference fixture anchors searchlight feature-model execution") {
    val patterns = patternMatrix(MvpaRReferenceFixtures.FeatureRsa.patternRows)
    val plan =
      FeatureSetPlan
        .searchlight(
          "r-feature-rsa-search",
          Vector(
            FeatureSet.unsafe(
              RoiId(302),
              MvpaRReferenceFixtures.FeatureRsa.searchlightPatternIndices,
              center = Some(2),
              label = Some("generated_searchlight")
            )
          )
        )
        .toOption
        .get
    val result =
      MvpaEngine
        .run(
          patterns,
          plan,
          featureResponse,
          FeatureModelAnalysis(
            featureDesign,
            FeaturePredictionDirection.FeaturesToPatterns,
            FeatureRidgeEstimator(MvpaRReferenceFixtures.FeatureRsa.lambda),
            storePrediction = true
          ),
          Some(featureFolds)
        )
        .toOption
        .get
    val success = result.successes.head
    val prediction = featurePrediction(success)

    assertEquals(result.featureSetPlan.map(_.kind), Some(FeatureSetKind.Searchlight))
    assertEquals(success.roiId.value, 302)
    assertEquals(prediction.targetNames, Vector("pattern_0", "pattern_2", "pattern_3"))
    assertMetricVectorEquals(success.metrics, MvpaRReferenceFixtures.FeatureRsa.EncodeSearchlight.metrics, RReferenceTolerance)
    assertMatrixEquals(prediction.predicted, MvpaRReferenceFixtures.FeatureRsa.EncodeSearchlight.predicted, RReferenceTolerance)
    assertMatrixEquals(prediction.observed, MvpaRReferenceFixtures.FeatureRsa.EncodeSearchlight.observed, RReferenceTolerance)
  }

  test("rMVPA naive_xdec reference fixture anchors regional cross-decoding") {
    val regional =
      FeatureSetPlan
        .regional(
          "r-naive-xdec-regional",
          MvpaRReferenceFixtures.NaiveXdec.regionalCases.map { expected =>
            FeatureSet.unsafe(RoiId(expected.roiId), expected.featureIndices, label = Some(expected.label))
          }
        )
        .toOption
        .get
    val result =
      CrossDomainMvpaEngine
        .run(xdecSource, xdecTarget, regional, xdecDesign, CrossDecoding.naive(storePredictions = true))
        .toOption
        .get

    assertEquals(result.featureSetPlan.map(_.kind), Some(FeatureSetKind.Region))
    assertNaiveXdecCases(result, MvpaRReferenceFixtures.NaiveXdec.regionalCases)
  }

  test("rMVPA naive_xdec reference fixture anchors searchlight scanner cross-decoding") {
    val searchlights =
      FeatureSetPlan
        .searchlight(
          "r-naive-xdec-search",
          MvpaRReferenceFixtures.NaiveXdec.searchlightCases.map { expected =>
            FeatureSet.unsafe(
              RoiId(expected.roiId),
              expected.featureIndices,
              center = expected.featureIndices.headOption,
              label = Some(expected.label)
            )
          }
        )
        .toOption
        .get
    val result =
      NaiveCrossDecodingScanner(storePredictions = true)
        .run(xdecSource, xdecTarget, searchlights, xdecDesign)
        .toOption
        .get

    assertEquals(result.featureSetPlan.map(_.kind), Some(FeatureSetKind.Searchlight))
    assertNaiveXdecCases(result, MvpaRReferenceFixtures.NaiveXdec.searchlightCases)
  }

  test("lightweight benchmark harness runs deterministic workloads with explicit checksums") {
    var tick = 0L
    def clock(): Long =
      val value = tick
      tick += 100L
      value

    val workloads = Vector(
      MvpaBenchmarkCase(
        "rdm_squared",
        iterations = 3,
        () => Rdm.squaredEuclidean(MvpaParityFixtures.Rdm.patterns).map(_.values.sum)
      ),
      MvpaBenchmarkCase(
        "crossnobis_normalized",
        iterations = 2,
        () => Right(Rdm.crossnobisDistances(MvpaParityFixtures.Crossnobis.means).values.sum)
      ),
      MvpaBenchmarkCase(
        "swift_predict",
        iterations = 2,
        () =>
          val fit = SwiftCentroidClassifier(FeatureScaling.None)
            .fit(MvpaParityFixtures.Classifiers.swiftPatterns, MvpaParityFixtures.Classifiers.swiftResponse)
          fit.flatMap(_.predict(MvpaParityFixtures.Classifiers.swiftPatterns)).map { prediction =>
            prediction.probabilities.dataArray.sum
          }
      )
    )

    val results = MvpaBenchmarkHarness.run(workloads, clock).toOption.get

    assertEquals(results.map(_.name), Vector("rdm_squared", "crossnobis_normalized", "swift_predict"))
    assertEquals(results.map(_.elapsedNanos), Vector(100L, 100L, 100L))
    assertEqualsDouble(results(0).checksum, 120.0, 1e-12)
    assertEqualsDouble(results(1).checksum, 8.0, 1e-12)
    assertEqualsDouble(results(2).checksum, 8.0, 1e-12)
  }

  test("lightweight benchmark harness validates workload shape") {
    val result = MvpaBenchmarkHarness.run(
      Vector(MvpaBenchmarkCase("bad", iterations = 0, () => Right(0.0))),
      () => 0L
    )

    assert(result.swap.toOption.get.message.contains("iterations"))
  }
