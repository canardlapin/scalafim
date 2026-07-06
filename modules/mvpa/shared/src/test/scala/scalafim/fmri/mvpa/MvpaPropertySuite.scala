package scalafim.fmri.mvpa

import scalafim.linalg.DoubleMatrix

class MvpaPropertySuite extends munit.FunSuite:
  private val Tolerance = 1e-10

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
      assertEqualsDouble(actual.values(i), expected.values(i), tolerance)
      i += 1

  private def patternMatrix(matrix: DoubleMatrix): PatternMatrix =
    PatternMatrix(
      matrix,
      (0 until matrix.rows).map(SampleIndex.unsafe).toVector,
      (0 until matrix.cols).map(FeatureIndex.unsafe).toVector
    )

  private def classificationPrediction(success: RoiOutcome.Success): ClassificationPrediction =
    success.payload match
      case Some(RoiPayload.Classification(prediction)) => prediction
      case other => fail(s"unexpected classification payload: $other")

  private def featurePrediction(success: RoiOutcome.Success): FeatureModelPrediction =
    success.payload match
      case Some(RoiPayload.FeatureModel(prediction)) => prediction
      case other => fail(s"unexpected feature model payload: $other")

  private def assertEquivalentResults(reference: MvpaResult, actual: MvpaResult): Unit =
    assertEquals(actual.analysisName, reference.analysisName)
    assertEquals(actual.featureSetPlan, reference.featureSetPlan)
    assertEquals(actual.outcomes.length, reference.outcomes.length)
    reference.outcomes.zip(actual.outcomes).foreach { case (expected, observed) =>
      (expected, observed) match
        case (e: RoiOutcome.Success, a: RoiOutcome.Success) =>
          assertEquals(a.roiId, e.roiId)
          assertEquals(a.features, e.features)
          assertMetricVectorEquals(a.metrics, e.metrics, Tolerance)
          val expectedPrediction = classificationPrediction(e)
          val actualPrediction = classificationPrediction(a)
          assertEquals(actualPrediction.classes, expectedPrediction.classes)
          assertEquals(actualPrediction.sampleIndices, expectedPrediction.sampleIndices)
          assertEquals(actualPrediction.predicted, expectedPrediction.predicted)
          assertMatrixEquals(actualPrediction.probabilities, expectedPrediction.probabilities, Tolerance)
        case (e: RoiOutcome.Failure, a: RoiOutcome.Failure) =>
          assertEquals(a.roiId, e.roiId)
          assertEquals(a.features, e.features)
          assertEquals(a.error.message, e.error.message)
        case other =>
          fail(s"outcome mismatch: $other")
    }

  private def generatedCrossDecodingCase(caseIndex: Int): (PatternMatrix, PatternMatrix, CrossDecodingDesign, FeatureSetPlan) =
    val classNames = Vector("a", "b", "c")
    val features = 7
    val sourceLabels = classNames.flatMap(label => Vector.fill(3)(label))
    val targetLabels =
      Vector("c", "a", "b", "c", "b", "a")

    def classIndex(label: String): Int =
      classNames.indexOf(label)

    def prototype(klass: Int, feature: Int): Double =
      val base =
        if feature % classNames.length == klass then 2.1
        else if (feature + klass) % 3 == 0 then 0.35
        else -0.75
      base + 0.13 * math.sin((klass + 1).toDouble * (feature + 2).toDouble)

    val sourceRows =
      sourceLabels.zipWithIndex.map { case (label, row) =>
        val klass = classIndex(label)
        Vector.tabulate(features) { feature =>
          prototype(klass, feature) +
            0.05 * (row % 3).toDouble +
            0.03 * math.cos((caseIndex + 1).toDouble * (row + 1).toDouble * (feature + 1).toDouble)
        }
      }

    val canonicalTargetRows =
      targetLabels.zipWithIndex.map { case (label, row) =>
        val klass = classIndex(label)
        Vector.tabulate(features) { feature =>
          1.08 * prototype(klass, feature) +
            0.2 * math.sin((caseIndex + 2).toDouble * (row + 1).toDouble + feature.toDouble) -
            0.04 * feature.toDouble
        }
      }

    val targetPermutation =
      (0 until features).map(feature => (feature * 3 + caseIndex + 1) % features).toVector
    val targetRows = canonicalTargetRows.map(row => targetPermutation.map(row))
    val source =
      PatternMatrix.fromRows(sourceRows)
    val target =
      PatternMatrix(
        DoubleMatrix.fromRows(targetRows),
        (20 until 20 + targetRows.length).map(SampleIndex.unsafe).toVector,
        targetPermutation.map(FeatureIndex.unsafe)
      )

    val baseSets =
      Vector(
        Vector(0, 1, 2),
        Vector(2, 3, 4),
        Vector(1, 4, 5, 6),
        Vector(0, 3, 6)
      )
    val sets =
      baseSets.zipWithIndex.map { case (indices, setIndex) =>
        val rotated = indices.map(index => (index + caseIndex + setIndex) % features)
        FeatureSet.unsafe(RoiId(400 + caseIndex * 10 + setIndex), rotated, center = rotated.headOption, label = Some(s"generated_${caseIndex}_$setIndex"))
      }
    val plan =
      FeatureSetPlan.searchlight(s"generated_xdec_$caseIndex", sets).toOption.get
    val design =
      CrossDecodingDesign.unsafe(sourceLabels, targetLabels)

    (source, target, design, plan)

  private def featureFolds: FoldPlan =
    FoldPlan.unsafe(
      Vector(
        Fold.unsafe("fold_a", Seq(2, 3, 4, 5), Seq(0, 1)),
        Fold.unsafe("fold_b", Seq(0, 1, 4, 5), Seq(2, 3)),
        Fold.unsafe("fold_c", Seq(0, 1, 2, 3), Seq(4, 5))
      ),
      samples = MvpaRReferenceFixtures.FeatureRsa.items.length
    )

  private def featureDesign(features: DoubleMatrix): FeatureModelDesign =
    FeatureModelDesign.unsafe(
      MvpaRReferenceFixtures.FeatureRsa.items,
      features,
      MvpaRReferenceFixtures.FeatureRsa.featureNames
    )

  private def featureResponse: Response =
    Response.categorical(Vector("a", "b", "c", "a", "b", "c")).toOption.get

  private def featurePlan(patterns: PatternMatrix): FeatureSetPlan =
    FeatureSetPlan
      .regional(
        "feature-property-region",
        Vector(FeatureSet.unsafe(RoiId(501), 0 until patterns.features, label = Some("all_patterns")))
      )
      .toOption
      .get

  private def runFeatureModel(
      patterns: PatternMatrix,
      design: FeatureModelDesign,
      direction: FeaturePredictionDirection
  ): (MetricVector, FeatureModelPrediction) =
    val result =
      MvpaEngine
        .run(
          patterns,
          featurePlan(patterns),
          featureResponse,
          FeatureModelAnalysis(
            design,
            direction,
            FeatureRidgeEstimator(MvpaRReferenceFixtures.FeatureRsa.lambda),
            storePrediction = true
          ),
          Some(featureFolds)
        )
        .toOption
        .get

    assertEquals(result.failures.length, 0)
    val success = result.successes.head
    (success.metrics, featurePrediction(success))

  private def transformColumns(matrix: DoubleMatrix, scales: Vector[Double], shifts: Vector[Double]): DoubleMatrix =
    require(scales.length == matrix.cols)
    require(shifts.length == matrix.cols)
    val out = matrix.copyData
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = matrix(row, col) * scales(col) + shifts(col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  private def xdecSource: PatternMatrix =
    patternMatrix(MvpaRReferenceFixtures.NaiveXdec.sourceRows)

  private def xdecTarget: PatternMatrix =
    patternMatrix(MvpaRReferenceFixtures.NaiveXdec.targetRows)

  private def xdecDesign: CrossDecodingDesign =
    CrossDecodingDesign.unsafe(
      MvpaRReferenceFixtures.NaiveXdec.sourceLabels,
      MvpaRReferenceFixtures.NaiveXdec.targetLabels
    )

  private def xdecSearchlightPlan: FeatureSetPlan =
    FeatureSetPlan
      .searchlight(
        "xdec-property-search",
        MvpaRReferenceFixtures.NaiveXdec.searchlightCases.map { expected =>
          FeatureSet.unsafe(RoiId(expected.roiId), expected.featureIndices, center = expected.featureIndices.headOption, label = Some(expected.label))
        }
      )
      .toOption
      .get

  test("naive cross-decoding scanner matches the reference engine over deterministic generated cases") {
    (0 until 12).foreach { caseIndex =>
      val (source, target, design, plan) = generatedCrossDecodingCase(caseIndex)
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

      assertEquivalentResults(reference, scanned)
    }
  }

  test("feature model encoding is invariant to affine source feature transforms") {
    val patterns = patternMatrix(MvpaRReferenceFixtures.FeatureRsa.patternRows)
    val original =
      runFeatureModel(
        patterns,
        featureDesign(MvpaRReferenceFixtures.FeatureRsa.featureRows),
        FeaturePredictionDirection.FeaturesToPatterns
      )
    val transformedFeatures =
      transformColumns(
        MvpaRReferenceFixtures.FeatureRsa.featureRows,
        scales = Vector(10.0, -2.0, 0.25),
        shifts = Vector(100.0, 0.5, -3.0)
      )
    val transformed =
      runFeatureModel(
        patterns,
        featureDesign(transformedFeatures),
        FeaturePredictionDirection.FeaturesToPatterns
      )

    assertMetricVectorEquals(transformed._1, original._1, Tolerance)
    assertMatrixEquals(transformed._2.predicted, original._2.predicted, Tolerance)
    assertMatrixEquals(transformed._2.observed, original._2.observed, Tolerance)
  }

  test("feature model decoding is invariant to affine source pattern transforms") {
    val patterns = patternMatrix(MvpaRReferenceFixtures.FeatureRsa.patternRows)
    val original =
      runFeatureModel(
        patterns,
        featureDesign(MvpaRReferenceFixtures.FeatureRsa.featureRows),
        FeaturePredictionDirection.PatternsToFeatures
      )
    val transformedPatterns =
      patternMatrix(
        transformColumns(
          MvpaRReferenceFixtures.FeatureRsa.patternRows,
          scales = Vector(2.0, -3.0, 0.5, 4.0),
          shifts = Vector(-10.0, 7.5, 0.25, 12.0)
        )
      )
    val transformed =
      runFeatureModel(
        transformedPatterns,
        featureDesign(MvpaRReferenceFixtures.FeatureRsa.featureRows),
        FeaturePredictionDirection.PatternsToFeatures
      )

    assertMetricVectorEquals(transformed._1, original._1, Tolerance)
    assertMatrixEquals(transformed._2.predicted, original._2.predicted, Tolerance)
    assertMatrixEquals(transformed._2.observed, original._2.observed, Tolerance)
  }

  test("naive cross-decoding scanner has a deterministic benchmark checksum") {
    var tick = 0L
    def clock(): Long =
      val value = tick
      tick += 250L
      value

    val results =
      MvpaBenchmarkHarness
        .run(
          Vector(
            MvpaBenchmarkCase(
              "naive_xdec_scanner",
              iterations = 3,
              () =>
                NaiveCrossDecodingScanner(storePredictions = true)
                  .run(xdecSource, xdecTarget, xdecSearchlightPlan, xdecDesign)
                  .map { result =>
                    result.successes.map { success =>
                      success.metrics("Accuracy").get + classificationPrediction(success).probabilities.dataArray.sum
                    }.sum
                  }
            )
          ),
          clock
        )
        .toOption
        .get

    assertEquals(results.map(_.name), Vector("naive_xdec_scanner"))
    assertEquals(results.head.iterations, 3)
    assertEquals(results.head.elapsedNanos, 250L)
    assertEqualsDouble(results.head.checksum, 63.0, Tolerance)
  }
