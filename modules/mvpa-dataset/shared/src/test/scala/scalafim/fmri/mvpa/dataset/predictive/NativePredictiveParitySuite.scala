package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*

/** Frozen independent R expectations survive deletion of the old executors. */
class NativePredictiveParitySuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val budget = right(MaterializationBudget(100000L))
  private def axis(name: String, keys: Vector[String]) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Samples, keys, "trial", "none", "one"))
  private def admitted(name: String, keys: Vector[String], x: DMat, y: Vector[Double]) =
    val samples = axis(name, keys)
    val mapping = right(NativeAxisMapping.fromAxis(samples, keys.indices.map(i => name.hashCode.toLong * 1000L + i).toVector, DataFingerprint.external(s"$name-fixture")))
    right(AlderPredictiveAdmission.materialized(samples.descriptor, x, DMat.dense(y.length, 1, y), keys, mapping, budget))
  private def select(matrix: DMat, columns: Vector[Int]): DMat =
    DMat.dense(matrix.rows, columns.length, Vector.tabulate(matrix.rows * columns.length)(i => matrix(i / columns.length, columns(i % columns.length))))
  private def matrixEquals(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    (0 until actual.rows).foreach: row =>
      (0 until actual.cols).foreach: column =>
        assertEqualsDouble(actual(row, column), expected(row, column), 1e-10)
  private def metricEquals(actual: MetricVector, expected: MetricVector): Unit =
    assertEquals(actual.names, expected.names)
    actual.names.foreach: name =>
      // Existing R fixture has exact tied decode RDM distances. Preserve its
      // declared rank-score caveat rather than changing the numerical fixture.
      val tolerance = if name == "RdmCorrelation" then 5e-3 else 1e-10
      assertEqualsDouble(actual(name).get, expected(name).get, tolerance)

  test("Swift R fixture retains unequal fold sizes, class columns and pooled sample accuracy"):
    val fixture = MvpaMigrationParityFixtures.SwiftCrossValidation
    val keys = fixture.labels.indices.map(i => s"item-$i").toVector
    val samples = axis("swift-parity", keys)
    val validation = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray.from(fixture.blocks))))), ScientificSeed.fromLong(23)))
    val classNames = fixture.labels.distinct
    val coding = right(SwiftTargetCoding(classNames.zipWithIndex.map((label, index) => index.toDouble -> label)))
    val y = fixture.labels.map(label => classNames.indexOf(label).toDouble)
    val result = right(AlderSwiftCentroid.crossValidate(admitted("swift-parity", keys, fixture.patterns.value, y), validation, coding))
    matrixEquals(result.probabilities, fixture.probabilities)
    assertEquals(result.classes.map(_.value), fixture.classes)
    assertEquals(result.rows.map(_.stableKey), keys)
    assertEquals(result.rows.map(_.predicted.value), fixture.predicted)
    assertEquals(result.assessment.correct, fixture.correct.toLong)
    assertEqualsDouble(result.assessment.accuracy, fixture.pooledAccuracy, 1e-12)
    assert(math.abs(result.assessment.accuracy - fixture.unweightedMeanFoldAccuracy) > 0.05)
    assertEquals(result.fits.map(_.trainingStableKeys.length), Vector(7, 6, 5))

  test("feature encoding and decoding retain every frozen R regional metric and prediction"):
    val fixture = MvpaRReferenceFixtures.FeatureRsa
    val samples = axis("feature-parity", fixture.items)
    val validation = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray(0, 0, 1, 1, 2, 2))))), ScientificSeed.fromLong(23)))
    val rows = admitted("feature-parity", fixture.items, fixture.patternRows, Vector.fill(6)(0.0))
    val features = right(FeatureModelDesign(fixture.items, fixture.featureRows, fixture.featureNames))
    val names = Vector.tabulate(fixture.patternRows.cols)(i => s"pattern_$i")
    FeaturePredictionDirection.values.foreach: direction =>
      val result = right(AlderFeatureModel.crossValidate(rows, features, direction, validation, names, budget, FeatureRidgeEstimator(fixture.lambda), true))
      val (expected, observed, metrics) = direction match
        case FeaturePredictionDirection.FeaturesToPatterns => (fixture.EncodeRegional.predicted, fixture.EncodeRegional.observed, fixture.EncodeRegional.metrics)
        case FeaturePredictionDirection.PatternsToFeatures => (fixture.DecodeRegional.predicted, fixture.DecodeRegional.observed, fixture.DecodeRegional.metrics)
      val prediction = result.prediction.getOrElse(fail("prediction missing"))
      matrixEquals(prediction.predicted, expected)
      matrixEquals(prediction.observed, observed)
      metricEquals(result.metrics.withEstimator(result.penalty.value), metrics)
      assertEquals(prediction.items, fixture.items)

  test("selected neural columns retain frozen R searchlight feature predictions and names"):
    val fixture = MvpaRReferenceFixtures.FeatureRsa
    val samples = axis("feature-selected", fixture.items)
    val validation = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray(0, 0, 1, 1, 2, 2))))), ScientificSeed.fromLong(23)))
    val rows = admitted("feature-selected", fixture.items, select(fixture.patternRows, fixture.searchlightPatternIndices), Vector.fill(6)(0.0))
    val features = right(FeatureModelDesign(fixture.items, fixture.featureRows, fixture.featureNames))
    val names = fixture.searchlightPatternIndices.map(i => s"pattern_$i")
    val result = right(AlderFeatureModel.crossValidate(rows, features, FeaturePredictionDirection.FeaturesToPatterns, validation, names, budget, FeatureRidgeEstimator(fixture.lambda), true))
    val prediction = result.prediction.getOrElse(fail("prediction missing"))
    assertEquals(prediction.targetNames, names)
    matrixEquals(prediction.predicted, fixture.EncodeSearchlight.predicted)
    matrixEquals(prediction.observed, fixture.EncodeSearchlight.observed)
    metricEquals(result.metrics.withEstimator(result.penalty.value), fixture.EncodeSearchlight.metrics)

  test("identified cross-decoding preserves all frozen R regional and searchlight probability tables"):
    val fixture = MvpaRReferenceFixtures.NaiveXdec
    val classes = fixture.sourceLabels.distinct
    val coding = right(SwiftTargetCoding(classes.zipWithIndex.map((label, index) => index.toDouble -> label)))
    (fixture.regionalCases ++ fixture.searchlightCases).foreach: expected =>
      val source = admitted(s"source-${expected.roiId}", fixture.sourceLabels.indices.map(i => s"s$i").toVector, select(fixture.sourceRows, expected.featureIndices), fixture.sourceLabels.map(label => classes.indexOf(label).toDouble))
      val target = admitted(s"target-${expected.roiId}", fixture.targetLabels.indices.map(i => s"t$i").toVector, select(fixture.targetRows, expected.featureIndices), fixture.targetLabels.map(label => classes.indexOf(label).toDouble))
      val features = right(AxisRef.fromStableKeys(s"features-${expected.roiId}", SpaceRole.Observed, expected.featureIndices.map(i => s"f$i"), "feature", "none", "one"))
      val binding = right(CrossDecodingFeatureBinding(features.descriptor, features.descriptor))
      val result = right(AlderCrossDecoding.correlationCentroid(source, target, binding, coding))
      matrixEquals(result.probabilities, expected.expectedProbabilities)
      assertEquals(result.classes.map(_.value), expected.expectedClasses)
      assertEquals(result.rows.map(_.predicted.value), expected.expectedPredicted)
      assertEqualsDouble(result.rows.count(row => row.predicted == row.observed).toDouble / result.rows.length, expected.expectedAccuracy, 1e-12)
      assertEquals(result.rows.map(_.stableKey), target.mapping.entriesByOrdinal.map(_.stableKey))
