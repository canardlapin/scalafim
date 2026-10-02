package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*

class AlderFeatureModelSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val items = Vector.tabulate(6)(i => s"item-$i")
  private val blocks = IArray(0, 0, 1, 1, 2, 2)
  private val featureRows = Vector(Vector(0.0, 1.0), Vector(1.0, 0.0), Vector(0.0, 2.0), Vector(2.0, 0.0), Vector(1.0, 3.0), Vector(3.0, 1.0))
  private val patternRows = featureRows.map(row => Vector(1.0 + 2.0 * row(0) + 0.5 * row(1), -2.0 - row(0) + 3.0 * row(1), 0.5 + row(0) - row(1)))
  private val featureDesign = right(FeatureModelDesign(items, DMat.dense(6, 2, featureRows.flatten), Vector("semantic", "visual")))
  private val sampleAxis = right(AxisRef.fromStableKeys("feature-items", SpaceRole.Samples, items, "item", "none", "one"))
  private val validation = right(ValidationDesign.bind(sampleAxis, right(FixedPartitions.once(right(Labels.retained(blocks)))), ScientificSeed.fromLong(23)))
  private val budget = right(MaterializationBudget(1000))
  private val names = Vector("pattern_0", "pattern_1", "pattern_2")
  private val estimator = FeatureRidgeEstimator(0.25)

  private def admitted(values: Vector[Vector[Double]]) =
    val mapping = right(NativeAxisMapping.fromAxis(sampleAxis, Vector.tabulate(6)(i => 100L + i), DataFingerprint.external("feature-fixture-v1")))
    right(AlderPredictiveAdmission.materialized(sampleAxis.descriptor, DMat.dense(6, values.head.length, values.flatten), DMat.dense(6, 1, Vector.fill(6)(0.0)), items, mapping, budget))
  private def payload(result: AlderFeatureModelResult): FeatureModelPrediction =
    result.prediction.getOrElse(fail("stored prediction omitted"))

  test("both orientations retain independent R multiresponse predictions, metrics and names"):
    // R base solve(crossprod(scale(X)) + 0.25 I, crossprod(scale(X), scale(Y)))
    // with training-only means/sample SD, restored target units and leave-block-out.
    val encode = Vector(1.96816319562991, 1.2458176852168, -0.450495049504951,
      3.39390576988733, -2.556162512803, 1.45049504950495,
      2.16057052493223, 3.7469892384786, -1.35328185328185,
      4.94049330485501, -3.66613817464881, 2.35328185328185,
      3.58359419464997, 5.03898690950484, -1.41235059760956,
      6.45212009106432, -2.61041548093341, 2.41235059760956)
    val decode = Vector(0.13996071892152, 1.16863882312333,
      1.11432592599795, 0.209791586654156, 0.0564323889703446, 1.98134370713711,
      1.98478692711536, 0.062524239701083, 0.777578504231281, 2.51590059710469,
      2.73023970101808, 0.594993341638386)
    FeaturePredictionDirection.values.foreach: direction =>
      val actual = right(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, direction, validation, names, budget, estimator, true))
      val (expected, observed, targetNames) = direction match
        case FeaturePredictionDirection.FeaturesToPatterns => (encode, patternRows, names)
        case FeaturePredictionDirection.PatternsToFeatures => (decode, featureRows, featureDesign.featureNames)
      val prediction = payload(actual)
      assertEquals(prediction.items, items)
      assertEquals(prediction.targetNames, targetNames)
      var row = 0
      while row < prediction.predicted.rows do
        var column = 0
        while column < prediction.predicted.cols do
          assertEqualsDouble(prediction.predicted(row, column), expected(row * targetNames.length + column), 1e-12)
          assertEqualsDouble(prediction.observed(row, column), observed(row)(column), 1e-12)
          column += 1
        row += 1
      val squaredError = expected.zip(observed.flatten).map((a, b) => (a - b) * (a - b)).sum / expected.length
      assertEqualsDouble(actual.metrics.mse, squaredError, 1e-12)
      assertEquals(actual.metrics.observations, 6)
      assertEquals(actual.metrics.targetColumns, targetNames.length)
      assertEquals(actual.penalty, estimator.penalty)
      assertEquals(actual.fits.length, 3)
      assertEquals(actual.fits.map(_.audit.component.id.render), Vector.fill(3)("scalafim.feature-model"))
      val omitted = right(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, direction, validation, names, budget, estimator, false))
      assertEquals(omitted.prediction, None)

  test("fold coefficients agree with an independent scalar standardized-ridge oracle and ignore held-out targets"):
    val x = Vector.tabulate(6)(_.toDouble)
    val design = right(FeatureModelDesign(items, DMat.dense(6, 1, x), Vector("linear")))
    val y = x.map(value => Vector(2.0 + 3.0 * value))
    val baseline = right(AlderFeatureModel.crossValidate(admitted(y), design, FeaturePredictionDirection.FeaturesToPatterns, validation, Vector("pattern_0"), budget, FeatureRidgeEstimator(1.0), true))
    val changed = right(AlderFeatureModel.crossValidate(admitted(y.updated(0, Vector(1e8)).updated(1, Vector(-1e8))), design, FeaturePredictionDirection.FeaturesToPatterns, validation, Vector("pattern_0"), budget, FeatureRidgeEstimator(1.0), true))
    // Four training rows have sample-standardized sum of squares n-1=3.0
    assertEqualsDouble(baseline.fits.head.coefficients(0, 0), 3.0 / 4.0, 1e-12)
    assertEqualsDouble(changed.fits.head.coefficients(0, 0), baseline.fits.head.coefficients(0, 0), 1e-12)
    assertEquals(changed.fits.head.targetMeans, baseline.fits.head.targetMeans)
    assertEquals(changed.fits.head.trainingStableKeys, Vector("item-2", "item-3", "item-4", "item-5"))

  test("wrong item order, feature names and insufficient projection budget fail admission"):
    val reordered = right(FeatureModelDesign(items.reverse, featureDesign.features, featureDesign.featureNames))
    assert(AlderFeatureModel.crossValidate(admitted(patternRows), reordered, FeaturePredictionDirection.PatternsToFeatures, validation, names, budget).isLeft)
    assert(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, FeaturePredictionDirection.PatternsToFeatures, validation, names.updated(1, names.head), budget).isLeft)
    assert(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, FeaturePredictionDirection.PatternsToFeatures, validation, names, right(MaterializationBudget(1))).isLeft)

  test("repeated exact validation averages each item across declared assessments"):
    val first = right(Labels.retained(blocks))
    val second = right(Labels.retained(IArray(0, 1, 2, 0, 1, 2)))
    val repeated = right(ValidationDesign.bind(sampleAxis, right(FixedPartitions.repeated(IArray(first, second))), ScientificSeed.fromLong(23)))
    val alternate = right(ValidationDesign.bind(sampleAxis, right(FixedPartitions.once(second)), ScientificSeed.fromLong(23)))
    val a = payload(right(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, FeaturePredictionDirection.FeaturesToPatterns, validation, names, budget, estimator, true)))
    val b = payload(right(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, FeaturePredictionDirection.FeaturesToPatterns, alternate, names, budget, estimator, true)))
    val combined = right(AlderFeatureModel.crossValidate(admitted(patternRows), featureDesign, FeaturePredictionDirection.FeaturesToPatterns, repeated, names, budget, estimator, true))
    val mean = payload(combined)
    assertEquals(combined.fits.length, 6)
    (0 until mean.predicted.rows).foreach: row =>
      (0 until mean.predicted.cols).foreach: column =>
        assertEqualsDouble(mean.predicted(row, column), (a.predicted(row, column) + b.predicted(row, column)) / 2.0, 1e-12)
