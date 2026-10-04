package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.SpaceRole
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*

class NativePredictiveLawsSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val items = Vector.tabulate(6)(i => s"item-$i")
  private val features = Vector(Vector(0.0,1.0), Vector(1.0,0.0), Vector(0.0,2.0), Vector(2.0,0.0), Vector(1.0,3.0), Vector(3.0,1.0))
  private val patterns = features.map(row => Vector(1 + 2 * row(0) + 0.5 * row(1), -2 - row(0) + 3 * row(1), 0.5 + row(0) - row(1)))
  private val axis = right(AxisRef.fromStableKeys("laws-items", SpaceRole.Samples, items, "item", "none", "one"))
  private val partitions = right(FixedPartitions.once(right(Labels.retained(IArray(0, 0, 1, 1, 2, 2)))))
  private val validation = right(ValidationDesign.bind(axis, partitions, ScientificSeed.fromLong(41)))
  private val budget = right(MaterializationBudget(10000))

  private def admitted(values: Vector[Vector[Double]]) =
    val mapping = right(NativeAxisMapping.fromAxis(axis, Vector.tabulate(6)(100L + _), DataFingerprint.external("native-laws")))
    right(AlderPredictiveAdmission.materialized(axis.descriptor, DMat.dense(6, values.head.length, values.flatten), DMat.dense(6, 1, Vector.fill(6)(0.0)), items, mapping, budget))
  private def featureResult(x: Vector[Vector[Double]], y: Vector[Vector[Double]], direction: FeaturePredictionDirection) =
    val design = right(FeatureModelDesign(items, DMat.dense(6, x.head.length, x.flatten), Vector.tabulate(x.head.length)(i => s"x$i")))
    right(AlderFeatureModel.crossValidate(admitted(y), design, direction, validation, Vector.tabulate(y.head.length)(i => s"p$i"), budget, FeatureRidgeEstimator(0.25), true)).prediction.get
  private def close(left: DMat, right: DMat, tolerance: Double = 1e-9): Unit =
    assertEquals(left.rows, right.rows); assertEquals(left.cols, right.cols)
    var r = 0
    while r < left.rows do
      var c = 0
      while c < left.cols do
        assertEqualsDouble(left(r,c), right(r,c), tolerance)
        c += 1
      r += 1

  test("native feature encoding and decoding obey source-affine laws"):
    val encode = featureResult(features, patterns, FeaturePredictionDirection.FeaturesToPatterns)
    val transformedFeatures = features.map(row => Vector(10 * row(0) + 100, -2 * row(1) + 0.5))
    val encodeAffine = featureResult(transformedFeatures, patterns, FeaturePredictionDirection.FeaturesToPatterns)
    close(encode.predicted, encodeAffine.predicted)
    val decode = featureResult(features, patterns, FeaturePredictionDirection.PatternsToFeatures)
    val transformedPatterns = patterns.map(row => Vector(2 * row(0) - 10, -3 * row(1) + 7, 0.5 * row(2) + 0.25))
    val decodeAffine = featureResult(features, transformedPatterns, FeaturePredictionDirection.PatternsToFeatures)
    close(decode.predicted, decodeAffine.predicted)

  test("native feature ridge remains finite for near-collinear high-dynamic-range rows"):
    val x = Vector.tabulate(6): row =>
      val value = (row - 2.5) * 1e6
      Vector(value, 2 * value + 0.001 * row, -0.5 * value + 0.0007 * (row % 2), 1e-6 * row)
    val y = x.zipWithIndex.map: (row, i) =>
      Vector(0.5 + 2e-6 * row(0) + 0.01 * math.sin(i), -1 - 3e-6 * row(0))
    val result = featureResult(x, y, FeaturePredictionDirection.FeaturesToPatterns)
    var r = 0
    while r < result.predicted.rows do
      var c = 0
      while c < result.predicted.cols do
        assert(result.predicted(r,c).isFinite)
        c += 1
      r += 1

  private def rows(name: String, featureKeys: Vector[String], values: Vector[Vector[Double]], targets: Vector[Double]) =
    val samples = right(AxisRef.fromStableKeys(s"$name-samples", SpaceRole.Samples, values.indices.map(i => s"$name-$i").toVector, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(samples, values.indices.map(i => name.hashCode.toLong + i).toVector, DataFingerprint.external(name)))
    right(AlderPredictiveAdmission.materialized(samples.descriptor, DMat.dense(values.length, featureKeys.length, values.flatten), DMat.dense(values.length, 1, targets), samples.toRecord.stableKeys, mapping, budget))
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "left", 1.0 -> "right")))

  test("selected non-finite feature columns fail while an independent selected table remains finite"):
    val design = right(FeatureModelDesign(items, DMat.dense(6, 2, features.flatten), Vector("x0", "x1")))
    val clean = AlderFeatureModel.crossValidate(admitted(patterns.map(_.take(2))), design, FeaturePredictionDirection.FeaturesToPatterns, validation, Vector("p0", "p1"), budget, FeatureRidgeEstimator(0.25), true)
    val poisoned = patterns.map(_.take(2)).updated(2, Vector(patterns(2)(0), Double.NaN))
    val bad = AlderFeatureModel.crossValidate(admitted(poisoned), design, FeaturePredictionDirection.FeaturesToPatterns, validation, Vector("p0", "p1"), budget, FeatureRidgeEstimator(0.25), true)
    assert(clean.isRight)
    assert(bad.isLeft)

  test("paired keyed feature permutations preserve cross-decoding probabilities"):
    val source = Vector(Vector(2.0,1.0,0.5), Vector(-2.0,-1.0,-0.5), Vector(-1.0,-2.0,0.25), Vector(1.0,2.0,-0.25))
    val target = Vector(Vector(1.5,1.0,0.4), Vector(-1.5,-1.0,-0.4))
    val labels = Vector(0.0,1.0,1.0,0.0)
    def run(keys: Vector[String], permutation: Vector[Int]) =
      val left = rows("paired-source" + keys.head, keys, source.map(row => permutation.map(row)), labels)
      val rightRows = rows("paired-target" + keys.head, keys, target.map(row => permutation.map(row)), Vector(0.0,1.0))
      val featureAxis = right(AxisRef.fromStableKeys("paired-features" + keys.head, SpaceRole.Observed, keys, "feature", "none", "one"))
      right(AlderCrossDecoding.correlationCentroid(left, rightRows, right(CrossDecodingFeatureBinding(featureAxis.descriptor, featureAxis.descriptor)), coding))
    val canonical = run(Vector("f0","f1","f2"), Vector(0,1,2))
    val permuted = run(Vector("f2","f0","f1"), Vector(2,0,1))
    close(canonical.probabilities, permuted.probabilities)

  test("a non-finite source is isolated from a separately admitted cross-domain evaluation"):
    val keys = Vector("f0", "f1")
    val target = rows("isolation-target", keys, Vector(Vector(1.0, 1.0), Vector(-1.0, -1.0)), Vector(0.0, 1.0))
    val featuresAxis = right(AxisRef.fromStableKeys("isolation-features", SpaceRole.Observed, keys, "feature", "none", "one"))
    val binding = right(CrossDecodingFeatureBinding(featuresAxis.descriptor, featuresAxis.descriptor))
    val clean = rows("isolation-clean", keys, Vector(Vector(2.0,1.0), Vector(-2.0,-1.0), Vector(-1.0,-2.0), Vector(1.0,2.0)), Vector(0.0,1.0,1.0,0.0))
    val poisoned = rows("isolation-poison", keys, Vector(Vector(Double.NaN,1.0), Vector(-2.0,-1.0), Vector(-1.0,-2.0), Vector(1.0,2.0)), Vector(0.0,1.0,1.0,0.0))
    assert(AlderCrossDecoding.correlationCentroid(clean, target, binding, coding).isRight)
    assert(AlderCrossDecoding.correlationCentroid(poisoned, target, binding, coding).isLeft)

  test("degenerate equal cross-domain prototypes yield a first-class equal probability tie"):
    val keys = Vector("f0", "f1")
    val source = rows("tie-source", keys, Vector.fill(4)(Vector(3.0,3.0)), Vector(0.0,1.0,0.0,1.0))
    val target = rows("tie-target", keys, Vector(Vector(9.0,9.0), Vector(-4.0,-4.0)), Vector(0.0,1.0))
    val featureAxis = right(AxisRef.fromStableKeys("tie-features", SpaceRole.Observed, keys, "feature", "none", "one"))
    val result = right(AlderCrossDecoding.correlationCentroid(source, target, right(CrossDecodingFeatureBinding(featureAxis.descriptor, featureAxis.descriptor)), coding))
    (0 until result.probabilities.rows).foreach: row =>
      assertEqualsDouble(result.probabilities(row, 0), 0.5, 1e-12)
      assertEqualsDouble(result.probabilities(row, 1), 0.5, 1e-12)
