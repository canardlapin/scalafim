package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.SpaceRole
import scalafim.fmri.mvpa.*

class AlderCrossDecodingSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "left", 1.0 -> "right")))
  private val features = right(AxisRef.fromStableKeys("features", SpaceRole.Observed, Vector("f0", "f1"), "feature", "none", "one"))
  private val binding = right(CrossDecodingFeatureBinding(features.descriptor, features.descriptor))

  private def rows(axisName: String, keys: Vector[String], input: Vector[Vector[Double]], target: Vector[Double]): AlderMaterializedRows[String] =
    val axis = right(AxisRef.fromStableKeys(axisName, SpaceRole.Samples, keys, "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(axis, keys.indices.map(i => axisName.hashCode.toLong + i).toVector, DataFingerprint.external(s"$axisName-source")))
    right(AlderPredictiveAdmission.materialized(axis.descriptor, DMat.dense(input.length, input.head.length, input.flatten),
      DMat.dense(target.length, 1, target), keys, mapping, right(MaterializationBudget(1000L))))

  test("cross-domain correlation fits only source rows and refuses same-axis or foreign preparation scopes"):
    val source = rows("source", Vector("s0", "s1", "s2", "s3"), Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0)), Vector(0.0, 1.0, 1.0, 0.0))
    val target = rows("target", Vector("t0", "t1"), Vector(Vector(1.5, 1.0), Vector(-1.5, -1.0)), Vector(0.0, 1.0))
    val result = right(AlderCrossDecoding.correlationCentroid(source, target, binding, coding,
      CrossDecodingPreparationScope.SourceOnly(source.mapping.axis, source.root.fingerprint)))
    assertEquals(result.sourceAxis, source.mapping.axis)
    assertEquals(result.targetAxis, target.mapping.axis)
    assertEquals(result.rows.map(_.stableKey), Vector("t0", "t1"))
    assertEquals(result.evaluationReceipt.role, alder.kernel.EvaluationRole.Test)
    assertEquals(result.evaluationReceipt.priorSelection, None)
    assert(AlderCrossDecoding.correlationCentroid(source, target, binding, coding,
      CrossDecodingPreparationScope.SourceOnly(target.mapping.axis, DataFingerprint.external("foreign"))).isLeft)
    assert(AlderCrossDecoding.correlationCentroid(source, target, binding, coding,
      CrossDecodingPreparationScope.SourceOnly(source.mapping.axis, DataFingerprint.external("wrong-source-receipt"))).isLeft)
    assert(AlderCrossDecoding.correlationCentroid(source, source, binding, coding).isLeft)
    val foreign = right(AxisRef.fromStableKeys("foreign-features", SpaceRole.Observed, Vector("g0", "g1"), "feature", "none", "one"))
    assert(CrossDecodingFeatureBinding(features.descriptor, foreign.descriptor).isLeft)

  test("target changes do not alter source fit evidence"):
    val source = rows("source", Vector("s0", "s1", "s2", "s3"), Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0)), Vector(0.0, 1.0, 1.0, 0.0))
    val target = rows("target", Vector("t0", "t1"), Vector(Vector(1.5, 1.0), Vector(-1.5, -1.0)), Vector(0.0, 1.0))
    val perturbed = rows("target", Vector("t0", "t1"), Vector(Vector(1.5, 1.0), Vector(-150.0, 100.0)), Vector(1.0, 0.0))
    val baseline = right(AlderCrossDecoding.correlationCentroid(source, target, binding, coding))
    val changed = right(AlderCrossDecoding.correlationCentroid(source, perturbed, binding, coding))
    assertEquals(baseline.sourceAudit.data.digest, changed.sourceAudit.data.digest)
    assertEquals(baseline.sourceAudit.data.policy, changed.sourceAudit.data.policy)
    coding.classes.indices.foreach: column =>
      assertEqualsDouble(baseline.probabilities(0, column), changed.probabilities(0, column), 1e-12)

  test("Swift and ridge LDA cross-domain heads retain source-only numerical predictions"):
    val x = Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0))
    val y = Vector(0.0, 1.0, 1.0, 0.0)
    val z = Vector(Vector(1.5, 1.0), Vector(-1.5, -1.0))
    val source = rows("source", Vector("s0", "s1", "s2", "s3"), x, y)
    val target = rows("target", Vector("t0", "t1"), z, Vector(0.0, 1.0))
    val heads = Vector(
      (SwiftCentroidClassifier(): Classifier, right(AlderCrossDecoding.swiftCentroid(source, target, binding, coding))),
      (RidgeLdaClassifier(0.5): Classifier, right(AlderCrossDecoding.ridgeLda(source, target, binding, coding, right(RidgePenalty(0.5)))))
    )
    heads.foreach: (classifier, result) =>
      val model = right(classifier.fit(PatternMatrix.fromRows(x), Response.Categorical(y.map(value => coding.label(value).get))))
      val expected = right(Classification.reorderProbabilities(right(model.predict(PatternMatrix.fromRows(z))), coding.classes))
      (0 until z.length).foreach: row =>
        coding.classes.indices.foreach: column =>
          assertEqualsDouble(result.probabilities(row, column), expected(row, column), 1e-12)

  test("native input axes cannot be overridden by an equal-width feature declaration"):
    val sample = right(AxisRef.fromStableKeys("native-source", SpaceRole.Samples, Vector("s0", "s1", "s2", "s3"), "trial", "none", "one"))
    val nativeFeatures = right(AxisRef.fromStableKeys("native-foreign", SpaceRole.Observed, Vector("f1", "f0"), "feature", "none", "one"))
    val response = right(AxisRef.fromStableKeys("native-response", SpaceRole.Observed, Vector("label"), "target", "none", "one"))
    val id = scalafim.response.SourceId.unsafe("native-source")
    val origin = right(EvidenceSource(id, scalafim.response.Provenance.source(scalafim.response.ProvenanceId.unsafe("native-source-root"), id)))
    val observations = right(Observations.fromDense(sample, nativeFeatures, DMat.dense(4, 2, Vector(2.0, 1.0, -2.0, -1.0, -1.0, -2.0, 1.0, 2.0)), multivar.core.ValueIdentity.source(multivar.core.ValueId.unsafe("native-patterns")), origin))
    val targets = right(MultiResponse.fromDense(sample, response, DMat.dense(4, 1, Vector(0.0, 1.0, 1.0, 0.0)), multivar.core.ValueIdentity.source(multivar.core.ValueId.unsafe("native-labels")), origin))
    val mapping = right(NativeAxisMapping.fromAxis(sample, Vector(200L, 201L, 202L, 203L), DataFingerprint.external("native-revision")))
    val source = right(AlderPredictiveAdmission.nativeTables(observations, targets, sample.toRecord.stableKeys, DataFingerprint.external("metadata"), mapping, right(NativeReadPolicy(2, right(MaterializationBudget(1000))))))
    val target = rows("target", Vector("t0", "t1"), Vector(Vector(1.5, 1.0), Vector(-1.5, -1.0)), Vector(0.0, 1.0))
    assert(AlderCrossDecoding.correlationCentroid(source, target, binding, coding).isLeft)
