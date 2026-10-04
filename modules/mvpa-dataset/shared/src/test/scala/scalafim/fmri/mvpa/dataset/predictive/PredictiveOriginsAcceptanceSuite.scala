package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Result-boundary oracle: inspect shared temporal/preparation support using
  * only the returned predictive result, without consulting an input registry.
  */
class PredictiveOriginsAcceptanceSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, role: SpaceRole, keys: Vector[String]) =
    right(AxisRef.fromStableKeys(name, role, keys, "fixture", "none", "one"))
  private val scans = axis("scans", SpaceRole.Samples, Vector.tabulate(12)(i => s"scan-$i"))
  private val neural = axis("neural", SpaceRole.Observed, Vector("f0", "f1"))
  private val targetAxis = axis("target", SpaceRole.Observed, Vector("label"))
  private val sourceId = SourceId.unsafe("shared-acquisition")
  private val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("shared-root"), sourceId)))
  private val raw = ValueIdentity.source(ValueId.unsafe("raw-acquisition-v1"))
  private val shared = ValueSupport.Bounded(scans.descriptor, raw, (0 until scans.size).toVector)
  private val joint = PreparationSupport.JointlyLearned(shared, shared)
  private val budget = right(MaterializationBudget(10000))
  private val x = Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0), Vector(3.0, 1.0), Vector(-3.0, -1.0))
  private val y = Vector(0.0, 1.0, 1.0, 0.0, 0.0, 1.0)

  private def rows(samples: AxisRef[String], ids: Vector[Long], values: Vector[Vector[Double]], labels: Vector[Double], start: Int): AlderMaterializedRows[String] =
    val valueId = ValueIdentity.source(ValueId.unsafe(s"prepared-${samples.descriptor.stableKey}"))
    val origins = right(EvidenceOrigins.make(source, valueId, AcquisitionCoordinates.OriginalTemporalAxis(scans.descriptor),
      ValueSupport.Bounded(scans.descriptor, raw, Vector.tabulate(samples.size)(_ + start)), joint, samples.descriptor))
    val observations = right(Observations.fromDense(samples, neural, DMat.dense(samples.size, 2, values.flatten), valueId, source, origins))
    val target = right(MultiResponse.fromDense(samples, targetAxis, DMat.dense(samples.size, 1, labels), ValueIdentity.source(ValueId.unsafe("labels-v1")), source))
    val mapping = right(NativeAxisMapping.fromAxis(samples, ids, DataFingerprint.external("declared-source-v1")))
    right(AlderPredictiveAdmission.nativeTables(observations, target, samples.toRecord.stableKeys, DataFingerprint.external("metadata-v1"), mapping,
      right(NativeReadPolicy(2, budget))))

  private def assertShared(receipt: NativeReadReceipt, output: AxisDescriptor): Unit =
    receipt.observationsIdentity.origins match
      case EvidenceOrigins.Bounded(value) =>
        assertEquals(value.acquisition, AcquisitionCoordinates.OriginalTemporalAxis(scans.descriptor))
        assertEquals(value.preparation, joint)
        assertEquals(value.outputAxis, output)
      case other => fail(s"shared origins disappeared from result: $other")

  test("both FeatureModel directions preserve inspectable jointly learned temporal support"):
    val samples = axis("items", SpaceRole.Samples, Vector.tabulate(6)(i => s"item-$i"))
    val admitted = rows(samples, Vector.tabulate(6)(_.toLong + 100), x, y, 0)
    val validation = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray(0, 0, 1, 1, 2, 2))))), ScientificSeed.fromLong(23)))
    val features = right(FeatureModelDesign(samples.toRecord.stableKeys, DMat.dense(6, 1, Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)), Vector("semantic")))
    FeaturePredictionDirection.values.foreach: direction =>
      val result = right(AlderFeatureModel.crossValidate(admitted, features, direction, validation, Vector("f0", "f1"), budget))
      assertShared(result.nativeRead.getOrElse(fail("native input receipt omitted")), samples.descriptor)
      assertEquals(result.nativeRead, admitted.nativeReadReceipt)

  test("all cross-domain heads retain both origins and make shared preparation visible"):
    val sourceSamples = axis("source-items", SpaceRole.Samples, Vector.tabulate(6)(i => s"source-$i"))
    val targetSamples = axis("target-items", SpaceRole.Samples, Vector("target-0", "target-1"))
    val train = rows(sourceSamples, Vector.tabulate(6)(_.toLong + 100), x, y, 0)
    val testRows = rows(targetSamples, Vector(200L, 201L), Vector(Vector(1.5, 1.0), Vector(-1.5, -1.0)), Vector(0.0, 1.0), 6)
    val features = right(CrossDecodingFeatureBinding(neural.descriptor, neural.descriptor))
    val coding = right(SwiftTargetCoding(Vector(0.0 -> "a", 1.0 -> "b")))
    val outputs = Vector(
      right(AlderCrossDecoding.correlationCentroid(train, testRows, features, coding)),
      right(AlderCrossDecoding.swiftCentroid(train, testRows, features, coding)),
      right(AlderCrossDecoding.ridgeLda(train, testRows, features, coding, right(RidgePenalty(0.5))))
    )
    outputs.foreach: result =>
      assertShared(result.sourceNativeRead.getOrElse(fail("source native receipt omitted")), sourceSamples.descriptor)
      assertShared(result.targetNativeRead.getOrElse(fail("target native receipt omitted")), targetSamples.descriptor)
      assertEquals(result.sourceNativeRead, train.nativeReadReceipt)
      assertEquals(result.targetNativeRead, testRows.nativeReadReceipt)
      assertEquals(result.evaluationReceipt.role, alder.kernel.EvaluationRole.Test)

  test("caller-materialized inputs expose receipt absence explicitly"):
    val samples = axis("materialized-items", SpaceRole.Samples, Vector.tabulate(6)(i => s"item-$i"))
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector.tabulate(6)(_.toLong + 100), DataFingerprint.external("materialized-v1")))
    val admitted = right(AlderPredictiveAdmission.materialized(samples.descriptor, DMat.dense(6, 2, x.flatten), DMat.dense(6, 1, y), samples.toRecord.stableKeys, mapping, budget))
    val validation = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray(0, 0, 1, 1, 2, 2))))), ScientificSeed.fromLong(23)))
    val features = right(FeatureModelDesign(samples.toRecord.stableKeys, DMat.dense(6, 1, Vector(0.0, 1.0, 2.0, 3.0, 4.0, 5.0)), Vector("semantic")))
    val result = right(AlderFeatureModel.crossValidate(admitted, features, FeaturePredictionDirection.PatternsToFeatures, validation, Vector("f0", "f1"), budget))
    assertEquals(result.nativeRead, None)
