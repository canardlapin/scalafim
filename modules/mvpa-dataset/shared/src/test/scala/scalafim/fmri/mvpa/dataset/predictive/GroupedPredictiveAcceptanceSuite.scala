package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{DigestAlgorithm, Draw, IndexSpace}
import scalafim.fmri.mvpa.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class GroupedPredictiveAcceptanceSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def valueId(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private val x = Vector(Vector(2.0, 1.0), Vector(-2.0, -1.0), Vector(-1.0, -2.0), Vector(1.0, 2.0), Vector(3.0, 1.0), Vector(-3.0, -1.0), Vector(2.0, 3.0))
  private val labels = Vector(0.0, 1.0, 1.0, 0.0, 0.0, 1.0, 0.0)
  private val runs = Vector("run-a", "run-a", "run-b", "run-b", "run-c", "run-c", "run-c")
  private val coding = right(SwiftTargetCoding(Vector(0.0 -> "a", 1.0 -> "b")))
  private val budget = right(MaterializationBudget(10000))
  private val sourceId = SourceId.unsafe("grouped-fixture")
  private val source = right(EvidenceSource(sourceId, Provenance.source(ProvenanceId.unsafe("grouped-root"), sourceId)))
  private val neural = right(AxisRef.fromStableKeys("grouped-neural", SpaceRole.Observed, Vector("v0", "v1"), "voxel", "psc", "raw"))
  private val response = right(AxisRef.fromStableKeys("grouped-target", SpaceRole.Observed, Vector("class"), "target", "none", "one"))

  private def evaluate(order: Vector[Int]): AlderSwiftCentroidResult =
    val samples = right(AxisRef.fromStableKeys("grouped-samples", SpaceRole.Samples, order.map(i => s"trial-$i"), "trial", "none", "one"))
    val groups = right(Column.fromValues(samples, order.map(runs), valueId("groups")))
    val validation = right(LeaveOneGroupOutDesign.bind(samples, groups, ScientificSeed.fromLong(23))(identity)).validation
    val observations = right(Observations.fromDense(samples, neural, DMat.dense(order.size, 2, order.flatMap(x)), valueId("patterns"), source))
    val targets = right(MultiResponse.fromDense(samples, response, DMat.dense(order.size, 1, order.map(labels)), valueId("labels"), source))
    val mapping = right(NativeAxisMapping.fromAxis(samples, order.map(i => 100L + i), DataFingerprint.external("grouped-v1")))
    val admitted = right(AlderPredictiveAdmission.nativeTables(observations, targets, samples.toRecord.stableKeys, DataFingerprint.external("metadata-v1"), mapping, right(NativeReadPolicy(2, budget))))
    right(AlderSwiftCentroid.crossValidate(admitted, validation, coding))

  test("identified unequal run groups retain keyed predictions after joint sample/target/group permutation"):
    val base = evaluate(x.indices.toVector)
    val reordered = evaluate(Vector(6, 2, 0, 5, 3, 1, 4))
    val byKey = base.rows.map(row => row.stableKey -> row.probabilities).toMap
    reordered.rows.foreach: row =>
      row.probabilities.indices.foreach: column =>
        assertEqualsDouble(row.probabilities(column), byKey(row.stableKey)(column), 1e-12)
      val heldRun = runs(row.stableKey.stripPrefix("trial-").toInt)
      assert(row.trainingStableKeys.forall(key => runs(key.stripPrefix("trial-").toInt) != heldRun))
    assertEquals(base.assessment.samples, 7L)
    assertEquals(reordered.assessment.correct, base.assessment.correct)
    assertEqualsDouble(reordered.assessment.accuracy, base.assessment.correct.toDouble / 7, 1e-12)

  test("foreign grouping declaration refuses before the numerical source can be read"):
    val samples = right(AxisRef.fromStableKeys("bound-samples", SpaceRole.Samples, Vector("a", "b"), "trial", "none", "one"))
    val foreign = right(AxisRef.fromStableKeys("foreign-samples", SpaceRole.Samples, Vector("a", "b"), "trial", "none", "one"))
    val groups = right(Column.fromValues(foreign, Vector("run-a", "run-b"), valueId("foreign-groups")))
    var reads = 0
    val poison = new DoubleLinearOperator:
      val rows = 2
      val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        throw new IllegalStateException("foreign grouping reached numerical evidence")
    val observations = right(Observations.fromOperator(samples, neural, poison, valueId("poison"), source))
    val refusal = Column.decode(samples, groups.toRecord).flatMap(group => LeaveOneGroupOutDesign.bind(samples, group, ScientificSeed.fromLong(23))(identity))
    assert(refusal.isLeft)
    assertEquals(observations.rows, 2)
    assertEquals(reads, 0)

  test("nested repeated draws reject reused native ids and preserve occurrence keys and cluster-held-out prediction"):
    val samples = right(AxisRef.fromStableKeys("draw-samples", SpaceRole.Samples, x.indices.map(i => s"trial-$i").toVector, "trial", "none", "one"))
    val first = right(ReindexingLeg.bind(samples, right(Draw.from(IArray(4, 1, 4, 3, 0, 3, 2, 5), right(IndexSpace.of(7))))))
    val second = right(first.continue(right(Draw.from(IArray(2, 1, 0, 3, 4, 6, 7, 5), right(IndexSpace.of(8))))))
    val composed = right(first.andThen(second))
    val expectedRoots = Vector(4, 1, 4, 3, 0, 2, 5, 3)
    assertEquals(composed.ordinals.toVector, expectedRoots)
    val observations = right(Observations.fromDense(samples, neural, DMat.dense(7, 2, x.flatten), valueId("draw-patterns"), source)).reindex(composed)
    val targets = right(MultiResponse.fromDense(samples, response, DMat.dense(7, 1, labels), valueId("draw-labels"), source)).reindex(composed)
    val groups = right(Column.fromValues(samples, runs, valueId("draw-groups"))).reindex(composed)
    val keys = composed.child.toRecord.stableKeys
    assertEquals(keys.distinct.size, expectedRoots.size)
    val duplicate = NativeAxisMapping.fromAxis(composed.child, expectedRoots.map(i => 100L + i), DataFingerprint.external("draw-v1"))
    assert(duplicate match
      case Left(AlderPredictiveAdmissionError.DuplicateNativeId(_)) => true
      case _ => false)
    val mapping = right(NativeAxisMapping.fromAxis(composed.child, expectedRoots.indices.map(i => 1000L + i).toVector, DataFingerprint.external("draw-v1")))
    val rows = right(AlderPredictiveAdmission.nativeTables(observations, targets, keys, DataFingerprint.external("draw-metadata"), mapping, right(NativeReadPolicy(2, budget))))
    val values = right(rows.root.training(mapping.nativeIds)).data.foldRows(Vector.empty[(Vector[Double], Vector[Double])])((out, _, row) => out :+ (row.input.toVector -> row.target.toVector))
    assertEquals(values, expectedRoots.map(i => x(i) -> Vector(labels(i))))
    val validation = right(LeaveOneGroupOutDesign.bind(composed.child, groups, ScientificSeed.fromLong(23))(identity)).validation
    val result = right(AlderSwiftCentroid.crossValidate(rows, validation, coding))
    assertEquals(result.rows.map(_.stableKey), keys)
    assertEquals(result.assessment.samples, 8L)
    val runByOccurrence = keys.zip(expectedRoots.map(runs)).toMap
    result.rows.foreach: row =>
      assert(row.trainingStableKeys.forall(key => runByOccurrence(key) != runByOccurrence(row.stableKey)))
