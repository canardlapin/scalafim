package scalafim.fmri.mvpa

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.{IndexSpace, Selection}
import scalafim.response.{DomainReference, Provenance, ProvenanceEvidence, ProvenanceId, SourceId}

class EvidenceOriginsSuite extends munit.FunSuite:
  private def right[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), result => result)

  private def axis(name: String, keys: Vector[String], role: SpaceRole = SpaceRole.Samples): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, keys, "fixture", "unit", "raw", Vector("fixture:v1")))

  private def source(name: String): EvidenceSource =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))

  private def identity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private final class Poison extends DoubleLinearOperator:
    val rows = 4
    val cols = 2
    var callbacks = 0
    def applyTo(input: DVec, output: MutableDVec): Unit =
      callbacks += 1
      throw IllegalStateException("poison callback")

  test("bounded direct support is tied to the original temporal axis"):
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val output = axis("trials", Vector("a", "b"))
    val raw = identity("raw-bold")
    val direct = ValueSupport.Bounded(temporal.descriptor, raw, Vector(0, 2))
    val origins = right(EvidenceOrigins.make(
      source("bold"), raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor), direct,
      PreparationSupport.FixedShared(ValueSupport.Unknown), output.descriptor
    ))
    assertEquals(origins.outputAssociation, Some(output.descriptor))

    val reordered = axis("scan", Vector("t1", "t0", "t2", "t3"))
    assert(EvidenceOrigins.make(
      source("bold"), raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(reordered.descriptor, raw, Vector(0, 2)), PreparationSupport.Unknown, output.descriptor
    ).isLeft)

    val fixed = right(EvidenceOrigins.make(
      source("bold"), raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor), direct,
      PreparationSupport.FixedShared(ValueSupport.Unknown), output.descriptor
    ))
    val jointlyLearned = right(EvidenceOrigins.make(
      source("bold"), raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor), direct,
      PreparationSupport.JointlyLearned(ValueSupport.Unknown, ValueSupport.Unknown), output.descriptor
    ))
    assert(fixed != jointlyLearned)

  test("reindexing changes output association without narrowing upstream support"):
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val samples = axis("trials", Vector("a", "b", "c", "d"))
    val neural = axis("voxels", Vector("v0", "v1"), SpaceRole.Observed)
    val raw = identity("raw-bold")
    val direct = ValueSupport.Bounded(temporal.descriptor, raw, Vector(0, 2))
    val globallyLearned = ValueSupport.Bounded(temporal.descriptor, identity("global-fit"), Vector(0, 1, 2, 3))
    val origins = right(EvidenceOrigins.make(
      source("bold"), raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor), direct,
      PreparationSupport.JointlyLearned(globallyLearned, ValueSupport.Unknown), samples.descriptor
    ))
    val observations = right(Observations.fromDense(samples, neural, DMat.dense(4, 2, Vector.fill(8)(0.0)), raw, source("bold"), origins))
    val leg = right(ReindexingLeg.bind(samples, right(Selection.from(IArray(1, 3), right(IndexSpace.of(4))))))
    val restricted = observations.reindex(leg)
    assertEquals(restricted.origins.outputAssociation, Some(leg.child.descriptor))
    assertEquals(restricted.origins, origins.reindexOutput(leg.child.descriptor, restricted.patterns.valueIdentity))
    assert(restricted.origins != EvidenceOrigins.Unknown)
    assert(restricted.identity != observations.identity)

  test("metadata-only origin validation does not call a matrix-free source"):
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val samples = axis("trials", Vector("a", "b", "c", "d"))
    val neural = axis("voxels", Vector("v0", "v1"), SpaceRole.Observed)
    val raw = identity("raw-bold")
    val evidence = source("bold")
    val origins = right(EvidenceOrigins.make(
      evidence, raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, raw, Vector(0, 1, 2, 3)), PreparationSupport.Unknown, samples.descriptor
    ))
    val poison = new Poison
    right(Observations.fromOperator(samples, neural, poison, raw, evidence, origins))
    assertEquals(poison.callbacks, 0)

  test("evidence records retain declared support through validated reconstruction"):
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val samples = axis("trials", Vector("a", "b", "c", "d"))
    val neural = axis("voxels", Vector("v0", "v1"), SpaceRole.Observed)
    val raw = identity("raw-bold")
    val evidence = source("bold")
    val origins = right(EvidenceOrigins.make(
      evidence, raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, raw, Vector(0, 2)),
      PreparationSupport.JointlyLearned(ValueSupport.Unknown, ValueSupport.Unknown), samples.descriptor
    ))
    val values = DMat.dense(4, 2, Vector.fill(8)(1.0))
    val observations = right(Observations.fromDense(samples, neural, values, raw, evidence, origins))
    val rebuilt = right(Observations.decode(samples, neural, observations.toRecord, values))
    assertEquals(rebuilt.origins, origins)
    assertEquals(rebuilt.identity, observations.identity)

  test("reindexed evidence records bind the reindexed output value identity"):
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val samples = axis("trials", Vector("a", "b", "c", "d"))
    val neural = axis("voxels", Vector("v0", "v1"), SpaceRole.Observed)
    val raw = identity("raw-bold")
    val evidence = source("bold")
    val origins = right(EvidenceOrigins.make(
      evidence, raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, raw, Vector(0, 1, 2, 3)), PreparationSupport.Unknown, samples.descriptor
    ))
    val observations = right(Observations.fromDense(samples, neural, DMat.dense(4, 2, Vector.fill(8)(1.0)), raw, evidence, origins))
    val leg = right(ReindexingLeg.bind(samples, right(Selection.from(IArray(1, 3), right(IndexSpace.of(4))))))
    val reindexed = observations.reindex(leg)
    val rebuilt = right(Observations.decode(leg.child, neural, reindexed.toRecord, DMat.dense(2, 2, Vector.fill(4)(1.0))))
    assertEquals(rebuilt.origins, reindexed.origins)
    assertEquals(rebuilt.identity, reindexed.identity)

  test("structurally reconstructed source equality and provenance evidence affect origin identity"):
    val temporal = axis("scan", Vector("t0", "t1", "t2", "t3"))
    val output = axis("trials", Vector("a", "b"))
    val raw = identity("raw-bold")
    val first = source("bold")
    val reconstructed = source("bold")
    assertEquals(first, reconstructed)
    val base = right(EvidenceOrigins.make(
      first, raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, raw, Vector(0)), PreparationSupport.Unknown, output.descriptor
    ))
    val rebuilt = right(EvidenceOrigins.make(
      reconstructed, raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, raw, Vector(0)), PreparationSupport.Unknown, output.descriptor
    ))
    assertEquals(base, rebuilt)
    assertEquals(base.identityDigest, rebuilt.identityDigest)
    val id = SourceId.unsafe("bold")
    val withEvidence = right(EvidenceSource(id, Provenance.source(
      ProvenanceId.unsafe("bold-root"), id, Vector(ProvenanceEvidence.Domain(DomainReference.unsafe("bids", "run-1")))
    )))
    val changedEvidence = right(EvidenceOrigins.make(
      withEvidence, raw, AcquisitionCoordinates.OriginalTemporalAxis(temporal.descriptor),
      ValueSupport.Bounded(temporal.descriptor, raw, Vector(0)), PreparationSupport.Unknown, output.descriptor
    ))
    assert(base != changedEvidence)
    assert(base.identityDigest != changedEvidence.identityDigest)
