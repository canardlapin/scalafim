package scalafim.fmri.mvpa.dataset

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import locus4s.provenance.*
import scalafim.fmri.mvpa.EvidenceSource
import scalafim.response.*

/** U0 specimen 3: a response read and MVPA evidence exchange one provider-owned
  * source DAG without translation.
  */
class ResponseProvenanceLineageSuite extends munit.FunSuite:
  private val sourceId = SourceId.unsafe("lineage-source")
  private val rootNode = ProvenanceId.unsafe("lineage-source:read")
  private val adapterNode = ProvenanceId.unsafe("lineage-source:evidence")

  test("an ordered response read carries its source DAG into MVPA evidence unchanged"):
    val responseSchema = schema("lineage")
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          sourceId,
          responseSchema,
          Array.tabulate(12)(_.toDouble),
          provenanceNode = Some(rootNode)
        )
        .fold(error => fail(error.message), identity)
    source
      .read(selection(responseSchema, Vector(2, 0), Vector(3, 1)))
      .value
      .map: result =>
        val read = result.fold(error => fail(error.message), identity)
        assertEquals(read.block.rowMajorCopy.toSeq.toVector, Vector(11.0, 9.0, 3.0, 1.0))
        val lineage =
          Provenance
            .derive(
              read.provenance,
              adapterNode,
              ProvenanceOperation.Adapter(OperationId.unsafe("lineage-specimen"))
            )
            .fold(error => fail(error.message), identity)
        val evidence =
          EvidenceSource(sourceId, lineage).fold(error => fail(error.message), identity)

        assertEquals(evidence.roots, Vector(adapterNode))
        assertEquals(evidence.nodes.map(_.id), Vector(rootNode, adapterNode))
        assertEquals(evidence.nodes.map(_.parents), Vector(Vector.empty, Vector(rootNode)))
        assertEquals(
          evidence.nodes.map(_.evidence),
          Vector.fill(2)(Vector(ProvenanceEvidence.NoneDeclared))
        )
        val restored = Provenance
          .make(evidence.nodes, evidence.roots)
          .fold(error => fail(error.message), identity)
        assertEquals(EvidenceSource(sourceId, restored), Right(evidence))
        assert(EvidenceSource(SourceId.unsafe("foreign-source"), lineage).isLeft)
      .unsafeToFuture()

  test("restored lineage refuses duplicate nodes, duplicate roots and out-of-order parents"):
    val lineage =
      Provenance
        .derive(
          Provenance.source(rootNode, sourceId),
          adapterNode,
          ProvenanceOperation.Selection
        )
        .fold(error => fail(error.message), identity)
    val nodes = lineage.nodes
    assertEquals(
      Provenance.make(nodes :+ nodes.head, lineage.roots),
      Left(ProvenanceError.DuplicateNode(rootNode))
    )
    assertEquals(
      Provenance.make(nodes, lineage.roots ++ lineage.roots),
      Left(ProvenanceError.DuplicateRoot(adapterNode))
    )
    assertEquals(
      Provenance.make(nodes.reverse, lineage.roots),
      Left(ProvenanceError.MissingParent(adapterNode, rootNode))
    )

  private def schema(id: String): ResponseSchema =
    val time =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis](s"$id:time"),
          origin = 0.5,
          interval = 1.0,
          count = 3,
          UnitId.unsafe("second")
        )
        .fold(error => fail(error.message), identity)
    val samples =
      SampleDomain
        .make(
          DomainId.unsafe[SampleAxis](s"$id:samples"),
          count = 4,
          SampleDomainKind.Volume(
            DomainReference.unsafe("test-space", s"$id-space"),
            None,
            DomainReference.unsafe("test-order", s"$id-order")
          )
        )
        .fold(error => fail(error.message), identity)
    ResponseSchema
      .make(
        ResponseSchemaId.unsafe(id),
        time,
        samples,
        SignalSchema(
          UnitId.unsafe("percent-signal"),
          CalibrationState.Applied,
          NonFinitePolicy.Reject
        )
      )
      .fold(error => fail(error.message), identity)

  private def selection(
      responseSchema: ResponseSchema,
      times: Vector[Int],
      samples: Vector[Int]
  ): ResolvedResponseSelection =
    val timepoints =
      OrderedIndices
        .fromInts(responseSchema.time.id, responseSchema.time.count, times)
        .fold(error => fail(error.message), identity)
    val sampleIndices =
      OrderedIndices
        .fromInts(responseSchema.samples.id, responseSchema.samples.count, samples)
        .fold(error => fail(error.message), identity)
    ResolvedResponseSelection
      .make(responseSchema, timepoints, sampleIndices)
      .fold(error => fail(error.message), identity)
