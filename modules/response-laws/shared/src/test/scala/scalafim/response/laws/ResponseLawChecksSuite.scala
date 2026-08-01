package scalafim.response.laws

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.response.*

import scala.concurrent.ExecutionContext.Implicits.{
  global as executionContext
}

class ResponseLawChecksSuite extends munit.FunSuite:
  test("one reusable source law checks selection partition order and receipts"):
    val responseSchema = schema("reusable")
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("reusable-source"),
          responseSchema,
          Array[Double](
            0.0, 1.0, 2.0, 3.0,
            10.0, 11.0, 12.0, 13.0,
            20.0, 21.0, 22.0, 23.0
          )
        )
        .fold(error => fail(error.message), identity)
    val whole =
      ResolvedResponseSelection
        .all(responseSchema)
        .fold(error => fail(error.message), identity)
    val selected =
      selection(responseSchema, Vector(2, 0), Vector(3, 1))
    val partitions =
      Vector(
        selection(responseSchema, Vector(2), Vector(3, 1)),
        selection(responseSchema, Vector(0), Vector(3, 1))
      )

    ResponseLawChecks
      .sourceReadLaws(
        "in-memory",
        source,
        whole,
        selected,
        partitions
      )
      .value
      .unsafeToFuture()
      .map:
        case Left(error) =>
          fail(error.message)
        case Right(result) =>
          assertEquals(result.toEither, Right(()))

  test("raw-bit and provenance checks report structured violations"):
    val responseSchema = schema("structured")
    val requested =
      selection(responseSchema, Vector(0), Vector(0))
    val positive =
      ResponseBlock
        .copyFromRowMajor(Array[Double](0.0), requested)
        .fold(error => fail(error.message), identity)
    val negative =
      ResponseBlock
        .copyFromRowMajor(Array[Double](-0.0), requested)
        .fold(error => fail(error.message), identity)
    val parent =
      Provenance.source(
        ProvenanceId.unsafe("parent-root"),
        SourceId.unsafe("parent-source")
      )
    val derived =
      Provenance
        .derive(
          parent,
          ProvenanceId.unsafe("derived-root"),
          ProvenanceOperation.Selection
        )
        .fold(error => fail(error.message), identity)

    val raw =
      ResponseLawChecks.rawBitPersistence(
        "raw",
        positive,
        negative
      )
    assertEquals(raw.failures.length, 1)
    assert(
      raw.failures.head
        .isInstanceOf[ResponseLawFailure.ScalarMismatch]
    )
    assertEquals(
      ResponseLawChecks.provenance("derived", parent, derived).toEither,
      Right(())
    )
    assert(
      ResponseLawChecks
        .provenance("not-derived", parent, parent)
        .failures
        .exists(
          _.isInstanceOf[
            ResponseLawFailure.MissingProvenanceParent
          ]
        )
    )

  private def schema(id: String): ResponseSchema =
    val time =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis](s"$id:time"),
          0.0,
          1.0,
          3,
          UnitId.unsafe("second")
        )
        .fold(error => fail(error.message), identity)
    val samples =
      SampleDomain
        .make(
          DomainId.unsafe[SampleAxis](s"$id:samples"),
          4,
          SampleDomainKind.Volume(
            DomainReference.unsafe("test-space", id),
            None,
            DomainReference.unsafe("test-order", id)
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
      schema: ResponseSchema,
      times: Vector[Int],
      samples: Vector[Int]
  ): ResolvedResponseSelection =
    val result: Either[ResponseFailure, ResolvedResponseSelection] =
      for
        timepoints <- OrderedIndices
          .fromInts(
            schema.time.id,
            schema.time.count,
            times
          )
          .left
          .map(error => error: ResponseFailure)
        sampleIndices <- OrderedIndices
          .fromInts(
            schema.samples.id,
            schema.samples.count,
            samples
          )
          .left
          .map(error => error: ResponseFailure)
        selected <- ResolvedResponseSelection
          .make(
            schema,
            timepoints,
            sampleIndices
          )
          .left
          .map(error => error: ResponseFailure)
      yield selected
    result.fold(error => fail(error.message), identity)
