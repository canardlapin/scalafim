package scalafim.dataset

import scalafim.archive.{
  ArchiveReadReceipt,
  ByteInterval,
  LocalityClaim,
  ObjectRange,
  PayloadId as ArchivePayloadId,
  PayloadPlanSummary,
  PhysicalLocality as ArchivePhysicalLocality,
  PhysicalObjectId,
  ReadObservation,
  ReceiptAccumulator,
  SelectionAxes as ArchiveSelectionAxes
}
import scalafim.response.{
  CalibrationState,
  DomainId,
  DomainReference,
  NonFinitePolicy,
  OrderedIndices,
  PhysicalReadUnit,
  ReceiptConformance,
  ResolvedResponseSelection,
  ResponseSchema,
  ResponseSchemaId,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  SelectionAxes as ResponseSelectionAxes,
  SignalSchema,
  TimeAxis,
  TimeDomain,
  UnitId
}

class ArchiveResponseReceiptAdapterSuite extends munit.FunSuite:
  test("every archive locality adapts to a conforming response receipt"):
    val requested = selection(schema("all-localities"), Vector(2, 0), Vector(3, 1))
    val payload = ArchivePayloadId.unsafe("bold")
    val objectId = PhysicalObjectId.unsafe("canonical/c/0")
    val otherObject = PhysicalObjectId.unsafe("canonical/c/1")

    val resident =
      receipt(
        payload,
        LocalityClaim.resident(ArchiveSelectionAxes.TimeAndSample),
        ArchivePhysicalLocality.Resident,
        Vector(ReadObservation.resident(payload))
      )
    val wholePayloadObservation =
      ReadObservation
        .wholePayload(payload, 96L)
        .fold(error => fail(error.message), identity)
    val wholePayload =
      receipt(
        payload,
        LocalityClaim.wholePayload(ArchiveSelectionAxes.TimeAndSample),
        ArchivePhysicalLocality.WholePayload,
        Vector(wholePayloadObservation)
      )
    val wholeObjectClaim =
      LocalityClaim
        .wholeObjects(
          ArchiveSelectionAxes.TimeAndSample,
          Vector(objectId, otherObject)
        )
        .fold(error => fail(error.message), identity)
    val wholeObject =
      receipt(
        payload,
        wholeObjectClaim,
        ArchivePhysicalLocality.WholeObject,
        Vector(
          ReadObservation
            .wholeObject(otherObject, 48L)
            .fold(error => fail(error.message), identity),
          ReadObservation
            .objectLength(objectId, 128L)
            .fold(error => fail(error.message), identity)
        )
      )
    val chunkClaim =
      LocalityClaim
        .chunkBounded(
          ArchiveSelectionAxes.TimeAndSample,
          Vector(objectId, otherObject)
        )
        .fold(error => fail(error.message), identity)
    val firstChunkAttempt =
      ReadObservation
        .byteRange(otherObject, ByteInterval.unsafe(4L, 8L), 8L)
        .fold(error => fail(error.message), identity)
    val secondChunkAttempt =
      ReadObservation
        .wholeObject(objectId, 20L)
        .fold(error => fail(error.message), identity)
    val chunkBounded =
      receipt(
        payload,
        chunkClaim,
        ArchivePhysicalLocality.ChunkBounded,
        Vector(firstChunkAttempt, secondChunkAttempt, firstChunkAttempt)
      )
    val rangeClaim =
      LocalityClaim
        .byteRangeBounded(
          ArchiveSelectionAxes.TimeAndSample,
          Vector(ObjectRange(objectId, ByteInterval.unsafe(0L, 64L)))
        )
        .fold(error => fail(error.message), identity)
    val byteRangeBounded =
      receipt(
        payload,
        rangeClaim,
        ArchivePhysicalLocality.ByteRangeBounded,
        Vector(
          ReadObservation
            .byteRange(objectId, ByteInterval.unsafe(8L, 16L), 16L)
            .fold(error => fail(error.message), identity),
          ReadObservation
            .byteRange(objectId, ByteInterval.unsafe(24L, 8L), 8L)
            .fold(error => fail(error.message), identity)
        )
      )

    val adapted =
      Vector(resident, wholePayload, wholeObject, chunkBounded, byteRangeBounded)
        .map(native =>
          ArchiveResponseReceiptAdapter
            .adapt(requested, native)
            .fold(error => fail(error.message), identity)
        )

    adapted.foreach: value =>
      assertEquals(
        ReceiptConformance.check(
          value.capabilities,
          value.response,
          value.layout
        ),
        Right(())
      )
      assertEquals(value.response.logical.timepoints, Vector(2, 0))
      assertEquals(value.response.logical.samples, Vector(3, 1))
      assertEquals(value.response.logical.logicalBytes, 32L)

    assertEquals(adapted(0).response.physical.physicalBytes,
      scalafim.response.PhysicalByteEvidence.Known(0L))
    assertEquals(adapted(1).response.physical.physicalBytes,
      scalafim.response.PhysicalByteEvidence.Known(96L))
    assertEquals(adapted(2).response.physical.physicalBytes,
      scalafim.response.PhysicalByteEvidence.Known(48L))
    assertEquals(adapted(3).response.physical.physicalBytes,
      scalafim.response.PhysicalByteEvidence.Known(36L))
    assertEquals(adapted(4).response.physical.physicalBytes,
      scalafim.response.PhysicalByteEvidence.Known(24L))

    val chunkIds =
      adapted(3).response.physical.byAxes.head.touched.collect:
        case PhysicalReadUnit.Chunk(_, chunk) => chunk.value
    assertEquals(
      chunkIds,
      Vector("canonical/c/1", "canonical/c/0", "canonical/c/1")
    )
    assertEquals(
      adapted(3).native.head.observations,
      Vector(firstChunkAttempt, secondChunkAttempt, firstChunkAttempt)
    )

  test("separate time and sample claims cover one mixed-axis logical read"):
    val requested = selection(schema("split-axes"), Vector(2, 0), Vector(3, 1))
    val timePayload = ArchivePayloadId.unsafe("temporal-coefficients")
    val samplePayload = ArchivePayloadId.unsafe("spatial-basis")
    val basisObject = PhysicalObjectId.unsafe("basis/c/0")
    val timeReceipt =
      receipt(
        timePayload,
        LocalityClaim.wholePayload(ArchiveSelectionAxes.Time),
        ArchivePhysicalLocality.WholePayload,
        Vector(
          ReadObservation
            .wholePayload(timePayload, 32L)
            .fold(error => fail(error.message), identity)
        )
      )
    val sampleReceipt =
      receipt(
        samplePayload,
        LocalityClaim
          .byteRangeBounded(
            ArchiveSelectionAxes.Sample,
            Vector(ObjectRange(basisObject, ByteInterval.unsafe(64L, 32L)))
          )
          .fold(error => fail(error.message), identity),
        ArchivePhysicalLocality.ByteRangeBounded,
        Vector(
          ReadObservation
            .byteRange(basisObject, ByteInterval.unsafe(72L, 16L), 16L)
            .fold(error => fail(error.message), identity)
        )
      )

    val adapted =
      ArchiveResponseReceiptAdapter
        .adapt(requested, Vector(sampleReceipt, timeReceipt))
        .fold(error => fail(error.message), identity)

    assertEquals(
      adapted.response.physical.byAxes.map(_.axes),
      Vector(ResponseSelectionAxes.Samples, ResponseSelectionAxes.Time)
    )
    assertEquals(
      adapted.response.physical.physicalBytes,
      scalafim.response.PhysicalByteEvidence.Known(48L)
    )
    assertEquals(
      ReceiptConformance.check(
        adapted.capabilities,
        adapted.response,
        adapted.layout
      ),
      Right(())
    )

  test("adaptation rejects archive axes that do not cover the logical selection"):
    val responseSchema = schema("axis-mismatch")
    val requested =
      selection(
        responseSchema,
        Vector(0, 1, 2),
        Vector(3, 1)
      )
    val payload = ArchivePayloadId.unsafe("bold")
    val native =
      receipt(
        payload,
        LocalityClaim.wholePayload(ArchiveSelectionAxes.Time),
        ArchivePhysicalLocality.WholePayload,
        Vector(
          ReadObservation
            .wholePayload(payload, 16L)
            .fold(error => fail(error.message), identity)
        )
      )

    ArchiveResponseReceiptAdapter.adapt(requested, native) match
      case Left(ArchiveResponseReceiptError.AxisCoverageMismatch(
            ResponseSelectionAxes.Samples,
            Vector(ResponseSelectionAxes.Time)
          )) =>
        ()
      case other =>
        fail(s"expected axis coverage mismatch, found $other")

  private def receipt(
      payload: ArchivePayloadId,
      claim: LocalityClaim,
      locality: ArchivePhysicalLocality,
      observations: Vector[ReadObservation]
  ): ArchiveReadReceipt =
    ArchiveReadReceipt
      .from(
        PayloadPlanSummary(payload, "adapter-law", claim),
        locality,
        ReceiptAccumulator.from(observations)
      )
      .fold(error => fail(error.message), identity)

  private def schema(id: String): ResponseSchema =
    val time =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis](s"$id:time"),
          origin = 0.0,
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
