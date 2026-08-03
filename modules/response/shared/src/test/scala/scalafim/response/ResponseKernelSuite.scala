package scalafim.response

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import scala.compiletime.testing.typeCheckErrors
import scala.concurrent.ExecutionContext.Implicits.{global as executionContext}

class ResponseKernelSuite extends munit.FunSuite:
  test("ordered indices preserve order and make duplicate policy explicit"):
    val domain = DomainId.unsafe[TimeAxis]("run-a:time")
    val ordered =
      OrderedIndices
        .fromInts(domain, 4, Vector(3, 0, 2))
        .fold(error => fail(error.message), identity)

    assertEquals(ordered.values, Vector(3, 0, 2))
    assertEquals(ordered(1).value, 0)
    assertEquals(
      OrderedIndices.fromInts(domain, 4, Vector(1, 1)).left.toOption,
      Some(IndexError.Duplicate(1))
    )
    val gathered =
      OrderedIndices
        .fromInts(domain, 4, Vector(1, 1), DuplicatePolicy.Allow)
        .fold(error => fail(error.message), identity)
    assertEquals(gathered.values, Vector(1, 1))
    assertEquals(gathered.indexOfDuplicate, Some(1))

  test("same-sized foreign domains cannot form or execute a selection"):
    val original = schema("original")
    val foreign = schema("foreign")
    val foreignTimes =
      OrderedIndices
        .fromInts(foreign.time.id, foreign.time.count, Vector(0, 1))
        .fold(error => fail(error.message), identity)
    val originalSamples =
      OrderedIndices
        .fromInts(original.samples.id, original.samples.count, Vector(0, 1))
        .fold(error => fail(error.message), identity)

    val error =
      ResolvedResponseSelection
        .make(original, foreignTimes, originalSamples)
        .left
        .toOption
        .get

    assertEquals(
      error,
      SelectionError.TimeDomainMismatch(original.time.id, foreign.time.id)
    )

  test("same-identity selections report cardinality drift explicitly"):
    val responseSchema = schema("cardinality")
    val wrongSizeTimes =
      OrderedIndices
        .fromInts(responseSchema.time.id, 4, Vector(0, 1))
        .fold(error => fail(error.message), identity)
    val samples =
      OrderedIndices
        .fromInts(
          responseSchema.samples.id,
          responseSchema.samples.count,
          Vector(0, 1)
        )
        .fold(error => fail(error.message), identity)

    assertEquals(
      ResolvedResponseSelection
        .make(responseSchema, wrongSizeTimes, samples)
        .left
        .toOption,
      Some(
        SelectionError.AxisCardinalityMismatch(
          SelectionAxes.Time,
          responseSchema.time.count,
          4
        )
      )
    )

  test("regular time domains reject overflow at the last coordinate"):
    val result =
      TimeDomain.regular(
        DomainId.unsafe[TimeAxis]("overflow:time"),
        origin = Double.MaxValue,
        interval = Double.MaxValue,
        count = 2,
        UnitId.unsafe("second")
      )

    result match
      case Left(SchemaError.InvalidCoordinate("last time coordinate", value)) =>
        assert(value.isInfinite)
      case other =>
        fail(s"expected last-coordinate overflow, got $other")

  test("time-domain equality includes coordinates and coordinate access is total"):
    val id = DomainId.unsafe[TimeAxis]("structural:time")
    val units = UnitId.unsafe("second")
    val first =
      TimeDomain
        .regular(id, origin = 0.0, interval = 1.0, count = 3, units)
        .fold(error => fail(error.message), identity)
    val same =
      TimeDomain
        .regular(id, origin = 0.0, interval = 1.0, count = 3, units)
        .fold(error => fail(error.message), identity)
    val shifted =
      TimeDomain
        .regular(id, origin = -0.0, interval = 1.0, count = 3, units)
        .fold(error => fail(error.message), identity)
    val valid = AxisIndex.fromInt[TimeAxis](2).toOption.get
    val invalid = AxisIndex.fromInt[TimeAxis](3).toOption.get

    assertEquals(first, same)
    assertNotEquals(first, shifted)
    assertEquals(
      first.coordinate(valid).map(_.rawBits),
      Right(java.lang.Double.doubleToRawLongBits(2.0))
    )
    assertEquals(first.coordinate(invalid), Left(IndexError.OutOfBounds(3, 3)))

  test("neutral sample references may identify the same external artifact"):
    val shared = DomainReference.unsafe("content", "shared-reference")
    val result =
      SampleDomain.make(
        DomainId.unsafe[SampleAxis]("shared:samples"),
        count = 3,
        SampleDomainKind.Surface(shared, shared, shared)
      )

    assert(result.isRight)

  test("response blocks copy public input and never expose case-class copy"):
    val requested = selection(schema("owned"), Vector(1, 0), Vector(2, 0))
    val input = Array[Double](12.0, 10.0, 2.0, 0.0)
    val block =
      ResponseBlock
        .copyFromRowMajor(input, requested)
        .fold(error => fail(error.message), identity)

    input(0) = -999.0
    assertEqualsDouble(block(0, 0), 12.0, 0.0)
    val copied = block.rowMajorCopy
    copied(0) = 999.0
    assertEqualsDouble(block(0, 0), 12.0, 0.0)
    assert(typeCheckErrors("""
      import scalafim.response.ResponseBlock
      def illegal(block: ResponseBlock) = block.copy()
    """).nonEmpty)

  test("in-memory source obeys selection, order, shape, and ownership laws"):
    val responseSchema = schema("memory")
    val values = Array[Double](
      0.0, 1.0, 2.0, 3.0,
      10.0, 11.0, 12.0, 13.0,
      20.0, 21.0, 22.0, 23.0
    )
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("memory-source"),
          responseSchema,
          values
        )
        .fold(error => fail(error.message), identity)
    values(11) = -1.0
    val requested = selection(responseSchema, Vector(2, 0), Vector(3, 1))

    val result =
      source
        .read(requested)
        .value
        .unsafeToFuture()

    result.map: evaluated =>
      val read = evaluated.fold(error => fail(error.message), identity)
      assertEquals(read.block.rows, 2)
      assertEquals(read.block.columns, 2)
      assertEquals(read.block.selection, requested)
      assertEquals(
        read.block.rowMajorCopy.toSeq.toVector,
        Vector(23.0, 21.0, 3.0, 1.0)
      )
      assertEquals(
        ReceiptConformance.check(source.capabilities, read.receipt, source.layout),
        Right(())
      )

  test("selection and partition reads agree exactly for dense storage"):
    val responseSchema = schema("partition")
    val values = Array[Double](
      0.0, 1.0, 2.0, 3.0,
      10.0, 11.0, 12.0, 13.0,
      20.0, 21.0, 22.0, 23.0
    )
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("partition-source"),
          responseSchema,
          values
        )
        .fold(error => fail(error.message), identity)
    (for
      together <- source
        .read(selection(responseSchema, Vector(2, 0), Vector(3, 1)))
        .value
      first <- source
        .read(selection(responseSchema, Vector(2), Vector(3, 1)))
        .value
      second <- source
        .read(selection(responseSchema, Vector(0), Vector(3, 1)))
        .value
    yield
      val togetherBlock = together.toOption.get.block
      val firstBlock = first.toOption.get.block
      val secondBlock = second.toOption.get.block
      assertEquals(
        togetherBlock.rowMajorCopy.toSeq.toVector,
        firstBlock.rowMajorCopy.toSeq.toVector ++ secondBlock.rowMajorCopy.toSeq.toVector
      )
    ).unsafeToFuture()

  test("planned reads retain heterogeneous path-dependent plans"):
    val responseSchema = schema("planned")
    val first =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("planned-first"),
          responseSchema,
          Array[Double](
            0.0, 1.0, 2.0, 3.0,
            10.0, 11.0, 12.0, 13.0,
            20.0, 21.0, 22.0, 23.0
          )
        )
        .toOption
        .get
    val second =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("planned-second"),
          responseSchema,
          Array[Double](
            100.0, 101.0, 102.0, 103.0,
            110.0, 111.0, 112.0, 113.0,
            120.0, 121.0, 122.0, 123.0
          )
        )
        .toOption
        .get
    val requested = selection(responseSchema, Vector(1), Vector(2))
    val reads: Vector[PlannedRead[IO]] =
      Vector(
        first.planned(requested).toOption.get,
        second.planned(requested).toOption.get
      )

    assertEquals(reads.map(_.summary.source.value), Vector("planned-first", "planned-second"))
    reads
      .traverse(_.execute.value)
      .map: results =>
        assertEquals(
          results.map(_.toOption.get.block(0, 0)),
          Vector(12.0, 112.0)
        )
      .unsafeToFuture()

  test("decode consistency is separate from scientific reconstruction error"):
    val oneUlp = java.lang.Math.nextUp(1.0)
    assert(!DecodeConsistency.ExactBits.agrees(1.0, oneUlp))
    val ulps =
      DecodeConsistency.ulpBounded(1).fold(error => fail(error.message), identity)
    assert(ulps.agrees(1.0, oneUlp))
    assert(!ulps.agrees(1.0, java.lang.Math.nextUp(oneUlp)))
    val tolerance =
      DecodeConsistency
        .absoluteRelative(1e-12, 1e-12)
        .fold(error => fail(error.message), identity)
    assert(tolerance.agrees(1.0, 1.0 + 1e-13))
    assert(!DecodeConsistency.ExactBits.agrees(0.0, -0.0))
    val reconstruction =
      ReconstructionErrorBounds
        .make(absolute = 1e-12, relative = 1e-12)
        .fold(error => fail(error.message), identity)
    assert(reconstruction.agrees(1.0, 1.0 + 1e-13))
    assert(!reconstruction.agrees(1.0, 1.0 + 1e-8))
    ReconstructionErrorBounds.make(Double.NaN, 0.0).left.toOption match
      case Some(ConsistencyError.InvalidTolerance(label, value)) =>
        assertEquals(label, "reconstruction absolute")
        assert(value.isNaN)
      case other =>
        fail(s"expected a NaN reconstruction-tolerance error, got $other")
    assertEquals(
      ReconstructionContract.DeterministicBounded(reconstruction),
      ReconstructionContract.DeterministicBounded(reconstruction)
    )

  test("locality conformance is axis-keyed rather than enum-ordered"):
    val payload = PayloadId.unsafe("bold")
    val objectId = ObjectId.unsafe("bold-object")
    val chunk0 = ChunkId.unsafe("chunk-0")
    val chunk1 = ChunkId.unsafe("chunk-1")
    val layout =
      OpenedLayout
        .make(Vector(PayloadLayout(payload, Vector(objectId), Vector(chunk0, chunk1))))
        .fold(error => fail(error.message), identity)
    val capabilities =
      ReadCapabilities
        .make(
          Vector(
            AxisLocality(SelectionAxes.Time, PhysicalLocality.WholePayload),
            AxisLocality(SelectionAxes.Samples, PhysicalLocality.ChunkBounded)
          )
        )
        .fold(error => fail(error.message), identity)
    val responseSchema = schema("mixed-locality")
    val requested = selection(responseSchema, Vector(0, 1), Vector(1))
    val receipt =
      ReadReceipt(
        LogicalReadSummary(
          responseSchema.id,
          SelectionAxes.TimeAndSamples,
          requested.timepoints.values,
          requested.samples.values,
          16L
        ),
        PhysicalReadSummary(
          Vector(
            AxisReadEvidence(
              SelectionAxes.Time,
              PhysicalLocality.WholePayload,
              Vector(PhysicalReadUnit.Payload(payload)),
              Vector(PhysicalReadUnit.Payload(payload))
            ),
            AxisReadEvidence(
              SelectionAxes.Samples,
              PhysicalLocality.ChunkBounded,
              Vector(PhysicalReadUnit.Chunk(payload, chunk1)),
              Vector(PhysicalReadUnit.Chunk(payload, chunk1))
            )
          ),
          PhysicalByteEvidence.Known(32L),
          0
        ),
        Vector(IntegrityEvidence.NotChecked),
        Vector.empty
      )

    assertEquals(ReceiptConformance.check(capabilities, receipt, layout), Right(()))
    val unknownPayload = PayloadId.unsafe("unknown")
    val invalidPayloadCover =
      receipt.copy(
        physical = receipt.physical.copy(
          byAxes = receipt.physical.byAxes.updated(
            0,
            receipt.physical.byAxes(0).copy(
              covering = Vector(PhysicalReadUnit.Payload(unknownPayload))
            )
          )
        )
      )
    assertEquals(
      ReceiptConformance.check(capabilities, invalidPayloadCover, layout).left.toOption,
      Some(ReceiptConformanceError.UnknownPayload(unknownPayload))
    )
    val invalid =
      receipt.copy(
        physical = receipt.physical.copy(
          byAxes = receipt.physical.byAxes.updated(
            1,
            receipt.physical.byAxes(1).copy(
              touched = Vector(PhysicalReadUnit.Chunk(payload, chunk0))
            )
          )
        )
      )
    assert(ReceiptConformance.check(capabilities, invalid, layout).isLeft)

  test("provenance construction requires parents to precede derived nodes"):
    val sourceId = SourceId.unsafe("provenance-source")
    val sourceNode = ProvenanceId.unsafe("source-node")
    val derivedNode = ProvenanceId.unsafe("derived-node")
    val source =
      ProvenanceNode(
        sourceNode,
        ProvenanceOperation.SourceRead(sourceId),
        Vector.empty,
        Vector(ProvenanceEvidence.NoneDeclared)
      )
    val derived =
      ProvenanceNode(
        derivedNode,
        ProvenanceOperation.Selection,
        Vector(sourceNode),
        Vector(ProvenanceEvidence.NoneDeclared)
      )

    assert(Provenance.make(Vector(source, derived), Vector(derivedNode)).isRight)
    assertEquals(
      Provenance.make(Vector(derived, source), Vector(derivedNode)).left.toOption,
      Some(ProvenanceError.MissingParent(derivedNode, sourceNode))
    )

  private def schema(id: String): ResponseSchema =
    val schemaId = ResponseSchemaId.unsafe(id)
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
        schemaId,
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
