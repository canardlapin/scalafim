package scalafim.dataset

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, NeuroSpace}
import scalafim.response.*
import scala.concurrent.ExecutionContext.Implicits.{global as executionContext}

class ResponseKernelAdaptersSuite extends munit.FunSuite:
  test("dataset adapter preserves selected values, order, shape, and receipt truth"):
    val dataset = fixtureDataset()
    val schemaId = ResponseSchemaId.unsafe("dataset-adapter")
    val source =
      DatasetResponseSource
        .fromDataset[IO](
          dataset,
          schemaId,
          SourceId.unsafe("dataset-adapter-source"),
          UnitId.unsafe("percent-signal")
        )
        .fold(error => fail(error.message), identity)
    val requested = selection(source.schema, Vector(2, 0), Vector(3, 1))

    val adapted =
      source
        .read(requested)
        .value
        .unsafeToFuture()
    val direct =
      dataset.series(
        DataSelection(
          TimepointSelection.indices(2, 0),
          VoxelSelection.indices(3, 1)
        )
      )

    adapted.map: evaluated =>
      val read = evaluated.fold(error => fail(error.message), identity)
      assertEquals(read.block.rows, direct.nTimepoints)
      assertEquals(read.block.columns, direct.nVoxels)
      assertEquals(read.block.selection, requested)
      var row = 0
      while row < read.block.rows do
        var column = 0
        while column < read.block.columns do
          assertEqualsDouble(read.block(row, column), direct.data(row, column), 0.0)
          column += 1
        row += 1
      assertEquals(
        ReceiptConformance.check(source.capabilities, read.receipt, source.layout),
        Right(())
      )
      read.receipt.physical.physicalBytes match
        case PhysicalByteEvidence.Unavailable(_) =>
          ()
        case other =>
          fail(s"dataset physical byte count must be unavailable, got $other")

  test("dataset adapter preserves current public duplicate rejection"):
    val dataset = fixtureDataset()
    val source =
      DatasetResponseSource
        .fromDataset[IO](
          dataset,
          ResponseSchemaId.unsafe("duplicates"),
          SourceId.unsafe("duplicate-source"),
          UnitId.unsafe("percent-signal")
        )
        .toOption
        .get
    val times =
      OrderedIndices
        .fromInts(
          source.schema.time.id,
          source.schema.time.count,
          Vector(1, 1),
          DuplicatePolicy.Allow
        )
        .toOption
        .get
    val samples =
      OrderedIndices
        .fromInts(source.schema.samples.id, source.schema.samples.count, Vector(0))
        .toOption
        .get
    val requested =
      ResolvedResponseSelection
        .make(source.schema, times, samples)
        .toOption
        .get

    assertEquals(
      source.plan(requested).left.toOption,
      Some(ReadPlanningError.DuplicateSelectionUnsupported(SelectionAxes.Time, 1))
    )

  test("sampling-frame adapter preserves regular timing raw bits"):
    val dataset = fixtureDataset()
    val schema =
      DatasetResponseSchema
        .fromDataset(
          dataset.dataset,
          ResponseSchemaId.unsafe("timing"),
          UnitId.unsafe("percent-signal")
        )
        .fold(error => fail(error.message), identity)

    schema.time match
      case regular: TimeDomain.Regular =>
        assertEquals(
          regular.origin.rawBits,
          java.lang.Double.doubleToRawLongBits(dataset.samplingFrame.startTime.head.value)
        )
        assertEquals(
          regular.interval.rawBits,
          java.lang.Double.doubleToRawLongBits(dataset.samplingFrame.tr.head.value)
        )
      case _ =>
        fail("single-run sampling frame must adapt to regular response time")

  test("backend attachment reports incompatible response cardinality precisely"):
    val dataset = fixtureDataset()
    val wrongTime =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis]("wrong:time"),
          0.5,
          1.0,
          count = 2,
          UnitId.unsafe("second")
        )
        .toOption
        .get
    val wrongSchema =
      DatasetResponseSchema.fromBackend(
        dataset.backend,
        ResponseSchemaId.unsafe("wrong"),
        wrongTime,
        UnitId.unsafe("percent-signal")
      )

    assertEquals(
      wrongSchema.left.toOption,
      Some(ResponseAdapterError.TimeCountMismatch(2, 3))
    )

  test("backend attachment rejects equal-sized foreign sample references"):
    val dataset = fixtureDataset()
    val original =
      DatasetResponseSchema
        .fromDataset(
          dataset.dataset,
          ResponseSchemaId.unsafe("foreign-attachment"),
          UnitId.unsafe("percent-signal")
        )
        .fold(error => fail(error.message), identity)
    val foreignSamples =
      SampleDomain
        .make(
          original.samples.id,
          original.samples.count,
          SampleDomainKind.Volume(
            DomainReference.unsafe("foreign-space", "space"),
            None,
            DomainReference.unsafe("foreign-order", "order")
          )
        )
        .fold(error => fail(error.message), identity)
    val foreignSchema =
      ResponseSchema
        .make(original.id, original.time, foreignSamples, original.signal)
        .fold(error => fail(error.message), identity)

    val attached =
      DatasetResponseSource.fromBackend[IO](
        dataset.backend,
        foreignSchema,
        SourceId.unsafe("foreign-attachment-source")
      )

    attached.left.toOption match
      case Some(ResponseAdapterError.SampleDomainMismatch(expected, actual)) =>
        assertEquals(actual, foreignSamples)
        assertNotEquals(expected, actual)
      case other =>
        fail(s"expected a sample-domain mismatch, got $other")

  private def fixtureDataset(): SynchronousFmriDataset =
    val backend =
      InMemoryDatasetBackend(
        DatasetId("response-adapter-fixture"),
        DMat.fromRows(
          Vector(
            Vector(0.0, 1.0, 2.0, 3.0),
            Vector(10.0, 11.0, 12.0, 13.0),
            Vector(20.0, 21.0, 22.0, 23.0)
          )
        ),
        NeuroSpace(Vector(4, 1, 1))
      )
    FmriDataset.unsafe(
      backend,
      SamplingFrame.regular(1.0, 3, startTime = 0.5).toOption.get
    )

  private def selection(
      schema: ResponseSchema,
      times: Vector[Int],
      samples: Vector[Int]
  ): ResolvedResponseSelection =
    val timepoints =
      OrderedIndices
        .fromInts(schema.time.id, schema.time.count, times)
        .fold(error => fail(error.message), identity)
    val sampleIndices =
      OrderedIndices
        .fromInts(schema.samples.id, schema.samples.count, samples)
        .fold(error => fail(error.message), identity)
    ResolvedResponseSelection
      .make(schema, timepoints, sampleIndices)
      .fold(error => fail(error.message), identity)
