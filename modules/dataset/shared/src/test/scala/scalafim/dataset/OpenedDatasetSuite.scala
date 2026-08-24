package scalafim.dataset

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, PrimitiveBuffers, SampleSpaces, SomeSampleSpace}
import scalafim.response.*
import scalafim.response.laws.ResponseLawChecks

import scala.concurrent.ExecutionContext.Implicits.{global as executionContext}

class OpenedDatasetSuite extends munit.FunSuite:

  test("checked attachment plans run-local selections and preserves read evidence"):
    val synchronous = fixture()
    val source = sourceFor(synchronous.dataset)
    val context = volumeContext(synchronous.dataset)
    val opened =
      OpenedDataset
        .attach(synchronous.dataset, source, context)
        .toEither
        .fold(issues => fail(issues.toNonEmptyList.toList.map(_.message).mkString("; ")), identity)
    val query =
      DatasetRunQuery(
        subject = Some(SubjectId("sub-01")),
        run = Some(RunId("run-2"))
      )
    val selection =
      DataSelection(
        time = TimepointSelection.indices(1, 0),
        voxels = VoxelSelection.indices(2, 0)
      )
    val plan =
      opened
        .plan(query, selection)
        .fold(error => fail(error.message), identity)

    assertEquals(plan.reads.length, 1)
    val read = plan.reads.head
    assertEquals(read.selection.timepoints.values, Vector(3, 2))
    assertEquals(read.selection.samples.values, Vector(2, 0))
    assertEquals(read.partition.localTimepoints, Vector(1, 0))
    assertEquals(read.datasetVoxels.map(_.value), Vector(2, 0))

    opened.execute(plan).value.unsafeToFuture().map: evaluated =>
      val result = evaluated.fold(error => fail(error.message), identity)
      val segment = result.segments.head
      assertNotEquals(
        segment.response.provenance.roots,
        source.provenance.roots
      )
      assertEquals(
        ResponseLawChecks
          .provenance(
            "dataset attachment",
            source.provenance,
            segment.response.provenance
          )
          .toEither,
        Right(())
      )
      assertEquals(
        segment.response.receipt.logical,
        ReadReceipt.resident(read.selection).logical
      )
      assertEquals(segment.response.block.rows, 2)
      assertEquals(segment.response.block.columns, 2)
      assertEqualsDouble(segment.response.block(0, 0), 32.0, 0.0)
      assertEqualsDouble(segment.response.block(0, 1), 30.0, 0.0)
      assertEqualsDouble(segment.response.block(1, 0), 22.0, 0.0)
      assertEqualsDouble(segment.response.block(1, 1), 20.0, 0.0)

      val series =
        result
          .toSegmentedFmriSeries(synchronous.dataset)
          .fold(error => fail(error.message), identity)
      assertEquals(series.segments.length, 1)
      assertEquals(series.segments.head.series.timepoints, Vector(3, 2))
      assertEquals(series.segments.head.series.voxelIndices, Vector(2, 0))

  test("multi-acquisition bridge preserves direct reads and segmentation"):
    val synchronous = fixture()
    val source = sourceFor(synchronous.dataset, "multi-bridge")
    val context = volumeContext(synchronous.dataset)
    val opened =
      OpenedDataset
        .attach(synchronous.dataset, source, context)
        .toEither
        .fold(
          issues =>
            fail(
              issues.toNonEmptyList.toList
                .map(_.message)
                .mkString("; ")
            ),
          identity
        )
    val plan =
      opened
        .plan(
          DatasetRunQuery.All,
          DataSelection(
            time = TimepointSelection.indices(1, 0),
            voxels = VoxelSelection.indices(2, 0)
          )
        )
        .fold(error => fail(error.message), identity)

    val program =
      for
        direct <- plan.reads.toVector.traverse(read =>
          source.read(read.selection).value
        )
        bridged <- opened.execute(plan).value
      yield
        val expected =
          direct.map(_.fold(error => fail(error.message), identity))
        val actual =
          bridged.fold(error => fail(error.message), identity)
        assertEquals(actual.assembly, AssemblyPolicy.Segmented)
        assertEquals(actual.segments.length, 2)
        assertEquals(
          actual.segments.toVector.map(_.run.run.value),
          Vector("run-1", "run-2")
        )
        expected
          .zip(actual.segments.toVector)
          .foreach: (reference, segment) =>
            assertEquals(
              ResponseLawChecks
                .blockAgreement(
                  "multi-acquisition dataset bridge",
                  reference.block,
                  segment.response.block,
                  source.consistency
                )
                .toEither,
              Right(())
            )
            assertEquals(
              ResponseLawChecks
                .provenance(
                  "multi-acquisition dataset provenance",
                  reference.provenance,
                  segment.response.provenance
                )
                .toEither,
              Right(())
            )

    program.unsafeToFuture()

  test("attachment accumulates timing, geometry, ordering, and signal failures"):
    val dataset = fixture().dataset
    val context = volumeContext(dataset)
    val expected = context.expectedSchema
    val wrongTime =
      TimeDomain
        .explicit(
          expected.time.id,
          Vector(0.0, 1.0, 2.0),
          UnitId.unsafe("millisecond")
        )
        .fold(error => fail(error.message), identity)
    val wrongSamples =
      SampleDomain
        .make(
          expected.samples.id,
          expected.samples.count,
          SampleDomainKind.Volume(
            DomainReference.unsafe("scalafim-image-space", "foreign-space"),
            Some(DomainReference.unsafe("scalafim-dataset-mask", "foreign-mask")),
            DomainReference.unsafe("scalafim-dataset-order", "foreign-order")
          )
        )
        .fold(error => fail(error.message), identity)
    val wrongSchema =
      ResponseSchema
        .make(
          expected.id,
          wrongTime,
          wrongSamples,
          SignalSchema(
            UnitId.unsafe("arbitrary"),
            CalibrationState.Applied,
            NonFinitePolicy.Reject
          )
        )
        .fold(error => fail(error.message), identity)
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("mismatch-source"),
          wrongSchema,
          PrimitiveBuffers.tabulate[Double](wrongTime.count * wrongSamples.count)(_.toDouble)
        )
        .fold(error => fail(error.message), identity)

    val issues =
      OpenedDataset
        .attach(dataset, source, context)
        .toEither
        .left
        .toOption
        .getOrElse(fail("expected attachment rejection"))
        .toNonEmptyList
        .toList

    assert(issues.exists(_.isInstanceOf[AttachmentIssue.TimeCountMismatch]))
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.TimeUnitsMismatch]))
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.GeometryMismatch]))
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.MaskIdentityMismatch]))
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.SampleOrderingMismatch]))
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.SignalUnitsMismatch]))
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.NonFinitePolicyMismatch]))

  test("explicit bijective sample alignment authorizes reordering without resampling"):
    val dataset = fixture().dataset
    val expected =
      DatasetResponseSchema
        .fromDataset(
          dataset,
          ResponseSchemaId.unsafe("aligned-schema"),
          UnitId.unsafe("percent-signal")
        )
        .fold(error => fail(error.message), identity)
    val actualSamples =
      expected.samples.kind match
        case SampleDomainKind.Volume(space, mask, _) =>
          SampleDomain
            .make(
              expected.samples.id,
              expected.samples.count,
              SampleDomainKind.Volume(
                space,
                mask,
                DomainReference.unsafe("scalafim-dataset-order", "2,0,1")
              )
            )
            .fold(error => fail(error.message), identity)
        case _ =>
          fail("fixture must be volumetric")
    val actualSchema =
      ResponseSchema
        .make(expected.id, expected.time, actualSamples, expected.signal)
        .fold(error => fail(error.message), identity)
    val sourceValues =
      Array(
        1.0, 2.0, 0.0,
        11.0, 12.0, 10.0,
        21.0, 22.0, 20.0,
        31.0, 32.0, 30.0
      )
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("aligned-source"),
          actualSchema,
          sourceValues
        )
        .fold(error => fail(error.message), identity)
    val alignment =
      SampleAlignment
        .make(Vector(2, 0, 1))
        .fold(error => fail(error.message), identity)
    val context =
      AcquisitionContext
        .volume(
          dataset,
          DatasetKey.unsafe("sub-01"),
          ResponseKey.unsafe("aligned"),
          expected.id,
          expected.signal.units,
          alignment = Some(alignment)
        )
        .fold(error => fail(error.message), identity)
    val opened =
      OpenedDataset
        .attach(dataset, source, context)
        .toEither
        .fold(issues => fail(issues.toNonEmptyList.toList.map(_.message).mkString("; ")), identity)
    val plan =
      opened
        .plan(
          DatasetRunQuery(run = Some(RunId("run-1"))),
          DataSelection(
            time = TimepointSelection.indices(0),
            voxels = VoxelSelection.indices(0, 1, 2)
          )
        )
        .fold(error => fail(error.message), identity)

    assertEquals(plan.reads.head.selection.samples.values, Vector(2, 0, 1))
    opened.execute(plan).value.unsafeToFuture().map: evaluated =>
      val block =
        evaluated
          .fold(error => fail(error.message), identity)
          .segments
          .head
          .response
          .block
      assertEqualsDouble(block(0, 0), 0.0, 0.0)
      assertEqualsDouble(block(0, 1), 1.0, 0.0)
      assertEqualsDouble(block(0, 2), 2.0, 0.0)

  test("surface adapter reports topology drift as a typed attachment issue"):
    val dataset = fixture().dataset
    val schemaId = ResponseSchemaId.unsafe("surface-schema")
    val surface = DomainReference.unsafe("surface", "fsaverage-lh")
    val expectedTopology = DomainReference.unsafe("topology", "triangles-a")
    val actualTopology = DomainReference.unsafe("topology", "triangles-b")
    val ordering = DomainReference.unsafe("surface-order", "vertex-order")
    val adapter =
      DatasetSampleDomainAdapter
        .surface(dataset.voxelDomain.nVoxels, surface, expectedTopology, ordering)
        .fold(error => fail(error.message), identity)
    val expectedSamples =
      adapter.responseDomain(schemaId).fold(error => fail(error.message), identity)
    val volumeContext = volumeContextFor(dataset, schemaId)
    val expected =
      ResponseSchema
        .make(
          schemaId,
          volumeContext.expectedSchema.time,
          expectedSamples,
          volumeContext.expectedSchema.signal
        )
        .fold(error => fail(error.message), identity)
    val alignment =
      SampleAlignment
        .identity(expected.samples.count)
        .fold(error => fail(error.message), identity)
    val context =
      AcquisitionContext
        .make(
          dataset.id,
          DatasetKey.unsafe("sub-01"),
          dataset.timeAxis.runIds,
          ResponseKey.unsafe("surface"),
          expected,
          adapter,
          alignment
        )
        .fold(error => fail(error.message), identity)
    val actualSamples =
      SampleDomain
        .make(
          expected.samples.id,
          expected.samples.count,
          SampleDomainKind.Surface(surface, actualTopology, ordering)
        )
        .fold(error => fail(error.message), identity)
    val actualSchema =
      ResponseSchema
        .make(expected.id, expected.time, actualSamples, expected.signal)
        .fold(error => fail(error.message), identity)
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("surface-source"),
          actualSchema,
          PrimitiveBuffers.tabulate[Double](actualSchema.time.count * actualSchema.samples.count)(_.toDouble)
        )
        .fold(error => fail(error.message), identity)

    val issues =
      OpenedDataset
        .attach(dataset, source, context)
        .toEither
        .left
        .toOption
        .getOrElse(fail("expected topology mismatch"))
        .toNonEmptyList
        .toList
    assert(issues.exists(_.isInstanceOf[AttachmentIssue.SurfaceTopologyMismatch]))

  test("a read plan cannot execute against another attachment"):
    val synchronous = fixture()
    val firstSource = sourceFor(synchronous.dataset, "first")
    val secondSource = sourceFor(synchronous.dataset, "second")
    val context = volumeContext(synchronous.dataset)
    val first =
      OpenedDataset.attach(synchronous.dataset, firstSource, context).toEither.toOption.get
    val second =
      OpenedDataset.attach(synchronous.dataset, secondSource, context).toEither.toOption.get
    val plan =
      first
        .plan(DatasetRunQuery(run = Some(RunId("run-1"))))
        .fold(error => fail(error.message), identity)

    second.execute(plan).value.unsafeToFuture().map: evaluated =>
      assert(evaluated.left.exists(_.isInstanceOf[DatasetError.AttachmentPlanMismatch]))

  private def fixture(): SynchronousFmriDataset =
    val backend =
      InMemoryDatasetBackend(
        DatasetId("opened-fixture"),
        DMat.fromRows(
          Vector(
            Vector(0.0, 1.0, 2.0),
            Vector(10.0, 11.0, 12.0),
            Vector(20.0, 21.0, 22.0),
            Vector(30.0, 31.0, 32.0)
          )
        ),
        SampleSpaces(Vector(3, 1, 1))
      )
    FmriDataset.unsafe(
      backend,
      SamplingFrame(blockLens = Seq(2, 2), tr = Seq(1.0)),
      Vector(RunId("run-1"), RunId("run-2"))
    )

  private def sourceFor(
      dataset: FmriDataset,
      suffix: String = "fixture"
  ): InMemoryResponseSource[IO] =
    val schema =
      DatasetResponseSchema
        .fromDataset(
          dataset,
          ResponseSchemaId.unsafe("opened-schema"),
          UnitId.unsafe("percent-signal")
        )
        .fold(error => fail(error.message), identity)
    InMemoryResponseSource
      .copyFromRowMajor[IO](
        SourceId.unsafe(s"opened-source-$suffix"),
        schema,
        Array(
          0.0, 1.0, 2.0,
          10.0, 11.0, 12.0,
          20.0, 21.0, 22.0,
          30.0, 31.0, 32.0
        )
      )
      .fold(error => fail(error.message), identity)

  private def volumeContext(
      dataset: FmriDataset
  ): AcquisitionContext =
    volumeContextFor(dataset, ResponseSchemaId.unsafe("opened-schema"))

  private def volumeContextFor(
      dataset: FmriDataset,
      schemaId: ResponseSchemaId
  ): AcquisitionContext =
    AcquisitionContext
      .volume(
        dataset,
        DatasetKey.unsafe("sub-01"),
        ResponseKey.unsafe("bold"),
        schemaId,
        UnitId.unsafe("percent-signal")
      )
      .fold(error => fail(error.message), identity)
