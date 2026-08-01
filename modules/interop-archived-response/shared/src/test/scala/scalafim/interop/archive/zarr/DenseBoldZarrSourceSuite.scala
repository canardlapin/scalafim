package scalafim.interop.archive.zarr

import cats.data.EitherT
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.archive.{ArchiveLocation, ArchiveReadReceipt}
import scalafim.archive.zarr.{
  ProfileFixtures,
  TestZarrRuntime,
  ZarrArchiveDriver
}
import scalafim.interop.archive.{
  ArchiveDriverId,
  ArchiveDriverRegistration,
  ArchiveDrivers,
  ArchivedResponseRegistry,
  ScalafimRuntime
}
import scalafim.response.{
  OrderedIndices,
  PhysicalLocality,
  PhysicalReadUnit,
  ResolvedResponseSelection,
  SampleAxis,
  SelectionAxes,
  TimeAxis
}
import zarr4s.AsyncMemoryStore

import scala.concurrent.Future

class DenseBoldZarrSourceSuite extends munit.FunSuite:
  test("dense Zarr runtime obeys exact selected-read and receipt laws"):
    val store =
      AsyncMemoryStore(ProfileFixtures.completeStartIndexedStore)
        .fold(error => fail(error.message), identity)
    val driver =
      ZarrArchiveDriver.fixed[IO](store, TestZarrRuntime.runtime)
    val registration =
      ArchiveDriverRegistration.make(
        ArchiveDriverId.unsafe("neuroarchive-zarr"),
        driver,
        _ => true
      )
    val drivers =
      ArchiveDrivers
        .build(registration)
        .fold(error => fail(error.message), identity)
    val responses =
      ArchivedResponseRegistry
        .build(DenseBoldZarrRepresentationFamily[IO]())
        .fold(error => fail(error.message), identity)
    val runtime =
      ScalafimRuntime.make(drivers, responses)

    val result =
      runtime
        .openResponse(ArchiveLocation.unsafe("memory://dense-zarr"))
        .use: source =>
          val whole =
            ResolvedResponseSelection
              .all(source.schema)
              .fold(error => fail(error.message), identity)
          val selected =
            selection(
              source.schema,
              times = Vector(1, 0),
              samples = Vector(11, 0)
            )
          for
            wholeRead <- source.read(whole).leftMap(error =>
              scalafim.archive.ArchiveError.InvalidArchive(error.message)
            )
            planned <- EitherT.fromEither[IO](
              source
                .planned(selected)
                .left
                .map(error =>
                  scalafim.archive.ArchiveError.InvalidArchive(error.message)
                )
            )
            selectedRead <- planned.execute.leftMap(error =>
              scalafim.archive.ArchiveError.InvalidArchive(error.message)
            )
          yield (source, planned.summary, wholeRead, selectedRead)
        .value
        .map:
          case Left(error) =>
            fail(error.message)
          case Right((source, summary, whole, selected)) =>
            assertEquals(
              source.capabilities.forAxes(SelectionAxes.Time),
              Some(PhysicalLocality.ByteRangeBounded)
            )
            assertEquals(
              source.capabilities.forAxes(SelectionAxes.Samples),
              Some(PhysicalLocality.ByteRangeBounded)
            )
            assertEquals(
              source.capabilities.forAxes(SelectionAxes.TimeAndSamples),
              Some(PhysicalLocality.ByteRangeBounded)
            )
            assertEquals(
              summary.localityFor(SelectionAxes.TimeAndSamples),
              Some(PhysicalLocality.ByteRangeBounded)
            )
            assertEquals(summary.rows, 2)
            assertEquals(summary.columns, 2)
            assertEquals(selected.block.rows, 2)
            assertEquals(selected.block.columns, 2)

            val expected = Vector(3.75, 1.0, 0.75, -2.0)
            var index = 0
            while index < expected.length do
              assertEqualsDouble(
                selected.block(index / 2, index % 2),
                expected(index),
                0.0
              )
              index += 1

            assertEquals(
              selected.block(0, 0),
              whole.block(1, 11)
            )
            assertEquals(
              selected.block(0, 1),
              whole.block(1, 0)
            )
            assertEquals(
              selected.block(1, 0),
              whole.block(0, 11)
            )
            assertEquals(
              selected.block(1, 1),
              whole.block(0, 0)
            )
            assertEquals(selected.receipt.fallbacks, Vector.empty)
            assertEquals(
              selected.receipt.logical.timepoints,
              Vector(1, 0)
            )
            assertEquals(
              selected.receipt.logical.samples,
              Vector(11, 0)
            )
            val evidence = selected.receipt.physical.byAxes
            assertEquals(evidence.length, 1)
            assertEquals(
              evidence.head.observed,
              PhysicalLocality.ByteRangeBounded
            )
            assert(
              evidence.head.touched.forall(
                _.isInstanceOf[PhysicalReadUnit.Range]
              )
            )
            assert(
              evidence.head.covering.forall(
                _.isInstanceOf[PhysicalReadUnit.Range]
              )
            )
            assert(evidence.head.touched.nonEmpty)
            assert(evidence.head.covering.nonEmpty)

    run(result)

  test("dense descriptor rejects response-schema drift before source use"):
    val store =
      AsyncMemoryStore(ProfileFixtures.completeStartIndexedStore)
        .fold(error => fail(error.message), identity)
    val opened =
      ZarrArchiveDriver
        .fixed[IO](store, TestZarrRuntime.runtime)
        .open(ArchiveLocation.unsafe("memory://descriptor"))
        .use: archive =>
          EitherT.fromEither[IO](
            scalafim.interop.archive.RepresentationEnvelope
              .fromRevision(archive.revision)
          ).map: envelope =>
            val malformed =
              envelope.copy(outputSchema = scalafim.archive.CanonicalValue.Null)
            assert(DenseBoldZarrDescriptor.decode(malformed).isLeft)
        .value
        .map:
          case Left(error) =>
            fail(error.message)
          case Right(_) =>
            ()

    run(opened)

  private def selection(
      schema: scalafim.response.ResponseSchema,
      times: Vector[Int],
      samples: Vector[Int]
  ): ResolvedResponseSelection =
    val timepoints =
      OrderedIndices
        .fromInts[TimeAxis](
          schema.time.id,
          schema.time.count,
          times
        )
        .fold(error => fail(error.message), identity)
    val sampleIndices =
      OrderedIndices
        .fromInts[SampleAxis](
          schema.samples.id,
          schema.samples.count,
          samples
        )
        .fold(error => fail(error.message), identity)
    ResolvedResponseSelection
      .make(schema, timepoints, sampleIndices)
      .fold(error => fail(error.message), identity)

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()
