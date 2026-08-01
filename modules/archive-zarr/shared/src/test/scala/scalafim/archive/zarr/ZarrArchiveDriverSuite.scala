package scalafim.archive.zarr

import cats.data.EitherT
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.unsafe.implicits.global
import scalafim.archive.{
  ArchiveError,
  ArchiveLocation,
  ArchiveValidationScope,
  ByteInterval,
  PhysicalObjectId,
  PublicationStatus,
  ReadObservation,
  SelectionAxes
}
import zarr4s.{
  ArraySelection,
  AsyncMemoryStore,
  AsyncObjectReader,
  Coordinate,
  PrimitiveBlock,
  ReadLimits,
  Region
}

import scala.concurrent.Future

class ZarrArchiveDriverSuite extends munit.FunSuite:
  test("typed Zarr execution preserves exact object ranges and byte accounting"):
    val store =
      AsyncMemoryStore(ProfileFixtures.completeStartIndexedStore)
        .fold(error => fail(error.message), identity)
    val descriptor = ProfileFixtures.descriptor(ProfileFixtures.startIndexedMetadata)
    val origin = Coordinate(0L, 0L, 0L, 0L)
      .fold(error => fail(error.message), identity)
    val region = Region.within(descriptor.shape, origin, descriptor.shape)
      .fold(error => fail(error.message), identity)
    val shard = PhysicalObjectId.unsafe("canonical/c/0/0/0/0")

    val program =
      ZarrArchiveDriver
        .fixed[IO](store, TestZarrRuntime.runtime)
        .open(ArchiveLocation.unsafe("memory://complete-zarr"))
        .use: opened =>
          val payload = opened.revision.manifest.payloads.head.id
          for
            plan <- EitherT.fromEither[IO](
              ZarrPayloadPlan.from(
                payload,
                ArraySelection.RegionSelection(region),
                SelectionAxes.TimeAndSample,
                Vector(shard)
              )
            )
            observed <- opened.payloads.execute(plan)
            contents <- opened.validateContents
          yield (opened.revision, opened.structure, observed, contents)
        .value
        .map:
          case Left(error) => fail(error.message)
          case Right((revision, structure, observed, contents)) =>
            assertEquals(structure.scope, ArchiveValidationScope.Structure)
            assertEquals(contents.scope, ArchiveValidationScope.Contents)
            assertEquals(
              revision.manifest.format.value,
              "neuroarchive-zarr@1"
            )
            assertEquals(
              revision.manifest.key.objectType.value,
              "org.scalafim/fmri-response"
            )
            assertEquals(
              revision.manifest.representation.map(_.key),
              Some(DenseBoldRevisionMetadata.Representation)
            )
            assertEquals(
              revision.manifest.payloads.map(_.role),
              Vector(DenseBoldRevisionMetadata.PayloadRole)
            )
            assertEquals(revision.manifest.integrity.entries, Vector.empty)
            assertEquals(contents.verifiedIntegrity, Vector.empty)
            revision.publication match
              case PublicationStatus.Published(digest) =>
                assertEquals(digest.algorithm, "sha256")
              case other => fail(s"expected published revision, found $other")

            observed.value.block match
              case PrimitiveBlock.Int16(values) =>
                assertEquals(values.toArray.toVector, Vector.tabulate(24)(_.toShort))
              case other => fail(s"expected int16 block, found $other")

            val ranges = observed.receipt.observations.collect:
              case ReadObservation.ByteRange(objectId, requested, bytes) =>
                (objectId.value, requested.offset, requested.length, bytes)
            assertEquals(
              ranges,
              Vector(
                ("canonical/c/0/0/0/0", 0L, 132L, 132L),
                ("canonical/c/0/0/0/0", 132L, 248L, 248L)
              )
            )
            assertEquals(observed.receipt.plan.locality.axes, SelectionAxes.TimeAndSample)
            assertEquals(observed.receipt.physicalBytes, 380L)
            assertEquals(observed.value.receipt.bytesRead, 380L)

    run(program)

  test("Zarr store resources release after success, typed failure, and cancellation"):
    val store =
      AsyncMemoryStore(ProfileFixtures.completeStartIndexedStore)
        .fold(error => fail(error.message), identity)
    val program =
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        bodyEntered <- Deferred[IO, Unit]
        driver = ZarrArchiveDriver.using[IO](
          _ =>
            Resource.make(
              events.update(_ :+ "acquire").as(store: AsyncObjectReader)
            )(_ => events.update(_ :+ "release")),
          TestZarrRuntime.runtime
        )
        success <- driver
          .open(ArchiveLocation.unsafe("memory://success"))
          .use(_ => EitherT.liftF(events.update(_ :+ "success")))
          .value
        failure <- driver
          .open(ArchiveLocation.unsafe("memory://failure"))
          .use(_ => EitherT.leftT[IO, Unit](ArchiveError.InvalidArchive("expected")))
          .value
        fiber <- driver
          .open(ArchiveLocation.unsafe("memory://cancel"))
          .use: _ =>
            EitherT.liftF[IO, ArchiveError, Unit](
              bodyEntered.complete(()).void *> IO.never
            )
          .value
          .start
        _ <- bodyEntered.get
        _ <- fiber.cancel
        observed <- events.get
      yield
        assertEquals(success, Right(()))
        assert(failure.isLeft)
        assertEquals(
          observed,
          Vector(
            "acquire",
            "success",
            "release",
            "acquire",
            "release",
            "acquire",
            "release"
          )
        )

    run(program)

  test("incomplete Zarr publication is a typed open error"):
    val incomplete =
      ProfileFixtures.completeStartIndexedStore - "canonical/c/0/0/0/0"
    val store = AsyncMemoryStore(incomplete).fold(error => fail(error.message), identity)
    val result =
      ZarrArchiveDriver
        .fixed[IO](store, TestZarrRuntime.runtime)
        .open(ArchiveLocation.unsafe("memory://incomplete"))
        .use(_ => EitherT.rightT[IO, ArchiveError](()))
        .value
        .map:
          case Left(_: ArchiveError.IncompletePublication) => ()
          case other => fail(s"expected incomplete publication, found $other")

    run(result)

  test("receipt-bearing Zarr plans reject nondeterministic parallel attempt order"):
    val payload = scalafim.archive.PayloadId.unsafe("bold")
    val shard = PhysicalObjectId.unsafe("canonical/c/0/0/0/0")
    val descriptor = ProfileFixtures.descriptor(ProfileFixtures.startIndexedMetadata)
    val origin = Coordinate(0L, 0L, 0L, 0L)
      .fold(error => fail(error.message), identity)
    val region = Region.within(descriptor.shape, origin, descriptor.shape)
      .fold(error => fail(error.message), identity)
    val result = ZarrPayloadPlan.from(
      payload,
      ArraySelection.RegionSelection(region),
      SelectionAxes.TimeAndSample,
      Vector(shard),
      ReadLimits(maxConcurrentRequests = 2)
    )
    assert(result.isLeft)

  test("byte-range-bounded Zarr plans preserve their exact range evidence"):
    val store =
      AsyncMemoryStore(ProfileFixtures.completeStartIndexedStore)
        .fold(error => fail(error.message), identity)
    val descriptor =
      ProfileFixtures.descriptor(ProfileFixtures.startIndexedMetadata)
    val origin = Coordinate(0L, 0L, 0L, 0L)
      .fold(error => fail(error.message), identity)
    val region = Region.within(descriptor.shape, origin, descriptor.shape)
      .fold(error => fail(error.message), identity)
    val shard = PhysicalObjectId.unsafe("canonical/c/0/0/0/0")
    val covering =
      scalafim.archive.ObjectRange(
        shard,
        ByteInterval.unsafe(0L, 380L)
      )
    val result =
      ZarrArchiveDriver
        .fixed[IO](store, TestZarrRuntime.runtime)
        .open(ArchiveLocation.unsafe("memory://range-bounded"))
        .use: opened =>
          for
            plan <- EitherT.fromEither[IO](
              ZarrPayloadPlan.byteRangeBounded(
                opened.revision.manifest.payloads.head.id,
                ArraySelection.RegionSelection(region),
                SelectionAxes.TimeAndSample,
                Vector(covering)
              )
            )
            observed <- opened.payloads.execute(plan)
          yield observed.receipt
        .value
        .map:
          case Left(error) =>
            fail(error.message)
          case Right(receipt) =>
            assertEquals(
              receipt.observedLocality,
              scalafim.archive.PhysicalLocality.ByteRangeBounded
            )
            assertEquals(
              receipt.observations.collect:
                case ReadObservation.ByteRange(_, interval, _) =>
                  interval.offset -> interval.length
              ,
              Vector(0L -> 132L, 132L -> 248L)
            )

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()
