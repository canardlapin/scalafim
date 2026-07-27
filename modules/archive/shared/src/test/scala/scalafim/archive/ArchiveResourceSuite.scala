package scalafim.archive

import cats.arrow.FunctionK
import cats.data.EitherT
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.unsafe.implicits.global

import scala.concurrent.Future

class ArchiveResourceSuite extends munit.FunSuite:
  test("canonical values retain raw floating bits and UTF-8 object order"):
    val nanBits = 0x7ff8000000000042L
    val canonical =
      CanonicalValue.Object
        .from(Vector(
          CanonicalKey.unsafe("é") -> CanonicalValue.float64Bits(nanBits),
          CanonicalKey.unsafe("a") -> CanonicalValue.float64(-0.0),
          CanonicalKey.unsafe("z") -> CanonicalValue.int64(Long.MaxValue)
        ))
        .fold(error => fail(error.message), identity)

    assertEquals(canonical.entries.map(_._1.value), Vector("a", "z", "é"))
    canonical.entries.head._2 match
      case CanonicalValue.Float64Bits(bits) =>
        assertEquals(bits, java.lang.Double.doubleToRawLongBits(-0.0))
      case other => fail(s"expected floating bits, found $other")
    canonical.entries.last._2 match
      case CanonicalValue.Float64Bits(bits) =>
        assertEquals(bits, nanBits)
      case other => fail(s"expected NaN bits, found $other")
    assert(CanonicalValue.Object.from(Vector(
      CanonicalKey.unsafe("same") -> CanonicalValue.Null,
      CanonicalKey.unsafe("same") -> CanonicalValue.Null
    )).isLeft)

  test("unknown representations remain inspectable independently of publication state"):
    val unknown = RepresentationKey.unsafe("example.org/unknown-response@7")
    val revision = fixtureRevision(
      Some(unknown),
      PublicationStatus.Incomplete(IncompleteReason.unsafe("writer interrupted"))
    )

    assertEquals(revision.manifest.key.representation, Some(unknown))
    revision.publication match
      case PublicationStatus.Incomplete(reason) =>
        assertEquals(reason.value, "writer interrupted")
      case other => fail(s"expected incomplete publication, found $other")

    val resolution = revision.requireRepresentation(
      Vector(RepresentationKey.unsafe("org.scalafim/dense-bold@1"))
    )
    resolution match
      case Left(ArchiveError.UnsupportedRepresentation(found, supported)) =>
        assertEquals(found, unknown)
        assertEquals(supported.map(_.value), Vector("org.scalafim/dense-bold@1"))
      case other => fail(s"expected unsupported representation, found $other")

  test("ordered receipt accumulation conforms for mixed axes without locality ordering"):
    val payload = PayloadId.unsafe("bold")
    val objectId = PhysicalObjectId.unsafe("canonical/c/0")
    val otherObject = PhysicalObjectId.unsafe("canonical/c/1")
    val claim =
      LocalityClaim
        .chunkBounded(SelectionAxes.TimeAndSample, Vector(objectId))
        .fold(error => fail(error.message), identity)
    val summary = PayloadPlanSummary(payload, "mixed-selection", claim)
    val first =
      ReadObservation
        .byteRange(objectId, ByteInterval.unsafe(0L, 16L), 16L)
        .fold(error => fail(error.message), identity)
    val second =
      ReadObservation
        .byteRange(objectId, ByteInterval.unsafe(32L, 8L), 8L)
        .fold(error => fail(error.message), identity)
    val accumulator =
      ReceiptAccumulator.Empty.append(first).append(second).append(first)
    val receipt =
      ArchiveReadReceipt
        .from(summary, PhysicalLocality.ChunkBounded, accumulator)
        .fold(error => fail(error.message), identity)

    assertEquals(receipt.observations, Vector(first, second, first))
    assertEquals(receipt.physicalBytes, 40L)
    assert(ArchiveReadReceipt.from(
      summary,
      PhysicalLocality.ChunkBounded,
      ReceiptAccumulator.Empty
    ).isLeft)

    val invalid =
      ReadObservation
        .wholeObject(otherObject, 8L)
        .fold(error => fail(error.message), identity)
    assert(ArchiveReadReceipt.from(
      summary,
      PhysicalLocality.ChunkBounded,
      ReceiptAccumulator.Empty.append(invalid)
    ).isLeft)

  test("archive resources release after success, typed failure, and cancellation"):
    val program =
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        acquired <- Deferred[IO, Unit]
        driver = trackingDriver(events, acquired)
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
          .use(_ => EitherT.liftF[IO, ArchiveError, Unit](IO.never))
          .value
          .start
        _ <- acquired.get
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

  private def trackingDriver(
      events: Ref[IO, Vector[String]],
      acquired: Deferred[IO, Unit]
  ): ArchiveDriver[IO] =
    new ArchiveDriver[IO]:
      def open(location: ArchiveLocation): ArchiveResource[IO, OpenArchive[IO]] =
        val resource =
          Resource.make(
            events.update(_ :+ "acquire") *>
              (if location.value.endsWith("cancel") then acquired.complete(()).void
               else IO.unit)
                .as(testOpenArchive)
          )(_ => events.update(_ :+ "release"))
        resource.mapK:
          new FunctionK[IO, [A] =>> EitherT[IO, ArchiveError, A]]:
            def apply[A](effect: IO[A]): EitherT[IO, ArchiveError, A] =
              EitherT.liftF(effect)

  private def testOpenArchive: OpenArchive[IO] =
    new OpenArchive[IO]:
      val revision: ArchiveRevision =
        fixtureRevision(
          Some(RepresentationKey.unsafe("example.org/unknown-response@7")),
          PublicationStatus.Staging
        )
      val structure: ArchiveValidation =
        ArchiveValidation(ArchiveValidationScope.Structure, Vector.empty, Vector.empty)
      val payloads: PayloadExecutor[IO] =
        new PayloadExecutor[IO]:
          def execute[A](
              plan: PayloadPlan[A]
          ): EitherT[IO, ArchiveError, Observed[A]] =
            EitherT.leftT(ArchiveError.UnsupportedPayloadPlan("test", plan.summary.operation))
      val validateContents: EitherT[IO, ArchiveError, ArchiveValidation] =
        EitherT.rightT(
          ArchiveValidation(ArchiveValidationScope.Contents, Vector.empty, Vector.empty)
        )

  private def fixtureRevision(
      representation: Option[RepresentationKey],
      publication: PublicationStatus
  ): ArchiveRevision =
    val key =
      ObjectKey
        .from(ObjectTypeId.unsafe("example.org/response"), 1, representation)
        .fold(error => fail(error.message), identity)
    val manifest =
      ArchiveManifest
        .from(key, CanonicalValue.Object.empty, Vector.empty)
        .fold(error => fail(error.message), identity)
    ArchiveRevision
      .from(
        ArchiveRevisionId.unsafe("revision-1"),
        manifest,
        publication,
        ArchiveProvenance(CreatorId.unsafe("test"))
      )
      .fold(error => fail(error.message), identity)

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()
