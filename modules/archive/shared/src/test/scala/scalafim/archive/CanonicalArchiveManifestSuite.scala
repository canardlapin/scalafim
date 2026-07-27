package scalafim.archive

import cats.data.EitherT
import cats.effect.{IO, Ref, Resource}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

import scala.concurrent.Future

class CanonicalArchiveManifestSuite extends munit.FunSuite:
  test("canonical manifest separates format object and representation versions"):
    val manifest = fixtureManifest()
    val rendered = CanonicalArchiveManifestCodec.render(manifest)
    val parsed =
      CanonicalArchiveManifestCodec
        .parse(rendered)
        .fold(error => fail(error.message), identity)

    assertEquals(parsed, manifest)
    assertEquals(parsed.format.name, "lna-hdf5")
    assertEquals(parsed.format.majorVersion, 2)
    assertEquals(parsed.key.objectType.namespace, "example.org")
    assertEquals(parsed.key.objectType.name, "fmri-response")
    assertEquals(parsed.key.schemaMajor, 3)
    assertEquals(
      parsed.representation.map(_.key.majorVersion),
      Some(7)
    )
    assertEquals(
      CanonicalArchiveManifestCodec.render(parsed),
      rendered
    )

    assert(
      CanonicalArchiveManifestCodec.parse(rendered + "trailing").isLeft
    )
    assert(ObjectTypeId.fromString("response").isLeft)
    assert(ArchiveFormatKey.fromString("lna-hdf5@02").isLeft)

  test("canonical values preserve raw scalar bits through manifest encoding"):
    val manifest = fixtureManifest()
    val parsed =
      CanonicalArchiveManifestCodec
        .parse(CanonicalArchiveManifestCodec.render(manifest))
        .fold(error => fail(error.message), identity)
    val attributes =
      parsed.attributes match
        case value: CanonicalValue.Object =>
          value
        case other =>
          fail(s"expected canonical object, found $other")
    val byKey =
      attributes.entries.map((key, value) => key.value -> value).toMap

    byKey("negative-zero") match
      case CanonicalValue.Float64Bits(bits) =>
        assertEquals(
          bits,
          java.lang.Double.doubleToRawLongBits(-0.0)
        )
      case other =>
        fail(s"expected float64 bits, found $other")
    byKey("nan") match
      case CanonicalValue.Float64Bits(bits) =>
        assertEquals(bits, 0x7ff8000000000042L)
      case other =>
        fail(s"expected NaN payload bits, found $other")

  test("canonical parser rejects truncated tagged and digest corruption"):
    val plain =
      CanonicalArchiveManifestCodec.render(fixtureManifest())
    val withIntegrity =
      fixtureManifestWithIntegrity()
    val protectedText =
      CanonicalArchiveManifestCodec.render(withIntegrity)
    val digestStart =
      protectedText.lastIndexOf("a" * 64)
    assert(digestStart >= 0)
    val floatMarker =
      plain.indexOf('D')
    assert(floatMarker >= 0)
    val corrupted =
      Vector(
        plain.dropRight(1),
        "BAD1" + plain.drop(4),
        plain.updated(floatMarker, 'X'),
        protectedText.updated(digestStart, 'g')
      )

    corrupted.foreach: fixture =>
      assert(
        CanonicalArchiveManifestCodec.parse(fixture).isLeft,
        s"corrupted manifest unexpectedly parsed: $fixture"
      )

  test("logical payload identity is independent of physical layout attributes"):
    val first =
      fixturePayloadDescriptor(
        CanonicalValue.string("chunks=4x4")
      )
    val second =
      fixturePayloadDescriptor(
        CanonicalValue.string("shard=64")
      )

    assertEquals(first.logicalIdentity, second.logicalIdentity)
    assertNotEquals(first, second)

  test("canonical writer publishes only after all admitted steps"):
    val program =
      for
        plan <- IO.fromEither(
          fixtureWritePlan().leftMap(error =>
            new IllegalArgumentException(error.message)
          )
        )
        prefixes <- plan.steps.indices.toVector.traverse: prefix =>
          for
            state <- Ref.of[IO, SinkState](SinkState.Empty)
            receipt <- CanonicalArchiveWriter
              .executeProperPrefix(plan, MemorySink(state), prefix)
              .value
            observed <- state.get
          yield
            assert(receipt.isRight)
            assertEquals(observed, SinkState.Empty)
        state <- Ref.of[IO, SinkState](SinkState.Empty)
        receipt <- CanonicalArchiveWriter
          .execute(plan, MemorySink(state))
          .value
        observed <- state.get
      yield
        assertEquals(prefixes.length, plan.steps.length)
        receipt match
          case Right(value) =>
            value.visibility match
              case CanonicalArchiveWriteVisibility.Published(digest) =>
                assertEquals(digest, PublishedDigest)
              case other =>
                fail(s"expected published receipt, found $other")
          case Left(error) =>
            fail(error.message)
        assert(observed.published)
        assertEquals(observed.manifest, Some(plan.encodedManifest))
        assertEquals(
          observed.payloads,
          plan.document.payloads.map(payload =>
            payload.descriptor.id -> payload.bytes
          ).toMap
        )

    run(program)

  test("canonical writer rejects unqualified payload roles"):
    val admitted = fixtureManifest()
    val unqualifiedDescriptor =
      PayloadDescriptor
        .from(
          PayloadId.unsafe("payload-01"),
          PayloadRoleId.unsafe("coefficients"),
          ScalarTypeId.unsafe("float64"),
          Vector(1L)
        )
        .fold(error => fail(error.message), identity)
    val unqualifiedManifest =
      ArchiveManifest
        .from(
          admitted.format,
          admitted.key,
          admitted.representation,
          admitted.attributes,
          Vector(unqualifiedDescriptor),
          admitted.integrity
        )
        .fold(error => fail(error.message), identity)
    val payload =
      CanonicalPayload
        .from(unqualifiedDescriptor, Vector[Byte](1))
        .fold(error => fail(error.message), identity)
    val document =
      CanonicalArchiveDocument
        .from(unqualifiedManifest, Vector(payload))
        .fold(error => fail(error.message), identity)

    CanonicalArchiveWritePlan.from(document) match
      case Left(ArchiveError.InvalidArchive(message)) =>
        assert(message.contains("namespaced payload role"))
      case other =>
        fail(s"expected rejected unqualified role, found $other")

  private def fixtureWritePlan()
      : Either[ArchiveError, CanonicalArchiveWritePlan] =
    val manifest = fixtureManifest()
    for
      payload <- CanonicalPayload.from(
        manifest.payloads.head,
        Vector[Byte](0, 0, 0, 0, 0, 0, 0, 1)
      )
      document <- CanonicalArchiveDocument.from(
        manifest,
        Vector(payload)
      )
      plan <- CanonicalArchiveWritePlan.from(document)
    yield plan

  private def fixtureManifest(): ArchiveManifest =
    val representation =
      RepresentationKey.unsafe("example.org/unknown-response@7")
    val persisted =
      PersistedRepresentation(
        representation,
        CanonicalValue.string("unknown-descriptor"),
        CanonicalValue.Null
      )
    val attributes =
      CanonicalValue.Object
        .from(Vector(
          CanonicalKey.unsafe("nan") ->
            CanonicalValue.float64Bits(0x7ff8000000000042L),
          CanonicalKey.unsafe("negative-zero") ->
            CanonicalValue.float64(-0.0),
          CanonicalKey.unsafe("maximum") ->
            CanonicalValue.int64(Long.MaxValue)
        ))
        .fold(error => fail(error.message), identity)
    val payload =
      fixturePayloadDescriptor(CanonicalValue.Object.empty)
    val key =
      ObjectKey
        .from(
          ObjectTypeId.unsafe("example.org/fmri-response"),
          schemaMajor = 3,
          Some(representation)
        )
        .fold(error => fail(error.message), identity)
    ArchiveManifest
      .from(
        ArchiveFormatKey.unsafe("lna-hdf5@2"),
        key,
        Some(persisted),
        attributes,
        Vector(payload),
        IntegrityManifest.Empty
      )
      .fold(error => fail(error.message), identity)

  private def fixtureManifestWithIntegrity(): ArchiveManifest =
    val plain = fixtureManifest()
    val integrity =
      IntegrityManifest
        .from(Vector(
          IntegrityEntry(
            plain.payloads.head.id,
            ContentDigest.unsafeSha256("a" * 64)
          )
        ))
        .fold(error => fail(error.message), identity)
    ArchiveManifest
      .from(
        plain.format,
        plain.key,
        plain.representation,
        plain.attributes,
        plain.payloads,
        integrity
      )
      .fold(error => fail(error.message), identity)

  private def fixturePayloadDescriptor(
      attributes: CanonicalValue
  ): PayloadDescriptor =
    PayloadDescriptor
      .from(
        PayloadId.unsafe("payload-01"),
        PayloadRoleId
          .from("example.org", "coefficients")
          .fold(error => fail(error.message), identity),
        ScalarTypeId.unsafe("float64"),
        Vector(1L),
        attributes
      )
      .fold(error => fail(error.message), identity)

  private val PublishedDigest =
    ContentDigest.unsafeSha256("0" * 64)

  private final case class SinkState(
      staging: Boolean,
      published: Boolean,
      payloads: Map[PayloadId, Vector[Byte]],
      manifest: Option[String]
  )

  private object SinkState:
    val Empty: SinkState =
      SinkState(
        staging = false,
        published = false,
        payloads = Map.empty,
        manifest = None
      )

  private final case class MemorySink(
      state: Ref[IO, SinkState]
  ) extends CanonicalArchiveSink[IO]:
    def stage(
        plan: CanonicalArchiveWritePlan
    ): ArchiveResource[IO, CanonicalArchiveTransaction[IO]] =
      val transaction =
        new CanonicalArchiveTransaction[IO]:
          def writePayload(
              payload: CanonicalPayload
          ): EitherT[IO, ArchiveError, Unit] =
            EitherT.liftF(
              state.update(current =>
                current.copy(
                  payloads =
                    current.payloads.updated(
                      payload.descriptor.id,
                      payload.bytes
                    )
                )
              )
            )

          def writeManifest(
              encoded: String
          ): EitherT[IO, ArchiveError, Unit] =
            EitherT.liftF(
              state.update(_.copy(manifest = Some(encoded)))
            )

          def publish: EitherT[IO, ArchiveError, ContentDigest] =
            EitherT:
              state.modify: current =>
                val expected =
                  plan.document.payloads.map(_.descriptor.id).toSet
                if current.payloads.keySet != expected ||
                    current.manifest != Some(plan.encodedManifest)
                then
                  current ->
                    Left(ArchiveError.IncompletePublication(
                      "memory sink lacks required staged content"
                    ))
                else
                  current.copy(
                    staging = false,
                    published = true
                  ) -> Right(PublishedDigest)

      val acquire =
        EitherT.liftF[IO, ArchiveError, CanonicalArchiveTransaction[IO]](
          state
            .set(SinkState.Empty.copy(staging = true))
            .as(transaction)
        )
      Resource
        .make(acquire): _ =>
          EitherT.liftF(
            state.update(current =>
              if current.published then current
              else SinkState.Empty
            )
          )

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()
