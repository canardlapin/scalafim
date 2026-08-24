package scalafim.interop.archive

import scalafim.image.SampleSpaces

import cats.arrow.FunctionK
import cats.data.EitherT
import cats.effect.{Deferred, IO, Ref, Resource}
import cats.effect.unsafe.implicits.global
import scalafim.archive.{
  ArchiveDriver,
  ArchiveError,
  ArchiveLocation,
  ArchiveManifest,
  ArchiveProvenance,
  ArchiveResource,
  ArchiveRevision,
  ArchiveRevisionId,
  ArchiveValidation,
  ArchiveValidationScope,
  CanonicalValue,
  ContentDigest,
  CreatorId,
  ObjectKey,
  ObjectTypeId,
  OpenArchive,
  PayloadExecutor,
  PayloadPlan,
  PublicationStatus,
  RepresentationKey
}
import scalafim.dataset.{
  AcquisitionContext,
  DatasetId,
  DatasetKey,
  DatasetResponseSchema,
  DatasetRunQuery,
  FmriDataset,
  InMemoryDatasetBackend,
  ResponseKey,
  RunId
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.image.{DMat, SomeSampleSpace}
import scalafim.response.{
  CalibrationState,
  DomainId,
  DomainReference,
  InMemoryResponseSource,
  NonFinitePolicy,
  ResponseSchema,
  ResponseSchemaId,
  ResponseSource,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  SignalSchema,
  SourceId,
  TimeAxis,
  TimeDomain,
  UnitId
}

import scala.concurrent.Future

class ArchivedResponseRuntimeSuite extends munit.FunSuite:
  test("representation keys are namespaced and major-versioned"):
    val key =
      RepresentationKey
        .from("example.org", "temporal-dct", 3)
        .fold(error => fail(error.message), identity)

    assertEquals(key.value, "example.org/temporal-dct@3")
    assertEquals(key.namespace, "example.org")
    assertEquals(key.name, "temporal-dct")
    assertEquals(key.majorVersion, 3)
    assert(RepresentationKey.fromString("temporal-dct").isLeft)
    assert(RepresentationKey.fromString("example.org/TemporalDct@1").isLeft)
    assert(RepresentationKey.fromString("example.org/temporal-dct@0").isLeft)
    assert(RepresentationKey.fromString("example.org/temporal-dct@01").isLeft)

  test("response and driver registries reject duplicates independent of order"):
    val firstKey = RepresentationKey.unsafe("example.org/alpha@1")
    val secondKey = RepresentationKey.unsafe("example.org/beta@2")
    val source = fixtureSource
    val first = fixedFamily(firstKey, source)
    val second = fixedFamily(secondKey, source)

    val forward =
      ArchivedResponseRegistry
        .build(first, second)
        .fold(error => fail(error.message), identity)
    val reverse =
      ArchivedResponseRegistry
        .build(second, first)
        .fold(error => fail(error.message), identity)

    assertEquals(
      forward.supportedKeys.map(_.value),
      Vector("example.org/alpha@1", "example.org/beta@2")
    )
    assertEquals(
      reverse.supportedKeys.map(_.value),
      forward.supportedKeys.map(_.value)
    )
    assertEquals(forward.resolve(secondKey).map(_.key), Right(secondKey))
    ArchivedResponseRegistry.build(first, first) match
      case Left(RuntimeRegistryError.DuplicateRepresentation(key)) =>
        assertEquals(key, firstKey)
      case other =>
        fail(s"expected duplicate representation, found $other")

    val driver = fixedDriver(fixtureOpenArchive(firstKey))
    val registration =
      ArchiveDriverRegistration.make(
        ArchiveDriverId.unsafe("memory"),
        driver,
        _ => true
      )
    ArchiveDrivers.build(registration, registration) match
      case Left(RuntimeRegistryError.DuplicateArchiveDriver(id)) =>
        assertEquals(id.value, "memory")
      case other =>
        fail(s"expected duplicate driver, found $other")

  test("unknown installed code differs from archive corruption"):
    val installedKey = RepresentationKey.unsafe("example.org/installed@1")
    val unknownKey = RepresentationKey.unsafe("example.org/unknown@7")
    val source = fixtureSource
    val registry =
      ArchivedResponseRegistry
        .build(fixedFamily(installedKey, source))
        .fold(error => fail(error.message), identity)

    val unknownRuntime =
      runtime(fixedDriver(fixtureOpenArchive(unknownKey)), registry)
    val corruptRuntime =
      runtime(
        fixedDriver(fixtureOpenArchive(
          unknownKey,
          structureScope = ArchiveValidationScope.Contents
        )),
        registry
      )
    val stagingRuntime =
      runtime(
        fixedDriver(fixtureOpenArchive(
          unknownKey,
          publication = PublicationStatus.Staging
        )),
        registry
      )
    val program =
      for
        unknown <- unknownRuntime
          .openResponse(ArchiveLocation.unsafe("memory://unknown"))
          .use(_ => EitherT.rightT[IO, ArchiveError](()))
          .value
        corrupt <- corruptRuntime
          .openResponse(ArchiveLocation.unsafe("memory://corrupt"))
          .use(_ => EitherT.rightT[IO, ArchiveError](()))
          .value
        staging <- stagingRuntime
          .openResponse(ArchiveLocation.unsafe("memory://staging"))
          .use(_ => EitherT.rightT[IO, ArchiveError](()))
          .value
      yield
        unknown match
          case Left(ArchiveError.UnsupportedRepresentation(found, supported)) =>
            assertEquals(found, unknownKey)
            assertEquals(supported, Vector(installedKey))
          case other =>
            fail(s"expected unsupported representation, found $other")
        corrupt match
          case Left(_: ArchiveError.InvalidArchive) =>
            ()
          case other =>
            fail(s"expected structural corruption, found $other")
        staging match
          case Left(_: ArchiveError.IncompletePublication) =>
            ()
          case other =>
            fail(s"expected publication failure before lookup, found $other")

    run(program)

  test("runtime composes and releases driver and response resources"):
    val key = RepresentationKey.unsafe("example.org/tracked@1")
    val program =
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        entered <- Deferred[IO, Unit]
        source = fixtureSource
        driver = trackingDriver(events, fixtureOpenArchive(key))
        family = trackingFamily(key, source, events)
        registry = ArchivedResponseRegistry
          .build(family)
          .fold(error => throw new IllegalArgumentException(error.message), identity)
        subject = runtime(driver, registry)
        success <- subject
          .openResponse(ArchiveLocation.unsafe("memory://success"))
          .use(opened =>
            EitherT.liftF(events.update(_ :+ "use-success").as(opened.sourceId))
          )
          .value
        failure <- subject
          .openResponse(ArchiveLocation.unsafe("memory://failure"))
          .use(_ =>
            EitherT.leftT[IO, Unit](ArchiveError.InvalidArchive("expected"))
          )
          .value
        fiber <- subject
          .openResponse(ArchiveLocation.unsafe("memory://cancel"))
          .use: _ =>
            EitherT.liftF[IO, ArchiveError, Unit](
              entered.complete(()).void *> IO.never
            )
          .value
          .start
        _ <- entered.get
        _ <- fiber.cancel
        observed <- events.get
      yield
        assertEquals(success, Right(source.sourceId))
        assert(failure.isLeft)
        assertEquals(
          observed,
          Vector(
            "driver-acquire",
            "family-acquire",
            "use-success",
            "family-release",
            "driver-release",
            "driver-acquire",
            "family-acquire",
            "family-release",
            "driver-release",
            "driver-acquire",
            "family-acquire",
            "family-release",
            "driver-release"
          )
        )

    run(program)

  test("family acquisition failure releases the already opened archive"):
    val key = RepresentationKey.unsafe("example.org/failing@1")
    val program =
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        driver = trackingDriver(events, fixtureOpenArchive(key))
        family = new ArchivedResponseFamily[IO]:
          val key: RepresentationKey =
            RepresentationKey.unsafe("example.org/failing@1")
          def open(
              envelope: RepresentationEnvelope,
              archive: ArchiveResponseAccess[IO]
          ): ArchiveResource[IO, ResponseSource[IO]] =
            Resource.eval(EitherT.leftT(
              ArchiveError.InvalidArchive("family rejected descriptor")
            ))
        registry = ArchivedResponseRegistry
          .build(family)
          .fold(error => throw new IllegalArgumentException(error.message), identity)
        subject = runtime(driver, registry)
        result <- subject
          .openResponse(ArchiveLocation.unsafe("memory://failure"))
          .use(_ => EitherT.rightT[IO, ArchiveError](()))
          .value
        observed <- events.get
      yield
        assert(result.isLeft)
        assertEquals(observed, Vector("driver-acquire", "driver-release"))

    run(program)

  test("runtime owns checked dataset attachment through success, failure, and cancellation"):
    val key = RepresentationKey.unsafe("example.org/dataset@1")
    val dataset = fixtureDataset
    val schemaId = ResponseSchemaId.unsafe("runtime-dataset-schema")
    val source = fixtureDatasetSource(dataset, schemaId)
    val acquisition = fixtureAcquisition(dataset, schemaId)
    val mismatchedAcquisition =
      fixtureAcquisition(
        dataset,
        ResponseSchemaId.unsafe("runtime-dataset-other-schema")
      )
    val program =
      for
        events <- Ref.of[IO, Vector[String]](Vector.empty)
        entered <- Deferred[IO, Unit]
        driver = trackingDriver(events, fixtureOpenArchive(key))
        family = trackingFamily(key, source, events)
        registry = ArchivedResponseRegistry
          .build(family)
          .fold(error => throw new IllegalArgumentException(error.message), identity)
        subject = runtime(driver, registry)
        success <- subject
          .openDataset(
            ArchiveLocation.unsafe("memory://dataset-success"),
            dataset,
            acquisition
          )
          .use: opened =>
            EitherT.liftF[IO, RuntimeOpenError, Either[scalafim.dataset.DatasetError, scalafim.dataset.DatasetReadResult]](
              events.update(_ :+ "use-dataset") *>
                opened
                  .read(
                    DatasetRunQuery(run = Some(RunId("run-1")))
                  )
                  .value
            )
          .value
        attachmentFailure <- subject
          .openDataset(
            ArchiveLocation.unsafe("memory://dataset-mismatch"),
            dataset,
            mismatchedAcquisition
          )
          .use(_ => EitherT.rightT[IO, RuntimeOpenError](()))
          .value
        fiber <- subject
          .openDataset(
            ArchiveLocation.unsafe("memory://dataset-cancel"),
            dataset,
            acquisition
          )
          .use: _ =>
            EitherT.liftF[IO, RuntimeOpenError, Unit](
              entered.complete(()).void *> IO.never
            )
          .value
          .start
        _ <- entered.get
        _ <- fiber.cancel
        observed <- events.get
      yield
        val result =
          success match
            case Right(Right(found)) =>
              found
            case other =>
              fail(s"expected successful dataset read, found $other")
        val block = result.segments.head.response.block
        assertEquals(block.rows, 1)
        assertEquals(block.columns, 1)
        assertEqualsDouble(block(0, 0), 42.0, 0.0)
        attachmentFailure match
          case Left(RuntimeOpenError.Attachment(issues)) =>
            assert(
              issues.toNonEmptyList.toList.exists(
                _.isInstanceOf[scalafim.dataset.AttachmentIssue.SchemaMismatch]
              )
            )
          case other =>
            fail(s"expected typed attachment failure, found $other")
        assertEquals(
          observed,
          Vector(
            "driver-acquire",
            "family-acquire",
            "use-dataset",
            "family-release",
            "driver-release",
            "driver-acquire",
            "family-acquire",
            "family-release",
            "driver-release",
            "driver-acquire",
            "family-acquire",
            "family-release",
            "driver-release"
          )
        )

    run(program)

  test("dataset opening preserves archive failure identity"):
    val dataset = fixtureDataset
    val schemaId = ResponseSchemaId.unsafe("runtime-dataset-schema")
    val source = fixtureDatasetSource(dataset, schemaId)
    val family =
      fixedFamily(
        RepresentationKey.unsafe("example.org/dataset@1"),
        source
      )
    val registry =
      ArchivedResponseRegistry
        .build(family)
        .fold(error => fail(error.message), identity)
    val drivers =
      ArchiveDrivers
        .build[IO]()
        .fold(error => fail(error.message), identity)
    val subject = ScalafimRuntime.make(drivers, registry)
    val result =
      subject
        .openDataset(
          ArchiveLocation.unsafe("memory://missing-driver"),
          dataset,
          fixtureAcquisition(dataset, schemaId)
        )
        .use(_ => EitherT.rightT[IO, RuntimeOpenError](()))
        .value

    run(
      result.map:
        case Left(RuntimeOpenError.Archive(ArchiveError.UnsupportedStorage(_))) =>
          ()
        case other =>
          fail(s"expected a preserved archive failure, found $other")
    )

  private def runtime(
      driver: ArchiveDriver[IO],
      registry: ArchivedResponseRegistry[IO]
  ): ScalafimRuntime[IO] =
    val registration =
      ArchiveDriverRegistration.make(
        ArchiveDriverId.unsafe("memory"),
        driver,
        _ => true
      )
    val drivers =
      ArchiveDrivers
        .build(registration)
        .fold(error => fail(error.message), identity)
    ScalafimRuntime.make(drivers, registry)

  private def fixedFamily(
      representation: RepresentationKey,
      source: ResponseSource[IO]
  ): ArchivedResponseFamily[IO] =
    new ArchivedResponseFamily[IO]:
      val key: RepresentationKey =
        representation
      def open(
          envelope: RepresentationEnvelope,
          archive: ArchiveResponseAccess[IO]
      ): ArchiveResource[IO, ResponseSource[IO]] =
        Resource.pure(source)

  private def trackingFamily(
      representation: RepresentationKey,
      source: ResponseSource[IO],
      events: Ref[IO, Vector[String]]
  ): ArchivedResponseFamily[IO] =
    new ArchivedResponseFamily[IO]:
      val key: RepresentationKey =
        representation
      def open(
          envelope: RepresentationEnvelope,
          archive: ArchiveResponseAccess[IO]
      ): ArchiveResource[IO, ResponseSource[IO]] =
        Resource
          .make(events.update(_ :+ "family-acquire").as(source))(
            _ => events.update(_ :+ "family-release")
          )
          .mapK(liftErrors)

  private def fixedDriver(
      opened: OpenArchive[IO]
  ): ArchiveDriver[IO] =
    new ArchiveDriver[IO]:
      def open(
          location: ArchiveLocation
      ): ArchiveResource[IO, OpenArchive[IO]] =
        Resource.pure(opened)

  private def trackingDriver(
      events: Ref[IO, Vector[String]],
      opened: OpenArchive[IO]
  ): ArchiveDriver[IO] =
    new ArchiveDriver[IO]:
      def open(
          location: ArchiveLocation
      ): ArchiveResource[IO, OpenArchive[IO]] =
        Resource
          .make(events.update(_ :+ "driver-acquire").as(opened))(
            _ => events.update(_ :+ "driver-release")
          )
          .mapK(liftErrors)

  private val liftErrors =
    new FunctionK[IO, [A] =>> EitherT[IO, ArchiveError, A]]:
      def apply[A](effect: IO[A]): EitherT[IO, ArchiveError, A] =
        EitherT.liftF(effect)

  private def fixtureOpenArchive(
      representation: RepresentationKey,
      structureScope: ArchiveValidationScope =
        ArchiveValidationScope.Structure,
      publication: PublicationStatus =
        PublicationStatus.Published(ContentDigest.unsafeSha256("0" * 64))
  ): OpenArchive[IO] =
    new OpenArchive[IO]:
      val revision: ArchiveRevision =
        fixtureRevision(representation, publication)
      val structure: ArchiveValidation =
        ArchiveValidation(structureScope, Vector.empty, Vector.empty)
      val payloads: PayloadExecutor[IO] =
        new PayloadExecutor[IO]:
          def execute[A](
              plan: PayloadPlan[A]
          ): EitherT[IO, ArchiveError, scalafim.archive.Observed[A]] =
            EitherT.leftT(ArchiveError.UnsupportedPayloadPlan(
              "memory",
              plan.summary.operation
            ))
      val validateContents
          : EitherT[IO, ArchiveError, ArchiveValidation] =
        EitherT.rightT(
          ArchiveValidation(
            ArchiveValidationScope.Contents,
            Vector.empty,
            Vector.empty
          )
        )

  private def fixtureRevision(
      representation: RepresentationKey,
      publication: PublicationStatus
  ): ArchiveRevision =
    val key =
      ObjectKey
        .from(
          ObjectTypeId.unsafe("example.org/response"),
          1,
          Some(representation)
        )
        .fold(error => fail(error.message), identity)
    val manifest =
      ArchiveManifest
        .from(key, CanonicalValue.Object.empty, Vector.empty)
        .fold(error => fail(error.message), identity)
    ArchiveRevision
      .from(
        ArchiveRevisionId.unsafe("runtime-revision"),
        manifest,
        publication,
        ArchiveProvenance(CreatorId.unsafe("test"))
      )
      .fold(error => fail(error.message), identity)

  private def fixtureSource: InMemoryResponseSource[IO] =
    val time =
      TimeDomain
        .regular(
          DomainId.unsafe[TimeAxis]("runtime-time"),
          0.0,
          1.0,
          1,
          UnitId.unsafe("second")
        )
        .fold(error => fail(error.message), identity)
    val samples =
      SampleDomain
        .make(
          DomainId.unsafe[SampleAxis]("runtime-samples"),
          1,
          SampleDomainKind.Volume(
            DomainReference.unsafe("test-space", "runtime"),
            None,
            DomainReference.unsafe("test-order", "linear")
          )
        )
        .fold(error => fail(error.message), identity)
    val schema =
      ResponseSchema
        .make(
          ResponseSchemaId.unsafe("runtime-schema"),
          time,
          samples,
          SignalSchema(
            UnitId.unsafe("unit"),
            CalibrationState.Applied,
            NonFinitePolicy.Reject
          )
        )
        .fold(error => fail(error.message), identity)
    InMemoryResponseSource
      .copyFromRowMajor[IO](
        SourceId.unsafe("runtime-source"),
        schema,
        Array[Double](1.0)
      )
      .fold(error => fail(error.message), identity)

  private def fixtureDataset: FmriDataset =
    val backend =
      InMemoryDatasetBackend(
        DatasetId("runtime-dataset"),
        DMat.fromRows(Vector(Vector(42.0))),
        SampleSpaces(Vector(1, 1, 1))
      )
    FmriDataset.unsafe(
      backend,
      SamplingFrame(blockLens = Seq(1), tr = Seq(1.0)),
      RunId("run-1")
    )

  private def fixtureAcquisition(
      dataset: FmriDataset,
      schemaId: ResponseSchemaId
  ): AcquisitionContext =
    AcquisitionContext
      .volume(
        dataset,
        DatasetKey.unsafe("sub-01"),
        ResponseKey.unsafe("bold"),
        schemaId,
        UnitId.unsafe("unit")
      )
      .fold(error => fail(error.message), identity)

  private def fixtureDatasetSource(
      dataset: FmriDataset,
      schemaId: ResponseSchemaId
  ): InMemoryResponseSource[IO] =
    val schema =
      DatasetResponseSchema
        .fromDataset(dataset, schemaId, UnitId.unsafe("unit"))
        .fold(error => fail(error.message), identity)
    InMemoryResponseSource
      .copyFromRowMajor[IO](
        SourceId.unsafe("runtime-dataset-source"),
        schema,
        Array[Double](42.0)
      )
      .fold(error => fail(error.message), identity)

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()
