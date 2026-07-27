package scalafim.interop.archive.lna

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.archive.{
  ArchiveLocation,
  RepresentationMetadata,
  PhysicalLocality as ArchivePhysicalLocality
}
import scalafim.archive.io.LnaHdf5Store
import scalafim.interop.archive.{
  ArchiveDrivers,
  ArchivedResponseRegistry,
  ScalafimRuntime
}
import scalafim.response.{
  DecodeConsistency,
  PhysicalByteEvidence,
  ResolvedResponseSelection,
  SourceId
}

import java.nio.file.{Files, Path}

class TemporalDctLnaHdf5Suite extends munit.FunSuite:
  test("HDF5 source agrees with in-memory selected decode and retains receipts"):
    val fixture = TemporalDctLnaFixtures.fixture("hdf5-read")
    val requested =
      TemporalDctLnaFixtures.selection(
        fixture.schema,
        Vector(3, 1),
        Vector(2, 0)
      )
    val expected =
      fixture.materialized
        .decode(requested)
        .fold(error => fail(error.message), identity)
    withTemporaryDirectory("scalafim-rra4-read-"): directory =>
      val path = directory.resolve("response.lna.h5")
      val location = ArchiveLocation.unsafe(path.toString)
      val plan =
        TemporalDctLnaWritePlan
          .create(fixture.materialized, fixture.space)
          .fold(error => fail(error.message), identity)

      val program =
        for
          write <- TemporalDctLnaHdf5Writer
            .default[IO]
            .execute(location, plan)
            .leftMap(error =>
              scalafim.archive.ArchiveError.InvalidArchive(error.message)
            )
          observed <- TemporalDctLnaHdf5
            .open[IO](
              location,
              SourceId.unsafe("hdf5-temporal-dct"),
              fixture.model
            )
            .use: source =>
              source
                .plan(requested)
                .fold(
                  error => cats.data.EitherT.leftT[IO, ArchivedTemporalDctRead](
                    scalafim.archive.ArchiveError.InvalidArchive(error.message)
                  ),
                  source.executeObserved
                )
        yield (write, observed)

      program.value.unsafeRunSync() match
        case Left(error) =>
          fail(error.message)
        case Right((write, observed)) =>
          write.visibility match
            case TemporalDctLnaWriteVisibility.Published(_) =>
              ()
            case other =>
              fail(s"expected published write receipt, got $other")
          assertEquals(observed.nativeReceipts.length, 3)
          assert(
            observed.nativeReceipts.forall(
              _.observedLocality == ArchivePhysicalLocality.WholePayload
            )
          )
          assertEquals(
            observed.result.receipt.physical.physicalBytes,
            PhysicalByteEvidence.Known(248L)
          )
          assertBlockAgreesExactly(observed.result.block, expected)

  test("every proper write-plan prefix leaves the destination absent"):
    val fixture = TemporalDctLnaFixtures.fixture("hdf5-prefix")
    val plan =
      TemporalDctLnaWritePlan
        .create(fixture.materialized, fixture.space)
        .fold(error => fail(error.message), identity)
    withTemporaryDirectory("scalafim-rra4-prefix-"): directory =>
      var completed = 0
      while completed < plan.steps.length do
        val path = directory.resolve(s"interrupted-$completed.lna.h5")
        val location = ArchiveLocation.unsafe(path.toString)
        val result =
          TemporalDctLnaHdf5Writer
            .default[IO]
            .executeInterruptedAfter(location, plan, completed)
            .value
            .unsafeRunSync()
        result match
          case Left(TemporalDctLnaWriteError.Interrupted(receipt)) =>
            assertEquals(receipt.completed, plan.steps.take(completed))
            assertEquals(
              receipt.visibility,
              TemporalDctLnaWriteVisibility.Absent
            )
          case other =>
            fail(s"expected interruption after prefix $completed, got $other")
        assert(!Files.exists(path))
        completed += 1

  test("HDF5 round trip preserves raw logical payload scalar bits"):
    val fixture = TemporalDctLnaFixtures.fixture("hdf5-bits")
    val signedZeroBasis = fixture.materialized.basis.rowMajorCopy
    signedZeroBasis(0) = -0.0
    val basis =
      scalafim.latent.TemporalBasisValues
        .copyFromRowMajor(
          fixture.materialized.basis.rows,
          fixture.materialized.basis.components,
          signedZeroBasis
        )
        .fold(error => fail(error.message), identity)
    val value =
      scalafim.latent.TemporalDctRepresentation
        .materialize(
          fixture.model,
          basis,
          fixture.materialized.loadings,
          fixture.materialized.offset
        )
        .fold(error => fail(error.message), identity)
    val plan =
      TemporalDctLnaWritePlan
        .create(value, fixture.space)
        .fold(error => fail(error.message), identity)
    withTemporaryDirectory("scalafim-rra4-bits-"): directory =>
      val path = directory.resolve("bits.lna.h5")
      val location = ArchiveLocation.unsafe(path.toString)
      TemporalDctLnaHdf5Writer
        .default[IO]
        .execute(location, plan)
        .value
        .unsafeRunSync()
        .fold(error => fail(error.message), identity)
      val stored =
        LnaHdf5Store.default
          .read(path)
          .fold(error => fail(error.message), identity)
      assertEquals(
        java.lang.Double.doubleToRawLongBits(
          stored
            .payload(
              stored.manifest.datasets
                .find(_.role == scalafim.archive.lna.DatasetRole.TemporalBasis)
                .get
                .path
            )
            .collect:
              case scalafim.archive.lna.Payload.DoubleMatrix(data, _) =>
                data(0, 0)
            .get
        ),
        java.lang.Double.doubleToRawLongBits(-0.0)
      )

      plan.archive.manifest.datasets.foreach: reference =>
        val expected = plan.archive.payload(reference.path).get
        val actual = stored.payload(reference.path).get
        assertPayloadRawBits(actual, expected)

  test("runtime opens temporal DCT from its narrow persisted descriptor"):
    val fixture = TemporalDctLnaFixtures.fixture("runtime-temporal-dct")
    val requested =
      TemporalDctLnaFixtures.selection(
        fixture.schema,
        Vector(2, 0),
        Vector(1, 2)
      )
    val expected =
      fixture.materialized
        .decode(requested)
        .fold(error => fail(error.message), identity)
    val plan =
      TemporalDctLnaWritePlan
        .create(fixture.materialized, fixture.space)
        .fold(error => fail(error.message), identity)
    withTemporaryDirectory("scalafim-rra5-runtime-"): directory =>
      val location =
        ArchiveLocation.unsafe(directory.resolve("response.lna.h5").toString)
      val runtime = temporalRuntime
      val program =
        for
          _ <- TemporalDctLnaHdf5Writer
            .default[IO]
            .execute(location, plan)
            .leftMap(error =>
              scalafim.archive.ArchiveError.InvalidArchive(error.message)
            )
          result <- runtime
            .openResponse(location)
            .use(
              _.read(requested).leftMap(error =>
                scalafim.archive.ArchiveError.InvalidArchive(error.message)
              )
            )
        yield result

      program.value.unsafeRunSync() match
        case Left(error) =>
          fail(error.message)
        case Right(result) =>
          assertEquals(result.block.selection, requested)
          assertBlockAgreesExactly(result.block, expected)

  test("named legacy family opens an un-enveloped LNA fixture"):
    val fixture = TemporalDctLnaFixtures.fixture("runtime-legacy")
    val plan =
      TemporalDctLnaWritePlan
        .create(fixture.materialized, fixture.space)
        .fold(error => fail(error.message), identity)
    val legacy =
      plan.archive.copy(
        manifest = plan.archive.manifest.copy(
          header = plan.archive.manifest.header --
            Vector(
              RepresentationMetadata.DescriptorHeader,
              RepresentationMetadata.OutputSchemaHeader
            )
        )
      )
    withTemporaryDirectory("scalafim-rra5-legacy-"): directory =>
      val path = directory.resolve("legacy.lna.h5")
      val location = ArchiveLocation.unsafe(path.toString)
      LnaHdf5Store.default
        .write(path, legacy)
        .fold(error => fail(error.message), identity)

      val result =
        legacyRuntime
          .openResponse(location)
          .use: source =>
            cats.data.EitherT.fromEither[IO](
              ResolvedResponseSelection
                .all(source.schema)
                .left
                .map(error =>
                  scalafim.archive.ArchiveError.InvalidArchive(error.message)
                )
            ).flatMap(selection =>
              source.read(selection).leftMap(error =>
                scalafim.archive.ArchiveError.InvalidArchive(error.message)
              )
            )
          .value
          .unsafeRunSync()
          .fold(error => fail(error.message), identity)

      assertEquals(result.block.rows, fixture.schema.time.count)
      assertEquals(result.block.columns, fixture.schema.samples.count)

  private def temporalRuntime: ScalafimRuntime[IO] =
    val drivers =
      ArchiveDrivers
        .build(LnaArchiveDriverRegistration.default[IO])
        .fold(error => fail(error.message), identity)
    val responses =
      ArchivedResponseRegistry
        .build(TemporalDctLnaRepresentationFamily[IO])
        .fold(error => fail(error.message), identity)
    ScalafimRuntime.make(drivers, responses)

  private def legacyRuntime: ScalafimRuntime[IO] =
    val drivers =
      ArchiveDrivers
        .build(LnaArchiveDriverRegistration.default[IO])
        .fold(error => fail(error.message), identity)
    val responses =
      ArchivedResponseRegistry
        .build(LegacyLnaHdf5RepresentationFamily.default[IO])
        .fold(error => fail(error.message), identity)
    ScalafimRuntime.make(drivers, responses)

  private def assertBlockAgreesExactly(
      actual: scalafim.response.ResponseBlock,
      expected: scalafim.response.ResponseBlock
  ): Unit =
    assertEquals(actual.selection, expected.selection)
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.columns do
        assert(
          DecodeConsistency.ExactBits.agrees(
            actual(row, column),
            expected(row, column)
          )
        )
        column += 1
      row += 1

  private def assertPayloadRawBits(
      actual: scalafim.archive.lna.Payload,
      expected: scalafim.archive.lna.Payload
  ): Unit =
    (actual, expected) match
      case (
            scalafim.archive.lna.Payload.DoubleMatrix(left, leftType),
            scalafim.archive.lna.Payload.DoubleMatrix(right, rightType)
          ) =>
        assertEquals(leftType, rightType)
        assertEquals(left.rows, right.rows)
        assertEquals(left.cols, right.cols)
        var row = 0
        while row < left.rows do
          var column = 0
          while column < left.cols do
            assertEquals(
              java.lang.Double.doubleToRawLongBits(left(row, column)),
              java.lang.Double.doubleToRawLongBits(right(row, column))
            )
            column += 1
          row += 1
      case (
            scalafim.archive.lna.Payload.DoubleVector(left, leftType),
            scalafim.archive.lna.Payload.DoubleVector(right, rightType)
          ) =>
        assertEquals(leftType, rightType)
        assertEquals(left.length, right.length)
        var index = 0
        while index < left.length do
          assertEquals(
            java.lang.Double.doubleToRawLongBits(left(index)),
            java.lang.Double.doubleToRawLongBits(right(index))
          )
          index += 1
      case _ =>
        fail(s"payload variants differ: $actual versus $expected")

  private def withTemporaryDirectory(
      prefix: String
  )(
      operation: Path => Unit
  ): Unit =
    val directory = Files.createTempDirectory(prefix)
    try operation(directory)
    finally
      val stream = Files.list(directory)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(path => Files.deleteIfExists(path))
      finally stream.close()
      Files.deleteIfExists(directory)
