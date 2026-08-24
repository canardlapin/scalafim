package scalafim.archive.io

import scalafim.image.SampleSpaces

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.archive.{
  ArchiveLocation,
  ArchiveValidationScope,
  PhysicalLocality,
  PublicationStatus,
  ReadObservation
}
import scalafim.archive.lna.{
  LnaArchiveManifestAdapter,
  LnaDType,
  LnaIntMatrixPlan,
  LnaPayloadRole,
  LnaPipeline,
  Payload,
  QuantParams
}
import gale.linalg.DMat
import scalafim.archive.lna.GaleArchiveTestData
import scalafim.image.SomeSampleSpace

import java.nio.file.Files
import scala.concurrent.Future

class LnaArchiveDriverSuite extends munit.FunSuite:
  test("eager HDF5 driver exposes pure revision and typed whole-payload execution"):
    val data =
      GaleArchiveTestData.matrixFromRows(Vector(
        Vector(0.0, 1.0, 2.0, 3.0),
        Vector(4.0, 5.0, 6.0, 7.0),
        Vector(8.0, 9.0, 10.0, 11.0)
      ))
    val archive =
      LnaPipeline
        .quantArchive(
          data,
          SampleSpaces(Vector(2, 2, 1)),
          params = QuantParams(bits = 8)
        )
        .fold(error => fail(error.message), identity)
    val integerRef =
      archive.manifest.datasets
        .find(_.dtype.exists(dtype =>
          dtype == LnaDType.UInt8 ||
            dtype == LnaDType.UInt16 ||
            dtype == LnaDType.Int32
        ))
        .getOrElse(fail("expected an integer payload"))
    val file = Files.createTempFile("scalafim-lna-driver-", ".h5")
    LnaHdf5Store.default.write(file, archive).fold(error => fail(error.message), identity)

    val program =
      LnaArchiveDriver
        .default[IO]
        .open(ArchiveLocation.unsafe(file.toString))
        .use: opened =>
          for
            contents <- opened.validateContents
            observed <- opened.payloads.execute(LnaIntMatrixPlan.full(integerRef.path))
          yield (opened.revision, opened.structure, contents, observed)
        .value
        .guarantee(IO.blocking(Files.deleteIfExists(file)).void)
        .map:
          case Left(error) => fail(error.message)
          case Right((revision, structure, contents, observed)) =>
            assertEquals(structure.scope, ArchiveValidationScope.Structure)
            assertEquals(contents.scope, ArchiveValidationScope.Contents)
            assertEquals(
              revision.manifest.format,
              LnaArchiveManifestAdapter.Format
            )
            assertEquals(
              revision.manifest.key.objectType,
              LnaArchiveManifestAdapter.ObjectType
            )
            assertEquals(
              revision.manifest.representation.map(_.key),
              Some(LnaArchiveManifestAdapter.PipelineRepresentation)
            )
            assert(
              revision.manifest.payloads.forall(_.role.isNamespaced)
            )
            assertEquals(
              revision.manifest.payloads
                .find(_.id.value == integerRef.path.value)
                .map(_.role),
              Some(LnaPayloadRole.fromDatasetRole(integerRef.role))
            )
            revision.publication match
              case PublicationStatus.Published(digest) =>
                assertEquals(digest.algorithm, "sha256")
              case other => fail(s"expected published revision, found $other")
            assertEquals(observed.value.dims, integerRef.dims)
            assertEquals(
              observed.receipt.observedLocality,
              PhysicalLocality.WholePayload
            )
            assertEquals(observed.receipt.observations.length, 1)
            observed.receipt.observations.head match
              case ReadObservation.WholePayload(payload, bytes) =>
                assertEquals(payload.value, integerRef.path.value)
                assertEquals(
                  bytes,
                  integerRef.dims.map(_.toLong).product * integerRef.dtype.get.bytes.toLong
                )
              case other => fail(s"expected whole-payload evidence, found $other")

    run(program)

  private def run[A](program: IO[A]): Future[A] =
    program.unsafeToFuture()
