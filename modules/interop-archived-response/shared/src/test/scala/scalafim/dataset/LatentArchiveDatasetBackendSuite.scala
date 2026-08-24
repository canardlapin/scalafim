package scalafim.dataset

import scalafim.image.SampleSpaces

import gale.linalg.{DMat, DVec}
import scalafim.archive.lna.GaleArchiveTestData
import scalafim.archive.RunLabel
import scalafim.archive.lna.{LnaPipeline, QuantParams}
import scalafim.image.SomeSampleSpace
import scalafim.latent.{
  BoldZipCoarseBasis,
  BoldZipDetailBasis,
  BoldZipPayload,
  BoldZipResidualEvent,
  BoldZipSpatialBasis,
  BoldZipTextureEntry,
  BoldZipLatentArchiveCodec,
  LatentArchiveRegistry,
  LatentSelection,
  TransportLatentArchiveCodec,
  TransportLatentResponse
}

class LatentArchiveDatasetBackendSuite extends munit.FunSuite:

  test("latent archive backend exposes reconstructed selections as FmriSeries") {
    val space = SampleSpaces(Vector(2, 2, 1))
    val data =
      GaleArchiveTestData.matrixFromRows(
        Vector(
          Vector(0.0, 1.0, 2.0, 3.0),
          Vector(4.0, 5.0, 6.0, 7.0),
          Vector(8.0, 9.0, 10.0, 11.0)
        )
      )

    val archive =
      LnaPipeline
        .quantArchive(data, space, params = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)

    val backend =
      LatentArchiveDatasetBackend
        .make(
          DatasetId("latent-demo"),
          archive,
          RunLabel.indexed(0),
          LatentArchiveRegistry.standard
        )
        .fold(err => fail(err.message), identity)
    val series =
      backend.readEither(
        DataSelection(
          time = TimepointSelection.indices(0, 2),
          voxels = VoxelSelection.indices(1, 3)
        )
      ).fold(err => fail(err.message), identity)

    assertEquals(series.nTimepoints, 2)
    assertEquals(series.nVoxels, 2)
    val expected = Vector(Vector(1.0, 3.0), Vector(9.0, 11.0))
    GaleTestData.toRows(series.data).zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actual, expectedValue) =>
        assert(math.abs(actual - expectedValue) < 2e-4)
      }
    }
  }

  test("latent archive backend reads transport archives through a typed latent plan") {
    val space = SampleSpaces(Vector(2, 2, 1))
    val decoder =
      GaleTestData.csrFromTriplets(
        rows = 4,
        cols = 2,
        rowIndices = Array(0, 1, 2, 2, 3, 3),
        colIndices = Array(0, 1, 0, 1, 0, 1),
        values = Array(1.0, 1.0, 1.0, 1.0, 2.0, -1.0)
      )
    val response =
      TransportLatentResponse
        .withIdentityTransform(
          coefficientsAnalysis = GaleTestData.matrixFromRows(
            Vector(
              Vector(1.0, 2.0),
              Vector(3.0, 4.0)
            )
          ),
          nativeDecoder = decoder,
          offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0, 40.0))),
          label = "transport-dataset"
        )
        .fold(err => fail(err.message), identity)
    val archive =
      TransportLatentArchiveCodec
        .toArchive(response, space)
        .fold(err => fail(err.message), identity)
    val backend =
      LatentArchiveDatasetBackend
        .make(
          DatasetId("transport-latent"),
          archive,
          RunLabel.indexed(0),
          LatentArchiveRegistry.standard
        )
        .fold(err => fail(err.message), identity)
    val series =
      backend.readEither(
        DataSelection(
          time = TimepointSelection.indices(1, 0),
          voxels = VoxelSelection.indices(3, 1)
        )
      ).fold(err => fail(err.message), identity)
    val expected =
      response
        .reconstruct(LatentSelection(timepoints = Some(Vector(1, 0)), samples = Some(Vector(3, 1))))
        .fold(err => fail(err.message), identity)

    assertRowsClose(GaleTestData.toRows(series.data), GaleTestData.toRows(expected), 1e-12)
  }

  test("latent archive backend reads BOLDZip archives through a typed latent plan") {
    val space = SampleSpaces(Vector(3, 1, 1))
    val spatialBasis =
      BoldZipSpatialBasis(
        sampleCount = 3,
        coarse = BoldZipCoarseBasis.MatrixBasis(GaleTestData.matrixFromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0)))),
        detail = BoldZipDetailBasis.IdentitySamples,
        label = "identity-detail"
      ).fold(err => fail(err.message), identity)
    val response =
      BoldZipPayload(
        temporalBasis = DMat.eye(4),
        carrierTheta = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 2.0, 3.0, 4.0),
            Vector(10.0, 20.0, 30.0, 40.0)
          )
        ),
        carrierLoadings = GaleTestData.matrixFromRows(Vector(Vector(2.0, 1.0))),
        spatialBasis = spatialBasis,
        texture = Vector(
          BoldZipTextureEntry.unsafe(atom = 0, carrier = 0, amplitude = 0.5),
          BoldZipTextureEntry.unsafe(atom = 1, carrier = 1, amplitude = 1.0, lag = 1)
        ),
        events = Vector(BoldZipResidualEvent.unsafe(atom = 2, frame = 2, amplitude = 3.0)),
        offset = Some(DVec.fromSeq(Vector(10.0, 20.0, 30.0))),
        label = "boldzip-dataset"
      ).fold(err => fail(err.message), identity)
    val archive =
      BoldZipLatentArchiveCodec
        .toArchive(response, space)
        .fold(err => fail(err.message), identity)
    val backend =
      LatentArchiveDatasetBackend
        .make(
          DatasetId("boldzip-latent"),
          archive,
          RunLabel.indexed(0),
          LatentArchiveRegistry.standard
        )
        .fold(err => fail(err.message), identity)
    val series =
      backend.readEither(
        DataSelection(
          time = TimepointSelection.indices(3, 1),
          voxels = VoxelSelection.indices(2, 0)
        )
      ).fold(err => fail(err.message), identity)
    val expected =
      response
        .reconstruct(LatentSelection(timepoints = Some(Vector(3, 1)), samples = Some(Vector(2, 0))))
        .fold(err => fail(err.message), identity)

    assertRowsClose(GaleTestData.toRows(series.data), GaleTestData.toRows(expected), 1e-12)
  }

  test("latent archive backend rejects a missing internal run during construction") {
    val space = SampleSpaces(Vector(1, 1, 1))
    val archive =
      LnaPipeline
        .quantArchive(GaleArchiveTestData.matrixFromRows(Vector(Vector(1.0))), space, params = QuantParams(bits = 16))
        .fold(err => fail(err.message), identity)

    val missing =
      LatentArchiveDatasetBackend.make(
        DatasetId("missing-run"),
        archive,
        RunLabel("not-present"),
        LatentArchiveRegistry.standard
      )

    assert(missing.left.exists(_.message.contains("run 'not-present' not found")))
  }

  private def assertRowsClose(
      actual: Vector[Vector[Double]],
      expected: Vector[Vector[Double]],
      tol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    assertEquals(if actual.isEmpty then 0 else actual.head.length, if expected.isEmpty then 0 else expected.head.length)
    actual.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actualValue, expectedValue) =>
        assertEqualsDouble(actualValue, expectedValue, tol)
      }
    }
