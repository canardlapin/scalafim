package scalafim.dataset

import scalafim.archive.lna.{LnaPipeline, QuantParams}
import scalafim.image.{DMat, NeuroSpace}

class LatentArchiveDatasetBackendSuite extends munit.FunSuite:

  test("latent archive backend exposes reconstructed selections as FmriSeries") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val data =
      DMat.fromRows(
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

    val backend = LatentArchiveDatasetBackend(DatasetId("latent-demo"), archive)
    val series =
      backend.read(
        DataSelection(
          time = IndexSelection.indices(0, 2),
          voxels = IndexSelection.indices(1, 3)
        )
      )

    assertEquals(series.nTimepoints, 2)
    assertEquals(series.nVoxels, 2)
    val expected = Vector(Vector(1.0, 3.0), Vector(9.0, 11.0))
    series.data.toRows.zip(expected).foreach { case (actualRow, expectedRow) =>
      actualRow.zip(expectedRow).foreach { case (actual, expectedValue) =>
        assert(math.abs(actual - expectedValue) < 2e-4)
      }
    }
  }
