package scalafim.dataset

import narr.NArray
import scalafim.image.{DMat, Mask, NeuroSpace}
import scalafim.latent.ExplicitLatentResponse
import scalafim.linalg.DoubleMatrix

class LatentResponseDatasetBackendSuite extends munit.FunSuite:

  private val space = NeuroSpace(Vector(2, 2, 1))

  private val denseData =
    DMat.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0, 4.0),
        Vector(5.0, 6.0, 7.0, 8.0),
        Vector(9.0, 10.0, 11.0, 12.0)
      )
    )

  test("latent response backend matches dense backend for selected masked voxels") {
    val mask = Mask.fromIndices(space, NArray(0, 2, 3))
    val response =
      ExplicitLatentResponse(
        basis = DoubleMatrix.eye(3),
        loadings = DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val dense = InMemoryDatasetBackend(DatasetId("dense"), denseData, space)
    val latent = LatentResponseDatasetBackend(DatasetId("latent"), response, space, mask)
    val selection =
      DataSelection(
        time = IndexSelection.indices(2, 0),
        voxels = IndexSelection.indices(3, 0)
      )

    val expected = dense.read(selection)
    val actual = latent.read(selection)

    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.data.toRows, expected.data.toRows)
  }

  test("latent response backend rejects selected voxels outside mask") {
    val mask = Mask.fromIndices(space, NArray(0, 2, 3))
    val response =
      ExplicitLatentResponse(
        basis = DoubleMatrix.eye(3),
        loadings = DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val latent = LatentResponseDatasetBackend(DatasetId("latent"), response, space, mask)

    interceptMessage[IllegalArgumentException]("voxel 1 is outside the latent mask") {
      latent.read(DataSelection(voxels = IndexSelection.indices(1)))
    }
  }

  test("latent response backend enforces mask cardinality") {
    val mask = Mask.fromIndices(space, NArray(0, 2))
    val response =
      ExplicitLatentResponse(
        basis = DoubleMatrix.eye(3),
        loadings = DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)

    val error = intercept[IllegalArgumentException] {
      LatentResponseDatasetBackend(DatasetId("latent"), response, space, mask)
    }
    assert(error.getMessage.contains("latent sample count must match mask cardinality"))
  }

  test("latent response backend defaults to an all-space mask") {
    val response =
      ExplicitLatentResponse(
        basis = DoubleMatrix.eye(3),
        loadings = DoubleMatrix.fromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(2.0, 6.0, 10.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val dense = InMemoryDatasetBackend(DatasetId("dense"), denseData, space)
    val latent = LatentResponseDatasetBackend(DatasetId("latent"), response, space)
    val selection =
      DataSelection(
        time = IndexSelection.indices(1),
        voxels = IndexSelection.indices(2, 1)
      )

    assertEquals(latent.read(selection).data.toRows, dense.read(selection).data.toRows)
  }
