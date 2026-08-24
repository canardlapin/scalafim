package scalafim.dataset

import scalafim.image.SampleSpaces

import gale.linalg.DMat as GaleDMat
import scalafim.image.{DMat, Mask, SomeSampleSpace}
import scalafim.latent.ExplicitLatentResponse

class LatentResponseDatasetBackendSuite extends munit.FunSuite:

  private val space = SampleSpaces(Vector(2, 2, 1))

  private val denseData =
    DMat.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0, 4.0),
        Vector(5.0, 6.0, 7.0, 8.0),
        Vector(9.0, 10.0, 11.0, 12.0)
      )
    )

  test("latent response backend matches dense backend for selected masked voxels") {
    val mask = Mask.fromIndices(space, Array(0, 2, 3))
    val response =
      ExplicitLatentResponse(
        basis = GaleDMat.eye(3),
        loadings = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val dense = InMemoryDatasetBackend(DatasetId("dense"), denseData, space)
    val latent = LatentResponseDatasetBackend.unsafe(DatasetId("latent"), response, space, mask)
    val selection =
      DataSelection(
        time = TimepointSelection.indices(2, 0),
        voxels = VoxelSelection.indices(3, 0)
      )

    val expected = dense.read(selection)
    val actual = latent.readEither(selection).fold(err => fail(err.message), identity)

    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.data.toRows, expected.data.toRows)
  }

  test("masked latent response default read uses the active voxel domain") {
    val mask = Mask.fromIndices(space, Array(0, 2, 3))
    val response =
      ExplicitLatentResponse(
        basis = GaleDMat.eye(3),
        loadings = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val latent = LatentResponseDatasetBackend.unsafe(DatasetId("latent"), response, space, mask)
    val series = latent.read()

    assertEquals(latent.voxelDomain.kind, VoxelDomainKind.ActiveMask)
    assertEquals(latent.voxelDomain.indices, Vector(0, 2, 3))
    assertEquals(series.voxelIndices, Vector(0, 2, 3))
    assertEquals(
      series.data.toRows,
      Vector(
        Vector(1.0, 3.0, 4.0),
        Vector(5.0, 7.0, 8.0),
        Vector(9.0, 11.0, 12.0)
      )
    )
  }

  test("masked latent response requires explicit full-spatial reads") {
    val mask = Mask.fromIndices(space, Array(0, 2, 3))
    val response =
      ExplicitLatentResponse(
        basis = GaleDMat.eye(3),
        loadings = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val latent = LatentResponseDatasetBackend.unsafe(DatasetId("latent"), response, space, mask)

    val failed =
      latent
        .readEither(DataSelection(voxels = VoxelSelection.AllSpatial))
        .left
        .toOption
        .getOrElse(fail("expected full-spatial request to hit the inactive voxel"))

    assertEquals(failed, DatasetError.VoxelOutsideMask(1))
  }

  test("latent response backend rejects selected voxels outside mask") {
    val mask = Mask.fromIndices(space, Array(0, 2, 3))
    val response =
      ExplicitLatentResponse(
        basis = GaleDMat.eye(3),
        loadings = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val latent = LatentResponseDatasetBackend.unsafe(DatasetId("latent"), response, space, mask)

    val failed =
      latent
        .readEither(DataSelection(voxels = VoxelSelection.indices(1)))
        .left
        .toOption
        .getOrElse(fail("expected voxel outside mask error"))
    assertEquals(failed, DatasetError.VoxelOutsideMask(1))

    interceptMessage[IllegalArgumentException](
      "voxel 1 is outside the readable sample mask"
    ) {
      latent.read(DataSelection(voxels = IndexSelection.indices(1)))
    }
  }

  test("voxel sample map preserves mask-to-latent sample order") {
    val mask = Mask.fromIndices(space, Array(0, 2, 3))
    val sampleMap = VoxelSampleMap.fromMask(mask, expectedSamples = 3).fold(err => fail(err.message), identity)
    val samples =
      sampleMap
        .samplesFor(Vector(VoxelIndex.unsafe(3), VoxelIndex.unsafe(0)))
        .fold(err => fail(err.message), identity)

    assertEquals(samples, Vector(2, 0))
    assertEquals(sampleMap.sampleVoxels.map(VoxelIndex.raw), Vector(0, 2, 3))
  }

  test("latent response backend enforces mask cardinality") {
    val mask = Mask.fromIndices(space, Array(0, 2))
    val response =
      ExplicitLatentResponse(
        basis = GaleDMat.eye(3),
        loadings = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)

    val error =
      LatentResponseDatasetBackend
        .make(DatasetId("latent"), response, space, mask)
        .left
        .toOption
        .getOrElse(fail("expected mask cardinality rejection"))
    assert(
      error.message.contains(
        "response sample count must match mask cardinality"
      )
    )
  }

  test("latent response backend defaults to an all-space mask") {
    val response =
      ExplicitLatentResponse(
        basis = GaleDMat.eye(3),
        loadings = GaleTestData.matrixFromRows(
          Vector(
            Vector(1.0, 5.0, 9.0),
            Vector(2.0, 6.0, 10.0),
            Vector(3.0, 7.0, 11.0),
            Vector(4.0, 8.0, 12.0)
          )
        )
      ).fold(err => fail(err.message), identity)
    val dense = InMemoryDatasetBackend(DatasetId("dense"), denseData, space)
    val latent = LatentResponseDatasetBackend.unsafe(DatasetId("latent"), response, space)
    val selection =
      DataSelection(
        time = TimepointSelection.indices(1),
        voxels = VoxelSelection.indices(2, 1)
      )

    assertEquals(latent.read(selection).data.toRows, dense.read(selection).data.toRows)
  }
