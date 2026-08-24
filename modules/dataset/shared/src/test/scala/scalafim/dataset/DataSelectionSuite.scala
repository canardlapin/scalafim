package scalafim.dataset

import scalafim.image.{DMat, Mask, SampleSpaces, SomeSampleSpace, VoxelCoord}

class DataSelectionSuite extends munit.FunSuite:

  test("mask domains reject equal-shaped spaces with different affines") {
    val translatedAffine =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 10.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(2, 2, 1)), timepoints = 3)
    val maskSpace = SampleSpaces(Vector(2, 2, 1), trans = Some(translatedAffine))
    val mask = Mask.fromIndices(maskSpace, Array[Int](0, 1))

    assert(VoxelDomain.fromMask(mask, shape).isLeft)
  }

  test("voxel coordinate selections resolve through exact dataset geometry and mask membership") {
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(2, 2, 1)), timepoints = 3)
    val full = VoxelDomain.full(shape).fold(error => fail(error.message), identity)
    val ordered =
      DataSelection(
        voxels = VoxelSelection.coords(
          VoxelCoord(1, 1, 0),
          VoxelCoord(0, 0, 0)
        )
      )
        .resolveEither(shape, full)
        .fold(error => fail(error.message), identity)
    assertEquals(ordered.voxels, Vector(3, 0))

    val mask = Mask.fromIndices(shape.space, Array[Int](0, 3))
    val active =
      VoxelDomain.fromMask(mask, shape).fold(error => fail(error.message), identity)
    assert(DataSelection(voxels = VoxelSelection.coords(VoxelCoord(1, 0, 0)))
      .resolveEither(shape, active)
      .left
      .toOption
      .contains(DatasetError.VoxelOutsideMask(2)))

    assert(DataSelection(voxels = VoxelSelection.coords(VoxelCoord(2, 0, 0)))
      .resolveEither(shape, full)
      .left
      .exists:
        case DatasetError.InvalidVoxelCoordinate(VoxelCoord(2, 0, 0), _) => true
        case _                                                           => false
    )
  }

  test("timepoint windows are checked, ordered, and may overlap") {
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(1, 1, 1)), timepoints = 6)
    val first =
      DataSelection(time = TimepointSelection.Window(TimepointIndex.unsafe(1), length = 3))
        .resolveEither(shape)
        .fold(error => fail(error.message), identity)
    val second =
      DataSelection(time = TimepointSelection.Window(TimepointIndex.unsafe(2), length = 3))
        .resolveEither(shape)
        .fold(error => fail(error.message), identity)

    assertEquals(first.timepoints, Vector(1, 2, 3))
    assertEquals(second.timepoints, Vector(2, 3, 4))
    assert(TimepointSelection.window(start = 1, length = 0).isLeft)
    assert(DataSelection(time = TimepointSelection.Window(TimepointIndex.unsafe(5), length = 2))
      .resolveEither(shape)
      .left
      .exists(_.message.contains("exceeds size 6")))
  }

  test("excluded timepoints are an explicit ordered censor selection") {
    val shape = DatasetShape.unsafe(SampleSpaces(Vector(1, 1, 1)), timepoints = 8)
    val resolved =
      DataSelection(time = TimepointSelection.excluding(2, 4, 5))
        .resolveEither(shape)
        .fold(error => fail(error.message), identity)

    assertEquals(resolved.timepoints, Vector(0, 1, 3, 6, 7))
    assert(TimepointSelection.fromExcludedInts(-1).isLeft)
    assert(DataSelection(time = TimepointSelection.excluding(7, 8))
      .resolveEither(shape)
      .left
      .exists(_.message.contains("out of bounds")))
    assert(DataSelection(time = TimepointSelection.excluding(0, 1, 2, 3, 4, 5, 6, 7))
      .resolveEither(shape)
      .left
      .toOption
      .contains(DatasetError.EmptySelection(DatasetAxis.Timepoint)))
  }
