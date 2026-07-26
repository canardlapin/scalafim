package scalafim.image

import narr.NArray

class OwnershipSuite extends munit.FunSuite:

  test("public NDArray construction and export do not retain mutable aliases") {
    val input = NArray[Int](1, 2, 3, 4)
    val array =
      NDArray.make[Int](input, Vector(2, 2)).fold(error => fail(error.message), identity)

    input(0) = 99
    assertEquals(array(0, 0), 1, clue = "")

    val exported = array.toNArray
    exported(1) = 88
    assertEquals(array(1, 0), 2, clue = "")
  }

  test("checked dense image construction owns its input buffer") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val input = NArray[Int](1, 2, 3, 4)
    val volume =
      NeuroVol
        .fromLinearChecked[Int](input, space)
        .fold(error => fail(error.message), identity)

    input(0) = 99
    assertEquals(volume.linear(0), 1, clue = "")
  }

  test("checked dense reconstruction reports shape failures") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val result = NeuroVol.fromLinearChecked(NArray[Int](1, 2, 3), space)

    assertEquals(
      result,
      Left(NeuroImageError.LinearSizeMismatch("NeuroVol", 4, 3)),
      clue = ""
    )
  }

  test("ROI construction and export do not retain mutable aliases") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val roi =
      VoxelRoi(
        VolumeSpace(space),
        Vector(VoxelCoord(0, 0, 0), VoxelCoord(1, 0, 0))
      )
    val input = NArray[Int](10, 20)
    val values =
      ROIVol.make[Int](space, roi, input).fold(error => fail(error.message), identity)

    input(0) = 99
    assertEquals(values(0), 10, clue = "")

    val exported = values.toNArray
    exported(1) = 88
    assertEquals(values(1), 20, clue = "")
  }

  test("ROI windows own public input and reconstruct only compatible geometry") {
    val space = NeuroSpace(Vector(2, 2, 1))
    val coords = ROICoords(Vector(Vector(0, 0, 0), Vector(1, 0, 0)))
    val input = NArray[Int](10, 20)
    val window =
      ROIVolWindow
        .make[Int](space, coords, input, centerIndex = 1, parentIndex = 1)
        .fold(error => fail(error.message), identity)

    input(1) = 99
    assertEquals(window(1), 20, clue = "")

    val exported = window.toNArray
    exported(1) = 88
    assertEquals(window(1), 20, clue = "")
    assertEquals(window.toROIVol.roi, window.selection.toVoxelRoi, clue = "")
  }
