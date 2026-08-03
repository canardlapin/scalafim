package scalafim.image


class VoxelRegionSuite extends munit.FunSuite:

  private val space =
    VolumeSpace(NeuroSpace(Vector(2, 2, 1)))

  private val translatedSpace =
    VolumeSpace(
      NeuroSpace(
        Vector(2, 2, 1),
        trans = Some(
          DMat.fromRows(
            Vector(
              Vector(1.0, 0.0, 0.0, 10.0),
              Vector(0.0, 1.0, 0.0, 0.0),
              Vector(0.0, 0.0, 1.0, 0.0),
              Vector(0.0, 0.0, 0.0, 1.0)
            )
          )
        )
      )
    )

  private def region(indices: Int*): VoxelRegion =
    VoxelRegion.make(space, Array(indices*)).fold(error => fail(error.message), identity)

  private def indices(region: VoxelRegion): Vector[Int] =
    val values = region.linearIndices
    Vector.tabulate(values.size)(i => values(i))

  test("region construction canonicalizes membership and implements set algebra") {
    val left = region(3, 1, 1)
    val right = region(1, 2)

    assertEquals(indices(left), Vector(1, 3), clue = "")
    assertEquals(left, region(1, 3), clue = "region equality should ignore construction order")
    assertEquals(indices(left.union(right).fold(error => fail(error.message), identity)), Vector(1, 2, 3), clue = "")
    assertEquals(indices(left.intersect(right).fold(error => fail(error.message), identity)), Vector(1), clue = "")
    assertEquals(indices(left.diff(right).fold(error => fail(error.message), identity)), Vector(3), clue = "")
    assertEquals(indices(left.xor(right).fold(error => fail(error.message), identity)), Vector(2, 3), clue = "")
    assertEquals(indices(left.complement), Vector(0, 2), clue = "")
  }

  test("region algebra rejects matching dimensions on a different physical grid") {
    val expected = region(0, 1)
    val translated =
      VoxelRegion.make(translatedSpace, Array[Int](0, 1))
        .fold(error => fail(error.message), identity)

    assert(expected.union(translated).isLeft)
    assert(expected.intersect(translated).isLeft)
  }

  test("ordered selections preserve feature order separately from region membership") {
    val selection =
      VoxelSelection.make(space, Array[Int](2, 0))
        .fold(error => fail(error.message), identity)

    assertEquals(selection.voxelCoords, Vector(VoxelCoord(0, 1, 0), VoxelCoord(0, 0, 0)), clue = "")
    assertEquals(indices(selection.region), Vector(0, 2), clue = "")
    val reversed =
      VoxelSelection.make(space, Array[Int](0, 2))
        .fold(error => fail(error.message), identity)
    assertNotEquals(selection, reversed, clue = "selection equality should preserve extraction order")
    assert(VoxelSelection.make(space, Array[Int](2, 2)).isLeft)
  }

  test("NeuroVol selection preserves ordered geometry and rejects cross-space regions") {
    val volume = NeuroVol.fromLinear(Array[Int](10, 11, 12, 13), space.toNeuroSpace)
    val selection =
      VoxelSelection.make(space, Array[Int](2, 0))
        .fold(error => fail(error.message), identity)
    val selected = volume.select(selection).fold(error => fail(error.message), identity)
    val values = selected.values

    assertEquals(Vector.tabulate(values.size)(i => values(i)), Vector(12, 10), clue = "")
    assertEquals(selected.voxelCoords, selection.voxelCoords, clue = "")

    val translated =
      VoxelRegion.make(translatedSpace, Array[Int](0))
        .fold(error => fail(error.message), identity)
    assert(volume.select(translated).isLeft)
  }

  test("NeuroVec selection returns time by ordered-voxel data") {
    val seriesSpace = space.addTime(2)
    val vector = NeuroVec.fromLinear(Array[Int](0, 1, 2, 3, 10, 11, 12, 13), seriesSpace.toNeuroSpace)
    val selection =
      VoxelSelection.make(space, Array[Int](2, 0))
        .fold(error => fail(error.message), identity)
    val selected = vector.select(selection).fold(error => fail(error.message), identity)

    assertEquals(selected.nTime, 2)
    assertEquals(selected.nVoxels, 2)
    assertEquals(selected(0, 0), 2)
    assertEquals(selected(1, 0), 12)
    assertEquals(selected(0, 1), 0)
    assertEquals(selected(1, 1), 10)
    assertEquals(selected.mapValues(_ + 1)(0, 0), 3)
  }

  test("legacy coordinate adapters validate against an explicit target space") {
    val coords = ROICoords(Vector(Vector(0, 0, 0), Vector(1, 1, 0)))
    val region = coords.asRegionIn(space).fold(error => fail(error.message), identity)

    assertEquals(indices(region), Vector(0, 3), clue = "")
    assert(ROICoords(Vector(Vector(2, 0, 0))).asRegionIn(space).isLeft)
  }

  test("ROIVol extraction no longer drops the ROI physical space") {
    val volume = NeuroVol.fromLinear(Array[Int](10, 11, 12, 13), space.toNeuroSpace)
    val roi = ROIVol[Int](translatedSpace.toNeuroSpace, Vector(Vector(0, 0, 0)), Array[Int](1))

    intercept[IllegalArgumentException] {
      volume(roi)
    }
  }
