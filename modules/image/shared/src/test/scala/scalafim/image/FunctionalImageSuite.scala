package scalafim.image

import cats.implicits.*

class FunctionalImageSuite extends munit.FunSuite:

  private val volumeSpace =
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

  private def vector(values: Array[Int]): Vector[Int] =
    Vector.tabulate(values.length)(i => values(i))

  private def vector(values: ravel.Array1[Int]): Vector[Int] =
    Vector.tabulate(values.size)(i => values(i))

  test("mapValues and coordinate-aware volume mapping preserve geometry") {
    val volume = NeuroVol.fromLinear(Array[Int](10, 20, 30, 40), volumeSpace.toNeuroSpace)
    val mapped = volume.mapValues(_ + 1)
    val located = volume.mapVoxels: (coord, value) =>
      value + coord.x + 10 * coord.y

    assertEquals(mapped.space, volume.space, clue = "")
    assertEquals(vector(mapped.copyLegacyLinear), Vector(11, 21, 31, 41), clue = "")
    assertEquals(vector(located.copyLegacyLinear), Vector(10, 21, 40, 51), clue = "")
  }

  test("NeuroVec mapping distinguishes voxel coordinates from time samples") {
    val space = volumeSpace.addTime(2)
    val series = NeuroVec.fromLinear(Array[Int](0, 0, 0, 0, 0, 0, 0, 0), space.toNeuroSpace)
    val mapped = series.mapSamples: (coord, time, _) =>
      coord.x + 10 * coord.y + 100 * time

    assertEquals(vector(mapped.copyLegacyLinear), Vector(0, 1, 10, 11, 100, 101, 110, 111), clue = "")
  }

  test("checked zipWith rejects a different physical grid") {
    val left = NeuroVol.fromLinear(Array[Int](1, 2, 3, 4), volumeSpace.toNeuroSpace)
    val right = NeuroVol.fromLinear(Array[Int](10, 20, 30, 40), volumeSpace.toNeuroSpace)
    val translated = NeuroVol.fromLinear(Array[Int](10, 20, 30, 40), translatedSpace.toNeuroSpace)

    val summed = left.zipWith(right)(_ + _).fold(error => fail(error.message), identity)
    assertEquals(vector(summed.copyLegacyLinear), Vector(11, 22, 33, 44), clue = "")
    assert(left.zipWith(translated)(_ + _).isLeft)
  }

  test("effectful traversal sequences failures without adding an image monad") {
    val volume = NeuroVol.fromLinear(Array[Int](1, 2, 3, 4), volumeSpace.toNeuroSpace)
    val success = volume.traverseValues[Option, Int](value => Some(value * 2))
    val failure = volume.traverseValues[Option, Int](value => Option.when(value < 3)(value))

    assertEquals(success.map(result => vector(result.copyLegacyLinear)), Some(Vector(2, 4, 6, 8)), clue = "")
    assertEquals(failure, None, clue = "")
  }

  test("RoiSeries mapping retains selection and exposes coordinates") {
    val series = NeuroVec.fromLinear(
      Array[Int](0, 1, 2, 3, 10, 11, 12, 13),
      volumeSpace.addTime(2).toNeuroSpace
    )
    val selection =
      VoxelSelection.make(volumeSpace, Array[Int](2, 0))
        .fold(error => fail(error.message), identity)
    val roi = series.select(selection).fold(error => fail(error.message), identity)
    val mapped = roi.mapSamples: (coord, time, value) =>
      value + coord.y * 100 + time * 1000

    assertEquals(mapped.selection, selection, clue = "")
    assertEquals(mapped(0, 0), 102)
    assertEquals(mapped(1, 0), 1112)
    assertEquals(mapped(0, 1), 0)
    assertEquals(mapped(1, 1), 1010)
  }

  test("region algebra and selection compose through Either pipelines") {
    val series = NeuroVec.fromLinear(
      Array[Int](0, 1, 2, 3, 10, 11, 12, 13),
      volumeSpace.addTime(2).toNeuroSpace
    )
    val left =
      VoxelRegion.make(volumeSpace, Array[Int](0, 2))
        .fold(error => fail(error.message), identity)
    val right =
      VoxelRegion.make(volumeSpace, Array[Int](1, 2))
        .fold(error => fail(error.message), identity)

    val result =
      for
        combined <- left.union(right)
        selected <- series.select(combined)
      yield selected.mapValues(_ + 1)

    val selected = result.fold(error => fail(error.message), identity)
    assertEquals(selected.selection.voxelCoords, Vector(VoxelCoord(0, 0, 0), VoxelCoord(1, 0, 0), VoxelCoord(0, 1, 0)), clue = "")
    assertEquals(selected(0, 0), 1)
    assertEquals(selected(1, 2), 13)
  }

  test("sparse selection requires an explicit missing-voxel policy") {
    val seriesSpace = volumeSpace.addTime(2)
    val dense = Array[Int](0, 1, 2, 3, 10, 11, 12, 13)
    val mask = Mask.fromIndices(volumeSpace.toNeuroSpace, Array[Int](0, 2))
    val sparse = SparseNeuroVec.fromDense[Int](dense, seriesSpace.toNeuroSpace, mask)
    val selection =
      VoxelSelection.make(volumeSpace, Array[Int](2, 1, 0))
        .fold(error => fail(error.message), identity)

    val required = sparse.select(selection, MissingVoxelPolicy.RequireCovered)
    required match
      case Left(SparseSelectionError.OutsideSupport(missing)) =>
        assertEquals(vector(missing.linearIndices), Vector(1), clue = "")
      case other =>
        fail(s"expected typed missing-support error, got $other")

    val dropped =
      sparse
        .select(selection, MissingVoxelPolicy.DropMissing)
        .fold(error => fail(error.message), identity)
    assertEquals(dropped.selection.voxelCoords, Vector(VoxelCoord(0, 1, 0), VoxelCoord(0, 0, 0)), clue = "")
    assertEquals(dropped(0, 0), 2)
    assertEquals(dropped(1, 0), 12)
    assertEquals(dropped(0, 1), 0)
    assertEquals(dropped(1, 1), 10)

    val filled =
      sparse
        .select(selection, MissingVoxelPolicy.Fill(-1))
        .fold(error => fail(error.message), identity)
    assertEquals(filled(0, 0), 2)
    assertEquals(filled(1, 0), 12)
    assertEquals(filled(0, 1), -1)
    assertEquals(filled(1, 1), -1)
    assertEquals(filled(0, 2), 0)
    assertEquals(filled(1, 2), 10)

    val translatedSelection =
      VoxelSelection.make(translatedSpace, Array[Int](0))
        .fold(error => fail(error.message), identity)
    sparse.select(translatedSelection, MissingVoxelPolicy.RequireCovered) match
      case Left(SparseSelectionError.Grid(_)) => ()
      case other => fail(s"expected typed sparse grid mismatch, got $other")
  }
