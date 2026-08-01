package scalafim.image

import image4s.geometry.CoordinateConvention

class TypedImageCoreSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assertEqualsDouble(actual, expected, tol)

  private def assertClose(actual: WorldPoint, expected: WorldPoint, tol: Double): Unit =
    assertClose(actual.x, expected.x, tol)
    assertClose(actual.y, expected.y, tol)
    assertClose(actual.z, expected.z, tol)

  private def assertClose(actual: VoxelPoint, expected: VoxelPoint, tol: Double): Unit =
    assertClose(actual.x, expected.x, tol)
    assertClose(actual.y, expected.y, tol)
    assertClose(actual.z, expected.z, tol)

  private def affineMatrix: DMat =
    DMat.fromRows(
      Vector(
        Vector(2.0, 0.0, 0.0, 10.0),
        Vector(0.0, 3.0, 0.0, 20.0),
        Vector(0.0, 0.0, 4.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private def toVector(indices: Array[Int]): Vector[Int] =
    Vector.tabulate(indices.length)(i => indices(i))

  private def toVector(indices: ravel.Array1[Int]): Vector[Int] =
    Vector.tabulate(indices.size)(i => indices(i))

  test("Affine3D is finite homogeneous and invertible") {
    val affine = Affine3D(affineMatrix)
    val voxel = VoxelPoint(1.5, 2.0, 3.0)
    val world = affine.voxelToWorld(voxel)

    assertClose(world, WorldPoint(13.0, 26.0, 42.0), 1e-10)
    assertClose(affine.worldToVoxel(world), voxel, 1e-10)

    val singular =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 0.0),
          Vector(0.0, 0.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    assert(Affine3D.make(singular).isLeft, clue = "singular affine should be rejected")

    val projective =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.1, 1.0)
        )
      )
    assert(Affine3D.make(projective).isLeft, clue = "non-affine homogeneous row should be rejected")
  }

  test("VolumeSpace and SeriesSpace distinguish exact 3D and 4D spaces") {
    val volume = VolumeSpace(NeuroSpace(Vector(2, 3, 4), trans = Some(affineMatrix)))
    val series = volume.addTime(5)

    assertEquals(volume.shape, SpatialDims(2, 3, 4), clue = "")
    assertEquals(series.nVolumes, 5, clue = "")
    assertEquals(series.volumeSpace, volume, clue = "")
    assert(VolumeSpace.make(series.toNeuroSpace).isLeft, clue = "4D space should not be a VolumeSpace")
    assert(SeriesSpace.make(volume.toNeuroSpace).isLeft, clue = "3D space should not be a SeriesSpace")
    assert(
      volume.asInstanceOf[AnyRef] eq volume.toNeuroSpace.asInstanceOf[AnyRef],
      clue = "VolumeSpace must be a zero-allocation refinement"
    )
    assert(
      series.asInstanceOf[AnyRef] eq series.toNeuroSpace.asInstanceOf[AnyRef],
      clue = "SeriesSpace must be a zero-allocation refinement"
    )
  }

  test("non-spatial refinements retain the exact image4s grid and frame") {
    val volume = NeuroSpace(Vector(2, 3, 4), trans = Some(affineMatrix))
    val volumeCanonical = NeuroSpace.canonical(volume)
    val series = volume.addDim(5, Some(Axis.Time))
    val seriesCanonical = NeuroSpace.canonical(series)
    val roundTrip = series.dropDim(3)
    val roundTripCanonical = NeuroSpace.canonical(roundTrip)

    assert(volumeCanonical.grid eq seriesCanonical.grid)
    assert(volumeCanonical.grid.frame eq seriesCanonical.grid.frame)
    assert(volumeCanonical.grid eq roundTripCanonical.grid)
    assert(volumeCanonical.grid.frame eq roundTripCanonical.grid.frame)
    assertEquals(seriesCanonical.nonSpatialAxes(0).map(_.kind), Some(image4s.AxisKind.Time))
    assertEquals(
      volumeCanonical.grid.frame.metadata.convention,
      CoordinateConvention.RAS
    )
  }

  test("image4s Sampled backs slice, volume, and series compatibility views") {
    val volumeSpace = NeuroSpace(Vector(2, 1, 1), trans = Some(affineMatrix))
    val volume = NeuroVol.fromLinear[Int](Array(10, 20), volumeSpace, "vol")
    val mapped = volume.map(_ + 1)
    val series = volume.toVec
    val slice = volume.slice(SpatialAxis.Z, 0)

    assertEquals(volume.typedSpace.toNeuroSpace, volumeSpace, clue = "")
    assertEquals(volume.label, "vol", clue = "")
    assertEquals(volume.sampled.metadata.label, "vol", clue = "")
    assertEquals(volume.ndim, 3, clue = "")
    assertEquals(mapped.linear(1), 21, clue = "")
    assertEquals(series.typedSpace.toNeuroSpace.ndim, 4, clue = "")
    assertEquals(series.volume(0).space, volume.space, clue = "")
    assertEquals(series.sampled.metadata.label, "vol", clue = "")
    assertEquals(series.volume(0).sampled.metadata.label, "vol", clue = "")
    assertEquals(series.volume(0).linear(1), volume.linear(1), clue = "")
    assertEquals(slice.typedSpace.toNeuroSpace.ndim, 2, clue = "")
    assertEquals(slice(1, 0), 20, clue = "")

    intercept[IllegalArgumentException] {
      NeuroSlice.fromLinear[Int](Array(1, 2), volumeSpace)
    }
  }

  test("typed coordinate overloads keep voxel and world points separate") {
    val affine = Affine3D(affineMatrix)
    val voxel = VoxelPoint(1.0, 2.0, 3.0)
    val world = SpatialCoordinates.voxelToWorld(voxel, affine)
    val back = SpatialCoordinates.worldToVoxel(world, affine)

    assertClose(world, WorldPoint(12.0, 26.0, 42.0), 1e-10)
    assertClose(back, voxel, 1e-10)
  }

  test("validated index sets canonicalize masks and reject duplicate sparse positions") {
    val space = NeuroSpace(Vector(3, 1, 1))
    val indexSet = VoxelIndexSet(space, Array(2, 0, 2))
    val mask = Mask.fromIndexSet(indexSet)

    assertEquals(indexSet.toVector, Vector(0, 2), clue = "")
    assertEquals(toVector(Mask.indices(mask)), Vector(0, 2), clue = "")
    assert(VoxelIndexSet.makeUnique(space, Array(1, 1)).isLeft, clue = "sparse index sets should reject duplicates")

    intercept[IllegalArgumentException] {
      SparseNeuroVol(Array(10.0, 20.0), Array(1, 1), space)
    }
  }

  test("VoxelRoi validates coordinates before ROI extraction") {
    val space = NeuroSpace(Vector(3, 1, 1), trans = Some(affineMatrix))
    val roi = VoxelRoi.fromRawUnsafe(space, Vector(Vector(0, 0, 0), Vector(2, 0, 0)))
    val vol = NeuroVol.fromLinear[Int](Array(10, 20, 30), space)

    val values = vol(roi)
    assertEquals(Vector.tabulate(values.size)(i => values(i)), Vector(10, 30), clue = "")
    assertEquals(toVector(roi.linearIndices), Vector(0, 2), clue = "")
    assert(VoxelRoi.fromRaw(space, Vector(Vector(3, 0, 0))).isLeft, clue = "ROI bounds should be checked")
    assert(VoxelRoi.fromRaw(space, Vector(Vector(1, 0, 0), Vector(1, 0, 0))).isLeft, clue = "ROI duplicates should be rejected")
  }
