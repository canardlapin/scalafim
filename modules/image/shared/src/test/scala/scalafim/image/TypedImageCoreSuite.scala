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
    val volume = VolumeSpace(SampleSpaces(Vector(2, 3, 4), trans = Some(affineMatrix)))
    val series = volume.addTime(5)

    assertEquals(volume.shape, SpatialDims(2, 3, 4), clue = "")
    assertEquals(series.nVolumes, 5, clue = "")
    assertEquals(
      GridCompatibility.volume(series.volumeSpace, volume),
      Right(()),
      clue = ""
    )
    assert(VolumeSpace.make(series.toSampleSpace).isLeft, clue = "4D space should not be a VolumeSpace")
    assert(SeriesSpace.make(volume.toSampleSpace).isLeft, clue = "3D space should not be a SeriesSpace")
    assert(
      volume.asInstanceOf[AnyRef] eq volume.toSampleSpace.asInstanceOf[AnyRef],
      clue = "VolumeSpace must be a zero-allocation refinement"
    )
    assert(
      series.asInstanceOf[AnyRef] eq series.toSampleSpace.asInstanceOf[AnyRef],
      clue = "SeriesSpace must be a zero-allocation refinement"
    )
  }

  test("non-spatial refinements retain the exact image4s grid and frame") {
    val volume = SampleSpaces(Vector(2, 3, 4), trans = Some(affineMatrix))
    val volumeCanonical = SampleSpaces.canonical(volume)
    val series = volume.addDim(5, Some(Axis.Time))
    val seriesCanonical = SampleSpaces.canonical(series)
    val roundTrip = series.dropDim(3)
    val roundTripCanonical = SampleSpaces.canonical(roundTrip)

    assert(volumeCanonical.grid eq seriesCanonical.grid)
    assert(volumeCanonical.grid.frame eq seriesCanonical.grid.frame)
    assert(volumeCanonical.grid eq roundTripCanonical.grid)
    assert(volumeCanonical.grid.frame eq roundTripCanonical.grid.frame)
    assertEquals(seriesCanonical.nonSpatialAxes(0).map(_.kind), Some(image4s.AxisKind.Time))
    assertEquals(
      volumeCanonical.grid.frame.convention,
      CoordinateConvention.RAS
    )
  }

  test("image4s Sampled backs singleton-D3 plane, volume, and series views") {
    val volumeSpace = SampleSpaces(Vector(2, 1, 1), trans = Some(affineMatrix))
    val volume = SomeLabelVolume.unsafeCopyFromCanonicalArray[Int](Array(10, 20), volumeSpace, "vol")
    val mapped = volume.mapValues[Int, image4s.Categorical](_ + 1)
    val series = volume.toSeries
    val plane =
      volume
        .plane(SpatialAxis.Z, 0)
        .fold(error => fail(error.message), identity)

    assertEquals(volume.typedSpace.toSampleSpace, volumeSpace, clue = "")
    assertEquals(volume.label, "vol", clue = "")
    assertEquals(volume.sampled.metadata.label, "vol", clue = "")
    assertEquals(volume.ndim, 3, clue = "")
    assertEquals(mapped.valueAtCanonicalOrdinal(1), 21, clue = "")
    assertEquals(series.typedSpace.toSampleSpace.ndim, 4, clue = "")
    assertEquals(
      GridCompatibility.exact(series.volume(0).space, volume.space),
      Right(()),
      clue = ""
    )
    assertEquals(series.sampled.metadata.label, "vol", clue = "")
    assertEquals(series.volume(0).sampled.metadata.label, "vol", clue = "")
    assertEquals(
      series.volume(0).valueAtCanonicalOrdinal(1),
      volume.valueAtCanonicalOrdinal(1),
      clue = ""
    )
    assertEquals(plane.grid.shape, Vector(2, 1, 1), clue = "")
    assertEquals(plane(1, 0, 0), 20, clue = "")
  }

  test("typed coordinate overloads keep voxel and world points separate") {
    val affine = Affine3D(affineMatrix)
    val voxel = VoxelPoint(1.0, 2.0, 3.0)
    val world = SpatialCoordinates.voxelToWorld(voxel, affine)
    val back = SpatialCoordinates.worldToVoxel(world, affine)

    assertClose(world, WorldPoint(12.0, 26.0, 42.0), 1e-10)
    assertClose(back, voxel, 1e-10)
  }

  test("regions canonicalize support while ordered selections reject duplicates") {
    val space = SampleSpaces(Vector(3, 1, 1))
    val packed =
      GridDomain
        .register(
          VolumeSpace(space).sampleSpace.grid,
          "typed region selection",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val region =
      locus4s.Region
        .fromOrdinals(domain.space, Vector(2, 0, 2))
        .toOption
        .get
    val mask = Mask.fromRegion(domain, region).toOption.get

    assertEquals(region.ordinalsInDomainOrder.toVector, Vector(0, 2), clue = "")
    assertEquals(
      Mask.region(domain, mask).toOption.get.ordinalsInDomainOrder.toVector,
      Vector(0, 2),
      clue = ""
    )
    assert(
      locus4s.Selection.fromOrdinals(domain.space, Vector(1, 1)).isLeft,
      clue = "ordered selections should reject duplicates"
    )
  }

  test("exact grid indices validate coordinates before selected extraction") {
    val space = SampleSpaces(Vector(3, 1, 1), trans = Some(affineMatrix))
    val packed =
      GridDomain
        .register(
          VolumeSpace(space).sampleSpace.grid,
          "typed coordinate selection",
          locus4s.DomainRegistry.empty
        )
        .toOption
        .get
    type Voxel = packed.S
    val domain = packed.value
    val selection =
      locus4s.Selection.fromOrdinals(domain.space, Vector(0, 2)).toOption.get
    val sampleSpace =
      image4s.SampleSpace.create(domain.grid, image4s.NonSpatialAxes.empty)
    val volume =
      NeuroVolume
        .categorical(
          sampleSpace,
          ravel.NDArray.fromSeq(ravel.Shape(3, 1, 1), Vector(10, 20, 30))
        )
        .toOption
        .get
    val values =
      SelectedVolume.gather(domain, volume, selection).toOption.get

    assertEquals(values.data.iterator.toVector, Vector(10, 30), clue = "")
    assertEquals(values.selection.ordinals.toVector, Vector(0, 2), clue = "")
    val outOfBounds =
      image4s.geometry.LatticeIndex
        .fromVector[image4s.geometry.D3](Vector(3, 0, 0))
        .toOption
        .get
    assert(domain.domainIndexAt(outOfBounds).isLeft, clue = "voxel bounds should be checked")
    assert(
      locus4s.Selection.fromOrdinals(domain.space, Vector(1, 1)).isLeft,
      clue = "selection duplicates should be rejected"
    )
  }
