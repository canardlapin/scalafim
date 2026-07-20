package scalafim.image

class SliceGeometrySuite extends munit.FunSuite:

  private val Tol = 1e-10

  private def assertClose(actual: Double, expected: Double): Unit =
    assertEqualsDouble(actual, expected, Tol)

  private def assertClose(actual: WorldPoint, expected: WorldPoint): Unit =
    assertClose(actual.x, expected.x)
    assertClose(actual.y, expected.y)
    assertClose(actual.z, expected.z)

  private def axisAlignedSpace: VolumeSpace =
    VolumeSpace(
      NeuroSpace(
        Vector(4, 3, 2),
        trans = Some(
          DMat.fromRows(
            Vector(
              Vector(2.0, 0.0, 0.0, 10.0),
              Vector(0.0, 3.0, 0.0, 20.0),
              Vector(0.0, 0.0, 4.0, 30.0),
              Vector(0.0, 0.0, 0.0, 1.0)
            )
          )
        )
      )
    )

  private def assertFootprintCovered(space: VolumeSpace, grid: SliceGrid): Unit =
    SliceGrid.boundaryCorners(space).foreach { corner =>
      val pixel = grid.project(corner).pixel
      assert(pixel.column >= -0.5 - Tol, clue = s"column ${pixel.column}")
      assert(pixel.column <= grid.dimensions.width - 0.5 + Tol, clue = s"column ${pixel.column}")
      assert(pixel.row >= -0.5 - Tol, clue = s"row ${pixel.row}")
      assert(pixel.row <= grid.dimensions.height - 0.5 + Tol, clue = s"row ${pixel.row}")
    }

  test("world directions are finite, normalized, and orthogonal by construction") {
    assert(UnitWorldVector.make(0.0, 0.0, 0.0).isLeft)
    assert(WorldVector.make(Double.NaN, 0.0, 0.0).isLeft)

    val direction = UnitWorldVector.unsafe(3.0, 4.0, 0.0)
    assertClose(direction.vector.length, 1.0)

    val diagonal = UnitWorldVector.unsafe(1.0, 1.0, 0.0)
    val invalid = SlicePlane.make(
      AnatomicalPlane.Axial,
      WorldPoint.Origin,
      AnatomicalDirection.Right.unit,
      diagonal
    )
    assert(invalid.isLeft)
  }

  test("canonical slice planes state the RAS display contract explicitly") {
    val leftOnLeft = SlicePlane.canonical(
      AnatomicalPlane.Axial,
      WorldPoint.Origin,
      LeftRightConvention.PatientLeftOnLeft
    )
    val rightOnLeft = SlicePlane.canonical(
      AnatomicalPlane.Axial,
      WorldPoint.Origin,
      LeftRightConvention.PatientRightOnLeft
    )

    assertClose(leftOnLeft.screenRight.x, 1.0)
    assertClose(leftOnLeft.screenUp.y, 1.0)
    assertClose(leftOnLeft.normal.z, 1.0)
    assertClose(rightOnLeft.screenRight.x, -1.0)
    assertClose(rightOnLeft.normal.z, -1.0)
    assertEquals(AnatomicalPlane.Axial.positiveNormal, AnatomicalDirection.Superior)

    val sagittalLeft = SlicePlane.canonical(
      AnatomicalPlane.Sagittal,
      WorldPoint.Origin,
      LeftRightConvention.PatientLeftOnLeft
    )
    val sagittalRight = SlicePlane.canonical(
      AnatomicalPlane.Sagittal,
      WorldPoint.Origin,
      LeftRightConvention.PatientRightOnLeft
    )
    assertEquals(sagittalLeft.screenRight, sagittalRight.screenRight)
  }

  test("axis-aligned grids cover voxel-cell boundaries and roundtrip pixel centers") {
    val space = axisAlignedSpace
    val cursor = space.voxelToWorld(VoxelPoint(1.5, 1.0, 0.5))
    val plane = SlicePlane.canonical(AnatomicalPlane.Axial, cursor)
    val grid = SliceGrid.covering(space, plane, PixelSpacing(1.0, 1.0))

    assertEquals(grid.dimensions, SliceDimensions(8, 9))
    assertFootprintCovered(space, grid)

    val pixel = PixelCoord(6, 7)
    val world = grid.worldAt(pixel).toOption.get
    val projected = grid.project(world)
    assertClose(projected.pixel.column, 6.0)
    assertClose(projected.pixel.row, 7.0)
    assertClose(projected.signedDistance, 0.0)
    assert(grid.containsProjection(world))
    assert(grid.worldAt(PixelCoord(grid.dimensions.width, 0)).isLeft)
  }

  test("screen mirroring changes presentation, never the world-space footprint") {
    val space = axisAlignedSpace
    val cursor = space.voxelToWorld(VoxelPoint(1.5, 1.0, 0.5))
    val left = SliceGrid.covering(
      space,
      SlicePlane.canonical(AnatomicalPlane.Axial, cursor, LeftRightConvention.PatientLeftOnLeft),
      PixelSpacing(1.0, 1.0)
    )
    val right = SliceGrid.covering(
      space,
      SlicePlane.canonical(AnatomicalPlane.Axial, cursor, LeftRightConvention.PatientRightOnLeft),
      PixelSpacing(1.0, 1.0)
    )

    assertEquals(left.dimensions, right.dimensions)
    val last = left.dimensions.width - 1
    assertClose(left.unsafeWorldAt(0, 0), right.unsafeWorldAt(last, 0))
    assertClose(left.unsafeWorldAt(last, 0), right.unsafeWorldAt(0, 0))
  }

  test("oblique grids cover all transformed boundary corners") {
    val affine = DMat.fromRows(
      Vector(
        Vector(0.0, -2.0, 0.25, 5.0),
        Vector(1.5, 0.0, 0.10, -4.0),
        Vector(0.0, 0.0, 3.0, 12.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
    val space = VolumeSpace(NeuroSpace(Vector(5, 4, 3), trans = Some(affine)))
    val cursor = space.voxelToWorld(VoxelPoint(2.0, 1.5, 1.0))

    AnatomicalPlane.values.foreach { anatomicalPlane =>
      val grid = SliceGrid.covering(
        space,
        SlicePlane.canonical(anatomicalPlane, cursor),
        PixelSpacing(0.75, 0.75)
      )
      assertFootprintCovered(space, grid)
    }
  }

  test("orthogonal plans are named, share one cursor, and pass through it") {
    val space = axisAlignedSpace
    val cursor = space.voxelToWorld(VoxelPoint(2.0, 1.0, 1.0))
    val grids = OrthogonalSliceGrids.native(space, cursor)

    assertEquals(grids.cursor, cursor)
    assertEquals(grids(AnatomicalPlane.Sagittal), grids.sagittal)
    assertEquals(grids(AnatomicalPlane.Coronal), grids.coronal)
    assertEquals(grids(AnatomicalPlane.Axial), grids.axial)
    grids.all.foreach { grid =>
      assertClose(grid.plane.signedDistance(cursor), 0.0)
      assertFootprintCovered(space, grid)
    }
  }

  test("slice dimensions and pixel spacing reject invalid states") {
    assert(SliceDimensions.make(0, 4).isLeft)
    assert(PixelSpacing.make(1.0, 0.0).isLeft)
    assert(PixelSpacing.make(Double.PositiveInfinity, 1.0).isLeft)
    assert(PixelCoord.make(-1, 0).isLeft)
    assert(ContinuousPixel.make(Double.NaN, 0.0).isLeft)
  }
