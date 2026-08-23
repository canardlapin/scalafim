package scalafim.image

class SpatialCoordinatesSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: SpatialPoint, expected: SpatialPoint, tol: Double): Unit =
    assertClose(actual.x, expected.x, tol)
    assertClose(actual.y, expected.y, tol)
    assertClose(actual.z, expected.z, tol)

  private def affine: DMat =
    DMat.fromRows(
      Vector(
        Vector(2.0, 0.0, 0.0, 10.0),
        Vector(0.0, 3.0, 0.0, 20.0),
        Vector(0.0, 0.0, 4.0, 30.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  test("RAS and LPS conversions flip the first two axes") {
    val lps = Vector(-10.0, -20.0, 30.0)
    val ras = SpatialCoordinates.lpsToRas(lps)

    assertClose(ras, Vector(10.0, 20.0, 30.0), 1e-10)
    assertClose(SpatialCoordinates.rasToLps(ras), lps, 1e-10)

    val points = Vector(lps, Vector(1.0, 2.0, 3.0))
    val converted = SpatialCoordinates.lpsToRasPoints(points)
    assertClose(converted(0), Vector(10.0, 20.0, 30.0), 1e-10)
    assertClose(converted(1), Vector(-1.0, -2.0, 3.0), 1e-10)
  }

  test("tkRAS conversions translate by c_ras and roundtrip") {
    val cRas = Vector(0.5, -2.0, 1.25)
    val tkras = Vector(10.0, 20.0, 30.0)
    val ras = SpatialCoordinates.tkrasToRas(tkras, cRas)

    assertClose(ras, Vector(10.5, 18.0, 31.25), 1e-10)
    assertClose(SpatialCoordinates.rasToTkras(ras, cRas), tkras, 1e-10)

    val points = SpatialCoordinates.tkrasToRasPoints(Vector(tkras, Vector(0.0, 0.0, 0.0)), cRas)
    assertClose(points(1), cRas, 1e-10)
  }

  test("voxel/world affine conversions use continuous zero-based voxel coordinates") {
    val voxel = Vector(1.5, 2.0, 3.0)
    val world = SpatialCoordinates.voxelToWorld(voxel, affine)
    assertClose(world, Vector(13.0, 26.0, 42.0), 1e-10)

    val back = SpatialCoordinates.worldToVoxel(world, affine).fold(err => fail(err.message), identity)
    assertClose(back, voxel, 1e-10)

    val many = SpatialCoordinates.voxelsToWorld(Vector(Vector(0.0, 0.0, 0.0), voxel), affine)
    assertClose(many(0), Vector(10.0, 20.0, 30.0), 1e-10)
    assertClose(many(1), world, 1e-10)
  }

  test("typed spatial points use the same coordinate conversions") {
    val voxel = SpatialPoint(1.5, 2.0, 3.0)
    val world = SpatialCoordinates.voxelToWorld(voxel, affine)

    assertClose(world, SpatialPoint(13.0, 26.0, 42.0), 1e-10)
    assertClose(
      SpatialCoordinates.worldToVoxel(world, affine).fold(err => fail(err.message), identity),
      voxel,
      1e-10
    )
    assertClose(SpatialCoordinates.lpsToRas(SpatialPoint(-1.0, -2.0, 3.0)), SpatialPoint(1.0, 2.0, 3.0), 1e-10)

    val voxels = Vector(SpatialPoint.Origin, voxel)
    val worlds = SpatialCoordinates.voxelPointsToWorld(voxels, affine)
    assertClose(worlds(0), SpatialPoint(10.0, 20.0, 30.0), 1e-10)
    assertClose(worlds(1), world, 1e-10)

    val roundtrip = SpatialCoordinates.worldPointsToVoxel(worlds, affine).fold(err => fail(err.message), identity)
    assertClose(roundtrip(0), voxels(0), 1e-10)
    assertClose(roundtrip(1), voxels(1), 1e-10)
  }

  test("worldToVoxel reports singular affine directly") {
    val singular =
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0, 0.0),
          Vector(0.0, 0.0, 0.0, 0.0),
          Vector(0.0, 0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )

    val out = SpatialCoordinates.worldToVoxel(Vector(1.0, 2.0, 3.0), singular)
    assert(out.isLeft, clue = "singular affine should be represented as an error")
  }

  test("GridSpec generates world coordinates in canonical last-axis-fastest order") {
    val grid = GridSpec(Vector(2, 2, 1), affine)
    val coords = grid.worldCoords

    assertEquals(coords.length, 4, clue = "")
    assertClose(coords(0), Vector(10.0, 20.0, 30.0), 1e-10)
    assertClose(coords(1), Vector(10.0, 23.0, 30.0), 1e-10)
    assertClose(coords(2), Vector(12.0, 20.0, 30.0), 1e-10)
    assertClose(coords(3), Vector(12.0, 23.0, 30.0), 1e-10)

    val fromObject = SpatialCoordinates.gridCoords(grid)
    assertEquals(fromObject, coords, clue = "")

    val points = grid.worldPoints
    assertClose(points(0), SpatialPoint(10.0, 20.0, 30.0), 1e-10)
    assertClose(points(3), SpatialPoint(12.0, 23.0, 30.0), 1e-10)
  }

  test("GridSpec exposes typed batch affine conversions") {
    val grid = GridSpec(Vector(3, 4, 5), affine)
    val voxels = Vector(SpatialPoint.Origin, SpatialPoint(1.0, 2.0, 3.0))
    val worlds = grid.voxelPointsToWorld(voxels)

    assertClose(worlds(0), SpatialPoint(10.0, 20.0, 30.0), 1e-10)
    assertClose(worlds(1), SpatialPoint(12.0, 26.0, 42.0), 1e-10)

    val roundtrip = grid.worldPointsToVoxel(worlds).fold(err => fail(err.message), identity)
    assertClose(roundtrip(0), voxels(0), 1e-10)
    assertClose(roundtrip(1), voxels(1), 1e-10)
  }

  test("GridSpec accepts typed spatial dims and reports invalid vector dims") {
    val shape = SpatialDims(2, 2, 1)
    val grid = GridSpec.fromSpatialDims(shape, affine)

    assertEquals(grid.shape, shape, clue = "")
    assertEquals(grid.dims, Vector(2, 2, 1), clue = "")
    assert(GridSpec.fromVector(Vector(2, 0, 1), affine).isLeft, clue = "grid dims should be positive")
    assert(GridSpec.fromVector(Vector(2, 2), affine).isLeft, clue = "grid dims should be exactly 3D")
  }

  test("GridSpec bridges to NeuroSpace without changing affine coordinates") {
    val grid = GridSpec(Vector(3, 4, 5), affine)
    val space = grid.toNeuroSpace
    val roundtrip = GridSpec.fromSpace(space)

    assertEquals(space.spatialDims, grid.dims, clue = "")
    assertEquals(roundtrip.affine, grid.affine, clue = "")
    assertClose(space.indexToCoord(Vector(1.0, 2.0, 3.0)), grid.voxelToWorld(Vector(1.0, 2.0, 3.0)), 1e-10)
  }
