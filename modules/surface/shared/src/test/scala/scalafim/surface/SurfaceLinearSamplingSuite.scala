package scalafim.surface

import image4s.geometry.{Affine, D3}
import scalafim.image.*
import scalafim.image.SampleSpaces.*

class SurfaceLinearSamplingSuite extends munit.FunSuite:

  private val space = SampleSpaces(Vector(3, 3, 3))

  private def affine(x: Int, y: Int, z: Int): Double =
    2.0 + 3.0 * x + 5.0 * y + 7.0 * z

  private def volume(values: (Int, Int, Int) => Double = affine): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(
      Array.tabulate(27): ordinal =>
        val grid = space.indexToGrid3D(ordinal)
        values(grid(0), grid(1), grid(2)),
      space,
      "linear-sampling"
    )

  private def surfaces(points: Vector[Vector[Double]], surfaceToWorld: Affine[D3] = Affine.identity[D3]): SurfaceGeometryPair =
    val mesh = TriangleMesh.fromRows(points, Vector((0, 1, 2)))
    val white = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White, surfaceToWorld)
    val pial = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial, surfaceToWorld)
    SurfaceGeometryPair(white, pial)

  private def sampler(points: Vector[Vector[Double]], interpolation: SurfaceVoxelInterpolation = SurfaceVoxelInterpolation.Linear,
      surfaceToWorld: Affine[D3] = Affine.identity[D3]): VolumeSurfaceSampler =
    VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(surfaces(points, surfaceToWorld), SurfaceSamplingPath.White,
      SurfaceSampleAggregation.Nearest, interpolation))

  test("linear sampling delegates affine values through transformed volume and surface coordinates"):
    val shifted = SampleSpaces(Vector(3, 3, 3), affine = Some(translation(10.0, -2.0, 4.0)))
    val shiftedVolume = SomeScalarVolume.unsafeCopyFromCanonicalArray(
      Array.tabulate(27): ordinal =>
        val grid = shifted.indexToGrid3D(ordinal)
        affine(grid(0), grid(1), grid(2)),
      shifted,
      "shifted-linear"
    )
    val surfaceShift = translation(10.0, -2.0, 4.0)
    val result = sampler(Vector(Vector(0.25, 0.5, 0.75), Vector(1.0, 1.0, 1.0), Vector(2.0, 2.0, 2.0)), surfaceToWorld = surfaceShift).sample(shiftedVolume)
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 2.0 + 3.0 * 0.25 + 5.0 * 0.5 + 7.0 * 0.75, 1e-12)
    assertEquals(result.tally, SurfaceSampleTally(3, 0, 0, 0, 3))

  test("nearest remains half-up by default and linear receipts expose only positive-weight contributors"):
    val points = Vector(Vector(0.5, 0.0, 0.0), Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0))
    val nearest = sampler(points, SurfaceVoxelInterpolation.Nearest).sample(volume())
    assertEqualsDouble(nearest.values.valueAt(VertexId(0)).get, affine(1, 0, 0), 0.0)
    val receipt = sampler(points).inspectVertex(volume(), VertexId(0))
    receipt.samples.head.outcome match
      case SurfaceSampleOutcome.Interpolated(contributors, value) =>
        assertEqualsDouble(value, affine(0, 0, 0) * 0.5 + affine(1, 0, 0) * 0.5, 1e-12)
        assertEquals(contributors.map(_.voxel), Vector(VoxelCoord(0, 0, 0), VoxelCoord(1, 0, 0)))
        contributors.foreach(contributor => assertEqualsDouble(contributor.weight, 0.5, 1e-12))
      case other => fail(s"expected interpolated receipt, got $other")
    val missing = sampler(points, SurfaceVoxelInterpolation.Nearest).inspectVertex(volume((_, _, _) => Double.NaN), VertexId(0))
    assert(missing.hasNonFiniteSamples)
    assertEquals(missing.acceptedCount, 0)
    missing.samples.head.outcome match
      case SurfaceSampleOutcome.Included(voxel, value) =>
        assertEquals(voxel, VoxelCoord(1, 0, 0))
        assert(value.isNaN)
      case other => fail(s"nearest inspection compatibility changed: $other")

  test("linear support is the closed voxel-centre hull, including exact maxima and huge coordinates"):
    val points = Vector(Vector(2.0, 2.0, 2.0), Vector(-1e-12, 0.0, 0.0), Vector(Double.MaxValue, 0.0, 0.0))
    val result = sampler(points).sample(volume())
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, affine(2, 2, 2), 0.0)
    assert(result.values.valueAt(VertexId(1)).exists(_.isNaN))
    assert(result.values.valueAt(VertexId(2)).exists(_.isNaN))
    assertEquals(result.tally, SurfaceSampleTally(3, 2, 0, 0, 1))

  test("sheared affine boundary admission uses exactly the provider's coordinate evaluation"):
    val affine = Affine.fromRowMajor[D3](Vector(
      1.0, -1.0, -1.0, 1.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0)).toOption.get
    val shifted = SampleSpaces(Vector(3, 3, 4), affine = Some(affine))
    val data = SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.fill(36)(7.0), shifted, "sheared")
    val points = Vector(Vector(.05, .46, 2.49), Vector(.25, .5, .75), Vector(.5, .5, .5))
    val sampled = sampler(points)
    val result = sampled.sample(data)
    assertEquals(result.tally, SurfaceSampleTally(3, 1, 0, 0, 2))
    assert(result.values.valueAt(VertexId(0)).get.isNaN)
    // At the interior point provider IJK is (.5,.5,.75), independent of the
    // implementation's returned contributors.
    sampled.inspectVertex(data, VertexId(1)).samples.head.outcome match
      case SurfaceSampleOutcome.Interpolated(contributors, value) =>
        assertEqualsDouble(value, 7.0, 1e-12)
        assertEquals(contributors.size, 8)
        contributors.foreach(c => assertEqualsDouble(c.weight, if c.voxel.z == 0 then .0625 else .1875, 1e-12))
      case other => fail(s"expected a linear receipt, got $other")

  test("default nearest sampling retains outside classification when finite transformed coordinates overflow"):
    val scale = Affine.fromRowMajor[D3](Vector(
      2.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0)).toOption.get
    val pair = surfaces(Vector(Vector(Double.MaxValue, 0.0, 0.0), Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0)), scale)
    val result = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.White)).sample(volume())
    assertEquals(result.tally, SurfaceSampleTally(3, 1, 0, 0, 2))
    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(0))
    assert(result.values.valueAt(VertexId(0)).get.isNaN)

  test("linear sampling rejects a masked or non-finite positive-weight corner as a whole point"):
    val points = Vector(Vector(0.5, 0.0, 0.0), Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0))
    val flags = Array.fill(27)(true)
    flags(space.gridToIndex3D(1, 0, 0)) = false
    val mask = SomeMaskVolume.unsafeCopyFromCanonicalArray(flags, space, "corner-mask")
    val masked = sampler(points).sample(volume(), Some(mask))
    assertEquals(masked.tally, SurfaceSampleTally(3, 0, 1, 0, 2))
    assertEquals(masked.sampleCounts.valueAt(VertexId(0)), Some(0))

    val nonFinite = sampler(points).sample(volume((x, y, z) => if x == 1 && y == 0 && z == 0 then Double.NaN else affine(x, y, z)))
    assertEquals(nonFinite.tally, SurfaceSampleTally(3, 0, 0, 1, 2))
    assertEquals(nonFinite.sampleCounts.valueAt(VertexId(0)), Some(0))
    assertEquals(nonFinite.nonFiniteCounts.valueAt(VertexId(0)), Some(1))

  test("zero-weight neighbours do not affect an exact voxel-centre linear sample"):
    val points = Vector(Vector(0.0, 0.0, 0.0), Vector(0.0, 1.0, 0.0), Vector(0.0, 0.0, 1.0))
    val flags = Array.fill(27)(true)
    flags(space.gridToIndex3D(1, 0, 0)) = false
    val mask = SomeMaskVolume.unsafeCopyFromCanonicalArray(flags, space, "zero-weight-mask")
    val maskedNeighbour = sampler(points).sample(volume(), Some(mask))
    assertEquals(maskedNeighbour.tally, SurfaceSampleTally(3, 0, 0, 0, 3))

    val nonFiniteNeighbour = sampler(points).sample(volume((x, y, z) => if x == 1 && y == 0 && z == 0 then Double.NaN else affine(x, y, z)))
    assertEquals(nonFiniteNeighbour.tally, SurfaceSampleTally(3, 0, 0, 0, 3))

  private def translation(x: Double, y: Double, z: Double): Affine[D3] =
    Affine.fromRowMajor[D3](Vector(
      1.0, 0.0, 0.0, x,
      0.0, 1.0, 0.0, y,
      0.0, 0.0, 1.0, z,
      0.0, 0.0, 0.0, 1.0
    )).toOption.get
