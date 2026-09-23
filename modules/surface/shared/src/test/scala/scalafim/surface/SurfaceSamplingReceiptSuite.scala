package scalafim.surface

import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.image.*
import scalafim.image.SampleSpaces.*

class SurfaceSamplingReceiptSuite extends munit.FunSuite:
  private val affine = Affine.fromRowMajor[D3](Vector(
    0.0, -2.0, 0.0, 10.0, 1.0, 0.0, 0.0, -5.0,
    0.0, 0.0, 3.0, 2.0, 0.0, 0.0, 0.0, 1.0)).toOption.get
  private val space = SampleSpaces(Vector(3, 3, 3), affine = Some(affine))
  private val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.tabulate[Double](27) { i =>
    val g = space.indexToGrid3D(i)
    (g(0) + 10 * g(1) + 100 * g(2)).toDouble
  }, space, "oblique")
  private def geometry(z: Double, kind: SurfaceKind) = SurfaceGeometry(
    TriangleMesh.fromRows(Vector(Vector(0.0, 0.0, z), Vector(1.0, 0.0, z), Vector(0.0, 1.0, z)), Vector((0, 1, 2))),
    Hemisphere.Left, kind, affine)
  private val pair = SurfaceGeometryPair(geometry(0, SurfaceKind.White), geometry(2, SurfaceKind.Pial))
  private val mask = SomeMaskVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.tabulate[Boolean](27) { i =>
    space.indexToGrid3D(i) != Vector(0, 0, 1)
  }, space, "mask")

  test("oblique native midpoint receipt reports exact requested world and selected voxel"):
    val sampler = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair))
    val receipt = sampler.inspectVertex(volume, VertexId(1), Some(mask))
    assertEqualsDouble(receipt.samples.head.world.x, 10.0, 1e-12)
    assertEqualsDouble(receipt.samples.head.world.y, -4.0, 1e-12)
    assertEqualsDouble(receipt.samples.head.world.z, 5.0, 1e-12)
    assertEquals(receipt.samples.head.outcome, SurfaceSampleOutcome.Included(VoxelCoord(1, 0, 1), 101.0))
    assertEqualsDouble(receipt.value, 101.0, 1e-12)
    assertEquals(receipt.contributingSampleIndices, Vector(0))

  test("normal-line receipts distinguish outside, masked and contributing samples in physical millimeters"):
    val sampler = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair,
      SurfaceSamplingPath.NormalLine(Vector(-6.0, 0.0, 3.0)), SurfaceSampleAggregation.Nearest))
    val receipt = sampler.inspectVertex(volume, VertexId(0), Some(mask))
    assertEquals(receipt.samples.map(_.outcome), Vector(SurfaceSampleOutcome.OutsideVolume,
      SurfaceSampleOutcome.Masked(VoxelCoord(0, 0, 1)), SurfaceSampleOutcome.Included(VoxelCoord(0, 0, 2), 200.0)))
    assertEqualsDouble(receipt.samples(0).world.z, -1.0, 1e-12)
    assertEqualsDouble(receipt.samples(1).world.z, 5.0, 1e-12)
    assertEqualsDouble(receipt.samples(2).world.z, 8.0, 1e-12)
    assertEquals(receipt.acceptedCount, 1)
    assertEquals(receipt.contributingSampleIndices, Vector(2))
    assertEqualsDouble(receipt.value, 200.0, 1e-12)

  test("all paths and reducers reconstruct ordinary values and counts, retaining duplicate requests"):
    val paths = Vector(SurfaceSamplingPath.White, SurfaceSamplingPath.Pial, SurfaceSamplingPath.Midpoint,
      SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.0, 1.0)), SurfaceSamplingPath.NormalLine(Vector(-6.0, 0.0, 3.0)))
    for path <- paths; reducer <- SurfaceSampleAggregation.values do
      val sampler = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair, path, reducer))
      val ordinary = sampler.sample(volume, Some(mask))
      for index <- 0 until 3 do
        val receipt = sampler.inspectVertex(volume, VertexId(index), Some(mask))
        assertEquals(receipt.acceptedCount, ordinary.sampleCounts.valueAt(VertexId(index)).get)
        val values = receipt.samples.collect { case SurfacePointSample(_, SurfaceSampleOutcome.Included(_, value)) => value }
        val expected = if values.isEmpty then Double.NaN else reducer match
          case SurfaceSampleAggregation.Nearest => values.head
          case SurfaceSampleAggregation.Average => values.sum / values.size
          case SurfaceSampleAggregation.Mode => values.groupBy(identity).toVector.sortBy { (value, occurrences) => (-occurrences.size, value) }.head._1
        if expected.isNaN then assert(receipt.value.isNaN)
        else assertEqualsDouble(receipt.value, expected, 1e-12)
        val actual = ordinary.values.valueAt(VertexId(index)).get
        if actual.isNaN then assert(receipt.value.isNaN)
        else assertEqualsDouble(receipt.value, actual, 1e-12)
        assertEquals(receipt.contributingSampleIndices,
          if reducer == SurfaceSampleAggregation.Nearest then receipt.acceptedSampleIndices.take(1) else receipt.acceptedSampleIndices)
    val repeated = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair,
      SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.0, 1.0)), SurfaceSampleAggregation.Average))
      .inspectVertex(volume, VertexId(0), Some(mask))
    assertEquals(repeated.acceptedCount, 3)
    assertEquals(repeated.samples(0).outcome, repeated.samples(1).outcome)
    assertEqualsDouble(repeated.value, 200.0 / 3.0, 1e-12)

  test("fractional-thickness Mode and Average match construction, independent of the bulk kernel"):
    // surfaceToWorld equals the grid's voxel-to-world affine, so surface coordinates
    // are voxel indices: vertex 1 is voxel (1, 0, 0) on white (z = 0) and (1, 0, 2)
    // on pial (z = 2). Fractions (0, 0, 1) request voxels (1,0,0), (1,0,0), (1,0,2)
    // holding 1, 1 and 201; none is masked.
    val path = SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.0, 1.0))
    val mode = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair, path, SurfaceSampleAggregation.Mode))
      .inspectVertex(volume, VertexId(1), Some(mask))
    assertEquals(mode.samples.map(_.outcome), Vector(
      SurfaceSampleOutcome.Included(VoxelCoord(1, 0, 0), 1.0),
      SurfaceSampleOutcome.Included(VoxelCoord(1, 0, 0), 1.0),
      SurfaceSampleOutcome.Included(VoxelCoord(1, 0, 2), 201.0)))
    assertEqualsDouble(mode.value, 1.0, 0.0)
    assertEquals(mode.contributingSampleIndices, Vector(0, 1, 2))
    val average = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair, path, SurfaceSampleAggregation.Average))
    assertEqualsDouble(average.inspectVertex(volume, VertexId(1), Some(mask)).value, 203.0 / 3.0, 1e-12)
    assertEqualsDouble(average.sample(volume, Some(mask)).values.valueAt(VertexId(1)).get, 203.0 / 3.0, 1e-12)

  test("nonfinite included values retain existing aggregation/count semantics and are explicit"):
    val nonfinite = SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](27, Double.NaN), space, "nan")
    val sampler = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair))
    val receipt = sampler.inspectVertex(nonfinite, VertexId(1))
    assertEquals(receipt.acceptedCount, 1)
    assert(receipt.hasNonFiniteSamples)
    assert(receipt.value.isNaN)
    assertEquals(sampler.sample(nonfinite).sampleCounts.valueAt(VertexId(1)), Some(1))
    assert(sampler.inspectVertexEither(volume, VertexId(3)).isLeft)
    val wrongMask = SomeMaskVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fillConst[Boolean](8, true),
      SampleSpaces(Vector(2, 2, 2)), "wrong-mask")
    assert(sampler.inspectVertexEither(volume, VertexId(0), Some(wrongMask)).isLeft)
