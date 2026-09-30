package scalafim.spatial

import image4s.geometry.{Affine, D3}
import scalafim.image.*
import scalafim.image.SampleSpaces.*
import scalafim.image.world.SubjectId
import scalafim.surface.*

class VolumeToSurfaceParitySuite extends munit.FunSuite:
  private val space = SampleSpaces(Vector(3, 4, 5))

  private def ok[A](result: Either[SpatialError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def polynomial(x: Double, y: Double, z: Double): Double =
    2.0 + 3.0 * x + 5.0 * y + 7.0 * z + 11.0 * x * y + 13.0 * x * z + 17.0 * y * z + 19.0 * x * y * z

  private def volume(grid: SomeSampleSpace = space): SomeScalarVolume[Double] =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](grid.spatialDims.product): ordinal =>
        val p = grid.indexToGrid3D(ordinal)
        polynomial(p(0), p(1), p(2)),
      grid,
      "multiaffine-oracle"
    )

  private def sampled(weights: SurfacePointWeights, input: SomeScalarVolume[Double]): Double =
    weights.cols.indices.map(i => weights.values(i) * input.valueAtCanonicalOrdinal(weights.cols(i))).sum

  test("trilinear interpolation reproduces a closed-form multiaffine polynomial on an oblique anisotropic shifted grid"):
    val affine = Affine.fromRowMajor[D3](Vector(
      2.0, 0.5, 0.0, 10.0,
      0.0, 3.0, 0.25, -7.0,
      0.0, 0.0, 4.0, 5.0,
      0.0, 0.0, 0.0, 1.0
    )).toOption.get
    val oblique = SampleSpaces(Vector(3, 4, 5), affine = Some(affine))
    val input = volume(oblique)
    for
      x <- Vector(0.0, 0.125, 0.75, 1.5, 2.0)
      y <- Vector(0.0, 0.25, 1.625, 3.0)
      z <- Vector(0.0, 0.375, 2.125, 4.0)
    do
      // Independent forward affine expression; do not use the lookup's voxel/world conversion as its oracle.
      val world = SpatialPoint(2.0 * x + 0.5 * y + 10.0, 3.0 * y + 0.25 * z - 7.0, 4.0 * z + 5.0)
      val weights = VolumeToSurfaceOperatorCompiler.sourcePointWeights(GridSpec.fromSpace(oblique), None, world, SamplingPolicy.Trilinear)
      assertEqualsDouble(sampled(weights, input), polynomial(x, y, z), 1e-10)
      assertEqualsDouble(weights.coverage, 1.0, 1e-12)
      assertEqualsDouble(weights.values.sum, 1.0, 1e-12)

  test("trilinear impulse weights, masks, and partial support retain unnormalized coverage and normalize values"):
    val grid = GridSpec.fromSpace(space)
    val point = SpatialPoint(0.25, 0.5, 0.75)
    val weights = VolumeToSurfaceOperatorCompiler.sourcePointWeights(grid, None, point, SamplingPolicy.Trilinear)
    val impulse = SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](60)(i => if i == space.gridToIndex3D(1, 0, 1) then 1.0 else 0.0),
      space,
      "impulse"
    )
    assertEqualsDouble(sampled(weights, impulse), 0.25 * 0.5 * 0.75, 1e-12)

    val mask = Mask.fromIndices(space, PrimitiveBuffers.fromArray(Array(
      space.gridToIndex3D(1, 0, 0), space.gridToIndex3D(1, 1, 0),
      space.gridToIndex3D(1, 0, 1), space.gridToIndex3D(1, 1, 1)
    )), "x-one-plane")
    val masked = VolumeToSurfaceOperatorCompiler.sourcePointWeights(grid, Some(mask), point, SamplingPolicy.Trilinear)
    assertEqualsDouble(masked.coverage, 0.25, 1e-12)
    assertEqualsDouble(sampled(masked, volume()), polynomial(1.0, 0.5, 0.75), 1e-12)
    assertEqualsDouble(sampled(masked, impulse), 0.5 * 0.75, 1e-12)

    val edge = VolumeToSurfaceOperatorCompiler.sourcePointWeights(grid, None, SpatialPoint(-0.25, 0.5, 0.75), SamplingPolicy.Trilinear)
    assertEqualsDouble(edge.coverage, 0.75, 1e-12)
    assertEqualsDouble(sampled(edge, volume()), polynomial(0.0, 0.5, 0.75), 1e-12)
    val outside = VolumeToSurfaceOperatorCompiler.sourcePointWeights(grid, None, SpatialPoint(-1.0, 0.5, 0.75), SamplingPolicy.Trilinear)
    assertEquals(outside.cols, Vector.empty)
    assertEqualsDouble(outside.coverage, 0.0, 0.0)

  private def pair(points: Vector[Vector[Double]]): SurfaceGeometryPair =
    def geometry(kind: SurfaceKind, dz: Double): SurfaceGeometry =
      SurfaceGeometry(
        TriangleMesh.fromRows(points.map(p => Vector(p(0), p(1), p(2) + dz)), Vector.tabulate(points.length - 2)(i => (0, i + 1, i + 2))),
        Hemisphere.Left,
        kind
      )
    SurfaceGeometryPair(geometry(SurfaceKind.White, 0.0), geometry(SurfaceKind.Pial, 1.0))

  private def compile(surfaces: SurfaceGeometryPair, path: SurfaceSamplingPath, sampling: SamplingPolicy, mask: Option[SomeMaskVolume] = None): SpatialOperator =
    val subject = ok(SubjectId("parity-subject").asSpatial)
    val source = ok(Domain.build(ok(DomainId("volume")), SpaceRef.Volume(subject, None, ok(Modality("bold"))), ok(SamplingGeometry.volume(space, mask))))
    val target = ok(Domain.build(ok(DomainId("surface")), SpaceRef.Surface(subject, Hemisphere.Left, SurfaceKind.White), ok(SamplingGeometry.surface(surfaces.white))))
    val edge = ok(Morphism.build(ok(MorphismId("projection")), source.id, target.id, MorphismKind.VolumeToSurface, RouteTag.Anatomical, 1.0, inverse = Inverse.AdjointOnly))
    val graph = ok(SpatialGraph.build(Vector(source, target), Vector(edge)))
    ok(VolumeToSurfaceOperatorCompiler.compile(graph, VolumeToSurfaceRequest(source.id, target.id, surfaces, path, sampling)))

  test("public compiled nearest routes match eager averaging across all sampling paths and masks at half-voxel boundaries"):
    val surfaces = pair(Vector(
      Vector(0.5 - 1e-8, 0.0, 0.0), Vector(0.5, 1.0, 0.0),
      Vector(2.5 - 1e-8, 2.0, 0.0), Vector(2.5, 3.0, 0.0), Vector(-0.5, 0.0, 1.0)
    ))
    val paths = Vector(
      SurfaceSamplingPath.White -> 1, SurfaceSamplingPath.Pial -> 1, SurfaceSamplingPath.Midpoint -> 1,
      SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.5, 1.0)) -> 3,
      SurfaceSamplingPath.NormalLine(Vector(-0.5, 0.0, 0.5)) -> 3
    )
    val input = volume()
    val mask = Mask.fromIndices(space, PrimitiveBuffers.fromArray((0 until 60).filter(i => i % 5 != 0).toArray), "omit-z-zero")
    val matrix = DoubleMatrix.fromRows(Vector.tabulate(60)(i => Vector(input.valueAtCanonicalOrdinal(i))))
    for
      (path, requested) <- paths
      selectedMask <- Vector(None, Some(mask))
    do
      val operator = compile(surfaces, path, SamplingPolicy.Nearest, selectedMask)
      val output = operator.forward(matrix).fold(error => fail(error.getMessage), identity)
      val eager = VolumeSurfaceSampler.sample(input, surfaces, path, SurfaceSampleAggregation.Average, selectedMask)
      surfaces.white.mesh.vertices.indices.foreach: row =>
        val id = VertexId(row)
        val count = eager.sampleCounts.valueAt(id).get
        assertEqualsDouble(operator.qc.coverage.rowCoverage(row), count.toDouble / requested, 1e-12)
        if count > 0 then assertEqualsDouble(output(row, 0), eager.values.valueAt(id).get, 1e-10)
        else assertEqualsDouble(output(row, 0), 0.0, 0.0) // Sparse empty row; eager returns NaN.

  test("public compiled trilinear midpoint route preserves asymmetric values and partial coverage"):
    val surfaces = pair(Vector(Vector(0.25, 0.5, 0.25), Vector(-0.25, 0.5, 0.25), Vector(-1.0, 0.5, 0.25)))
    val operator = compile(surfaces, SurfaceSamplingPath.Midpoint, SamplingPolicy.Trilinear)
    val input = volume()
    val output = operator.forward(DoubleMatrix.fromRows(Vector.tabulate(60)(i => Vector(input.valueAtCanonicalOrdinal(i)))))
      .fold(error => fail(error.getMessage), identity)
    assertEqualsDouble(output(0, 0), polynomial(0.25, 0.5, 0.75), 1e-12)
    assertEqualsDouble(output(1, 0), polynomial(0.0, 0.5, 0.75), 1e-12)
    assertEqualsDouble(output(2, 0), 0.0, 0.0)
    assertEquals(operator.qc.coverage.rowCoverage, Vector(1.0, 0.75, 0.0))

  private val outsidePoints = Vector(
    Vector(4294967296.0, 0.0, 0.0), Vector(-4294967296.0, 1.0, 0.0),
    Vector(0.0, 4294967296.0, 0.0), Vector(0.0, 0.0, 4294967296.0)
  )

  test("nearest spatial weights reject far-outside coordinates before narrowing indices"):
    outsidePoints.foreach: point =>
      val weights = VolumeToSurfaceOperatorCompiler.sourcePointWeights(
        GridSpec.fromSpace(space), None, SpatialPoint(point(0), point(1), point(2)), SamplingPolicy.Nearest
      )
      assertEquals(weights.cols, Vector.empty)
      assertEqualsDouble(weights.coverage, 0.0, 0.0)

  test("eager nearest sampling rejects far-outside coordinates before narrowing indices"):
    val surfaces = pair(outsidePoints :+ Vector(0.0, 0.0, 0.0))
    val eager = VolumeSurfaceSampler.sample(volume(), surfaces, SurfaceSamplingPath.White)
    assertEquals((0 until 5).map(i => eager.sampleCounts.valueAt(VertexId(i)).get).toVector, Vector(0, 0, 0, 0, 1))
    assertEquals(eager.tally, SurfaceSampleTally(5, 4, 0, 0, 1))

  test("public nearest operator gives uncovered rows for far-outside coordinates"):
    val surfaces = pair(outsidePoints :+ Vector(0.0, 0.0, 0.0))
    val operator = compile(surfaces, SurfaceSamplingPath.White, SamplingPolicy.Nearest)
    assertEquals(operator.qc.coverage.rowCoverage, Vector(0.0, 0.0, 0.0, 0.0, 1.0))
