package scalafim.surface

import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.image.*
import scalafim.image.SampleSpaces.*

class SurfaceSamplingSuite extends munit.FunSuite:

  private val space = SampleSpaces(Vector(3, 3, 3))
  private val volume =
    SomeScalarVolume.unsafeCopyFromCanonicalArray(
      PrimitiveBuffers.tabulate[Double](27) { idx =>
        val g = space.indexToGrid3D(idx)
        g(0).toDouble + 10.0 * g(1).toDouble + 100.0 * g(2).toDouble
      },
      space,
      "synthetic"
    )

  private val pair =
    SurfaceGeometryPair(
      surfaceAtZ(0.0, SurfaceKind.White),
      surfaceAtZ(2.0, SurfaceKind.Pial)
    )

  test("SurfaceGeometryPair exposes a shared cortical domain"):
    assertEquals(pair.domainEither, scala.util.Right(SurfaceDomain(CorticalHemisphere.Left, 3)))

  test("midpoint path is a named policy that preserves midpoint sampling semantics"):
    val plan = VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.Midpoint, SurfaceSampleAggregation.Nearest)
    val sampler = VolumeSurfaceSampler(plan)
    val result = sampler.sample(volume)

    assertEquals(sampler.plan, plan)
    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(1))
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 100.0, 1e-12)
    assertEqualsDouble(result.values.valueAt(VertexId(1)).get, 101.0, 1e-12)
    assertEqualsDouble(result.values.valueAt(VertexId(2)).get, 110.0, 1e-12)

  test("white and pial paths sample endpoint surfaces"):
    val white =
      VolumeSurfaceSampler.sample(volume, pair, path = SurfaceSamplingPath.White)
    val pial =
      VolumeSurfaceSampler.sample(volume, pair, path = SurfaceSamplingPath.Pial)

    assertEqualsDouble(white.values.valueAt(VertexId(0)).get, 0.0, 1e-12)
    assertEqualsDouble(pial.values.valueAt(VertexId(0)).get, 200.0, 1e-12)

  test("surfaceToWorld transforms are applied before SomeSampleSpace coordinate lookup"):
    val flat = flatTriangle(SurfaceKind.White)
    val translated =
      SurfaceGeometry(
        flat.mesh,
        Hemisphere.Left,
        SurfaceKind.Pial,
        translation(0.0, 0.0, 2.0)
      )
    val result =
      VolumeSurfaceSampler.sample(
        volume,
        SurfaceGeometryPair(flat, translated),
        path = SurfaceSamplingPath.Midpoint
      )

    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 100.0, 1e-12)

  test("fractional thickness supports average aggregation"):
    val result =
      VolumeSurfaceSampler.sample(
        volume,
        pair,
        path = SurfaceSamplingPath.FractionalThickness(Vector(0.0, 1.0)),
        aggregation = SurfaceSampleAggregation.Average
      )

    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(2))
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 100.0, 1e-12)
    assertEqualsDouble(result.values.valueAt(VertexId(1)).get, 101.0, 1e-12)

  test("mode aggregation chooses the most frequent sampled value"):
    val result =
      VolumeSurfaceSampler.sample(
        volume,
        pair,
        path = SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.0, 1.0)),
        aggregation = SurfaceSampleAggregation.Mode
      )

    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(3))
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 0.0, 1e-12)

  test("normal-line sampling uses offsets around the midpoint"):
    val result =
      VolumeSurfaceSampler.sample(
        volume,
        pair,
        path = SurfaceSamplingPath.NormalLine(Vector(-1.0, 0.0, 1.0)),
        aggregation = SurfaceSampleAggregation.Average
      )

    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(3))
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 100.0, 1e-12)

  test("masking can produce explicit empty samples"):
    val mask =
      SomeMaskVolume.unsafeCopyFromCanonicalArray(
        PrimitiveBuffers.fillConst[Boolean](27, false),
        space,
        "empty-mask"
      )
    val result = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(volume, Some(mask))

    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(0))
    assert(result.values.valueAt(VertexId(0)).exists(_.isNaN))

  test("volume-to-surface morphism wraps reusable sampling plans"):
    val morphism =
      VolToSurfMorphism(
        SurfaceDomainId("volume"),
        SurfaceDomainId("surface"),
        VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.Midpoint, SurfaceSampleAggregation.Nearest)
      )
    val result = morphism.sample(volume)

    assertEquals(morphism.kind, SurfaceMorphismKind.VolumeToSurface)
    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(1))
    assertEqualsDouble(result.values.valueAt(VertexId(0)).get, 100.0, 1e-12)

  test("surface-to-surface morphism applies explicit target-to-source vertex maps"):
    val geometry = surfaceAtZ(0.0, SurfaceKind.White)
    val field = SurfaceField.full(geometry, Vector(1.0, 2.0, 3.0), "source")
    val mapping =
      SurfaceVertexMapping.nearestIndex(
        sourceGeometry = geometry,
        targetGeometry = geometry,
        sourceForTarget = Vector(VertexId(2), VertexId(1), VertexId(0))
      )
    val morphism =
      SurfToSurfMorphism(SurfaceDomainId("source-surface"), SurfaceDomainId("target-surface"), mapping)
    val out = morphism.resample(field)

    assertEquals(morphism.kind, SurfaceMorphismKind.SurfaceToSurface)
    assertEqualsDouble(out.valueAt(VertexId(0)).get, 3.0, 1e-12)
    assertEqualsDouble(out.valueAt(VertexId(1)).get, 2.0, 1e-12)
    assertEqualsDouble(out.valueAt(VertexId(2)).get, 1.0, 1e-12)

  test("out-of-bounds sample points produce empty samples"):
    val shifted =
      SurfaceGeometry(
        pair.white.mesh,
        Hemisphere.Left,
        SurfaceKind.White,
        translation(10.0, 0.0, 0.0)
      )
    val result =
      VolumeSurfaceSampler.sample(
        volume,
        SurfaceGeometryPair(shifted, shifted),
        path = SurfaceSamplingPath.White
      )

    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(0))
    assert(result.values.valueAt(VertexId(0)).exists(_.isNaN))

  test("sampling plans validate surface pairs, paths, and masks"):
    val mismatched =
      SurfaceGeometry(
        TriangleMesh.fromRows(
          Vector(
            Vector(0.0, 0.0, 0.0),
            Vector(1.0, 0.0, 0.0),
            Vector(0.0, 1.0, 0.0),
            Vector(1.0, 1.0, 0.0)
          ),
          Vector((0, 1, 2), (1, 3, 2))
        ),
        Hemisphere.Left,
        SurfaceKind.Pial
      )

    interceptMessage[IllegalArgumentException]("requirement failed: white and pial surfaces must have the same vertex count"):
      SurfaceGeometryPair(pair.white, mismatched)
    assert(SurfaceGeometryPair.fromEither(pair.white, mismatched).isLeft)

    interceptMessage[IllegalArgumentException]("requirement failed: white and pial surfaces must have the same hemisphere"):
      SurfaceGeometryPair(pair.white, SurfaceGeometry(pair.pial.mesh, Hemisphere.Right, SurfaceKind.Pial))

    val rewound =
      SurfaceGeometry(
        TriangleMesh.fromRows(
          Vector(
            Vector(0.0, 0.0, 2.0),
            Vector(1.0, 0.0, 2.0),
            Vector(0.0, 1.0, 2.0)
          ),
          Vector((0, 2, 1))
        ),
        Hemisphere.Left,
        SurfaceKind.Pial
      )
    interceptMessage[IllegalArgumentException]("requirement failed: white and pial surfaces must share ordered triangle topology"):
      SurfaceGeometryPair(pair.white, rewound)
    assert(SurfaceGeometryPair.fromEither(pair.white, rewound).isLeft)

    interceptMessage[IllegalArgumentException]("requirement failed: fractional thickness path must contain at least one fraction"):
      VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.FractionalThickness(Vector.empty))

    interceptMessage[IllegalArgumentException]("requirement failed: fractional thickness values must be in [0, 1]"):
      VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.FractionalThickness(Vector(-0.1)))

    interceptMessage[IllegalArgumentException]("requirement failed: normal-line path must contain at least one offset"):
      VolumeSurfaceSamplingPlan(pair, SurfaceSamplingPath.NormalLine(Vector.empty))

    val badMask =
      SomeMaskVolume.unsafeCopyFromCanonicalArray(
        PrimitiveBuffers.fillConst[Boolean](8, true),
        SampleSpaces(Vector(2, 2, 2)),
        "bad-mask"
      )
    interceptMessage[IllegalArgumentException]("requirement failed: mask/volume space mismatch"):
      VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(volume, Some(badMask))

  test("the sample tally observes every requested point exactly once"):
    val inside = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(volume)
    assertEquals(inside.tally, SurfaceSampleTally(requested = 3, outsideVolume = 0, masked = 0, nonFinite = 0, accepted = 3))

    val ribbon = VolumeSurfaceSampler.sample(volume, pair, path = SurfaceSamplingPath.FractionalThickness(Vector(0.0, 0.5, 1.0)))
    assertEquals(ribbon.tally.requested, 9L)
    assertEquals(ribbon.tally.accepted, 9L)

    val emptyMask = SomeMaskVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.fillConst[Boolean](27, false), space, "empty-mask")
    val masked = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(volume, Some(emptyMask))
    assertEquals(masked.tally, SurfaceSampleTally(requested = 3, outsideVolume = 0, masked = 3, nonFinite = 0, accepted = 0))

    val shifted = SurfaceGeometry(pair.white.mesh, Hemisphere.Left, SurfaceKind.White, translation(10.0, 0.0, 0.0))
    val outside = VolumeSurfaceSampler.sample(volume, SurfaceGeometryPair(shifted, shifted), path = SurfaceSamplingPath.White)
    assertEquals(outside.tally, SurfaceSampleTally(requested = 3, outsideVolume = 3, masked = 0, nonFinite = 0, accepted = 0))

  test("non-finite volume values are tallied apart from accepted samples"):
    // Vertex 0 samples voxel (0, 0, 1) at the midpoint.
    val withNaN =
      SomeScalarVolume.unsafeCopyFromCanonicalArray(
        PrimitiveBuffers.tabulate[Double](27) { idx =>
          val g = space.indexToGrid3D(idx)
          if g == Vector(0, 0, 1) then Double.NaN else g(0).toDouble + 10.0 * g(1).toDouble + 100.0 * g(2).toDouble
        },
        space,
        "with-nan"
      )
    val result = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(withNaN)
    assertEquals(result.tally, SurfaceSampleTally(requested = 3, outsideVolume = 0, masked = 0, nonFinite = 1, accepted = 2))
    assertEquals(result.tally.rejected, 1L)
    // Per-vertex counts keep their documented meaning: in-volume, in-mask samples.
    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(1))
    assert(result.values.valueAt(VertexId(0)).exists(_.isNaN))

  test("sample tallies must account for every requested sample"):
    intercept[IllegalArgumentException](SurfaceSampleTally(requested = 3, outsideVolume = 1, masked = 0, nonFinite = 0, accepted = 1))
    intercept[IllegalArgumentException](SurfaceSampleTally(requested = 0, outsideVolume = -1, masked = 0, nonFinite = 0, accepted = 1))


  Vector(0 -> 3, 1 -> 4, 2 -> 5).foreach: (axis, length) =>
    test(s"eager nearest sampling has absolute half-up ties and support boundaries on axis $axis"):
      val grid = SampleSpaces(Vector(3, 4, 5))
      val encoded = SomeScalarVolume.unsafeCopyFromCanonicalArray(
        PrimitiveBuffers.tabulate[Double](60): ordinal =>
          val x = ordinal / 20
          val y = (ordinal / 5) % 4
          val z = ordinal % 5
          x.toDouble + 10.0 * y + 100.0 * z,
        grid,
        "absolute-nearest-oracle"
      )
      val epsilon = 1e-8
      val cases = Vector(
        (-0.5 - epsilon, None), (-0.5, Some(0)), (-0.5 + epsilon, Some(0)),
        (0.5 - epsilon, Some(0)), (0.5, Some(1)), (0.5 + epsilon, Some(1)),
        (length - 0.5 - epsilon, Some(length - 1)), (length - 0.5, None), (length - 0.5 + epsilon, None)
      )
      val points = cases.map((coordinate, _) => Vector(1.0, 1.0, 1.0).updated(axis, coordinate))
      val mesh = TriangleMesh.fromRows(points, Vector.tabulate(points.length - 2)(i => (0, i + 1, i + 2)))
      val surfaces = SurfaceGeometryPair(
        SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White),
        SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial)
      )
      val result = VolumeSurfaceSampler.sample(encoded, surfaces, SurfaceSamplingPath.White)
      cases.zipWithIndex.foreach: (entry, row) =>
        val (coordinate, expectedIndex) = entry
        val vertex = VertexId(row)
        assertEquals(result.sampleCounts.valueAt(vertex), Some(if expectedIndex.isDefined then 1 else 0),
          s"axis=$axis coordinate=$coordinate")
        expectedIndex match
          case Some(index) =>
            val p = Vector(1, 1, 1).updated(axis, index)
            assertEqualsDouble(result.values.valueAt(vertex).get, p(0) + 10.0 * p(1) + 100.0 * p(2), 0.0)
          case None => assert(result.values.valueAt(vertex).exists(_.isNaN))
      assertEquals(result.tally, SurfaceSampleTally(9, 3, 0, 0, 6))

  private def surfaceAtZ(z: Double, kind: SurfaceKind): SurfaceGeometry =
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(0.0, 0.0, z),
          Vector(1.0, 0.0, z),
          Vector(0.0, 1.0, z)
        ),
        Vector((0, 1, 2))
      ),
      Hemisphere.Left,
      kind
    )

  private def flatTriangle(kind: SurfaceKind): SurfaceGeometry =
    surfaceAtZ(0.0, kind)

  private def translation(x: Double, y: Double, z: Double): Affine[D3] =
    Affine.fromRowMajor[D3](
      Vector(
        1.0, 0.0, 0.0, x,
        0.0, 1.0, 0.0, y,
        0.0, 0.0, 1.0, z,
        0.0, 0.0, 0.0, 1.0
      )
    ).toOption.get
