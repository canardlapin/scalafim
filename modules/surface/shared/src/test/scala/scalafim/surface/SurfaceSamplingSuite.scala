package scalafim.surface

import scalafim.image.DMat
import scalafim.image.NeuroSpace
import scalafim.image.NeuroVol
import scalafim.image.NArrayUtil
import scalafim.image.SpatialDomainId

class SurfaceSamplingSuite extends munit.FunSuite:

  private val space = NeuroSpace(Vector(3, 3, 3))
  private val volume =
    NeuroVol.fromLinear(
      NArrayUtil.tabulate[Double](27) { idx =>
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

  test("surfaceToWorld transforms are applied before NeuroSpace coordinate lookup"):
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
      NeuroVol.fromLinear(
        NArrayUtil.fillConst[Boolean](27, false),
        space,
        "empty-mask"
      )
    val result = VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(volume, Some(mask))

    assertEquals(result.sampleCounts.valueAt(VertexId(0)), Some(0))
    assert(result.values.valueAt(VertexId(0)).exists(_.isNaN))

  test("volume-to-surface morphism wraps reusable sampling plans"):
    val morphism =
      VolToSurfMorphism(
        SpatialDomainId("volume"),
        SpatialDomainId("surface"),
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
      SurfToSurfMorphism(SpatialDomainId("source-surface"), SpatialDomainId("target-surface"), mapping)
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
      NeuroVol.fromLinear(
        NArrayUtil.fillConst[Boolean](8, true),
        NeuroSpace(Vector(2, 2, 2)),
        "bad-mask"
      )
    interceptMessage[IllegalArgumentException]("requirement failed: mask/volume space mismatch"):
      VolumeSurfaceSampler(VolumeSurfaceSamplingPlan(pair)).sample(volume, Some(badMask))

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

  private def translation(x: Double, y: Double, z: Double): DMat =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, x),
        Vector(0.0, 1.0, 0.0, y),
        Vector(0.0, 0.0, 1.0, z),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )
