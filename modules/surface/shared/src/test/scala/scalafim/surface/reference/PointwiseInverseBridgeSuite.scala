package scalafim.surface.reference

import scalafim.image.*
import scalafim.image.SampleSpaces.{indexToGrid3D, spatialDims}
import scalafim.image.world.WorldSpace
import scalafim.surface.*
import PointMapFixtures.*

/** Pointwise-inverse bridge contracts. Expectations come from construction: a
  * constant displacement then translation has the closed-form inverse
  * `x = y - t - d`; a smooth field is checked against a test-local fixed-point
  * solve of the declared map and by round trip through it.
  */
class PointwiseInverseBridgeSuite extends munit.FunSuite:
  private val a = TemplateId.unsafe("MNI152NLin2009cAsym")
  private val b = TemplateId.unsafe("MNI152NLin6Asym")
  private val release = TemplateRelease.unsafe("templateflow-24.2.0")
  private val frameA = TemplateFrame.unsafe("MNI152NLin2009cAsym", "templateflow-24.2.0")
  private val frameB = TemplateFrame.unsafe("MNI152NLin6Asym", "templateflow-24.2.0")

  private val fieldDims = Vector(16, 16, 16)
  private val fieldGrid = translation(-8.0, -8.0, -8.0)
  private val d = Vector(0.75, -0.5, 0.25)
  private val t = Vector(0.4, -0.3, 0.2)

  private def declared(stages: Vector[PointMapStage]): DeclaredPointMap =
    DeclaredPointMap.unsafeAssumeVerified(a, b, Some(release), PointMap.make(stages).toOption.get)

  private val constantMap = declared(Vector(
    PointMapStage.DisplacementStage(field(fieldDims, fieldGrid)(_ => d)),
    PointMapStage.AffineStage(translation(t(0), t(1), t(2)))))

  private def smooth(p: Vector[Double]): Vector[Double] =
    Vector(0.6 * math.sin(p(1) / 5.0), 0.5 * math.cos(p(2) / 6.0) - 0.5, 0.4 * math.sin(p(0) / 4.0))

  private val rotation =
    val (c, s) = (math.cos(0.05), math.sin(0.05))
    affine(c, -s, 0, 0.3, s, c, 0, -0.2, 0, 0, 1, 0.1, 0, 0, 0, 1)

  private val smoothMap = declared(Vector(
    PointMapStage.DisplacementStage(field(fieldDims, fieldGrid)(smooth)),
    PointMapStage.AffineStage(rotation)))

  private def inverter(map: DeclaredPointMap, policy: InversePolicy = InversePolicy.Default): PointMapInverter =
    map.map.inverter(policy).fold(e => fail(e.message), identity)

  private def forward(map: DeclaredPointMap, p: WorldPoint): WorldPoint =
    map.map.forward(p).placed.getOrElse(fail(s"forward map did not place $p"))

  private def distance(p: WorldPoint, q: WorldPoint): Double =
    math.sqrt(math.pow(p.x - q.x, 2) + math.pow(p.y - q.y, 2) + math.pow(p.z - q.z, 2))

  /** Test-local oracle: fixed-point solve of the declared map to machine precision. */
  private def solve(map: DeclaredPointMap, y: WorldPoint): WorldPoint =
    var x = y
    for _ <- 0 until 200 do
      val f = forward(map, x)
      x = WorldPoint(x.x - (f.x - y.x), x.y - (f.y - y.y), x.z - (f.z - y.z))
    assert(distance(forward(map, x), y) < 1e-12, s"oracle did not converge at $y")
    x

  private val probes = Vector(
    point(0.0, 0.0, 0.0), point(1.3, -2.7, 0.6), point(-3.1, 2.2, -1.4),
    point(2.85, 3.15, -3.05), point(-0.45, -3.6, 3.3), point(3.6, -0.15, 2.45))

  private def inverted(outcome: PointMapOutcome): PointMapOutcome.Inverted = outcome match
    case i: PointMapOutcome.Inverted => i
    case other => fail(s"expected Inverted, got $other")

  test("a constant displacement then translation inverts to x = y - t - d"):
    val solver = inverter(constantMap)
    for y <- probes do
      val x = inverted(solver.place(y))
      assertEqualsDouble(x.point.x, y.x - t(0) - d(0), 1e-8)
      assertEqualsDouble(x.point.y, y.y - t(1) - d(1), 1e-8)
      assertEqualsDouble(x.point.z, y.z - t(2) - d(2), 1e-8)
      assert(x.residualMm <= InversePolicy.Default.toleranceMm)

  test("a smooth field: placements match an independent solve and round-trip through the declared map"):
    val solver = inverter(smoothMap)
    for y <- probes do
      val x = inverted(solver.place(y))
      assert(distance(x.point, solve(smoothMap, y)) <= 1e-7, s"placement error at $y")
      assert(distance(forward(smoothMap, x.point), y) <= InversePolicy.Default.toleranceMm, s"round trip at $y")
      assert(x.iterations >= 1 && x.iterations <= InversePolicy.Default.maxIterations)

  test("support exits, iteration limits and divergence are typed and never placed"):
    assertEquals(inverter(constantMap).place(point(40.0, 0.0, 0.0)).asInstanceOf[PointMapOutcome.InverseUnsolved].failure,
      InverseFailure.LeftSupport)
    val limited = inverter(smoothMap, InversePolicy.make(1e-14, 1).toOption.get).place(point(1.3, -2.7, 0.6))
    limited match
      case PointMapOutcome.InverseUnsolved(InverseFailure.MaxIterations, iterations, Some(best)) =>
        assertEquals(iterations, 1)
        assert(best > 1e-14)
      case other => fail(s"expected an iteration-limited query, got $other")
    assertEquals(limited.placed, None)
    // An expanding displacement (d = 1.5 x) is not a contraction: fixed-point iteration diverges.
    val expanding = declared(Vector(PointMapStage.DisplacementStage(field(fieldDims, fieldGrid)(p => p.map(_ * 1.5)))))
    val diverged = inverter(expanding).place(point(0.5, -0.4, 0.3))
    assert(diverged.placed.isEmpty)
    diverged match
      case PointMapOutcome.InverseUnsolved(InverseFailure.Diverged, _, _) => ()
      case other => fail(s"expected divergence, got $other")

  // A route from fsLR-like anatomy in B to a ramp volume in A.
  private val testMesh = StandardCorticalMesh.declare(CorticalMeshFamily.FsLR, "test6", 6).toOption.get
  private val space = SampleSpaces.inWorld(SampleSpaces(Vector(8, 8, 8), affine = Some(translation(-4.0, -4.0, -4.0))),
    WorldSpace.template("MNI152NLin2009cAsym").toOption.get).toOption.get
  private val code: (Int, Int, Int) => Double = (i, j, k) => 1.0 + i + 10.0 * j + 100.0 * k
  private val ramp = SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.tabulate(space.spatialDims.product) { ordinal =>
    val g = space.indexToGrid3D(ordinal)
    code(g(0), g(1), g(2))
  }, space, "pointwise-route-fixture")

  // Continuous voxel indices in A, away from half-voxel boundaries. v4 lies
  // outside the displacement support; v5 is on the medial wall.
  private val indicesA = Vector(
    Vector(1.2, 2.3, 4.1), Vector(5.8, 3.1, 2.2), Vector(3.0, 6.2, 1.1),
    Vector(2.4, 1.3, 5.7), Vector(3.0, 3.0, 3.0), Vector(4.1, 4.2, 3.9))
  private val worldA = indicesA.map(i => Vector(i(0) - 4.0, i(1) - 4.0, i(2) - 4.0))
  private val worldB = worldA.zipWithIndex.map: (p, v) =>
    if v == 4 then Vector(30.0, 0.0, 0.0) else Vector.tabulate(3)(c => p(c) + d(c) + t(c))
  private val faces = Vector((0, 1, 2), (2, 3, 4), (3, 4, 5))

  private def anatomy(points: Vector[Vector[Double]], frame: TemplateFrame): SamplingAnatomy =
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(points, faces), Hemisphere.Left, SurfaceKind.Midthickness)
    val surface = DeclaredSurface.unsafeAssumeVerified(FrameDeclaration.make(frame,
      FrameBasis.literature("10.1093/cercor/bhr291", "synthetic fixture declared in this frame").toOption.get,
      AssetProvenance.make(TemplateId.unsafe("fsLR"), "tpl-fsLR/midthickness.surf.gii", "test", "0" * 64).toOption.get)
      .toOption.get, geometry)
    val wall = MedialWallMask.fromCortexFlags(surface.geometry.meshDomainEither.toOption.get,
      Vector(true, true, true, true, true, false)).toOption.get
    SamplingAnatomy.make(CorticalMeshReference.make(testMesh, surface.geometry, wall).toOption.get,
      AnatomicalGeometry.Midthickness(surface)).toOption.get

  private def declaredVolume: DeclaredVolume =
    val bundle = DataAsset.make("synthetic-bundle", "1" * 64).toOption.get
    DeclaredVolume.unsafeAssumeVerified(FrameDeclaration.make(frameA,
      FrameBasis.derived("synthetic fixture", Vector(bundle)).toOption.get,
      DataAsset.make("synthetic-volume.nii.gz", "2" * 64).toOption.get).toOption.get, ramp)

  test("a pointwise-inverse route reproduces the same-frame mapping, with per-vertex receipts and disclosure"):
    val bridge = FrameBridge.displacement(constantMap, PointMapUse.Inverse(InversePolicy.Default)).fold(e => fail(e.message), identity)
    val request = RouteRequest(VolumeReference.make(frameA, space).toOption.get, testMesh, CorticalHemisphere.Left,
      MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
    val bridged = SurfaceRoute.admit(request, anatomy(worldB, frameB), Some(bridge)).fold(r => fail(r.message), identity)
    val direct = SurfaceRoute.admit(request, anatomy(worldA, frameA)).fold(r => fail(r.message), identity)
    val viaBridge = bridged.map(declaredVolume).fold(e => fail(e.message), identity)
    val sameFrame = direct.map(declaredVolume).fold(e => fail(e.message), identity)
    for v <- Vector(0, 1, 2, 3) do
      val i = indicesA(v).map(x => math.floor(x + 0.5).toInt)
      assertEquals(viaBridge.valueAt(VertexId(v)), Some(code(i(0), i(1), i(2))))
      assertEquals(viaBridge.valueAt(VertexId(v)), sameFrame.valueAt(VertexId(v)))
      assertEquals(viaBridge.coverageAt(VertexId(v)), Some(VertexCoverage.Mapped))
    assertEquals(viaBridge.coverageAt(VertexId(4)), Some(VertexCoverage.BridgeUnavailable))
    assertEquals(viaBridge.valueAt(VertexId(4)), None)
    assertEquals(viaBridge.coverageAt(VertexId(5)), Some(VertexCoverage.MedialWall))
    val placement = bridged.bridgePlacement.getOrElse(fail("no bridge placement"))
    assert(placement.outcomesAt(4).head.isInstanceOf[PointMapOutcome.InverseUnsolved])
    assert(placement.outcomesAt(0).head.isInstanceOf[PointMapOutcome.Inverted])
    val summary = placement.inverseSummary.getOrElse(fail("no inverse summary"))
    assertEquals((summary.converged, summary.unplaced), (5, Map(InverseFailureKind.LeftSupport -> 1)))
    assert(summary.worstResidualMm.exists(_ <= InversePolicy.Default.toleranceMm))
    assertEquals(direct.bridgePlacement.flatMap(_.inverseSummary), None)
    assertEquals(bridged.disclosure.bridgeExactness,
      BridgeExactness.Approximate(NumericalBridgeMethod.PointwiseFixedPoint(InversePolicy.Default)))
    assertEquals(direct.disclosure.bridgeExactness, BridgeExactness.Exact)
    val evidence = bridged.inspect(declaredVolume, VertexId(1)).fold(e => fail(e.message), identity)
    assertEqualsDouble(evidence.samples.head.world.x, worldA(1)(0), 1e-8)
    assertEqualsDouble(evidence.samples.head.world.y, worldA(1)(1), 1e-8)
    assertEqualsDouble(evidence.samples.head.world.z, worldA(1)(2), 1e-8)
