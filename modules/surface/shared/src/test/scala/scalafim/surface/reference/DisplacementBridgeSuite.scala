package scalafim.surface.reference

import scalafim.image.*
import scalafim.image.SampleSpaces.{indexToGrid3D}
import scalafim.surface.*
import PointMapFixtures.*

/** Point-map bridges: endpoints follow from the map's frames and declared use,
  * vertices are placed before the unchanged sampling kernel, and a vertex the
  * map cannot place is never sampled.
  */
class DisplacementBridgeSuite extends munit.FunSuite:
  private val a = TemplateId.unsafe("MNI152NLin2009cAsym")
  private val b = TemplateId.unsafe("MNI152NLin6Asym")
  private val release = TemplateRelease.unsafe("templateflow@synthetic")
  private val frameA = TemplateFrame.make(a, release).toOption.get
  private val frameB = TemplateFrame.make(b, release).toOption.get

  private val dims = Vector(6, 6, 6)
  private val grid = affine(2, 0, 0, -5, 0, 2, 0, -5, 0, 0, 2, -5, 0, 0, 0, 1)
  private val space = SampleSpaces(dims, affine = Some(grid))
  private val code: (Int, Int, Int) => Double = (i, j, k) => 1.0 + i + 10.0 * j + 100.0 * k
  private val ramp = SomeScalarVolume.unsafeCopyFromCanonicalArray(Array.tabulate(216) { o =>
    val g = space.indexToGrid3D(o)
    code(g(0), g(1), g(2))
  }, space, "ramp")

  // A -> B point map: y = x + c on the volume grid.
  private val c = Vector(1.25, -0.75, 0.5)
  private val pointMap = DeclaredPointMap.unsafeAssumeVerified(a, b, Some(release),
    PointMap.make(Vector(PointMapStage.DisplacementStage(field(dims, grid)(_ => c)))).toOption.get)
  private val inverse = PointMapUse.Inverse(InversePolicy.make(1e-10, 50).toOption.get)

  // Vertices at continuous voxel indices of the A grid; vertex 4 lies far outside the field.
  private val indices = Vector(Vector(1.2, 2.3, 3.1), Vector(4.4, 1.1, 0.8), Vector(2.6, 4.2, 2.2),
    Vector(0.9, 0.4, 4.6), Vector(40.0, 2.0, 2.0), Vector(3.3, 3.3, 1.4))
  private def worldA(index: Vector[Double]): Vector[Double] = grid(index).toOption.get
  private val faces = Vector((0, 1, 2), (2, 3, 4), (3, 4, 5))
  private def surface(points: Vector[Vector[Double]]) =
    SurfaceGeometry(TriangleMesh.fromRows(points, faces), Hemisphere.Left, SurfaceKind.Midthickness)
  private val inA = surface(indices.map(worldA))
  private val inB = surface(indices.map(i => Vector.tabulate(3)(d => worldA(i)(d) + c(d))))
  private val mesh = StandardCorticalMesh.declare(CorticalMeshFamily.FsLR, "bridge6", 6).toOption.get
  private val reference = CorticalMeshReference.make(mesh, inA, MedialWallMask.fromCortexFlags(
    inA.meshDomainEither.toOption.get, Vector.fill(6)(true)).toOption.get).toOption.get

  private def declaration(frame: TemplateFrame, name: String) = FrameDeclaration.make(frame,
    FrameBasis.literature("10.1093/cercor/bhr291", "synthetic fixture declared in this frame").toOption.get,
    DataAsset.make(name, "3" * 64).toOption.get).toOption.get
  private def anatomy(g: SurfaceGeometry, frame: TemplateFrame) = SamplingAnatomy.make(reference,
    AnatomicalGeometry.Midthickness(DeclaredSurface.unsafeAssumeVerified(declaration(frame, "mid.surf.gii"), g))).toOption.get
  private val source = VolumeReference.make(frameA, space).toOption.get
  private val request = RouteRequest(source, mesh, CorticalHemisphere.Left, MappingMethod.MidthicknessNearest, ValueSemantics.Continuous)
  private val declaredRamp = DeclaredVolume.unsafeAssumeVerified(declaration(frameA, "ramp.nii.gz"), ramp)

  test("endpoints follow the map's frames and the declared use; releases must agree"):
    val forward = FrameBridge.displacement(pointMap, PointMapUse.Forward).toOption.get
    assertEquals((forward.from, forward.to), (frameA, frameB))
    val inv = FrameBridge.displacement(pointMap, inverse).toOption.get
    assertEquals((inv.from, inv.to), (frameB, frameA))
    assert(inv.display.contains("inverse"))
    assert(FrameBridge.displacement(pointMap, inverse, Some(TemplateRelease.unsafe("other"))).isLeft)
    val unrevisioned = DeclaredPointMap.unsafeAssumeVerified(a, b, None, pointMap.map)
    assert(FrameBridge.displacement(unrevisioned, inverse).isLeft)
    assertEquals(FrameBridge.displacement(unrevisioned, inverse, Some(release)).map(_.to), Right(frameA))

  test("an inverse point-map bridge reproduces the same-frame mapping and never samples unplaced vertices"):
    val bridge = FrameBridge.displacement(pointMap, inverse).toOption.get
    val bridged = SurfaceRoute.admit(request, anatomy(inB, frameB), Some(bridge)).fold(r => fail(r.message), r => r)
    val direct = SurfaceRoute.admit(request, anatomy(inA, frameA)).fold(r => fail(r.message), r => r)
    val viaBridge = bridged.map(declaredRamp).fold(e => fail(e.message), m => m)
    val same = direct.map(declaredRamp).fold(e => fail(e.message), m => m)
    for v <- Vector(0, 1, 2, 3, 5) do
      assertEquals(viaBridge.valueAt(VertexId(v)), same.valueAt(VertexId(v)), s"vertex $v")
      assertEquals(viaBridge.coverageAt(VertexId(v)), Some(VertexCoverage.Mapped))
    assertEquals(viaBridge.coverageAt(VertexId(4)), Some(VertexCoverage.BridgeUnavailable))
    assertEquals(viaBridge.acceptedLookupsAt(VertexId(4)), Some(0))
    assertEquals(bridged.bridgePlacement.map(_.unavailableCount), Some(1))
    val evidence = bridged.inspect(declaredRamp, VertexId(4)).toOption.get
    assertEquals((evidence.coverage, evidence.samples, evidence.bridge), (VertexCoverage.BridgeUnavailable, Vector.empty,
      Vector(PointMapOutcome.OutsideSupport)))
    val placed = bridged.inspect(declaredRamp, VertexId(0)).toOption.get
    placed.bridge match
      case Vector(PointMapOutcome.Converged(point, residual, _)) =>
        assert(residual <= 1e-10)
        for (p, e) <- point.toVector.zip(worldA(indices(0))) do assertEqualsDouble(p, e, 1e-9)
        assertEqualsDouble(placed.samples.head.world.x, point.x, 0.0)
      case other => fail(s"expected one converged outcome; got $other")
    assertEquals(bridged.disclosure.bridge, Some(bridge))

  test("forward use of the same map is the reversed bridge for this route"):
    val forward = FrameBridge.displacement(pointMap, PointMapUse.Forward).toOption.get
    assertEquals(SurfaceRoute.admit(request, anatomy(inB, frameB), Some(forward)).left.toOption,
      Some(RouteRefusal.ReversedBridge((frameB, frameA))))

  test("a vertex whose inverse does not converge is unavailable, not sampled at its last iterate"):
    val lin = field(dims, grid)(p => Vector(0.05 * p(0), 0.05 * p(1), 0.05 * p(2)))
    val slow = DeclaredPointMap.unsafeAssumeVerified(a, b, Some(release),
      PointMap.make(Vector(PointMapStage.DisplacementStage(lin))).toOption.get)
    val bridge = FrameBridge.displacement(slow, PointMapUse.Inverse(InversePolicy.make(1e-12, 1).toOption.get)).toOption.get
    val route = SurfaceRoute.admit(request, anatomy(inA, frameB), Some(bridge)).fold(r => fail(r.message), r => r)
    val mapped = route.map(declaredRamp).fold(e => fail(e.message), m => m)
    assertEquals(mapped.count(VertexCoverage.BridgeUnavailable), 6)
    assert(route.inspect(declaredRamp, VertexId(0)).toOption.get.bridge.head.isInstanceOf[PointMapOutcome.NonConvergent])
