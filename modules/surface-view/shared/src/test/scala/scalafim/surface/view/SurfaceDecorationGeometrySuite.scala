package scalafim.surface.view

import intaglio.Rgba32
import scalafim.surface.{FaceId, VertexId}

class SurfaceDecorationGeometrySuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(e => fail(String.valueOf(e)), identity)
  private val surface = SurfaceFaceFixture.Surface
  private val black = Rgba32.unsafe(0, 0, 0)
  private val markerId = ok(SurfaceDecorationId.make("geometry-marker"))
  private val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(0)))

  private def camera(projection: Array[Float]): SurfaceCameraPacket =
    SurfaceCameraPacket(new FloatBufferView(Array(
      1f, 0f, 0f, 0f,
      0f, 1f, 0f, 0f,
      0f, 0f, 1f, -5f,
      0f, 0f, 0f, 1f
    )), new FloatBufferView(projection), 0.0, 0.0, 1.0)

  private val orthographic = Array(
    1f, 0f, 0f, 0f,
    0f, 1f, 0f, 0f,
    0f, 0f, -.002f, -1f,
    0f, 0f, 0f, 1f
  )

  private val perspective = Array(
    1f, 0f, 0f, 0f,
    0f, 1f, 0f, 0f,
    0f, 0f, -1.1f, -.2f,
    0f, 0f, -1f, 0f
  )

  private val rolledView = Array(
    0f, 1f, 0f, 0f,
    -1f, 0f, 0f, 0f,
    0f, 0f, 1f, -5f,
    0f, 0f, 0f, 1f
  )

  private def planFor(cameraPacket: SurfaceCameraPacket, fit: SurfaceViewportFit = SurfaceViewportFit.Fill): SurfaceCompositionPlan =
    val base = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    ok(SurfaceCompositionPlan.make(Vector(base.views.head.copy(camera = cameraPacket, viewportFit = fit)), base.resources))

  private def pixelPacket(plan: SurfaceCompositionPlan, point: SurfaceDecorationWorldPoint): SurfaceMarkerPacket =
    val marker = SurfaceMarkerDecoration(markerId, anchor, SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(10)), black)
    ok(SurfaceDecorationCompiler.compile(Vector(marker), plan)).markers.head.copy(point = point)

  private def project(camera: SurfaceCameraPacket, point: SurfaceDecorationWorldPoint,
      width: Int, height: Int): (Double, Double) =
    val view = camera.viewMatrix
    val projection = camera.projectionMatrix
    val ex = view(0) * point.x + view(1) * point.y + view(2) * point.z + view(3)
    val ey = view(4) * point.x + view(5) * point.y + view(6) * point.z + view(7)
    val ez = view(8) * point.x + view(9) * point.y + view(10) * point.z + view(11)
    val ew = view(12) * point.x + view(13) * point.y + view(14) * point.z + view(15)
    val cx = projection(0) * ex + projection(1) * ey + projection(2) * ez + projection(3) * ew
    val cy = projection(4) * ex + projection(5) * ey + projection(6) * ez + projection(7) * ew
    val cw = projection(12) * ex + projection(13) * ey + projection(14) * ez + projection(15) * ew
    ((cx / cw + 1.0) * width / 2.0, (cy / cw + 1.0) * height / 2.0)

  private def point(mesh: SurfaceDecorationMesh, vertex: Int): SurfaceDecorationWorldPoint =
    SurfaceDecorationWorldPoint(mesh.positions(vertex * 3), mesh.positions(vertex * 3 + 1), mesh.positions(vertex * 3 + 2))

  private def distance(left: (Double, Double), right: (Double, Double)): Double =
    math.hypot(left._1 - right._1, left._2 - right._2)

  test("world marker radius, stable origin, and scientific mesh buffers remain separate"):
    val plan = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val marker = SurfaceMarkerDecoration(markerId, anchor, SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.world(2)), black)
    val packets = ok(SurfaceDecorationCompiler.compile(Vector(marker), plan))
    val geometry = ok(SurfaceDecorationGeometry.lower(packets, plan, 256, 256))
    val mesh = geometry.meshes.head
    assertEquals(mesh.id, packets.markers.head.id)
    assertEquals(mesh.origin, packets.markers.head.origin)
    assertEquals(mesh.target, SurfaceDecorationTarget.Anchor(anchor))
    assertEqualsDouble(mesh.positions(3) - packets.markers.head.point.x, 2.0, 1e-6)
    assertEquals(geometry.canvasWidth, 256)
    assertEquals(plan.resources.meshes.values.head.positions.length, SurfaceFaceFixture.plan.meshes.head.positions.length)

  test("contour segments retain typed source edges and halos are disjoint meshes"):
    val plan = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val width = ok(SurfaceDecorationSize.world(.1))
    val contour = SurfaceContourDecoration(ok(SurfaceDecorationId.make("contour")), surface, Vector(1, 2, 2, 2), SurfaceContourPolicy.Transitions,
      SurfaceContourStyle(width, black, Some(SurfaceContourHalo(black, width))))
    val packets = ok(SurfaceDecorationCompiler.compile(Vector(contour), plan))
    assert(packets.contours.forall(packet => packet.sourceEdge._1.index <= packet.sourceEdge._2.index))
    val geometry = ok(SurfaceDecorationGeometry.lower(packets, plan, 256, 256))
    assertEquals(geometry.meshes.length, packets.contours.length * 3)
    assert(geometry.meshes.exists(_.id.suffix.endsWith("halo-left")))

  test("pixel marker axes follow rectangular Fill and square Contain viewports"):
    val fill = planFor(camera(orthographic))
    val fillPacket = pixelPacket(fill, SurfaceDecorationWorldPoint(0, 0, 0))
    val fillMesh = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(fillPacket), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(fill, 1, 1)), fill, 400, 200)).meshes.head
    assertEqualsDouble(fillMesh.positions(3), .05, 1e-6)
    assertEqualsDouble(fillMesh.positions(16), .1, 1e-6)

    val contain = planFor(camera(orthographic), SurfaceViewportFit.Contain(1.0))
    val containPacket = pixelPacket(contain, SurfaceDecorationWorldPoint(0, 0, 0))
    val containMesh = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(containPacket), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(contain, 1, 1)), contain, 400, 200)).meshes.head
    assertEqualsDouble(containMesh.positions(3), .1, 1e-6)
    assertEqualsDouble(containMesh.positions(16), .1, 1e-6)

  test("perspective pixel radius changes with depth while world radius does not"):
    val plan = planFor(camera(perspective))
    val near = pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 0))
    val far = near.copy(point = SurfaceDecorationWorldPoint(0, 0, -5))
    val pixelGeometry = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(near, far), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 200, 200))
    assertEqualsDouble(pixelGeometry.meshes(1).positions(3) / pixelGeometry.meshes.head.positions(3), 2.0, 1e-6)

    val world = near.copy(size = ok(SurfaceDecorationSize.world(.25)))
    val worldFar = world.copy(point = far.point)
    val worldGeometry = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(world, worldFar), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 200, 200))
    assertEqualsDouble(worldGeometry.meshes.head.positions(3), worldGeometry.meshes(1).positions(3), 1e-6)

  test("pixel axes rotate with camera roll"):
    val rolled = camera(orthographic).copy(viewMatrix = new FloatBufferView(rolledView))
    val plan = planFor(rolled)
    val mesh = ok(SurfaceDecorationGeometry.lower(
      SurfaceDecorationPackets(Vector(pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 0))), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)),
      plan, 400, 200)).meshes.head
    assertEqualsDouble(mesh.positions(3), 0.0, 1e-6)
    assertEqualsDouble(mesh.positions(4), .05, 1e-6)
    assertEqualsDouble(mesh.positions(15), -.1, 1e-6)
    assertEqualsDouble(mesh.positions(16), 0.0, 1e-6)

  test("pixel contour is full projected width and halo bands are outside its core"):
    val plan = planFor(camera(perspective))
    val marker = pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 0))
    val contour = SurfaceContourPacket(SurfaceDecorationElementId(markerId, "projected-edge"), marker.origin, surface,
      VertexId(0) -> VertexId(1), SurfaceDecorationWorldPoint(0, 0, 0), SurfaceDecorationWorldPoint(1, 0, -5),
      SurfaceContourStyle(ok(SurfaceDecorationSize.pixels(10)), black,
        Some(SurfaceContourHalo(black, ok(SurfaceDecorationSize.pixels(4))))))
    val meshes = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector.empty, Vector.empty, Vector(contour), SurfaceDecorationBinding.key(plan, 1, 1)), plan, 200, 200)).meshes
    val leftHalo = meshes(0)
    val core = meshes(2)
    assertEqualsDouble(distance(project(plan.views.head.camera, point(core, 0), 200, 200),
      project(plan.views.head.camera, point(core, 2), 200, 200)), 10.0, 1e-4)
    assertEqualsDouble(distance(project(plan.views.head.camera, point(leftHalo, 0), 200, 200),
      project(plan.views.head.camera, point(leftHalo, 2), 200, 200)), 2.0, 1e-4)
    val projectedStart = project(plan.views.head.camera, contour.start, 200, 200)
    assert(distance(projectedStart, project(plan.views.head.camera, point(leftHalo, 2), 200, 200)) >
      distance(projectedStart, project(plan.views.head.camera, point(core, 0), 200, 200)))

  test("degenerate contour remains finite and anisotropic pixel spheres refuse lowering"):
    val plan = planFor(camera(orthographic))
    val marker = pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 0))
    val contour = SurfaceContourPacket(SurfaceDecorationElementId(markerId, "degenerate-edge"), marker.origin, surface,
      VertexId(0) -> VertexId(0), marker.point, marker.point,
      SurfaceContourStyle(ok(SurfaceDecorationSize.world(.1)), black))
    val degenerate = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector.empty, Vector.empty, Vector(contour), SurfaceDecorationBinding.key(plan, 1, 1)), plan, 400, 200))
    assert(degenerate.meshes.head.positions.length > 0)
    assert((0 until degenerate.meshes.head.positions.length).forall(i => degenerate.meshes.head.positions(i).isFinite))

    val sphere = marker.copy(shape = SurfaceMarkerShape.Sphere)
    assertEquals(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(sphere), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 400, 200),
      Left(SurfaceDecorationError.AnisotropicPixelSphere))

  test("copied size and face anchors retain their construction invariants"):
    val size = ok(SurfaceDecorationSize.pixels(2))
    val face = ok(SurfaceFaceAnchor.make(surface, FaceId(0), .2, .3, .5))
    assert(SurfaceDecorationSize.pixels(Double.NaN).isLeft)
    assert(SurfaceDecorationSize.pixels(0.0).isLeft)
    assert(compileErrors("""scalafim.surface.view.SurfaceDecorationSize.pixels(1).toOption.get.copy(value = 0.0)""").nonEmpty)
    assert(compileErrors("""scalafim.surface.view.SurfaceFaceAnchor.make(scalafim.surface.view.SurfaceFaceFixture.Surface, scalafim.surface.FaceId(0), 1, 0, 0).toOption.get.copy(a = .6)""").nonEmpty)

  test("packets compiled before placement changes cannot be rebound to the new plan"):
    val plan = planFor(camera(orthographic))
    val marker = SurfaceMarkerDecoration(markerId, anchor, SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(10)), black)
    val packets = ok(SurfaceDecorationCompiler.compile(Vector(marker), plan))
    val view = plan.views.head
    val moved = ok(SurfaceCompositionPlan.make(Vector(view.copy(instances = view.instances.map(_.copy(translationX = .2)))), plan.resources))
    assertEquals(SurfaceDecorationGeometry.lower(packets, moved, 200, 200), Left(SurfaceDecorationError.StalePackets))
