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

  test("asymmetric orthographic decoration projection and picking include frustum translation"):
    val shifted = orthographic.clone()
    shifted(3) = -.5f
    shifted(7) = .25f
    val plan = planFor(camera(shifted))
    val centre = SurfaceDecorationWorldPoint(0, 0, 0)
    val marker = pixelPacket(plan, centre)
    val geometry = ok(SurfaceDecorationGeometry.lower(
      SurfaceDecorationPackets(Vector(marker), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 400, 200))
    // Independent NDC: (-.5,+.25) -> pixel (100,75), not canvas centre.
    val projected = SurfaceDecorationPicking.project(plan, plan.views.head.id, centre, 400, 200).get
    assertEqualsDouble(projected.x, 100.0, 1e-6)
    assertEqualsDouble(projected.y, 75.0, 1e-6)
    assertEquals(SurfaceDecorationPicking.pick(geometry, plan, 100, 75, None).map(_.id), Some(marker.id))
    assertEquals(SurfaceDecorationPicking.pick(geometry, plan, 200, 100, None), None)

  test("overlay picking follows declared mesh order regardless of world depth"):
    val plan = planFor(camera(orthographic))
    val near = pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 1)).copy(
      depthPolicy = SurfaceDecorationDepthPolicy.Overlay)
    val far = near.copy(id = SurfaceDecorationElementId(ok(SurfaceDecorationId.make("far-overlay")), "glyph"),
      point = SurfaceDecorationWorldPoint(0, 0, -1))
    val geometry = ok(SurfaceDecorationGeometry.lower(
      SurfaceDecorationPackets(Vector(near, far), Vector.empty, Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 400, 200))
    assertEquals(SurfaceDecorationPicking.pick(geometry, plan, 200, 100, Some(0.0)).map(_.id), Some(far.id))
    assertEquals(SurfaceDecorationPicking.pick(geometry.copy(meshes = geometry.meshes.reverse), plan,
      200, 100, Some(0.0)).map(_.id), Some(near.id))
    val depthTested = geometry.copy(meshes = geometry.meshes.map(_.copy(depthPolicy = SurfaceDecorationDepthPolicy.DepthTest)))
    assertEquals(SurfaceDecorationPicking.pick(depthTested, plan, 200, 100, None).map(_.id), Some(near.id))
    assertEquals(SurfaceDecorationPicking.pick(depthTested, plan, 200, 100, Some(0.0)), None)

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

  test("rounded contour caps preserve pixel widths through camera and output-size changes"):
    for projection <- Vector(orthographic, perspective); (width, height) <- Vector((400, 200), (800, 600)) do
      val plan = planFor(camera(projection))
      val style = SurfaceContourStyle(ok(SurfaceDecorationSize.pixels(8)), black,
        Some(SurfaceContourHalo(Rgba32.unsafe(255, 255, 255, 153), ok(SurfaceDecorationSize.pixels(4)))),
        mode = SurfaceContourMode.RoundedStrokes)
      val contour = SurfaceContourDecoration(ok(SurfaceDecorationId.make("rounded")), surface,
        Vector(1, 2, 2, 2), SurfaceContourPolicy.Transitions, style)
      val source = ok(SurfaceDecorationCompiler.compile(Vector(contour), plan))
      // Short diagonal edges discriminate round end caps from butt-ended bands.
      val first = source.contours.head.copy(start = SurfaceDecorationWorldPoint(-.03, -.02, .2),
        end = SurfaceDecorationWorldPoint(.03, .02, -.2))
      val second = first.copy(id = SurfaceDecorationElementId(first.id.decoration, "second"))
      val packets = source.copy(contours = Vector(first, second))
      val geometry = ok(SurfaceDecorationGeometry.lower(packets, plan, width, height))
      assert(SurfaceDecorationValidation.validate(geometry, plan, width, height).isRight)
      val firstMesh = geometry.meshes.head
      val replaced = firstMesh.copy(positions = new FloatBufferView(Array.tabulate(firstMesh.positions.length)(firstMesh.positions.apply)))
      assert(SurfaceDecorationValidation.validate(geometry.copy(meshes = Vector(replaced)), plan, width, height).isLeft)
      assert(SurfaceDecorationValidation.validate(geometry.copy(meshes = Vector(firstMesh.copy(roundedStroke = None))), plan, width, height).isLeft)
      if projection(15) == 0 then
        val behind = first.copy(start = SurfaceDecorationWorldPoint(0, 0, 6))
        assertEquals(SurfaceDecorationGeometry.lower(packets.copy(contours = Vector(behind)), plan, width, height),
          Left(SurfaceDecorationError.InvalidProjection))
      assertEquals(geometry.meshes.map(_.id.suffix), Vector(first.id.suffix, "second", s"${first.id.suffix}-halo", "second-halo"))
      assert(geometry.meshes.forall(_.depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest))
      assertEquals(geometry.meshes.head.target, SurfaceDecorationTarget.Edge(surface, first.sourceEdge._1, first.sourceEdge._2))
      for (mesh, strokeWidth) <- Vector((geometry.meshes.head, 8.0), (geometry.meshes(2), 12.0)) do
        val start = project(plan.views.head.camera, first.start, width, height)
        val end = project(plan.views.head.camera, first.end, width, height)
        assertEqualsDouble(distance(project(plan.views.head.camera, point(mesh, 0), width, height),
          project(plan.views.head.camera, point(mesh, 2), width, height)), strokeWidth, 1e-5)
        for i <- 5 to 37 do
          assertEqualsDouble(distance(project(plan.views.head.camera, point(mesh, i), width, height), start), strokeWidth / 2, 1e-5)
        for i <- 39 to 71 do
          assertEqualsDouble(distance(project(plan.views.head.camera, point(mesh, i), width, height), end), strokeWidth / 2, 1e-5)
      assert(SurfaceDecorationValidation.validate(geometry, plan, width + 1, height).isLeft)

  test("rounded contours reject unsupported world widths and nonopaque cores at construction"):
    intercept[IllegalArgumentException]:
      SurfaceContourStyle(ok(SurfaceDecorationSize.world(1)), black, mode = SurfaceContourMode.RoundedStrokes)
    intercept[IllegalArgumentException]:
      SurfaceContourStyle(ok(SurfaceDecorationSize.pixels(1)), Rgba32.unsafe(0, 0, 0, 128), mode = SurfaceContourMode.RoundedStrokes)
    intercept[IllegalArgumentException]:
      SurfaceContourStyle(ok(SurfaceDecorationSize.pixels(1)), black,
        depthPolicy = SurfaceDecorationDepthPolicy.Overlay, mode = SurfaceContourMode.RoundedStrokes)

  test("ordered depth testing refuses arbitrary translucent spheres and copied capsule geometry"):
    val plan = planFor(camera(orthographic))
    val sphere = pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 0)).copy(
      shape = SurfaceMarkerShape.Sphere, size = ok(SurfaceDecorationSize.world(.2)),
      color = Rgba32.unsafe(255, 0, 0, 153), depthPolicy = SurfaceDecorationDepthPolicy.OrderedDepthTest)
    val geometry = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(sphere), Vector.empty,
      Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 400, 200))
    assert(SurfaceDecorationValidation.validate(geometry, plan, 400, 200).isLeft)

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

  test("sphere marker retains radius, outward winding and nondegenerate pole faces"):
    val plan = planFor(camera(orthographic))
    val marker = pixelPacket(plan, SurfaceDecorationWorldPoint(0, 0, 0))
      .copy(shape = SurfaceMarkerShape.Sphere, size = ok(SurfaceDecorationSize.world(2)))
    val mesh = ok(SurfaceDecorationGeometry.lower(SurfaceDecorationPackets(Vector(marker), Vector.empty,
      Vector.empty, SurfaceDecorationBinding.key(plan, 1, 1)), plan, 200, 200)).meshes.head
    assertEquals(mesh.id, marker.id)
    for vertex <- 0 until mesh.positions.length / 3 do
      val p = point(mesh, vertex)
      assertEqualsDouble(math.sqrt(p.x * p.x + p.y * p.y + p.z * p.z), 2, 1e-6)
    for axis <- 0 until 3 do
      val values = (0 until mesh.positions.length / 3).map(v => mesh.positions(v * 3 + axis).toDouble)
      assertEqualsDouble(values.min, -2, 1e-6)
      assertEqualsDouble(values.max, 2, 1e-6)
    for face <- 0 until mesh.indices.length / 3 do
      val a = point(mesh, mesh.indices(face * 3))
      val b = point(mesh, mesh.indices(face * 3 + 1))
      val c = point(mesh, mesh.indices(face * 3 + 2))
      val ux = b.x - a.x
      val uy = b.y - a.y
      val uz = b.z - a.z
      val vx = c.x - a.x
      val vy = c.y - a.y
      val vz = c.z - a.z
      val nx = uy * vz - uz * vy
      val ny = uz * vx - ux * vz
      val nz = ux * vy - uy * vx
      assert(nx * nx + ny * ny + nz * nz > 1e-12, s"degenerate sphere face $face")
      assert(nx * a.x + ny * a.y + nz * a.z > 0, s"inward sphere face $face")

  test("packets compiled before placement changes cannot be rebound to the new plan"):
    val plan = planFor(camera(orthographic))
    val marker = SurfaceMarkerDecoration(markerId, anchor, SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(10)), black)
    val packets = ok(SurfaceDecorationCompiler.compile(Vector(marker), plan))
    val view = plan.views.head
    val moved = ok(SurfaceCompositionPlan.make(Vector(view.copy(instances = view.instances.map(_.copy(translationX = .2)))), plan.resources))
    assertEquals(SurfaceDecorationGeometry.lower(packets, moved, 200, 200), Left(SurfaceDecorationError.StalePackets))
