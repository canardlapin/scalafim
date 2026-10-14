package scalafim.surface.view

import intaglio.Rgba32
import scalafim.surface.VertexId

class SurfaceDecorationsSuite extends munit.FunSuite:
  private val surface = SurfaceFaceFixture.Surface
  private val black = Rgba32.unsafe(0, 0, 0)

  private def ok[E, A](value: Either[E, A]): A = value.fold(error => fail(s"unexpected failure: $error"), identity)
  private def id(value: String): SurfaceDecorationId = ok(SurfaceDecorationId.make(value))
  private def vertex(value: Int): SurfaceDecorationAnchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surface, VertexId(value)))
  private val size = ok(SurfaceDecorationSize.pixels(2.0))

  test("transition contours assign independent stable edge ids and ROI exterior omits internal parcel borders"):
    val composition = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val labels = Vector(1, 2, 2, 2)
    val transitions = SurfaceContourDecoration(id("transitions"), surface, labels,
      SurfaceContourPolicy.Transitions, SurfaceContourStyle(size, black))
    val outlined = ok(SurfaceDecorationCompiler.compile(Vector(transitions), composition))
    assertEquals(outlined.contours.map(_.id).distinct.length, outlined.contours.length)
    assertEquals(outlined.contours.map(_.id.suffix).toSet, Set("edge-0-1", "edge-0-2", "edge-0-3"))

    val exterior = transitions.copy(id = id("exterior"), policy = SurfaceContourPolicy.RoiExterior(Set(1, 2)))
    assertEquals(ok(SurfaceDecorationCompiler.compile(Vector(exterior), composition)).contours, Vector.empty)

  test("one scientific marker lowers for each repeated display instance with distinct origins"):
    val base = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val view = base.views.head
    val second = view.instances.head.copy(
      id = ok(SurfaceInstanceId.make("second")), translationX = 3.0
    )
    val composition = ok(SurfaceCompositionPlan.make(Vector(view.copy(instances = view.instances :+ second)), base.resources))
    val marker = SurfaceMarkerDecoration(id("marker"), vertex(0), SurfaceMarkerShape.Sphere, size, black)
    val packets = ok(SurfaceDecorationCompiler.compile(Vector(marker), composition))
    assertEquals(packets.markers.length, 2)
    assertEquals(packets.markers.map(_.origin.instance).distinct.length, 2)
    assertEqualsDouble(packets.markers(1).point.x - packets.markers(0).point.x, 3.0, 0.0)

  test("malformed face weights and unresolved scientific vertices return typed errors"):
    assertEquals(
      SurfaceFaceAnchor.make(surface, scalafim.surface.FaceId(0), Double.NaN, 0.0, 1.0),
      Left(SurfaceDecorationError.InvalidBarycentric)
    )
    val composition = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val absent = SurfaceMarkerDecoration(id("absent"), vertex(99), SurfaceMarkerShape.Disk, size, black)
    assertEquals(
      SurfaceDecorationCompiler.compile(Vector(absent), composition),
      Left(SurfaceDecorationError.VertexOutOfRange(surface, VertexId(99)))
    )

  test("a current geometry update moves anchors without changing their stable id"):
    val original = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val mesh = original.resources.meshes.values.head
    val moved = new Array[Float](mesh.positions.length)
    var index = 0
    while index < moved.length do
      moved(index) = mesh.positions(index)
      index += 1
    moved(0) = moved(0) + 5.0f
    val updatedMesh = mesh.copy(positions = new FloatBufferView(moved))
    val updated = ok(SurfaceCompositionPlan.make(original.views,
      original.resources.copy(meshes = original.resources.meshes.updated(mesh.resourceKey, updatedMesh))))
    val marker = SurfaceMarkerDecoration(id("moving"), vertex(0), SurfaceMarkerShape.Crosshair, size, black)
    val before = ok(SurfaceDecorationCompiler.compile(Vector(marker), original)).markers.head
    val after = ok(SurfaceDecorationCompiler.compile(Vector(marker), updated)).markers.head
    assertEquals(before.id, after.id)
    assertEqualsDouble(after.point.x - before.point.x, 5.0, 1e-6)

  test("source-domain sample buffers retain isolated label slots and nearest partitions use exact source corners"):
    val original = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val mesh = original.resources.meshes.values.head
    val samples = new Array[Float](15)
    var index = 0
    while index < mesh.positions.length / 3 do
      val source = mesh.sourceVertex(index)
      for axis <- 0 until 3 do samples(source * 3 + axis) = mesh.positions(index * 3 + axis)
      index += 1
    val sampled = mesh.copy(
      samplePositions = Some(new FloatBufferView(samples)),
      sampleNormals = mesh.sampleNormals.map(v => new FloatBufferView(Array.tabulate(15)(i => if i < v.length then v(i) else 0f)))
    )
    val sampledPlan = ok(SurfaceCompositionPlan.make(original.views,
      original.resources.copy(meshes = original.resources.meshes.updated(mesh.resourceKey, sampled))))
    val contour = SurfaceContourDecoration(id("isolated"), surface, Vector(0, 1, 1, 1, 9),
      SurfaceContourPolicy.Transitions, SurfaceContourStyle(size, black))
    assert(ok(SurfaceDecorationCompiler.compile(Vector(contour), sampledPlan)).contours.nonEmpty)

    val nearestPlan = ok(SurfaceCompositionPlan.fromLegacy(SurfaceNearestFixture.plan))
    val marker = SurfaceMarkerDecoration(id("nearest"), vertex(0), SurfaceMarkerShape.Disk, size, black)
    val point = ok(SurfaceDecorationCompiler.compile(Vector(marker), nearestPlan)).markers.head.point
    assertEqualsDouble(point.x, mesh.positions(0), 1e-6)
    assertEqualsDouble(point.y, mesh.positions(1), 1e-6)
    assertEqualsDouble(point.z, mesh.positions(2), 1e-6)
