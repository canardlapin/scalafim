package scalafim.surface.view

import scalafim.surface.VertexId

class SurfaceNetworkDecorationsSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(String.valueOf(error)), identity)
  private val left = SurfaceFaceFixture.Surface
  private val right = SurfaceId.unsafe("network-right")

  private def composition(translation: Double = 5.0): (SurfaceCompositionPlan, SurfaceInstanceId, SurfaceInstanceId) =
    val base = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val view = base.views.head
    val leftInstance = view.instances.head
    val rightId = ok(SurfaceInstanceId.make("network-right-copy"))
    val rightKey = SurfaceResourceKey("network-right-mesh")
    val rightMesh = base.resources.meshes(leftInstance.mesh).copy(surface = right, resourceKey = rightKey)
    val rightInstance = SurfaceCompositionInstance(rightId, right, rightKey, Vector.empty, translationX = translation)
    val resources = base.resources.copy(meshes = base.resources.meshes + (rightKey -> rightMesh))
    (ok(SurfaceCompositionPlan.make(Vector(view.copy(instances = Vector(leftInstance, rightInstance))), resources)), leftInstance.id, rightId)

  private def display(weights: Vector[Double] = Vector(.75),
      style: SurfaceNetworkStyle = SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(.1), sides = 6).toOption.get): SurfaceNetworkDisplay =
    val leftNode = ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe("left-node"), left, VertexId(0), SurfaceFaceFixture.geometry))
    val rightNode = ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe("right-node"), right, VertexId(1), SurfaceFaceFixture.geometry))
    val edges = weights.map(weight => SurfaceNetworkEdge.unsafe(leftNode.id, rightNode.id, weight))
    val network = ok(SurfaceNetwork.make(Vector(leftNode, rightNode), edges))
    ok(SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All, style))

  private def distinctDisplay(style: SurfaceNetworkStyle): SurfaceNetworkDisplay =
    val source = ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe("left-node"), left, VertexId(0), SurfaceFaceFixture.geometry))
    val positive = ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe("right-positive"), right, VertexId(1), SurfaceFaceFixture.geometry))
    val negative = ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe("right-negative"), right, VertexId(2), SurfaceFaceFixture.geometry))
    val network = ok(SurfaceNetwork.make(Vector(source, positive, negative), Vector(
      SurfaceNetworkEdge.unsafe(source.id, positive.id, .25),
      SurfaceNetworkEdge.unsafe(source.id, negative.id, -.75)
    )))
    ok(SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All, style))

  private def lower(plan: SurfaceCompositionPlan, leftId: SurfaceInstanceId, rightId: SurfaceInstanceId,
      shown: SurfaceNetworkDisplay = display()): SurfaceDecorationGeometry =
    ok(SurfaceNetworkDecorations.lower(shown, plan,
      SurfaceNetworkDecorationMapping(plan.views.head.id, Map(left -> leftId, right -> rightId)),
      SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.world(.2)), 300, 200))

  test("cross-surface edges use mapped translated render vertices and keep scientific buffers unchanged"):
    val (plan, leftId, rightId) = composition()
    val before = plan.resources.meshes.values.map(mesh => mesh.resourceKey -> mesh.positions).toMap
    val geometry = lower(plan, leftId, rightId)
    val nodes = geometry.meshes.collect:
      case mesh @ SurfaceDecorationMesh(_, _, SurfaceDecorationTarget.Anchor(_), _, _, _, _, _, _, _) => mesh
    assertEquals(nodes.length, 2)
    val rightNode = nodes.find(_.origin.instance == rightId).get
    assertEqualsDouble(rightNode.positions(0), 6.0, 1e-6)
    val edge = geometry.meshes.collectFirst:
      case mesh @ SurfaceDecorationMesh(_, origin, SurfaceDecorationTarget.NetworkEdge(source, target), _, _, _, _, _, _, _) =>
        assertEquals(origin.instance, leftId)
        assertEquals(source.surface, left)
        assertEquals(target.surface, right)
        mesh
    assert(edge.nonEmpty)
    plan.resources.meshes.values.foreach(mesh => assertEquals(mesh.positions, before(mesh.resourceKey)))

  test("morph positions are refreshed while stable decoration ids survive"):
    val (before, leftId, rightId) = composition()
    val first = lower(before, leftId, rightId)
    val rightInstance = before.views.head.instances.find(_.id == rightId).get
    val source = before.resources.meshes(rightInstance.mesh)
    val positions = new Array[Float](source.positions.length)
    var i = 0
    while i < positions.length do
      positions(i) = source.positions(i) + (if i % 3 == 0 then 2f else 0f)
      i += 1
    val samples = source.samplePositions.map: old =>
      val moved = new Array[Float](old.length)
      i = 0
      while i < moved.length do
        moved(i) = old(i) + (if i % 3 == 0 then 2f else 0f)
        i += 1
      new FloatBufferView(moved)
    val changedMesh = source.copy(positions = new FloatBufferView(positions), samplePositions = samples)
    val after = ok(SurfaceCompositionPlan.make(before.views, before.resources.copy(meshes = before.resources.meshes.updated(source.resourceKey, changedMesh))))
    val second = lower(after, leftId, rightId)
    val oldRight = first.meshes.find(_.origin.instance == rightId).get
    val newRight = second.meshes.find(_.origin.instance == rightId).get
    assertEquals(oldRight.id, newRight.id)
    assertEqualsDouble(newRight.positions(0) - oldRight.positions(0), 2.0, 1e-6)

  test("signed edge colors, absolute-weight radius, and node glyphs are explicit"):
    val (plan, leftId, rightId) = composition()
    val shown = distinctDisplay(SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(.1), sides = 6).toOption.get)
    val scaled = ok(SurfaceNetworkDecorationRadius.byAbsoluteWeight(
      SurfaceNetworkRadius.unsafe(.4), SurfaceNetworkRadius.unsafe(.05), SurfaceNetworkRadius.unsafe(.5)))
    val geometry = ok(SurfaceNetworkDecorations.lower(shown, plan,
      SurfaceNetworkDecorationMapping(plan.views.head.id, Map(left -> leftId, right -> rightId)),
      SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.world(.2)), 300, 200, Some(scaled)))
    val edges = geometry.meshes.collect { case mesh @ SurfaceDecorationMesh(_, _, SurfaceDecorationTarget.NetworkEdge(_, _), _, _, _, _, _, _, _) => mesh }
    assertEquals(edges.length, 2)
    assertEquals(edges.map(_.id).distinct.length, 2)
    assert(edges.exists(_.color == SurfaceNetworkPalette().positive))
    assert(edges.exists(_.color == SurfaceNetworkPalette().negative))
    assertEquals(geometry.meshes.count(_.target.isInstanceOf[SurfaceDecorationTarget.Anchor]), 3)
    val nearRadius = math.abs(edges.find(_.color == SurfaceNetworkPalette().positive).get.positions(2))
    val farRadius = math.abs(edges.find(_.color == SurfaceNetworkPalette().negative).get.positions(2))
    assert(farRadius > nearRadius)

  test("duplicate ordered endpoints are refused until the network has stable edge ids"):
    val (plan, leftId, rightId) = composition()
    val duplicate = SurfaceNetworkDecorations.lower(display(Vector(.5, -.5)), plan,
      SurfaceNetworkDecorationMapping(plan.views.head.id, Map(left -> leftId, right -> rightId)),
      SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.world(.2)), 300, 200)
    assert(duplicate.left.exists(_.isInstanceOf[SurfaceDecorationError.NetworkMapping]))

  test("line style lowers to a four-vertex camera-facing ribbon"):
    val (plan, leftId, rightId) = composition()
    val geometry = lower(plan, leftId, rightId,
      display(style = SurfaceNetworkStyle.line(SurfaceNetworkRadius.unsafe(.1))))
    val ribbon = geometry.meshes.collectFirst:
      case mesh @ SurfaceDecorationMesh(_, _, SurfaceDecorationTarget.NetworkEdge(_, _), _, _, _, _, _, _, _) => mesh
    assertEquals(ribbon.get.positions.length, 12)
    assertEquals(ribbon.get.indices.length, 6)

  test("missing or ambiguous instance choices are refused rather than inferred"):
    val (plan, leftId, _) = composition()
    val missing = SurfaceNetworkDecorations.lower(display(), plan,
      SurfaceNetworkDecorationMapping(plan.views.head.id, Map(left -> leftId)),
      SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.world(.2)), 300, 200)
    assert(missing.left.exists(_.isInstanceOf[SurfaceDecorationError.NetworkMapping]))

  test("validation refuses a cross-surface target vertex outside the mapped scientific domain"):
    val (plan, leftId, rightId) = composition()
    val valid = lower(plan, leftId, rightId)
    val invalid = valid.meshes.map:
      case mesh @ SurfaceDecorationMesh(_, _, SurfaceDecorationTarget.NetworkEdge(source, _), _, _, _, _, _, _, _) =>
        mesh.copy(target = SurfaceDecorationTarget.NetworkEdge(source, SurfaceVertexAnchor.make(right, VertexId(99))))
      case mesh => mesh
    assert(SurfaceDecorationValidation.validate(valid.copy(meshes = invalid), plan, 300, 200).isLeft)
