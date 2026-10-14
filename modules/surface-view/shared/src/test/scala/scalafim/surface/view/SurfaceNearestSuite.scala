package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceNearestSuite extends munit.FunSuite:
  private val fixture = SurfaceNearestFixture

  test("six subtriangles per face cover equal areas and stay within their sample region"):
    val plan = fixture.plan
    val mesh = plan.meshes.head
    assertEquals(plan.profile.facesPacked, 12)
    assertEquals(plan.profile.verticesPacked, 36)
    assertEquals(plan.profile.primitiveBytes, 1448L) // positions, normals, indices, owners, colors, original faces, camera
    for face <- 0 until 12 do
      val offset = face * 9
      def x(corner: Int): Double = mesh.positions(offset + corner * 3).toDouble
      def y(corner: Int): Double = mesh.positions(offset + corner * 3 + 1).toDouble
      val area = ((x(1) - x(0)) * (y(2) - y(0)) - (y(1) - y(0)) * (x(2) - x(0))) / 2
      assertEqualsDouble(area, 1.0 / 3.0, 1e-7)
      val owner = mesh.sourceVertex(face * 3)
      for corner <- 0 until 3 do
        assertEquals(mesh.sourceVertex(face * 3 + corner), owner)
        assertEquals(plan.layers.head.colors(face * 3 + corner), fixture.Palette(owner).toPackedInt)
      for a <- 1 until 9; b <- 1 until (10 - a) do
        val wa = a / 10.0
        val wb = b / 10.0
        val wc = 1.0 - wa - wb
        if wc > 1e-8 then
          val weights = mesh.sourceBarycentric(face, wa, wb, wc)
          assertEquals(mesh.pickedVertex(face, wa, wb, wc), owner)
          assertEqualsDouble(weights._1 + weights._2 + weights._3, 1.0, 1e-12)
          val ids = mesh.sourceFaceVertices(face)
          val points = SurfaceFaceFixture.geometry.mesh.vertices
          val expectedX = weights._1 * points(ids._1).x + weights._2 * points(ids._2).x + weights._3 * points(ids._3).x
          assertEqualsDouble(wa * x(0) + wb * x(1) + wc * x(2), expectedX, 1e-7)

  test("sample ties use the smallest original id, independent of winding"):
    assertEquals(SurfaceNearestPartition.nearestVertex(8, 2, 5, 0.5, 0.5, 0.0), 2)
    assertEquals(SurfaceNearestPartition.nearestVertex(8, 2, 5, 1.0 / 3, 1.0 / 3, 1.0 / 3), 2)
    assertEquals(SurfaceNearestPartition.nearestVertex(5, 8, 2, 0.0, 0.5, 0.5), 2)

  test("time, visibility, and opacity updates preserve partition topology"):
    val model = fixture.model
    val state = fixture.state(model)
    val before = SurfaceCompiler.compile(model, state).toOption.get
    for action <- Vector(SurfaceViewerAction.SetTimepoint(1), SurfaceViewerAction.SetLayerVisible(fixture.Layer, false),
        SurfaceViewerAction.SetLayerOpacity(fixture.Layer, DisplayOpacity.unsafe(0.4))) do
      val after = SurfaceCompiler.compile(model, SurfaceViewer.reduce(model, state, action).toOption.get).toOption.get
      assertEquals(after.receipt.meshKeys, before.receipt.meshKeys)
      assertEquals(after.meshes.head.geometryKey, before.meshes.head.geometryKey)
    val changed = SurfaceCompiler.compile(model,
      SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetTimepoint(1)).toOption.get).toOption.get
    assertEquals(changed.layers.head.colors(0), fixture.Palette(3).toPackedInt)

  test("scene revision 3 persists nearest sampling, rejects incompatible restoration and capability"):
    val model = fixture.model
    val ref = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:test"), SurfaceContentDigest.unsafe("b" * 64))
    val bindings = SurfaceSceneBindings(Map(fixture.Surface -> ref), Map(fixture.Layer -> ref))
    val provenance = SurfaceProvenance.make("test", "1", "2026-09-07").toOption.get
    val doc = SurfaceSceneDocument.capture(model, fixture.state(model), bindings, provenance).toOption.get
    val decoded = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(doc)).toOption.get
    assertEquals(decoded.layers.head.vertexInterpolation, SurfaceVertexInterpolation.NearestSample)
    assertEquals(decoded.restore(model, bindings).toOption.get, fixture.state(model))
    val legacy = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(doc).replace("nearest-sample", "color")).toOption.get
    assert(legacy.restore(model, bindings).isLeft)
    val capability = SurfaceBackendCapabilities(SurfaceBackendId.unsafe("test"), SurfacePlanRevision.Current, Set.empty)
    assert(decoded.admit(capability).isLeft)
    assert(decoded.admit(capability.copy(features = Set(SurfaceBackendFeature.NearestVertexSampling))).isRight)
    val invalid = SurfaceSceneCodec.encode(doc).replace("\"association\":\"vertex\"", "\"association\":\"face\"")
    assert(SurfaceSceneCodec.decode(invalid).isLeft)
    assert(SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(doc).replace("nearest-sample", "scalar")).isLeft)

  test("mixed color interpolation and nearest samples retain original samples for fragment evaluation"):
    val model = fixture.model
    val legacy = SurfaceLayer.scalar(SurfaceLayerId.unsafe("legacy"), fixture.Surface, SurfaceFaceFixture.geometry,
      Array(0.0, 1.0, 2.0, 3.0), ScalarColorizer(DisplayWindow.unsafe(0.0, 3.0))).toOption.get
    val mixed = SurfaceViewerModel.make(model.surfaces, model.layers :+ legacy).toOption.get
    val plan = SurfaceCompiler.compile(mixed, fixture.state(mixed)).toOption.get
    assert(plan.fragmentSurfaces(fixture.Surface))
    assertEquals(plan.layers.last.sampleColors.get.length, 4)
    assertEquals(plan.meshes.head.sampleNormals.get.length, 12)

  test("morphs retain sample ownership, original picks, and layer keys"):
    val original = fixture.model
    val geometry = original.surfaces.head.geometry
    val moved = SurfaceGeometry(TriangleMesh.fromRows(
      geometry.mesh.vertices.map(p => Seq(p.x, p.y, p.z + 2.0)), Seq((0, 1, 2), (0, 2, 3))),
      Hemisphere.Left, SurfaceKind.Pial)
    val asset = SurfaceAsset.make(fixture.Surface, SurfaceSet.of(geometry.kind, geometry, moved.kind -> moved)).toOption.get
    val model = SurfaceViewerModel.make(Vector(asset), original.layers).toOption.get
    val state = SurfaceViewer.reduce(model, fixture.state(model),
      SurfaceViewerAction.SelectFace(fixture.Surface, FaceId(1), VertexId(3))).toOption.get
    val before = SurfaceCompiler.compile(model, state).toOption.get
    val started = SurfaceViewer.reduce(model, state, SurfaceViewerAction.BeginGeometryMorph(fixture.Surface, moved.kind)).toOption.get
    val midway = SurfaceViewer.reduce(model, started,
      SurfaceViewerAction.SetGeometryMorphFraction(fixture.Surface, SurfaceMorphFraction.unsafe(0.5))).toOption.get
    val after = SurfaceCompiler.compile(model, midway).toOption.get
    assertEquals(after.receipt.meshKeys, before.receipt.meshKeys)
    assertEquals(after.receipt.layerKeys, before.receipt.layerKeys)
    assertNotEquals(after.meshes.head.geometryKey, before.meshes.head.geometryKey)
    assertEquals(after.readouts.head.face, Some(1))
    assertEquals(after.readouts.head.layerValues.head._2, "3")
    for corner <- 0 until 36 do
      assertEqualsDouble(after.meshes.head.positions(corner * 3 + 2).toDouble, 1.0, 1e-6)
      assertEquals(after.meshes.head.sourceVertex(corner), before.meshes.head.sourceVertex(corner))

  test("attached network geometry retains cortical partition provenance and accounts for its buffers"):
    val nodes = Vector(0, 2).map: vertex =>
      SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"node-$vertex"), fixture.Surface,
        VertexId(vertex), SurfaceFaceFixture.geometry).toOption.get
    val network = SurfaceNetwork.make(nodes, Vector(SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 1.0))).toOption.get
    val display = SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.025), sides = 6).toOption.get).toOption.get
    val after = SurfaceNetworkCompiler.attach(fixture.plan, fixture.Surface, SurfaceLayerId.unsafe("network"), display).toOption.get.plan
    val mesh = after.meshes.head
    assertEquals(mesh.sourceFace(11), 1)
    assertEquals(mesh.sourceFace(12), 2)
    assertEquals(mesh.pickedVertex(11, 0.8, 0.1, 0.1), 3)
    assertEquals(mesh.sourceVertices.get.length, mesh.positions.length / 3)
    assert(mesh.sourceFaceVertices(12)._1 >= 4)
    assertEquals(after.profile.facesPacked, 24)
    assertEquals(after.profile.colorValuesWritten, 96)
    assertEquals(after.profile.primitiveBytes, 2168L)
