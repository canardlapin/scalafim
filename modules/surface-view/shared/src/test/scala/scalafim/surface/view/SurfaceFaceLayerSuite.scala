package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceFaceLayerSuite extends munit.FunSuite:
  private val fixture = SurfaceFaceFixture

  test("face colors lower to separate render corners with original identity"):
    val plan = fixture.plan
    val mesh = plan.meshes.head
    assertEquals(mesh.positions.length, 18)
    assertEquals(Vector.tabulate(6)(mesh.sourceVertex), Vector(0, 1, 2, 0, 2, 3))
    assertEquals(Vector.tabulate(6)(plan.layers.head.colors(_)),
      Vector.fill(3)(fixture.Red.toPackedInt) ++ Vector.fill(3)(fixture.Blue.toPackedInt))
    assertEquals(plan.layers.head.association, SurfaceSampleAssociation.Face)
    assertEquals(plan.profile.verticesPacked, 6)

  test("face selection reads the chosen face without inventing a vertex-to-face conversion"):
    val model = fixture.model
    val state = fixture.state(model)
    val selected = SurfaceViewer.reduce(model, state,
      SurfaceViewerAction.SelectFace(fixture.Surface, FaceId(1), VertexId(2))).toOption.get
    val plan = SurfaceCompiler.compile(model, selected).toOption.get
    assertEquals(plan.readouts.head.face, Some(1))
    assertEquals(plan.readouts.head.layerValues.head._2, fixture.Blue.toString)
    val vertexOnly = SurfaceViewer.reduce(model, state, SurfaceViewerAction.Select(fixture.Surface, VertexId(2))).toOption.get
    assertEquals(SurfaceCompiler.compile(model, vertexOnly).toOption.get.readouts.head.layerValues, Vector.empty)
    assert(SurfaceViewer.reduce(model, state, SurfaceViewerAction.SelectFace(fixture.Surface, FaceId(1), VertexId(1))).isLeft)
    assert(SurfaceViewer.reduce(model, state, SurfaceViewerAction.SelectFace(fixture.Surface, FaceId(2), VertexId(0))).isLeft)

  test("time and visibility do not change corner topology"):
    val model = fixture.model
    val state = fixture.state(model)
    val original = SurfaceCompiler.compile(model, state).toOption.get
    val next = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetTimepoint(1)).toOption.get
    val changed = SurfaceCompiler.compile(model, next).toOption.get
    assertEquals(changed.receipt.meshKeys, original.receipt.meshKeys)
    assertEquals(changed.layers.head.colors(0), fixture.Blue.toPackedInt)
    assertNotEquals(changed.receipt.layerKeys, original.receipt.layerKeys)
    val hidden = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetLayerVisible(fixture.Layer, false)).toOption.get
    assertEquals(SurfaceCompiler.compile(model, hidden).toOption.get.receipt.meshKeys, original.receipt.meshKeys)

  test("mixed vertex and face layers preserve source colors and morph correspondence"):
    val original = fixture.model
    val geometry = original.surfaces.head.geometry
    val movedMesh = TriangleMesh.fromRows(
      geometry.mesh.vertices.map(point => Seq(point.x, point.y, point.z + 2.0)),
      Seq((0, 1, 2), (0, 2, 3)))
    val moved = SurfaceGeometry(movedMesh, Hemisphere.Left, SurfaceKind.Pial)
    val family = SurfaceSet.of(geometry.kind, geometry, moved.kind -> moved)
    val underlayId = SurfaceLayerId.unsafe("underlay")
    val underlay = SurfaceLayer.scalar(underlayId, fixture.Surface, geometry,
      Array(0.0, 1.0, 2.0, 3.0), ScalarColorizer(DisplayWindow.unsafe(0.0, 3.0))).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(fixture.Surface, family).toOption.get),
      underlay +: original.layers).toOption.get
    val state = fixture.state(model)
    val before = SurfaceCompiler.compile(model, state).toOption.get
    assertEquals(Vector.tabulate(6)(before.layers.head.colors(_)),
      Vector(0, 85, 170, 0, 170, 255).map(gray => Rgba32.unsafe(gray, gray, gray).toPackedInt))
    val started = SurfaceViewer.reduce(model, state, SurfaceViewerAction.BeginGeometryMorph(fixture.Surface, moved.kind)).toOption.get
    val midway = SurfaceViewer.reduce(model, started,
      SurfaceViewerAction.SetGeometryMorphFraction(fixture.Surface, SurfaceMorphFraction.unsafe(0.5))).toOption.get
    val after = SurfaceCompiler.compile(model, midway).toOption.get
    assertEquals(after.receipt.meshKeys, before.receipt.meshKeys)
    assertEquals(after.receipt.layerKeys, before.receipt.layerKeys)
    assertNotEquals(after.meshes.head.geometryKey, before.meshes.head.geometryKey)
    for corner <- 0 until 6 do
      assertEqualsDouble(after.meshes.head.positions(corner * 3 + 2).toDouble, 1.0, 1e-6)
      assertEquals(after.meshes.head.sourceVertex(corner), before.meshes.head.sourceVertex(corner))

  test("face scalar windows and thresholds act on face observations"):
    val geometry = fixture.geometry
    val field = SurfaceFaceField.make(geometry, Array(-2.0, 2.0)).toOption.get
    val layer = SurfaceLayer.faceScalar(fixture.Layer, fixture.Surface, field,
      ScalarColorizer(DisplayWindow.unsafe(-2.0, 2.0)))
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(fixture.Surface, geometry).toOption.get), Vector(layer)).toOption.get
    val state = SurfaceViewer.reduce(model, fixture.state(model), SurfaceViewerAction.SetLayerThreshold(
      fixture.Layer, DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-3.0, 0.0)))).toOption.get
    val plan = SurfaceCompiler.compile(model, state).toOption.get
    assertEquals(plan.layers.head.colors(0), Rgba32.unsafe(0, 0, 0, 0).toPackedInt)
    assertEquals(plan.layers.head.colors(3), Rgba32.unsafe(255, 255, 255).toPackedInt)

  test("network attachment preserves the cortical source-id map"):
    val nodes = Vector(0, 2).map: vertex =>
      SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"node-$vertex"), fixture.Surface,
        VertexId(vertex), fixture.geometry).toOption.get
    val network = SurfaceNetwork.make(nodes,
      Vector(SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(1).id, 1.0))).toOption.get
    val display = SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All,
      SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(0.025), sides = 6).toOption.get).toOption.get
    val before = fixture.plan
    val after = SurfaceNetworkCompiler.attach(before, fixture.Surface, SurfaceLayerId.unsafe("network"), display).toOption.get.plan
    val mesh = after.meshes.head
    assertEquals(Vector.tabulate(6)(mesh.sourceVertex), Vector(0, 1, 2, 0, 2, 3))
    assertEquals(mesh.sourceVertices.get.length, mesh.positions.length / 3)
    assert(mesh.sourceVertex(6) >= 4)
    assertEquals(Vector.tabulate(6)(after.layers.head.colors(_)), Vector.tabulate(6)(before.layers.head.colors(_)))
    // 18 positions/normals, 14 triangles, 18 source IDs, two 18-color layers,
    // and two 4x4 camera matrices, all stored in four-byte elements.
    assertEquals(after.profile.primitiveBytes, 944L)
    assertEquals(after.profile.colorValuesWritten, 36)

  test("current revision roundtrips association and face selection; revision 1 remains vertex-only"):
    val model = fixture.model
    val state = SurfaceViewer.reduce(model, fixture.state(model),
      SurfaceViewerAction.SelectFace(fixture.Surface, FaceId(1), VertexId(2))).toOption.get
    val reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:test"), SurfaceContentDigest.unsafe("a" * 64))
    val bindings = SurfaceSceneBindings(Map(fixture.Surface -> reference), Map(fixture.Layer -> reference))
    val provenance = SurfaceProvenance.make("test", "1", "2026-09-07").toOption.get
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val decoded = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(document)).toOption.get
    val v2 = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(document)
      .replace(",\"legends\":[]", "").replace("\"revision\":6", "\"revision\":2")
      .replace(",\"scalarInterpolation\":false", "")
      .replace(",\"scalarMappingKey\":null", "")
      .replace(",\"vertexInterpolation\":\"color\"", "")).toOption.get
    assertEquals(v2.revision, SurfaceDocumentRevision.V2)
    assertEquals(v2.layers.head.association, SurfaceSampleAssociation.Face)
    assert(v2.restore(model, bindings).isRight)
    assertEquals(decoded.layers.head.association, SurfaceSampleAssociation.Face)
    assertEquals(decoded.selection, state.selection)
    assertEquals(decoded.revision, SurfaceDocumentRevision.V6)
    val unsupported = SurfaceBackendCapabilities(SurfaceBackendId.unsafe("vertex-only"), SurfacePlanRevision.Current, Set.empty)
    assert(decoded.admit(unsupported).isLeft)
    assert(decoded.admit(unsupported.copy(features = Set(SurfaceBackendFeature.FacewiseData))).isRight)
    val restored = decoded.restore(model, bindings).toOption.get
    assertEquals(restored.selection, state.selection)
    assertEquals(SurfaceCompiler.compile(model, restored).toOption.get.receipt.layerKeys,
      SurfaceCompiler.compile(model, state).toOption.get.receipt.layerKeys)
