package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceScalarMappingSuite extends munit.FunSuite:
  private val surface = SurfaceFaceFixture.Surface
  private val layerId = SurfaceLayerId.unsafe("mapped")
  private val blue = Rgba32.unsafe(0, 0, 200)
  private val white = Rgba32.unsafe(200, 200, 200)
  private val red = Rgba32.unsafe(200, 0, 0)
  private val mapping = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-4.0, 8.0), 0.0, -1.0, 2.0,
    ScalarRamp.linear(blue, white), ScalarRamp.linear(white, red)).toOption.get)
  private def model(colorizer: Colorizer[Double] = mapping.colorizer): SurfaceViewerModel =
    val layer = SurfaceLayer.scalar(layerId, surface, SurfaceFaceFixture.geometry,
      Array(-2.5, -1.0, 2.0, 5.0), colorizer).toOption.get
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, SurfaceFaceFixture.geometry).toOption.get), Vector(layer)).toOption.get

  test("vertex and face scalar layers expose the effective descriptor and tabulated colors"):
    val viewer = model()
    val plan = SurfaceCompiler.compile(viewer, SurfaceFaceFixture.state(viewer)).toOption.get
    assertEquals(plan.layers.head.scalarMapping.get.canonicalKey, mapping.canonicalKey)
    val expected = Vector(Rgba32.unsafe(100, 100, 200), white, white, Rgba32.unsafe(200, 100, 100))
    assertEquals(Vector.tabulate(4)(index => plan.layers.head.colors(index)), expected.map(_.toPackedInt))
    val field = SurfaceFaceField.make(SurfaceFaceFixture.geometry, Array(-2.5, 5.0)).toOption.get
    val face = SurfaceLayer.faceScalar(layerId, surface, field, mapping.colorizer)
    val faceModel = SurfaceViewerModel.make(viewer.surfaces, Vector(face)).toOption.get
    val facePlan = SurfaceCompiler.compile(faceModel, SurfaceFaceFixture.state(faceModel)).toOption.get
    assertEquals(facePlan.layers.head.scalarMapping.get.canonicalKey, mapping.canonicalKey)
    assertEquals(Vector.tabulate(6)(index => facePlan.layers.head.colors(index)),
      Vector.fill(3)(expected.head.toPackedInt) ++ Vector.fill(3)(expected.last.toPackedInt))

  test("checked window overrides reject centers or tails outside the new domain"):
    val viewer = model()
    val state = SurfaceFaceFixture.state(viewer)
    val invalid = DisplayWindow.unsafe(-0.5, 8.0)
    assert(SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.SetLayerWindow(layerId, invalid)).isLeft)
    val corrupted = state.copy(presentations = state.presentations.updated(layerId,
      state.presentations(layerId).copy(window = Some(invalid), visible = false)))
    assert(SurfaceCompiler.compile(viewer, corrupted).isLeft)
    val changed = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.SetLayerWindow(layerId, DisplayWindow.unsafe(-8.0, 10.0))).toOption.get
    val plan = SurfaceCompiler.compile(viewer, changed).toOption.get
    assertEqualsDouble(plan.layers.head.scalarMapping.get.scale.window.lower, -8.0, 0.0)

  test("mapping identity changes even when sampled vertex colors happen to remain equal"):
    val viewer = model()
    val state = SurfaceFaceFixture.state(viewer)
    val initial = SurfaceCompiler.compile(viewer, state).toOption.get
    val changed = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.SetLayerThreshold(layerId,
      DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-0.5, 0.5)))).toOption.get
    val plan = SurfaceCompiler.compile(viewer, changed).toOption.get
    assertEquals(Vector.tabulate(4)(plan.layers.head.colors(_)), Vector.tabulate(4)(initial.layers.head.colors(_)))
    assertNotEquals(plan.receipt.layerKeys, initial.receipt.layerKeys)
    assertNotEquals(plan.layers.head.scalarMapping.get.canonicalKey, mapping.canonicalKey)
    assertEquals(plan.receipt.meshKeys, initial.receipt.meshKeys)

  test("camera and opacity leave the effective unlit mapping unchanged"):
    val viewer = model()
    val state = SurfaceFaceFixture.state(viewer)
    for action <- Vector(SurfaceViewerAction.OrbitBy(20.0, 10.0), SurfaceViewerAction.SetLayerOpacity(layerId, DisplayOpacity.unsafe(0.5))) do
      val updated = SurfaceViewer.reduce(viewer, state, action).toOption.get
      assertEquals(SurfaceCompiler.compile(viewer, updated).toOption.get.layers.head.scalarMapping.get.canonicalKey, mapping.canonicalKey)

  test("opaque colorizers get no invented metadata or unsupported adjustment capabilities"):
    val viewer = model(Colorizer.constant[Double](red))
    val state = SurfaceFaceFixture.state(viewer)
    assertEquals(SurfaceCompiler.compile(viewer, state).toOption.get.layers.head.scalarMapping, None)
    assert(SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.SetLayerWindow(layerId, DisplayWindow.unsafe(0.0, 1.0))).isLeft)
    assert(SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.SetLayerThreshold(layerId, DisplayThreshold.Disabled)).isLeft)

  test("current scene revision rejects changed mapping metadata; revision 3 still decodes"):
    val viewer = model()
    val ref = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:mapped"), SurfaceContentDigest.unsafe("c" * 64))
    val bindings = SurfaceSceneBindings(Map(surface -> ref), Map(layerId -> ref))
    val provenance = SurfaceProvenance.make("mapping-test", "1", "2026-09-07").toOption.get
    val doc = SurfaceSceneDocument.capture(viewer, SurfaceFaceFixture.state(viewer), bindings, provenance).toOption.get
    val json = SurfaceSceneCodec.encode(doc)
    val decoded = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(decoded.revision, SurfaceDocumentRevision.V6)
    assertEquals(decoded.layers.head.scalarMappingKey, Some(mapping.canonicalKey))
    assert(decoded.restore(viewer, bindings).isRight)
    assert(decoded.restore(model(mapping.copy(invalid = red).colorizer), bindings).isLeft)
    val old = SurfaceSceneCodec.decode(json.replace(",\"legends\":[]", "").replace("\"revision\":6", "\"revision\":3")
      .replace(",\"scalarInterpolation\":false", "")
      .replaceAll(",\"scalarMappingKey\":(?:null|\"[^\"]*\")", "")).toOption.get
    assertEquals(old.revision, SurfaceDocumentRevision.V3)
    assert(old.restore(viewer, bindings).isRight)
    val v4 = SurfaceSceneCodec.decode(json.replace(",\"legends\":[]", "").replace("\"revision\":6", "\"revision\":4")
      .replace(",\"scalarInterpolation\":false", "")).toOption.get
    assertEquals(v4.revision, SurfaceDocumentRevision.V4)
    assert(v4.restore(viewer, bindings).isRight)
    assert(v4.restore(model(mapping.copy(invalid = red).colorizer), bindings).isLeft)
