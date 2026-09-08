package scalafim.surface.view

import intaglio.*

class SurfaceSceneLegendSuite extends munit.FunSuite:
  private val surface = SurfaceFaceFixture.Surface
  private val scalarId = SurfaceLayerId.unsafe("scalar")
  private val splitId = SurfaceLayerId.unsafe("split")
  private val categoryId = SurfaceLayerId.unsafe("category")
  private val red = Rgba32.unsafe(200, 0, 0)
  private val blue = Rgba32.unsafe(0, 0, 200)
  private val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-4, 8), ScalarRamp.linear(blue, red)))
  private val title = LegendTitle.make("Contrast \"A\"", Some("signal %")).toOption.get
  private def model(fallback: Rgba32 = blue): SurfaceViewerModel =
    val geometry = SurfaceFaceFixture.geometry
    val scalar = SurfaceLayer.scalar(scalarId, surface, geometry, Array(-4.0, -1, 2, 8), mapping.colorizer).toOption.get
    val splitMapping = mapping.copy(scale = ScalarScale.split(DisplayWindow.unsafe(-4, 8), 0, -1, 2,
      ScalarRamp.linear(blue, red), ScalarRamp.linear(blue, red)).toOption.get)
    val split = SurfaceLayer.scalar(splitId, surface, geometry, Array(-4.0, -1, 2, 8), splitMapping.colorizer).toOption.get
    val category = SurfaceLayer.labels(categoryId, surface, geometry, Array(1, 2, 1, 2),
      LabelColorizer(Map(1 -> red, 2 -> blue), fallback), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(scalar, split, category)).toOption.get
  private val requests = Vector(
    SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(scalarId), title, Vector(AxisTick.unsafe(-4, "low"), AxisTick.unsafe(8, "high")), showInvalid = false),
    SurfaceLegendRequest.Split(SurfaceLegendLayers.one(splitId), title),
    SurfaceLegendRequest.Categorical(SurfaceLegendLayers.one(categoryId), title, Map(2 -> "Visual", 1 -> "Motor")),
    SurfaceLegendRequest.Manual(title, Vector(SurfaceLegendItem.unsafe("Selection", red))))
  private val reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:legends"), SurfaceContentDigest.unsafe("d" * 64))
  private val bindings = SurfaceSceneBindings(Map(surface -> reference), Vector(scalarId, splitId, categoryId).map(_ -> reference).toMap)
  private val provenance = SurfaceProvenance.make("legend-test", "1", "2026-09-07").toOption.get
  private def capture(viewer: SurfaceViewerModel, state: SurfaceViewerState): SurfaceSceneDocument =
    SurfaceSceneDocument.capture(viewer, state, bindings, provenance, legendRequests = requests).toOption.get

  test("revision 6 roundtrips all legend alternatives, metadata, effective identities and interpolation"):
    val viewer = model()
    val initial = SurfaceFaceFixture.state(viewer)
    val state = SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.SetLayerThreshold(scalarId,
      DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-1, 1)))).toOption.get
    val document = capture(viewer, state)
    val json = SurfaceSceneCodec.encode(document)
    val restored = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(restored.revision, SurfaceDocumentRevision.V6)
    assertEquals(SurfaceSceneCodec.encode(restored), json)
    assertEquals(restored.legends.map(_.canonicalKey), document.legends.map(_.canonicalKey))
    assertEquals(restored.restore(viewer, bindings), Right(state))
    assertEquals(restored.legends.length, 4)
    assertEquals(restored.layers.find(_.id == categoryId).get.vertexInterpolation, SurfaceVertexInterpolation.NearestSample)

  test("restoration rejects stale legend identity after saved threshold changes or category palette changes"):
    val viewer = model()
    val document = capture(viewer, SurfaceFaceFixture.state(viewer))
    val json = SurfaceSceneCodec.encode(document)
    val changed = json.replaceFirst("\"threshold\":null", "\"threshold\":{\"kind\":\"transparent-band\",\"lower\":-1,\"upper\":1}")
    assertNotEquals(changed, json)
    assert(SurfaceSceneCodec.decode(changed).toOption.get.restore(viewer, bindings).isLeft)
    assert(document.restore(model(fallback = red), bindings).isLeft)
    val tampered = json.replace(SceneJson.Str(document.legends.head.canonicalKey).render, "\"stale\"")
    assertNotEquals(tampered, json)
    assert(SurfaceSceneCodec.decode(tampered).toOption.get.restore(viewer, bindings).isLeft)

  test("fresh capture follows window changes and camera changes preserve saved legend identity"):
    val viewer = model()
    val state = SurfaceFaceFixture.state(viewer)
    val initial = capture(viewer, state)
    val camera = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.OrbitBy(30, 5)).toOption.get
    assertEquals(capture(viewer, camera).legends.map(_.canonicalKey), initial.legends.map(_.canonicalKey))
    val changed = SurfaceViewer.reduce(viewer, state,
      SurfaceViewerAction.SetLayerWindow(scalarId, DisplayWindow.unsafe(-8, 16))).toOption.get
    val updated = capture(viewer, changed)
    assertNotEquals(updated.legends.head.canonicalKey, initial.legends.head.canonicalKey)
    assertEquals(updated.restore(viewer, bindings), Right(changed))

  test("legend codec rejects duplicate categories and applies unknown-field policy within requests"):
    val viewer = model()
    val json = SurfaceSceneCodec.encode(capture(viewer, SurfaceFaceFixture.state(viewer)))
    val duplicate = json.replace("\"id\":2,\"label\":\"Visual\"", "\"id\":1,\"label\":\"Visual\"")
    assertNotEquals(duplicate, json)
    assert(SurfaceSceneCodec.decode(duplicate).isLeft)
    val extended = json.replace("\"fallbackLabel\":", "\"futureLegendOption\":true,\"fallbackLabel\":")
    assert(SurfaceSceneCodec.decode(extended).isLeft)
    val decoded = SurfaceSceneCodec.decode(extended, SurfaceSceneReadPolicy(SurfaceUnknownFieldPolicy.Ignore)).toOption.get
    assertEquals(decoded.restore(viewer, bindings), Right(SurfaceFaceFixture.state(viewer)))
    assert(SurfaceSceneCodec.decode(json.replace("\"revision\":6", "\"revision\":5")).isLeft)

  test("revision 5 documents without legends remain readable and preserve their JSON shape"):
    val viewer = model()
    val document = SurfaceSceneDocument.capture(viewer, SurfaceFaceFixture.state(viewer), bindings, provenance).toOption.get
    val old = SurfaceSceneCodec.encode(document).replace("\"revision\":6", "\"revision\":5").replace(",\"legends\":[]", "")
    val decoded = SurfaceSceneCodec.decode(old).toOption.get
    assertEquals(decoded.legends, Vector.empty)
    assertEquals(SurfaceSceneCodec.encode(decoded), old)
    assert(decoded.restore(viewer, bindings).isRight)
