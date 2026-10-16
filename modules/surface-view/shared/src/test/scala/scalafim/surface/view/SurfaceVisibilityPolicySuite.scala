package scalafim.surface.view

import intaglio.DisplayOpacity

class SurfaceVisibilityPolicySuite extends munit.FunSuite:
  private val model = SurfaceFaceFixture.model
  private val reference = SurfaceExternalReference(
    SurfaceAssetUri.unsafe("fixture:visibility"),
    SurfaceContentDigest.unsafe("e" * 64)
  )
  private val bindings = SurfaceSceneBindings(
    Map(SurfaceFaceFixture.Surface -> reference),
    Map(SurfaceFaceFixture.Layer -> reference)
  )
  private val provenance = SurfaceProvenance.make("visibility-test", "1", "2026-10-14").toOption.get

  private def configured: SurfaceViewerState =
    val initial = SurfaceFaceFixture.state(model)
    val withPipeline = SurfaceViewer.reduce(model, initial,
      SurfaceViewerAction.SetColorPipeline(SurfaceColorPipeline.DisplaySrgbLinearLight)).toOption.get
    SurfaceViewer.reduce(model, withPipeline,
      SurfaceViewerAction.SetSurfaceOpacity(SurfaceFaceFixture.Surface, DisplayOpacity.unsafe(0.4)))
      .flatMap(state => SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetSurfaceBase(intaglio.Rgba32.unsafe(0, 0, 0, 0))))
      .toOption.get

  test("state actions validate identities and compiler carries nondefault display policy"):
    val state = configured
    assert(SurfaceViewer.reduce(model, state,
      SurfaceViewerAction.SetSurfaceOpacity(SurfaceId.unsafe("unknown"), DisplayOpacity.unsafe(0.5))).isLeft)
    val plan = SurfaceCompiler.compile(model, state).toOption.get
    assertEquals(plan.colorPipeline, SurfaceColorPipeline.DisplaySrgbLinearLight)
    assertEquals(plan.surfaceOpacities, Map(SurfaceFaceFixture.Surface -> DisplayOpacity.unsafe(0.4)))
    assertEquals(plan.surfaceBase, intaglio.Rgba32.unsafe(0, 0, 0, 0))
    assert(plan.fragmentSurfaces(SurfaceFaceFixture.Surface))
    val composition = SurfaceCompositionPlan.fromLegacy(plan).toOption.get
    assertEquals(composition.views.head.colorPipeline, SurfaceColorPipeline.DisplaySrgbLinearLight)
    assertEquals(composition.views.head.instances.head.opacity, DisplayOpacity.unsafe(0.4))

  test("revision 8 roundtrips display policy and rejects opacity for an undeclared surface"):
    val document = SurfaceSceneDocument.capture(model, configured, bindings, provenance).toOption.get
    val json = SurfaceSceneCodec.encode(document)
    val decoded = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(decoded.revision, SurfaceDocumentRevision.V8)
    assertEquals(decoded.colorPipeline, SurfaceColorPipeline.DisplaySrgbLinearLight)
    assertEquals(decoded.surfaceOpacities, Map(SurfaceFaceFixture.Surface -> DisplayOpacity.unsafe(0.4)))
    assertEquals(decoded.surfaceBase, intaglio.Rgba32.unsafe(0, 0, 0, 0))
    assertEquals(decoded.restore(model, bindings), Right(configured))
    val unknown = json.replace("\"surface\":\"facewise-left\",\"opacity\":0.4", "\"surface\":\"unknown\",\"opacity\":0.4")
    assert(SurfaceSceneCodec.decode(unknown).isLeft)

  test("pre-version-8 scene documents restore legacy policy and opaque surfaces"):
    val document = SurfaceSceneDocument.capture(model, configured, bindings, provenance).toOption.get
    val legacy = SurfaceSceneCodec.encode(document)
      .replace(",\"colorPipeline\":\"display-srgb-linear-light\",\"surfaceOpacities\":[{\"surface\":\"facewise-left\",\"opacity\":0.4}],\"surfaceBase\":[0,0,0,0]", "")
      .replace("\"revision\":8", "\"revision\":7")
    val decoded = SurfaceSceneCodec.decode(legacy).toOption.get
    assertEquals(decoded.colorPipeline, SurfaceColorPipeline.LegacyDisplayBytes)
    assertEquals(decoded.surfaceOpacities, Map.empty)
    assertEquals(decoded.surfaceBase, SurfaceDisplayDefaults.LegacySurfaceBase)
    val restored = decoded.restore(model, bindings).toOption.get
    assertEquals(restored.colorPipeline, SurfaceColorPipeline.LegacyDisplayBytes)
    assertEquals(restored.surfaceOpacities, Map.empty)
