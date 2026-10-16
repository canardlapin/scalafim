package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceSceneDocumentSuite extends munit.FunSuite:
  private val leftId = SurfaceId.unsafe("left")
  private val rightId = SurfaceId.unsafe("right")
  private val activationId = SurfaceLayerId.unsafe("activation")
  private val labelsId = SurfaceLayerId.unsafe("labels")

  test("revision 1 documents retain vertex association and their original JSON shape"):
    val (model, state, bindings, provenance) = fixture()
    val current = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val json = SurfaceSceneCodec.encode(current).replaceAll(",\"surfaceViewpoints\":\\[\\],\"geometryStates\":.*}$", "}").replaceAll(",\"aspectRatio\":1(?:[.]0)?", "").replace(",\"framing\":\"bounds-fit\"", "").replace("\"revision\":9", "\"revision\":1").replace(",\"legends\":[]", "")
      .replace(",\"scalarInterpolation\":false", "")
      .replaceAll(",\"scalarMappingKey\":(?:null|\"[^\"]*\")", "")
      .replace(",\"vertexInterpolation\":\"color\"", "")
      .replace(",\"association\":\"vertex\"", "").replace(",\"face\":null", "")
    assert(!json.contains("\"association\""))
    val decoded = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(decoded.revision, SurfaceDocumentRevision.V1)
    assert(decoded.layers.forall(_.association == SurfaceSampleAssociation.Vertex))
    assertEquals(SurfaceSceneCodec.encode(decoded), json)

  test("canonical document roundtrip reproduces state, camera, and semantic render resources"):
    val (model, state, bindings, provenance) = fixture()
    val document = SurfaceSceneDocument.capture(
      model,
      state,
      bindings,
      provenance,
      Set(SurfaceBackendFeature.DepthBuffer, SurfaceBackendFeature.BilateralViewports)
    ).toOption.get
    val encoded = SurfaceSceneCodec.encode(document)
    val decoded = SurfaceSceneCodec.decode(encoded).toOption.get
    val encodedAgain = SurfaceSceneCodec.encode(decoded)
    val restored = decoded.restore(model, bindings).toOption.get

    assertEquals(encodedAgain, encoded)
    assertEquals(restored, state)
    assert(!encoded.contains("vertexValues"))
    assert(encoded.contains("asset://surfaces/left"))
    assert(encoded.indexOf("\"analysis\"") < encoded.indexOf("\"subject\""))

    val before = SurfaceCompiler.compile(model, state).toOption.get
    val after = SurfaceCompiler.compile(model, restored).toOption.get
    assertEquals(after.meshes.map(_.resourceKey), before.meshes.map(_.resourceKey))
    assertEquals(after.layers.map(_.resourceKey), before.layers.map(_.resourceKey))
    assertEquals(after.layers.map(layer => Vector.tabulate(layer.colors.length)(layer.colors.apply)),
      before.layers.map(layer => Vector.tabulate(layer.colors.length)(layer.colors.apply)))
    assertEquals(Vector.tabulate(after.camera.viewMatrix.length)(after.camera.viewMatrix.apply),
      Vector.tabulate(before.camera.viewMatrix.length)(before.camera.viewMatrix.apply))
    assertEquals(Vector.tabulate(after.camera.projectionMatrix.length)(after.camera.projectionMatrix.apply),
      Vector.tabulate(before.camera.projectionMatrix.length)(before.camera.projectionMatrix.apply))
    assertEquals((after.camera.directionX, after.camera.directionY, after.camera.directionZ),
      (before.camera.directionX, before.camera.directionY, before.camera.directionZ))
    assertEquals(after.slots, before.slots)
    assertEquals(after.drawPasses, before.drawPasses)

  test("camera aspect survives document encoding and restoration"):
    val (model, initial, bindings, provenance) = fixture()
    val state = SurfaceViewer.reduce(model, initial,
      SurfaceViewerAction.SetAspectRatio(CameraAspectRatio.unsafe(1.75))).toOption.get
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val restored = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(document)).toOption.get
      .restore(model, bindings).toOption.get
    assertEqualsDouble(restored.camera.aspectRatio.value, 1.75, 0.0)
    assertEquals(restored, state)

  test("unknown fields and future revisions obey explicit read policy"):
    val (model, state, bindings, provenance) = fixture()
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val encoded = SurfaceSceneCodec.encode(document)
    val extended = encoded.dropRight(1) + ",\"futureExtension\":{\"answer\":42}}"
    assertEquals(
      SurfaceSceneCodec.decode(extended).left.toOption,
      Some(SurfaceSceneError.UnknownField("$", "futureExtension"))
    )
    val permissive = SurfaceSceneCodec.decode(
      extended,
      SurfaceSceneReadPolicy(SurfaceUnknownFieldPolicy.Ignore)
    ).toOption.get
    assertEquals(SurfaceSceneCodec.encode(permissive), encoded)

    val future = encoded.replace("\"revision\":9", "\"revision\":10")
    assertEquals(
      SurfaceSceneCodec.decode(future).left.toOption,
      Some(SurfaceSceneError.UnsupportedRevision(10))
    )

  test("canonical and historical revision 8 discriminators remain distinct"):
    val (model, state, bindings, provenance) = fixture()
    val v9 = SurfaceSceneCodec.encode(SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get)
    val canonical = v9.replaceAll(",\"surfaceViewpoints\":\\[\\],\"geometryStates\":.*}$", "}").replace("\"revision\":9", "\"revision\":8")
    assertEquals(SurfaceSceneCodec.decode(canonical).toOption.get.revision, SurfaceDocumentRevision.V8)
    val historical = v9.replace("\"revision\":9", "\"revision\":8").replace(",\"framing\":\"bounds-fit\"", "")
    val recovered = SurfaceSceneCodec.decode(historical).toOption.get
    assertEquals(recovered.revision, SurfaceDocumentRevision.V9)
    val hybrid = historical.dropRight(1) + ",\"colorPipeline\":\"legacy-display-bytes\"}"
    assert(SurfaceSceneCodec.decode(hybrid).isLeft)
    assert(SurfaceSceneCodec.decode(hybrid, SurfaceSceneReadPolicy(SurfaceUnknownFieldPolicy.Ignore)).isLeft)

  test("external digests and exact mesh identities are checked before state restoration"):
    val (model, state, bindings, provenance) = fixture()
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val wrongReference = SurfaceExternalReference(
      SurfaceAssetUri.unsafe("asset://surfaces/left"),
      SurfaceContentDigest.unsafe("f" * 64)
    )
    val wrongBindings = bindings.copy(surfaces = bindings.surfaces.updated(leftId, wrongReference))
    assertEquals(
      document.restore(model, wrongBindings).left.toOption,
      Some(SurfaceSceneError.AssetReferenceMismatch("left"))
    )

    val changedLeft = SurfaceAsset.make(leftId, geometry(Hemisphere.Left, SurfaceKind.Pial, 0.0)).toOption.get
    val changedModel = SurfaceViewerModel.make(
      model.surfaces.updated(0, changedLeft),
      model.layers
    ).toOption.get
    assert(document.restore(changedModel, bindings).left.exists(_.message.contains("identity differs")))

    val rewound = geometry(Hemisphere.Left, SurfaceKind.Inflated, 0.0, reversed = true)
    val mismatchedLayer = SurfaceLayer.scalar(
      activationId,
      leftId,
      rewound,
      Array.fill(rewound.vertexCount * 2)(1.0),
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 1.0)),
      frameCount = 2
    ).toOption.get
    val mismatchedModel = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, rewound).toOption.get, model.surfaces(1)),
      Vector(mismatchedLayer, model.layers(1))
    ).toOption.get
    assert(document.restore(mismatchedModel, bindings).left.exists(_.message.contains("identity differs")))

  test("backend-neutral requirements admit capabilities without naming an implementation"):
    val (model, state, bindings, provenance) = fixture()
    val document = SurfaceSceneDocument.capture(
      model,
      state,
      bindings,
      provenance,
      Set(SurfaceBackendFeature.DepthBuffer, SurfaceBackendFeature.WorldClipping)
    ).toOption.get
    val incomplete = SurfaceBackendCapabilities(
      SurfaceBackendId.unsafe("test"),
      SurfacePlanRevision.Current,
      Set(SurfaceBackendFeature.DepthBuffer)
    )
    assertEquals(
      document.admit(incomplete).left.toOption,
      Some(SurfaceSceneError.MissingCapabilities(Vector(SurfaceBackendFeature.WorldClipping)))
    )
    assert(!SurfaceSceneCodec.encode(document).toLowerCase.contains("javafx"))
    assert(!SurfaceSceneCodec.encode(document).toLowerCase.contains("three"))

  test("codec rejects duplicate fields, malformed JSON, invalid digests, and invalid document state"):
    assert(SurfaceContentDigest.make("not-a-digest").isLeft)
    assert(SurfaceSceneCodec.decode("{\"revision\":1,\"revision\":1}").left.exists(_.message.contains("duplicate field")))
    assert(SurfaceSceneCodec.decode("{oops").isLeft)

    val (model, state, bindings, provenance) = fixture()
    val encoded = SurfaceSceneCodec.encode(
      SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    )
    val invalidTime = encoded.replace("\"timepoint\":1", "\"timepoint\":99")
    assert(SurfaceSceneCodec.decode(invalidTime).left.exists(_.message.contains("timepoint 99")))

  test("every display threshold kind roundtrips through the scene codec"):
    val (model, state, bindings, provenance) = fixture()
    val thresholds = Vector(
      DisplayThreshold.Disabled,
      DisplayThreshold.transparentBand(-0.2, 0.3).toOption.get,
      DisplayThreshold.below(-0.25).toOption.get,
      DisplayThreshold.above(1.5).toOption.get,
      DisplayThreshold.twoSidedMagnitude(0.4).toOption.get,
      DisplayThreshold.twoSided(ThresholdBand.unsafe(-0.5, 0.25), Some(ThresholdBand.unsafe(-1.0, 1.75))).toOption.get
    )
    // Exhaustive over the enum: a new Intaglio case warns here until the codec and this suite cover it.
    def kind(threshold: DisplayThreshold): String =
      threshold match
        case DisplayThreshold.Disabled => "disabled"
        case DisplayThreshold.TransparentBand(_) => "transparent-band"
        case DisplayThreshold.Below(_) => "below"
        case DisplayThreshold.Above(_) => "above"
        case DisplayThreshold.TwoSided(_, _) => "two-sided"
    assertEquals(thresholds.map(kind).toSet, Set("disabled", "transparent-band", "below", "above", "two-sided"))
    assertEquals(thresholds.collect { case DisplayThreshold.TwoSided(_, outer) => outer.isDefined }, Vector(false, true))

    thresholds.foreach: threshold =>
      val thresholded = SurfaceViewer.reduce(
        model,
        state,
        SurfaceViewerAction.SetLayerThreshold(activationId, threshold)
      ).toOption.get
      val document = SurfaceSceneDocument.capture(model, thresholded, bindings, provenance).toOption.get
      val encoded = SurfaceSceneCodec.encode(document)
      assert(encoded.contains(s"\"kind\":\"${kind(threshold)}\""), clue(encoded))
      val decoded = SurfaceSceneCodec.decode(encoded).toOption.get
      assertEquals(SurfaceSceneCodec.encode(decoded), encoded)
      assertEquals(decoded.restore(model, bindings).toOption.get, thresholded)

  test("threshold decoding rejects invalid cutoffs, nesting, and fields foreign to the kind"):
    val (model, state, bindings, provenance) = fixture()
    def encodedWith(threshold: DisplayThreshold): String =
      val thresholded = SurfaceViewer.reduce(
        model,
        state,
        SurfaceViewerAction.SetLayerThreshold(activationId, threshold)
      ).toOption.get
      SurfaceSceneCodec.encode(SurfaceSceneDocument.capture(model, thresholded, bindings, provenance).toOption.get)

    val above = encodedWith(DisplayThreshold.above(1.5).toOption.get)
    val aboveJson = "{\"kind\":\"above\",\"cutoff\":1.5}"
    assert(above.contains(aboveJson), clue(above))
    assert(SurfaceSceneCodec.decode(above.replace(aboveJson, "{\"kind\":\"above\",\"cutoff\":1e999}"))
      .left.exists(_.message.contains("expected finite number")))
    assert(SurfaceSceneCodec.decode(above.replace(aboveJson, "{\"kind\":\"above\"}"))
      .left.exists(_.message.contains("missing required field")))
    assertEquals(
      SurfaceSceneCodec.decode(above.replace(aboveJson, "{\"kind\":\"above\",\"cutoff\":1.5,\"lower\":0.0}"))
        .left.toOption.collect { case SurfaceSceneError.UnknownField(_, field) => field },
      Some("lower")
    )
    val ignored = SurfaceSceneCodec.decode(
      above.replace(aboveJson, "{\"kind\":\"above\",\"cutoff\":1.5,\"lower\":0.0}"),
      SurfaceSceneReadPolicy(SurfaceUnknownFieldPolicy.Ignore)
    ).toOption.get
    assertEquals(SurfaceSceneCodec.encode(ignored), above)

    val twoSided = encodedWith(DisplayThreshold.twoSidedMagnitude(0.4, Some(1.5)).toOption.get)
    val twoSidedJson = "{\"kind\":\"two-sided\",\"inner\":[-0.4,0.4],\"outer\":[-1.5,1.5]}"
    assert(twoSided.contains(twoSidedJson), clue(twoSided))
    val notNested = twoSided.replace(twoSidedJson, "{\"kind\":\"two-sided\",\"inner\":[-0.4,0.4],\"outer\":[-0.2,1.5]}")
    assert(SurfaceSceneCodec.decode(notNested).left.exists(_.isInstanceOf[SurfaceSceneError.InvalidState]))
    val malformed = twoSided.replace(twoSidedJson, "{\"kind\":\"two-sided\",\"inner\":[0.4],\"outer\":null}")
    assert(SurfaceSceneCodec.decode(malformed).left.exists(_.message.contains("expected two numbers")))
    val scalarOuter = twoSided.replace(twoSidedJson, "{\"kind\":\"two-sided\",\"inner\":[-0.4,0.4],\"outer\":1.5}")
    assert(SurfaceSceneCodec.decode(scalarOuter).left.exists(_.message.contains("expected null or a two-number array")))
    val missingOuter = twoSided.replace(twoSidedJson, "{\"kind\":\"two-sided\",\"inner\":[-0.4,0.4]}")
    assert(SurfaceSceneCodec.decode(missingOuter).left.exists(_.message.contains("missing required field")))

  test("two-sided threshold accepts the legacy V8 form and re-encodes canonically"):
    val (model, state, bindings, provenance) = fixture()
    val thresholded = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetLayerThreshold(
      activationId, DisplayThreshold.twoSidedMagnitude(0.4, Some(1.5)).toOption.get
    )).toOption.get
    val canonical = SurfaceSceneCodec.encode(SurfaceSceneDocument.capture(model, thresholded, bindings, provenance).toOption.get)
    val canonicalJson = "{\"kind\":\"two-sided\",\"inner\":[-0.4,0.4],\"outer\":[-1.5,1.5]}"
    val legacyJson = "{\"kind\":\"two-sided\",\"lower\":-0.4,\"upper\":0.4,\"outer\":[-1.5,1.5]}"
    assert(canonical.contains(canonicalJson), clue(canonical))
    val legacy = canonical.replace(canonicalJson, legacyJson)
    val decodedCanonical = SurfaceSceneCodec.decode(canonical).toOption.get
    val decodedLegacy = SurfaceSceneCodec.decode(legacy).toOption.get
    assertEquals(decodedLegacy.restore(model, bindings).toOption.get, decodedCanonical.restore(model, bindings).toOption.get)
    assertEquals(SurfaceSceneCodec.encode(decodedLegacy), canonical)

    val malformedLegacy = legacy.replace(legacyJson,
      "{\"kind\":\"two-sided\",\"lower\":-0.4,\"outer\":null}")
    assert(SurfaceSceneCodec.decode(malformedLegacy).left.exists(_.message.contains("missing required field")))
    val mixed = canonical.replace(canonicalJson,
      "{\"kind\":\"two-sided\",\"inner\":[-0.4,0.4],\"lower\":-0.4,\"outer\":[-1.5,1.5]}")
    assert(SurfaceSceneCodec.decode(mixed).isLeft)
    assert(SurfaceSceneCodec.decode(mixed, SurfaceSceneReadPolicy(SurfaceUnknownFieldPolicy.Ignore)).isLeft)

  test("actual September writer restores nondefault viewpoints and morph coordinates"):
    val (model, expected, bindings, _) = frozenGeometryFixture()
    val document = SurfaceSceneCodec.decode(frozenHistoricalV8).toOption.get
    assertEquals(document.revision, SurfaceDocumentRevision.V9)
    assertEquals(document.framing, CameraFraming.BoundsFit)
    assertEquals(document.restore(model,bindings).toOption.get,expected)
    val plan = SurfaceCompiler.compile(model,document.restore(model,bindings).toOption.get).toOption.get
    assertEquals(plan.meshes.head.positions.unsafeArray.toVector, Vector(-1f,-2f,1f,-1f,4f,1f,-1f,-1f,5f))
    assertEquals(document.surfaceViewpoints,Map(SurfaceId.unsafe("frozen-left") -> SurfaceViewpoint.Medial(CorticalHemisphere.Left)))
    val canonical = SurfaceSceneCodec.decode(frozenCanonicalV8).toOption.get
    assertEquals(canonical.revision,SurfaceDocumentRevision.V8)
    val canonicalEncoded = SurfaceSceneCodec.encode(canonical)
    assertEquals(SurfaceSceneCodec.decode(canonicalEncoded).toOption.get,canonical)
    assert(!canonicalEncoded.contains("geometryStates") && !canonicalEncoded.contains("surfaceViewpoints"))

  test("revision 9 preserves complete geometry, appearance and per-surface camera state"):
    val (model, initial, bindings, provenance) = frozenGeometryFixture()
    val id = model.surfaces.head.id
    val rich = Vector(SurfaceViewerAction.SetColorPipeline(SurfaceColorPipeline.DisplaySrgbLinearLight),
      SurfaceViewerAction.SetSurfaceOpacity(id,DisplayOpacity.unsafe(0.4)),
      SurfaceViewerAction.SetSurfaceBase(Rgba32.unsafe(10,20,30,100)))
      .foldLeft(initial)((s,a) => SurfaceViewer.reduce(model,s,a).toOption.get)
    for state <- Vector(rich,SurfaceViewer.reduce(model,rich,SurfaceViewerAction.SetGeometryState(id,SurfaceKind.Inflated)).toOption.get) do
      val saved = SurfaceSceneDocument.capture(model,state,bindings,provenance).toOption.get
      val decoded = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(saved)).toOption.get
      assertEquals(decoded.restore(model,bindings).toOption.get,state)
      assertEquals(decoded.geometryStates.size,1)
      assertEquals(decoded.geometryStates.head.variants.keySet,Set(SurfaceKind.White,SurfaceKind.Inflated))

  test("geometry duplicates, missing evidence, bad endpoints and changed content are refused"):
    val (model,state,bindings,provenance) = frozenGeometryFixture()
    val saved = SurfaceSceneDocument.capture(model,state,bindings,provenance).toOption.get
    val geometry = saved.geometryStates.head
    val root = SceneJsonParser.parse(SurfaceSceneCodec.encode(saved)).toOption.get.asInstanceOf[SceneJson.Obj]
    def rejected(states: Vector[SurfaceSceneGeometry]): Unit =
      val next = SceneJson.Obj(root.fields.map:
        case ("geometryStates",_) => "geometryStates" -> SceneJson.Arr(states.map(SurfaceSceneGeometryCodec.encode))
        case entry => entry)
      assert(SurfaceSceneCodec.decode(next.render).isLeft)
    rejected(Vector.empty)
    rejected(Vector(geometry,geometry))
    rejected(Vector(geometry.copy(surface=SurfaceId.unsafe("absent"))))
    rejected(Vector(geometry.copy(variants=geometry.variants - SurfaceKind.Inflated)))
    val endpoint = SurfaceGeometryPresentation.Morphing(SurfaceKind.White,SurfaceKind.Inflated,SurfaceMorphFraction.unsafe(1.0))
    rejected(Vector(geometry.copy(presentation=endpoint)))
    val changed = geometry.variants.updated(SurfaceKind.Inflated,reference("asset://changed",'c'))
    assert(saved.restore(model,bindings.copy(geometries=Map(geometry.surface -> changed))).isLeft)
    assert(SurfaceSceneDocument.capture(model,state,bindings.copy(geometries=Map.empty),provenance).isLeft)
    val extra = geometry.variants.updated(SurfaceKind.Pial,reference("asset://extra",'d'))
    assert(saved.restore(model,bindings.copy(geometries=Map(geometry.surface -> extra))).isLeft)
    val badDefault = geometry.variants.updated(SurfaceKind.White,reference("asset://changed-default",'e'))
    assert(SurfaceSceneDocument.capture(model,state,bindings.copy(geometries=Map(geometry.surface -> badDefault)),provenance).isLeft)

  test("historical revision 8 rejects contradictory recognized fields under both unknown-field policies"):
    val root = SceneJsonParser.parse(frozenHistoricalV8).toOption.get.asInstanceOf[SceneJson.Obj]
    def altered(change: SceneJson.Obj => SceneJson.Obj): String = change(root).render
    val bad = Vector(
      altered(r => SceneJson.Obj(r.fields.filterNot(_._1 == "geometryStates"))),
      altered(r => SceneJson.Obj(r.fields.filterNot(_._1 == "surfaceViewpoints"))),
      altered(r => SceneJson.Obj(r.fields :+ ("colorPipeline" -> SceneJson.Str("legacy-display-bytes")))),
      altered(r => SceneJson.Obj(r.fields :+ ("surfaceOpacities" -> SceneJson.Arr(Vector.empty)))),
      altered(r => SceneJson.Obj(r.fields.map:
        case ("camera",c: SceneJson.Obj) => "camera" -> SceneJson.Obj(c.fields :+ ("framing" -> SceneJson.Str("bounds-fit")))
        case entry => entry)),
      frozenHistoricalV8.replace("\"revision\":8","\"revision\":7")
    )
    for json <- bad; policy <- Vector(SurfaceUnknownFieldPolicy.Reject,SurfaceUnknownFieldPolicy.Ignore) do
      assert(SurfaceSceneCodec.decode(json,SurfaceSceneReadPolicy(policy)).isLeft,json)

  private def frozenGeometryFixture(): (SurfaceViewerModel,SurfaceViewerState,SurfaceSceneBindings,SurfaceProvenance) =
    val id = SurfaceId.unsafe("frozen-left")
    def geometry(kind: SurfaceKind, dx: Double) = SurfaceGeometry(TriangleMesh.fromRows(
      Vector(Vector(-3d+dx,-2d,1d),Vector(-3d+dx,4d,1d),Vector(-3d+dx,-1d,5d)),Vector((0,1,2))),Hemisphere.Left,kind)
    val family = SurfaceSet.of(SurfaceKind.White,geometry(SurfaceKind.White,0),SurfaceKind.Inflated -> geometry(SurfaceKind.Inflated,8))
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id,family).toOption.get),Vector.empty).toOption.get
    val actions = Vector(SurfaceViewerAction.SetSurfaceViewpoints(Map(id -> SurfaceViewpoint.Medial(CorticalHemisphere.Left))),
      SurfaceViewerAction.SetAspectRatio(CameraAspectRatio.unsafe(2)),SurfaceViewerAction.BeginGeometryMorph(id,SurfaceKind.Inflated),
      SurfaceViewerAction.SetGeometryMorphFraction(id,SurfaceMorphFraction.unsafe(0.25)))
    val state = actions.foldLeft(SurfaceViewerState.initial(model))((s,a) => SurfaceViewer.reduce(model,s,a).toOption.get)
    def ref(uri: String,c: Char) = SurfaceExternalReference(SurfaceAssetUri.unsafe(uri),SurfaceContentDigest.unsafe(c.toString*64))
    val white = ref("asset://frozen/white",'a'); val inflated = ref("asset://frozen/inflated",'b')
    val bindings = SurfaceSceneBindings(Map(id -> white),Map.empty,Map(id -> Map(SurfaceKind.White -> white,SurfaceKind.Inflated -> inflated)))
    val provenance = SurfaceProvenance.make("frozen-writer","1","2026-09-21T00:00:00Z",Vector.empty).toOption.get
    (model,state,bindings,provenance)

  private def fixture(): (SurfaceViewerModel, SurfaceViewerState, SurfaceSceneBindings, SurfaceProvenance) =
    val left = geometry(Hemisphere.Left, SurfaceKind.Inflated, -1.0)
    val right = geometry(Hemisphere.Right, SurfaceKind.Inflated, 1.0)
    val activation = SurfaceLayer.scalar(
      activationId,
      leftId,
      left,
      Array(-1.0, 0.0, 1.0, 2.0, 2.0, 1.0, 0.0, -1.0),
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 2.0), ColorRamp.Heat),
      frameCount = 2,
      opacity = DisplayOpacity.unsafe(0.8),
      blendMode = DisplayBlendMode.Screen
    ).toOption.get
    val labels = SurfaceLayer.labels(
      labelsId,
      rightId,
      right,
      Array(1, 2, 1, 2, 2, 1, 2, 1),
      LabelColorizer(Map(
        1 -> Rgba32.unsafe(230, 40, 60),
        2 -> Rgba32.unsafe(30, 120, 240)
      )),
      frameCount = 2,
      blendMode = DisplayBlendMode.Normal
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(
        SurfaceAsset.make(leftId, left).toOption.get,
        SurfaceAsset.make(rightId, right).toOption.get
      ),
      Vector(activation, labels)
    ).toOption.get
    val actions = Vector(
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(leftId, rightId, BilateralOrder.RightThenLeft)),
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Medial(CorticalHemisphere.Left)),
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(1.7))),
      SurfaceViewerAction.SetZoom(CameraZoom.unsafe(1.25)),
      SurfaceViewerAction.SetPan(0.2, -0.3),
      SurfaceViewerAction.SetOrbit(SurfaceOrbit.unsafe(17.0, -11.0)),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
      SurfaceViewerAction.SetClipping(SurfaceClipping.worldPlanes(Vector(
        WorldClipPlane.unsafe(1.0, 2.0, 0.5, -0.25, ClipKeepSide.Negative)
      )).toOption.get),
      SurfaceViewerAction.SetTimepoint(1),
      SurfaceViewerAction.Select(rightId, VertexId(2)),
      SurfaceViewerAction.SetLayerVisible(labelsId, false),
      SurfaceViewerAction.SetLayerOpacity(activationId, DisplayOpacity.unsafe(0.55)),
      SurfaceViewerAction.SetLayerWindow(activationId, DisplayWindow.unsafe(-0.75, 1.5)),
      SurfaceViewerAction.SetLayerThreshold(
        activationId,
        DisplayThreshold.transparentBand(-0.2, 0.3).toOption.get
      ),
      SurfaceViewerAction.MoveLayer(labelsId, 0)
    )
    var state = SurfaceViewerState.initial(model)
    actions.foreach(action => state = SurfaceViewer.reduce(model, state, action).toOption.get)
    val bindings = SurfaceSceneBindings(
      Map(
        leftId -> reference("asset://surfaces/left", 'a'),
        rightId -> reference("asset://surfaces/right", 'b')
      ),
      Map(
        activationId -> reference("asset://layers/activation", 'c'),
        labelsId -> reference("asset://layers/labels", 'd')
      )
    )
    val provenance = SurfaceProvenance.make(
      "scalafim-test",
      "1.0.0",
      "2026-07-21T17:00:00Z",
      Vector("subject" -> "sub-01", "analysis" -> "task-memory")
    ).toOption.get
    (model, state, bindings, provenance)

  private def reference(uri: String, digit: Char): SurfaceExternalReference =
    SurfaceExternalReference(SurfaceAssetUri.unsafe(uri), SurfaceContentDigest.unsafe(digit.toString * 64))

  private def geometry(
    hemisphere: Hemisphere,
    kind: SurfaceKind,
    x: Double,
    reversed: Boolean = false
  ): SurfaceGeometry =
    val faces =
      if reversed then Vector((0, 2, 1), (0, 1, 3), (0, 2, 3), (1, 2, 3))
      else Vector((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
    SurfaceGeometry(
      TriangleMesh.fromRows(
        Vector(
          Vector(x, 0.0, 0.0),
          Vector(x + 0.4, 0.0, 0.0),
          Vector(x, 0.8, 0.0),
          Vector(x, 0.0, 1.2)
        ),
        faces
      ),
      hemisphere,
      kind
    )

  // Frozen output from the actual writer at 7c3ff0a0edc92be0e2fe35c431d5b9e458c92ab4; never regenerated by the codec under test.
  private def frozenHistoricalV8: String = """{"schema":"scalafim.surface-scene","revision":8,"assets":[{"id":"frozen-left","uri":"asset://frozen/white","sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","hemisphere":"left","kind":"white","vertexCount":3,"faceCount":1,"topologyIdentity":"2b8fbfed481ceb14"}],"layers":[],"layout":{"kind":"single","surface":"frozen-left"},"camera":{"viewpoint":"lateral-left","projection":{"kind":"perspective","value":35.0},"zoom":1.0,"pan":[0.0,0.0],"orbit":[0.0,0.0],"aspectRatio":2.0},"lighting":{"kind":"directional","ambient":0.35,"diffuse":0.65,"direction":[-0.4,-0.5,0.7681145747868608]},"clipping":{"kind":"disabled"},"timepoint":0,"selection":null,"layerStates":[],"requiredFeatures":[],"provenance":{"producer":"frozen-writer","softwareVersion":"1","createdAt":"2026-09-21T00:00:00Z","entries":{}},"legends":[],"surfaceViewpoints":[{"surface":"frozen-left","viewpoint":"medial-left"}],"geometryStates":[{"surface":"frozen-left","presentation":{"kind":"morphing","from":"white","to":"inflated","fraction":0.25},"variants":[{"kind":"inflated","uri":"asset://frozen/inflated","sha256":"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"},{"kind":"white","uri":"asset://frozen/white","sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"}]}]}"""

  // Frozen output from the actual writer at 56d0cbcbfa55c7edb8651d19153b9e8c7897d88d; never regenerated by the codec under test.
  private def frozenCanonicalV8: String = """{"schema":"scalafim.surface-scene","revision":8,"assets":[{"id":"frozen-left","uri":"asset://frozen/white","sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","hemisphere":"left","kind":"white","vertexCount":3,"faceCount":1,"topologyIdentity":"2b8fbfed481ceb14"}],"layers":[],"layout":{"kind":"single","surface":"frozen-left"},"camera":{"viewpoint":"lateral-left","projection":{"kind":"perspective","value":35.0},"zoom":1.0,"pan":[0.0,0.0],"orbit":[0.0,0.0],"aspectRatio":2.0,"framing":"bounds-fit"},"lighting":{"kind":"directional","ambient":0.35,"diffuse":0.65,"direction":[-0.4,-0.5,0.7681145747868608]},"clipping":{"kind":"disabled"},"timepoint":0,"selection":null,"layerStates":[],"requiredFeatures":[],"provenance":{"producer":"frozen-writer","softwareVersion":"1","createdAt":"2026-09-21T00:00:00Z","entries":{}},"legends":[]}"""
