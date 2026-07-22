package scalafim.surface.view

import scalafim.graphics.*
import scalafim.surface.*

class SurfaceSceneDocumentSuite extends munit.FunSuite:
  private val leftId = SurfaceId.unsafe("left")
  private val rightId = SurfaceId.unsafe("right")
  private val activationId = SurfaceLayerId.unsafe("activation")
  private val labelsId = SurfaceLayerId.unsafe("labels")

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

    val future = encoded.replace("\"revision\":1", "\"revision\":2")
    assertEquals(
      SurfaceSceneCodec.decode(future).left.toOption,
      Some(SurfaceSceneError.UnsupportedRevision(2))
    )

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
