package scalafim.surface.view

import scalafim.surface.*

class SurfacePairedCameraSuite extends munit.FunSuite:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")
  private def asset(id: SurfaceId, hemisphere: Hemisphere, x: Double): SurfaceAsset =
    val mesh = TriangleMesh.fromRows(
      Vector(Vector(x, -8.0, -3.0), Vector(x + 4.0, 11.0, -2.0),
        Vector(x + 1.0, -3.0, 9.0), Vector(x + 2.0, 2.0, 1.0)),
      Vector((0, 1, 2), (0, 3, 1), (1, 3, 2), (2, 3, 0)))
    SurfaceAsset.make(id, SurfaceGeometry(mesh, hemisphere, SurfaceKind.Inflated)).toOption.get
  private val model = SurfaceViewerModel.make(Vector(asset(left, Hemisphere.Left, -40),
    asset(right, Hemisphere.Right, 21)), Vector.empty).toOption.get
  private val lateral = Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left),
    right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right))
  private val medial = Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left),
    right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right))
  private def reduce(state: SurfaceViewerState, action: SurfaceViewerAction): SurfaceViewerState =
    SurfaceViewer.reduce(model, state, action).fold(error => fail(error.message), identity)
  private def paired(views: Map[SurfaceId, SurfaceViewpoint] = lateral): SurfaceViewerState =
    reduce(reduce(SurfaceViewerState.initial(model), SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left, right))),
      SurfaceViewerAction.SetSurfaceViewpoints(views))
  private def compile(state: SurfaceViewerState): SurfaceRenderPlan =
    SurfaceCompiler.compile(model, state).fold(error => fail(error.message), identity)

  test("paired lateral and medial viewpoints have opposite anatomical directions without changing geometry"):
    val baseline = compile(paired(Map.empty))
    for (views, leftX) <- Vector(lateral -> -1.0, medial -> 1.0) do
      val plan = compile(paired(views))
      assertEqualsDouble(plan.cameraFor(left).directionX, leftX, 1e-12)
      assertEqualsDouble(plan.cameraFor(right).directionX, -leftX, 1e-12)
      assertEquals(plan.receipt.meshKeys, baseline.receipt.meshKeys)
      assertEquals(plan.slots, baseline.slots)
      plan.meshes.zip(baseline.meshes).foreach: (actual, expected) =>
        assertEquals(actual.positions.unsafeArray.toVector, expected.positions.unsafeArray.toVector)
        assertEquals(actual.indices.unsafeArray.toVector, expected.indices.unsafeArray.toVector)
      assert(plan.validCameras)
      assertEquals(plan.profile.primitiveBytes - baseline.profile.primitiveBytes, 2L * 32 * 4)

  test("navigation is shared, reset retains anatomical viewpoints, focus retains hidden overrides"):
    val initial = paired()
    val moved = reduce(reduce(initial, SurfaceViewerAction.OrbitBy(20, 13)),
      SurfaceViewerAction.SetZoom(CameraZoom.unsafe(1.7)))
    assertEquals(moved.cameraFor(left).orbit, moved.cameraFor(right).orbit)
    assertEquals(moved.cameraFor(left).zoom, moved.cameraFor(right).zoom)
    val focused = reduce(moved, SurfaceViewerAction.SetLayout(SurfaceLayout.Single(right)))
    assertEquals(focused.surfaceViewpoints, lateral)
    assertEquals(compile(focused).surfaceCameras.keySet, Set(right))
    val reset = reduce(focused, SurfaceViewerAction.ResetCamera)
    assertEquals(reset.surfaceViewpoints, lateral)
    assertEquals(reset.camera.zoom, CameraZoom.Default)
    assertEquals(reset.camera.orbit, SurfaceOrbit.Zero)
    val shared = reduce(reset, SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal))
    assertEquals(shared.surfaceViewpoints, Map.empty[SurfaceId, SurfaceViewpoint])
    assertEquals(shared.cameraFor(right).viewpoint, SurfaceViewpoint.Dorsal)

  test("unknown surface overrides fail atomically and compile rejects invalid copied state"):
    val state = paired()
    val invalid = lateral.updated(SurfaceId.unsafe("missing"), SurfaceViewpoint.Dorsal)
    assert(SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetSurfaceViewpoints(invalid)).isLeft)
    assertEquals(state.surfaceViewpoints, lateral)
    assert(SurfaceCompiler.compile(model, state.copy(surfaceViewpoints = invalid)).isLeft)
    assert(SurfaceViewer.reduce(model, state.copy(surfaceViewpoints = invalid), SurfaceViewerAction.FitCamera).isLeft)

  test("explicit fit contains both effective cameras with common pixel scale across projections and orbit"):
    for
      views <- Vector(lateral, medial, Map(left -> SurfaceViewpoint.Dorsal, right -> SurfaceViewpoint.Anterior))
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default),
        CameraProjection.Orthographic(OrthographicScale.unsafe(1.0)))
      orbit <- Vector(SurfaceOrbit.Zero, SurfaceOrbit.unsafe(31, -17))
    do
      val state = paired(views)
      val fit = reduce(state.copy(camera = state.camera.copy(projection = projection, orbit = orbit)), SurfaceViewerAction.FitCamera)
      val plan = compile(fit)
      val extents = plan.slots.flatMap: slot =>
        val camera = plan.cameraFor(slot.surface)
        val v = camera.viewMatrix
        val p = camera.projectionMatrix
        val positions = plan.meshes.find(_.surface == slot.surface).get.positions
        (0 until positions.length by 3).map: index =>
          val x = positions(index) + slot.worldOffsetX
          val y = positions(index + 1) + slot.worldOffsetY
          val z = positions(index + 2) + slot.worldOffsetZ
          val eye = Vector.tabulate(4)(row => v(row * 4) * x + v(row * 4 + 1) * y + v(row * 4 + 2) * z + v(row * 4 + 3))
          val clip = Vector.tabulate(4)(row => (0 until 4).map(k => p(row * 4 + k) * eye(k)).sum)
          assert(clip(3) > 0)
          math.max(math.abs(clip(0) / clip(3)), math.abs(clip(1) / clip(3)))
      assert(extents.max <= 0.90001, s"$views / $projection / $orbit: ${extents.max}")
      for (width, height) <- Vector(900.0 -> 300.0, 300.0 -> 900.0, 1024.0 -> 768.0) do
        plan.viewportFit.resolve(plan.slots, width, height).foreach: slot =>
          val p = plan.cameraFor(slot.surface).projectionMatrix
          assertEqualsDouble(width * slot.viewport.width * p(0), height * slot.viewport.height * p(5), 1e-3)

  test("camera keys are value stable and independent of map insertion order or lighting"):
    val first = compile(paired(lateral))
    val reordered = compile(paired(lateral.toVector.reverse.toMap))
    val lit = compile(paired().copy(lighting = SurfaceLighting.Unlit))
    assertEquals(first.receipt.cameraKey, reordered.receipt.cameraKey)
    assertEquals(first.receipt.cameraKey, lit.receipt.cameraKey)
    assertNotEquals(first.receipt.cameraKey, compile(paired(medial)).receipt.cameraKey)

  test("revision7 persists paired viewpoints and fitted aspect; revision6 remains readable"):
    val state = reduce(paired(), SurfaceViewerAction.FitCamera)
    val reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:paired"), SurfaceContentDigest.unsafe("a" * 64))
    val bindings = SurfaceSceneBindings(Map(left -> reference, right -> reference), Map.empty)
    val provenance = SurfaceProvenance.make("paired-camera-test", "1", "2026-09-10").toOption.get
    val document = SurfaceSceneDocument.capture(model, state, bindings, provenance).toOption.get
    val encoded = SurfaceSceneCodec.encode(document)
    val decoded = SurfaceSceneCodec.decode(encoded).toOption.get
    assertEquals(decoded.restore(model, bindings), Right(state))
    assertEquals(SurfaceSceneCodec.encode(decoded), encoded)
    val incapable = SurfaceBackendCapabilities(SurfaceBackendId.unsafe("legacy"), SurfacePlanRevision.Current, Set.empty)
    assertEquals(decoded.admit(incapable).left.toOption,
      Some(SurfaceSceneError.MissingCapabilities(Vector(SurfaceBackendFeature.PerSurfaceCameras))))
    val old = SurfaceSceneDocument.make(SurfaceDocumentRevision.V6, document.assets, document.layers,
      state.layout, SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal), state.lighting, state.clipping, 0, None,
      document.layerStates, Set.empty, provenance).toOption.get
    val oldJson = SurfaceSceneCodec.encode(old)
    assert(!oldJson.contains("surfaceViewpoints"))
    assert(!oldJson.contains("aspectRatio"))
    val legacy = SurfaceSceneCodec.decode(oldJson).toOption.get
    assertEquals(SurfaceSceneCodec.encode(legacy), oldJson)
    assertEquals(legacy.restore(model, bindings).toOption.get.surfaceViewpoints, Map.empty[SurfaceId, SurfaceViewpoint])
    assert(SurfaceSceneCodec.decode(encoded.replace("\"surface\":\"right\"", "\"surface\":\"left\"")).isLeft)
    assert(SurfaceSceneCodec.decode(encoded.replace("\"surface\":\"right\"", "\"surface\":\"absent\"")).isLeft)
