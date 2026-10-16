package scalafim.surface.view

import scalafim.surface.*

class SurfacePairedCameraSuite extends munit.FunSuite:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")

  private def asset(id: SurfaceId, hemisphere: Hemisphere, x: Double): SurfaceAsset =
    val mesh = TriangleMesh.fromRows(
      Vector(Vector(x, -8.0, -3.0), Vector(x + 4.0, 11.0, -2.0),
        Vector(x + 1.0, -3.0, 9.0), Vector(x + 2.0, 2.0, 1.0)),
      Vector((0, 1, 2), (0, 3, 1), (1, 3, 2), (2, 3, 0))
    )
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

  test("paired lateral and medial cameras retain opposite directions and geometry"):
    val baseline = compile(paired(Map.empty))
    for (views, leftX) <- Vector(lateral -> -1.0, medial -> 1.0) do
      val plan = compile(paired(views))
      assertEqualsDouble(plan.cameraFor(left).directionX, leftX, 1e-12)
      assertEqualsDouble(plan.cameraFor(right).directionX, -leftX, 1e-12)
      assertEquals(plan.receipt.meshKeys, baseline.receipt.meshKeys)
      assertEquals(plan.meshes.map(_.positions.unsafeArray.toVector), baseline.meshes.map(_.positions.unsafeArray.toVector))
      assert(plan.validCameras)
      assertEquals(plan.profile.primitiveBytes - baseline.profile.primitiveBytes, 2L * 32L * 4L)

  test("shared navigation survives focus and reset while SetViewpoint clears overrides"):
    val moved = reduce(reduce(paired(), SurfaceViewerAction.OrbitBy(20, 13)), SurfaceViewerAction.SetZoom(CameraZoom.unsafe(1.7)))
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

  test("unknown overrides fail before mutation and fit uses each effective orientation"):
    val state = paired()
    val invalid = lateral.updated(SurfaceId.unsafe("missing"), SurfaceViewpoint.Dorsal)
    assert(SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetSurfaceViewpoints(invalid)).isLeft)
    assertEquals(state.surfaceViewpoints, lateral)
    assert(SurfaceCompiler.compile(model, state.copy(surfaceViewpoints = invalid)).isLeft)
    assert(SurfaceViewer.reduce(model, state.copy(surfaceViewpoints = invalid), SurfaceViewerAction.FitCamera).isLeft)
    for
      views <- Vector(lateral, medial)
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default),
        CameraProjection.Orthographic(OrthographicScale.unsafe(1.0)))
    do
      val fit = reduce(paired(views).copy(camera = paired(views).camera.copy(projection = projection,
        orbit = SurfaceOrbit.unsafe(31, -17))), SurfaceViewerAction.FitCamera)
      val plan = compile(fit)
      assert(plan.validCameras)
      for (width, height) <- Vector(900.0 -> 300.0, 300.0 -> 900.0) do
        plan.viewportFit.resolve(plan.slots, width, height).foreach: slot =>
          val p = plan.cameraFor(slot.surface).projectionMatrix
          assertEqualsDouble(width * slot.viewport.width * p(0), height * slot.viewport.height * p(5), 1e-3)

  test("camera identity is deterministic and legacy composition keeps effective cameras"):
    val first = compile(paired(lateral))
    val reordered = compile(paired(lateral.toVector.reverse.toMap))
    assertEquals(first.receipt.cameraKey, reordered.receipt.cameraKey)
    assertNotEquals(first.receipt.cameraKey, compile(paired(medial)).receipt.cameraKey)
    val composition = SurfaceCompositionPlan.fromLegacy(first).fold(error => fail(error.toString), identity)
    assertEquals(composition.views.map(_.camera), first.slots.map(slot => first.cameraFor(slot.surface)))

  test("an explicit typed pose remains authoritative over a single-surface override"):
    val base = SurfaceViewerState.initial(model)
    val display = SurfaceDisplayFrame.declare(model, left, SurfaceDisplayWorld.frame).toOption.get
    val pose = SurfaceCompiler.cameraPose(model, base, display).toOption.get
    val overridden = base.copy(surfaceViewpoints = Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left)))
    val expected = SurfaceCompiler.compile(model, base, display, pose).toOption.get
    val actual = SurfaceCompiler.compile(model, overridden, display, pose).toOption.get
    assertEquals(actual.surfaceCameras, Map.empty[SurfaceId, SurfaceCameraPacket])
    assertEquals(actual.camera.viewMatrix.unsafeArray.toVector, expected.camera.viewMatrix.unsafeArray.toVector)
