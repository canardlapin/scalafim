package scalafim.surface.view

import neuroim4s.spaces.Spaces

class SurfaceCompositionNavigationSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(error => fail(s"$error"), identity)
  private val frame = Spaces.MNI152NLin2009cAsym
  private val model = SurfaceFaceFixture.model
  private val state = SurfaceFaceFixture.state(model)
  private val display = ok(SurfaceDisplayFrame.declare(model, SurfaceFaceFixture.Surface, frame))
  private val pose = ok(SurfaceCompiler.cameraPose(model, state, display))
  private val left = SurfaceCompositionInputView(
    ok(SurfaceViewId.make("left")), SurfaceViewport(0, 0, .5, 1), state.camera, pose,
    Vector(SurfaceCompositionInputInstance(ok(SurfaceInstanceId.make("left-copy")), SurfaceFaceFixture.Surface, display)),
    state.lighting, state.clipping)
  private val right = left.copy(id = ok(SurfaceViewId.make("right")), viewport = SurfaceViewport(.5, 0, .5, 1),
    instances = Vector(left.instances.head.copy(id = ok(SurfaceInstanceId.make("right-copy")))))

  test("independent navigation changes only the selected view and never mesh coordinates"):
    val before = ok(SurfaceCompositionState.make(Vector(left, right)))
    val moved = ok(before.navigate(left.id, SurfaceCompositionNavigation.PanBy(2, -3)))
    assertEquals(moved.views(1), right)
    assertEqualsDouble(moved.views.head.pose.distance, pose.distance, 1e-10)
    assert(moved.views.head.pose.target.coordinates != pose.target.coordinates)
    val first = ok(SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), before.views))
    val next = ok(SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), moved.views))
    assertEquals(next.receipt.meshKeys, first.receipt.meshKeys)
    assertEquals(next.receipt.layerKeys, first.receipt.layerKeys)
    assertEquals(next.receipt.cameraKeys(1), first.receipt.cameraKeys(1))
    assert(next.receipt.cameraKeys.head != first.receipt.cameraKeys.head)

  test("linked orbit applies deltas while retaining distinct targets and distances"):
    val otherTarget = ok(SurfaceCameraPose.pointIn[frame.type](frame, pose.target.coordinates.updated(0, 12.0)))
    val otherPose = ok(pose.focusedOn(otherTarget))
    val other = right.copy(pose = otherPose)
    val before = ok(SurfaceCompositionState.make(Vector(left, other), Vector(Set(left.id, right.id))))
    val moved = ok(before.navigate(right.id, SurfaceCompositionNavigation.OrbitBy(21, -13)))
    assertEquals(moved.views.head.pose.target.coordinates, pose.target.coordinates)
    assertEquals(moved.views(1).pose.target.coordinates, otherTarget.coordinates)
    moved.views.foreach(view => assertEqualsDouble(view.pose.distance, pose.distance, 1e-10))
    assert(moved.views.head.pose.eye.coordinates != pose.eye.coordinates)
    assert(moved.views(1).pose.eye.coordinates != otherPose.eye.coordinates)

  test("zoom links magnification but a panel resize remains independent"):
    val before = ok(SurfaceCompositionState.make(Vector(left, right), Vector(Set(left.id, right.id))))
    val zoomed = ok(before.navigate(left.id, SurfaceCompositionNavigation.ZoomBy(2)))
    zoomed.views.foreach(view => assertEqualsDouble(view.camera.zoom.value, state.camera.zoom.value * 2, 1e-12))
    val resized = ok(zoomed.resize(left.id, CameraAspectRatio.unsafe(1.7)))
    assertEqualsDouble(resized.views.head.camera.aspectRatio.value, 1.7, 1e-12)
    assertEquals(resized.views(1).camera, zoomed.views(1).camera)

  test("invalid links and navigation are typed refusals without partial updates"):
    assert(SurfaceCompositionState.make(Vector(left, right), Vector(Set(left.id))).isLeft)
    assert(SurfaceCompositionState.make(Vector(left, right), Vector(Set(left.id, right.id), Set(left.id, right.id))).isLeft)
    val before = ok(SurfaceCompositionState.make(Vector(left, right)))
    assert(before.navigate(left.id, SurfaceCompositionNavigation.PanBy(Double.NaN, 0)).isLeft)
    assert(before.navigate(left.id, SurfaceCompositionNavigation.ZoomBy(0)).isLeft)
    assert(before.navigate(left.id, SurfaceCompositionNavigation.OrbitBy(0, Double.PositiveInfinity)).isLeft)
    assertEquals(before.views, Vector(left, right))

  test("perspective zoom changes the compiled view and halves eye distance"):
    val perspective = left.copy(camera = left.camera.copy(projection =
      CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35))))
    val before = ok(SurfaceCompositionState.make(Vector(perspective)))
    val after = ok(before.navigate(left.id, SurfaceCompositionNavigation.ZoomBy(2)))
    assertEqualsDouble(after.views.head.pose.distance, before.views.head.pose.distance / 2, 1e-10)
    val presentation = SurfaceCompositionPresentation.fromViewerState(state)
    val first = ok(SurfaceCompositionCompiler.compile(model, presentation, before.views))
    val next = ok(SurfaceCompositionCompiler.compile(model, presentation, after.views))
    assert(first.receipt.cameraKeys != next.receipt.cameraKeys)
    assertEquals(first.receipt.meshKeys, next.receipt.meshKeys)
