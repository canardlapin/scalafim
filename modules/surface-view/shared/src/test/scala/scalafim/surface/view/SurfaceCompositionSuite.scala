package scalafim.surface.view

import scalafim.image.world.Spaces
import scalafim.surface.*

class SurfaceCompositionSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A =
    value.fold(error => fail(s"unexpected failure: $error"), identity)

  private val frame = Spaces.MNI152NLin2009cAsym

  test("typed composition reuses resources for one mesh in four views"):
    val model = SurfaceFaceFixture.model
    val state = SurfaceFaceFixture.state(model)
    val display = ok(SurfaceDisplayFrame.declare(model, SurfaceFaceFixture.Surface, frame))
    val pose = ok(SurfaceCompiler.cameraPose(model, state, display))
    val presentation = SurfaceCompositionPresentation.fromViewerState(state)
    val camera = state.camera
    val boxes = Vector(
      SurfaceViewport(0.0, 0.0, 0.5, 0.5),
      SurfaceViewport(0.5, 0.0, 0.5, 0.5),
      SurfaceViewport(0.0, 0.5, 0.5, 0.5),
      SurfaceViewport(0.5, 0.5, 0.5, 0.5)
    )
    val views = boxes.zipWithIndex.map: (box, index) =>
      SurfaceCompositionInputView(
        ok(SurfaceViewId.make(s"view-$index")),
        box,
        camera,
        pose,
        Vector(SurfaceCompositionInputInstance(
          ok(SurfaceInstanceId.make(s"instance-$index")),
          SurfaceFaceFixture.Surface,
          display
        )),
        state.lighting,
        state.clipping
      )
    val plan = ok(SurfaceCompositionCompiler.compile(model, presentation, views))
    assertEquals(plan.views.length, 4)
    assertEquals(plan.resources.meshes.size, 1)
    assertEquals(plan.resources.layers.size, 1)
    assertEquals(plan.views.map(_.instances.head.mesh).distinct.length, 1)
    assertEquals(plan.receipt.meshKeys, plan.resources.meshes.keys.toVector.sortBy(_.value))

  test("legacy lowering preserves each slot's camera, offsets, fit, and fragment metadata"):
    val old = SurfaceScalarFixture.plan().copy(viewportFit = SurfaceViewportFit.Contain(1.0))
    val lowered = ok(SurfaceCompositionPlan.fromLegacy(old))
    val view = lowered.views.head
    val slot = old.slots.head
    assertEquals(view.viewport, slot.viewport)
    assertEquals(view.camera, old.camera)
    assertEquals(view.instances.head.translationX, slot.worldOffsetX)
    assertEquals(view.viewportFit, SurfaceViewportFit.Contain(1.0))
    assertEquals(view.fragmentSurfaces, Set(SurfaceFaceFixture.Surface))

  test("composition plan rejects missing resources, invalid transforms, and empty input"):
    val old = SurfaceFaceFixture.plan
    val valid = ok(SurfaceCompositionPlan.fromLegacy(old))
    val view = valid.views.head
    val missing = view.instances.head.copy(mesh = SurfaceResourceKey("missing"))
    assertEquals(
      SurfaceCompositionPlan.make(Vector(view.copy(instances = Vector(missing))), valid.resources),
      Left(SurfaceCompositionError.MissingMesh(SurfaceResourceKey("missing")))
    )
    val invalidTranslation = view.instances.head.copy(translationX = Double.NaN)
    assertEquals(
      SurfaceCompositionPlan.make(Vector(view.copy(instances = Vector(invalidTranslation))), valid.resources),
      Left(SurfaceCompositionError.InvalidTranslation(invalidTranslation.id))
    )
    assertEquals(
      SurfaceCompositionPlan.make(Vector.empty, valid.resources),
      Left(SurfaceCompositionError.EmptyComposition)
    )

  test("composition rejects a stale display witness"):
    val original = SurfaceFaceFixture.model
    val state = SurfaceFaceFixture.state(original)
    val display = ok(SurfaceDisplayFrame.declare(original, SurfaceFaceFixture.Surface, frame))
    val pose = ok(SurfaceCompiler.cameraPose(original, state, display))
    val rebuilt = SurfaceFaceFixture.model
    val input = SurfaceCompositionInputView(
      ok(SurfaceViewId.make("stale")),
      SurfaceViewport(0.0, 0.0, 1.0, 1.0),
      state.camera,
      pose,
      Vector(SurfaceCompositionInputInstance(ok(SurfaceInstanceId.make("stale-instance")), SurfaceFaceFixture.Surface, display)),
      state.lighting,
      state.clipping
    )
    SurfaceCompositionCompiler.compile(rebuilt, SurfaceCompositionPresentation.fromViewerState(state), Vector(input)) match
      case Left(SurfaceCompositionError.Compilation(SurfaceCameraError.DisplayMismatch(SurfaceFaceFixture.Surface, _))) => ()
      case other => fail(s"expected stale display refusal, got $other")


  test("malformed packet buffers are rejected at the composition boundary"):
    val valid = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val key = valid.receipt.meshKeys.head
    val mesh = valid.resources.meshes(key)
    val malformed = mesh.copy(indices = new IntBufferView(Array(0, 1, 999)))
    assert(SurfaceCompositionPlan.make(valid.views,
      valid.resources.copy(meshes = valid.resources.meshes.updated(key, malformed))).isLeft)
    val badCamera = valid.views.head.camera.copy(directionX = Double.NaN)
    assertEquals(SurfaceCompositionPlan.make(Vector(valid.views.head.copy(camera = badCamera)), valid.resources),
      Left(SurfaceCompositionError.InvalidCamera(valid.views.head.id)))

  test("composition compiler rejects out-of-domain scientific selection"):
    val model = SurfaceFaceFixture.model
    val state = SurfaceFaceFixture.state(model)
    val display = ok(SurfaceDisplayFrame.declare(model, SurfaceFaceFixture.Surface, frame))
    val pose = ok(SurfaceCompiler.cameraPose(model, state, display))
    val view = SurfaceCompositionInputView(ok(SurfaceViewId.make("selection")), SurfaceViewport(0, 0, 1, 1),
      state.camera, pose, Vector(SurfaceCompositionInputInstance(ok(SurfaceInstanceId.make("instance")),
        SurfaceFaceFixture.Surface, display)), state.lighting, state.clipping)
    val presentation = SurfaceCompositionPresentation.fromViewerState(state)
    val invalid = presentation.copy(selection = Some(SurfaceSelection(SurfaceFaceFixture.Surface, VertexId(999))))
    assert(SurfaceCompositionCompiler.compile(model, invalid, Vector(view)).isLeft)

  test("composition refuses singular, nonrigid, and malformed camera packets"):
    val valid = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val view = valid.views.head
    val original = view.camera
    val invalid = Vector(
      original.copy(projectionMatrix = new FloatBufferView(Array.fill(16)(0f))),
      original.copy(viewMatrix = new FloatBufferView(Array.tabulate(16)(i => if i == 0 then 2f else original.viewMatrix(i)))),
      original.copy(viewMatrix = new FloatBufferView(Array.tabulate(16)(i => if i == 12 then 1f else original.viewMatrix(i)))),
      original.copy(projectionMatrix = new FloatBufferView(Array.tabulate(16)(i => if i == 10 then 0f else original.projectionMatrix(i)))))
    invalid.foreach(camera => assertEquals(SurfaceCompositionPlan.make(Vector(view.copy(camera = camera)), valid.resources),
      Left(SurfaceCompositionError.InvalidCamera(view.id))))

  test("immutable resource keys reject conflicting reuse before native interpretation"):
    val before = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val mesh = before.resources.meshes.values.head
    val moved = mesh.copy(positions = new FloatBufferView(Array.tabulate(mesh.positions.length)(i => mesh.positions(i) + 1f)))
    val changed = ok(SurfaceCompositionPlan.make(before.views, before.resources.copy(meshes = before.resources.meshes.updated(mesh.resourceKey, moved))))
    assertEquals(SurfaceCompositionPlan.validateReuse(before, changed), Left(SurfaceCompositionError.ConflictingMesh(mesh.resourceKey)))
    assertEquals(SurfaceCompositionPlan.validateReuse(before, before), Right(()))
    val layer = before.resources.layers.values.head
    val recolored = layer.copy(colors = new IntBufferView(Array.fill(layer.colors.length)(0xff00ffff)))
    val changedColors = ok(SurfaceCompositionPlan.make(before.views, before.resources.copy(layers = before.resources.layers.updated(layer.resourceKey, recolored))))
    assertEquals(SurfaceCompositionPlan.validateReuse(before, changedColors), Left(SurfaceCompositionError.ConflictingLayer(layer.resourceKey)))

  test("partition source indices are bounded before evaluators can index them"):
    val before = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val mesh = before.resources.meshes.values.head
    val triangle = SurfaceMappingTriangle(0, (Int.MaxValue, 0, 1), SurfaceFaceWeights(1, 0, 0),
      SurfaceFaceWeights(0, 1, 0), SurfaceFaceWeights(0, 0, 1))
    val invalid = mesh.copy(constantPartition = Some(Vector.fill(mesh.indices.length / 3)(triangle)))
    assert(SurfaceCompositionPlan.make(before.views, before.resources.copy(meshes = before.resources.meshes.updated(mesh.resourceKey, invalid))).isLeft)

  test("compiled geometry revision changes retain topology keys and pass native reuse checks"):
    val original = SurfaceFaceFixture.geometry
    val moved = SurfaceGeometry(TriangleMesh.fromRows(
      original.mesh.vertices.map(p => Seq(p.x * 1.2, p.y * 1.2, p.z)),
      original.mesh.faces.map(f => (f.a.index, f.b.index, f.c.index))), original.hemisphere, SurfaceKind.Pial)
    val asset = ok(SurfaceAsset.make(SurfaceFaceFixture.Surface,
      SurfaceSet.of(original.kind, original, SurfaceKind.Pial -> moved)))
    val model = ok(SurfaceViewerModel.make(Vector(asset), Vector.empty))
    val initial = SurfaceFaceFixture.state(model)
    val morphing = ok(SurfaceViewer.reduce(model, initial, SurfaceViewerAction.BeginGeometryMorph(SurfaceFaceFixture.Surface, SurfaceKind.Pial)))
    val halfway = ok(SurfaceViewer.reduce(model, morphing,
      SurfaceViewerAction.SetGeometryMorphFraction(SurfaceFaceFixture.Surface, SurfaceMorphFraction.unsafe(.5))))
    val a = ok(SurfaceCompositionPlan.fromLegacy(ok(SurfaceCompiler.compile(model, initial))))
    val b = ok(SurfaceCompositionPlan.fromLegacy(ok(SurfaceCompiler.compile(model, halfway))))
    assertEquals(a.receipt.meshKeys, b.receipt.meshKeys)
    assertEquals(SurfaceCompositionPlan.validateReuse(a, b), Right(()))

  test("face-sample provenance and infinite-far perspective projections are refused"):
    val base = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val mesh = base.resources.meshes.values.head
    val triangle = SurfaceMappingTriangle(Int.MaxValue, (0, 1, 2), SurfaceFaceWeights(1, 0, 0),
      SurfaceFaceWeights(0, 1, 0), SurfaceFaceWeights(0, 0, 1))
    val invalid = mesh.copy(constantPartition = Some(Vector.fill(mesh.indices.length / 3)(triangle)))
    assert(SurfaceCompositionPlan.make(base.views,
      base.resources.copy(meshes = base.resources.meshes.updated(mesh.resourceKey, invalid))).isLeft)
    val model = SurfaceFaceFixture.model
    val state = SurfaceFaceFixture.state(model)
    val packet = ok(SurfaceCompiler.compile(model, state.copy(camera = state.camera.copy(
      projection = CameraProjection.Perspective(FieldOfViewDegrees.Default))))).camera
    val projection = new FloatBufferView(Array.tabulate(16)(i => if i == 10 then -1f else packet.projectionMatrix(i)))
    assert(SurfaceCompositionPlan.make(Vector(base.views.head.copy(camera = packet.copy(projectionMatrix = projection))), base.resources).isLeft)
