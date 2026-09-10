package scalafim.surface.view.three

import scalafim.surface.*
import scalafim.surface.view.*

class ThreePairedCameraSuite extends munit.FunSuite:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")
  private def asset(id: SurfaceId, hemisphere: Hemisphere, sign: Double): SurfaceAsset =
    val positions = Vector(3.0, 1.0).flatMap(x =>
      Vector(Vector(sign * x, -2.0, -1.0), Vector(sign * x, 3.0, -1.0), Vector(sign * x, -1.0, 4.0)))
    val faces = if sign < 0 then Vector((0, 2, 1), (3, 4, 5)) else Vector((0, 1, 2), (3, 5, 4))
    SurfaceAsset.make(id, SurfaceGeometry(TriangleMesh.fromRows(positions, faces), hemisphere, SurfaceKind.Inflated)).toOption.get
  private val model = SurfaceViewerModel.make(Vector(asset(left, Hemisphere.Left, -1), asset(right, Hemisphere.Right, 1)), Vector.empty).toOption.get
  private def state(medial: Boolean = false): SurfaceViewerState =
    val initial = SurfaceViewerState.initial(model)
    initial.copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = if medial then Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left), right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right))
        else Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left), right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right)))
  private def plan(value: SurfaceViewerState): SurfaceRenderPlan = SurfaceCompiler.compile(model, value).toOption.get

  private val size = ThreeCanvasSize.unsafe(600, 300)
  test("initial and incremental commands carry both surface cameras before drawing"):
    val before = plan(state())
    val initial = ThreeSurfaceProgram.compile(None, before, size).toOption.get
    val cameraIndex = initial.commands.indexWhere(_.isInstanceOf[ThreeSurfaceCommand.UpdateSurfaceCameras])
    assert(cameraIndex >= 0)
    assert(cameraIndex < initial.commands.indexOf(ThreeSurfaceCommand.Draw))
    val after = plan(state(true))
    val update = ThreeSurfaceProgram.compile(Some(before -> size), after, size).toOption.get
    assert(update.dirty.camera)
    assert(!update.dirty.geometry && !update.dirty.layerData && !update.dirty.material)
    assert(update.commands.exists:
      case ThreeSurfaceCommand.UpdateSurfaceCameras(cameras) => cameras.keySet == Set(left, right)
      case _ => false)
    assert(ThreeSurfaceProgram.compile(Some(after -> size), plan(state(true)), size).toOption.get.dirty.isClean)

  test("returning to a global camera explicitly clears the runtime overrides"):
    val before = plan(state())
    val after = plan(state().copy(surfaceViewpoints = Map.empty))
    val update = ThreeSurfaceProgram.compile(Some(before -> size), after, size).toOption.get
    assert(update.commands.contains(ThreeSurfaceCommand.UpdateSurfaceCameras(Map.empty)))

  test("mismatched projection and hidden camera identities are refused before commands"):
    val original = plan(state())
    val other = original.camera.copy(projectionMatrix = new FloatBufferView(Array.fill(16)(0.0f)))
    assert(ThreeSurfaceProgram.compile(None, original.copy(surfaceCameras = Map(right -> other)), size).isLeft)
    assert(ThreeSurfaceProgram.compile(None, original.copy(surfaceCameras = Map(SurfaceId.unsafe("absent") -> original.camera)), size).isLeft)

  test("legacy runtimes refuse paired cameras before mutation and preserve the prior rendered state"):
    val runtime = new LegacyRuntime
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val global = plan(state().copy(surfaceViewpoints = Map.empty))
    assert(backend.render(global, size).isRight)
    val before = runtime.mutations
    assert(!backend.capabilities.supports(SurfaceBackendFeature.PerSurfaceCameras))
    assert(backend.render(plan(state()), size).left.exists(_.message.contains("per-surface cameras")))
    assertEquals(runtime.mutations, before)
    assert(backend.render(global, size).toOption.get.dirty.isClean)

  private class LegacyRuntime extends ThreeSurfaceRuntime:
    var mutations = 0
    def contextState: ThreeContextState = ThreeContextState.Available
    private def changed[A](value: A): Either[ThreeSurfaceError, A] =
      mutations += 1
      Right(value)
    def uploadGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long] = changed(0L)
    def updateGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long] = changed(0L)
    def uploadColors(colors: Vector[ThreeSurfaceColors]): Either[ThreeSurfaceError, Long] = changed(0L)
    def updateLighting(lighting: SurfaceLighting): Either[ThreeSurfaceError, Unit] = changed(())
    def updateCamera(camera: SurfaceCameraPacket, clipping: SurfaceClipping): Either[ThreeSurfaceError, Unit] = changed(())
    def updateLayout(slots: Vector[SurfaceViewSlot], fit: SurfaceViewportFit): Either[ThreeSurfaceError, Unit] = changed(())
    def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit] = changed(())
    def draw(): Either[ThreeSurfaceError, Unit] = changed(())
    def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]] = Right(None)
    def disposeResources(keys: Vector[SurfaceResourceKey]): Either[ThreeSurfaceError, Int] = changed(keys.size)
    def dispose(): Either[ThreeSurfaceError, Unit] = changed(())
