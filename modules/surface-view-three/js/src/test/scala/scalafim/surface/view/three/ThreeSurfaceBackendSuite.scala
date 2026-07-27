package scalafim.surface.view.three

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*

class ThreeSurfaceBackendSuite extends munit.FunSuite:
  private val surfaceId = SurfaceId.unsafe("left")
  private val layerId = SurfaceLayerId.unsafe("activation")
  private val size = ThreeCanvasSize.unsafe(640, 480, 2.0)

  test("Three adapter consumes CPU-projected fields and network tubes as ordinary resources"):
    val plan = SurfaceFeatureFixture.plan
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val receipt = backend.render(plan, size).toOption.get
    assertEquals(receipt.geometryUploads, 1)
    assertEquals(receipt.colorUploads, 1)
    assert(plan.meshes.head.indices.length / 3 > 4)
    assertEquals(runtime.uploadedColors.head.rgb.length, plan.meshes.head.positions.length)

  test("GPU projection capability is explicit and unsupported policies fail before WebGL interpretation"):
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    assert(!backend.capabilities.supports(SurfaceBackendFeature.GpuVolumeProjection))
    assert(backend.capabilities.caveats.exists(_.contains("shared CPU oracle")))

    val fixture = SurfaceFeatureFixture.projectionCase
    val unsupported = fixture.morphism.copy(plan = fixture.morphism.plan.copy(
      path = SurfaceSamplingPath.White
    ))
    val rejected = ThreeVolumeProjector.project(
      scala.scalajs.js.Dynamic.literal(),
      scala.scalajs.js.Dynamic.literal(),
      unsupported,
      fixture.volume,
      fixture.policy
    )
    assert(rejected.left.exists(_.message.contains("midpoint sampling")))

  test("publication preset configures an exact browser canvas size"):
    val publication = ThreeCanvasSize.publication(
      SurfacePublicationPreset.ManuscriptDoubleColumn,
      pixelRatio = 2.0
    ).toOption.get
    assertEquals(publication.width, 2400)
    assertEquals(publication.height, 1400)
    assertEqualsDouble(publication.pixelRatio, 2.0, 0.0)

  test("full-frame clearing disables the stale bilateral scissor before clearing"):
    val calls = ArrayBuffer.empty[String]
    val setClearColor: js.Function2[Int, Double, Unit] = (color, alpha) =>
      calls += s"color:$color:$alpha"
    val setScissorTest: js.Function1[Boolean, Unit] = enabled =>
      calls += s"scissor:$enabled"
    val clear: js.Function3[Boolean, Boolean, Boolean, Unit] = (color, depth, stencil) =>
      calls += s"clear:$color:$depth:$stencil"
    val renderer = js.Dynamic.literal(
      setClearColor = setClearColor,
      setScissorTest = setScissorTest,
      clear = clear
    )
    ThreeJsRuntime.clearFrame(renderer)
    assertEquals(
      calls.toVector,
      Vector("color:16777215:1", "scissor:false", "clear:true:true:true", "scissor:true")
    )

  private final class RecordingRuntime(var state: ThreeContextState = ThreeContextState.Available)
      extends ThreeSurfaceRuntime:
    val calls = ArrayBuffer.empty[String]
    var uploadedColors = Vector.empty[ThreeSurfaceColors]
    var nextPick: Option[ThreePick] = None

    def contextState: ThreeContextState = state

    def uploadGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long] =
      calls += s"geometry:${meshes.length}"
      Right(meshes.iterator.map(mesh => mesh.positions.length * 4L + mesh.normals.length * 4L + mesh.indices.length * 4L).sum)

    def updateGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long] =
      calls += s"geometry-update:${meshes.length}"
      Right(meshes.iterator.map(mesh => mesh.positions.length * 4L + mesh.normals.length * 4L).sum)

    def uploadColors(colors: Vector[ThreeSurfaceColors]): Either[ThreeSurfaceError, Long] =
      calls += s"colors:${colors.length}"
      uploadedColors = colors
      Right(colors.iterator.map(_.rgb.length * 4L).sum)

    def updateLighting(lighting: SurfaceLighting): Either[ThreeSurfaceError, Unit] =
      calls += "lighting"
      Right(())

    def updateCamera(camera: SurfaceCameraPacket, clipping: SurfaceClipping): Either[ThreeSurfaceError, Unit] =
      calls += "camera"
      Right(())

    def updateLayout(slots: Vector[SurfaceViewSlot]): Either[ThreeSurfaceError, Unit] =
      calls += s"layout:${slots.length}"
      Right(())

    def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit] =
      calls += s"resize:${size.width}x${size.height}"
      Right(())

    def draw(): Either[ThreeSurfaceError, Unit] =
      calls += "draw"
      Right(())

    def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]] =
      calls += "pick"
      Right(nextPick)

    def disposeResources(keys: Vector[SurfaceResourceKey]): Either[ThreeSurfaceError, Int] =
      calls += s"dispose:${keys.length}"
      Right(keys.length)

    def dispose(): Either[ThreeSurfaceError, Unit] =
      calls += "close"
      Right(())

  test("typed compiler separates geometry, color, camera, resize, and on-demand draw dirtiness"):
    val (viewer, initial) = fixture()
    val firstPlan = compile(viewer, initial)
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    assertEquals(backend.capabilities.id.value, "three-webgl")
    assert(backend.capabilities.supports(SurfaceBackendFeature.NativePicking))
    assert(!backend.capabilities.supports(SurfaceBackendFeature.WorldClipping))

    val firstObserved = backend.renderObserved(firstPlan, size, SurfaceAdmissionPath.ColdLoad).toOption.get
    val first = firstObserved.native
    assertEquals(first.geometryUploads, 1)
    assertEquals(first.colorUploads, 1)
    assertEquals(first.drawCalls, 1)
    assertEquals(runtime.calls.toVector, Vector(
      "geometry:1", "colors:1", "lighting", "camera", "layout:1", "resize:640x480", "draw"
    ))
    assertEquals(SurfaceBackendAdmission.validate(firstObserved.observation), Vector.empty)
    assertEquals(firstObserved.observation.geometryUploads, 1)
    assertEquals(firstObserved.observation.layerUploads, 1)
    assertEquals(firstObserved.observation.drawCalls, 1)

    runtime.calls.clear()
    val unchanged = backend.render(firstPlan, size).toOption.get
    assertEquals(unchanged.commandsApplied, 0)
    assertEquals(runtime.calls.toVector, Vector.empty)
    backend.render(firstPlan, size, forceDraw = true).toOption.get
    assertEquals(runtime.calls.toVector, Vector("draw"))

    runtime.calls.clear()
    val dorsal = SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)
    ).toOption.get
    val cameraObserved = backend.renderObserved(
      compile(viewer, dorsal), size, SurfaceAdmissionPath.CameraOnly
    ).toOption.get
    val cameraReceipt = cameraObserved.native
    assert(cameraReceipt.dirty.camera)
    assertEquals(cameraReceipt.geometryUploads, 0)
    assertEquals(cameraReceipt.colorUploads, 0)
    assertEquals(runtime.calls.toVector, Vector("camera", "draw"))
    assertEquals(SurfaceBackendAdmission.validate(cameraObserved.observation), Vector.empty)

    runtime.calls.clear()
    val thresholded = SurfaceViewer.reduce(
      viewer,
      dorsal,
      SurfaceViewerAction.SetLayerThreshold(
        layerId,
        DisplayThreshold.transparentBand(-0.5, 0.5).toOption.get
      )
    ).toOption.get
    val colorReceipt = backend.render(compile(viewer, thresholded), size).toOption.get
    assert(colorReceipt.dirty.layerData)
    assertEquals(colorReceipt.geometryUploads, 0)
    assertEquals(colorReceipt.colorUploads, 1)
    assertEquals(runtime.calls.toVector, Vector("dispose:1", "colors:1", "draw"))

    runtime.calls.clear()
    val resized = backend.render(compile(viewer, thresholded), ThreeCanvasSize.unsafe(800, 600)).toOption.get
    assert(resized.dirty.canvas)
    assertEquals(runtime.calls.toVector, Vector("resize:800x600", "draw"))

    val stats = backend.stats
    assertEquals(stats.geometryUploads, 1L)
    assertEquals(stats.colorUploads, 2L)
    assertEquals(stats.drawCalls, 5L)

  test("context absence, loss, disposal, and invalid picks are explicit failures"):
    assertEquals(
      ThreeSurfaceBackend.create(new RecordingRuntime(ThreeContextState.Absent)).left.toOption,
      Some(ThreeSurfaceError.ContextUnavailable)
    )
    assertEquals(
      ThreeSurfaceBackend.create(new RecordingRuntime(ThreeContextState.Lost)).left.toOption,
      Some(ThreeSurfaceError.ContextLost)
    )

    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val (viewer, initial) = fixture()
    backend.render(compile(viewer, initial), size).toOption.get
    runtime.state = ThreeContextState.Lost
    assertEquals(backend.render(compile(viewer, initial), size).left.toOption, Some(ThreeSurfaceError.ContextLost))
    runtime.state = ThreeContextState.Available
    assert(backend.pick(Double.NaN, 1.0).left.exists(_.message.contains("finite")))
    backend.dispose().toOption.get
    assertEquals(backend.render(compile(viewer, initial), size).left.toOption, Some(ThreeSurfaceError.BackendDisposed))
    assertEquals(backend.resourceKeys, Set.empty)
    assert(runtime.calls.contains("close"))

  test("topology-preserving morphs update only retained position and normal buffers"):
    val template = fixtureGeometry()
    val white = SurfaceGeometry(
      template.mesh,
      template.hemisphere,
      SurfaceKind.White,
      template.surfaceToWorld
    )
    val shiftedMesh = TriangleMesh.fromRows(
      template.mesh.vertices.map(point => Seq(point.x + 2.0, point.y, point.z)),
      template.mesh.faces.map(face => (face.a.index, face.b.index, face.c.index))
    )
    val pial = SurfaceGeometry(shiftedMesh, Hemisphere.Left, SurfaceKind.Pial, DMat.eye(4))
    val asset = SurfaceAsset.make(
      surfaceId,
      SurfaceSet.of(SurfaceKind.White, white, SurfaceKind.Pial -> pial)
    ).toOption.get
    val layer = SurfaceLayer.scalar(
      layerId,
      surfaceId,
      white,
      Array(-1.0, 0.0, 1.0, 2.0),
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 2.0))
    ).toOption.get
    val viewer = SurfaceViewerModel.make(Vector(asset), Vector(layer)).toOption.get
    val initial = SurfaceViewerState.initial(viewer)
    val started = SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.BeginGeometryMorph(surfaceId, SurfaceKind.Pial)
    ).toOption.get
    val halfway = SurfaceViewer.reduce(
      viewer,
      started,
      SurfaceViewerAction.SetGeometryMorphFraction(surfaceId, SurfaceMorphFraction.unsafe(0.5))
    ).toOption.get
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    backend.render(compile(viewer, initial), size).toOption.get
    runtime.calls.clear()

    val receipt = backend.render(compile(viewer, halfway), size).toOption.get
    assert(receipt.dirty.geometry)
    assertEquals(receipt.geometryUploads, 0)
    assertEquals(receipt.geometryUpdates, 1)
    assertEquals(receipt.colorUploads, 0)
    assertEquals(runtime.calls.toVector, Vector("geometry-update:1", "draw"))
    assertEquals(receipt.uploadedBytes, white.vertexCount.toLong * 3L * 2L * 4L)
    assertEquals(backend.stats.geometryUploads, 1L)
    assertEquals(backend.stats.geometryUpdates, 1L)

  test("world clipping is rejected explicitly before Three.js interpretation"):
    val (viewer, initial) = fixture()
    val clipping = SurfaceClipping.worldPlanes(Vector(
      WorldClipPlane.unsafe(1.0, 0.0, 0.0, 0.0)
    )).toOption.get
    val result = ThreeSurfaceProgram.compile(
      None,
      compile(viewer, initial).copy(clipping = clipping),
      size
    )
    assert(result.left.exists(_.message.contains("world clipping planes are unsupported")))

  test("picking preserves Three.js face, closest vertex, world point, and surface identity"):
    val runtime = new RecordingRuntime()
    val expected = ThreePick(surfaceId, 2, 3, 1.0, 2.0, 3.0)
    runtime.nextPick = Some(expected)
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val (viewer, initial) = fixture()
    backend.render(compile(viewer, initial), size).toOption.get
    assertEquals(backend.pick(320.0, 240.0).toOption.flatten, Some(expected))

  test("SurfViewJS scalar threshold fixture has the same semantic compositing"):
    val (viewer, initial) = fixture()
    val thresholded = SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetLayerThreshold(
        layerId,
        DisplayThreshold.transparentBand(-0.5, 0.5).toOption.get
      )
    ).toOption.get
    val program = ThreeSurfaceProgram.compile(None, compile(viewer, thresholded), size).toOption.get
    val colors = program.commands.collectFirst:
      case ThreeSurfaceCommand.UploadColors(values) => values.head.rgb
    .get

    // SurfViewJS ColorMap hides values inside the central threshold band. Its
    // semantic oracle for [-1, 0, 1, 2] is black, cortical base, 2/3 gray,
    // white. ScalaFIM interpolates rather than quantizing the LUT index.
    val expected = Vector(
      0.0f, 0.0f, 0.0f,
      184.0f / 255.0f, 184.0f / 255.0f, 184.0f / 255.0f,
      170.0f / 255.0f, 170.0f / 255.0f, 170.0f / 255.0f,
      1.0f, 1.0f, 1.0f
    )
    assertEquals(colors.toVector, expected)

  test("SurfViewJS label fixture preserves exact categorical colors"):
    val geometry = fixtureGeometry()
    val asset = SurfaceAsset.make(surfaceId, geometry).toOption.get
    val atlasId = SurfaceLayerId.unsafe("atlas")
    val atlas = SurfaceLayer.labels(
      atlasId,
      surfaceId,
      geometry,
      Array(1, 2, 1, 0),
      LabelColorizer(Map(
        0 -> Rgba32.unsafe(0, 0, 0, 0),
        1 -> Rgba32.unsafe(230, 40, 60),
        2 -> Rgba32.unsafe(30, 120, 240)
      ))
    ).toOption.get
    val viewer = SurfaceViewerModel.make(Vector(asset), Vector(atlas)).toOption.get
    val colors = compiledColors(viewer, SurfaceViewerState.initial(viewer))

    assertEquals(colors.toVector, Vector(
      230.0f / 255.0f, 40.0f / 255.0f, 60.0f / 255.0f,
      30.0f / 255.0f, 120.0f / 255.0f, 240.0f / 255.0f,
      230.0f / 255.0f, 40.0f / 255.0f, 60.0f / 255.0f,
      184.0f / 255.0f, 184.0f / 255.0f, 184.0f / 255.0f
    ))

  test("SurfViewJS multilayer fixture preserves order, opacity, and blend mode"):
    val geometry = fixtureGeometry()
    val asset = SurfaceAsset.make(surfaceId, geometry).toOption.get
    val redId = SurfaceLayerId.unsafe("red")
    val blueId = SurfaceLayerId.unsafe("blue")
    val red = SurfaceLayer.packedRgba(
      redId,
      surfaceId,
      geometry,
      Vector.fill(geometry.vertexCount)(Rgba32.unsafe(255, 0, 0)),
      opacity = DisplayOpacity.unsafe(0.5)
    ).toOption.get
    val blue = SurfaceLayer.packedRgba(
      blueId,
      surfaceId,
      geometry,
      Vector.fill(geometry.vertexCount)(Rgba32.unsafe(0, 0, 255)),
      opacity = DisplayOpacity.unsafe(0.25),
      blendMode = DisplayBlendMode.Additive
    ).toOption.get
    val viewer = SurfaceViewerModel.make(Vector(asset), Vector(red, blue)).toOption.get
    val colors = compiledColors(viewer, SurfaceViewerState.initial(viewer))

    val oneVertex = Vector(220.0f / 255.0f, 92.0f / 255.0f, 133.0f / 255.0f)
    assertEquals(colors.toVector, Vector.fill(geometry.vertexCount)(oneVertex).flatten)

  private def fixture(): (SurfaceViewerModel, SurfaceViewerState) =
    val geometry = fixtureGeometry()
    val asset = SurfaceAsset.make(surfaceId, geometry).toOption.get
    val layer = SurfaceLayer.scalar(
      layerId,
      surfaceId,
      geometry,
      Array(-1.0, 0.0, 1.0, 2.0),
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 2.0))
    ).toOption.get
    val viewer = SurfaceViewerModel.make(Vector(asset), Vector(layer)).toOption.get
    (viewer, SurfaceViewerState.initial(viewer))

  private def fixtureGeometry(): SurfaceGeometry =
    val mesh = TriangleMesh.fromRows(
      Seq(
        Seq(0.0, 0.0, 0.0),
        Seq(1.0, 0.0, 0.0),
        Seq(0.0, 1.0, 0.0),
        Seq(0.0, 0.0, 1.0)
      ),
      Seq((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
    )
    SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Inflated, DMat.eye(4))

  private def compiledColors(viewer: SurfaceViewerModel, state: SurfaceViewerState): Array[Float] =
    val program = ThreeSurfaceProgram.compile(None, compile(viewer, state), size).toOption.get
    program.commands.collectFirst:
      case ThreeSurfaceCommand.UploadColors(values) => values.head.rgb
    .get

  private def compile(viewer: SurfaceViewerModel, state: SurfaceViewerState): SurfaceRenderPlan =
    SurfaceCompiler.compile(viewer, state).toOption.get
