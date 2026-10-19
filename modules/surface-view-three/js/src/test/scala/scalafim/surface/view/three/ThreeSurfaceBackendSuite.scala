package scalafim.surface.view.three

import image4s.geometry.Affine
import image4s.geometry.D3
import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class ThreeSurfaceBackendSuite extends munit.FunSuite:
  private val surfaceId = SurfaceId.unsafe("left")
  private val layerId = SurfaceLayerId.unsafe("activation")
  private val size = ThreeCanvasSize.unsafe(640, 480, 2.0)

  test("native depth fixture covers its full pixel and strictly separates near and far depth"):
    val points = ThreeDepthAdmission.Positions.grouped(3).toVector
    val near = points.take(3).map(p => ((p(0).toFloat.toDouble + 1) * 4, (1 - p(1).toFloat.toDouble) * 4))
    def area(a: (Double, Double), b: (Double, Double), p: (Double, Double)): Double =
      (b._1 - a._1) * (p._2 - a._2) - (b._2 - a._2) * (p._1 - a._1)
    val total = area(near(0), near(1), near(2))
    for x <- Vector(3.0, 4.0); y <- Vector(3.0, 4.0); edge <- 0 until 3 do
      assert(area(near(edge), near((edge + 1) % 3), (x, y)) / total > 0.01)
    assert(points.take(3).map(_(2)).max + 0.01 < points.drop(3).map(_(2)).min)
    assert(ThreeDepthAdmission.admit(4, 255, 0, 0).isRight)
    assert(ThreeDepthAdmission.admit(4, 192, 64, 0).isLeft)
    assert(ThreeDepthAdmission.admit(0, 255, 255, 255).isLeft)
    assert(ThreeDepthAdmission.admit(4, 0, 255, 0).isLeft)

  test("fractional viewport projection matches logical pixel positions at native device ratios"):
    val viewport = SurfaceViewport(0.0731, 0.13728036703084256, 0.8317, 0.7254392659383149)
    for ratio <- Vector(1.0, 1.5, 2.0); dimensions <- Vector((768, 768), (800, 600)); perspective <- Vector(false, true) do
      val (width, height) = dimensions
      val size = ThreeCanvasSize.unsafe(width, height, ratio)
      val native = ThreeJsRuntime.nativeViewport(viewport, size)
      // Column-major matrices with an explicit nonconstant homogeneous w in
      // the perspective case. Expected screen positions use the unrounded slot.
      val original = js.Array[Double](0.7, 0, 0, 0, 0, 1.1, 0, 0,
        0, 0, -0.2, if perspective then -1 else 0, 0, 0, -0.1, if perspective then 0 else 1)
      val corrected = ThreeJsRuntime.viewportProjection(original, native)
      def multiply(matrix: js.Array[Double], point: Vector[Double]): Vector[Double] =
        Vector.tabulate(4)(row => (0 until 4).map(column => matrix(column * 4 + row) * point(column)).sum)
      for point <- Vector(Vector(-0.3, 0.2, -2.0, 1.0), Vector(0.4, -0.5, -3.0, 1.0)) do
        val clip = multiply(original, point)
        val adjusted = multiply(corrected, point)
        val expectedX = (viewport.x + (clip(0) / clip(3) + 1) * viewport.width / 2) * width * ratio
        val expectedY = (1 - viewport.y - viewport.height + (clip(1) / clip(3) + 1) * viewport.height / 2) * height * ratio
        assertEqualsDouble(native.x + (adjusted(0) / adjusted(3) + 1) * native.width / 2, expectedX, 1e-10)
        assertEqualsDouble(native.y + (adjusted(1) / adjusted(3) + 1) * native.height / 2, expectedY, 1e-10)
        assertEqualsDouble(adjusted(2), clip(2), 0.0)
        assertEqualsDouble(adjusted(3), clip(3), 0.0)


  private def depthCamera(perspective: Boolean): js.Array[Double] =
    val near = 0.01
    val far = 1000.0
    js.Array[Double](1, 0, 0, 0, 0, 1, 0, 0,
      0, 0, if perspective then -(far + near) / (far - near) else -2 / (far - near),
      if perspective then -1 else 0,
      0, 0, if perspective then -2 * far * near / (far - near) else -(far + near) / (far - near),
      if perspective then 0 else 1)

  private val identityMatrix = js.Array[Double](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1)

  private def projectedDepth(matrix: js.Array[Double], z: Double): Double =
    (matrix(10) * z + matrix(14)) / (matrix(11) * z + matrix(15))

  test("explicit depth preservation keeps supplied coefficients for every occupied interval"):
    for perspective <- Vector(false, true) do
      val projection = depthCamera(perspective)
      for objects <- Vector(Vector.empty, Vector((identityMatrix, ThreeJsRuntime.PositionBounds(-1, -1, -200, 1, 1, -100))),
          Vector((identityMatrix, ThreeJsRuntime.PositionBounds(-1, -1, -1, 1, 1, 1))),
          Vector((identityMatrix, ThreeJsRuntime.PositionBounds(-1, -1, -1100, 1, 1, -.005)))) do
        val actual = ThreeJsRuntime.depthProjection(projection, objects, ThreeDepthPrecision.PreserveDepthProjection)
        assertEquals(actual.toVector, projection.toVector)

  test("occupied depth conditioning preserves screen coordinates and separates close perspective surfaces"):
    val bounds = ThreeJsRuntime.PositionBounds(-2, -3, -200, 2, 3, -100)
    for perspective <- Vector(false, true) do
      val original = depthCamera(perspective)
      val adjusted = ThreeJsRuntime.depthProjection(original, identityMatrix, bounds)
      for column <- 0 until 4; row <- Vector(0, 1, 3) do
        assertEqualsDouble(adjusted(column * 4 + row), original(column * 4 + row), 0)
      val depths = Vector(-100.0, -125.0, -150.0, -175.0, -200.0).map(projectedDepth(adjusted, _))
      assert(depths.forall(d => d > -1 && d < 1))
      assert(depths.sliding(2).forall(pair => pair(0) < pair(1)))
      assert(depths.last - depths.head > 1.0)
      if perspective then
        val oldSeparation = projectedDepth(original, -150.001) - projectedDepth(original, -150.0)
        val newSeparation = projectedDepth(adjusted, -150.001) - projectedDepth(adjusted, -150.0)
        // A 24-bit depth buffer maps NDC [-1,1] onto 2^24 discrete depths.
        assert(oldSeparation / 2 * (1 << 24) < 0.01)
        assert(newSeparation / 2 * (1 << 24) > 10)

  test("occupied depth conditioning retains reached clip planes and rejects uncertain eye crossings"):
    for perspective <- Vector(false, true) do
      val original = depthCamera(perspective)
      for (minZ, maxZ, boundary, outside) <- Vector(
          (-0.02, -0.005, -0.01, -0.009), (-1100.0, -900.0, -1000.0, -1001.0)) do
        val adjusted = ThreeJsRuntime.depthProjection(original, identityMatrix,
          ThreeJsRuntime.PositionBounds(-1, -1, minZ, 1, 1, maxZ))
        assertEqualsDouble(projectedDepth(adjusted, boundary), if boundary == -0.01 then -1.0 else 1.0, 1e-8)
        assert(math.abs(projectedDepth(adjusted, outside)) > 1.0)
      val clipped = ThreeJsRuntime.depthProjection(original, identityMatrix,
        ThreeJsRuntime.PositionBounds(-1, -1, -2000, 1, 1, -1500))
      for z <- Vector(-2000.0, -1750.0, -1500.0) do
        assert(projectedDepth(clipped, z) > 1.0)
    val perspective = depthCamera(true)
    for bounds <- Vector(
        ThreeJsRuntime.PositionBounds(-1, -1, -1, 1, 1, 1),
        ThreeJsRuntime.PositionBounds(-1, -1, 1, 1, 1, 2),
        ThreeJsRuntime.PositionBounds(-1, -1, Double.NaN, 1, 1, -1)) do
      assertEquals(ThreeJsRuntime.depthProjection(perspective, identityMatrix, bounds).toVector, perspective.toVector)

  test("depth conditioning uses a common interval for translated and overlapping draw slots"):
    val original = depthCamera(true)
    val shifted = identityMatrix.slice(0, 16)
    shifted(14) = -100
    val box = ThreeJsRuntime.PositionBounds(-1, -1, -101, 1, 1, -100)
    val objects = Vector((identityMatrix, box), (shifted, box))
    val adjusted = ThreeJsRuntime.depthProjection(original, objects)
    assertEquals(adjusted.toVector, ThreeJsRuntime.depthProjection(original, objects.reverse).toVector)
    val depths = Vector(-100.0, -101.0, -200.0, -201.0).map(projectedDepth(adjusted, _))
    assert(depths.forall(d => d > -1 && d < 1))
    assert(depths.sliding(2).forall(pair => pair(0) < pair(1)))

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

  test("typed runtime clear colour survives resize and draw cycles"):
    val observed = ArrayBuffer.empty[(Int, Double)]
    val setClearColor: js.Function2[Int, Double, Unit] = (color, alpha) =>
      observed += color -> alpha
    val noOpScissor: js.Function1[Boolean, Unit] = _ => ()
    val noOpClear: js.Function3[Boolean, Boolean, Boolean, Unit] = (_, _, _) => ()
    val isContextLost: js.Function0[Boolean] = () => false
    val webgl = js.Dynamic.literal(isContextLost = isContextLost)
    val renderer = js.Dynamic.literal(
      setClearColor = setClearColor,
      setScissorTest = noOpScissor,
      clear = noOpClear,
      getContext = (() => webgl): js.Function0[js.Any],
      setPixelRatio = ((_: Double) => ()): js.Function1[Double, Unit],
      setSize = ((_: Int, _: Int, _: Boolean) => ()): js.Function3[Int, Int, Boolean, Unit]
    )
    val three = FakeThree.runtime(renderer, webgl)
    val options = ThreeJsRuntimeOptions(Rgba32.unsafe(18, 24, 32, 128))
    val runtime = ThreeJsRuntime.create(three, js.Dynamic.literal(), options).toOption.get
    // Construction performs two native-depth draws before the viewer owns its frame.
    assertEquals(observed.map(_._1).toVector, Vector(0xffffff, 0xffffff))
    observed.clear()

    assertEquals(runtime.resize(ThreeCanvasSize.unsafe(320, 240)), Right(()))
    assertEquals(runtime.draw(), Right(()))
    assertEquals(runtime.resize(ThreeCanvasSize.unsafe(640, 480, 2.0)), Right(()))
    assertEquals(runtime.draw(), Right(()))

    assertEquals(observed.map(_._1).toVector, Vector(0x121820, 0x121820))
    observed.foreach((_, alpha) => assertEqualsDouble(alpha, 128.0 / 255.0, 0.0))

  test("disposal forces WebGL context loss exactly once and reports the context as lost"):
    var loseContextCalls = 0
    var rendererDisposals = 0
    val loseContext: js.Function0[Unit] = () => loseContextCalls += 1
    val extension = js.Dynamic.literal(loseContext = loseContext)
    val isContextLost: js.Function0[Boolean] = () => false
    val getExtension: js.Function1[String, js.Any] = name =>
      if name == "WEBGL_lose_context" then extension else null
    val webgl = js.Dynamic.literal(isContextLost = isContextLost, getExtension = getExtension)
    val getContext: js.Function1[String, js.Any] = kind =>
      if kind == "webgl2" then js.undefined else if kind == "webgl" then webgl else null
    val canvas = js.Dynamic.literal(getContext = getContext)
    val renderer = js.Dynamic.literal(
      getContext = (() => webgl): js.Function0[js.Any],
      dispose = (() => rendererDisposals += 1): js.Function0[Unit]
    )
    val three = FakeThree.runtime(renderer, webgl)
    val runtime = ThreeJsRuntime.create(three, canvas).toOption.get
    assertEquals(runtime.contextState, ThreeContextState.Available)
    assertEquals(runtime.dispose(), Right(()))
    assertEquals(runtime.dispose(), Right(()))
    assertEquals(loseContextCalls, 1)
    assertEquals(rendererDisposals, 1)
    assertEquals(runtime.contextState, ThreeContextState.Lost)

  test("rejected depth admission releases its probe resources, renderer, and context exactly once"):
    var targetDisposals = 0
    var geometryDisposals = 0
    var materialDisposals = 0
    var rendererDisposals = 0
    var contextLosses = 0
    val webgl = lifecycleWebgl(() => contextLosses += 1)
    val canvas = lifecycleCanvas(webgl)
    val renderer = js.Dynamic.literal(
      getContext = (() => webgl): js.Function0[js.Any],
      dispose = (() => rendererDisposals += 1): js.Function0[Unit]
    )
    val three = FakeThree.runtime(renderer, webgl, Map(
      "WebGLRenderTarget" -> disposable(() => targetDisposals += 1),
      "BufferGeometry" -> geometry(() => geometryDisposals += 1),
      "ShaderMaterial" -> disposable(() => materialDisposals += 1)
    ))
    renderer.updateDynamic("readRenderTargetPixels")(((target: js.Any, x: Int, y: Int, width: Int, height: Int,
        pixel: scala.scalajs.js.typedarray.Uint8Array) =>
      pixel(0) = 0; pixel(1) = 255; pixel(2) = 0; pixel(3) = 255
    ): js.Function6[js.Any, Int, Int, Int, Int, scala.scalajs.js.typedarray.Uint8Array, Unit])

    assert(ThreeJsRuntime.create(three, canvas).left.exists(_.isInstanceOf[ThreeSurfaceError.NativeDepthMismatch]))
    assertEquals(targetDisposals, 1)
    assertEquals(geometryDisposals, 1)
    assertEquals(materialDisposals, 1)
    assertEquals(rendererDisposals, 1)
    assertEquals(contextLosses, 1)

  test("depth probe constructor failure releases already-built resources, renderer, and context exactly once"):
    var targetDisposals = 0
    var geometryDisposals = 0
    var materialDisposals = 0
    var rendererDisposals = 0
    var contextLosses = 0
    val webgl = lifecycleWebgl(() => contextLosses += 1)
    val canvas = lifecycleCanvas(webgl)
    val renderer = js.Dynamic.literal(
      getContext = (() => webgl): js.Function0[js.Any],
      dispose = (() => rendererDisposals += 1): js.Function0[Unit]
    )
    val three = FakeThree.runtime(renderer, webgl, Map(
      "WebGLRenderTarget" -> disposable(() => targetDisposals += 1),
      "BufferGeometry" -> geometry(() => geometryDisposals += 1)
    ), Map("ShaderMaterial" -> FakeThree.throwing("probe material failure")))

    assert(ThreeJsRuntime.create(three, canvas).left.exists(_.isInstanceOf[ThreeSurfaceError.RuntimeFailure]))
    assertEquals(targetDisposals, 1)
    assertEquals(geometryDisposals, 1)
    assertEquals(materialDisposals, 0)
    assertEquals(rendererDisposals, 1)
    assertEquals(contextLosses, 1)

  private def lifecycleWebgl(onLoseContext: () => Unit): js.Dynamic =
    val extension = js.Dynamic.literal(loseContext = (() => onLoseContext()): js.Function0[Unit])
    js.Dynamic.literal(
      isContextLost = (() => false): js.Function0[Boolean],
      getExtension = ((name: String) => if name == "WEBGL_lose_context" then extension else null): js.Function1[String, js.Any]
    )

  private def lifecycleCanvas(webgl: js.Dynamic): js.Dynamic =
    js.Dynamic.literal(getContext = ((kind: String) => if kind == "webgl2" then js.undefined else webgl): js.Function1[String, js.Any])

  private def disposable(onDispose: () => Unit): js.Dynamic =
    js.Dynamic.literal(dispose = (() => onDispose()): js.Function0[Unit])

  private def geometry(onDispose: () => Unit): js.Dynamic =
    val value = disposable(onDispose)
    value.updateDynamic("setAttribute")(((_: String, _: js.Any) => ()): js.Function2[String, js.Any, Unit])
    value.updateDynamic("setIndex")(((_: js.Any) => ()): js.Function1[js.Any, Unit])
    value

  private object FakeThree:
    /** Lifecycle-only mock. Native depth correctness is checked separately by
      * ThreeDepthAdmission's analytic fixture and actual browser captures.
      */
    def runtime(renderer: js.Dynamic, webgl: js.Dynamic,
        constructors: Map[String, js.Dynamic] = Map.empty,
        directConstructors: Map[String, js.Dynamic] = Map.empty): js.Dynamic =
      webgl.updateDynamic("SAMPLES")(0)
      webgl.updateDynamic("getParameter")(((_: js.Any) => 0): js.Function1[js.Any, Int])
      renderer.updateDynamic("getRenderTarget")((() => null): js.Function0[js.Any])
      renderer.updateDynamic("setRenderTarget")(((_: js.Any) => ()): js.Function1[js.Any, Unit])
      renderer.updateDynamic("render")(((_: js.Any, _: js.Any) => ()): js.Function2[js.Any, js.Any, Unit])
      renderer.updateDynamic("readRenderTargetPixels")(((target: js.Any, x: Int, y: Int, width: Int, height: Int,
          pixel: scala.scalajs.js.typedarray.Uint8Array) =>
        pixel(0) = 255; pixel(1) = 0; pixel(2) = 0; pixel(3) = 255
      ): js.Function6[js.Any, Int, Int, Int, Int, scala.scalajs.js.typedarray.Uint8Array, Unit])
      if js.isUndefined(renderer.selectDynamic("setClearColor")) then
        renderer.updateDynamic("setClearColor")(((_: Int, _: Double) => ()): js.Function2[Int, Double, Unit])
      if js.isUndefined(renderer.selectDynamic("clear")) then
        renderer.updateDynamic("clear")(((_: Boolean, _: Boolean, _: Boolean) => ()): js.Function3[Boolean, Boolean, Boolean, Unit])
      val disposable = js.Dynamic.literal(dispose = (() => ()): js.Function0[Unit])
      val geometry = js.Dynamic.literal(
        setAttribute = ((_: String, _: js.Any) => ()): js.Function2[String, js.Any, Unit],
        setIndex = ((_: js.Any) => ()): js.Function1[js.Any, Unit],
        dispose = (() => ()): js.Function0[Unit])
      val projection = js.Dynamic.literal(elements = js.Array[Double](1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1))
      projection.updateDynamic("clone")((() => projection): js.Function0[js.Any])
      projection.updateDynamic("copy")(((_: js.Any) => projection): js.Function1[js.Any, js.Any])
      def constructor(name: String, fallback: js.Dynamic): js.Dynamic =
        directConstructors.getOrElse(name, constructing(constructors.getOrElse(name, fallback)))
      js.Dynamic.literal(
        WebGLRenderer = constructor("WebGLRenderer", renderer),
        WebGLRenderTarget = constructor("WebGLRenderTarget", disposable),
        BufferGeometry = constructor("BufferGeometry", geometry),
        BufferAttribute = constructing(js.Dynamic.literal()),
        ShaderMaterial = constructor("ShaderMaterial", disposable),
        Mesh = constructing(js.Dynamic.literal()),
        Camera = constructing(js.Dynamic.literal()),
        Scene = constructing(js.Dynamic.literal(add = ((_: js.Any) => ()): js.Function1[js.Any, Unit])),
        PerspectiveCamera = constructing(js.Dynamic.literal(projectionMatrix = projection)),
        Raycaster = constructing(js.Dynamic.literal()))
    /** A `new`-able JavaScript constructor whose instances are `instance`. */
    def constructing(instance: js.Dynamic): js.Dynamic =
      val factory: js.Function0[js.Any] = () => instance
      js.Dynamic.newInstance(js.Dynamic.global.Function)(
        "factory",
        "return function() { return factory(); };"
      ).applyDynamic("call")(null, factory)

    def throwing(message: String): js.Dynamic =
      val factory: js.Function0[js.Any] = () => throw new RuntimeException(message)
      js.Dynamic.newInstance(js.Dynamic.global.Function)(
        "factory",
        "return function() { return factory(); };"
      ).applyDynamic("call")(null, factory)

  private final class RecordingRuntime(var state: ThreeContextState = ThreeContextState.Available)
      extends ThreeSurfaceRuntime:
    val calls = ArrayBuffer.empty[String]
    var uploadedColors = Vector.empty[ThreeSurfaceColors]
    var fitted: SurfaceViewportFit = SurfaceViewportFit.Fill
    var nextPick: Option[ThreePick] = None
    var nextDisplayPick: Option[SurfaceDisplayHit[ThreePick]] = None
    var decorationFailure = false
    var clearFailure = false
    var drawFailure = false

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

    def updateLayout(slots: Vector[SurfaceViewSlot], fit: SurfaceViewportFit): Either[ThreeSurfaceError, Unit] =
      fitted = fit
      calls += s"layout:${slots.length}"
      Right(())

    override def updateComposition(plan: SurfaceCompositionPlan): Either[ThreeSurfaceError, Unit] =
      calls += s"composition:${plan.receipt.instanceCount}"
      Right(())

    override def clearComposition(): Either[ThreeSurfaceError, Unit] =
      calls += "composition-clear"
      if clearFailure then Left(ThreeSurfaceError.RuntimeFailure("composition clear", "injected")) else Right(())

    override def updateDecorations(geometry: SurfaceDecorationGeometry): Either[ThreeSurfaceError, Unit] =
      calls += s"decorations:${geometry.meshes.length}"
      if decorationFailure then Left(ThreeSurfaceError.RuntimeFailure("decoration update", "injected")) else Right(())

    override def clearDecorations(): Either[ThreeSurfaceError, Unit] =
      calls += "decorations-clear"
      if clearFailure then Left(ThreeSurfaceError.RuntimeFailure("decoration clear", "injected")) else Right(())

    def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit] =
      calls += s"resize:${size.width}x${size.height}"
      Right(())

    def draw(): Either[ThreeSurfaceError, Unit] =
      calls += "draw"
      if drawFailure then
        drawFailure = false
        Left(ThreeSurfaceError.RuntimeFailure("draw", "injected"))
      else Right(())

    def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]] =
      calls += "pick"
      Right(nextPick)

    override def pickDisplay(x: Double, y: Double): Either[ThreeSurfaceError, Option[SurfaceDisplayHit[ThreePick]]] =
      calls += "display-pick"
      Right(nextDisplayPick)

    def disposeResources(keys: Vector[SurfaceResourceKey]): Either[ThreeSurfaceError, Int] =
      calls += s"dispose:${keys.length}"
      Right(keys.length)

    def dispose(): Either[ThreeSurfaceError, Unit] =
      calls += "close"
      Right(())

  test("four composition views reuse one uploaded surface resource and camera-only edits stay upload-clean"):
    val (viewer, state) = fixture()
    val legacy = compile(viewer, state)
    val composition = fourViewComposition(legacy)
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get

    val initial = backend.render(composition, size, forceDraw = false).toOption.get
    assertEquals(initial.geometryUploads, 1)
    assertEquals(initial.colorUploads, 1)
    assertEquals(runtime.calls.toVector, Vector(
      "geometry:1", "colors:1", "resize:640x480", "composition:4", "draw"
    ))

    runtime.calls.clear()
    val moved = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.OrbitBy(15, 10)).toOption.get
    val editedView = composition.views(0).copy(camera = compile(viewer, moved).camera)
    val edited = SurfaceCompositionPlan.make(editedView +: composition.views.tail, composition.resources).toOption.get
    val cameraOnly = backend.render(edited, size, forceDraw = false).toOption.get
    assert(!cameraOnly.dirty.geometry)
    assertEquals(cameraOnly.geometryUploads, 0)
    assertEquals(cameraOnly.colorUploads, 0)
    assertEquals(runtime.calls.toVector, Vector("composition:4", "draw"))

  test("composition rejects conflicting occurrences before runtime mutation"):
    val (viewer, state) = fixture()
    val source = SurfaceCompositionPlan.fromLegacy(compile(viewer, state)).toOption.get
    val view = source.views.head
    val conflicting = view.instances.head.copy(
      id = SurfaceInstanceId.make("conflicting-instance").toOption.get,
      layerOrder = Vector.empty
    )
    val plan = SurfaceCompositionPlan.make(Vector(view.copy(instances = view.instances :+ conflicting)), source.resources).toOption.get
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    assert(backend.render(plan, size, forceDraw = false).left.exists(_.message.contains("incompatible mesh")))
    assertEquals(runtime.calls.toVector, Vector.empty)

  test("composition-to-legacy transition removes native occurrences before legacy commands"):
    val (viewer, state) = fixture()
    val legacy = compile(viewer, state)
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    backend.render(fourViewComposition(legacy), size, forceDraw = false).toOption.get
    runtime.calls.clear()
    backend.render(legacy, size).toOption.get
    assertEquals(runtime.calls.headOption, Some("composition-clear"))

  test("decorations validate before native mutation and never reupload scientific resources"):
    val (viewer, state) = fixture()
    val plan = fourViewComposition(compile(viewer, state))
    val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surfaceId, VertexId(0)))
    val marker = SurfaceMarkerDecoration(
      SurfaceDecorationId.make("native-marker").toOption.get,
      anchor,
      SurfaceMarkerShape.Disk,
      SurfaceDecorationSize.world(0.1).toOption.get,
      Rgba32.unsafe(255, 0, 0)
    )
    val packets = SurfaceDecorationCompiler.compile(Vector(marker), plan).toOption.get
    val geometry = SurfaceDecorationGeometry.lower(packets, plan, size.width, size.height).toOption.get
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val receipt = backend.renderDecorated(plan, geometry, size).toOption.get
    assertEquals(receipt.geometryUploads, 1)
    assertEquals(receipt.colorUploads, 1)
    assert(runtime.calls.contains(s"decorations:${geometry.meshes.length}"))
    runtime.calls.clear()
    backend.renderDecorated(plan, geometry, size).toOption.get
    assertEquals(runtime.calls.count(_.startsWith("geometry:")), 0)
    assertEquals(runtime.calls.count(_.startsWith("decorations")), 0)
    assertEquals(runtime.calls.count(_ == "draw"), 1)
    runtime.calls.clear()
    backend.render(plan, size).toOption.get
    assertEquals(runtime.calls.toVector, Vector("decorations-clear", "draw"))
    backend.renderDecorated(plan, geometry, size).toOption.get
    val moved = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.OrbitBy(15, 10)).toOption.get
    val movedPlan = SurfaceCompositionPlan.make(
      plan.views.updated(0, plan.views.head.copy(camera = compile(viewer, moved).camera)), plan.resources).toOption.get
    val movedPackets = SurfaceDecorationCompiler.compile(Vector(marker), movedPlan).toOption.get
    val movedGeometry = SurfaceDecorationGeometry.lower(movedPackets, movedPlan, size.width, size.height).toOption.get
    runtime.calls.clear()
    val cameraOnly = backend.renderDecorated(movedPlan, movedGeometry, size).toOption.get
    assertEquals(cameraOnly.geometryUploads, 0)
    assertEquals(cameraOnly.geometryUpdates, 0)
    assertEquals(cameraOnly.colorUploads, 0)
    assert(runtime.calls.contains(s"decorations:${movedGeometry.meshes.length}"))
    runtime.calls.clear()
    backend.render(plan, size).toOption.get
    assertEquals(runtime.calls.headOption, Some("decorations-clear"))
    runtime.calls.clear()
    backend.renderDecorated(plan, geometry, size).toOption.get
    assert(runtime.calls.contains(s"decorations:${geometry.meshes.length}"))
    runtime.calls.clear()
    backend.renderDecorated(plan, geometry.copy(), size).toOption.get
    assertEquals(runtime.calls.headOption, Some("decorations-clear"))
    assert(runtime.calls.contains(s"decorations:${geometry.meshes.length}"))

  test("stale decoration geometry refuses before fake runtime mutation"):
    val (viewer, state) = fixture()
    val plan = fourViewComposition(compile(viewer, state))
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val stale = SurfaceDecorationGeometry(Vector.empty, Vector.empty, size.width - 1, size.height, "stale")
    assert(backend.renderDecorated(plan, stale, size).isLeft)
    assertEquals(runtime.calls.toVector, Vector.empty)

  test("ordered strokes admit solid alpha only with opaque scientific surfaces before native mutation"):
    val (viewer, state) = fixture()
    val plan = fourViewComposition(compile(viewer, state))
    val marker = SurfaceContourDecoration(SurfaceDecorationId.make("ordered").toOption.get,
      surfaceId, Vector.tabulate(plan.resources.meshes.values.head.positions.length / 3)(i => if i == 0 then 0 else 1),
      SurfaceContourPolicy.Transitions, SurfaceContourStyle(SurfaceDecorationSize.pixels(8).toOption.get,
        Rgba32.unsafe(255, 0, 0), Some(SurfaceContourHalo(Rgba32.unsafe(255, 255, 255, 153),
          SurfaceDecorationSize.pixels(2).toOption.get)), mode = SurfaceContourMode.RoundedStrokes))
    def lower(value: SurfaceCompositionPlan) = SurfaceDecorationGeometry.lower(
      SurfaceDecorationCompiler.compile(Vector(marker), value).toOption.get, value, size.width, size.height).toOption.get
    val geometry = lower(plan)
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    assert(backend.renderDecorated(plan, geometry, size).isRight)
    runtime.calls.clear()
    val defaultDepth = geometry.copy(meshes = geometry.meshes.map(_.copy(depthPolicy = SurfaceDecorationDepthPolicy.DepthTest)))
    assert(backend.renderDecorated(plan, defaultDepth, size).isLeft)
    assertEquals(runtime.calls.toVector, Vector.empty)
    val translucent = SurfaceCompositionPlan.make(plan.views.map(view => view.copy(instances =
      view.instances.map(_.copy(opacity = DisplayOpacity.make(.5).toOption.get)))), plan.resources).toOption.get
    assert(backend.renderDecorated(translucent, lower(translucent), size).isLeft)
    assertEquals(runtime.calls.toVector, Vector.empty)

  test("failed decoration replacement clears the old native decoration state"):
    val (viewer, state) = fixture()
    val plan = fourViewComposition(compile(viewer, state))
    val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surfaceId, VertexId(0)))
    val marker = SurfaceMarkerDecoration(SurfaceDecorationId.make("replace-marker").toOption.get, anchor,
      SurfaceMarkerShape.Disk, SurfaceDecorationSize.world(0.1).toOption.get, Rgba32.unsafe(255, 0, 0))
    val geometry = SurfaceDecorationGeometry.lower(
      SurfaceDecorationCompiler.compile(Vector(marker), plan).toOption.get, plan, size.width, size.height).toOption.get
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    backend.renderDecorated(plan, geometry, size).toOption.get
    runtime.calls.clear()
    runtime.decorationFailure = true
    val replacement = geometry.copy(meshes = geometry.meshes.map(_.copy(color = Rgba32.unsafe(0, 255, 0))))
    assert(backend.renderDecorated(plan, replacement, size, forceDraw = true).isLeft)
    assertEquals(runtime.calls.headOption, Some("decorations-clear"))
    runtime.calls.clear()
    runtime.decorationFailure = false
    backend.renderDecorated(plan, geometry, size).toOption.get
    assert(runtime.calls.contains(s"decorations:${geometry.meshes.length}"))

  test("draw failure after a successful decoration install is cleared by a later plain render"):
    val (viewer, state) = fixture()
    val plan = fourViewComposition(compile(viewer, state))
    val anchor = SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surfaceId, VertexId(0)))
    val marker = SurfaceMarkerDecoration(SurfaceDecorationId.make("draw-failure-marker").toOption.get, anchor,
      SurfaceMarkerShape.Disk, SurfaceDecorationSize.world(0.1).toOption.get, Rgba32.unsafe(255, 0, 0))
    val geometry = SurfaceDecorationGeometry.lower(
      SurfaceDecorationCompiler.compile(Vector(marker), plan).toOption.get, plan, size.width, size.height).toOption.get
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    backend.renderDecorated(plan, geometry, size).toOption.get
    runtime.calls.clear()
    runtime.drawFailure = true
    assert(backend.renderDecorated(plan, geometry, size).isLeft)
    runtime.calls.clear()
    backend.render(plan, size).toOption.get
    assertEquals(runtime.calls.toVector, Vector("decorations-clear", "draw"))

  test("failed native cleanup invalidates decoration reuse before an identical retry"):
    val (viewer, state) = fixture()
    val legacy = compile(viewer, state)
    val plan = fourViewComposition(legacy)
    val marker = SurfaceMarkerDecoration(SurfaceDecorationId.make("clear-failure-marker").toOption.get,
      SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surfaceId, VertexId(0))),
      SurfaceMarkerShape.Disk, SurfaceDecorationSize.world(0.1).toOption.get, Rgba32.unsafe(255, 0, 0))
    val geometry = SurfaceDecorationGeometry.lower(
      SurfaceDecorationCompiler.compile(Vector(marker), plan).toOption.get, plan, size.width, size.height).toOption.get
    for clearComposition <- Vector(false, true) do
      val runtime = new RecordingRuntime()
      val backend = ThreeSurfaceBackend.create(runtime).toOption.get
      backend.renderDecorated(plan, geometry, size).toOption.get
      runtime.calls.clear()
      runtime.clearFailure = true
      val failed = if clearComposition then backend.render(legacy, size) else backend.render(plan, size)
      assert(failed.isLeft)
      assertEquals(runtime.calls.toVector,
        Vector(if clearComposition then "composition-clear" else "decorations-clear"))
      runtime.calls.clear()
      runtime.clearFailure = false
      backend.renderDecorated(plan, geometry, size).toOption.get
      assertEquals(runtime.calls.headOption, Some("decorations-clear"))
      assert(runtime.calls.contains(s"decorations:${geometry.meshes.length}"))

  test("display picking preserves tagged decoration hits without manufacturing a surface id"):
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    val origin = SurfaceDisplayOrigin(SurfaceViewId.make("display-pick").toOption.get,
      SurfaceInstanceId.make("display-pick-instance").toOption.get)
    val id = SurfaceDecorationElementId(SurfaceDecorationId.make("picked-decoration").toOption.get, "marker")
    val target = SurfaceDecorationTarget.Anchor(SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(surfaceId, VertexId(0))))
    runtime.nextDisplayPick = Some(SurfaceDisplayHit.Decoration(SurfaceDecorationHit(id, origin, target, 3.0)))
    assertEquals(backend.pickDisplay(10, 20).toOption.flatten, runtime.nextDisplayPick)
    assertEquals(runtime.calls.toVector, Vector("display-pick"))

  private def fourViewComposition(plan: SurfaceRenderPlan): SurfaceCompositionPlan =
    val source = SurfaceCompositionPlan.fromLegacy(plan).toOption.get
    val base = source.views.head
    val boxes = Vector(
      SurfaceViewport(0.0, 0.0, 0.5, 0.5), SurfaceViewport(0.5, 0.0, 0.5, 0.5),
      SurfaceViewport(0.0, 0.5, 0.5, 0.5), SurfaceViewport(0.5, 0.5, 0.5, 0.5)
    )
    val views = boxes.zipWithIndex.map: (box, index) =>
      val instance = base.instances.head.copy(id = SurfaceInstanceId.make(s"instance-$index").toOption.get)
      base.copy(id = SurfaceViewId.make(s"view-$index").toOption.get, viewport = box, instances = Vector(instance))
    SurfaceCompositionPlan.make(views, source.resources).toOption.get

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
    val pial = SurfaceGeometry(shiftedMesh, Hemisphere.Left, SurfaceKind.Pial, Affine.identity[D3])
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
    // Composition must preserve the same topology-resource / geometry-revision
    // contract, including both transitions to and from legacy rendering.
    val originalPlan = compile(viewer, initial)
    val morphedPlan = compile(viewer, halfway)
    val originalComposition = SurfaceCompositionPlan.fromLegacy(originalPlan).toOption.get
    val morphedComposition = SurfaceCompositionPlan.fromLegacy(morphedPlan).toOption.get
    assertEquals(originalPlan.meshes.head.resourceKey, morphedPlan.meshes.head.resourceKey)
    assert(originalPlan.meshes.head.geometryKey != morphedPlan.meshes.head.geometryKey)
    val toComposition = backend.render(originalComposition, size).toOption.get
    assertEquals(toComposition.geometryUpdates, 1)
    assertEquals(toComposition.geometryUploads, 0)
    val withinComposition = backend.render(morphedComposition, size).toOption.get
    assertEquals(withinComposition.geometryUpdates, 1)
    assertEquals(withinComposition.geometryUploads, 0)
    val toLegacy = backend.render(originalPlan, size).toOption.get
    assertEquals(toLegacy.geometryUpdates, 1)
    assertEquals(toLegacy.geometryUploads, 0)

  test("world clipping without original fragment samples is rejected before Three.js interpretation"):
    val (viewer, initial) = fixture()
    val clipping = SurfaceClipping.worldPlanes(Vector(
      WorldClipPlane.unsafe(1.0, 0.0, 0.0, 0.0)
    )).toOption.get
    val result = ThreeSurfaceProgram.compile(
      None,
      compile(viewer, initial).copy(clipping = clipping),
      size
    )
    assert(result.left.exists(_.message.contains("world clipping requires fragment packets")))

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
      Seq((0, 1, 2), (0, 3, 1), (0, 2, 3), (1, 3, 2))
    )
    SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Inflated, Affine.identity[D3])

  private def compiledColors(viewer: SurfaceViewerModel, state: SurfaceViewerState): Array[Float] =
    val program = ThreeSurfaceProgram.compile(None, compile(viewer, state), size).toOption.get
    program.commands.collectFirst:
      case ThreeSurfaceCommand.UploadColors(values) => values.head.rgb
    .get

  private def compile(viewer: SurfaceViewerModel, state: SurfaceViewerState): SurfaceRenderPlan =
    SurfaceCompiler.compile(viewer, state).toOption.get

  test("fitting policy changes reach the runtime without geometry, colors or camera uploads"):
    val (viewer, state) = fixture()
    val first = compile(viewer, state)
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    backend.render(first, size).toOption.get
    assertEquals(runtime.fitted, first.viewportFit)
    runtime.calls.clear()
    val second = first.copy(viewportFit = SurfaceViewportFit.Fill)
    val receipt = backend.render(second, size).toOption.get
    assert(receipt.dirty.layout)
    assertEquals(receipt.geometryUploads, 0)
    assertEquals(receipt.colorUploads, 0)
    assertEquals(runtime.calls.toVector, Vector("layout:1", "draw"))
    assertEquals(runtime.fitted, SurfaceViewportFit.Fill)
    backend.dispose().toOption.get

  test("composition rejects conflicting resource bytes without native commands"):
    val (viewer, state) = fixture()
    val before = fourViewComposition(compile(viewer, state))
    val runtime = new RecordingRuntime()
    val backend = ThreeSurfaceBackend.create(runtime).toOption.get
    backend.render(before, size).toOption.get
    runtime.calls.clear()
    val layer = before.resources.layers.values.head
    val changed = layer.copy(colors = new IntBufferView(Array.fill(layer.colors.length)(0xff00ffff)))
    val after = SurfaceCompositionPlan.make(before.views, before.resources.copy(layers =
      before.resources.layers.updated(layer.resourceKey, changed))).toOption.get
    assert(backend.render(after, size).isLeft)
    assertEquals(runtime.calls.toVector, Vector.empty)
