package scalafim.surface.view.three

import scala.collection.mutable
import scala.scalajs.js
import scala.scalajs.js.typedarray.{Float32Array, Uint32Array}

import scalafim.surface.view.*

/** Thin interpreter over a host-owned Three.js namespace. The host may pass an
  * imported ES module (`import * as THREE from "three"`) or `window.THREE`.
  * No Three.js object escapes this adapter.
  */
final class ThreeJsRuntime private (
  three: js.Dynamic,
  canvas: js.Dynamic,
  renderer: js.Dynamic,
  scene: js.Dynamic,
  camera: js.Dynamic,
  raycaster: js.Dynamic
) extends ThreeSurfaceRuntime:
  private final case class Bundle(
    surface: SurfaceId,
    meshKey: SurfaceResourceKey,
    geometry: js.Dynamic,
    material: js.Dynamic,
    mesh: js.Dynamic,
    var colorKeys: Vector[SurfaceResourceKey],
    var colorArray: Float32Array
  )

  private val bundles = mutable.LinkedHashMap.empty[SurfaceId, Bundle]
  private var slots = Vector.empty[SurfaceViewSlot]
  private var canvasSize = ThreeCanvasSize.unsafe(1, 1)
  private var disposed = false

  def contextState: ThreeContextState =
    if disposed then ThreeContextState.Lost
    else if missing(canvas) || missing(renderer) then ThreeContextState.Absent
    else
      try
        val context = renderer.applyDynamic("getContext")()
        if missing(context) then ThreeContextState.Absent
        else if js.typeOf(context.selectDynamic("isContextLost")) == "function" &&
          context.applyDynamic("isContextLost")().asInstanceOf[Boolean]
        then ThreeContextState.Lost
        else ThreeContextState.Available
      catch case _: Throwable => ThreeContextState.Absent

  override def supportsGpuVolumeProjection: Boolean =
    if contextState != ThreeContextState.Available then false
    else
      try
        renderer.selectDynamic("capabilities").selectDynamic("isWebGL2").asInstanceOf[Boolean] &&
          renderer.selectDynamic("extensions").applyDynamic("has")("EXT_color_buffer_float").asInstanceOf[Boolean]
      catch case _: Throwable => false

  def uploadGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long] =
    attempt("geometry upload"):
      var bytes = 0L
      meshes.foreach: packet =>
        bundles.remove(packet.surface).foreach(disposeBundle)
        val positions = floats(packet.positions)
        val normals = floats(packet.normals)
        val indices = ints(packet.indices)
        val geometry = construct("BufferGeometry")()
        val position = construct("BufferAttribute")(positions, 3)
        val normal = construct("BufferAttribute")(normals, 3)
        position.applyDynamic("setUsage")(three.selectDynamic("DynamicDrawUsage"))
        normal.applyDynamic("setUsage")(three.selectDynamic("DynamicDrawUsage"))
        geometry.applyDynamic("setAttribute")("position", position)
        geometry.applyDynamic("setAttribute")("normal", normal)
        geometry.applyDynamic("setIndex")(construct("BufferAttribute")(indices, 1))
        geometry.applyDynamic("computeBoundingSphere")()
        val material = newMaterial()
        val mesh = construct("Mesh")(geometry, material)
        mesh.updateDynamic("matrixAutoUpdate")(false)
        mesh.updateDynamic("frustumCulled")(true)
        mesh.selectDynamic("userData").updateDynamic("scalafimSurfaceId")(packet.surface.value)
        scene.applyDynamic("add")(mesh)
        bundles(packet.surface) = Bundle(
          packet.surface,
          packet.resourceKey,
          geometry,
          material,
          mesh,
          Vector.empty,
          new Float32Array(0)
        )
        bytes += positions.byteLength.toLong + normals.byteLength.toLong + indices.byteLength.toLong
      bytes

  def updateGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long] =
    attempt("geometry update"):
      var bytes = 0L
      meshes.foreach: packet =>
        val bundle = bundles.getOrElse(
          packet.surface,
          throw new IllegalArgumentException(s"surface '${packet.surface.value}' has no uploaded geometry")
        )
        if bundle.meshKey != packet.resourceKey then
          throw new IllegalArgumentException(s"surface '${packet.surface.value}' changed topology resource")
        val position = bundle.geometry.applyDynamic("getAttribute")("position")
        val normal = bundle.geometry.applyDynamic("getAttribute")("normal")
        val positionArray = position.selectDynamic("array").asInstanceOf[Float32Array]
        val normalArray = normal.selectDynamic("array").asInstanceOf[Float32Array]
        if positionArray.length != packet.positions.length || normalArray.length != packet.normals.length then
          throw new IllegalArgumentException(s"surface '${packet.surface.value}' changed vertex count")
        var index = 0
        while index < positionArray.length do
          positionArray(index) = packet.positions(index)
          normalArray(index) = packet.normals(index)
          index += 1
        position.updateDynamic("needsUpdate")(true)
        normal.updateDynamic("needsUpdate")(true)
        bundle.geometry.applyDynamic("computeBoundingSphere")()
        bytes += positionArray.byteLength.toLong + normalArray.byteLength.toLong
      bytes

  def uploadColors(colors: Vector[ThreeSurfaceColors]): Either[ThreeSurfaceError, Long] =
    attempt("color upload"):
      var bytes = 0L
      colors.foreach: packet =>
        val bundle = bundles.getOrElse(
          packet.surface,
          throw new IllegalArgumentException(s"surface '${packet.surface.value}' has no uploaded geometry")
        )
        val source = packet.rgb
        val target =
          if bundle.colorArray.length == source.length then bundle.colorArray
          else new Float32Array(source.length)
        var index = 0
        while index < source.length do
          target(index) = source(index)
          index += 1
        val current = bundle.geometry.applyDynamic("getAttribute")("color")
        if missing(current) || bundle.colorArray.length != source.length then
          bundle.geometry.applyDynamic("setAttribute")("color", construct("BufferAttribute")(target, 3))
        else current.updateDynamic("needsUpdate")(true)
        bundle.colorArray = target
        bundle.colorKeys = packet.resourceKeys
        bytes += target.byteLength.toLong
      bytes

  def updateLighting(lighting: SurfaceLighting): Either[ThreeSurfaceError, Unit] =
    attempt("lighting update"):
      val (ambient, diffuse, x, y, z) =
        lighting match
          case SurfaceLighting.Unlit => (1.0, 0.0, 0.0, 0.0, 1.0)
          case SurfaceLighting.Directional(a, d, dx, dy, dz) => (a.value, d.value, dx, dy, dz)
      bundles.valuesIterator.foreach: bundle =>
        val uniforms = bundle.material.selectDynamic("uniforms")
        uniforms.selectDynamic("uAmbient").updateDynamic("value")(ambient)
        uniforms.selectDynamic("uDiffuse").updateDynamic("value")(diffuse)
        uniforms.selectDynamic("uLightDirection").selectDynamic("value").applyDynamic("set")(x, y, z)
      ()

  def updateCamera(
    packet: SurfaceCameraPacket,
    clipping: SurfaceClipping
  ): Either[ThreeSurfaceError, Unit] =
    attempt("camera update"):
      camera.selectDynamic("matrixWorldInverse").applyDynamic("fromArray")(columnMajor(packet.viewMatrix))
      camera.selectDynamic("matrixWorld").applyDynamic("copy")(camera.selectDynamic("matrixWorldInverse"))
      camera.selectDynamic("matrixWorld").applyDynamic("invert")()
      camera.selectDynamic("projectionMatrix").applyDynamic("fromArray")(columnMajor(packet.projectionMatrix))
      camera.selectDynamic("projectionMatrixInverse").applyDynamic("copy")(camera.selectDynamic("projectionMatrix"))
      camera.selectDynamic("projectionMatrixInverse").applyDynamic("invert")()
      camera.updateDynamic("matrixWorldNeedsUpdate")(false)
      ()

  def updateLayout(next: Vector[SurfaceViewSlot]): Either[ThreeSurfaceError, Unit] =
    attempt("layout update"):
      slots = next

  def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit] =
    attempt("resize"):
      renderer.applyDynamic("setPixelRatio")(size.pixelRatio)
      renderer.applyDynamic("setSize")(size.width, size.height, false)
      canvasSize = size

  def draw(): Either[ThreeSurfaceError, Unit] =
    attempt("draw"):
      renderer.updateDynamic("autoClear")(false)
      ThreeJsRuntime.clearFrame(renderer)
      slots.foreach: slot =>
        bundles.valuesIterator.foreach: bundle =>
          val visible = bundle.surface == slot.surface
          bundle.mesh.updateDynamic("visible")(visible)
          if visible then position(bundle, slot)
        val viewport = slot.viewport
        val x = math.round(viewport.x * canvasSize.width).toInt
        val y = math.round((1.0 - viewport.y - viewport.height) * canvasSize.height).toInt
        val width = math.max(1, math.round(viewport.width * canvasSize.width).toInt)
        val height = math.max(1, math.round(viewport.height * canvasSize.height).toInt)
        renderer.applyDynamic("setViewport")(x, y, width, height)
        renderer.applyDynamic("setScissor")(x, y, width, height)
        renderer.applyDynamic("render")(scene, camera)
      renderer.applyDynamic("setScissorTest")(false)
      bundles.valuesIterator.foreach(_.mesh.updateDynamic("visible")(true))
      ()

  def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]] =
    attempt("pick"):
      val slot = slots.find: candidate =>
        val viewport = candidate.viewport
        val left = viewport.x * canvasSize.width
        val top = viewport.y * canvasSize.height
        x >= left && x < left + viewport.width * canvasSize.width &&
          y >= top && y < top + viewport.height * canvasSize.height
      slot.flatMap: selected =>
        bundles.get(selected.surface).flatMap: bundle =>
          position(bundle, selected)
          val viewport = selected.viewport
          val localX = (x / canvasSize.width - viewport.x) / viewport.width
          val localY = (y / canvasSize.height - viewport.y) / viewport.height
          val pointer = construct("Vector2")(localX * 2.0 - 1.0, 1.0 - localY * 2.0)
          raycaster.applyDynamic("setFromCamera")(pointer, camera)
          val hits = raycaster.applyDynamic("intersectObject")(bundle.mesh, false).asInstanceOf[js.Array[js.Dynamic]]
          hits.headOption.flatMap(hit => decodePick(bundle, selected, hit))

  def disposeResources(keys: Vector[SurfaceResourceKey]): Either[ThreeSurfaceError, Int] =
    attempt("resource disposal"):
      val keySet = keys.toSet
      val surfaces = bundles.iterator.collect:
        case (surface, bundle) if keySet(bundle.meshKey) => surface
      .toVector
      surfaces.foreach(surface => bundles.remove(surface).foreach(disposeBundle))
      keys.length

  def dispose(): Either[ThreeSurfaceError, Unit] =
    attempt("backend disposal"):
      bundles.valuesIterator.foreach(disposeBundle)
      bundles.clear()
      slots = Vector.empty
      if !disposed then
        disposed = true
        renderer.applyDynamic("dispose")()
        loseContext()
      ()

  /** `renderer.dispose()` releases Three.js state but leaves the WebGL context
    * alive until the canvas is collected, and browsers evict the oldest live
    * contexts once a small budget is exceeded. Forcing loss returns this
    * canvas's slot immediately.
    */
  private def loseContext(): Unit =
    try
      val webgl2 = canvas.applyDynamic("getContext")("webgl2")
      val context = if missing(webgl2) then canvas.applyDynamic("getContext")("webgl") else webgl2
      if !missing(context) && js.typeOf(context.selectDynamic("getExtension")) == "function" then
        val extension = context.applyDynamic("getExtension")("WEBGL_lose_context")
        if !missing(extension) && js.typeOf(extension.selectDynamic("loseContext")) == "function" then
          extension.applyDynamic("loseContext")()
    catch case _: Throwable => ()

  private def decodePick(bundle: Bundle, slot: SurfaceViewSlot, hit: js.Dynamic): Option[ThreePick] =
    val faceIndexValue = hit.selectDynamic("faceIndex")
    if missing(faceIndexValue) then None
    else
      val face = faceIndexValue.asInstanceOf[Int]
      val indices = bundle.geometry.applyDynamic("getIndex")().selectDynamic("array").asInstanceOf[Uint32Array]
      val positions = bundle.geometry.applyDynamic("getAttribute")("position")
      val point = hit.selectDynamic("point")
      val localX = point.selectDynamic("x").asInstanceOf[Double] - slot.worldOffsetX
      val localY = point.selectDynamic("y").asInstanceOf[Double] - slot.worldOffsetY
      val localZ = point.selectDynamic("z").asInstanceOf[Double] - slot.worldOffsetZ
      val base = face * 3
      val candidates = Vector(
        indices(base).toInt,
        indices(base + 1).toInt,
        indices(base + 2).toInt
      )
      val vertex = candidates.minBy: candidate =>
        val dx = positions.applyDynamic("getX")(candidate).asInstanceOf[Double] - localX
        val dy = positions.applyDynamic("getY")(candidate).asInstanceOf[Double] - localY
        val dz = positions.applyDynamic("getZ")(candidate).asInstanceOf[Double] - localZ
        dx * dx + dy * dy + dz * dz
      Some(ThreePick(
        bundle.surface,
        face,
        vertex,
        localX,
        localY,
        localZ
      ))

  private def position(bundle: Bundle, slot: SurfaceViewSlot): Unit =
    bundle.mesh.selectDynamic("position").applyDynamic("set")(
      slot.worldOffsetX,
      slot.worldOffsetY,
      slot.worldOffsetZ
    )
    bundle.mesh.applyDynamic("updateMatrix")()
    bundle.mesh.applyDynamic("updateMatrixWorld")(true)

  private def disposeBundle(bundle: Bundle): Unit =
    scene.applyDynamic("remove")(bundle.mesh)
    bundle.geometry.applyDynamic("dispose")()
    bundle.material.applyDynamic("dispose")()

  private def newMaterial(): js.Dynamic =
    val uniforms = js.Dynamic.literal(
      uAmbient = js.Dynamic.literal(value = 1.0),
      uDiffuse = js.Dynamic.literal(value = 0.0),
      uLightDirection = js.Dynamic.literal(value = construct("Vector3")(0.0, 0.0, 1.0))
    )
    construct("ShaderMaterial")(js.Dynamic.literal(
      uniforms = uniforms,
      vertexShader = ThreeJsRuntime.VertexShader,
      fragmentShader = ThreeJsRuntime.FragmentShader,
      vertexColors = true,
      // SurfaceRenderPlan v1 is two-sided. External FreeSurfer/GIFTI winding
      // must not silently select an anatomical inside or outside here.
      side = three.selectDynamic("DoubleSide")
    ))

  private def floats(values: FloatBufferView): Float32Array =
    val out = new Float32Array(values.length)
    var index = 0
    while index < values.length do
      out(index) = values(index)
      index += 1
    out

  private def ints(values: IntBufferView): Uint32Array =
    val out = new Uint32Array(values.length)
    var index = 0
    while index < values.length do
      out(index) = values(index).toDouble
      index += 1
    out

  private def columnMajor(values: FloatBufferView): Float32Array =
    val out = new Float32Array(16)
    var row = 0
    while row < 4 do
      var column = 0
      while column < 4 do
        out(column * 4 + row) = values(row * 4 + column)
        column += 1
      row += 1
    out

  private def construct(name: String)(arguments: js.Any*): js.Dynamic =
    val constructor = three.selectDynamic(name)
    if missing(constructor) then throw new IllegalArgumentException(s"THREE.$name is unavailable")
    js.Dynamic.newInstance(constructor)(arguments*)

  private def attempt[A](operation: String)(body: => A): Either[ThreeSurfaceError, A] =
    try Right(body)
    catch
      case error: Throwable => Left(ThreeSurfaceError.RuntimeFailure(operation, Option(error.getMessage).getOrElse(error.toString)))

  private def missing(value: js.Dynamic): Boolean =
    value == null || js.isUndefined(value)

object ThreeJsRuntime:
  private val VertexShader =
    """
      |uniform float uAmbient;
      |uniform float uDiffuse;
      |uniform vec3 uLightDirection;
      |varying vec3 vColor;
      |void main() {
      |  float shade = min(1.0, uAmbient + uDiffuse * max(0.0, dot(normalize(normal), normalize(uLightDirection))));
      |  vColor = color * shade;
      |  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
      |}
      |""".stripMargin

  private val FragmentShader =
    """
      |varying vec3 vColor;
      |void main() {
      |  gl_FragColor = vec4(vColor, 1.0);
      |}
      |""".stripMargin

  private[three] def clearFrame(renderer: js.Dynamic): Unit =
    renderer.applyDynamic("setClearColor")(0xffffff, 1.0)
    // The previous frame leaves the last bilateral viewport as the active
    // scissor. Clear with scissoring disabled or the first viewport retains
    // old geometry and visibly tears during a morph.
    renderer.applyDynamic("setScissorTest")(false)
    renderer.applyDynamic("clear")(true, true, true)
    renderer.applyDynamic("setScissorTest")(true)

  def create(three: js.Dynamic, canvas: js.Dynamic): Either[ThreeSurfaceError, ThreeJsRuntime] =
    if three == null || js.isUndefined(three) || canvas == null || js.isUndefined(canvas) then
      Left(ThreeSurfaceError.ContextUnavailable)
    else
      try
        val renderer = js.Dynamic.newInstance(three.selectDynamic("WebGLRenderer"))(js.Dynamic.literal(
          canvas = canvas,
          antialias = true,
          alpha = false,
          preserveDrawingBuffer = false
        ))
        val scene = js.Dynamic.newInstance(three.selectDynamic("Scene"))()
        val camera = js.Dynamic.newInstance(three.selectDynamic("PerspectiveCamera"))()
        camera.updateDynamic("matrixAutoUpdate")(false)
        val raycaster = js.Dynamic.newInstance(three.selectDynamic("Raycaster"))()
        val runtime = new ThreeJsRuntime(three, canvas, renderer, scene, camera, raycaster)
        runtime.contextState match
          case ThreeContextState.Available => Right(runtime)
          case ThreeContextState.Lost => Left(ThreeSurfaceError.ContextLost)
          case ThreeContextState.Absent => Left(ThreeSurfaceError.ContextUnavailable)
      catch
        case error: Throwable =>
          Left(ThreeSurfaceError.RuntimeFailure(
            "initialization",
            Option(error.getMessage).getOrElse(error.toString)
          ))
