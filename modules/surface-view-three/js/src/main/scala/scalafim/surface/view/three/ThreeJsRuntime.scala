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
    var material: js.Dynamic,
    mesh: js.Dynamic,
    pickMesh: js.Dynamic,
    var colorKeys: Vector[SurfaceResourceKey],
    var colorArray: Float32Array,
    var packet: SurfaceMeshPacket,
    var shaderIdentity: Option[(String, String)] = None
  ):
    var depthBounds = ThreeJsRuntime.positionBounds(packet.positions)

  private val bundles = mutable.LinkedHashMap.empty[SurfaceId, Bundle]
  private var slots = Vector.empty[SurfaceViewSlot]
  private var viewportFit: SurfaceViewportFit = SurfaceViewportFit.Fill
  private def fittedSlots: Vector[SurfaceViewSlot] = viewportFit.resolve(slots, canvasSize.width.toDouble, canvasSize.height.toDouble)
  private var canvasSize = ThreeCanvasSize.unsafe(1, 1)

  def contextState: ThreeContextState =
    if missing(canvas) || missing(renderer) then ThreeContextState.Absent
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

  override def supportsFragmentLayers: Boolean =
    contextState == ThreeContextState.Available &&
      renderer.selectDynamic("capabilities").applyDynamic("getMaxPrecision")("highp").asInstanceOf[String] == "highp"

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
          makePickMesh(packet, mesh, geometry, material),
          Vector.empty,
          new Float32Array(0),
          packet
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
        if bundle.packet.nearestPartition.nonEmpty then
          bundle.pickMesh.selectDynamic("geometry").applyDynamic("computeBoundingSphere")()
        bundle.packet = packet
        bundle.depthBounds = ThreeJsRuntime.positionBounds(packet.positions)
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

  override def uploadFragments(packets: Vector[ThreeFragmentPacket]): Either[ThreeSurfaceError, Long] =
    attempt("fragment shader upload"):
      packets.foreach: packet =>
        val bundle = bundles(packet.surface)
        require(packet.attributes.forall(_.values.length == bundle.packet.positions.length / 3 * 4), "fragment attribute count differs from geometry")
      var bytes = 0L
      packets.foreach: packet =>
        val bundle = bundles(packet.surface)
        packet.attributes.foreach: attribute =>
          val current = bundle.geometry.applyDynamic("getAttribute")(attribute.name)
          val values = if missing(current) then new Float32Array(attribute.values.length)
            else current.selectDynamic("array").asInstanceOf[Float32Array]
          var index = 0
          while index < attribute.values.length do
            values(index) = attribute.values(index)
            index += 1
          if missing(current) then bundle.geometry.applyDynamic("setAttribute")(attribute.name, construct("BufferAttribute")(values, 4))
          else current.updateDynamic("needsUpdate")(true)
          bytes += values.byteLength.toLong
        val identity = (packet.vertexShader, packet.fragmentShader)
        if !bundle.shaderIdentity.contains(identity) then
          val material = newMaterial(Some(packet))
          bundle.material.applyDynamic("dispose")()
          bundle.material = material
          bundle.mesh.updateDynamic("material")(material)
          bundle.pickMesh.updateDynamic("material")(material)
          bundle.shaderIdentity = Some(identity)
        bundle.colorKeys = packet.resourceKeys
      bytes

  override def validateFragments(packets: Vector[ThreeFragmentPacket]): Either[ThreeSurfaceError, Unit] =
    val capabilities = renderer.selectDynamic("capabilities")
    val maxAttributes = capabilities.selectDynamic("maxAttributes").asInstanceOf[Int]
    val maxVaryings = capabilities.selectDynamic("maxVaryings").asInstanceOf[Int]
    if packets.exists(p => p.attributes.length + 2 > maxAttributes || p.attributes.length + 1 > maxVaryings) then
      Left(ThreeSurfaceError.InvalidPlan("fragment layers exceed native attribute or varying capacity"))
    else Right(())

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

  def updateLayout(next: Vector[SurfaceViewSlot], fit: SurfaceViewportFit): Either[ThreeSurfaceError, Unit] =
    attempt("layout update"):
      slots = next
      viewportFit = fit

  def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit] =
    attempt("resize"):
      renderer.applyDynamic("setPixelRatio")(size.pixelRatio)
      renderer.applyDynamic("setSize")(size.width, size.height, false)
      canvasSize = size

  def draw(): Either[ThreeSurfaceError, Unit] =
    attempt("draw"):
      renderer.updateDynamic("autoClear")(false)
      ThreeJsRuntime.clearFrame(renderer)
      val originalProjection = camera.selectDynamic("projectionMatrix").applyDynamic("clone")()
      val drawSlots = fittedSlots
      val depthInputs = drawSlots.flatMap: slot =>
        bundles.get(slot.surface).map: bundle =>
          position(bundle, slot)
          val modelView = camera.selectDynamic("matrixWorldInverse").applyDynamic("clone")()
            .applyDynamic("multiply")(bundle.mesh.selectDynamic("matrixWorld"))
          (modelView.selectDynamic("elements").asInstanceOf[js.Array[Double]], bundle.depthBounds)
      // All slots share the same depth transform, including overlapping slots.
      val depthProjection = ThreeJsRuntime.depthProjection(
        originalProjection.selectDynamic("elements").asInstanceOf[js.Array[Double]], depthInputs)
      try
        drawSlots.foreach: slot =>
          bundles.valuesIterator.foreach: bundle =>
            val visible = bundle.surface == slot.surface
            bundle.mesh.updateDynamic("visible")(visible)
            if visible then position(bundle, slot)
          val viewport = ThreeJsRuntime.nativeViewport(slot.viewport, canvasSize)
          // glViewport has integral device coordinates. Correct clip x/y so
          // its pixel positions still agree with the fractional logical slot
          // used by the reference raster and continuous scientific picking.
          val projection = ThreeJsRuntime.viewportProjection(
            depthProjection, viewport)
          camera.selectDynamic("projectionMatrix").applyDynamic("fromArray")(projection)
          val ratio = canvasSize.pixelRatio
          renderer.applyDynamic("setViewport")(viewport.x / ratio, viewport.y / ratio,
            viewport.width / ratio, viewport.height / ratio)
          renderer.applyDynamic("setScissor")(viewport.x / ratio, viewport.y / ratio,
            viewport.width / ratio, viewport.height / ratio)
          renderer.applyDynamic("render")(scene, camera)
      finally camera.selectDynamic("projectionMatrix").applyDynamic("copy")(originalProjection)
      renderer.applyDynamic("setScissorTest")(false)
      bundles.valuesIterator.foreach(_.mesh.updateDynamic("visible")(true))
      ()

  def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]] =
    attempt("pick"):
      val slot = fittedSlots.find: candidate =>
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
          // The camera carries our compiled matrices and may use either projection.
          // Three's setFromCamera dispatches on the native camera class, which does
          // not change when our projection changes. Unproject the clip segment instead.
          val inverseProjection = camera.selectDynamic("projectionMatrixInverse")
          val world = camera.selectDynamic("matrixWorld")
          val near = construct("Vector3")(localX * 2.0 - 1.0, 1.0 - localY * 2.0, -1.0)
            .applyDynamic("applyMatrix4")(inverseProjection).applyDynamic("applyMatrix4")(world)
          val far = construct("Vector3")(localX * 2.0 - 1.0, 1.0 - localY * 2.0, 1.0)
            .applyDynamic("applyMatrix4")(inverseProjection).applyDynamic("applyMatrix4")(world)
          val direction = far.applyDynamic("sub")(near)
          raycaster.updateDynamic("near")(0.0)
          raycaster.updateDynamic("far")(direction.applyDynamic("length")())
          raycaster.applyDynamic("set")(near, direction.applyDynamic("normalize")())
          val hits = raycaster.applyDynamic("intersectObject")(bundle.pickMesh, false).asInstanceOf[js.Array[js.Dynamic]]
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
      renderer.applyDynamic("dispose")()
      slots = Vector.empty
      ()

  private def decodePick(bundle: Bundle, slot: SurfaceViewSlot, hit: js.Dynamic): Option[ThreePick] =
    val faceIndexValue = hit.selectDynamic("faceIndex")
    if missing(faceIndexValue) then None
    else
      val face = faceIndexValue.asInstanceOf[Int]
      val indices = bundle.pickMesh.selectDynamic("geometry").applyDynamic("getIndex")().selectDynamic("array").asInstanceOf[Uint32Array]
      val positions = bundle.pickMesh.selectDynamic("geometry").applyDynamic("getAttribute")("position")
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
      val triangle = construct("Triangle")(
        construct("Vector3")().applyDynamic("fromBufferAttribute")(positions, candidates(0)),
        construct("Vector3")().applyDynamic("fromBufferAttribute")(positions, candidates(1)),
        construct("Vector3")().applyDynamic("fromBufferAttribute")(positions, candidates(2)))
      val weights = triangle.applyDynamic("getBarycoord")(
        construct("Vector3")(localX, localY, localZ), construct("Vector3")())
      if missing(weights) then return None
      val wa = weights.selectDynamic("x").asInstanceOf[Double]
      val wb = weights.selectDynamic("y").asInstanceOf[Double]
      val wc = weights.selectDynamic("z").asInstanceOf[Double]
      val original = (wa, wb, wc)
      val vertex = SurfaceNearestPartition.nearestVertex(
        bundle.packet.sourceVertex(candidates(0)), bundle.packet.sourceVertex(candidates(1)),
        bundle.packet.sourceVertex(candidates(2)), wa, wb, wc)
      Some(ThreePick(
        bundle.surface,
        face,
        vertex,
        localX,
        localY,
        localZ,
        Some(original)
      ))

  /** Raycast original triangles to avoid introducing pick gaps at display-only seams.
    * The proxy shares the live position attribute and owns only a CPU index buffer.
    */
  private def makePickMesh(packet: SurfaceMeshPacket, mesh: js.Dynamic, geometry: js.Dynamic, material: js.Dynamic): js.Dynamic =
    packet.nearestPartition match
      case None => mesh
      case Some(partition) =>
        val faceCount = partition.originalFaceCount + packet.indices.length / 3 - partition.renderFaceCount
        val indices = new Uint32Array(faceCount * 3)
        var face = 0
        while face < faceCount do
          var corner = 0
          while corner < 3 do
            val renderCorner = if face < partition.originalFaceCount then face * 18 + corner * 6
              else (partition.renderFaceCount + face - partition.originalFaceCount) * 3 + corner
            indices(face * 3 + corner) = packet.indices(renderCorner)
            corner += 1
          face += 1
        val pickGeometry = construct("BufferGeometry")()
        pickGeometry.applyDynamic("setAttribute")("position", geometry.applyDynamic("getAttribute")("position"))
        pickGeometry.applyDynamic("setIndex")(construct("BufferAttribute")(indices, 1))
        pickGeometry.applyDynamic("computeBoundingSphere")()
        val proxy = construct("Mesh")(pickGeometry, material)
        proxy.updateDynamic("matrixAutoUpdate")(false)
        proxy

  private def position(bundle: Bundle, slot: SurfaceViewSlot): Unit =
    bundle.mesh.selectDynamic("position").applyDynamic("set")(
      slot.worldOffsetX,
      slot.worldOffsetY,
      slot.worldOffsetZ
    )
    bundle.mesh.applyDynamic("updateMatrix")()
    bundle.mesh.applyDynamic("updateMatrixWorld")(true)
    bundle.pickMesh.selectDynamic("matrixWorld").applyDynamic("copy")(bundle.mesh.selectDynamic("matrixWorld"))

  private def disposeBundle(bundle: Bundle): Unit =
    scene.applyDynamic("remove")(bundle.mesh)
    if bundle.packet.nearestPartition.nonEmpty then bundle.pickMesh.selectDynamic("geometry").applyDynamic("dispose")()
    bundle.geometry.applyDynamic("dispose")()
    bundle.material.applyDynamic("dispose")()

  private def newMaterial(fragment: Option[ThreeFragmentPacket] = None): js.Dynamic =
    val uniforms = js.Dynamic.literal(
      uAmbient = js.Dynamic.literal(value = 1.0),
      uDiffuse = js.Dynamic.literal(value = 0.0),
      uLightDirection = js.Dynamic.literal(value = construct("Vector3")(0.0, 0.0, 1.0))
    )
    construct("ShaderMaterial")(js.Dynamic.literal(
      uniforms = uniforms,
      vertexShader = fragment.fold(ThreeJsRuntime.VertexShader)(_.vertexShader),
      fragmentShader = fragment.fold(ThreeJsRuntime.FragmentShader)(_.fragmentShader),
      vertexColors = fragment.isEmpty,
      precision = "highp",
      toneMapped = false,
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
  private[three] final case class PositionBounds(minX: Double, minY: Double, minZ: Double,
      maxX: Double, maxY: Double, maxZ: Double)

  private[three] def positionBounds(positions: FloatBufferView): PositionBounds =
    var minX = Double.PositiveInfinity
    var minY = Double.PositiveInfinity
    var minZ = Double.PositiveInfinity
    var maxX = Double.NegativeInfinity
    var maxY = Double.NegativeInfinity
    var maxZ = Double.NegativeInfinity
    var i = 0
    while i < positions.length do
      minX = math.min(minX, positions(i).toDouble)
      minY = math.min(minY, positions(i + 1).toDouble)
      minZ = math.min(minZ, positions(i + 2).toDouble)
      maxX = math.max(maxX, positions(i).toDouble)
      maxY = math.max(maxY, positions(i + 1).toDouble)
      maxZ = math.max(maxZ, positions(i + 2).toDouble)
      i += 3
    PositionBounds(minX, minY, minZ, maxX, maxY, maxZ)

  /** Redistribute unused native depth precision without changing x, y or w.
    * Bounds include every uploaded vertex, including appended network geometry.
    * Positive-w linear-fractional extrema over a box occur at its corners.
    * The envelope allows float32 matrix uploads and both shader matrix products;
    * an eye-plane crossing or an unusable envelope leaves the projection alone.
    * Clamping the occupied interval to [-1, 1] retains requested clipping planes
    * wherever geometry reaches them. The scientific inverse stays unchanged.
    */
  private[three] def depthProjection(projection: js.Array[Double], modelView: js.Array[Double],
      bounds: PositionBounds): js.Array[Double] =
    depthProjection(projection, Vector((modelView, bounds)))

  private[three] def depthProjection(projection: js.Array[Double],
      objects: Vector[(js.Array[Double], PositionBounds)]): js.Array[Double] =
    // Sixteen float epsilons conservatively cover each four-term dot product,
    // coefficient rounding and propagation through the second matrix product.
    val roundoff = 16.0 * math.pow(2.0, -23)
    var low = Double.PositiveInfinity
    var high = Double.NegativeInfinity
    var corner = 0
    while corner < objects.length * 8 do
      val (modelView, bounds) = objects(corner / 8)
      val point = Array(
        if (corner & 1) == 0 then bounds.minX else bounds.maxX,
        if (corner & 2) == 0 then bounds.minY else bounds.maxY,
        if (corner & 4) == 0 then bounds.minZ else bounds.maxZ, 1.0)
      val eye = new Array[Double](4)
      val error = new Array[Double](4)
      var row = 0
      while row < 4 do
        var column = 0
        while column < 4 do
          val term = modelView(column * 4 + row) * point(column)
          eye(row) += term
          error(row) += math.abs(term) * roundoff
          column += 1
        row += 1
      def clip(row: Int): (Double, Double) =
        var value = 0.0
        var uncertainty = 0.0
        var column = 0
        while column < 4 do
          val coefficient = projection(column * 4 + row)
          value += coefficient * eye(column)
          uncertainty += math.abs(coefficient) * (error(column) + math.abs(eye(column)) * roundoff)
          column += 1
        (value, uncertainty)
      val (z, ez) = clip(2)
      val (w, ew) = clip(3)
      if !z.isFinite || !ez.isFinite || !w.isFinite || !ew.isFinite || w - ew <= 0 then return projection
      val depths = Array((z - ez) / (w - ew), (z - ez) / (w + ew),
        (z + ez) / (w - ew), (z + ez) / (w + ew))
      low = math.min(low, depths.min - roundoff)
      high = math.max(high, depths.max + roundoff)
      corner += 1
    low = math.max(-1.0, low)
    high = math.min(1.0, high)
    val span = high - low
    if !span.isFinite || span <= 0 || span >= 1 then projection
    else
      val scale = 2.0 / span
      val offset = -(high + low) / span
      val adjusted = projection.slice(0, projection.length)
      var column = 0
      while column < 4 do
        val i = column * 4
        adjusted(i + 2) = scale * projection(i + 2) + offset * projection(i + 3)
        column += 1
      adjusted

  private[three] final case class NativeViewport(x: Int, y: Int, width: Int, height: Int,
      scaleX: Double, scaleY: Double, offsetX: Double, offsetY: Double)

  private[three] def nativeViewport(viewport: SurfaceViewport, size: ThreeCanvasSize): NativeViewport =
    val targetX = viewport.x * size.width * size.pixelRatio
    val targetY = (1 - viewport.y - viewport.height) * size.height * size.pixelRatio
    val targetWidth = viewport.width * size.width * size.pixelRatio
    val targetHeight = viewport.height * size.height * size.pixelRatio
    val x = math.round(targetX).toInt
    val y = math.round(targetY).toInt
    val width = math.max(1, math.round(targetWidth).toInt)
    val height = math.max(1, math.round(targetHeight).toInt)
    NativeViewport(x, y, width, height, targetWidth / width, targetHeight / height,
      (2 * (targetX - x) + targetWidth - width) / width,
      (2 * (targetY - y) + targetHeight - height) / height)

  private[three] def viewportProjection(matrix: js.Array[Double], viewport: NativeViewport): js.Array[Double] =
    val adjusted = matrix.slice(0, matrix.length)
    var column = 0
    while column < 4 do
      val offset = column * 4
      adjusted(offset) = viewport.scaleX * matrix(offset) + viewport.offsetX * matrix(offset + 3)
      adjusted(offset + 1) = viewport.scaleY * matrix(offset + 1) + viewport.offsetY * matrix(offset + 3)
      column += 1
    adjusted

  private val VertexShader =
    """
      |uniform float uAmbient;
      |uniform float uDiffuse;
      |uniform vec3 uLightDirection;
      |centroid varying vec3 vColor;
      |void main() {
      |  // Generated nearest corners carry interpolated source normals. Preserve
      |  // the reference legacy vertex-lighting contract without renormalizing.
      |  float shade = min(1.0, uAmbient + uDiffuse * max(0.0, dot(normal, normalize(uLightDirection))));
      |  vColor = color * shade;
      |  gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);
      |}
      |""".stripMargin

  private val FragmentShader =
    """
      |centroid varying vec3 vColor;
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
        ThreeDepthAdmission.check(three, renderer) match
          case Left(error) =>
            renderer.applyDynamic("dispose")()
            return Left(error)
          case Right(_) => ()
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
