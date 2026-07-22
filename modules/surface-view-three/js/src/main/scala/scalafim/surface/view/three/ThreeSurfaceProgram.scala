package scalafim.surface.view.three

import scalafim.graphics.*
import scalafim.surface.view.*

final case class ThreeCanvasSize private (width: Int, height: Int, pixelRatio: Double)

object ThreeCanvasSize:
  def publication(preset: SurfacePublicationPreset, pixelRatio: Double = 1.0): Either[ThreeSurfaceError, ThreeCanvasSize] =
    make(preset.width, preset.height, pixelRatio)

  def make(width: Int, height: Int, pixelRatio: Double = 1.0): Either[ThreeSurfaceError, ThreeCanvasSize] =
    if width <= 0 || height <= 0 || !pixelRatio.isFinite || pixelRatio <= 0.0 then
      Left(ThreeSurfaceError.InvalidCanvasSize(width, height, pixelRatio))
    else Right(new ThreeCanvasSize(width, height, pixelRatio))

  def unsafe(width: Int, height: Int, pixelRatio: Double = 1.0): ThreeCanvasSize =
    make(width, height, pixelRatio).fold(error => throw new IllegalArgumentException(error.message), identity)

enum ThreeContextState:
  case Available, Lost, Absent

enum ThreeSurfaceError:
  case InvalidCanvasSize(width: Int, height: Int, pixelRatio: Double)
  case ContextUnavailable
  case ContextLost
  case InvalidPlan(reason: String)
  case RuntimeFailure(operation: String, reason: String)
  case BackendDisposed

  def message: String =
    this match
      case InvalidCanvasSize(width, height, ratio) =>
        s"invalid Three.js canvas size ${width}x$height at pixel ratio $ratio"
      case ContextUnavailable => "WebGL is unavailable; use surface-view-raster as the deterministic fallback"
      case ContextLost => "the WebGL context is lost; restore it before rendering"
      case InvalidPlan(reason) => s"invalid surface render plan: $reason"
      case RuntimeFailure(operation, reason) => s"Three.js $operation failed: $reason"
      case BackendDisposed => "the Three.js surface backend has been disposed"

final case class ThreeDirtySet(
  geometry: Boolean,
  layerData: Boolean,
  material: Boolean,
  camera: Boolean,
  layout: Boolean,
  clipping: Boolean,
  canvas: Boolean,
  removedResources: Vector[SurfaceResourceKey]
):
  def isClean: Boolean =
    !geometry && !layerData && !material && !camera && !layout && !clipping && !canvas && removedResources.isEmpty

enum ThreeSurfaceCommand:
  case DisposeResources(keys: Vector[SurfaceResourceKey])
  case UploadGeometry(meshes: Vector[SurfaceMeshPacket])
  case UpdateGeometry(meshes: Vector[SurfaceMeshPacket])
  case UploadColors(colors: Vector[ThreeSurfaceColors])
  case UpdateLighting(lighting: SurfaceLighting)
  case UpdateCamera(camera: SurfaceCameraPacket, clipping: SurfaceClipping)
  case UpdateLayout(slots: Vector[SurfaceViewSlot])
  case Resize(size: ThreeCanvasSize)
  case Draw

final case class ThreeSurfaceColors(
  surface: SurfaceId,
  resourceKeys: Vector[SurfaceResourceKey],
  rgb: Array[Float]
):
  override def equals(other: Any): Boolean =
    other match
      case that: ThreeSurfaceColors =>
        surface == that.surface && resourceKeys == that.resourceKeys && java.util.Arrays.equals(rgb, that.rgb)
      case _ => false

  override def hashCode: Int =
    31 * (31 * surface.hashCode + resourceKeys.hashCode) + java.util.Arrays.hashCode(rgb)

final case class ThreeSurfaceProgram(
  commands: Vector[ThreeSurfaceCommand],
  dirty: ThreeDirtySet,
  receipt: SurfaceRenderReceipt
)

object ThreeSurfaceProgram:
  def compile(
    previous: Option[(SurfaceRenderPlan, ThreeCanvasSize)],
    next: SurfaceRenderPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean = false
  ): Either[ThreeSurfaceError, ThreeSurfaceProgram] =
    validate(next).map: _ =>
      previous match
        case None => initial(next, size)
        case Some((before, beforeSize)) => incremental(before, beforeSize, next, size, forceDraw)

  private def initial(next: SurfaceRenderPlan, size: ThreeCanvasSize): ThreeSurfaceProgram =
    val commands = Vector(
      ThreeSurfaceCommand.UploadGeometry(next.meshes),
      ThreeSurfaceCommand.UploadColors(ThreeSurfaceCompositor.colors(next)),
      ThreeSurfaceCommand.UpdateLighting(next.lighting),
      ThreeSurfaceCommand.UpdateCamera(next.camera, next.clipping),
      ThreeSurfaceCommand.UpdateLayout(next.slots),
      ThreeSurfaceCommand.Resize(size),
      ThreeSurfaceCommand.Draw
    )
    ThreeSurfaceProgram(
      commands,
      ThreeDirtySet(
        geometry = true,
        layerData = true,
        material = true,
        camera = true,
        layout = true,
        clipping = next.clipping != SurfaceClipping.Disabled,
        canvas = true,
        removedResources = Vector.empty
      ),
      next.receipt
    )

  private def incremental(
    before: SurfaceRenderPlan,
    beforeSize: ThreeCanvasSize,
    next: SurfaceRenderPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean
  ): ThreeSurfaceProgram =
    val topology = before.receipt.meshKeys != next.receipt.meshKeys
    val geometry = topology || before.meshes.map(_.geometryKey) != next.meshes.map(_.geometryKey)
    val layerData = layerSignature(before) != layerSignature(next)
    val material = before.lighting != next.lighting
    val camera = before.receipt.cameraKey != next.receipt.cameraKey
    val layout = before.slots != next.slots
    val clipping = before.clipping != next.clipping
    val canvas = beforeSize != size
    val nextKeys = (next.receipt.meshKeys ++ next.receipt.layerKeys).toSet
    val removed = (before.receipt.meshKeys ++ before.receipt.layerKeys).filterNot(nextKeys)
    val commands = Vector.newBuilder[ThreeSurfaceCommand]
    if removed.nonEmpty then commands += ThreeSurfaceCommand.DisposeResources(removed)
    if topology then commands += ThreeSurfaceCommand.UploadGeometry(next.meshes)
    else if geometry then commands += ThreeSurfaceCommand.UpdateGeometry(next.meshes)
    if topology || layerData then commands += ThreeSurfaceCommand.UploadColors(ThreeSurfaceCompositor.colors(next))
    if material then commands += ThreeSurfaceCommand.UpdateLighting(next.lighting)
    if camera || clipping then commands += ThreeSurfaceCommand.UpdateCamera(next.camera, next.clipping)
    if layout then commands += ThreeSurfaceCommand.UpdateLayout(next.slots)
    if canvas then commands += ThreeSurfaceCommand.Resize(size)
    val dirty = ThreeDirtySet(geometry, layerData, material, camera, layout, clipping, canvas, removed)
    if !dirty.isClean || forceDraw then commands += ThreeSurfaceCommand.Draw
    ThreeSurfaceProgram(commands.result(), dirty, next.receipt)

  private def validate(plan: SurfaceRenderPlan): Either[ThreeSurfaceError, Unit] =
    if plan.slots.length != plan.meshes.length then
      Left(ThreeSurfaceError.InvalidPlan("slot and mesh counts differ"))
    else if plan.clipping.isInstanceOf[SurfaceClipping.WorldPlanes] then
      Left(ThreeSurfaceError.InvalidPlan(
        "world clipping planes are unsupported by the Three.js backend; use the reference raster backend"
      ))
    else if plan.camera.viewMatrix.length != 16 || plan.camera.projectionMatrix.length != 16 then
      Left(ThreeSurfaceError.InvalidPlan("camera matrices must be 4x4"))
    else if plan.meshes.exists(mesh => mesh.positions.length % 3 != 0 || mesh.normals.length != mesh.positions.length) then
      Left(ThreeSurfaceError.InvalidPlan("mesh position and normal buffers are inconsistent"))
    else Right(())

  private def layerSignature(plan: SurfaceRenderPlan): Vector[(SurfaceResourceKey, Double, String)] =
    plan.layers.map(layer => (layer.resourceKey, layer.opacity.toDouble, layer.blendMode.toString))

private object ThreeSurfaceCompositor:
  private val Base = Rgba32.unsafe(184, 184, 184)

  def colors(plan: SurfaceRenderPlan): Vector[ThreeSurfaceColors] =
    plan.meshes.map: mesh =>
      val vertexCount = mesh.positions.length / 3
      val packed = Array.fill(vertexCount)(Base.packedInt)
      val keys = Vector.newBuilder[SurfaceResourceKey]
      var layerIndex = 0
      while layerIndex < plan.layers.length do
        val layer = plan.layers(layerIndex)
        if layer.surface == mesh.surface then
          keys += layer.resourceKey
          var vertex = 0
          while vertex < vertexCount do
            val under = rgba(packed(vertex))
            val over = rgba(layer.colors(vertex))
            packed(vertex) = layer.blendMode.composite(under, over, layer.opacity).packedInt
            vertex += 1
        layerIndex += 1
      val rgb = new Array[Float](vertexCount * 3)
      var vertex = 0
      while vertex < vertexCount do
        val pixel = packed(vertex)
        val offset = vertex * 3
        rgb(offset) = ((pixel >>> 24) & 0xff).toFloat / 255.0f
        rgb(offset + 1) = ((pixel >>> 16) & 0xff).toFloat / 255.0f
        rgb(offset + 2) = ((pixel >>> 8) & 0xff).toFloat / 255.0f
        vertex += 1
      ThreeSurfaceColors(mesh.surface, keys.result(), rgb)

  private def rgba(packed: Int): Rgba32 =
    Rgba32.packUnsafe(
      (packed >>> 24) & 0xff,
      (packed >>> 16) & 0xff,
      (packed >>> 8) & 0xff,
      packed & 0xff
    )
