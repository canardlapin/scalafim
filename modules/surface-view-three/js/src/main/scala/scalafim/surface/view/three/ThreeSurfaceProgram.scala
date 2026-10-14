package scalafim.surface.view.three

import intaglio.*
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
  case NativeDepthMismatch(samples: Int, red: Int, green: Int, blue: Int)

  def message: String =
    this match
      case InvalidCanvasSize(width, height, ratio) =>
        s"invalid Three.js canvas size ${width}x$height at pixel ratio $ratio"
      case ContextUnavailable => "WebGL is unavailable; use surface-view-raster as the deterministic fallback"
      case ContextLost => "the WebGL context is lost; restore it before rendering"
      case InvalidPlan(reason) => s"invalid surface render plan: $reason"
      case RuntimeFailure(operation, reason) => s"Three.js $operation failed: $reason"
      case BackendDisposed => "the Three.js surface backend has been disposed"
      case NativeDepthMismatch(samples, red, green, blue) =>
        s"native depth verification failed with $samples samples: expected red foreground, observed RGB($red,$green,$blue); use hardware WebGL, a host canvas created without antialiasing, or the reference raster"

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
  case UploadFragments(packets: Vector[ThreeFragmentPacket])
  case UpdateLighting(lighting: SurfaceLighting)
  case UpdateCamera(camera: SurfaceCameraPacket, clipping: SurfaceClipping)
  case UpdateLayout(slots: Vector[SurfaceViewSlot], fit: SurfaceViewportFit = SurfaceViewportFit.Fill)
  case UpdateComposition(plan: SurfaceCompositionPlan)
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
  receipt: SurfaceRenderReceipt,
  resourcePlan: Option[SurfaceRenderPlan] = None,
  composition: Option[SurfaceCompositionPlan] = None
)

object ThreeSurfaceProgram:
  /** Compiles composition resources through the established single-plan
    * uploader, then replaces legacy camera/layout commands with one native
    * composition update. This keeps resource dirtiness identical to legacy
    * rendering while allowing the runtime to own display occurrences.
    */
  def compileComposition(
    previous: Option[(SurfaceRenderPlan, ThreeCanvasSize)],
    previousComposition: Option[SurfaceCompositionPlan],
    next: SurfaceCompositionPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean = false
  ): Either[ThreeSurfaceError, ThreeSurfaceProgram] =
    val reuse = previous.fold[Either[SurfaceCompositionError, Unit]](Right(())): (plan, _) =>
      SurfaceCompositionPlan.validateResourceReuse(SurfaceCompositionResources(
        plan.meshes.map(m => m.resourceKey -> m).toMap, plan.layers.map(l => l.resourceKey -> l).toMap), next.resources)
    reuse.left.map(error => ThreeSurfaceError.InvalidPlan(s"immutable composition resource changed: $error"))
      .flatMap(_ => compositionProxy(next)).flatMap: proxy =>
      compile(previous, proxy, size, forceDraw).map: base =>
        val viewChanged = previousComposition.forall(_.views != next.views)
        val changed = viewChanged || !base.dirty.isClean
        val retained = base.commands.filter:
          case ThreeSurfaceCommand.UpdateLighting(_) | ThreeSurfaceCommand.UpdateCamera(_, _) |
              ThreeSurfaceCommand.UpdateLayout(_, _) => false
          case _ => true
        val commands =
          if changed then
            val beforeDraw = retained.takeWhile(_ != ThreeSurfaceCommand.Draw)
            val afterDraw = retained.drop(beforeDraw.length)
            val tail = if afterDraw.nonEmpty then afterDraw else Vector(ThreeSurfaceCommand.Draw)
            beforeDraw ++ Vector(ThreeSurfaceCommand.UpdateComposition(next)) ++ tail
          else retained
        ThreeSurfaceProgram(commands, base.dirty.copy(camera = base.dirty.camera || viewChanged),
          base.receipt, Some(proxy), Some(next))

  private def compositionProxy(composition: SurfaceCompositionPlan): Either[ThreeSurfaceError, SurfaceRenderPlan] =
    if composition.views.map(view => (view.colorPipeline, view.surfaceBase)).distinct.length > 1 then
      return Left(ThreeSurfaceError.InvalidPlan("native composition requires a common color pipeline and surface base"))
    if composition.views.exists(_.instances.exists(_.opacity != DisplayOpacity.Opaque)) || composition.views.exists(_.surfaceBase.alpha != 255) then
      return Left(ThreeSurfaceError.InvalidPlan("surface alpha requires the Canvas raster host"))
    val active = composition.views.flatMap(view => view.instances.map(instance => (view, instance)))
    val descriptions = scala.collection.mutable.LinkedHashMap.empty[SurfaceId, (SurfaceResourceKey, Vector[SurfaceResourceKey], Boolean)]
    var error: Option[ThreeSurfaceError] = None
    active.foreach: (view, instance) =>
      if error.isEmpty then
        if view.clipping match
            case SurfaceClipping.WorldPlanes(planes) => planes.length > 8 || view.instances.exists(i => !view.fragmentSurfaces(i.surface))
            case _ => false
        then error = Some(ThreeSurfaceError.InvalidPlan("world clipping requires fragment packets and at most eight planes"))
        else
          val description = (instance.mesh, instance.layerOrder, view.fragmentSurfaces(instance.surface))
          descriptions.get(instance.surface) match
            case Some(existing) if existing != description =>
              error = Some(ThreeSurfaceError.InvalidPlan(
                s"surface '${instance.surface.value}' has incompatible mesh, layers, or fragment policy across composition instances"))
            case None => descriptions(instance.surface) = description
            case _ => ()
    error match
      case Some(value) => Left(value)
      case None =>
        val meshes = descriptions.iterator.map { case (_, (mesh, _, _)) =>
          composition.resources.meshes(mesh) }.toVector
        val layers = descriptions.iterator.flatMap { case (_, (_, keys, _)) => keys.map(composition.resources.layers) }.toVector
        val first = composition.views.head
        val slots = meshes.map(mesh => SurfaceViewSlot(mesh.surface, SurfaceViewport(0.0, 0.0, 1.0, 1.0)))
        val fragments = descriptions.iterator.collect:
          case (surface, (_, _, true)) => surface
        .toSet
        val profile = SurfaceProfile(meshes.length, meshes.map(_.positions.length / 3).sum,
          meshes.map(_.indices.length / 3).sum, layers.length, layers.map(_.colors.length).sum, 0L)
        val receipt = SurfaceRenderReceipt(meshes.map(_.resourceKey), layers.map(_.resourceKey),
          "composition-resource-proxy", 0, 0)
        Right(SurfaceRenderPlan(slots, meshes, layers, first.camera, first.lighting, SurfaceClipping.Disabled,
          Vector.empty, first.chrome, first.readouts, profile, receipt, first.viewportFit, fragments, first.colorPipeline, Map.empty, first.surfaceBase))
  def compile(
    previous: Option[(SurfaceRenderPlan, ThreeCanvasSize)],
    next: SurfaceRenderPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean = false
  ): Either[ThreeSurfaceError, ThreeSurfaceProgram] =
    SurfaceThresholdEdgeValidation.validate(next).left.map(ThreeSurfaceError.InvalidPlan.apply).flatMap: _ =>
      validate(next).flatMap: _ =>
        previous match
          case None => initial(next, size)
          case Some((before, beforeSize)) => incremental(before, beforeSize, next, size, forceDraw)

  private def geometry(plan: SurfaceRenderPlan): Vector[SurfaceMeshPacket] =
    plan.meshes.map(mesh => if plan.fragmentSurfaces(mesh.surface) then ThreeFragmentShader.geometry(mesh) else mesh)

  private def layerCommands(plan: SurfaceRenderPlan): Either[ThreeSurfaceError, Vector[ThreeSurfaceCommand]] =
    val fragments = Vector.newBuilder[ThreeFragmentPacket]
    var error: Option[ThreeSurfaceError] = None
    plan.meshes.filter(mesh => plan.fragmentSurfaces(mesh.surface)).foreach: mesh =>
      if error.isEmpty then ThreeFragmentShader.compile(mesh, plan.layers, plan.colorPipeline, plan.surfaceBase) match
        case Left(value) => error = Some(value)
        case Right(value) => fragments += value
    error match
      case Some(value) => Left(value)
      case None =>
        val colors = ThreeSurfaceCompositor.colors(plan.copy(meshes = plan.meshes.filterNot(mesh => plan.fragmentSurfaces(mesh.surface))))
        val shaders = fragments.result()
        Right(Option.when(colors.nonEmpty)(ThreeSurfaceCommand.UploadColors(colors)).toVector ++
          Option.when(shaders.nonEmpty)(ThreeSurfaceCommand.UploadFragments(shaders)))

  private def initial(next: SurfaceRenderPlan, size: ThreeCanvasSize): Either[ThreeSurfaceError, ThreeSurfaceProgram] =
    layerCommands(next).map: data =>
      val commands = Vector(ThreeSurfaceCommand.UploadGeometry(geometry(next))) ++ data ++ Vector(
      ThreeSurfaceCommand.UpdateLighting(next.lighting),
      ThreeSurfaceCommand.UpdateCamera(next.camera, next.clipping),
      ThreeSurfaceCommand.UpdateLayout(next.slots, next.viewportFit),
      ThreeSurfaceCommand.Resize(size),
      ThreeSurfaceCommand.Draw
    )
      ThreeSurfaceProgram(commands, ThreeDirtySet(geometry = true, layerData = true, material = true,
        camera = true, layout = true, clipping = next.clipping != SurfaceClipping.Disabled,
        canvas = true, removedResources = Vector.empty), next.receipt)

  private def incremental(
    before: SurfaceRenderPlan,
    beforeSize: ThreeCanvasSize,
    next: SurfaceRenderPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean
  ): Either[ThreeSurfaceError, ThreeSurfaceProgram] =
    val topology = before.receipt.meshKeys != next.receipt.meshKeys || before.fragmentSurfaces != next.fragmentSurfaces
    val geometry = topology || before.meshes.map(_.geometryKey) != next.meshes.map(_.geometryKey)
    val layerData = layerSignature(before) != layerSignature(next) || before.colorPipeline != next.colorPipeline || before.surfaceBase != next.surfaceBase
    val material = before.lighting != next.lighting
    val camera = before.receipt.cameraKey != next.receipt.cameraKey
    val layout = before.slots != next.slots || before.viewportFit != next.viewportFit
    val clipping = before.clipping != next.clipping
    val canvas = beforeSize != size
    val nextKeys = (next.receipt.meshKeys ++ next.receipt.layerKeys).toSet
    val removed = (before.receipt.meshKeys ++ before.receipt.layerKeys).filterNot(nextKeys)
    val commands = Vector.newBuilder[ThreeSurfaceCommand]
    if removed.nonEmpty then commands += ThreeSurfaceCommand.DisposeResources(removed)
    if topology then commands += ThreeSurfaceCommand.UploadGeometry(ThreeSurfaceProgram.geometry(next))
    else if geometry then commands += ThreeSurfaceCommand.UpdateGeometry(ThreeSurfaceProgram.geometry(next))
    if topology || layerData then
      layerCommands(next) match
        case Left(error) => return Left(error)
        case Right(data) => commands ++= data
    if topology || material || (layerData && next.fragmentSurfaces.nonEmpty) then commands += ThreeSurfaceCommand.UpdateLighting(next.lighting)
    if camera || clipping then commands += ThreeSurfaceCommand.UpdateCamera(next.camera, next.clipping)
    if layout then commands += ThreeSurfaceCommand.UpdateLayout(next.slots, next.viewportFit)
    if canvas then commands += ThreeSurfaceCommand.Resize(size)
    val dirty = ThreeDirtySet(geometry, layerData, material, camera, layout, clipping, canvas, removed)
    if !dirty.isClean || forceDraw then commands += ThreeSurfaceCommand.Draw
    Right(ThreeSurfaceProgram(commands.result(), dirty, next.receipt))

  private def validate(plan: SurfaceRenderPlan): Either[ThreeSurfaceError, Unit] =
    if plan.layers.exists(layer => (layer.interpolation == SurfaceMapInterpolation.VertexScalar || layer.scalarField.nonEmpty) && !plan.fragmentSurfaces(layer.surface)) then
      Left(ThreeSurfaceError.InvalidPlan("scalar layer requires fragment surface semantics"))
    else if plan.slots.length != plan.meshes.length then
      Left(ThreeSurfaceError.InvalidPlan("slot and mesh counts differ"))
    else if plan.surfaceBase.alpha != 255 || plan.surfaceOpacities.values.exists(_ != DisplayOpacity.Opaque) then
      Left(ThreeSurfaceError.InvalidPlan("surface alpha requires the Canvas raster host"))
    else if plan.clipping match
        case SurfaceClipping.WorldPlanes(planes) => planes.length > 8 || plan.meshes.exists(mesh => !plan.fragmentSurfaces(mesh.surface))
        case _ => false
    then Left(ThreeSurfaceError.InvalidPlan("world clipping requires fragment packets and at most eight planes"))
    else if (plan.colorPipeline != SurfaceColorPipeline.LegacyDisplayBytes || plan.surfaceBase != SurfaceDisplayDefaults.LegacySurfaceBase) &&
        plan.meshes.exists(mesh => !plan.fragmentSurfaces(mesh.surface)) then
      Left(ThreeSurfaceError.InvalidPlan("explicit color policies require fragment packets"))
    else if plan.camera.viewMatrix.length != 16 || plan.camera.projectionMatrix.length != 16 then
      Left(ThreeSurfaceError.InvalidPlan("camera matrices must be 4x4"))
    else if plan.meshes.exists(mesh => mesh.positions.length % 3 != 0 || mesh.normals.length != mesh.positions.length) then
      Left(ThreeSurfaceError.InvalidPlan("mesh position and normal buffers are inconsistent"))
    else Right(())

  private def layerSignature(plan: SurfaceRenderPlan): Vector[(SurfaceResourceKey, Double, String, SurfaceLayerCoverage)] =
    plan.layers.map(layer => (layer.resourceKey, layer.opacity.toDouble, layer.blendMode.toString, layer.coverage))

private object ThreeSurfaceCompositor:
  private val Base = Rgba32.unsafe(184, 184, 184)

  def colors(plan: SurfaceRenderPlan): Vector[ThreeSurfaceColors] =
    plan.meshes.map: mesh =>
      val vertexCount = mesh.positions.length / 3
      val packed = Array.fill(vertexCount)(Base.toPackedInt)
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
            packed(vertex) = layer.blendMode.composite(under, over, layer.opacity).toPackedInt
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
    Rgba32.fromPackedInt(packed)
