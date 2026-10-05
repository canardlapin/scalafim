package scalafim.surface.view.javafx

import javafx.application.{ConditionalFeature, Platform}
import javafx.scene.{Group, SceneAntialiasing, SnapshotParameters, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color

import scalafim.surface.view.*

final case class JavaFxDirtySet(
  geometry: Boolean,
  layerData: Boolean,
  material: Boolean,
  camera: Boolean,
  layout: Boolean,
  clipping: Boolean,
  removedResources: Vector[SurfaceResourceKey]
):
  def isClean: Boolean =
    !geometry && !layerData && !material && !camera && !layout && !clipping && removedResources.isEmpty

enum JavaFxSurfaceCommand:
  case RebuildGeometry(plan: SurfaceRenderPlan)
  case UpdateGeometry(plan: SurfaceRenderPlan)
  case UpdateAtlases(plan: SurfaceRenderPlan)
  case UpdateMaterial(mode: JavaFxMaterialMode)
  case UpdateCamera(plan: SurfaceRenderPlan)
  case UpdateLayout(slots: Vector[SurfaceViewSlot])
  case DisposeResources(keys: Vector[SurfaceResourceKey])

final case class JavaFxSurfaceProgram(
  commands: Vector[JavaFxSurfaceCommand],
  dirty: JavaFxDirtySet,
  receipt: SurfaceRenderReceipt
)

object JavaFxSurfaceProgram:
  def compile(
    previous: Option[SurfaceRenderPlan],
    next: SurfaceRenderPlan,
    config: JavaFxAtlasConfig = JavaFxAtlasConfig.Default
  ): JavaFxSurfaceProgram =
    previous match
      case None =>
        JavaFxSurfaceProgram(
          Vector(
            JavaFxSurfaceCommand.RebuildGeometry(next),
            JavaFxSurfaceCommand.UpdateMaterial(materialMode(next)),
            JavaFxSurfaceCommand.UpdateCamera(next),
            JavaFxSurfaceCommand.UpdateLayout(next.slots)
          ),
          JavaFxDirtySet(
            geometry = true,
            layerData = true,
            material = true,
            camera = true,
            layout = true,
            clipping = next.clipping != SurfaceClipping.Disabled,
            removedResources = Vector.empty
          ),
          next.receipt
        )
      case Some(before) =>
        val topology = before.receipt.meshKeys != next.receipt.meshKeys
        val geometry = topology || before.meshes.map(_.geometryKey) != next.meshes.map(_.geometryKey)
        val material = before.lighting != next.lighting
        val layerData = layerSignature(before) != layerSignature(next) ||
          (config.lightingPolicy == JavaFxAtlasLighting.WorldVertexLambert && (material || geometry))
        val camera = before.receipt.cameraKey != next.receipt.cameraKey
        val layout = before.slots != next.slots
        val clipping = before.clipping != next.clipping
        val nextKeys = (next.receipt.meshKeys ++ next.receipt.layerKeys).toSet
        val removed = (before.receipt.meshKeys ++ before.receipt.layerKeys).filterNot(nextKeys)
        val commands = Vector.newBuilder[JavaFxSurfaceCommand]
        if removed.nonEmpty then commands += JavaFxSurfaceCommand.DisposeResources(removed)
        if topology then commands += JavaFxSurfaceCommand.RebuildGeometry(next)
        else if geometry then commands += JavaFxSurfaceCommand.UpdateGeometry(next)
        if layerData && !topology then commands += JavaFxSurfaceCommand.UpdateAtlases(next)
        if material then commands += JavaFxSurfaceCommand.UpdateMaterial(materialMode(next))
        if camera || clipping then commands += JavaFxSurfaceCommand.UpdateCamera(next)
        if layout then commands += JavaFxSurfaceCommand.UpdateLayout(next.slots)
        JavaFxSurfaceProgram(
          commands.result(),
          JavaFxDirtySet(geometry, layerData, material, camera, layout, clipping, removed),
          next.receipt
        )

  private def layerSignature(plan: SurfaceRenderPlan): Vector[(SurfaceResourceKey, Double, String)] =
    plan.layers.map(layer => (layer.resourceKey, layer.opacity.toDouble, layer.blendMode.toString))

  private[javafx] def materialMode(plan: SurfaceRenderPlan): JavaFxMaterialMode =
    plan.lighting match
      case SurfaceLighting.Unlit => JavaFxMaterialMode.Unlit
      case _ => JavaFxMaterialMode.Lit

final case class JavaFxCapabilityReport(
  scene3d: Boolean,
  depthBuffer: Boolean,
  antialiasing: Boolean,
  fallback: Option[String]
):

  def admissionCapabilities: SurfaceBackendCapabilities =
    val available =
      if scene3d then Set(
        SurfaceBackendFeature.HardwareAcceleration,
        SurfaceBackendFeature.DepthBuffer,
        SurfaceBackendFeature.BackFaceCulling,
        SurfaceBackendFeature.Lighting,
        SurfaceBackendFeature.BilateralViewports,
        SurfaceBackendFeature.NativePicking,
        SurfaceBackendFeature.HighResolutionSnapshot
      )
      else Set.empty[SurfaceBackendFeature]
    SurfaceBackendCapabilities(
      SurfaceBackendId.unsafe("javafx-scene3d"),
      SurfacePlanRevision.Current,
      available,
      fallback.toVector ++ Vector("world clipping planes are not yet supported")
    )

object JavaFxCapabilityReport:
  def current: JavaFxCapabilityReport =
    val scene3d = Platform.isSupported(ConditionalFeature.SCENE3D)
    JavaFxCapabilityReport(
      scene3d = scene3d,
      depthBuffer = scene3d,
      antialiasing = scene3d,
      fallback = if scene3d then None else Some("use surface-view-raster for deterministic headless rendering")
    )

final case class JavaFxSnapshotConfig private (
  width: Int,
  height: Int,
  antialiasing: SceneAntialiasing,
  transparent: Boolean
)

object JavaFxSnapshotConfig:
  def publication(
    preset: SurfacePublicationPreset,
    antialiasing: SceneAntialiasing = SceneAntialiasing.BALANCED,
    transparent: Boolean = false
  ): JavaFxSnapshotConfig =
    new JavaFxSnapshotConfig(preset.width, preset.height, antialiasing, transparent)

  def make(
    width: Int,
    height: Int,
    antialiasing: SceneAntialiasing = SceneAntialiasing.BALANCED,
    transparent: Boolean = false
  ): Either[JavaFxSurfaceError, JavaFxSnapshotConfig] =
    if width <= 0 || height <= 0 then Left(JavaFxSurfaceError.IncompatiblePlan(s"invalid snapshot size ${width}x$height"))
    else Right(new JavaFxSnapshotConfig(width, height, antialiasing, transparent))

final case class JavaFxInterpretReceipt(
  dirty: JavaFxDirtySet,
  commandsApplied: Int,
  resourceCount: Int,
  atlasUpdates: Int,
  geometryUpdates: Int,
  geometryBytesUpdated: Long,
  elapsedNanos: Long,
  textureCoordinateBytesUpdated: Long = 0L
)

final case class JavaFxObservedReceipt(
  native: JavaFxInterpretReceipt,
  observation: SurfaceBackendObservation
)

/** FX-thread interpreter and host hook. It owns neither Application nor Stage;
  * callers mount `root` or request a SubScene/snapshot explicitly.
  */
final class JavaFxSurfaceBackend private (
  val root: Group,
  val capabilities: JavaFxCapabilityReport,
  config: JavaFxAtlasConfig
):
  private var currentPlan: Option[SurfaceRenderPlan] = None
  private var current: Option[JavaFxSurfaceProbeResult] = None
  private var mountedScene: Option[SubScene] = None
  private var disposed = false

  def render(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxInterpretReceipt] =
    JavaFxSurfaceBackend.validateCapabilities(plan).flatMap(_ => requireFxThread()).flatMap: _ =>
      if disposed then Left(JavaFxSurfaceError.IncompatiblePlan("backend has been disposed"))
      else scala.util.boundary[Either[JavaFxSurfaceError, JavaFxInterpretReceipt]]:
        val started = System.nanoTime()
        val incremental = JavaFxSurfaceProgram.compile(currentPlan, plan, config)
        val rebuildsGeometry = incremental.commands.exists:
          case JavaFxSurfaceCommand.RebuildGeometry(_) => true
          case _ => false
        val updatesBuffers = incremental.commands.exists:
          case JavaFxSurfaceCommand.UpdateAtlases(_) | JavaFxSurfaceCommand.UpdateGeometry(_) => true
          case _ => false
        val needsReplacement =
          if updatesBuffers && !rebuildsGeometry then current.get.requiresAtlasRebuild(plan, JavaFxSurfaceProgram.materialMode(plan))
          else Right(false)
        val program = needsReplacement match
          case Left(error) => scala.util.boundary.break(Left(error))
          case Right(false) => incremental
          case Right(true) =>
            // Complete construction and resource validation precede the scene swap.
            val full = JavaFxSurfaceProgram.compile(None, plan, config)
            full.copy(dirty = full.dirty.copy(removedResources = incremental.dirty.removedResources))
        var atlasUpdates = 0
        var geometryUpdates = 0
        var geometryBytesUpdated = 0L
        var textureCoordinateBytesUpdated = 0L
        var index = 0
        var failure: Option[JavaFxSurfaceError] = None
        while index < program.commands.length && failure.isEmpty do
          program.commands(index) match
            case JavaFxSurfaceCommand.DisposeResources(_) => ()
            case JavaFxSurfaceCommand.RebuildGeometry(next) =>
              JavaFxSurfaceProbe.compile(next, JavaFxSurfaceProgram.materialMode(next), config) match
                case Left(error) => failure = Some(error)
                case Right(probe) =>
                  mountedScene.foreach: scene =>
                    probe.setViewportSize(scene.getWidth.toInt, scene.getHeight.toInt)
                    probe.attachCamera(scene)
                  current.foreach(_.root.getChildren.clear())
                  current = Some(probe)
                  root.getChildren.setAll(probe.root)
            case JavaFxSurfaceCommand.UpdateGeometry(next) =>
              current.get.updateGeometry(next, JavaFxSurfaceProgram.materialMode(next)) match
                case Left(error) => failure = Some(error)
                case Right(receipt) =>
                  geometryUpdates += 1
                  geometryBytesUpdated += receipt.bytesUpdated
            case JavaFxSurfaceCommand.UpdateAtlases(next) =>
              current.get.updateColors(next, commit = true, mode = JavaFxSurfaceProgram.materialMode(next)) match
                case Left(error) => failure = Some(error)
                case Right(receipt) =>
                  atlasUpdates += receipt.atlasesUpdated
                  textureCoordinateBytesUpdated += receipt.textureCoordinateBytesUpdated
            case JavaFxSurfaceCommand.UpdateMaterial(mode) =>
              current.foreach: probe =>
                probe.setMaterialMode(mode) match
                  case Left(error) => failure = Some(error)
                  case Right(_) => ()
            case JavaFxSurfaceCommand.UpdateCamera(next) =>
              current.get.applyCamera(next) match
                case Left(error) => failure = Some(error)
                case Right(_) => ()
            case JavaFxSurfaceCommand.UpdateLayout(slots) => current.foreach(_.setLayout(slots))
          index += 1
        failure match
          case Some(error) => Left(error)
          case None =>
            currentPlan = Some(plan)
            Right(JavaFxInterpretReceipt(
              program.dirty,
              program.commands.length,
              resourceKeys.size,
              atlasUpdates,
              geometryUpdates,
              geometryBytesUpdated,
              System.nanoTime() - started,
              textureCoordinateBytesUpdated
            ))

  def renderObserved(
    plan: SurfaceRenderPlan,
    path: SurfaceAdmissionPath
  ): Either[JavaFxSurfaceError, JavaFxObservedReceipt] =
    val previous = currentPlan
    render(plan).map: receipt =>
      val events = Vector.newBuilder[SurfaceResourceEvent]
      if receipt.dirty.geometry && receipt.geometryUpdates == 0 then
        current.toVector.flatMap(_.chunks).foreach: chunk =>
          events += SurfaceResourceEvent.MeshUploaded(
            chunk.meshKey,
            chunk.mesh.getPoints.size() / 3,
            chunk.renderedFaceCount,
            (chunk.mesh.getPoints.size().toLong + chunk.mesh.getNormals.size() +
              chunk.mesh.getTexCoords.size() + chunk.mesh.getFaces.size()) * 4L
          )
      if receipt.dirty.layerData then
        current.toVector.flatMap(_.chunks).foreach: chunk =>
          events += SurfaceResourceEvent.LayerUploaded(
            SurfaceResourceKey(s"javafx-atlas:${chunk.meshKey.value}:${chunk.faceStart}"),
            chunk.faceCount * 3,
            chunk.atlas.width,
            chunk.atlas.height,
            chunk.atlas.width.toLong * chunk.atlas.height.toLong * 4L
          )
      if !receipt.dirty.geometry && !receipt.dirty.layerData then
        previous.foreach: prior =>
          (prior.receipt.meshKeys ++ prior.receipt.layerKeys).foreach(key => events += SurfaceResourceEvent.CacheHit(key))
      current.foreach: result =>
        events += SurfaceResourceEvent.DrawSubmitted(plan.receipt.drawPassCount, result.receipt.facesUploaded)
      val observation = SurfaceBackendObservation(
        capabilities.admissionCapabilities,
        SurfacePlanRevision.Current,
        path,
        events.result(),
        Vector(SurfacePhaseTiming.unsafe(SurfaceRenderPhase.RenderSubmission, receipt.elapsedNanos))
      )
      JavaFxObservedReceipt(receipt, observation)

  def newSubScene(config: JavaFxSnapshotConfig): Either[JavaFxSurfaceError, SubScene] =
    requireFxThread().flatMap: _ =>
      current match
        case None => Left(JavaFxSurfaceError.IncompatiblePlan("render a plan before creating a SubScene"))
        case Some(_) if mountedScene.nonEmpty =>
          Left(JavaFxSurfaceError.IncompatiblePlan("the backend root already belongs to a SubScene"))
        case Some(probe) =>
          probe.setViewportSize(config.width, config.height)
          val scene = new SubScene(root, config.width, config.height, true, config.antialiasing)
          probe.attachCamera(scene)
          scene.setFill(if config.transparent then Color.TRANSPARENT else Color.WHITE)
          mountedScene = Some(scene)
          Right(scene)

  def snapshot(config: JavaFxSnapshotConfig): Either[JavaFxSurfaceError, WritableImage] =
    requireFxThread().flatMap: _ =>
      val target = mountedScene match
        case Some(scene) => Right(scene)
        case None => newSubScene(config)
      target.flatMap: scene =>
        val resize = scene.getWidth != config.width || scene.getHeight != config.height
        val fill = if config.transparent then Color.TRANSPARENT else Color.WHITE
        if scene.getAntiAliasing != config.antialiasing then
          Left(JavaFxSurfaceError.IncompatiblePlan("snapshot antialiasing differs from the mounted SubScene"))
        else if resize && (scene.widthProperty().isBound || scene.heightProperty().isBound) then
          Left(JavaFxSurfaceError.IncompatiblePlan("cannot resize a bound SubScene for a snapshot"))
        else if scene.fillProperty().isBound && scene.getFill != fill then
          Left(JavaFxSurfaceError.IncompatiblePlan("cannot change a bound SubScene fill for a snapshot"))
        else
          val width = scene.getWidth
          val height = scene.getHeight
          val priorFill = scene.getFill
          try
            if resize then
              scene.setWidth(config.width)
              scene.setHeight(config.height)
            current.get.setViewportSize(config.width, config.height)
            if priorFill != fill then scene.setFill(fill)
            val parameters = new SnapshotParameters()
            parameters.setFill(fill)
            Right(scene.snapshot(parameters, new WritableImage(config.width, config.height)))
          finally
            if priorFill != fill then scene.setFill(priorFill)
            if resize then
              scene.setWidth(width)
              scene.setHeight(height)
            current.get.setViewportSize(scene.getWidth.toInt, scene.getHeight.toInt)

  def resourceKeys: Set[SurfaceResourceKey] =
    currentPlan.toSet.flatMap(plan => plan.receipt.meshKeys ++ plan.receipt.layerKeys)

  /** The successfully painted plan remains authoritative after a refused update. */
  def pickingPlan: Option[SurfaceRenderPlan] = currentPlan

  private[javafx] def chunks: Vector[JavaFxSurfaceChunk] =
    current.toVector.flatMap(_.chunks)

  def dispose(): Either[JavaFxSurfaceError, Unit] =
    requireFxThread().map: _ =>
      current.foreach(_.root.getChildren.clear())
      root.getChildren.clear()
      mountedScene.foreach: scene =>
        scene.setRoot(new Group())
        scene.setCamera(null)
      mountedScene = None
      current = None
      currentPlan = None
      disposed = true

  private def requireFxThread(): Either[JavaFxSurfaceError, Unit] =
    if Platform.isFxApplicationThread then Right(())
    else Left(JavaFxSurfaceError.IncompatiblePlan("JavaFX interpreter must run on the Application Thread"))

object JavaFxSurfaceBackend:
  private[javafx] def validateCapabilities(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, Unit] =
    plan.clipping match
      case SurfaceClipping.WorldPlanes(_) =>
        Left(JavaFxSurfaceError.IncompatiblePlan(
          "world clipping planes are unsupported by JavaFX Scene3D; use the reference raster backend"
        ))
      case _ => Right(())

  def create(config: JavaFxAtlasConfig = JavaFxAtlasConfig.Default): Either[JavaFxSurfaceError, JavaFxSurfaceBackend] =
    if config.encoding != JavaFxAtlasEncoding.LegacyTriangle then
      Left(JavaFxSurfaceError.SamplerUnqualified(config.encoding))
    else createDiagnostic(config)

  /** Test/probe host only. It never grants production sampler admission. */
  private[javafx] def createDiagnostic(config: JavaFxAtlasConfig): Either[JavaFxSurfaceError, JavaFxSurfaceBackend] =
    if !Platform.isFxApplicationThread then
      Left(JavaFxSurfaceError.IncompatiblePlan("JavaFX backend creation must run on the Application Thread"))
    else
      Right(new JavaFxSurfaceBackend(new Group(), JavaFxCapabilityReport.current, config))
