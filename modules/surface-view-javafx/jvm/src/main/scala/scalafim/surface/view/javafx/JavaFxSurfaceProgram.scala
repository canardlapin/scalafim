package scalafim.surface.view.javafx

import javafx.application.{ConditionalFeature, Platform}
import javafx.scene.{Camera, Group, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color

import scalafim.surface.view.*
import intaglio.DisplayBlendMode

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
  case UpdateLayout(slots: Vector[SurfaceViewSlot], fit: SurfaceViewportFit = SurfaceViewportFit.Fill)
  case DisposeResources(keys: Vector[SurfaceResourceKey])

final case class JavaFxSurfaceProgram(
  commands: Vector[JavaFxSurfaceCommand],
  dirty: JavaFxDirtySet,
  receipt: SurfaceRenderReceipt
)

object JavaFxSurfaceProgram:
  def compile(previous: Option[SurfaceRenderPlan], next: SurfaceRenderPlan): JavaFxSurfaceProgram =
    previous match
      case None =>
        JavaFxSurfaceProgram(
          Vector(
            JavaFxSurfaceCommand.RebuildGeometry(next),
            JavaFxSurfaceCommand.UpdateMaterial(materialMode(next)),
            JavaFxSurfaceCommand.UpdateCamera(next),
            JavaFxSurfaceCommand.UpdateLayout(next.slots, next.viewportFit)
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
        val layerData = layerSignature(before) != layerSignature(next)
        val material = before.lighting != next.lighting
        val camera = before.receipt.cameraKey != next.receipt.cameraKey
        val layout = before.slots != next.slots || before.viewportFit != next.viewportFit
        val clipping = before.clipping != next.clipping
        val nextKeys = (next.receipt.meshKeys ++ next.receipt.layerKeys).toSet
        val removed = (before.receipt.meshKeys ++ before.receipt.layerKeys).filterNot(nextKeys)
        val commands = Vector.newBuilder[JavaFxSurfaceCommand]
        if removed.nonEmpty then commands += JavaFxSurfaceCommand.DisposeResources(removed)
        if topology then commands += JavaFxSurfaceCommand.RebuildGeometry(next)
        else if geometry then commands += JavaFxSurfaceCommand.UpdateGeometry(next)
        val shadedGeometry = geometry && materialMode(next) == JavaFxMaterialMode.Lit &&
          before.meshes.zip(next.meshes).exists((a, b) => !java.util.Arrays.equals(a.normals.unsafeArray, b.normals.unsafeArray))
        if (layerData || material || shadedGeometry) && !topology then
          commands += JavaFxSurfaceCommand.UpdateAtlases(next)
        if material then commands += JavaFxSurfaceCommand.UpdateMaterial(materialMode(next))
        if camera || clipping then commands += JavaFxSurfaceCommand.UpdateCamera(next)
        if layout then commands += JavaFxSurfaceCommand.UpdateLayout(next.slots, next.viewportFit)
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
  fallback: Option[String],
  approximateFragments: Boolean = false
):

  def admissionCapabilities: SurfaceBackendCapabilities =
    val available =
      if scene3d then Set(
        SurfaceBackendFeature.HardwareAcceleration,
        SurfaceBackendFeature.FacewiseData,
        SurfaceBackendFeature.NearestVertexSampling,
        SurfaceBackendFeature.DepthBuffer,
        SurfaceBackendFeature.BackFaceCulling,
        SurfaceBackendFeature.Lighting,
        SurfaceBackendFeature.BilateralViewports,
        SurfaceBackendFeature.NativePicking,
        SurfaceBackendFeature.HighResolutionSnapshot
      )
      else Set.empty[SurfaceBackendFeature]
    SurfaceBackendCapabilities(
      SurfaceBackendId.unsafe(if approximateFragments then "javafx-scene3d-bounded-v1" else "javafx-scene3d"),
      SurfacePlanRevision.Current,
      available ++ (if scene3d && approximateFragments then Set(SurfaceBackendFeature.ScalarInterpolation, SurfaceBackendFeature.FragmentComposition) else Set.empty),
      fallback.toVector ++ Vector("world clipping planes are not yet supported") ++
        (if approximateFragments then Vector("scalar/layer fragments use opt-in bounded constant-color geometry; boundary coverage and native rounding are separate from the color certificate") else Vector.empty)
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
  approximation: Option[JavaFxApproximationReceipt] = None,
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
  config: JavaFxAtlasConfig,
  approximationConfig: Option[JavaFxApproximationConfig] = None
):
  private var currentPrepared: Option[JavaFxPreparedSurface] = None
  private var currentPlan: Option[SurfaceRenderPlan] = None
  private var current: Option[JavaFxSurfaceProbeResult] = None
  private var disposed = false

  def render(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxInterpretReceipt] =
    val started = System.nanoTime()
    requireFxThread().flatMap: _ =>
      if disposed then Left(JavaFxSurfaceError.IncompatiblePlan("backend has been disposed"))
      else plan.clipping match
        case SurfaceClipping.WorldPlanes(_) => Left(JavaFxSurfaceError.IncompatiblePlan("world clipping planes are unsupported by JavaFX Scene3D"))
        case _ => prepare(plan).flatMap(prepared => renderPrepared(plan, prepared))
          .map(_.copy(elapsedNanos = System.nanoTime() - started))

  private def prepare(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, JavaFxPreparedSurface] =
    approximationConfig match
      case None => JavaFxSurfaceBackend.validateCapabilities(plan).map(_ => JavaFxPreparedSurface(plan, None))
      case Some(settings) =>
        def signature(value: SurfaceRenderPlan): (Vector[SurfaceResourceKey], Vector[SurfaceResourceKey], Vector[(SurfaceResourceKey, Double, DisplayBlendMode, SurfaceLayerCoverage)], SurfaceLighting, Set[SurfaceId]) =
          (value.receipt.meshKeys, value.meshes.map(_.geometryKey),
            value.layers.map(l => (l.resourceKey, l.opacity.toDouble, l.blendMode, l.coverage)), value.lighting, value.fragmentSurfaces)
        (currentPlan, currentPrepared) match
          case (Some(before), Some(cached)) if signature(before) == signature(plan) &&
              before.meshes.zip(plan.meshes).forall((a, b) => java.util.Arrays.equals(
                a.sampleNormals.getOrElse(a.normals).unsafeArray, b.sampleNormals.getOrElse(b.normals).unsafeArray)) =>
            Right(cached.copy(plan = cached.plan.copy(slots = plan.slots, camera = plan.camera, clipping = plan.clipping,
              chrome = plan.chrome, readouts = plan.readouts, viewportFit = plan.viewportFit,
              receipt = cached.plan.receipt.copy(cameraKey = plan.receipt.cameraKey, timepoint = plan.receipt.timepoint)),
              approximation = cached.approximation.map(_.copy(preparationNanos = 0L, reused = true))))
          case _ => JavaFxSurfaceApproximation.prepare(plan, settings, config)

  private def renderPrepared(source: SurfaceRenderPlan, prepared: JavaFxPreparedSurface): Either[JavaFxSurfaceError, JavaFxInterpretReceipt] =
    val plan = prepared.plan
    JavaFxSurfaceBackend.validateCapabilities(plan).flatMap(_ => requireFxThread()).flatMap: _ =>
      if disposed then Left(JavaFxSurfaceError.IncompatiblePlan("backend has been disposed"))
      else
        val started = System.nanoTime()
        val program = JavaFxSurfaceProgram.compile(currentPrepared.map(_.plan), plan)
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
                  val mounted = current.toVector.flatMap(_.detachScenes())
                  current.foreach(_.root.getChildren.clear())
                  current = Some(probe)
                  root.getChildren.setAll(probe.root)
                  mounted.foreach(probe.attachCamera)
            case JavaFxSurfaceCommand.UpdateGeometry(next) =>
              current.get.updateGeometry(next) match
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
              current.foreach(_.setMaterialMode(mode))
            case JavaFxSurfaceCommand.UpdateCamera(next) =>
              current.get.applyCamera(next) match
                case Left(error) => failure = Some(error)
                case Right(_) => ()
            case JavaFxSurfaceCommand.UpdateLayout(slots, fit) => current.foreach(_.setLayout(slots, fit))
          index += 1
        failure match
          case Some(error) => Left(error)
          case None =>
            currentPlan = Some(source)
            currentPrepared = Some(prepared)
            Right(JavaFxInterpretReceipt(
              program.dirty,
              program.commands.length,
              resourceKeys.size,
              atlasUpdates,
              geometryUpdates,
              geometryBytesUpdated,
              System.nanoTime() - started,
              prepared.approximation,
              textureCoordinateBytesUpdated
            ))

  def renderObserved(
    plan: SurfaceRenderPlan,
    path: SurfaceAdmissionPath
  ): Either[JavaFxSurfaceError, JavaFxObservedReceipt] =
    val previous = currentPrepared.map(_.plan)
    render(plan).map: receipt =>
      val events = Vector.newBuilder[SurfaceResourceEvent]
      if receipt.dirty.geometry && receipt.geometryUpdates == 0 then
        current.toVector.flatMap(_.chunks).foreach: chunk =>
          events += SurfaceResourceEvent.MeshUploaded(
            chunk.meshKey,
            chunk.mesh.getPoints.size() / 3,
            chunk.faceCount,
            (chunk.mesh.getPoints.size().toLong + chunk.mesh.getNormals.size() +
              chunk.mesh.getTexCoords.size() + chunk.mesh.getFaces.size()) * 4
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
      if receipt.textureCoordinateBytesUpdated > 0 then
        val count = (receipt.textureCoordinateBytesUpdated / 8).toInt
        events += SurfaceResourceEvent.LayerUploaded(SurfaceResourceKey("javafx-texture-coordinates"),
          count, count, 1, receipt.textureCoordinateBytesUpdated)
      if !receipt.dirty.geometry && !receipt.dirty.layerData && receipt.textureCoordinateBytesUpdated == 0 then
        previous.foreach: prior =>
          (prior.receipt.meshKeys ++ prior.receipt.layerKeys).foreach(key => events += SurfaceResourceEvent.CacheHit(key))
      current.foreach: result =>
        events += SurfaceResourceEvent.DrawSubmitted(result.chunks.length, result.receipt.facesUploaded)
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
        case Some(probe) if probe.mountedScene.nonEmpty =>
          Left(JavaFxSurfaceError.IncompatiblePlan("backend already has a mounted SubScene; reuse that scene or create another backend"))
        case Some(probe) =>
          probe.setViewportSize(config.width, config.height)
          val scene = new SubScene(root, config.width, config.height, false, config.antialiasing)
          probe.attachCamera(scene)
          scene.setFill(if config.transparent then Color.TRANSPARENT else Color.WHITE)
          Right(scene)

  def snapshot(config: JavaFxSnapshotConfig): Either[JavaFxSurfaceError, WritableImage] =
    requireFxThread().flatMap: _ =>
      current match
        case None => Left(JavaFxSurfaceError.IncompatiblePlan("render a plan before taking a snapshot"))
        case Some(probe) => probe.mountedScene match
          case Some(scene) => Right(probe.snapshot(scene, config))
          case None => newSubScene(config).map: scene =>
            try probe.snapshot(scene, config)
            finally
              val _ = probe.detachScenes()
              scene.setRoot(new Group())

  def resourceKeys: Set[SurfaceResourceKey] =
    currentPrepared.toSet.flatMap(prepared => prepared.plan.receipt.meshKeys ++ prepared.plan.receipt.layerKeys)

  private[javafx] def pickingPlan: Option[SurfaceRenderPlan] = currentPrepared.map(_.plan)

  private[javafx] def viewportCameras: Vector[Camera] =
    current.toVector.flatMap(_.viewportCameras)

  private[javafx] def chunks: Vector[JavaFxSurfaceChunk] =
    current.toVector.flatMap(_.chunks)

  def dispose(): Either[JavaFxSurfaceError, Unit] =
    requireFxThread().map: _ =>
      current.foreach: probe =>
        val _ = probe.detachScenes()
        probe.root.getChildren.clear()
      root.getChildren.clear()
      current = None
      currentPlan = None
      currentPrepared = None
      disposed = true

  private def requireFxThread(): Either[JavaFxSurfaceError, Unit] =
    if Platform.isFxApplicationThread then Right(())
    else Left(JavaFxSurfaceError.IncompatiblePlan("JavaFX interpreter must run on the Application Thread"))

object JavaFxSurfaceBackend:
  private[javafx] def validateCapabilities(plan: SurfaceRenderPlan): Either[JavaFxSurfaceError, Unit] =
    if plan.fragmentSurfaces.nonEmpty || plan.layers.exists(layer => layer.interpolation == SurfaceMapInterpolation.VertexScalar || layer.scalarField.nonEmpty) then
      Left(JavaFxSurfaceError.IncompatiblePlan("scalar fragment interpolation has not been admitted for JavaFX; use the reference raster backend"))
    else plan.clipping match
      case SurfaceClipping.WorldPlanes(_) =>
        Left(JavaFxSurfaceError.IncompatiblePlan(
          "world clipping planes are unsupported by JavaFX Scene3D; use the reference raster backend"
        ))
      case _ => Right(())

  def create(config: JavaFxAtlasConfig = JavaFxAtlasConfig.Default): Either[JavaFxSurfaceError, JavaFxSurfaceBackend] =
    if !Platform.isFxApplicationThread then
      Left(JavaFxSurfaceError.IncompatiblePlan("JavaFX backend creation must run on the Application Thread"))
    else
      Right(new JavaFxSurfaceBackend(new Group(), JavaFxCapabilityReport.current, config))


  /** Opt-in scalar and mixed-layer rendering with a checked geometry error bound.
    * Data, mapping, and lighting changes may rebuild derived topology; camera
    * changes reuse it. Resource and approximation costs are returned in receipts.
    */
  def createApproximate(settings: JavaFxApproximationConfig, config: JavaFxAtlasConfig = JavaFxAtlasConfig.Default)
      : Either[JavaFxSurfaceError, JavaFxSurfaceBackend] =
    if !Platform.isFxApplicationThread then
      Left(JavaFxSurfaceError.IncompatiblePlan("JavaFX backend creation must run on the Application Thread"))
    else Right(new JavaFxSurfaceBackend(new Group(), JavaFxCapabilityReport.current.copy(approximateFragments = true), config, Some(settings)))
