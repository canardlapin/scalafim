package scalafim.surface.view.three

import intaglio.Rgba32
import scalafim.surface.view.*

/** Browser-interpreter options. The clear colour is an already-validated
  * `Rgba32`; it does not leak Three.js values into the shared surface model.
  */
final case class ThreeJsRuntimeOptions(clearColor: Rgba32)

object ThreeJsRuntimeOptions:
  val Default: ThreeJsRuntimeOptions =
    ThreeJsRuntimeOptions(Rgba32.unsafe(255, 255, 255))

final case class ThreePick(
  surface: SurfaceId,
  face: Int,
  vertex: Int,
  worldX: Double,
  worldY: Double,
  worldZ: Double
)

final case class ThreeRuntimeStats(
  drawCalls: Long,
  geometryUploads: Long,
  geometryUpdates: Long,
  colorUploads: Long,
  uploadedBytes: Long,
  disposedResources: Long
)

trait ThreeSurfaceRuntime:
  def contextState: ThreeContextState
  def supportsGpuVolumeProjection: Boolean = false
  def uploadGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long]
  def updateGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long]
  def uploadColors(colors: Vector[ThreeSurfaceColors]): Either[ThreeSurfaceError, Long]
  def updateLighting(lighting: SurfaceLighting): Either[ThreeSurfaceError, Unit]
  def updateCamera(camera: SurfaceCameraPacket, clipping: SurfaceClipping): Either[ThreeSurfaceError, Unit]
  def updateLayout(slots: Vector[SurfaceViewSlot]): Either[ThreeSurfaceError, Unit]
  def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit]
  def draw(): Either[ThreeSurfaceError, Unit]
  def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]]
  def disposeResources(keys: Vector[SurfaceResourceKey]): Either[ThreeSurfaceError, Int]
  def dispose(): Either[ThreeSurfaceError, Unit]

final case class ThreeInterpretReceipt(
  dirty: ThreeDirtySet,
  commandsApplied: Int,
  drawCalls: Int,
  geometryUploads: Int,
  geometryUpdates: Int,
  colorUploads: Int,
  uploadedBytes: Long,
  resourceCount: Int,
  elapsedNanos: Long
)

final case class ThreeObservedReceipt(
  native: ThreeInterpretReceipt,
  observation: SurfaceBackendObservation
)

final class ThreeSurfaceBackend private (runtime: ThreeSurfaceRuntime):
  private val admittedFeatures =
    val base = Set(
      SurfaceBackendFeature.HardwareAcceleration,
      SurfaceBackendFeature.DepthBuffer,
      SurfaceBackendFeature.BackFaceCulling,
      SurfaceBackendFeature.Lighting,
      SurfaceBackendFeature.BilateralViewports,
      SurfaceBackendFeature.NativePicking,
      SurfaceBackendFeature.HighResolutionSnapshot
    )
    if runtime.supportsGpuVolumeProjection then base + SurfaceBackendFeature.GpuVolumeProjection else base

  val capabilities: SurfaceBackendCapabilities = SurfaceBackendCapabilities(
    SurfaceBackendId.unsafe("three-webgl"),
    SurfacePlanRevision.Current,
    admittedFeatures,
    Vector("world clipping planes are not yet supported") ++
      Option.when(!runtime.supportsGpuVolumeProjection)("GPU volume projection is unavailable; use the shared CPU oracle")
  )

  private var current: Option[(SurfaceRenderPlan, ThreeCanvasSize)] = None
  private var disposed = false
  private var resources = Set.empty[SurfaceResourceKey]
  private var totalDrawCalls = 0L
  private var totalGeometryUploads = 0L
  private var totalGeometryUpdates = 0L
  private var totalColorUploads = 0L
  private var totalUploadedBytes = 0L
  private var totalDisposedResources = 0L

  def render(
    plan: SurfaceRenderPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean = false
  ): Either[ThreeSurfaceError, ThreeInterpretReceipt] =
    if disposed then Left(ThreeSurfaceError.BackendDisposed)
    else
      requireContext().flatMap: _ =>
        ThreeSurfaceProgram.compile(current, plan, size, forceDraw).flatMap: program =>
          val started = System.nanoTime()
          var applied = 0
          var draws = 0
          var geometryUploads = 0
          var geometryUpdates = 0
          var colorUploads = 0
          var uploadedBytes = 0L
          var failure: Option[ThreeSurfaceError] = None
          var index = 0
          while index < program.commands.length && failure.isEmpty do
            val outcome =
              program.commands(index) match
                case ThreeSurfaceCommand.DisposeResources(keys) =>
                  runtime.disposeResources(keys).map: count =>
                    totalDisposedResources += count
                    resources --= keys
                case ThreeSurfaceCommand.UploadGeometry(meshes) =>
                  runtime.uploadGeometry(meshes).map: bytes =>
                    geometryUploads += meshes.length
                    uploadedBytes += bytes
                    totalGeometryUploads += meshes.length
                    resources ++= meshes.map(_.resourceKey)
                case ThreeSurfaceCommand.UpdateGeometry(meshes) =>
                  runtime.updateGeometry(meshes).map: bytes =>
                    geometryUpdates += meshes.length
                    uploadedBytes += bytes
                    totalGeometryUpdates += meshes.length
                case ThreeSurfaceCommand.UploadColors(colors) =>
                  runtime.uploadColors(colors).map: bytes =>
                    colorUploads += colors.length
                    uploadedBytes += bytes
                    totalColorUploads += colors.length
                    resources ++= colors.flatMap(_.resourceKeys)
                case ThreeSurfaceCommand.UpdateLighting(lighting) => runtime.updateLighting(lighting)
                case ThreeSurfaceCommand.UpdateCamera(camera, clipping) => runtime.updateCamera(camera, clipping)
                case ThreeSurfaceCommand.UpdateLayout(slots) => runtime.updateLayout(slots)
                case ThreeSurfaceCommand.Resize(canvasSize) => runtime.resize(canvasSize)
                case ThreeSurfaceCommand.Draw =>
                  runtime.draw().map: _ =>
                    draws += 1
                    totalDrawCalls += 1
            outcome match
              case Left(error) => failure = Some(error)
              case Right(_) => applied += 1
            index += 1
          failure match
            case Some(error) => Left(error)
            case None =>
              totalUploadedBytes += uploadedBytes
              current = Some((plan, size))
              Right(ThreeInterpretReceipt(
                program.dirty,
                applied,
                draws,
                geometryUploads,
                geometryUpdates,
                colorUploads,
                uploadedBytes,
                resources.size,
                System.nanoTime() - started
              ))

  def renderObserved(
    plan: SurfaceRenderPlan,
    size: ThreeCanvasSize,
    path: SurfaceAdmissionPath,
    forceDraw: Boolean = false
  ): Either[ThreeSurfaceError, ThreeObservedReceipt] =
    val previous = current
    render(plan, size, forceDraw).map: receipt =>
      val events = Vector.newBuilder[SurfaceResourceEvent]
      if receipt.geometryUploads > 0 then
        plan.meshes.foreach: mesh =>
          events += SurfaceResourceEvent.MeshUploaded(
            mesh.resourceKey,
            mesh.positions.length / 3,
            mesh.indices.length / 3,
            (mesh.positions.length.toLong + mesh.normals.length.toLong + mesh.indices.length.toLong) * 4L
          )
      if receipt.colorUploads > 0 then
        plan.meshes.foreach: mesh =>
          val keys = plan.layers.iterator.filter(_.surface == mesh.surface).map(_.resourceKey.value).mkString("+")
          events += SurfaceResourceEvent.LayerUploaded(
            SurfaceResourceKey(s"three-colors:${mesh.surface.value}:$keys"),
            mesh.positions.length / 3,
            mesh.positions.length / 3,
            1,
            mesh.positions.length.toLong / 3L * 12L
          )
      if !receipt.dirty.geometry && !receipt.dirty.layerData then
        previous.foreach: (prior, _) =>
          (prior.receipt.meshKeys ++ prior.receipt.layerKeys).foreach(key => events += SurfaceResourceEvent.CacheHit(key))
      if receipt.dirty.canvas then events += SurfaceResourceEvent.Resized(size.width, size.height)
      if receipt.drawCalls > 0 then
        events += SurfaceResourceEvent.DrawSubmitted(receipt.drawCalls, plan.profile.facesPacked)
      val observation = SurfaceBackendObservation(
        capabilities,
        SurfacePlanRevision.Current,
        path,
        events.result(),
        Vector(SurfacePhaseTiming.unsafe(SurfaceRenderPhase.RenderSubmission, receipt.elapsedNanos))
      )
      ThreeObservedReceipt(receipt, observation)

  def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]] =
    if disposed then Left(ThreeSurfaceError.BackendDisposed)
    else if !x.isFinite || !y.isFinite then Left(ThreeSurfaceError.InvalidPlan("pick coordinates must be finite"))
    else requireContext().flatMap(_ => runtime.pick(x, y))

  def stats: ThreeRuntimeStats =
    ThreeRuntimeStats(
      totalDrawCalls,
      totalGeometryUploads,
      totalGeometryUpdates,
      totalColorUploads,
      totalUploadedBytes,
      totalDisposedResources
    )

  def resourceKeys: Set[SurfaceResourceKey] = resources

  def dispose(): Either[ThreeSurfaceError, Unit] =
    if disposed then Right(())
    else
      runtime.dispose().map: _ =>
        current = None
        resources = Set.empty
        disposed = true

  private def requireContext(): Either[ThreeSurfaceError, Unit] =
    runtime.contextState match
      case ThreeContextState.Available => Right(())
      case ThreeContextState.Lost => Left(ThreeSurfaceError.ContextLost)
      case ThreeContextState.Absent => Left(ThreeSurfaceError.ContextUnavailable)

object ThreeSurfaceBackend:
  def create(runtime: ThreeSurfaceRuntime): Either[ThreeSurfaceError, ThreeSurfaceBackend] =
    runtime.contextState match
      case ThreeContextState.Available => Right(new ThreeSurfaceBackend(runtime))
      case ThreeContextState.Lost => Left(ThreeSurfaceError.ContextLost)
      case ThreeContextState.Absent => Left(ThreeSurfaceError.ContextUnavailable)
