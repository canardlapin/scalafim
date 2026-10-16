package scalafim.surface.view.three

import intaglio.Rgba32
import scalafim.surface.view.*
import intaglio.alpha

/** Browser-interpreter options. The clear colour is an already-validated
  * `Rgba32`; it does not leak Three.js values into the shared surface model.
  */
enum ThreeDepthPrecision:
  /** Redistribute unused depth precision over occupied geometry. */
  case ConditionOccupiedRange
  /** Keep the supplied depth projection, including its finite depth precision.
    * Fractional viewport correction may still adjust clip X/Y.
    */
  case PreserveDepthProjection

final case class ThreeJsRuntimeOptions(clearColor: Rgba32,
  depthPrecision: ThreeDepthPrecision = ThreeDepthPrecision.ConditionOccupiedRange)

/** Matrix submitted for one native draw, after depth and viewport correction.
  * Values are rounded to the Float32 uniforms consumed by WebGL.
  */
final case class ThreeProjectionReceipt(view: Option[SurfaceViewId], depthPrecision: ThreeDepthPrecision,
  columnMajorProjection: Vector[Float], viewport: (Int, Int, Int, Int))

object ThreeJsRuntimeOptions:
  val Default: ThreeJsRuntimeOptions =
    ThreeJsRuntimeOptions(Rgba32.unsafe(255, 255, 255))

final case class ThreePick(
  surface: SurfaceId,
  face: Int,
  vertex: Int,
  worldX: Double,
  worldY: Double,
  worldZ: Double,
  barycentric: Option[(Double, Double, Double)] = None,
  origin: Option[SurfaceDisplayOrigin] = None
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
  def supportsFragmentLayers: Boolean = false
  def validateFragments(packets: Vector[ThreeFragmentPacket]): Either[ThreeSurfaceError, Unit] =
    if packets.isEmpty || supportsFragmentLayers then Right(())
    else Left(ThreeSurfaceError.InvalidPlan("runtime does not support fragment layers"))
  def uploadFragments(packets: Vector[ThreeFragmentPacket]): Either[ThreeSurfaceError, Long] =
    Left(ThreeSurfaceError.InvalidPlan("runtime does not support fragment layers"))
  def uploadGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long]
  def updateGeometry(meshes: Vector[SurfaceMeshPacket]): Either[ThreeSurfaceError, Long]
  def uploadColors(colors: Vector[ThreeSurfaceColors]): Either[ThreeSurfaceError, Long]
  def updateLighting(lighting: SurfaceLighting): Either[ThreeSurfaceError, Unit]
  def updateCamera(camera: SurfaceCameraPacket, clipping: SurfaceClipping): Either[ThreeSurfaceError, Unit]
  def updateLayout(slots: Vector[SurfaceViewSlot], fit: SurfaceViewportFit): Either[ThreeSurfaceError, Unit]
  /** Installs native display occurrences after shared resources have been uploaded. */
  def updateComposition(plan: SurfaceCompositionPlan): Either[ThreeSurfaceError, Unit] =
    Left(ThreeSurfaceError.InvalidPlan("runtime does not support multi-view composition"))
  /** Removes native composition clones before returning to legacy rendering. */
  def clearComposition(): Either[ThreeSurfaceError, Unit] = Right(())
  /** Installs display-owned, unlit decoration geometry. Implementations must
    * reject unresolved labels rather than silently dropping scientific text. */
  def updateDecorations(geometry: SurfaceDecorationGeometry): Either[ThreeSurfaceError, Unit] =
    if geometry.meshes.isEmpty && geometry.labels.isEmpty then Right(())
    else Left(ThreeSurfaceError.InvalidPlan("runtime does not support surface decorations"))
  def clearDecorations(): Either[ThreeSurfaceError, Unit] = Right(())
  def resize(size: ThreeCanvasSize): Either[ThreeSurfaceError, Unit]
  def draw(): Either[ThreeSurfaceError, Unit]
  def pick(x: Double, y: Double): Either[ThreeSurfaceError, Option[ThreePick]]
  def pickDisplay(x: Double, y: Double): Either[ThreeSurfaceError, Option[SurfaceDisplayHit[ThreePick]]] =
    pick(x, y).map(_.map(SurfaceDisplayHit.Scientific(_)))
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
      SurfaceBackendFeature.PerSurfaceCameras,
      SurfaceBackendFeature.FacewiseData,
      SurfaceBackendFeature.NearestVertexSampling,
      SurfaceBackendFeature.DepthBuffer,
      SurfaceBackendFeature.BackFaceCulling,
      SurfaceBackendFeature.Lighting,
      SurfaceBackendFeature.BilateralViewports,
      SurfaceBackendFeature.NativePicking,
      SurfaceBackendFeature.HighResolutionSnapshot
    )
    val fragments = if runtime.supportsFragmentLayers then base ++ Set(SurfaceBackendFeature.ScalarInterpolation,
      SurfaceBackendFeature.FragmentComposition, SurfaceBackendFeature.WorldClipping,
      SurfaceBackendFeature.DisplayLinearLight, SurfaceBackendFeature.ThresholdEdgeOverlay, SurfaceBackendFeature.SurfaceBaseColor) else base
    if runtime.supportsGpuVolumeProjection then fragments + SurfaceBackendFeature.GpuVolumeProjection else fragments

  val capabilities: SurfaceBackendCapabilities = SurfaceBackendCapabilities(
    SurfaceBackendId.unsafe("three-webgl"),
    SurfacePlanRevision.Current,
    admittedFeatures,
    Vector("native world clipping supports at most eight planes; surface alpha uses CanvasRasterSurfaceHost") ++
      Option.when(!runtime.supportsGpuVolumeProjection)("GPU volume projection is unavailable; use the shared CPU oracle")
  )

  private var current: Option[(SurfaceRenderPlan, ThreeCanvasSize)] = None
  private var currentComposition: Option[SurfaceCompositionPlan] = None
  private var decorationsActive = false
  private var currentDecorations: Option[(SurfaceCompositionPlan, SurfaceDecorationGeometry, ThreeCanvasSize)] = None
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
    else if plan.surfaceCameras.nonEmpty then
      SurfaceCompositionPlan.fromLegacy(plan).left.map(error => ThreeSurfaceError.InvalidPlan(error.toString))
        .flatMap(render(_, size, forceDraw))
    else if plan.fragmentSurfaces.nonEmpty && !runtime.supportsFragmentLayers then
      Left(ThreeSurfaceError.InvalidPlan("runtime does not support fragment layers"))
    else
      SurfaceThresholdEdgeValidation.validate(plan).left.map(ThreeSurfaceError.InvalidPlan.apply).flatMap: _ =>
        requireContext().flatMap: _ =>
          val reuse = currentComposition.fold[Either[SurfaceCompositionError, Unit]](Right(())): previous =>
            SurfaceCompositionPlan.validateResourceReuse(previous.resources, SurfaceCompositionResources(
              plan.meshes.map(m => m.resourceKey -> m).toMap, plan.layers.map(l => l.resourceKey -> l).toMap))
          reuse.left.map(error => ThreeSurfaceError.InvalidPlan(s"immutable composition resource changed: $error"))
            .flatMap(_ => ThreeSurfaceProgram.compile(current, plan, size, forceDraw)).flatMap: program =>
            val packets = program.commands.collect:
              case ThreeSurfaceCommand.UploadFragments(values) => values
            runtime.validateFragments(packets.flatten).map(_ => program)
          .flatMap: program =>
            // Cleanup may fail after native objects have already been removed.
            // Never reuse decoration identities after such an attempt.
            currentDecorations = None
            val clear = if currentComposition.nonEmpty then runtime.clearComposition() else Right(())
            clear.flatMap: _ =>
              currentComposition = None
              decorationsActive = false
              currentDecorations = None
              interpret(Right(program), plan, size)

  /** Renders a validated multi-view composition while sharing each surface's
    * uploaded mesh and layer resources across its display occurrences.
    */
  def render(
    composition: SurfaceCompositionPlan,
    size: ThreeCanvasSize
  ): Either[ThreeSurfaceError, ThreeInterpretReceipt] =
    render(composition, size, forceDraw = false)

  def render(
    composition: SurfaceCompositionPlan,
    size: ThreeCanvasSize,
    forceDraw: Boolean
  ): Either[ThreeSurfaceError, ThreeInterpretReceipt] =
    renderComposition(composition, size, forceDraw, clearDecorations = true)

  private def renderComposition(composition: SurfaceCompositionPlan, size: ThreeCanvasSize,
      forceDraw: Boolean, clearDecorations: Boolean): Either[ThreeSurfaceError, ThreeInterpretReceipt] =
    if disposed then Left(ThreeSurfaceError.BackendDisposed)
    else
      requireContext().flatMap: _ =>
        val removingDecorations = clearDecorations && decorationsActive
        ThreeSurfaceProgram.compileComposition(current, currentComposition, composition, size,
          forceDraw || removingDecorations).flatMap: program =>
          val packets = program.commands.collect:
            case ThreeSurfaceCommand.UploadFragments(values) => values
          runtime.validateFragments(packets.flatten).map(_ => program)
        .flatMap: program =>
          if clearDecorations then currentDecorations = None
          val clear = if removingDecorations then runtime.clearDecorations() else Right(())
          clear.flatMap: _ =>
            if clearDecorations then
              decorationsActive = false
              currentDecorations = None
            interpret(Right(program), program.resourcePlan.get, size)

  /** Renders a composition and its already-lowered decoration meshes. Geometry
    * is tied to this exact logical canvas size, so a resize requires lowering
    * again before native objects are touched. */
  def renderDecorated(
    plan: SurfaceCompositionPlan,
    geometry: SurfaceDecorationGeometry,
    size: ThreeCanvasSize,
    forceDraw: Boolean = false
  ): Either[ThreeSurfaceError, ThreeInterpretReceipt] =
    val started = System.nanoTime()
    SurfaceDecorationValidation.validate(geometry, plan, size.width, size.height)
      .left.map(ThreeSurfaceError.InvalidPlan(_)).flatMap: _ =>
        if geometry.meshes.exists(mesh => mesh.texture.isEmpty && mesh.color.alpha != 255 &&
            mesh.depthPolicy != SurfaceDecorationDepthPolicy.OrderedDepthTest) then
          Left(ThreeSurfaceError.InvalidPlan("semi-transparent solid decorations are not yet qualified"))
        else
          // Compilation validates all composition and resource invariants before
          // either the scientific or decoration native state can be changed.
          val reuse = currentDecorations.exists: (previousPlan, previousGeometry, previousSize) =>
            (plan eq previousPlan) && (geometry eq previousGeometry) && size == previousSize
          renderComposition(plan, size, forceDraw || reuse, clearDecorations = !reuse).flatMap: receipt =>
            if reuse then Right(receipt.copy(elapsedNanos = System.nanoTime() - started))
            else runtime.updateDecorations(geometry).flatMap: _ =>
              // The native install is already live before draw submission. Keep
              // this flag even if draw fails so a later plain render removes it.
              decorationsActive = true
              currentDecorations = Some((plan, geometry, size))
              runtime.draw().map: _ =>
                totalDrawCalls += 1
                receipt.copy(commandsApplied = receipt.commandsApplied + 2, drawCalls = receipt.drawCalls + 1,
                  elapsedNanos = System.nanoTime() - started)

  private def interpret(
    compiled: Either[ThreeSurfaceError, ThreeSurfaceProgram],
    plan: SurfaceRenderPlan,
    size: ThreeCanvasSize
  ): Either[ThreeSurfaceError, ThreeInterpretReceipt] =
    compiled.flatMap: program =>
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
                case ThreeSurfaceCommand.UploadFragments(packets) =>
                  runtime.uploadFragments(packets).map: bytes =>
                    colorUploads += packets.length
                    uploadedBytes += bytes
                    totalColorUploads += packets.length
                    resources ++= packets.flatMap(_.resourceKeys)
                case ThreeSurfaceCommand.UpdateLighting(lighting) => runtime.updateLighting(lighting)
                case ThreeSurfaceCommand.UpdateCamera(camera, clipping) => runtime.updateCamera(camera, clipping)
                case ThreeSurfaceCommand.UpdateLayout(slots, fit) => runtime.updateLayout(slots, fit)
                case ThreeSurfaceCommand.UpdateComposition(composition) => runtime.updateComposition(composition)
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
              program.composition.foreach(value => currentComposition = Some(value))
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
      if receipt.geometryUploads > 0 || receipt.geometryUpdates > 0 then
        plan.meshes.foreach: mesh =>
          val vertices = if plan.fragmentSurfaces(mesh.surface) then mesh.indices.length else mesh.positions.length / 3
          events += SurfaceResourceEvent.MeshUploaded(
            mesh.resourceKey,
            vertices,
            mesh.indices.length / 3,
            (vertices.toLong * 6 + (if receipt.geometryUploads > 0 then mesh.indices.length.toLong else 0L)) * 4L
          )
      if receipt.colorUploads > 0 then
        plan.meshes.foreach: mesh =>
          val keys = plan.layers.iterator.filter(_.surface == mesh.surface).map(_.resourceKey.value).mkString("+")
          val count = if plan.fragmentSurfaces(mesh.surface) then mesh.indices.length else mesh.positions.length / 3
          val bytes = if plan.fragmentSurfaces(mesh.surface) then count.toLong * 16 * plan.layers.count(_.surface == mesh.surface)
            else count.toLong * 12
          events += SurfaceResourceEvent.LayerUploaded(
            SurfaceResourceKey(s"three-colors:${mesh.surface.value}:$keys"),
            count,
            count,
            1,
            bytes
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

  def pickDisplay(x: Double, y: Double): Either[ThreeSurfaceError, Option[SurfaceDisplayHit[ThreePick]]] =
    if disposed then Left(ThreeSurfaceError.BackendDisposed)
    else if !x.isFinite || !y.isFinite then Left(ThreeSurfaceError.InvalidPlan("pick coordinates must be finite"))
    else requireContext().flatMap(_ => runtime.pickDisplay(x, y))

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
        currentComposition = None
        decorationsActive = false
        currentDecorations = None
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
