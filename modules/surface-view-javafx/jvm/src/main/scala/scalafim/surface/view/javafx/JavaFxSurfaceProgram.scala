package scalafim.surface.view.javafx

import javafx.application.{ConditionalFeature, Platform}
import javafx.beans.Observable
import javafx.scene.{Camera, Group, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Paths}
import java.security.MessageDigest
import scala.util.Try

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
        SurfaceBackendFeature.PerSurfaceCameras,
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

enum JavaFxPrismPipeline(val id: String):
  case Metal extends JavaFxPrismPipeline("mtl")
  case Direct3d extends JavaFxPrismPipeline("d3d")
  case Es2 extends JavaFxPrismPipeline("es2")
  case Software extends JavaFxPrismPipeline("sw")
  case Unknown extends JavaFxPrismPipeline("unknown")

object JavaFxPrismPipeline:
  private val Initialized = """(?m)^Initialized prism pipeline:\s+([A-Za-z0-9_.$]+)\s*$""".r

  def observe(verboseLog: String): Either[String, (JavaFxPrismPipeline, String)] =
    Initialized.findAllMatchIn(verboseLog).map(_.group(1)).toVector.distinct match
      case Vector(className) => Right((fromClassName(className), className))
      case Vector()          => Left("Prism verbose log contains no initialized pipeline")
      case classes => Left(s"Prism verbose log contains multiple initialized pipelines: ${classes.mkString(", ")}")

  private def fromClassName(className: String): JavaFxPrismPipeline =
    className match
      case "com.sun.prism.mtl.MTLPipeline" => JavaFxPrismPipeline.Metal
      case "com.sun.prism.d3d.D3DPipeline" => JavaFxPrismPipeline.Direct3d
      case "com.sun.prism.es2.ES2Pipeline" => JavaFxPrismPipeline.Es2
      case "com.sun.prism.sw.SWPipeline"   => JavaFxPrismPipeline.Software
      case _                               => JavaFxPrismPipeline.Unknown

final case class JavaFxArtifactReceipt(
    component: String,
    module: String,
    implementationVersion: Option[String],
    codeSource: Option[String],
    sha256: Option[String]
)

final case class JavaFxRuntimeCapabilityReceipt(
    javaFxRuntimeVersion: String,
    javaRuntimeName: String,
    javaRuntimeVersion: String,
    osName: String,
    osVersion: String,
    osArch: String,
    requestedPipelines: Vector[String],
    noFallback: Boolean,
    verbose: Boolean,
    observedPipeline: JavaFxPrismPipeline,
    observedPipelineClass: String,
    relevantJvmArguments: Vector[String],
    moduleMutationArguments: Vector[String],
    artifacts: Vector[JavaFxArtifactReceipt]
):
  def fallbackUsed: Boolean = requestedPipelines.headOption.exists(_ != observedPipeline.id)

  /** Every failure is returned so a qualification receipt cannot hide a second packaging error behind the first one
    * encountered.
    */
  def stockMetalFailures(
      expectedJavaFxVersion: String,
      expectedArtifactSha256: Map[String, String]
  ): Vector[String] =
    val requiredArtifacts = Set("javafx-base", "javafx-graphics")
    val observedComponents = artifacts.map(_.component)
    val artifactFailures = Vector(
      Option.when(expectedArtifactSha256.keySet != requiredArtifacts)(
        s"expected artifact checksums must name exactly ${requiredArtifacts.toVector.sorted.mkString(",")}, found ${expectedArtifactSha256.keySet.toVector.sorted.mkString(",")}"
      ),
      Option.when(
        observedComponents.toSet != requiredArtifacts || observedComponents.distinct.size != observedComponents.size
      )(
        s"runtime artifacts must contain exactly one javafx-base and one javafx-graphics, found ${observedComponents.mkString(",")}"
      )
    ).flatten ++ requiredArtifacts.toVector.sorted.flatMap: component =>
      for
        expected <- expectedArtifactSha256.get(component).toVector
        artifact <- artifacts.find(_.component == component).toVector
        failure <- Vector(
          Option.when(artifact.codeSource.isEmpty)(s"$component has no observable code source"),
          Option.when(artifact.sha256 != Some(expected))(
            s"$component SHA-256 ${artifact.sha256.getOrElse("<unavailable>")} does not match expected $expected"
          )
        ).flatten
      yield failure
    Vector(
      Option.when(javaFxRuntimeVersion != expectedJavaFxVersion)(
        s"JavaFX runtime $javaFxRuntimeVersion does not match expected $expectedJavaFxVersion"
      ),
      Option.when(!osName.startsWith("Mac OS"))(s"stock Metal qualification requires macOS, found $osName"),
      Option.when(requestedPipelines.headOption != Some("mtl"))(
        s"Metal is not the first requested Prism pipeline: ${requestedPipelines.mkString(",")}"
      ),
      Option.when(!noFallback)("prism.noFallback must be true"),
      Option.when(!verbose)("prism.verbose must be true so the active pipeline is observable"),
      Option.when(observedPipeline != JavaFxPrismPipeline.Metal)(
        s"active Prism pipeline is $observedPipelineClass, not MTLPipeline"
      ),
      Option.when(fallbackUsed)(
        s"Prism fell back from ${requestedPipelines.headOption.getOrElse("<none>")} to ${observedPipeline.id}"
      ),
      Option.when(moduleMutationArguments.nonEmpty)(
        s"module mutation is active: ${moduleMutationArguments.mkString(" ")}"
      )
    ).flatten ++ artifactFailures

  def toJson: String =
    def escaped(value: String): String =
      value.flatMap:
        case '"'                    => "\\\""
        case '\\'                   => "\\\\"
        case '\n'                   => "\\n"
        case '\r'                   => "\\r"
        case '\t'                   => "\\t"
        case char if char.isControl => f"\\u${char.toInt}%04x"
        case char                   => char.toString
    def quoted(value: String): String = s"\"${escaped(value)}\""
    def optional(value: Option[String]): String = value.fold("null")(quoted)
    def strings(values: Vector[String]): String = values.map(quoted).mkString("[", ",", "]")
    val artifactJson = artifacts.map: artifact =>
      s"""{"component":${quoted(artifact.component)},"module":${quoted(
          artifact.module
        )},"implementationVersion":${optional(artifact.implementationVersion)},"codeSource":${optional(
          artifact.codeSource
        )},"sha256":${optional(artifact.sha256)}}"""
    s"""{"javaFxRuntimeVersion":${quoted(javaFxRuntimeVersion)},"javaRuntimeName":${quoted(
        javaRuntimeName
      )},"javaRuntimeVersion":${quoted(javaRuntimeVersion)},"osName":${quoted(osName)},"osVersion":${quoted(
        osVersion
      )},"osArch":${quoted(osArch)},"requestedPipelines":${strings(
        requestedPipelines
      )},"noFallback":$noFallback,"verbose":$verbose,"observedPipeline":${quoted(
        observedPipeline.id
      )},"observedPipelineClass":${quoted(
        observedPipelineClass
      )},"fallbackUsed":$fallbackUsed,"relevantJvmArguments":${strings(
        relevantJvmArguments
      )},"moduleMutationArguments":${strings(moduleMutationArguments)},"artifacts":${artifactJson.mkString(
        "[",
        ",",
        "]"
      )}}"""

object JavaFxRuntimeCapabilityReceipt:
  def current(verboseLog: String): Either[String, JavaFxRuntimeCapabilityReceipt] =
    JavaFxPrismPipeline
      .observe(verboseLog)
      .map: (pipeline, pipelineClass) =>
        val inputArguments = ManagementFactory.getRuntimeMXBean.getInputArguments.toArray(new Array[String](0)).toVector
        val relevant = inputArguments.filter: argument =>
          argument.startsWith("-Dprism.") || argument.startsWith("--patch-module") ||
            argument.startsWith("--add-exports") || argument.startsWith("--add-opens") ||
            argument.startsWith("--enable-native-access")
        val mutations = relevant.filter: argument =>
          argument.startsWith("--patch-module") || argument.startsWith("--add-exports") || argument.startsWith(
            "--add-opens"
          )
        JavaFxRuntimeCapabilityReceipt(
          javaFxRuntimeVersion = Option(System.getProperty("javafx.runtime.version"))
            .orElse(Option(classOf[_root_.javafx.scene.Scene].getPackage.getImplementationVersion))
            .getOrElse("unknown"),
          javaRuntimeName = System.getProperty("java.runtime.name", "unknown"),
          javaRuntimeVersion = System.getProperty("java.runtime.version", "unknown"),
          osName = System.getProperty("os.name", "unknown"),
          osVersion = System.getProperty("os.version", "unknown"),
          osArch = System.getProperty("os.arch", "unknown"),
          requestedPipelines = System.getProperty("prism.order", "").split(',').toVector.map(_.trim).filter(_.nonEmpty),
          noFallback = java.lang.Boolean.getBoolean("prism.noFallback"),
          verbose = java.lang.Boolean.getBoolean("prism.verbose"),
          observedPipeline = pipeline,
          observedPipelineClass = pipelineClass,
          relevantJvmArguments = relevant,
          moduleMutationArguments = mutations,
          artifacts = Vector(
            artifact("javafx-base", classOf[Observable]),
            artifact("javafx-graphics", classOf[_root_.javafx.scene.Scene])
          )
        )

  private def artifact(component: String, clazz: Class[?]): JavaFxArtifactReceipt =
    val location = Option(clazz.getProtectionDomain)
      .flatMap(domain => Option(domain.getCodeSource))
      .flatMap(source => Option(source.getLocation))
    JavaFxArtifactReceipt(
      component = component,
      module = Option(clazz.getModule.getName).getOrElse("unnamed"),
      implementationVersion = Option(clazz.getPackage.getImplementationVersion),
      codeSource = location.map(_.toExternalForm),
      sha256 = location
        .filter(_.getProtocol == "file")
        .flatMap: url =>
          Try:
            val digest = MessageDigest.getInstance("SHA-256")
            val input = Files.newInputStream(Paths.get(url.toURI))
            try
              val buffer = new Array[Byte](64 * 1024)
              var read = input.read(buffer)
              while read >= 0 do
                if read > 0 then digest.update(buffer, 0, read)
                read = input.read(buffer)
              digest.digest().map(byte => f"${byte & 0xff}%02x").mkString
            finally input.close()
          .toOption
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
            Right(cached.copy(plan = cached.plan.copy(slots = plan.slots, camera = plan.camera, surfaceCameras = plan.surfaceCameras, clipping = plan.clipping,
              chrome = plan.chrome, readouts = plan.readouts, viewportFit = plan.viewportFit,
              receipt = cached.plan.receipt.copy(cameraKey = plan.receipt.cameraKey, timepoint = plan.receipt.timepoint)),
              approximation = cached.approximation.map(_.copy(preparationNanos = 0L, reused = true))))
          case _ => JavaFxSurfaceApproximation.prepare(plan, settings, config)

  private def renderPrepared(source: SurfaceRenderPlan, prepared: JavaFxPreparedSurface): Either[JavaFxSurfaceError, JavaFxInterpretReceipt] =
    val plan = prepared.plan
    JavaFxSurfaceBackend.validateCapabilities(plan).flatMap(_ => requireFxThread()).flatMap: _ =>
      if disposed then Left(JavaFxSurfaceError.IncompatiblePlan("backend has been disposed"))
      else scala.util.boundary[Either[JavaFxSurfaceError, JavaFxInterpretReceipt]]:
        val started = System.nanoTime()
        val incremental = JavaFxSurfaceProgram.compile(currentPrepared.map(_.plan), plan)
        val changesAtlas = incremental.commands.exists:
          case JavaFxSurfaceCommand.UpdateAtlases(_) => true
          case _ => false
        val rebuild = if changesAtlas then current.get.requiresAtlasRebuild(plan, JavaFxSurfaceProgram.materialMode(plan)) else Right(false)
        val program = rebuild match
          case Left(error) => scala.util.boundary.break(Left(error))
          case Right(false) => incremental
          case Right(true) =>
            // Compile a complete replacement before detaching the old scene.
            // Colour-driven topology changes must be visible in resource receipts.
            val full = JavaFxSurfaceProgram.compile(None, plan)
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
              JavaFxSurfaceProbe.compileRetaining(next, JavaFxSurfaceProgram.materialMode(next), config, current) match
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
            chunk.renderedFaceCount,
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
    if !plan.validCameras then Left(JavaFxSurfaceError.IncompatiblePlan(
      "camera packets must be finite 4x4 matrices, reference visible surfaces and share a projection"))
    else if plan.fragmentSurfaces.nonEmpty || plan.layers.exists(layer => layer.interpolation == SurfaceMapInterpolation.VertexScalar || layer.scalarField.nonEmpty) then
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
