package scalafim.surface.view.javafx

import javafx.application.Platform
import javafx.geometry.Rectangle2D
import javafx.scene.{DepthTest, Group, ParallelCamera, PerspectiveCamera, SceneAntialiasing, SnapshotParameters, SubScene}
import javafx.scene.image.WritableImage
import javafx.scene.input.PickResult
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.scene.shape.{CullFace, MeshView, TriangleMesh}
import javafx.scene.transform.Affine
import intaglio.*
import scalafim.surface.{FaceId, VertexId}
import scalafim.surface.view.*

final case class JavaFxCompositionReceipt(native: JavaFxInterpretReceipt, views: Int, instances: Int, meshViews: Int)
final case class JavaFxCompositionPick(origin: SurfaceDisplayOrigin, scientific: JavaFxSurfacePick,
  worldX: Double, worldY: Double, worldZ: Double)

/** One native depth domain per view. Display occurrences share the resources
  * owned by the existing production backend; they never own or dispose an atlas.
  * The caller owns the Stage and calls resize when its host dimensions change.
  */
final class JavaFxSurfaceComposition private (private val backend: JavaFxSurfaceBackend):
  private final case class Occurrence(origin: SurfaceDisplayOrigin, chunk: JavaFxSurfaceChunk, node: MeshView)
  private final case class DecorationOccurrence(packet: SurfaceDecorationMesh, node: MeshView)
  private final case class ViewScene(id: SurfaceViewId, scene: SubScene, instances: Group, cameraTransform: Affine,
    layoutTransform: Affine, projectionSpace: Affine, perspective: PerspectiveCamera, parallel: ParallelCamera)
  val root: Group = new Group()
  private var current: Option[SurfaceCompositionPlan] = None
  private var size: Option[JavaFxSnapshotConfig] = None
  private var views = Vector.empty[ViewScene]
  private var occurrences = Vector.empty[Occurrence]
  private var pool = Vector.empty[JavaFxSurfaceChunk]
  private var decorations = Vector.empty[DecorationOccurrence]
  private var decorationGeometry: Option[SurfaceDecorationGeometry] = None
  private var disposed = false

  def plan: Option[SurfaceCompositionPlan] = current
  def resourceKeys: Set[SurfaceResourceKey] = backend.resourceKeys
  def subScenes: Map[SurfaceViewId, SubScene] = views.map(view => view.id -> view.scene).toMap

  def render(next: SurfaceCompositionPlan, dimensions: JavaFxSnapshotConfig): Either[JavaFxSurfaceError, JavaFxCompositionReceipt] =
    requireActive().flatMap: _ =>
      val reuse = current.fold[Either[SurfaceCompositionError, Unit]](Right(()))(SurfaceCompositionPlan.validateReuse(_, next))
      reuse.left.map(error => JavaFxSurfaceError.IncompatiblePlan(s"immutable composition resource changed: $error"))
        .flatMap(_ => JavaFxSurfaceComposition.resourcePlan(next)).flatMap: proxy =>
          backend.render(proxy).flatMap: receipt =>
            nativeStep("composition render"):
              clearDecorations()
              val chunks = backend.chunks
              val changedInstances = current.forall(_.views.map(v => (v.id, v.instances)) != next.views.map(v => (v.id, v.instances)))
              if chunks != pool || changedInstances || size.forall(_.antialiasing != dimensions.antialiasing) then
                rebuildViews(next, chunks, dimensions)
              current = Some(next)
              pool = chunks
              size = Some(dimensions)
              layout(next, dimensions)
              JavaFxCompositionReceipt(receipt, next.views.length, next.views.map(_.instances.length).sum, occurrences.length)

  def renderDecorated(next: SurfaceCompositionPlan, geometry: SurfaceDecorationGeometry,
      dimensions: JavaFxSnapshotConfig): Either[JavaFxSurfaceError, JavaFxCompositionReceipt] =
    requireActive().flatMap: _ =>
      SurfaceDecorationValidation.validate(geometry, next, dimensions.width, dimensions.height)
        .left.map(JavaFxSurfaceError.IncompatiblePlan(_)).flatMap: _ =>
          JavaFxSurfaceComposition.validateDepthPolicy(geometry).flatMap(_ => validateLabelPlanes(geometry, next))
            .flatMap(_ => stageDecorations(geometry)).flatMap: staged =>
            render(next, dimensions).flatMap: receipt =>
              nativeStep("decoration mount"):
                decorations = staged
                decorationGeometry = Some(geometry)
                // JavaFX depth-tested nodes also write depth. Opaque geometry
                // precedes transparent label planes; the latter draw far first.
                staged.sortBy: decoration =>
                  val packet = decoration.packet
                  val group = if packet.depthPolicy == SurfaceDecorationDepthPolicy.Overlay then 2
                    else if packet.texture.nonEmpty then 1 else 0
                  val depth = if group == 1 then labelEyeDepth(packet, next) else 0.0
                  (group, depth)
                .foreach: decoration =>
                  views.find(_.id == decoration.packet.origin.view).get.instances.getChildren.add(decoration.node)
                receipt

  def pickDisplay(result: PickResult): Either[JavaFxInteractionError, SurfaceDisplayHit[JavaFxCompositionPick]] =
    if disposed then Left(JavaFxInteractionError.Disposed)
    else if !Platform.isFxApplicationThread then
      Left(JavaFxInteractionError.Backend(JavaFxSurfaceError.IncompatiblePlan("composition picking requires the Application Thread")))
    else if result == null || result.getIntersectedNode == null then Left(JavaFxInteractionError.PickMiss)
    else
      val scientific = pick(result).toOption
      val nativeDecoration = decorations.find(d => d.packet.pickable && (d.node eq result.getIntersectedNode))
      val origin = scientific.map(_.origin).orElse(nativeDecoration.map(_.packet.origin))
      val nativePoint = result.getIntersectedPoint
      val projected = for
        plan <- current
        dimensions <- size
        occurrence <- origin
        view <- plan.views.find(_.id == occurrence.view)
        instance <- view.instances.find(_.id == occurrence.instance)
        point = if scientific.nonEmpty then SurfaceDecorationWorldPoint(nativePoint.getX + instance.translationX,
          nativePoint.getY + instance.translationY, nativePoint.getZ + instance.translationZ)
        else SurfaceDecorationWorldPoint(nativePoint.getX, nativePoint.getY, nativePoint.getZ)
        pixel <- SurfaceDecorationPicking.project(plan, occurrence.view, point, dimensions.width, dimensions.height)
      yield pixel
      val hit = for
        geometry <- decorationGeometry
        plan <- current
        point <- projected
        hit <- SurfaceDecorationPicking.pick(geometry, plan, point.x, point.y, scientific.map(_ => point.depth))
      yield hit
      hit.map(value => Right(SurfaceDisplayHit.Decoration(value)))
        .getOrElse(scientific.map(value => Right(SurfaceDisplayHit.Scientific(value)))
          .getOrElse(Left(JavaFxInteractionError.UnknownPickNode)))

  private def labelEyeDepth(mesh: SurfaceDecorationMesh, plan: SurfaceCompositionPlan): Double =
    val matrix = plan.views.find(_.id == mesh.origin.view).get.camera.viewMatrix
    matrix(8) * mesh.positions(0).toDouble + matrix(9) * mesh.positions(1) +
      matrix(10) * mesh.positions(2) + matrix(11)

  /** Only camera-facing texture planes admit a consistent object depth order.
    * Arbitrary intersecting textured meshes need the raster visibility route.
    */
  private def validateLabelPlanes(geometry: SurfaceDecorationGeometry,
      plan: SurfaceCompositionPlan): Either[JavaFxSurfaceError, Unit] =
    val invalid = geometry.meshes.exists: mesh =>
      if mesh.texture.nonEmpty && mesh.depthPolicy == SurfaceDecorationDepthPolicy.DepthTest then
        val matrix = plan.views.find(_.id == mesh.origin.view).get.camera.viewMatrix
        val expected = labelEyeDepth(mesh, plan)
        val scale = (0 until mesh.positions.length).map(i => math.abs(mesh.positions(i))).max.max(1f)
        val tolerance = 8 * java.lang.Math.ulp(scale).toDouble
        (0 until mesh.positions.length / 3).exists: vertex =>
          val i = vertex * 3
          val actual = matrix(8) * mesh.positions(i).toDouble + matrix(9) * mesh.positions(i + 1) +
            matrix(10) * mesh.positions(i + 2) + matrix(11)
          math.abs(actual - expected) > tolerance
      else false
    if invalid then Left(JavaFxSurfaceError.IncompatiblePlan("depth-tested textures require camera-facing label planes"))
    else Right(())

  private def stageDecorations(geometry: SurfaceDecorationGeometry): Either[JavaFxSurfaceError, Vector[DecorationOccurrence]] =
    try Right(geometry.meshes.map: packet =>
      val mesh = new TriangleMesh()
      mesh.getPoints.setAll(Array.tabulate(packet.positions.length)(packet.positions(_))*)
      val uv = packet.texture.map(t => Array.tabulate(t.coordinates.length)(t.coordinates(_))).getOrElse(Array(0.5f, 0.5f))
      mesh.getTexCoords.setAll(uv*)
      val faces = Array.tabulate(packet.indices.length * 2): index =>
        if index % 2 == 0 then packet.indices(index / 2)
        else if packet.texture.nonEmpty then packet.indices(index / 2) else 0
      mesh.getFaces.setAll(faces*)
      val image = packet.texture match
        case Some(texture) =>
          val result = new WritableImage(texture.image.width, texture.image.height)
          var y = 0
          while y < texture.image.height do
            var x = 0
            while x < texture.image.width do
              val c = texture.image.pixelUnsafe(x, y)
              result.getPixelWriter.setArgb(x, y, (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue)
              x += 1
            y += 1
          result
        case None =>
          val result = new WritableImage(1, 1)
          val c = packet.color
          result.getPixelWriter.setArgb(0, 0, (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue)
          result
      val material = new PhongMaterial(Color.BLACK)
      material.setDiffuseMap(image)
      material.setSelfIlluminationMap(image)
      material.setSpecularColor(Color.TRANSPARENT)
      val node = new MeshView(mesh)
      node.setMaterial(material)
      node.setCullFace(CullFace.NONE)
      node.setMouseTransparent(!packet.pickable)
      node.setDepthTest(if packet.depthPolicy == SurfaceDecorationDepthPolicy.DepthTest then DepthTest.ENABLE else DepthTest.DISABLE)
      DecorationOccurrence(packet, node)
    )
    catch case scala.util.control.NonFatal(error) =>
      Left(JavaFxSurfaceError.IncompatiblePlan(s"decoration staging failed: ${error.getMessage}"))

  private def clearDecorations(): Unit =
    decorations.foreach: decoration =>
      views.find(_.id == decoration.packet.origin.view).foreach(_.instances.getChildren.remove(decoration.node))
      decoration.node.setMesh(null)
      decoration.node.setMaterial(null)
    decorations = Vector.empty
    decorationGeometry = None

  def resize(dimensions: JavaFxSnapshotConfig): Either[JavaFxSurfaceError, Unit] =
    requireActive().flatMap: _ =>
      current.toRight(JavaFxSurfaceError.IncompatiblePlan("render a composition before resizing")).flatMap: plan =>
        if decorations.nonEmpty then
          Left(JavaFxSurfaceError.IncompatiblePlan("decorations must be lowered again at the new size; use renderDecorated"))
        else if size.exists(_.antialiasing != dimensions.antialiasing) then
          Left(JavaFxSurfaceError.IncompatiblePlan("resize cannot change native antialiasing; render with the new configuration"))
        else nativeStep("composition resize"):
          layout(plan, dimensions)
          size = Some(dimensions)

  def snapshot(): Either[JavaFxSurfaceError, WritableImage] =
    requireActive().flatMap: _ =>
      size.toRight(JavaFxSurfaceError.IncompatiblePlan("render a composition before snapshotting")).flatMap: dimensions =>
        nativeStep("composition snapshot"):
          val parameters = new SnapshotParameters()
          parameters.setFill(if dimensions.transparent then Color.TRANSPARENT else Color.WHITE)
          parameters.setViewport(new Rectangle2D(0, 0, dimensions.width, dimensions.height))
          root.snapshot(parameters, new WritableImage(dimensions.width, dimensions.height))

  def pick(result: PickResult): Either[JavaFxInteractionError, JavaFxCompositionPick] =
    if disposed then Left(JavaFxInteractionError.Disposed)
    else if !Platform.isFxApplicationThread then
      Left(JavaFxInteractionError.Backend(JavaFxSurfaceError.IncompatiblePlan("composition picking requires the Application Thread")))
    else if result == null || result.getIntersectedNode == null then Left(JavaFxInteractionError.PickMiss)
    else occurrences.find(_.node eq result.getIntersectedNode) match
      case None => Left(JavaFxInteractionError.UnknownPickNode)
      case Some(occurrence) =>
        val localFace = result.getIntersectedFace
        occurrence.chunk.packetFace(localFace).toRight(JavaFxInteractionError.InvalidPickedFace(localFace)).map: face =>
          val packet = current.get.resources.meshes(occurrence.chunk.meshKey)
          val point = result.getIntersectedPoint
          val weights = JavaFxSurfaceComposition.barycentric(packet, face, point.getX, point.getY, point.getZ)
          val original = packet.sourceBarycentric(face, weights._1, weights._2, weights._3)
          val scientific = JavaFxSurfacePick(packet.surface, FaceId(packet.sourceFace(face)),
            VertexId(packet.pickedVertex(face, weights._1, weights._2, weights._3)), original._1, original._2, original._3)
          // Native PickResult is already in the MeshView's local coordinates;
          // display translation is on the node and must not be subtracted twice.
          JavaFxCompositionPick(occurrence.origin, scientific, point.getX, point.getY, point.getZ)

  def dispose(): Either[JavaFxSurfaceError, Unit] =
    if !Platform.isFxApplicationThread then Left(JavaFxSurfaceError.IncompatiblePlan("composition disposal requires the Application Thread"))
    else if disposed then Right(())
    else backend.dispose().map: _ =>
      releaseViews()
      pool = Vector.empty
      current = None
      size = None
      disposed = true

  /** On a native failure, detach every mounted occurrence. Retained resource
    * ownership stays with the backend, and the next render rebuilds the host.
    */
  private def nativeStep[A](operation: String)(body: => A): Either[JavaFxSurfaceError, A] =
    try Right(body)
    catch
      case scala.util.control.NonFatal(error) =>
        releaseViews()
        current = None
        pool = Vector.empty
        size = None
        Left(JavaFxSurfaceError.IncompatiblePlan(s"$operation failed: ${error.getClass.getSimpleName}: ${error.getMessage}"))

  private def requireActive(): Either[JavaFxSurfaceError, Unit] =
    if !Platform.isFxApplicationThread then Left(JavaFxSurfaceError.IncompatiblePlan("composition requires the Application Thread"))
    else if disposed then Left(JavaFxSurfaceError.IncompatiblePlan("composition has been disposed"))
    else Right(())

  private def releaseViews(): Unit =
    clearDecorations()
    views.foreach: view =>
      view.scene.setCamera(null)
      view.scene.setRoot(new Group())
    occurrences.foreach: occurrence =>
      occurrence.node.setMesh(null)
      occurrence.node.setMaterial(null)
    views = Vector.empty
    occurrences = Vector.empty
    root.getChildren.clear()

  private def rebuildViews(plan: SurfaceCompositionPlan, chunks: Vector[JavaFxSurfaceChunk], dimensions: JavaFxSnapshotConfig): Unit =
    val nextOccurrences = Vector.newBuilder[Occurrence]
    val nextViews = plan.views.map: view =>
      val instances = new Group()
      instances.setDepthTest(DepthTest.ENABLE)
      val layoutTransform = new Affine()
      instances.getTransforms.add(layoutTransform)
      view.instances.foreach: instance =>
        chunks.filter(_.meshKey == instance.mesh).foreach: chunk =>
          val node = new MeshView(chunk.mesh)
          node.setMaterial(chunk.material)
          node.setCullFace(CullFace.NONE)
          node.setDepthTest(DepthTest.ENABLE)
          node.setTranslateX(instance.translationX)
          node.setTranslateY(instance.translationY)
          node.setTranslateZ(instance.translationZ)
          instances.getChildren.add(node)
          nextOccurrences += Occurrence(SurfaceDisplayOrigin(view.id, instance.id), chunk, node)
      val cameraTransform = new Affine()
      val projectionSpace = new Affine()
      val content = new Group(instances)
      content.setDepthTest(DepthTest.ENABLE)
      content.getTransforms.addAll(projectionSpace, cameraTransform)
      val scene = new SubScene(content, 1, 1, true, dimensions.antialiasing)
      ViewScene(view.id, scene, instances, cameraTransform, layoutTransform, projectionSpace,
        new PerspectiveCamera(false), new ParallelCamera())
    releaseViews()
    views = nextViews
    occurrences = nextOccurrences.result()
    root.getChildren.setAll(nextViews.map(_.scene)*)

  private def layout(plan: SurfaceCompositionPlan, dimensions: JavaFxSnapshotConfig): Unit =
    val slots = plan.views.map(view => SurfaceViewSlot(view.instances.head.surface, view.viewport))
    val boxes = plan.views.head.viewportFit.resolve(slots, dimensions.width, dimensions.height).map(_.viewport)
    plan.views.zip(views).zip(boxes).foreach: (entry, box) =>
      val (view, mounted) = entry
      val width = dimensions.width * box.width
      val height = dimensions.height * box.height
      mounted.scene.setTranslateX(dimensions.width * box.x)
      mounted.scene.setTranslateY(dimensions.height * box.y)
      mounted.scene.setWidth(width)
      mounted.scene.setHeight(height)
      mounted.scene.setFill(if dimensions.transparent then Color.TRANSPARENT else Color.WHITE)
      val surface = view.instances.head.surface
      val cameraPlan = SurfaceRenderPlan(Vector(SurfaceViewSlot(surface, SurfaceViewport(0, 0, 1, 1))),
        Vector.empty, Vector.empty, view.camera, view.lighting, view.clipping, Vector.empty, Scene.empty, Vector.empty,
        SurfaceProfile(0, 0, 0, 0, 0, 0L), SurfaceRenderReceipt(Vector.empty, Vector.empty, "camera", 0, 0))
      JavaFxSurfaceProbe.configureCamera(mounted.perspective, mounted.cameraTransform, cameraPlan)
      JavaFxSurfaceProbe.configureLayout(cameraPlan, Map(surface -> mounted.layoutTransform), width, height)
      if JavaFxSurfaceProbe.isOrthographic(view.camera) then
        mounted.projectionSpace.setToIdentity()
        val eye = height * .5 / math.tan(math.Pi / 12)
        val halfDepth = math.max(width, height) * .5
        mounted.parallel.setNearClip((eye - halfDepth) / eye)
        mounted.parallel.setFarClip((eye + halfDepth) / eye)
        mounted.scene.setCamera(mounted.parallel)
      else
        val eye = height * .5 / math.tan(math.toRadians(mounted.perspective.getFieldOfView) * .5)
        mounted.projectionSpace.setToTransform(eye, 0, 0, width * .5, 0, eye, 0, height * .5, 0, 0, eye, -eye)
        mounted.scene.setCamera(mounted.perspective)

object JavaFxSurfaceComposition:
  private[javafx] def validateDepthPolicy(geometry: SurfaceDecorationGeometry): Either[JavaFxSurfaceError, Unit] =
    if geometry.meshes.exists(_.depthPolicy == SurfaceDecorationDepthPolicy.OrderedDepthTest) then
      Left(JavaFxSurfaceError.IncompatiblePlan("ordered depth testing without depth writes requires the raster fallback"))
    else Right(())

  val DefaultAtlas: JavaFxAtlasConfig = JavaFxAtlasConfig.make(lightingPolicy = JavaFxAtlasLighting.WorldVertexLambert).toOption.get

  def create(config: JavaFxAtlasConfig = DefaultAtlas): Either[JavaFxSurfaceError, JavaFxSurfaceComposition] =
    if config.lightingPolicy != JavaFxAtlasLighting.WorldVertexLambert then
      Left(JavaFxSurfaceError.IncompatiblePlan("composition requires explicit world-space vertex lighting"))
    else JavaFxSurfaceBackend.create(config).map(new JavaFxSurfaceComposition(_))

  /** Preflight is toolkit-free, deterministic, and occurs before native mutation.
    * A shared baked atlas needs the same layer stack and lighting in every view.
    */
  private[javafx] def resourcePlan(plan: SurfaceCompositionPlan): Either[JavaFxSurfaceError, SurfaceRenderPlan] =
    val first = plan.views.head
    if plan.views.exists(v => v.camera.projectionMatrix(3) != 0f || v.camera.projectionMatrix(7) != 0f) then
      return Left(JavaFxSurfaceError.IncompatiblePlan("asymmetric orthographic frusta require JavaFxRasterSurfaceHost"))
    val active = plan.views.flatMap(view => view.instances.map(instance => (view, instance)))
    if plan.views.exists(view =>
      view.colorPipeline != SurfaceColorPipeline.LegacyDisplayBytes ||
        view.surfaceBase != SurfaceDisplayDefaults.LegacySurfaceBase) ||
        active.exists { case (_, instance) => instance.opacity != DisplayOpacity.Opaque } then
      return Left(JavaFxSurfaceError.IncompatiblePlan("composition requires the legacy color pipeline, gray opaque base, and opaque surfaces"))
    if plan.views.exists(_.lighting != first.lighting) then
      return Left(JavaFxSurfaceError.IncompatiblePlan("a shared JavaFX atlas requires identical lighting across views"))
    if plan.views.exists(view => view.clipping.isInstanceOf[SurfaceClipping.WorldPlanes] || view.fragmentSurfaces.nonEmpty) then
      return Left(JavaFxSurfaceError.IncompatiblePlan("composition contains unsupported JavaFX fragment or world-clipping semantics"))
    val descriptions = scala.collection.mutable.LinkedHashMap.empty[SurfaceId, SurfaceCompositionInstance]
    var activeIndex = 0
    while activeIndex < active.length do
      val instance = active(activeIndex)._2
      descriptions.get(instance.surface) match
        case Some(previous) if previous.mesh != instance.mesh || previous.layerOrder != instance.layerOrder =>
          return Left(JavaFxSurfaceError.IncompatiblePlan("a shared JavaFX surface requires identical mesh and ordered layers across instances"))
        case None => descriptions(instance.surface) = instance
        case _ => ()
      activeIndex += 1
    val instances = descriptions.values.toVector
    val meshes = instances.map(instance => plan.resources.meshes(instance.mesh))
    val layers = instances.flatMap(instance => instance.layerOrder.map(plan.resources.layers))
    val slots = meshes.map(mesh => SurfaceViewSlot(mesh.surface, SurfaceViewport(0, 0, 1, 1)))
    val passes = instances.zipWithIndex.flatMap: (instance, index) =>
      instance.layerOrder.map(key => SurfaceDrawPass(index, instance.mesh, key, plan.resources.layers(key).blendMode))
    val bytes = meshes.map(mesh => (mesh.positions.length.toLong + mesh.normals.length + mesh.indices.length) * 4).sum +
      layers.map(_.colors.length.toLong * 4).sum
    val profile = SurfaceProfile(meshes.length, meshes.map(_.positions.length / 3).sum, meshes.map(_.indices.length / 3).sum,
      layers.length, layers.map(_.colors.length).sum, bytes)
    val proxy = SurfaceRenderPlan(slots, meshes, layers, first.camera, first.lighting, SurfaceClipping.Disabled,
      passes, Scene.empty, Vector.empty, profile,
      SurfaceRenderReceipt(meshes.map(_.resourceKey), layers.map(_.resourceKey), "composition-resource-pool", passes.length, 0))
    JavaFxSurfaceBackend.validateCapabilities(proxy).map(_ => proxy)

  private[javafx] def barycentric(packet: SurfaceMeshPacket, face: Int, x: Double, y: Double, z: Double): (Double, Double, Double) =
    val offset = face * 3
    val a = packet.indices(offset) * 3
    val b = packet.indices(offset + 1) * 3
    val c = packet.indices(offset + 2) * 3
    val ab = Vector.tabulate(3)(i => packet.positions(b + i).toDouble - packet.positions(a + i))
    val ac = Vector.tabulate(3)(i => packet.positions(c + i).toDouble - packet.positions(a + i))
    val ap = Vector(x - packet.positions(a), y - packet.positions(a + 1), z - packet.positions(a + 2))
    def dot(u: Vector[Double], v: Vector[Double]) = u.zip(v).map((l, r) => l * r).sum
    val d00 = dot(ab, ab); val d01 = dot(ab, ac); val d11 = dot(ac, ac)
    val d20 = dot(ap, ab); val d21 = dot(ap, ac)
    val denominator = d00 * d11 - d01 * d01
    if denominator == 0 then (1.0, 0.0, 0.0)
    else
      val wb = (d11 * d20 - d01 * d21) / denominator
      val wc = (d00 * d21 - d01 * d20) / denominator
      (1 - wb - wc, wb, wc)
