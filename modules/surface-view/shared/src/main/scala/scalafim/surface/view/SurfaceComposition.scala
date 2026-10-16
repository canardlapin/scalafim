package scalafim.surface.view

import image4s.geometry.{D3, Frame}
import intaglio.{DisplayBlendMode, DisplayOpacity, Rgba32, Scene}

opaque type SurfaceViewId = String
object SurfaceViewId:
  def make(value: String): Either[SurfaceCompositionError, SurfaceViewId] =
    if value.trim.nonEmpty then Right(value) else Left(SurfaceCompositionError.BlankViewId)
  extension (value: SurfaceViewId) def value: String = value

opaque type SurfaceInstanceId = String
object SurfaceInstanceId:
  def make(value: String): Either[SurfaceCompositionError, SurfaceInstanceId] =
    if value.trim.nonEmpty then Right(value) else Left(SurfaceCompositionError.BlankInstanceId)
  extension (value: SurfaceInstanceId) def value: String = value

enum SurfaceCompositionError:
  case BlankViewId, BlankInstanceId, EmptyComposition
  case DuplicateView(id: SurfaceViewId)
  case DuplicateInstance(id: SurfaceInstanceId)
  case InvalidViewport(id: SurfaceViewId)
  case OverlappingViewports(left: SurfaceViewId, right: SurfaceViewId)
  case InvalidTranslation(id: SurfaceInstanceId)
  case InvalidCamera(id: SurfaceViewId)
  case EmptyView(id: SurfaceViewId)
  case UnknownSurface(surface: SurfaceId)
  case StaleDisplay(surface: SurfaceId)
  case Compilation(error: SurfaceCameraError)
  case MissingMesh(key: SurfaceResourceKey)
  case MeshSurfaceMismatch(key: SurfaceResourceKey, surface: SurfaceId)
  case MissingLayer(key: SurfaceResourceKey)
  case ConflictingMesh(key: SurfaceResourceKey)
  case ConflictingLayer(key: SurfaceResourceKey)
  case DuplicateLayerInInstance(id: SurfaceInstanceId, key: SurfaceResourceKey)
  case InvalidPresentation(reason: String)
  case InvalidResource(key: SurfaceResourceKey, reason: String)

/** A display occurrence; scientific coordinates and identities remain on its surface. */
final case class SurfaceDisplayOrigin(view: SurfaceViewId, instance: SurfaceInstanceId)

final case class SurfaceCompositionInstance(
  id: SurfaceInstanceId,
  surface: SurfaceId,
  mesh: SurfaceResourceKey,
  layerOrder: Vector[SurfaceResourceKey],
  translationX: Double = 0.0,
  translationY: Double = 0.0,
  translationZ: Double = 0.0,
  opacity: DisplayOpacity = DisplayOpacity.Opaque
)

final case class SurfaceCompositionView(
  id: SurfaceViewId,
  viewport: SurfaceViewport,
  camera: SurfaceCameraPacket,
  instances: Vector[SurfaceCompositionInstance],
  lighting: SurfaceLighting,
  clipping: SurfaceClipping,
  chrome: Scene = Scene.empty,
  readouts: Vector[SurfaceReadout] = Vector.empty,
  viewportFit: SurfaceViewportFit = SurfaceViewportFit.Fill,
  fragmentSurfaces: Set[SurfaceId] = Set.empty,
  colorPipeline: SurfaceColorPipeline = SurfaceColorPipeline.LegacyDisplayBytes,
  surfaceBase: Rgba32 = SurfaceDisplayDefaults.LegacySurfaceBase
)

final case class SurfaceCompositionResources(
  meshes: Map[SurfaceResourceKey, SurfaceMeshPacket],
  layers: Map[SurfaceResourceKey, SurfaceLayerPacket]
)

final case class SurfaceCompositionReceipt(
  viewCount: Int,
  instanceCount: Int,
  meshKeys: Vector[SurfaceResourceKey],
  layerKeys: Vector[SurfaceResourceKey],
  cameraKeys: Vector[String]
)

final class SurfaceCompositionPlan private (
  val views: Vector[SurfaceCompositionView],
  val resources: SurfaceCompositionResources,
  val receipt: SurfaceCompositionReceipt
)

object SurfaceCompositionPlan:
  def make(views: Vector[SurfaceCompositionView], resources: SurfaceCompositionResources): Either[SurfaceCompositionError, SurfaceCompositionPlan] =
    if views.isEmpty then return Left(SurfaceCompositionError.EmptyComposition)
    if views.map(_.viewportFit).distinct.length != 1 then
      return Left(SurfaceCompositionError.InvalidPresentation("all composition views must use the same viewport fit"))
    resources.meshes.find((key, mesh) => key.value.trim.isEmpty || mesh.resourceKey != key) match
      case Some((key, _)) => return Left(SurfaceCompositionError.ConflictingMesh(key))
      case None => ()
    resources.layers.find((key, layer) => key.value.trim.isEmpty || layer.resourceKey != key) match
      case Some((key, _)) => return Left(SurfaceCompositionError.ConflictingLayer(key))
      case None => ()
    val resourceMeshes = resources.meshes.valuesIterator
    while resourceMeshes.hasNext do
      val mesh = resourceMeshes.next()
      invalidMesh(mesh) match
        case Some(reason) => return Left(SurfaceCompositionError.InvalidResource(mesh.resourceKey, reason))
        case None => ()
    var index = 0
    while index < views.length do
      val view = views(index)
      if !validViewport(view.viewport) then return Left(SurfaceCompositionError.InvalidViewport(view.id))
      if !validCamera(view.camera) then return Left(SurfaceCompositionError.InvalidCamera(view.id))
      if view.instances.isEmpty then return Left(SurfaceCompositionError.EmptyView(view.id))
      var instanceIndex = 0
      while instanceIndex < view.instances.length do
        val instance = view.instances(instanceIndex)
        if !instance.translationX.isFinite || !instance.translationY.isFinite || !instance.translationZ.isFinite then
          return Left(SurfaceCompositionError.InvalidTranslation(instance.id))
        resources.meshes.get(instance.mesh) match
          case None => return Left(SurfaceCompositionError.MissingMesh(instance.mesh))
          case Some(mesh) if mesh.resourceKey != instance.mesh => return Left(SurfaceCompositionError.ConflictingMesh(instance.mesh))
          case Some(mesh) if mesh.surface != instance.surface => return Left(SurfaceCompositionError.MeshSurfaceMismatch(instance.mesh, instance.surface))
          case Some(_) => ()
        var layerIndex = 0
        while layerIndex < instance.layerOrder.length do
          val key = instance.layerOrder(layerIndex)
          if instance.layerOrder.take(layerIndex).contains(key) then
            return Left(SurfaceCompositionError.DuplicateLayerInInstance(instance.id, key))
          resources.layers.get(key) match
            case None => return Left(SurfaceCompositionError.MissingLayer(key))
            case Some(layer) if layer.resourceKey != key => return Left(SurfaceCompositionError.ConflictingLayer(key))
            case Some(layer) if layer.surface != instance.surface => return Left(SurfaceCompositionError.MeshSurfaceMismatch(key, instance.surface))
            case Some(layer) =>
              invalidLayer(resources.meshes(instance.mesh), layer, view.fragmentSurfaces(instance.surface) || view.colorPipeline == SurfaceColorPipeline.DisplaySrgbLinearLight) match
                case Some(reason) => return Left(SurfaceCompositionError.InvalidResource(key, reason))
                case None => ()
          layerIndex += 1
        val stack = instance.layerOrder.map(resources.layers)
        if stack.exists(_.thresholdEdge.nonEmpty) &&
            (view.colorPipeline != SurfaceColorPipeline.DisplaySrgbLinearLight || stack.dropRight(1).exists(_.thresholdEdge.nonEmpty) ||
              stack.exists(_.interpolation != SurfaceMapInterpolation.VertexColor) || stack.last.blendMode != DisplayBlendMode.Normal) then
          return Left(SurfaceCompositionError.InvalidPresentation("threshold edges require one topmost normal layer in a linear vertex-color stack"))
        if SurfaceFragmentEvaluator.required(instance.layerOrder.map(resources.layers(_).interpolation)) && !view.fragmentSurfaces(instance.surface) then
          return Left(SurfaceCompositionError.InvalidPresentation("mixed interpolation requires fragment composition"))
        instanceIndex += 1
      var other = index + 1
      while other < views.length do
        if intersects(view.viewport, views(other).viewport) then return Left(SurfaceCompositionError.OverlappingViewports(view.id, views(other).id))
        other += 1
      index += 1
    val ids = views.map(_.id)
    duplicate(ids).fold {
      val instances = views.flatMap(_.instances)
      duplicate(instances.map(_.id)).fold {
        val meshKeys = resources.meshes.keys.toVector.sortBy(_.value)
        val layerKeys = resources.layers.keys.toVector.sortBy(_.value)
        Right(new SurfaceCompositionPlan(views, resources,
          SurfaceCompositionReceipt(views.length, instances.length, meshKeys, layerKeys, views.map(view => cameraKey(view.camera)))))
      }(id => Left(SurfaceCompositionError.DuplicateInstance(id)))
    }(id => Left(SurfaceCompositionError.DuplicateView(id)))

  def fromLegacy(plan: SurfaceRenderPlan): Either[SurfaceCompositionError, SurfaceCompositionPlan] =
    if plan.meshes.map(_.surface).distinct.length != plan.meshes.length then
      return Left(SurfaceCompositionError.InvalidPresentation("legacy plan has duplicate surfaces"))
    if plan.meshes.map(_.resourceKey).distinct.length != plan.meshes.length then
      return Left(SurfaceCompositionError.InvalidPresentation("legacy plan has duplicate mesh keys"))
    if plan.layers.map(_.resourceKey).distinct.length != plan.layers.length then
      return Left(SurfaceCompositionError.InvalidPresentation("legacy plan has duplicate layer keys"))
    val meshTable = plan.meshes.map(mesh => mesh.resourceKey -> mesh).toMap
    val layerTable = plan.layers.map(layer => layer.resourceKey -> layer).toMap
    val layersBySurface = plan.layers.groupMap(_.surface)(_.resourceKey)
    val views = Vector.newBuilder[SurfaceCompositionView]
    var index = 0
    while index < plan.slots.length do
      val slot = plan.slots(index)
      plan.meshes.find(_.surface == slot.surface) match
        case None => return Left(SurfaceCompositionError.UnknownSurface(slot.surface))
        case Some(mesh) =>
          val instance = SurfaceCompositionInstance(
            SurfaceInstanceId.make(s"legacy-instance-$index").toOption.get, slot.surface, mesh.resourceKey,
            layersBySurface.getOrElse(slot.surface, Vector.empty), slot.worldOffsetX, slot.worldOffsetY, slot.worldOffsetZ,
            plan.surfaceOpacities.getOrElse(slot.surface, DisplayOpacity.Opaque))
          views += SurfaceCompositionView(SurfaceViewId.make(s"legacy-$index").toOption.get, slot.viewport,
            plan.cameraFor(slot.surface), Vector(instance), plan.lighting, plan.clipping, plan.chrome, plan.readouts,
            plan.viewportFit, plan.fragmentSurfaces, plan.colorPipeline, plan.surfaceBase)
      index += 1
    make(views.result(), SurfaceCompositionResources(meshTable, layerTable))

  /** Resource keys name immutable payloads. Native caches must reject a caller
    * reusing a revision for different bytes before any rendering mutation.
    * Mesh resource keys name topology; geometryKey names mutable positions/normals.
    */
  private[view] def validateReuse(before: SurfaceCompositionPlan, after: SurfaceCompositionPlan): Either[SurfaceCompositionError, Unit] =
    validateResourceReuse(before.resources, after.resources)

  private[view] def validateResourceReuse(before: SurfaceCompositionResources, after: SurfaceCompositionResources): Either[SurfaceCompositionError, Unit] =
    val meshes = before.meshes.iterator
    while meshes.hasNext do
      val (key, value) = meshes.next()
      after.meshes.get(key) match
        case Some(next) if !SurfaceCompositionCompiler.compatibleMeshReuse(value, next) => return Left(SurfaceCompositionError.ConflictingMesh(key))
        case _ => ()
    val layers = before.layers.iterator
    while layers.hasNext do
      val (key, value) = layers.next()
      after.layers.get(key) match
        case Some(next) if !SurfaceCompositionCompiler.sameLayer(value, next) => return Left(SurfaceCompositionError.ConflictingLayer(key))
        case _ => ()
    Right(())

  private def invalidMesh(mesh: SurfaceMeshPacket): Option[String] =
    val count = mesh.positions.length / 3
    val samples = mesh.samplePositions.orElse(mesh.sampleNormals).fold(count)(_.length / 3)
    if mesh.positions.length % 3 != 0 || mesh.normals.length != mesh.positions.length || mesh.indices.length % 3 != 0 then
      Some("mesh position, normal, and triangle buffer shapes disagree")
    else if (0 until mesh.positions.length).exists(i => !mesh.positions(i).isFinite || !mesh.normals(i).isFinite) then
      Some("mesh positions and normals must be finite")
    else if (0 until mesh.indices.length).exists(i => mesh.indices(i) < 0 || mesh.indices(i) >= count) then
      Some("triangle index lies outside render vertices")
    else if mesh.sourceVertices.exists(v => v.length != count || (0 until v.length).exists(i => v(i) < 0)) then
      Some("source vertex provenance must cover every render vertex")
    else if mesh.sampleNormals.exists(v => v.length % 3 != 0 || (0 until v.length).exists(i => !v(i).isFinite)) ||
        mesh.samplePositions.exists(v => v.length % 3 != 0 || (0 until v.length).exists(i => !v(i).isFinite)) then
      Some("sample position and normal buffers must contain finite triples")
    else if mesh.samplePositions.nonEmpty && mesh.sampleNormals.nonEmpty && mesh.samplePositions.get.length != mesh.sampleNormals.get.length then
      Some("sample positions and normals disagree")
    else if mesh.sourceVertices.exists(v => (0 until v.length).exists(i => v(i) >= samples)) then
      Some("source vertex lies outside the sample domain")
    else if mesh.nearestPartition.exists(partition => partition.originalIndices.length % 3 != 0 ||
        partition.renderFaceCount > mesh.indices.length / 3 ||
        (0 until partition.originalIndices.length).exists(i => partition.originalIndices(i) < 0 || partition.originalIndices(i) >= samples)) then
      Some("nearest partition provenance is malformed")
    else if mesh.constantPartition.exists(triangles => triangles.length != mesh.indices.length / 3 || triangles.exists(t =>
        t.sourceFace < 0 || Vector(t.sourceVertices._1, t.sourceVertices._2, t.sourceVertices._3).exists(i => i < 0 || i >= samples))) then
      Some("constant partition provenance must cover every render face and reference original samples")
    else None

  private def invalidLayer(mesh: SurfaceMeshPacket, layer: SurfaceLayerPacket, fragments: Boolean): Option[String] =
    val scalar = layer.interpolation == SurfaceMapInterpolation.VertexScalar
    val samples = mesh.sampleNormals.getOrElse(mesh.normals).length / 3
    val faces = mesh.nearestPartition.fold(mesh.indices.length / 3)(_.originalFaceCount)
    if layer.colors.length != mesh.positions.length / 3 then Some("render color count differs from mesh vertices")
    else if SurfaceThresholdEdgeValidation.validatePacket(mesh, layer).isLeft then
      SurfaceThresholdEdgeValidation.validatePacket(mesh, layer).left.toOption
    else if scalar != layer.scalarField.nonEmpty then Some("scalar interpolation and scalar field disagree")
    else if scalar && !fragments then Some("scalar interpolation requires fragment composition")
    else if fragments && mesh.sourceVertices.nonEmpty && mesh.sampleNormals.isEmpty then Some("fragment provenance requires original sample normals")
    else if layer.association == SurfaceSampleAssociation.Face && mesh.constantPartition.exists(triangles =>
        !layer.sampleColors.exists(colors => triangles.forall(_.sourceFace < colors.length))) then
      Some("constant partition source face lies outside face samples")
    else if fragments && !layer.sampleColors.exists(colors =>
        if layer.association == SurfaceSampleAssociation.Face then
          mesh.constantPartition.fold(colors.length == faces)(triangles => triangles.forall(_.sourceFace < colors.length))
        else colors.length == samples) then
      Some("fragment colors differ from the original sample domain")
    else if layer.scalarField.exists(field => layer.association != SurfaceSampleAssociation.Vertex ||
        field.samples.length != samples || !layer.scalarMapping.exists(_.canonicalKey == field.mapping.canonicalKey)) then
      Some("scalar samples and mapping differ from their declared domain")
    else None

  private def duplicate[A](values: Vector[A]): Option[A] = values.find(value => values.count(_ == value) > 1)
  private def validViewport(box: SurfaceViewport): Boolean =
    box.x.isFinite && box.y.isFinite && box.width.isFinite && box.height.isFinite &&
      box.x >= 0 && box.y >= 0 && box.width > 0 && box.height > 0 && box.x + box.width <= 1 && box.y + box.height <= 1
  private[view] def validCamera(camera: SurfaceCameraPacket): Boolean =
    val v = camera.viewMatrix
    val p = camera.projectionMatrix
    if v.length != 16 || p.length != 16 || !(0 until 16).forall(i => v(i).isFinite && p(i).isFinite) then false
    else
      def close(a: Double, b: Double): Boolean = math.abs(a - b) <= 1e-5
      val rigid = (0 until 3).forall(row => (0 until 3).forall(other =>
        close((0 until 3).map(col => v(row * 4 + col).toDouble * v(other * 4 + col)).sum, if row == other then 1 else 0))) &&
        v(12) == 0 && v(13) == 0 && v(14) == 0 && v(15) == 1
      val determinant = v(0).toDouble * (v(5).toDouble * v(10) - v(6).toDouble * v(9)) -
        v(1).toDouble * (v(4).toDouble * v(10) - v(6).toDouble * v(8)) +
        v(2).toDouble * (v(4).toDouble * v(9) - v(5).toDouble * v(8))
      val direction = close(camera.directionX, v(8)) && close(camera.directionY, v(9)) && close(camera.directionZ, v(10))
      val axes = Vector(1, 2, 4, 6, 8, 9, 12, 13).forall(p(_) == 0) && p(0) > 0 && p(5) > 0
      val projection = (p(14) == -1 && p(15) == 0 && p(3) == 0 && p(7) == 0 && p(10) < -1 && p(11) < 0) ||
        (p(14) == 0 && p(15) == 1 && p(10) < 0)
      rigid && close(determinant, 1) && direction && axes && projection
  private def cameraKey(camera: SurfaceCameraPacket): String =
    ((0 until 16).map(index => s"${camera.viewMatrix(index)}:${camera.projectionMatrix(index)}") :+
      s"${camera.directionX}:${camera.directionY}:${camera.directionZ}").mkString("camera:")
  private def intersects(a: SurfaceViewport, b: SurfaceViewport): Boolean =
    a.x < b.x + b.width && b.x < a.x + a.width && a.y < b.y + b.height && b.y < a.y + a.height

final case class SurfaceCompositionPresentation(
  timepoint: Int,
  selection: Option[SurfaceSelection],
  geometryPresentations: Map[SurfaceId, SurfaceGeometryPresentation],
  layerOrder: Vector[SurfaceLayerId],
  presentations: Map[SurfaceLayerId, SurfaceLayerPresentation],
  colorPipeline: SurfaceColorPipeline = SurfaceColorPipeline.LegacyDisplayBytes,
  surfaceOpacities: Map[SurfaceId, DisplayOpacity] = Map.empty,
  surfaceBase: Rgba32 = SurfaceDisplayDefaults.LegacySurfaceBase
)
object SurfaceCompositionPresentation:
  def fromViewerState(state: SurfaceViewerState): SurfaceCompositionPresentation =
    SurfaceCompositionPresentation(state.timepoint, state.selection, state.geometryPresentations, state.layerOrder, state.presentations,
      state.colorPipeline, state.surfaceOpacities, state.surfaceBase)

final case class SurfaceCompositionInputInstance[F <: Frame[D3]](
  id: SurfaceInstanceId, surface: SurfaceId, display: SurfaceDisplayFrame[F],
  translationX: Double = 0.0, translationY: Double = 0.0, translationZ: Double = 0.0,
  opacity: DisplayOpacity = DisplayOpacity.Opaque
)
final case class SurfaceCompositionInputView[F <: Frame[D3]](
  id: SurfaceViewId, viewport: SurfaceViewport, camera: SurfaceCamera, pose: SurfaceCameraPose[F],
  instances: Vector[SurfaceCompositionInputInstance[F]], lighting: SurfaceLighting, clipping: SurfaceClipping,
  colorPipeline: SurfaceColorPipeline = SurfaceColorPipeline.LegacyDisplayBytes
)

object SurfaceCompositionCompiler:
  def compile[F <: Frame[D3]](model: SurfaceViewerModel, presentation: SurfaceCompositionPresentation,
    inputViews: Vector[SurfaceCompositionInputView[F]]): Either[SurfaceCompositionError, SurfaceCompositionPlan] =
    validatePresentation(model, presentation) match
      case Some(error) => return Left(error)
      case None => ()
    val meshes = scala.collection.mutable.Map.empty[SurfaceResourceKey, SurfaceMeshPacket]
    val layers = scala.collection.mutable.Map.empty[SurfaceResourceKey, SurfaceLayerPacket]
    val views = Vector.newBuilder[SurfaceCompositionView]
    var viewIndex = 0
    while viewIndex < inputViews.length do
      val input = inputViews(viewIndex)
      if input.instances.isEmpty then return Left(SurfaceCompositionError.EmptyView(input.id))
      val instances = Vector.newBuilder[SurfaceCompositionInstance]
      var camera: Option[SurfaceCameraPacket] = None
      var chrome = Scene.empty
      var readouts = Vector.empty[SurfaceReadout]
      val fit = SurfaceViewportFit.Fill
      var fragments = Set.empty[SurfaceId]
      var instanceIndex = 0
      while instanceIndex < input.instances.length do
        val item = input.instances(instanceIndex)
        if item.display.surfaceId != item.surface then return Left(SurfaceCompositionError.StaleDisplay(item.surface))
        val state = SurfaceViewerState.initial(model).copy(
          layout = SurfaceLayout.Single(item.surface), camera = input.camera, framing = CameraFraming.BoundsFit,
          lighting = input.lighting, clipping = input.clipping, timepoint = presentation.timepoint,
          selection = presentation.selection, geometryPresentations = presentation.geometryPresentations,
          layerOrder = presentation.layerOrder, presentations = presentation.presentations,
          colorPipeline = input.colorPipeline, surfaceOpacities = presentation.surfaceOpacities,
          surfaceBase = presentation.surfaceBase
        )
        SurfaceCompiler.compile(model, state, item.display, input.pose) match
          case Left(error) => return Left(SurfaceCompositionError.Compilation(error))
          case Right(compiled) =>
            var meshIndex = 0
            while meshIndex < compiled.meshes.length do
              val mesh = compiled.meshes(meshIndex)
              meshes.get(mesh.resourceKey) match
                case Some(previous) if !sameMesh(previous, mesh) => return Left(SurfaceCompositionError.ConflictingMesh(mesh.resourceKey))
                case None => meshes(mesh.resourceKey) = mesh
                case _ => ()
              meshIndex += 1
            var layerIndex = 0
            while layerIndex < compiled.layers.length do
              val layer = compiled.layers(layerIndex)
              layers.get(layer.resourceKey) match
                case Some(previous) if !sameLayer(previous, layer) => return Left(SurfaceCompositionError.ConflictingLayer(layer.resourceKey))
                case None => layers(layer.resourceKey) = layer
                case _ => ()
              layerIndex += 1
            val mesh = compiled.meshes.find(_.surface == item.surface).get
            instances += SurfaceCompositionInstance(item.id, item.surface, mesh.resourceKey,
              compiled.layers.filter(_.surface == item.surface).map(_.resourceKey), item.translationX, item.translationY, item.translationZ,
              multiplyOpacity(item.opacity, presentation.surfaceOpacities.getOrElse(item.surface, DisplayOpacity.Opaque)))
            if camera.isEmpty then
              camera = Some(compiled.camera)
              chrome = compiled.chrome
              readouts = compiled.readouts
            fragments = fragments ++ compiled.fragmentSurfaces
        instanceIndex += 1
      views += SurfaceCompositionView(input.id, input.viewport, camera.get, instances.result(), input.lighting, input.clipping,
        chrome, readouts, fit, fragments, input.colorPipeline, presentation.surfaceBase)
      viewIndex += 1
    SurfaceCompositionPlan.make(views.result(), SurfaceCompositionResources(meshes.toMap, layers.toMap))

  private def validatePresentation(model: SurfaceViewerModel, presentation: SurfaceCompositionPresentation): Option[SurfaceCompositionError] =
    val expected = model.layers.map(_.id)
    if presentation.layerOrder.distinct.length != presentation.layerOrder.length || presentation.layerOrder.toSet != expected.toSet then
      Some(SurfaceCompositionError.InvalidPresentation("layer order must contain each model layer exactly once"))
    else if presentation.presentations.keySet != expected.toSet then
      Some(SurfaceCompositionError.InvalidPresentation("layer presentations must contain each model layer exactly once"))
    else if presentation.geometryPresentations.keySet != model.surfaces.map(_.id).toSet then
      Some(SurfaceCompositionError.InvalidPresentation("geometry presentations must contain each model surface exactly once"))
    else if !presentation.surfaceOpacities.keySet.subsetOf(model.surfaces.map(_.id).toSet) then
      Some(SurfaceCompositionError.InvalidPresentation("surface opacities must reference model surfaces"))
    else
      val initial = SurfaceViewerState.initial(model)
      val action = presentation.selection.map: selected =>
        selected.face match
          case Some(face) => SurfaceViewerAction.SelectFace(selected.surface, face, selected.vertex)
          case None => SurfaceViewerAction.Select(selected.surface, selected.vertex)
      action.flatMap(value => SurfaceViewer.reduce(model, initial, value).left.toOption)
        .map(error => SurfaceCompositionError.Compilation(SurfaceCameraError.View(error)))

  private def multiplyOpacity(left: DisplayOpacity, right: DisplayOpacity): DisplayOpacity =
    DisplayOpacity.unsafe(left.toDouble * right.toDouble)

  private[view] def compatibleMeshReuse(left: SurfaceMeshPacket, right: SurfaceMeshPacket): Boolean =
    if left.geometryKey == right.geometryKey then sameMesh(left, right)
    else
      left.surface == right.surface && left.resourceKey == right.resourceKey &&
        left.positions.length == right.positions.length && left.normals.length == right.normals.length &&
        sameInts(left.indices, right.indices) && sameOptional(left.sourceVertices, right.sourceVertices)(sameInts) &&
        left.samplePositions.map(_.length) == right.samplePositions.map(_.length) &&
        left.sampleNormals.map(_.length) == right.sampleNormals.map(_.length) &&
        sameOptional(left.nearestPartition, right.nearestPartition)((a, b) => sameInts(a.originalIndices, b.originalIndices)) &&
        left.constantPartition == right.constantPartition

  private[view] def sameMesh(left: SurfaceMeshPacket, right: SurfaceMeshPacket): Boolean =
    left.surface == right.surface && left.resourceKey == right.resourceKey && left.geometryKey == right.geometryKey &&
      sameFloats(left.positions, right.positions) && sameFloats(left.normals, right.normals) && sameInts(left.indices, right.indices) &&
      sameOptional(left.sourceVertices, right.sourceVertices)(sameInts) &&
      sameOptional(left.sampleNormals, right.sampleNormals)(sameFloats) &&
      sameOptional(left.samplePositions, right.samplePositions)(sameFloats) &&
      sameOptional(left.nearestPartition, right.nearestPartition)((a, b) => sameInts(a.originalIndices, b.originalIndices)) &&
      left.constantPartition == right.constantPartition

  private[view] def sameLayer(left: SurfaceLayerPacket, right: SurfaceLayerPacket): Boolean =
    left.layer == right.layer && left.surface == right.surface && left.resourceKey == right.resourceKey &&
      left.opacity == right.opacity && left.blendMode == right.blendMode && left.association == right.association &&
      left.interpolation == right.interpolation && left.coverage == right.coverage && sameInts(left.colors, right.colors) &&
      left.scalarMapping.map(_.canonicalKey) == right.scalarMapping.map(_.canonicalKey) &&
      sameOptional(left.sampleColors, right.sampleColors)(sameInts) &&
      sameOptional(left.thresholdEdge, right.thresholdEdge)((a, b) => sameFloats(a.signs, b.signs) && sameInts(a.colors, b.colors)) &&
      sameOptional(left.scalarField, right.scalarField)((a, b) =>
        a.mapping.canonicalKey == b.mapping.canonicalKey && a.samples.length == b.samples.length &&
          (0 until a.samples.length).forall(index =>
            java.lang.Double.doubleToLongBits(a.samples(index)) == java.lang.Double.doubleToLongBits(b.samples(index))))

  private def sameOptional[A](left: Option[A], right: Option[A])(same: (A, A) => Boolean): Boolean =
    (left, right) match
      case (None, None) => true
      case (Some(a), Some(b)) => same(a, b)
      case _ => false

  private def sameFloats(left: FloatBufferView, right: FloatBufferView): Boolean =
    if left.length != right.length then false
    else
      var index = 0
      while index < left.length do
        if left(index) != right(index) then return false
        index += 1
      true

  private def sameInts(left: IntBufferView, right: IntBufferView): Boolean =
    if left.length != right.length then false
    else
      var index = 0
      while index < left.length do
        if left(index) != right(index) then return false
        index += 1
      true

