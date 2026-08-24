package scalafim.surface.view

import intaglio.*
import locus4s.Index
import mesh4s.geometry.D3PointConsumer
import scalafim.surface.*
import scala.util.hashing.MurmurHash3

private[view] final case class SurfaceWorldBounds(
  minimumX: Double,
  minimumY: Double,
  minimumZ: Double,
  maximumX: Double,
  maximumY: Double,
  maximumZ: Double
)

/** Renderer-local, immutable view of one coordinate realization. The local
  * Double coordinates are copied once from the typed mesh4s realization; the
  * world-space Float buffers and normals are then reused by every fixed-frame
  * compilation and every backend.
  */
private[view] final class SurfaceRendererGeometry private[view] (
  val source: SurfaceGeometry,
  private val localCoordinates: Array[Double],
  val packet: SurfaceMeshPacket,
  val worldBounds: SurfaceWorldBounds
):
  inline def coordinateAt(offset: Int): Double =
    localCoordinates(offset)

/** One derived renderer rendition for an ingested surface family. Topology
  * indices and every fixed coordinate packet are packed exactly once.
  */
private[view] final class SurfaceRendererRendition private (
  val topologyKey: SurfaceResourceKey,
  val indices: IntBufferView,
  private val geometries: Map[SurfaceKind, SurfaceRendererGeometry]
):
  def geometry(kind: SurfaceKind): SurfaceRendererGeometry =
    geometries(kind)

private[view] object SurfaceRendererRendition:
  def from(
    id: SurfaceId,
    surfaces: SurfaceSet,
    domain: SurfaceMeshDomain
  ): SurfaceRendererRendition =
    val indices = new IntBufferView(surfaces.default.mesh.faceIndices)
    val topologyKey = SurfaceResourceKey(
      s"mesh:${id.value}:${domain.display}:${domain.topology.stableKey}"
    )
    val packed = surfaces.surfaces.map: (kind, geometry) =>
      val local = copyLocalCoordinates(geometry)
      val world = worldPositions(local, geometry.surfaceToWorld)
      val normals = computeNormals(world, indices.unsafeArray)
      val packet = SurfaceMeshPacket(
        id,
        topologyKey,
        new FloatBufferView(world),
        new FloatBufferView(normals),
        indices,
        Some(geometryKey(id, domain, world))
      )
      kind -> new SurfaceRendererGeometry(
        geometry,
        local,
        packet,
        bounds(local, geometry.surfaceToWorld)
      )
    new SurfaceRendererRendition(topologyKey, indices, packed)

  def worldPositions(
    local: Array[Double],
    transform: scalafim.image.DMat
  ): Array[Float] =
    val world = new Array[Float](local.length)
    var offset = 0
    while offset < local.length do
      val x = local(offset)
      val y = local(offset + 1)
      val z = local(offset + 2)
      val wx = transform(0, 0) * x + transform(0, 1) * y + transform(0, 2) * z + transform(0, 3)
      val wy = transform(1, 0) * x + transform(1, 1) * y + transform(1, 2) * z + transform(1, 3)
      val wz = transform(2, 0) * x + transform(2, 1) * y + transform(2, 2) * z + transform(2, 3)
      val ww = transform(3, 0) * x + transform(3, 1) * y + transform(3, 2) * z + transform(3, 3)
      val inverseW = if ww == 0.0 then 1.0 else 1.0 / ww
      world(offset) = (wx * inverseW).toFloat
      world(offset + 1) = (wy * inverseW).toFloat
      world(offset + 2) = (wz * inverseW).toFloat
      offset += 3
    world

  def computeNormals(positions: Array[Float], indices: Array[Int]): Array[Float] =
    val accum = new Array[Double](positions.length)
    var face = 0
    while face < indices.length do
      val a = indices(face) * 3
      val b = indices(face + 1) * 3
      val c = indices(face + 2) * 3
      val abx = positions(b) - positions(a)
      val aby = positions(b + 1) - positions(a + 1)
      val abz = positions(b + 2) - positions(a + 2)
      val acx = positions(c) - positions(a)
      val acy = positions(c + 1) - positions(a + 1)
      val acz = positions(c + 2) - positions(a + 2)
      val nx = aby * acz - abz * acy
      val ny = abz * acx - abx * acz
      val nz = abx * acy - aby * acx
      accum(a) += nx; accum(a + 1) += ny; accum(a + 2) += nz
      accum(b) += nx; accum(b + 1) += ny; accum(b + 2) += nz
      accum(c) += nx; accum(c + 1) += ny; accum(c + 2) += nz
      face += 3
    val normals = new Array[Float](positions.length)
    var offset = 0
    while offset < normals.length do
      val x = accum(offset)
      val y = accum(offset + 1)
      val z = accum(offset + 2)
      val norm = math.sqrt(x * x + y * y + z * z)
      if norm > 0.0 then
        normals(offset) = (x / norm).toFloat
        normals(offset + 1) = (y / norm).toFloat
        normals(offset + 2) = (z / norm).toFloat
      offset += 3
    normals

  def geometryKey(
    id: SurfaceId,
    domain: SurfaceMeshDomain,
    positions: Array[Float]
  ): SurfaceResourceKey =
    var hash = MurmurHash3.stringHash(domain.display)
    var index = 0
    while index < positions.length do
      hash = MurmurHash3.mix(hash, java.lang.Float.floatToIntBits(positions(index)))
      index += 1
    SurfaceResourceKey(
      s"mesh:${id.value}:${domain.topology.stableKey}:${hex(MurmurHash3.finalizeHash(hash, positions.length))}"
    )

  private def copyLocalCoordinates(geometry: SurfaceGeometry): Array[Double] =
    val local = new Array[Double](geometry.vertexCount * 3)
    val realization = geometry.mesh.realization
    realization.positions.foreachD3(
      new D3PointConsumer[realization.topology.Vertex]:
        def apply(
          index: Index[realization.topology.Vertex],
          x: Double,
          y: Double,
          z: Double
        ): Unit =
          val offset = index.ordinal * 3
          local(offset) = x
          local(offset + 1) = y
          local(offset + 2) = z
    )
    local

  private def bounds(
    local: Array[Double],
    transform: scalafim.image.DMat
  ): SurfaceWorldBounds =
    var minimumX = Double.PositiveInfinity
    var minimumY = Double.PositiveInfinity
    var minimumZ = Double.PositiveInfinity
    var maximumX = Double.NegativeInfinity
    var maximumY = Double.NegativeInfinity
    var maximumZ = Double.NegativeInfinity
    var offset = 0
    while offset < local.length do
      val x = local(offset)
      val y = local(offset + 1)
      val z = local(offset + 2)
      val wx = transform(0, 0) * x + transform(0, 1) * y + transform(0, 2) * z + transform(0, 3)
      val wy = transform(1, 0) * x + transform(1, 1) * y + transform(1, 2) * z + transform(1, 3)
      val wz = transform(2, 0) * x + transform(2, 1) * y + transform(2, 2) * z + transform(2, 3)
      val ww = transform(3, 0) * x + transform(3, 1) * y + transform(3, 2) * z + transform(3, 3)
      val inverseW = if ww == 0.0 then 1.0 else 1.0 / ww
      val worldX = wx * inverseW
      val worldY = wy * inverseW
      val worldZ = wz * inverseW
      minimumX = math.min(minimumX, worldX)
      minimumY = math.min(minimumY, worldY)
      minimumZ = math.min(minimumZ, worldZ)
      maximumX = math.max(maximumX, worldX)
      maximumY = math.max(maximumY, worldY)
      maximumZ = math.max(maximumZ, worldZ)
      offset += 3
    SurfaceWorldBounds(minimumX, minimumY, minimumZ, maximumX, maximumY, maximumZ)

  private def hex(value: Int): String =
    val raw = java.lang.Integer.toHexString(value)
    "0" * (8 - raw.length) + raw

final case class SurfaceAsset private (
  id: SurfaceId,
  geometries: SurfaceSet,
  domain: SurfaceMeshDomain,
  private[view] val cameraBounds: SurfaceWorldBounds,
  private[view] val rendition: SurfaceRendererRendition
):
  def geometry: SurfaceGeometry = geometries.default

  private[view] def resolve(presentation: SurfaceGeometryPresentation): Either[SurfaceViewError, SurfaceGeometry] =
    presentation.resolve(geometries)

object SurfaceAsset:
  def make(id: SurfaceId, geometry: SurfaceGeometry): Either[SurfaceViewError, SurfaceAsset] =
    make(id, SurfaceSet.of(geometry.kind, geometry))

  def make(id: SurfaceId, geometries: SurfaceSet): Either[SurfaceViewError, SurfaceAsset] =
    geometries.meshDomainEither
      .left.map(error => SurfaceViewError.InvalidBilateralLayout(error.message))
      .map: domain =>
        val rendition = SurfaceRendererRendition.from(id, geometries, domain)
        new SurfaceAsset(
          id,
          geometries,
          domain,
          rendition.geometry(geometries.defaultKind).worldBounds,
          rendition
        )

  /** Camera framing belongs to the canonical geometry, not to the union of
    * every presentation state. This keeps a family stable while preventing a
    * distant inflated or spherical state from displacing the folded cortex.
    */
final case class SurfaceViewerModel private (
  surfaces: Vector[SurfaceAsset],
  layers: Vector[SurfaceLayer],
  frameCount: Int
):
  private lazy val surfacesById: Map[SurfaceId, SurfaceAsset] = surfaces.map(surface => surface.id -> surface).toMap
  private lazy val layersById: Map[SurfaceLayerId, SurfaceLayer] = layers.map(layer => layer.id -> layer).toMap

  def surface(id: SurfaceId): Option[SurfaceAsset] = surfacesById.get(id)
  def layer(id: SurfaceLayerId): Option[SurfaceLayer] = layersById.get(id)

object SurfaceViewerModel:
  def make(surfaces: Vector[SurfaceAsset], layers: Vector[SurfaceLayer]): Either[SurfaceViewError, SurfaceViewerModel] =
    if surfaces.isEmpty then Left(SurfaceViewError.EmptySurfaces)
    else
      firstDuplicate(surfaces.map(_.id)) match
        case Some(id) => Left(SurfaceViewError.DuplicateSurfaceId(id))
        case None =>
          firstDuplicate(layers.map(_.id)) match
            case Some(id) => Left(SurfaceViewError.DuplicateLayerId(id))
            case None =>
              val byId = surfaces.map(surface => surface.id -> surface).toMap
              var index = 0
              while index < layers.length do
                val layer = layers(index)
                byId.get(layer.surfaceId) match
                  case None => return Left(SurfaceViewError.UnknownSurface(layer.surfaceId))
                  case Some(surface) if !layer.isCompatibleWith(surface.geometry) =>
                    return Left(SurfaceViewError.IncompatibleLayerDomain(layer.id, surface.id))
                  case _ => ()
                index += 1
              val dynamicCounts = layers.iterator.map(_.frameCount).filter(_ > 1).toVector.distinct
              if dynamicCounts.length > 1 then Left(SurfaceViewError.IncompatibleFrameCounts(dynamicCounts.sorted))
              else Right(new SurfaceViewerModel(surfaces, layers, dynamicCounts.headOption.getOrElse(1)))

  private def firstDuplicate[A](values: Vector[A]): Option[A] =
    val seen = scala.collection.mutable.HashSet.empty[A]
    values.find(value => !seen.add(value))

final case class SurfaceLayerPresentation(
  id: SurfaceLayerId,
  visible: Boolean,
  opacity: DisplayOpacity,
  window: Option[DisplayWindow],
  threshold: Option[DisplayThreshold]
)

final case class SurfaceViewerState private[view] (
  layout: SurfaceLayout,
  camera: SurfaceCamera,
  lighting: SurfaceLighting,
  clipping: SurfaceClipping,
  timepoint: Int,
  selection: Option[SurfaceSelection],
  geometryPresentations: Map[SurfaceId, SurfaceGeometryPresentation],
  layerOrder: Vector[SurfaceLayerId],
  presentations: Map[SurfaceLayerId, SurfaceLayerPresentation]
)

object SurfaceViewerState:
  def initial(model: SurfaceViewerModel): SurfaceViewerState =
    val first = model.surfaces.head
    val viewpoint = SurfaceViewpoint.Lateral(first.domain.hemisphere)
    val presentations = model.layers.map: layer =>
      layer.id -> SurfaceLayerPresentation(layer.id, visible = true, layer.opacity, None, None)
    new SurfaceViewerState(
      layout = SurfaceLayout.Single(first.id),
      camera = SurfaceCamera.unsafe(viewpoint),
      lighting = SurfaceLighting.Default,
      clipping = SurfaceClipping.Disabled,
      timepoint = 0,
      selection = None,
      geometryPresentations = model.surfaces.map: surface =>
        surface.id -> SurfaceGeometryPresentation.Fixed(surface.geometries.defaultKind)
      .toMap,
      layerOrder = model.layers.map(_.id),
      presentations = presentations.toMap
    )

enum SurfaceViewerAction:
  case SetLayout(layout: SurfaceLayout)
  case SetViewpoint(viewpoint: SurfaceViewpoint)
  case SetProjection(projection: CameraProjection)
  case SetZoom(zoom: CameraZoom)
  case SetPan(x: Double, y: Double)
  case SetOrbit(orbit: SurfaceOrbit)
  case OrbitBy(yawDegrees: Double, pitchDegrees: Double)
  case ResetCamera
  case SetLighting(lighting: SurfaceLighting)
  case SetClipping(clipping: SurfaceClipping)
  case SetTimepoint(index: Int)
  case Select(surface: SurfaceId, vertex: VertexId)
  case ClearSelection
  case SetGeometryState(surface: SurfaceId, kind: SurfaceKind)
  case BeginGeometryMorph(surface: SurfaceId, target: SurfaceKind)
  case BeginGeometryLens(
    surface: SurfaceId,
    target: SurfaceKind,
    center: VertexId,
    innerRadius: SurfaceLensRadius,
    outerRadius: SurfaceLensRadius,
    policy: SurfaceLensDeformationPolicy = SurfaceLensDeformationPolicy.Natural
  )
  case ClearGeometryLens(surface: SurfaceId)
  case SetGeometryMorphFraction(surface: SurfaceId, fraction: SurfaceMorphFraction)
  case SetLayerVisible(layer: SurfaceLayerId, visible: Boolean)
  case SetLayerOpacity(layer: SurfaceLayerId, opacity: DisplayOpacity)
  case SetLayerWindow(layer: SurfaceLayerId, window: DisplayWindow)
  case SetLayerThreshold(layer: SurfaceLayerId, threshold: DisplayThreshold)
  case MoveLayer(layer: SurfaceLayerId, index: Int)

object SurfaceViewer:
  def reduce(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    action: SurfaceViewerAction
  ): Either[SurfaceViewError, SurfaceViewerState] =
    action match
      case SurfaceViewerAction.SetLayout(layout) => validateLayout(model, layout).map(_ => state.copy(layout = layout))
      case SurfaceViewerAction.SetViewpoint(viewpoint) =>
        Right(state.copy(camera = state.camera.copy(viewpoint = viewpoint)))
      case SurfaceViewerAction.SetProjection(projection) =>
        Right(state.copy(camera = state.camera.copy(projection = projection)))
      case SurfaceViewerAction.SetZoom(zoom) => Right(state.copy(camera = state.camera.copy(zoom = zoom)))
      case SurfaceViewerAction.SetPan(x, y) =>
        SurfaceCamera.make(state.camera.viewpoint, state.camera.projection, state.camera.zoom, x, y, state.camera.orbit)
          .map(camera => state.copy(camera = camera))
      case SurfaceViewerAction.SetOrbit(orbit) => Right(state.copy(camera = state.camera.copy(orbit = orbit)))
      case SurfaceViewerAction.OrbitBy(yawDegrees, pitchDegrees) =>
        SurfaceOrbit.make(
          state.camera.orbit.yawDegrees + yawDegrees,
          (state.camera.orbit.pitchDegrees + pitchDegrees).max(-89.0).min(89.0)
        ).map(orbit => state.copy(camera = state.camera.copy(orbit = orbit)))
      case SurfaceViewerAction.ResetCamera =>
        Right(state.copy(camera = SurfaceCamera.unsafe(state.camera.viewpoint, state.camera.projection)))
      case SurfaceViewerAction.SetLighting(lighting) => Right(state.copy(lighting = lighting))
      case SurfaceViewerAction.SetClipping(clipping) => Right(state.copy(clipping = clipping))
      case SurfaceViewerAction.SetTimepoint(index) =>
        if index >= 0 && index < model.frameCount then Right(state.copy(timepoint = index))
        else Left(SurfaceViewError.TimepointOutOfBounds(index, model.frameCount))
      case SurfaceViewerAction.Select(surfaceId, vertex) =>
        model.surface(surfaceId) match
          case None => Left(SurfaceViewError.UnknownSurface(surfaceId))
          case Some(surface) if vertex.index >= surface.geometry.vertexCount =>
            Left(SurfaceViewError.InvalidSelection(surfaceId, vertex.index))
          case Some(_) => Right(state.copy(selection = Some(SurfaceSelection(surfaceId, vertex))))
      case SurfaceViewerAction.ClearSelection => Right(state.copy(selection = None))
      case SurfaceViewerAction.SetGeometryState(surfaceId, kind) =>
        requireGeometry(model, surfaceId, kind).map: _ =>
          state.copy(geometryPresentations = state.geometryPresentations.updated(
            surfaceId,
            SurfaceGeometryPresentation.Fixed(kind)
          ))
      case SurfaceViewerAction.BeginGeometryMorph(surfaceId, target) =>
        beginGeometryMorph(model, state, surfaceId, target)
      case SurfaceViewerAction.BeginGeometryLens(surfaceId, target, center, innerRadius, outerRadius, policy) =>
        beginGeometryLens(model, state, surfaceId, target, center, innerRadius, outerRadius, policy)
      case SurfaceViewerAction.ClearGeometryLens(surfaceId) =>
        clearGeometryLens(state, surfaceId)
      case SurfaceViewerAction.SetGeometryMorphFraction(surfaceId, fraction) =>
        state.geometryPresentations.get(surfaceId) match
          case None => Left(SurfaceViewError.UnknownSurface(surfaceId))
          case Some(SurfaceGeometryPresentation.Fixed(_)) =>
            Left(SurfaceViewError.IncompatibleMorph("a transition must be started before its fraction is updated"))
          case Some(SurfaceGeometryPresentation.Morphing(from, to, _)) =>
            val next =
              if fraction.value == 1.0 then SurfaceGeometryPresentation.Fixed(to)
              else SurfaceGeometryPresentation.Morphing(from, to, fraction)
            Right(state.copy(geometryPresentations = state.geometryPresentations.updated(surfaceId, next)))
          case Some(SurfaceGeometryPresentation.RevealLens(from, to, deformation, _)) =>
            val next = SurfaceGeometryPresentation.RevealLens(from, to, deformation, fraction)
            Right(state.copy(geometryPresentations = state.geometryPresentations.updated(surfaceId, next)))
      case SurfaceViewerAction.SetLayerVisible(id, visible) =>
        updatePresentation(model, state, id)(presentation => Right(presentation.copy(visible = visible)))
      case SurfaceViewerAction.SetLayerOpacity(id, opacity) =>
        updatePresentation(model, state, id)(presentation => Right(presentation.copy(opacity = opacity)))
      case SurfaceViewerAction.SetLayerWindow(id, window) =>
        updatePresentation(model, state, id): presentation =>
          requireCapability(model, id, "display windows", _.supportsWindow)
            .map(_ => presentation.copy(window = Some(window)))
      case SurfaceViewerAction.SetLayerThreshold(id, threshold) =>
        updatePresentation(model, state, id): presentation =>
          requireCapability(model, id, "display thresholds", _.supportsThreshold)
            .map(_ => presentation.copy(threshold = Some(threshold)))
      case SurfaceViewerAction.MoveLayer(id, index) =>
        if !state.layerOrder.contains(id) then Left(SurfaceViewError.UnknownLayer(id))
        else if index < 0 || index >= state.layerOrder.length then
          Left(SurfaceViewError.InvalidLayerPosition(index, state.layerOrder.length))
        else
          val without = state.layerOrder.filterNot(_ == id)
          Right(state.copy(layerOrder = without.patch(index, Vector(id), 0)))

  private def updatePresentation(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    id: SurfaceLayerId
  )(
    update: SurfaceLayerPresentation => Either[SurfaceViewError, SurfaceLayerPresentation]
  ): Either[SurfaceViewError, SurfaceViewerState] =
    if model.layer(id).isEmpty then Left(SurfaceViewError.UnknownLayer(id))
    else
      state.presentations.get(id) match
        case None => Left(SurfaceViewError.UnknownLayer(id))
        case Some(presentation) =>
          update(presentation).map(next => state.copy(presentations = state.presentations.updated(id, next)))

  private def beginGeometryMorph(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    surfaceId: SurfaceId,
    target: SurfaceKind
  ): Either[SurfaceViewError, SurfaceViewerState] =
    requireGeometry(model, surfaceId, target).flatMap: _ =>
      state.geometryPresentations.get(surfaceId) match
        case None => Left(SurfaceViewError.UnknownSurface(surfaceId))
        case Some(SurfaceGeometryPresentation.Fixed(current)) =>
          val next =
            if current == target then SurfaceGeometryPresentation.Fixed(current)
            else SurfaceGeometryPresentation.Morphing(current, target, SurfaceMorphFraction.unsafe(0.0))
          Right(state.copy(geometryPresentations = state.geometryPresentations.updated(surfaceId, next)))
        case Some(current @ SurfaceGeometryPresentation.Morphing(from, to, fraction)) =>
          if target == to then Right(state)
          else if target == from then
            val reversed = SurfaceGeometryPresentation.Morphing(
              to,
              from,
              SurfaceMorphFraction.unsafe(1.0 - fraction.value)
            )
            Right(state.copy(geometryPresentations = state.geometryPresentations.updated(surfaceId, reversed)))
          else Left(SurfaceViewError.IncompatibleMorph(
            s"cannot interrupt ${from.label}->${to.label} with target '${target.label}'"
          ))
        case Some(SurfaceGeometryPresentation.RevealLens(_, _, _, _)) =>
          Left(SurfaceViewError.IncompatibleMorph("clear the active reveal lens before starting a global morph"))

  private def beginGeometryLens(
    model: SurfaceViewerModel,
    state: SurfaceViewerState,
    surfaceId: SurfaceId,
    target: SurfaceKind,
    center: VertexId,
    innerRadius: SurfaceLensRadius,
    outerRadius: SurfaceLensRadius,
    policy: SurfaceLensDeformationPolicy
  ): Either[SurfaceViewError, SurfaceViewerState] =
    requireGeometry(model, surfaceId, target).flatMap: _ =>
      state.geometryPresentations.get(surfaceId) match
        case None => Left(SurfaceViewError.UnknownSurface(surfaceId))
        case Some(SurfaceGeometryPresentation.Fixed(from)) =>
          if from == target then Left(SurfaceViewError.IncompatibleMorph("reveal lens target must differ from its source"))
          else
            requireGeometry(model, surfaceId, from).flatMap: source =>
              requireGeometry(model, surfaceId, target).flatMap: endpoint =>
                SurfaceGeodesicLens.make(source, center, innerRadius, outerRadius).flatMap: lens =>
                  SurfaceLensDeformation.make(source, endpoint, lens, policy).map: deformation =>
                    state.copy(geometryPresentations = state.geometryPresentations.updated(
                      surfaceId,
                      SurfaceGeometryPresentation.RevealLens(
                        from,
                        target,
                        deformation,
                        SurfaceMorphFraction.unsafe(0.0)
                      )
                    ))
        case Some(SurfaceGeometryPresentation.Morphing(_, _, _)) =>
          Left(SurfaceViewError.IncompatibleMorph("finish or reset the global morph before pinning a reveal lens"))
        case Some(SurfaceGeometryPresentation.RevealLens(from, to, _, fraction)) =>
          if target != to then Left(SurfaceViewError.IncompatibleMorph(
            s"cannot retarget active reveal lens ${from.label}->${to.label} to '${target.label}'"
          ))
          else
            requireGeometry(model, surfaceId, from).flatMap: source =>
              requireGeometry(model, surfaceId, to).flatMap: endpoint =>
                SurfaceGeodesicLens.make(source, center, innerRadius, outerRadius).flatMap: lens =>
                  SurfaceLensDeformation.make(source, endpoint, lens, policy).map: deformation =>
                    state.copy(geometryPresentations = state.geometryPresentations.updated(
                      surfaceId,
                      SurfaceGeometryPresentation.RevealLens(from, to, deformation, fraction)
                    ))

  private def clearGeometryLens(
    state: SurfaceViewerState,
    surfaceId: SurfaceId
  ): Either[SurfaceViewError, SurfaceViewerState] =
    state.geometryPresentations.get(surfaceId) match
      case None => Left(SurfaceViewError.UnknownSurface(surfaceId))
      case Some(SurfaceGeometryPresentation.Fixed(_)) => Right(state)
      case Some(SurfaceGeometryPresentation.Morphing(_, _, _)) =>
        Left(SurfaceViewError.IncompatibleMorph("global morph is not a reveal lens"))
      case Some(SurfaceGeometryPresentation.RevealLens(from, _, _, _)) =>
        Right(state.copy(geometryPresentations = state.geometryPresentations.updated(
          surfaceId,
          SurfaceGeometryPresentation.Fixed(from)
        )))

  private def requireGeometry(
    model: SurfaceViewerModel,
    surfaceId: SurfaceId,
    kind: SurfaceKind
  ): Either[SurfaceViewError, SurfaceGeometry] =
    model.surface(surfaceId) match
      case None => Left(SurfaceViewError.UnknownSurface(surfaceId))
      case Some(surface) =>
        surface.geometries.get(kind).toRight(SurfaceViewError.IncompatibleMorph(
          s"surface '${surfaceId.value}' has no '${kind.label}' geometry"
        ))

  private def requireCapability(
    model: SurfaceViewerModel,
    id: SurfaceLayerId,
    capability: String,
    supported: SurfaceLayer => Boolean
  ): Either[SurfaceViewError, Unit] =
    model.layer(id) match
      case None => Left(SurfaceViewError.UnknownLayer(id))
      case Some(layer) if supported(layer) => Right(())
      case Some(_) => Left(SurfaceViewError.LayerCapabilityUnsupported(id, capability))

  private[view] def validateLayout(model: SurfaceViewerModel, layout: SurfaceLayout): Either[SurfaceViewError, Unit] =
    layout match
      case SurfaceLayout.Single(id) =>
        if model.surface(id).nonEmpty then Right(()) else Left(SurfaceViewError.UnknownSurface(id))
      case SurfaceLayout.Bilateral(leftId, rightId, _) =>
        (model.surface(leftId), model.surface(rightId)) match
          case (None, _) => Left(SurfaceViewError.UnknownSurface(leftId))
          case (_, None) => Left(SurfaceViewError.UnknownSurface(rightId))
          case (Some(left), Some(right)) if leftId == rightId =>
            Left(SurfaceViewError.InvalidBilateralLayout("left and right ids must differ"))
          case (Some(left), Some(right))
              if left.domain.hemisphere != CorticalHemisphere.Left || right.domain.hemisphere != CorticalHemisphere.Right =>
            Left(SurfaceViewError.InvalidBilateralLayout("slots must contain left- then right-hemisphere geometry"))
          case _ => Right(())
