package scalafim.surface.view

import image4s.geometry.{D3, Frame, Vec}
import intaglio.*

opaque type SurfaceResourceKey = String

object SurfaceResourceKey:
  private[view] def apply(value: String): SurfaceResourceKey = value

  extension (key: SurfaceResourceKey)
    def value: String = key

final class FloatBufferView private[view] (private val values: Array[Float]):
  def length: Int = values.length
  inline def apply(index: Int): Float = values(index)
  private[scalafim] def unsafeArray: Array[Float] = values

/** Raw scalar samples retain Double precision, including nonfinite values. */
final class DoubleBufferView private[view] (private val values: Array[Double]):
  def length: Int = values.length
  inline def apply(index: Int): Double = values(index)

final class SurfaceScalarPacket private[view] (
  val samples: DoubleBufferView,
  val mapping: ScalarMapping
)

final class IntBufferView private[view] (private val values: Array[Int]):
  def length: Int = values.length
  inline def apply(index: Int): Int = values(index)
  private[scalafim] def unsafeArray: Array[Int] = values

final case class SurfaceViewport(x: Double, y: Double, width: Double, height: Double)

/** A surface-specific viewport plus a display-only world translation. The
  * offset recenters independently stored hemispheres for bilateral viewing;
  * scientific coordinates and readouts remain unchanged.
  */
final case class SurfaceViewSlot(
  surface: SurfaceId,
  viewport: SurfaceViewport,
  worldOffsetX: Double = 0.0,
  worldOffsetY: Double = 0.0,
  worldOffsetZ: Double = 0.0
)

final case class SurfaceMeshPacket(
  surface: SurfaceId,
  resourceKey: SurfaceResourceKey,
  positions: FloatBufferView,
  normals: FloatBufferView,
  indices: IntBufferView,
  private val geometryRevision: Option[SurfaceResourceKey] = None,
  sourceVertices: Option[IntBufferView] = None,
  nearestPartition: Option[SurfaceNearestPartition] = None,
  sampleNormals: Option[FloatBufferView] = None,
  samplePositions: Option[FloatBufferView] = None,
  constantPartition: Option[Vector[SurfaceMappingTriangle]] = None
):
  def geometryKey: SurfaceResourceKey = geometryRevision.getOrElse(resourceKey)
  /** Original sample owner (generated partition corners are not scientific vertices). */
  def sourceVertex(renderVertex: Int): Int = sourceVertices.fold(renderVertex)(_(renderVertex))

  def sourceFace(renderFace: Int): Int = constantPartition match
    case Some(triangles) => triangles(renderFace).sourceFace
    case None => nearestPartition.fold(renderFace)(_.sourceFace(renderFace))

  def sourceFaceVertices(renderFace: Int): (Int, Int, Int) = constantPartition match
    case Some(triangles) => triangles(renderFace).sourceVertices
    case None => nearestPartition match
      case Some(partition) if renderFace < partition.renderFaceCount =>
        val offset = partition.sourceFace(renderFace) * 3
        (partition.originalIndices(offset), partition.originalIndices(offset + 1), partition.originalIndices(offset + 2))
      case _ =>
        val offset = renderFace * 3
        (sourceVertex(indices(offset)), sourceVertex(indices(offset + 1)), sourceVertex(indices(offset + 2)))

  def sourceBarycentric(renderFace: Int, a: Double, b: Double, c: Double): (Double, Double, Double) =
    constantPartition match
      case None => nearestPartition.fold((a, b, c))(_.sourceWeights(renderFace, a, b, c))
      case Some(triangles) =>
        val t = triangles(renderFace)
        (a * t.a.a + b * t.b.a + c * t.c.a,
         a * t.a.b + b * t.b.b + c * t.c.b,
         a * t.a.c + b * t.b.c + c * t.c.c)

  def pickedVertex(renderFace: Int, a: Double, b: Double, c: Double): Int =
    val (ia, ib, ic) = sourceFaceVertices(renderFace)
    val (wa, wb, wc) = sourceBarycentric(renderFace, a, b, c)
    SurfaceNearestPartition.nearestVertex(ia, ib, ic, wa, wb, wc)

/** Original face domain for a layer. Appended display geometry has its own
  * coverage; invalid scalar colors must never substitute for domain exclusion.
  */
enum SurfaceLayerCoverage(private val firstFace: Int, private val untilFace: Int):
  case All extends SurfaceLayerCoverage(0, Int.MaxValue)
  case Faces(first: Int, until: Int) extends SurfaceLayerCoverage(first, until)

  require(firstFace >= 0 && untilFace >= firstFace)

  def contains(face: Int): Boolean = face >= firstFace && face < untilFace

/** Colors are indexed by render vertex. For face association the compiler
  * repeats the face color at its three separate render corners; association
  * records the scientific sample domain, not a second buffer indexing rule.
  */
final case class SurfaceLayerPacket(
  layer: SurfaceLayerId,
  surface: SurfaceId,
  resourceKey: SurfaceResourceKey,
  colors: IntBufferView,
  opacity: DisplayOpacity,
  blendMode: DisplayBlendMode,
  association: SurfaceSampleAssociation = SurfaceSampleAssociation.Vertex,
  interpolation: SurfaceMapInterpolation = SurfaceMapInterpolation.VertexColor,
  scalarMapping: Option[ScalarMapping] = None,
  scalarField: Option[SurfaceScalarPacket] = None,
  sampleColors: Option[IntBufferView] = None,
  coverage: SurfaceLayerCoverage = SurfaceLayerCoverage.All,
  thresholdEdge: Option[SurfaceThresholdEdgePacket] = None
)

final case class SurfaceCameraPacket(
  viewMatrix: FloatBufferView,
  projectionMatrix: FloatBufferView,
  directionX: Double,
  directionY: Double,
  directionZ: Double
)

object SurfaceCameraPacket:
  /** Checked explicit camera for public composition clients. Uses the supplied
    * up direction and frustum without fitting the mesh. High-level viewpoint
    * cameras and scene-document framing retain their existing behavior.
    */
  def lookAt[F <: Frame[D3]](pose: SurfaceCameraPose[F], up: Vec[F, D3],
      frustum: SurfaceCameraFrustum): Either[SurfaceCameraFrustumError, SurfaceCameraPacket] =
    Frame.alignOwners[D3, F, F](up.frame, pose.frame).flatMap(_.vectorToRight(up))
      .left.map(SurfaceCameraFrustumError.FrameMismatch.apply).flatMap: ownedUp =>
        checkedPacket(pose, ownedUp, frustum)

  private def checkedPacket[F <: Frame[D3]](pose: SurfaceCameraPose[F], up: Vec[F, D3],
      frustum: SurfaceCameraFrustum): Either[SurfaceCameraFrustumError, SurfaceCameraPacket] =
    val u = up.coordinates
    val direction = pose.direction.coordinates
    val norm = math.sqrt(u(0) * u(0) + u(1) * u(1) + u(2) * u(2))
    val parallel = if norm == 0.0 then Double.NaN else math.abs((u(0) * direction(0) + u(1) * direction(1) + u(2) * direction(2)) / norm)
    if !u.forall(_.isFinite) || !norm.isFinite || norm <= 0.0 || !parallel.isFinite || parallel >= 1.0 - 1e-12 then
      Left(SurfaceCameraFrustumError.InvalidUp(u(0), u(1), u(2)))
    else projection(frustum).flatMap: matrix =>
      val depthRepresentable = frustum match
        case _: SurfaceCameraFrustum.Perspective => matrix(10) < -1f && matrix(11) < 0f
        case _: SurfaceCameraFrustum.Orthographic => matrix(10) < 0f && matrix(11) < -1f
      if !matrix.forall(_.isFinite) || matrix(0) <= 0f || matrix(5) <= 0f || !depthRepresentable then
        Left(SurfaceCameraFrustumError.UnrepresentableProjection)
      else
        val eye = pose.eye.coordinates
        val target = pose.target.coordinates
        val view = SurfaceCompiler.lookAt(eye(0), eye(1), eye(2), target(0), target(1), target(2), u(0), u(1), u(2))
        if !view.forall(_.isFinite) then Left(SurfaceCameraFrustumError.UnrepresentablePose)
        else Right(SurfaceCameraPacket(new FloatBufferView(view), new FloatBufferView(matrix), direction(0), direction(1), direction(2)))

  private def projection(frustum: SurfaceCameraFrustum): Either[SurfaceCameraFrustumError, Array[Float]] = frustum match
    case SurfaceCameraFrustum.Perspective(fov, aspect, near, far) if fov.isFinite && fov > 0 && fov < 180 && aspect.isFinite && aspect > 0 && near.isFinite && far.isFinite && near > 0 && far > near =>
      Right(SurfaceCompiler.perspective(fov, aspect, near, far))
    case SurfaceCameraFrustum.Perspective(fov, aspect, near, far) => Left(SurfaceCameraFrustumError.InvalidPerspective(fov, aspect, near, far))
    case SurfaceCameraFrustum.Orthographic(left, right, bottom, top, near, far) if Vector(left, right, bottom, top, near, far).forall(_.isFinite) && right > left && top > bottom && near > 0 && far > near =>
      Right(Array(
        (2.0 / (right - left)).toFloat, 0f, 0f, (-(right + left) / (right - left)).toFloat,
        0f, (2.0 / (top - bottom)).toFloat, 0f, (-(top + bottom) / (top - bottom)).toFloat,
        0f, 0f, (2.0 / (near - far)).toFloat, ((far + near) / (near - far)).toFloat,
        0f, 0f, 0f, 1f
      ))
    case SurfaceCameraFrustum.Orthographic(left, right, bottom, top, near, far) =>
      Left(SurfaceCameraFrustumError.InvalidOrthographic(left, right, bottom, top, near, far))

final case class SurfaceDrawPass(
  slot: Int,
  mesh: SurfaceResourceKey,
  layer: SurfaceResourceKey,
  blendMode: DisplayBlendMode
)

final case class SurfaceReadout(
  surface: SurfaceId,
  vertex: Int,
  worldX: Double,
  worldY: Double,
  worldZ: Double,
  layerValues: Vector[(SurfaceLayerId, String)],
  face: Option[Int] = None
)

final case class SurfaceProfile(
  meshesPacked: Int,
  verticesPacked: Int,
  facesPacked: Int,
  layersColored: Int,
  colorValuesWritten: Int,
  primitiveBytes: Long
)

final case class SurfaceRenderReceipt(
  meshKeys: Vector[SurfaceResourceKey],
  layerKeys: Vector[SurfaceResourceKey],
  cameraKey: String,
  drawPassCount: Int,
  timepoint: Int
)

final case class SurfaceRenderPlan(
  slots: Vector[SurfaceViewSlot],
  meshes: Vector[SurfaceMeshPacket],
  layers: Vector[SurfaceLayerPacket],
  camera: SurfaceCameraPacket,
  lighting: SurfaceLighting,
  clipping: SurfaceClipping,
  drawPasses: Vector[SurfaceDrawPass],
  chrome: Scene,
  readouts: Vector[SurfaceReadout],
  profile: SurfaceProfile,
  receipt: SurfaceRenderReceipt,
  viewportFit: SurfaceViewportFit = SurfaceViewportFit.Fill,
  fragmentSurfaces: Set[SurfaceId] = Set.empty,
  colorPipeline: SurfaceColorPipeline = SurfaceColorPipeline.LegacyDisplayBytes,
  surfaceOpacities: Map[SurfaceId, DisplayOpacity] = Map.empty,
  surfaceBase: Rgba32 = SurfaceDisplayDefaults.LegacySurfaceBase,
  surfaceCameras: Map[SurfaceId, SurfaceCameraPacket] = Map.empty
):
  /** The same effective camera must be used by draw, fit and picking paths. */
  def cameraFor(surface: SurfaceId): SurfaceCameraPacket = surfaceCameras.getOrElse(surface, camera)

  /** CPU projection also supports general homogeneous matrices. Native camera
    * backends additionally require the rigid camera contract below. */
  private[view] def validCameraBuffers: Boolean =
    val packets = camera +: surfaceCameras.values.toVector
    surfaceCameras.keys.forall(id => slots.exists(_.surface == id)) && packets.forall(packet =>
      packet.viewMatrix.length == 16 && packet.projectionMatrix.length == 16 &&
      (0 until 16).forall(i => packet.viewMatrix(i).isFinite && packet.projectionMatrix(i).isFinite) &&
      packet.directionX.isFinite && packet.directionY.isFinite && packet.directionZ.isFinite) &&
      surfaceCameras.values.forall(packet =>
        (0 until 16).forall(index => packet.projectionMatrix(index) == camera.projectionMatrix(index)))

  private[view] def validCameras: Boolean =
    validCameraBuffers && (camera +: surfaceCameras.values.toVector).forall(SurfaceCompositionPlan.validCamera)
