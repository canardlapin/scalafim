package scalafim.surface.view

import scalafim.graphics.*
import scalafim.surface.*

enum SurfaceViewError:
  case BlankSurfaceId
  case BlankLayerId
  case EmptySurfaces
  case DuplicateSurfaceId(id: SurfaceId)
  case DuplicateLayerId(id: SurfaceLayerId)
  case UnknownSurface(id: SurfaceId)
  case UnknownLayer(id: SurfaceLayerId)
  case IncompatibleLayerDomain(layer: SurfaceLayerId, surface: SurfaceId)
  case InvalidFrameCount(value: Int)
  case InvalidDataLength(expected: Int, actual: Int)
  case IncompatibleFrameCounts(counts: Vector[Int])
  case TimepointOutOfBounds(index: Int, count: Int)
  case InvalidTimeAxis(reason: String)
  case BlankTemporalFieldId
  case TimeOutsideAxis(value: Double, minimum: Double, maximum: Double)
  case FrameReadFailed(field: String, frame: Int, reason: String)
  case InvalidFrameCacheCapacity(value: Int)
  case InvalidPlaybackRate(value: Double)
  case InvalidPlaybackDelta(value: Double)
  case InvalidMorphFraction(value: Double)
  case InvalidLensRadius(value: Double)
  case InvalidLensBand(inner: Double, outer: Double)
  case InvalidLensRelaxationSteps(value: Int)
  case UnsafeLensDeformation(invertedTriangles: Int)
  case IncompatibleMorph(reason: String)
  case InvalidAnnotation(reason: String)
  case InvalidSelectionHistoryCapacity(value: Int)
  case InvalidLinkRadius(value: Double)
  case InvalidVertexIndex(index: Int, count: Int)
  case LinkDistanceExceeded(distance: Double, maximum: Double)
  case InvalidProjection(reason: String)
  case InvalidNetwork(reason: String)
  case InvalidZoom(value: Double)
  case InvalidFieldOfView(value: Double)
  case InvalidOrthographicScale(value: Double)
  case InvalidClipRange(near: Double, far: Double)
  case InvalidClipPlane(normalX: Double, normalY: Double, normalZ: Double, offset: Double)
  case EmptyClipPlanes
  case InvalidPan(x: Double, y: Double)
  case InvalidOrbit(yawDegrees: Double, pitchDegrees: Double)
  case InvalidLightFraction(value: Double)
  case InvalidLightDirection(x: Double, y: Double, z: Double)
  case InvalidBilateralLayout(reason: String)
  case InvalidPublication(reason: String)
  case InvalidLayerPosition(index: Int, count: Int)
  case InvalidSelection(surface: SurfaceId, vertex: Int)
  case LayerCapabilityUnsupported(layer: SurfaceLayerId, capability: String)
  case DisplayFailure(cause: DisplayError)

  def message: String =
    this match
      case BlankSurfaceId => "surface id must not be blank"
      case BlankLayerId => "surface layer id must not be blank"
      case EmptySurfaces => "a surface viewer requires at least one surface"
      case DuplicateSurfaceId(id) => s"surface id '${id.value}' is duplicated"
      case DuplicateLayerId(id) => s"surface layer id '${id.value}' is duplicated"
      case UnknownSurface(id) => s"surface '${id.value}' does not exist"
      case UnknownLayer(id) => s"surface layer '${id.value}' does not exist"
      case IncompatibleLayerDomain(layer, surface) =>
        s"layer '${layer.value}' does not have the exact mesh domain of surface '${surface.value}'"
      case InvalidFrameCount(value) => s"surface frame count must be positive; got $value"
      case InvalidDataLength(expected, actual) => s"surface data length must be $expected; got $actual"
      case IncompatibleFrameCounts(counts) =>
        s"dynamic surface layers must have one shared frame count; got ${counts.mkString(", ")}"
      case TimepointOutOfBounds(index, count) => s"timepoint $index is outside 0..${count - 1}"
      case InvalidTimeAxis(reason) => s"invalid surface time axis: $reason"
      case BlankTemporalFieldId => "surface temporal field id must be non-empty"
      case TimeOutsideAxis(value, minimum, maximum) =>
        s"surface time $value is outside [$minimum, $maximum]"
      case FrameReadFailed(field, frame, reason) =>
        s"could not read frame $frame from temporal field '$field': $reason"
      case InvalidFrameCacheCapacity(value) => s"surface frame cache capacity must be non-negative; got $value"
      case InvalidPlaybackRate(value) => s"surface playback rate must be finite and non-zero; got $value"
      case InvalidPlaybackDelta(value) => s"surface playback tick must be finite and non-negative; got $value"
      case InvalidMorphFraction(value) => s"surface morph fraction must be finite and in [0, 1]; got $value"
      case InvalidLensRadius(value) => s"surface lens radius must be finite and non-negative; got $value"
      case InvalidLensBand(inner, outer) =>
        s"surface lens radii must satisfy 0 <= inner < outer; got [$inner, $outer]"
      case InvalidLensRelaxationSteps(value) =>
        s"surface lens relaxation steps must be in [1, 1000]; got $value"
      case UnsafeLensDeformation(invertedTriangles) =>
        s"natural surface lens would invert $invertedTriangles triangles during its animation"
      case IncompatibleMorph(reason) => s"incompatible surface morph: $reason"
      case InvalidAnnotation(reason) => s"invalid surface annotation: $reason"
      case InvalidSelectionHistoryCapacity(value) =>
        s"surface selection history capacity must be positive; got $value"
      case InvalidLinkRadius(value) => s"surface link radius must be finite and non-negative; got $value"
      case InvalidVertexIndex(index, count) => s"surface vertex $index is outside 0..${count - 1}"
      case LinkDistanceExceeded(distance, maximum) =>
        s"nearest surface vertex is $distance world units away, exceeding $maximum"
      case InvalidProjection(reason) => s"invalid surface projection: $reason"
      case InvalidNetwork(reason) => s"invalid surface network: $reason"
      case InvalidZoom(value) => s"camera zoom must be finite and positive; got $value"
      case InvalidFieldOfView(value) => s"perspective field of view must be finite and in (0, 180); got $value"
      case InvalidOrthographicScale(value) => s"orthographic scale must be finite and positive; got $value"
      case InvalidClipRange(near, far) => s"clip range must satisfy finite 0 < near < far; got [$near, $far]"
      case InvalidClipPlane(x, y, z, offset) =>
        s"world clip plane requires a finite non-zero normal and finite offset; got (($x, $y, $z), $offset)"
      case EmptyClipPlanes => "world clipping requires at least one plane"
      case InvalidPan(x, y) => s"camera pan must be finite; got ($x, $y)"
      case InvalidOrbit(yaw, pitch) =>
        s"camera orbit must have finite yaw and pitch in [-89, 89]; got ($yaw, $pitch)"
      case InvalidLightFraction(value) => s"light fractions must be finite and in [0, 1]; got $value"
      case InvalidLightDirection(x, y, z) => s"light direction must be finite and non-zero; got ($x, $y, $z)"
      case InvalidBilateralLayout(reason) => s"invalid bilateral surface layout: $reason"
      case InvalidPublication(reason) => s"invalid surface publication: $reason"
      case InvalidLayerPosition(index, count) => s"layer position $index is outside 0..${count - 1}"
      case InvalidSelection(surface, vertex) => s"vertex $vertex is not valid for surface '${surface.value}'"
      case LayerCapabilityUnsupported(layer, capability) =>
        s"layer '${layer.value}' does not support $capability"
      case DisplayFailure(cause) => cause.message

opaque type SurfaceId = String

object SurfaceId:
  def make(value: String): Either[SurfaceViewError, SurfaceId] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(SurfaceViewError.BlankSurfaceId) else Right(trimmed)

  def unsafe(value: String): SurfaceId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SurfaceId)
    def value: String = id

opaque type SurfaceLayerId = String

object SurfaceLayerId:
  def make(value: String): Either[SurfaceViewError, SurfaceLayerId] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(SurfaceViewError.BlankLayerId) else Right(trimmed)

  def unsafe(value: String): SurfaceLayerId =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: SurfaceLayerId)
    def value: String = id

opaque type CameraZoom = Double

object CameraZoom:
  def make(value: Double): Either[SurfaceViewError, CameraZoom] =
    if value.isFinite && value > 0.0 then Right(value) else Left(SurfaceViewError.InvalidZoom(value))

  def unsafe(value: Double): CameraZoom = make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  val Default: CameraZoom = 1.0

  extension (zoom: CameraZoom)
    def value: Double = zoom

opaque type FieldOfViewDegrees = Double

object FieldOfViewDegrees:
  def make(value: Double): Either[SurfaceViewError, FieldOfViewDegrees] =
    if value.isFinite && value > 0.0 && value < 180.0 then Right(value)
    else Left(SurfaceViewError.InvalidFieldOfView(value))

  def unsafe(value: Double): FieldOfViewDegrees =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  val Default: FieldOfViewDegrees = 35.0

  extension (fov: FieldOfViewDegrees)
    def value: Double = fov

opaque type OrthographicScale = Double

object OrthographicScale:
  def make(value: Double): Either[SurfaceViewError, OrthographicScale] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(SurfaceViewError.InvalidOrthographicScale(value))

  def unsafe(value: Double): OrthographicScale =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  val Default: OrthographicScale = 1.0

  extension (scale: OrthographicScale)
    def value: Double = scale

opaque type LightFraction = Double

object LightFraction:
  def make(value: Double): Either[SurfaceViewError, LightFraction] =
    if value.isFinite && value >= 0.0 && value <= 1.0 then Right(value)
    else Left(SurfaceViewError.InvalidLightFraction(value))

  def unsafe(value: Double): LightFraction =
    make(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (fraction: LightFraction)
    def value: Double = fraction

final case class SurfaceOrbit private (yawDegrees: Double, pitchDegrees: Double)

object SurfaceOrbit:
  def make(yawDegrees: Double, pitchDegrees: Double): Either[SurfaceViewError, SurfaceOrbit] =
    if yawDegrees.isFinite && pitchDegrees.isFinite && pitchDegrees >= -89.0 && pitchDegrees <= 89.0 then
      Right(new SurfaceOrbit(normalizeYaw(yawDegrees), pitchDegrees))
    else Left(SurfaceViewError.InvalidOrbit(yawDegrees, pitchDegrees))

  def unsafe(yawDegrees: Double, pitchDegrees: Double): SurfaceOrbit =
    make(yawDegrees, pitchDegrees).fold(error => throw new IllegalArgumentException(error.message), identity)

  val Zero: SurfaceOrbit = new SurfaceOrbit(0.0, 0.0)

  private def normalizeYaw(value: Double): Double =
    val wrapped = value % 360.0
    if wrapped <= -180.0 then wrapped + 360.0
    else if wrapped > 180.0 then wrapped - 360.0
    else wrapped

enum SurfaceViewpoint:
  case Lateral(hemisphere: CorticalHemisphere)
  case Medial(hemisphere: CorticalHemisphere)
  case Anterior, Posterior, Dorsal, Ventral

  def cameraDirection: (Double, Double, Double) =
    this match
      case Lateral(CorticalHemisphere.Left) => (-1.0, 0.0, 0.0)
      case Lateral(CorticalHemisphere.Right) => (1.0, 0.0, 0.0)
      case Medial(CorticalHemisphere.Left) => (1.0, 0.0, 0.0)
      case Medial(CorticalHemisphere.Right) => (-1.0, 0.0, 0.0)
      case Anterior => (0.0, 1.0, 0.0)
      case Posterior => (0.0, -1.0, 0.0)
      case Dorsal => (0.0, 0.0, 1.0)
      case Ventral => (0.0, 0.0, -1.0)

enum CameraProjection:
  case Perspective(fieldOfView: FieldOfViewDegrees)
  case Orthographic(scale: OrthographicScale)

enum SurfaceLighting:
  case Unlit
  case Directional(
    ambient: LightFraction,
    diffuse: LightFraction,
    directionX: Double,
    directionY: Double,
    directionZ: Double
  )

object SurfaceLighting:
  def directional(
    ambient: Double,
    diffuse: Double,
    directionX: Double,
    directionY: Double,
    directionZ: Double
  ): Either[SurfaceViewError, SurfaceLighting] =
    for
      checkedAmbient <- LightFraction.make(ambient)
      checkedDiffuse <- LightFraction.make(diffuse)
      norm = math.sqrt(directionX * directionX + directionY * directionY + directionZ * directionZ)
      result <-
        if directionX.isFinite && directionY.isFinite && directionZ.isFinite && norm > 0.0 then
          Right(Directional(checkedAmbient, checkedDiffuse, directionX / norm, directionY / norm, directionZ / norm))
        else Left(SurfaceViewError.InvalidLightDirection(directionX, directionY, directionZ))
    yield result

  val Default: SurfaceLighting =
    Directional(LightFraction.unsafe(0.35), LightFraction.unsafe(0.65), -0.4, -0.5, 0.7681145747868608)

enum ClipKeepSide:
  case Positive, Negative

final case class WorldClipPlane private (
  normalX: Double,
  normalY: Double,
  normalZ: Double,
  offset: Double,
  keep: ClipKeepSide
):
  /** Non-negative values are retained, regardless of the requested side. */
  def orientedDistance(x: Double, y: Double, z: Double): Double =
    val raw = normalX * x + normalY * y + normalZ * z + offset
    if keep == ClipKeepSide.Positive then raw else -raw

object WorldClipPlane:
  def make(
    normalX: Double,
    normalY: Double,
    normalZ: Double,
    offset: Double,
    keep: ClipKeepSide = ClipKeepSide.Positive
  ): Either[SurfaceViewError, WorldClipPlane] =
    val norm = math.sqrt(normalX * normalX + normalY * normalY + normalZ * normalZ)
    if normalX.isFinite && normalY.isFinite && normalZ.isFinite && offset.isFinite && norm > 0.0 then
      Right(new WorldClipPlane(normalX / norm, normalY / norm, normalZ / norm, offset / norm, keep))
    else Left(SurfaceViewError.InvalidClipPlane(normalX, normalY, normalZ, offset))

  def unsafe(
    normalX: Double,
    normalY: Double,
    normalZ: Double,
    offset: Double,
    keep: ClipKeepSide = ClipKeepSide.Positive
  ): WorldClipPlane =
    make(normalX, normalY, normalZ, offset, keep)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

enum SurfaceClipping:
  case Disabled
  case NearFar(near: Double, far: Double)
  case WorldPlanes(planes: Vector[WorldClipPlane])

  this match
    case WorldPlanes(planes) => require(planes.nonEmpty, "world clipping requires at least one plane")
    case _ => ()

object SurfaceClipping:
  def nearFar(near: Double, far: Double): Either[SurfaceViewError, SurfaceClipping] =
    if near.isFinite && far.isFinite && near > 0.0 && near < far then Right(NearFar(near, far))
    else Left(SurfaceViewError.InvalidClipRange(near, far))

  def worldPlanes(planes: Vector[WorldClipPlane]): Either[SurfaceViewError, SurfaceClipping] =
    if planes.nonEmpty then Right(WorldPlanes(planes)) else Left(SurfaceViewError.EmptyClipPlanes)

final case class SurfaceCamera private[view] (
  viewpoint: SurfaceViewpoint,
  projection: CameraProjection,
  zoom: CameraZoom,
  panX: Double,
  panY: Double,
  orbit: SurfaceOrbit
)

object SurfaceCamera:
  def make(
    viewpoint: SurfaceViewpoint,
    projection: CameraProjection = CameraProjection.Perspective(FieldOfViewDegrees.Default),
    zoom: CameraZoom = CameraZoom.Default,
    panX: Double = 0.0,
    panY: Double = 0.0,
    orbit: SurfaceOrbit = SurfaceOrbit.Zero
  ): Either[SurfaceViewError, SurfaceCamera] =
    if panX.isFinite && panY.isFinite then Right(new SurfaceCamera(viewpoint, projection, zoom, panX, panY, orbit))
    else Left(SurfaceViewError.InvalidPan(panX, panY))

  def unsafe(
    viewpoint: SurfaceViewpoint,
    projection: CameraProjection = CameraProjection.Perspective(FieldOfViewDegrees.Default),
    zoom: CameraZoom = CameraZoom.Default,
    panX: Double = 0.0,
    panY: Double = 0.0,
    orbit: SurfaceOrbit = SurfaceOrbit.Zero
  ): SurfaceCamera =
    make(viewpoint, projection, zoom, panX, panY, orbit).fold(error => throw new IllegalArgumentException(error.message), identity)

enum BilateralOrder:
  case LeftThenRight, RightThenLeft

enum SurfaceLayout:
  case Single(surface: SurfaceId)
  case Bilateral(left: SurfaceId, right: SurfaceId, order: BilateralOrder = BilateralOrder.LeftThenRight)

final case class SurfaceSelection(surface: SurfaceId, vertex: VertexId)
