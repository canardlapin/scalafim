package scalafim.surface.view

import image4s.geometry.{D3, Frame, GeometryError, Point, Vec}
import scalafim.surface.{FramedSurface, SurfaceFrameError, SurfacePlacement}

/** Failures of placing a surface camera, or a displayed surface, in a typed frame. */
enum SurfaceCameraError derives CanEqual:
  case UnknownSurface(surface: SurfaceId)
  case Surface(error: SurfaceFrameError)
  case Geometry(error: GeometryError)
  case View(error: SurfaceViewError)

  /** The viewer does not display the framed surface this camera is expressed against. */
  case DisplayMismatch(surface: SurfaceId, reason: String)

  /** A typed pose needs a single-surface layout showing its display surface. */
  case LayoutMismatch(surface: SurfaceId, layout: SurfaceLayout)

  /** The eye and the target coincide, so there is no viewing direction. */
  case DegeneratePose(distance: Double)

  def message: String =
    this match
      case UnknownSurface(surface)          => s"unknown surface ${surface.value}"
      case Surface(error)                   => error.message
      case Geometry(error)                  => s"surface camera geometry failed: ${error.message}"
      case View(error)                      => error.message
      case DisplayMismatch(surface, reason) => s"surface ${surface.value} is not displayed as declared: $reason"
      case LayoutMismatch(surface, layout)  => s"a typed camera pose needs the single-surface layout of ${surface.value}; the layout is $layout"
      case DegeneratePose(distance)         => s"camera eye and target must be distinct finite points; distance is $distance"

/** Evidence that the viewer surface `surfaceId` displays `surface`: the coordinates the compiler renders for its
  * canonical geometry (stored coordinates through `surfaceToWorld`) are the placed coordinates of `surface`, in the frame
  * `F`. Camera poses and linked cursors for that surface are typed in `F`.
  *
  * Camera framing follows the canonical geometry, so while another presentation (for example an inflated surface) is
  * shown, poses remain expressed in the canonical geometry's frame, exactly as the viewpoint camera's framing does.
  */
final class SurfaceDisplayFrame[F <: Frame[D3]] private (
    val surfaceId: SurfaceId,
    val surface: FramedSurface[F],
    private[view] val asset: SurfaceAsset
):
  def frame: F = surface.frame

  override def toString: String = s"SurfaceDisplayFrame(${surfaceId.value}, $frame)"

object SurfaceDisplayFrame:
  /** Declare that surface `id`'s display coordinates are points in `frame`. */
  def declare(model: SurfaceViewerModel, id: SurfaceId, frame: Frame[D3]): Either[SurfaceCameraError, SurfaceDisplayFrame[frame.type]] =
    for
      asset <- model.surface(id).toRight(SurfaceCameraError.UnknownSurface(id))
      framed <- FramedSurface.in(frame)(asset.geometry, SurfacePlacement.SurfaceToWorld).left.map(SurfaceCameraError.Surface.apply)
    yield new SurfaceDisplayFrame(id, framed, asset)

  /** Bind a framed surface to the viewer surface that displays it. Topology must agree, and every displayed vertex must
    * lie within `toleranceMm` of the framed surface's placed vertex.
    */
  def bind[F <: Frame[D3]](
      model: SurfaceViewerModel,
      id: SurfaceId,
      surface: FramedSurface[F],
      toleranceMm: Double = 1e-6
  ): Either[SurfaceCameraError, SurfaceDisplayFrame[F]] =
    for
      shown <- declare(model, id, surface.frame)
      _ <-
        if !(toleranceMm.isFinite && toleranceMm >= 0.0) then
          Left(SurfaceCameraError.DisplayMismatch(id, s"tolerance must be finite and non-negative; got $toleranceMm"))
        else if !shown.surface.hasSameTopology(surface) then
          Left(SurfaceCameraError.DisplayMismatch(id, "the framed surface has another topology"))
        else
          val displayed = shown.surface.coordinates
          val placed = surface.coordinates
          var worst = 0.0
          var i = 0
          while i < placed.length do
            worst = math.max(worst, math.abs(displayed(i) - placed(i)))
            i += 1
          if worst <= toleranceMm then Right(())
          else Left(SurfaceCameraError.DisplayMismatch(id, s"a displayed vertex lies $worst mm from the framed surface"))
    yield new SurfaceDisplayFrame(id, surface, shown.asset)

/** A surface camera as typed points of the frame `F`: where it looks (`target`) and where it stands (`eye`).
  *
  * `direction` is the unit vector from the target to the eye. `SurfaceCompiler.compile(model, state, display, pose)`
  * renders a pose against a [[SurfaceDisplayFrame]] in the same frame, and `SurfaceCompiler.cameraPose` gives the pose
  * the renderer-neutral viewpoint camera ([[SurfaceCamera]]) takes.
  */
final class SurfaceCameraPose[F <: Frame[D3]] private (val target: Point[F, D3], val eye: Point[F, D3], val direction: Vec[F, D3]):
  def frame: F = target.frame

  /** Distance from the eye to the target, in the frame's units (millimetres for world frames). */
  def distance: Double =
    val (t, e) = (target.coordinates, eye.coordinates)
    math.sqrt((e(0) - t(0)) * (e(0) - t(0)) + (e(1) - t(1)) * (e(1) - t(1)) + (e(2) - t(2)) * (e(2) - t(2)))

  /** The same view translated so that it looks at `point`: the eye keeps its offset from the target. */
  def focusedOn(point: Point[F, D3]): Either[SurfaceCameraError, SurfaceCameraPose[F]] =
    val (t, e) = (target.coordinates, eye.coordinates)
    for
      owned <- SurfaceCameraPose.own(point, frame)
      p = owned.coordinates
      moved <- SurfaceCameraPose.pointIn(frame, Vector(p(0) + e(0) - t(0), p(1) + e(1) - t(1), p(2) + e(2) - t(2)))
      pose <- SurfaceCameraPose.make(owned, moved)
    yield pose

  override def toString: String = s"SurfaceCameraPose(target=${target.coordinates}, eye=${eye.coordinates}, frame=$frame)"

object SurfaceCameraPose:
  /** A pose looking from `eye` at `target`; both must be points of one frame (the eye is rebound to the target's frame
    * owner when it belongs to another owner of the same persistent key), and they must be distinct.
    */
  def make[F <: Frame[D3]](target: Point[F, D3], eye: Point[F, D3]): Either[SurfaceCameraError, SurfaceCameraPose[F]] =
    for
      ownedEye <- own(eye, target.frame)
      t = target.coordinates
      e = ownedEye.coordinates
      d = Vector(e(0) - t(0), e(1) - t(1), e(2) - t(2))
      norm = math.sqrt(d(0) * d(0) + d(1) * d(1) + d(2) * d(2))
      _ <- if norm.isFinite && norm > 0.0 then Right(()) else Left(SurfaceCameraError.DegeneratePose(norm))
      direction <- vecIn(target.frame, d.map(_ / norm))
    yield new SurfaceCameraPose(target, ownedEye, direction)

  private[view] def pointIn[F <: Frame[D3]](frame: F, coordinates: Vector[Double]): Either[SurfaceCameraError, Point[F, D3]] =
    (for
      self <- Frame.alignOwners[D3, frame.type, F](frame, frame)
      raw <- Point.fromVector[D3](frame, coordinates)
      owned <- self.pointToRight(raw)
    yield owned).left.map(SurfaceCameraError.Geometry.apply)

  private def vecIn[F <: Frame[D3]](frame: F, coordinates: Vector[Double]): Either[SurfaceCameraError, Vec[F, D3]] =
    (for
      self <- Frame.alignOwners[D3, frame.type, F](frame, frame)
      raw <- Vec.fromVector[D3](frame, coordinates)
      owned <- self.vectorToRight(raw)
    yield owned).left.map(SurfaceCameraError.Geometry.apply)

  private[view] def own[F <: Frame[D3]](point: Point[F, D3], owner: F): Either[SurfaceCameraError, Point[F, D3]] =
    if point.belongsTo(owner) then Right(point)
    else
      Frame
        .alignOwners[D3, F, F](point.frame, owner)
        .flatMap(_.pointToRight(point))
        .left
        .map(SurfaceCameraError.Geometry.apply)
