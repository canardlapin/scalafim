package scalafim.surface.view

import image4s.geometry.{D3, Frame}

/** Deltas are applied independently in every linked camera's local basis.
  * Linking a medial and lateral view therefore retains their different poses.
  * Pan uses world units; zoom scales projection magnification, never geometry.
  */
enum SurfaceCompositionNavigation:
  case OrbitBy(yawDegrees: Double, pitchDegrees: Double)
  case PanBy(right: Double, up: Double)
  case ZoomBy(factor: Double)

final class SurfaceCompositionState[F <: Frame[D3]] private (
  val views: Vector[SurfaceCompositionInputView[F]],
  val cameraLinks: Vector[Set[SurfaceViewId]]
):
  def navigate(view: SurfaceViewId, change: SurfaceCompositionNavigation)
      : Either[SurfaceCompositionError, SurfaceCompositionState[F]] =
    if !views.exists(_.id == view) then Left(SurfaceCompositionError.InvalidPresentation(s"unknown view ${view.value}"))
    else
      val targets = cameraLinks.find(_(view)).getOrElse(Set(view))
      val changed = Vector.newBuilder[SurfaceCompositionInputView[F]]
      var index = 0
      while index < views.length do
        val next = views(index)
        if targets(next.id) then
          SurfaceCompositionState.move(next, change) match
            case Left(error) => return Left(error)
            case Right(value) => changed += value
        else changed += next
        index += 1
      Right(new SurfaceCompositionState(changed.result(), cameraLinks))

  /** Resizing one panel changes only its projection aspect, even when linked. */
  def resize(view: SurfaceViewId, aspect: CameraAspectRatio)
      : Either[SurfaceCompositionError, SurfaceCompositionState[F]] =
    if !views.exists(_.id == view) then Left(SurfaceCompositionError.InvalidPresentation(s"unknown view ${view.value}"))
    else
      val resized = views.map: item =>
        if item.id == view then item.copy(camera = item.camera.copy(aspectRatio = aspect)) else item
      Right(new SurfaceCompositionState(resized, cameraLinks))

object SurfaceCompositionState:
  def make[F <: Frame[D3]](views: Vector[SurfaceCompositionInputView[F]], cameraLinks: Vector[Set[SurfaceViewId]] = Vector.empty)
      : Either[SurfaceCompositionError, SurfaceCompositionState[F]] =
    if views.isEmpty then Left(SurfaceCompositionError.EmptyComposition)
    else if views.map(_.id).distinct.length != views.length then
      Left(SurfaceCompositionError.InvalidPresentation("view ids must be unique"))
    else if cameraLinks.exists(_.size < 2) then
      Left(SurfaceCompositionError.InvalidPresentation("a camera link must contain at least two views"))
    else if cameraLinks.flatten.exists(id => !views.exists(_.id == id)) then
      Left(SurfaceCompositionError.InvalidPresentation("camera link refers to an unknown view"))
    else if cameraLinks.flatten.toVector.distinct.length != cameraLinks.map(_.size).sum then
      Left(SurfaceCompositionError.InvalidPresentation("a view may belong to only one camera link"))
    else Right(new SurfaceCompositionState(views, cameraLinks))

  private def move[F <: Frame[D3]](view: SurfaceCompositionInputView[F], change: SurfaceCompositionNavigation)
      : Either[SurfaceCompositionError, SurfaceCompositionInputView[F]] =
    def invalid(reason: String) = Left(SurfaceCompositionError.InvalidPresentation(reason))
    val target = view.pose.target.coordinates
    val eye = view.pose.eye.coordinates
    val direction = Vector.tabulate(3)(axis => (eye(axis) - target(axis)) / view.pose.distance)
    val upSeed = if math.abs(direction(2)) <= 0.9 then Vector(0.0, 0.0, 1.0) else Vector(0.0, 1.0, 0.0)
    val right = normalized(cross(upSeed, direction))
    val up = cross(direction, right)
    def withPose(t: Vector[Double], e: Vector[Double]) =
      (for
        targetPoint <- SurfaceCameraPose.pointIn(view.pose.frame, t)
        eyePoint <- SurfaceCameraPose.pointIn(view.pose.frame, e)
        pose <- SurfaceCameraPose.make(targetPoint, eyePoint)
      yield view.copy(pose = pose)).left.map(SurfaceCompositionError.Compilation.apply)
    change match
      case SurfaceCompositionNavigation.ZoomBy(factor) =>
        if !factor.isFinite || factor <= 0 then invalid("zoom factor must be finite and positive")
        else CameraZoom.make(view.camera.zoom.value * factor)
          .left.map(error => SurfaceCompositionError.Compilation(SurfaceCameraError.View(error)))
          .flatMap: zoom =>
            view.camera.projection match
              case _: CameraProjection.Orthographic => Right(view.copy(camera = view.camera.copy(zoom = zoom)))
              case _: CameraProjection.Perspective =>
                withPose(target, Vector.tabulate(3)(axis => target(axis) + (eye(axis) - target(axis)) / factor))
                  .map(moved => moved.copy(camera = moved.camera.copy(zoom = zoom)))
      case SurfaceCompositionNavigation.PanBy(dx, dy) =>
        if !dx.isFinite || !dy.isFinite then invalid("pan delta must be finite")
        else
          val delta = Vector.tabulate(3)(axis => right(axis) * dx + up(axis) * dy)
          withPose(Vector.tabulate(3)(axis => target(axis) + delta(axis)), Vector.tabulate(3)(axis => eye(axis) + delta(axis)))
      case SurfaceCompositionNavigation.OrbitBy(yaw, pitch) =>
        if !yaw.isFinite || !pitch.isFinite then invalid("orbit delta must be finite")
        else
          val offset = Vector.tabulate(3)(axis => eye(axis) - target(axis))
          val yawed = rotate(offset, up, math.toRadians(yaw % 360.0))
          val yawedRight = rotate(right, up, math.toRadians(yaw % 360.0))
          val moved = rotate(yawed, yawedRight, math.toRadians(pitch % 360.0))
          withPose(target, Vector.tabulate(3)(axis => target(axis) + moved(axis)))

  private def cross(a: Vector[Double], b: Vector[Double]): Vector[Double] =
    Vector(a(1) * b(2) - a(2) * b(1), a(2) * b(0) - a(0) * b(2), a(0) * b(1) - a(1) * b(0))
  private def normalized(value: Vector[Double]): Vector[Double] =
    val norm = math.sqrt(value.map(x => x * x).sum)
    value.map(_ / norm)
  private def rotate(value: Vector[Double], axis: Vector[Double], radians: Double): Vector[Double] =
    val cosine = math.cos(radians)
    val sine = math.sin(radians)
    val product = cross(axis, value)
    val dot = axis.zip(value).map((a, b) => a * b).sum
    Vector.tabulate(3)(index => value(index) * cosine + product(index) * sine + axis(index) * dot * (1 - cosine))
