package scalafim.image.view

import image4s.geometry.{D3, Frame, GeometryError, Point}
import scalafim.image.WorldPoint

/** Failures of reading or setting the viewer cursor as a typed point. */
enum ViewerCursorError derives CanEqual:
  /** The frame is not the viewer's reference frame (neither its runtime owner nor its persistent key). */
  case FrameMismatch(cause: GeometryError)
  case Geometry(cause: GeometryError)

  def message: String =
    this match
      case FrameMismatch(cause) => s"cursor frame is not the viewer's reference frame: ${cause.message}"
      case Geometry(cause)      => s"cursor point is invalid: ${cause.message}"

/** The viewer cursor as a typed point of the reference frame.
  *
  * [[ViewerState.cursor]] holds world coordinates of the model's reference grid, whose frame is known only at runtime.
  * These entry points check a caller's static frame against that reference frame (one runtime owner or one persistent
  * key) before a coordinate crosses, so a point from another world, for example an MNI point shown over a subject's
  * T1w reference, is rejected rather than read as reference coordinates. A point in another world reaches the cursor
  * through a typed map first, such as a `WorldLink`.
  */
object ViewerCursor:
  /** The cursor as a point of `frame`, which must be the model's reference frame. */
  def point[F <: Frame[D3]](model: ViewerModel, state: ViewerState, frame: F): Either[ViewerCursorError, Point[F, D3]] =
    for
      _ <- reference(model, frame)
      self <- Frame.alignOwners[D3, frame.type, F](frame, frame).left.map(ViewerCursorError.Geometry.apply)
      raw <- Point
        .fromVector[D3](frame, Vector(state.cursor.x, state.cursor.y, state.cursor.z))
        .left
        .map(ViewerCursorError.Geometry.apply)
      owned <- self.pointToRight(raw).left.map(ViewerCursorError.Geometry.apply)
    yield owned

  /** The action that moves the cursor to `point`, whose frame must be the model's reference frame. */
  def set[F <: Frame[D3]](model: ViewerModel, point: Point[F, D3]): Either[ViewerCursorError, ViewerAction] =
    reference(model, point.frame).map: _ =>
      val c = point.coordinates
      ViewerAction.SetCursor(WorldPoint(c(0), c(1), c(2)))

  private def reference(model: ViewerModel, frame: Frame[D3]): Either[ViewerCursorError, Unit] =
    Frame
      .alignOwners[D3, Frame[D3], Frame[D3]](frame, model.referenceSpace.frame)
      .left
      .map(ViewerCursorError.FrameMismatch.apply)
      .map(_ => ())
