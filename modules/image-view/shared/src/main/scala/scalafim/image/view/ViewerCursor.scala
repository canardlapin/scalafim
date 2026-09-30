package scalafim.image.view

import image4s.geometry.{D3, Frame, GeometryError, Point}

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
  * [[ViewerState.cursor]] retains the frame that owns its coordinates. These entry points check a caller's static
  * frame against the model's reference frame (one runtime owner or one persistent key) before a point crosses, so a
  * point from another world, for example an MNI point shown over a subject's T1w reference, is rejected rather than
  * read as reference coordinates. A point in another world reaches the cursor through a typed map first, such as a
  * `WorldLink`.
  */
object ViewerCursor:
  /** The cursor as a point of `frame`, which must be the model's reference frame. */
  def point[F <: Frame[D3]](model: ViewerModel, state: ViewerState, frame: F): Either[ViewerCursorError, Point[F, D3]] =
    pointFor(model, state.cursor, frame)

  private def pointFor[A <: Frame[D3], F <: Frame[D3]](model: ViewerModel, cursor: Point[A, D3], frame: F): Either[ViewerCursorError, Point[F, D3]] =
    for
      _ <- Frame
        .alignOwners[D3, A, Frame[D3]](cursor.frame, model.referenceSpace.frame)
        .left
        .map(ViewerCursorError.FrameMismatch.apply)
      aligned <- Frame
        .alignOwners[D3, A, F](cursor.frame, frame)
        .left
        .map(ViewerCursorError.FrameMismatch.apply)
      owned <- aligned.pointToRight(cursor).left.map(ViewerCursorError.Geometry.apply)
    yield owned

  /** The action that moves the cursor to `point`, whose frame must be the model's reference frame. */
  def set[F <: Frame[D3]](model: ViewerModel, point: Point[F, D3]): Either[ViewerCursorError, ViewerAction] =
    Frame
      .alignOwners[D3, F, Frame[D3]](point.frame, model.referenceSpace.frame)
      .left
      .map(ViewerCursorError.FrameMismatch.apply)
      .flatMap(_.pointToRight(point).left.map(ViewerCursorError.Geometry.apply))
      .map(ViewerAction.SetCursor.apply)
