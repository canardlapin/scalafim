package scalafim.fmri.motion

import scalafim.image.DMat

final case class MotionTrace private (poses: Vector[RigidPose]):
  require(poses.nonEmpty, "MotionTrace must be non-empty")

  def length: Int = poses.length
  def isEmpty: Boolean = poses.isEmpty
  def nonEmpty: Boolean = poses.nonEmpty

  def apply(index: FrameIndex): Either[MotionError, RigidPose] =
    val i = index.value
    if i >= 0 && i < poses.length then Right(poses(i))
    else Left(MotionError.FrameIndexOutOfBounds(i, poses.length))

  def unsafeFrame(index: Int): RigidPose =
    poses(index)

  def matrices: Vector[DMat] =
    poses.map(_.toMatrix)

object MotionTrace:
  def make(poses: Vector[RigidPose]): Either[MotionError, MotionTrace] =
    if poses.nonEmpty then Right(unsafe(poses))
    else Left(MotionError.EmptyTrace)

  def unsafe(poses: Vector[RigidPose]): MotionTrace =
    new MotionTrace(poses)

  def identity(nFrames: Int): Either[MotionError, MotionTrace] =
    if nFrames >= 1 then Right(unsafe(Vector.fill(nFrames)(RigidPose.identity)))
    else Left(MotionError.InvalidInt("nFrames", nFrames, "must be positive"))
