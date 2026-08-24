package scalafim.fmri.motion

import image4s.geometry.Affine
import image4s.geometry.D3

final case class MotionTrace private (aligned: FrameAligned[RigidPose]):
  require(aligned.nonEmpty, "MotionTrace must be non-empty")

  def frameCount: FrameCount = aligned.frameCount
  def poses: Vector[RigidPose] = aligned.toVector
  def length: Int = aligned.length
  def isEmpty: Boolean = aligned.isEmpty
  def nonEmpty: Boolean = aligned.nonEmpty

  def apply(index: FrameIndex): Either[MotionError, RigidPose] =
    aligned(index)

  def unsafeFrame(index: Int): RigidPose =
    aligned.unsafeFrame(index)

  def affines: Vector[Affine[D3]] =
    poses.map(_.toAffine)

object MotionTrace:
  def make(poses: Vector[RigidPose]): Either[MotionError, MotionTrace] =
    if poses.nonEmpty then Right(unsafe(poses))
    else Left(MotionError.EmptyTrace)

  def unsafe(poses: Vector[RigidPose]): MotionTrace =
    new MotionTrace(FrameAligned.unsafe(poses))

  def unsafe(aligned: FrameAligned[RigidPose]): MotionTrace =
    new MotionTrace(aligned)

  def identity(nFrames: Int): Either[MotionError, MotionTrace] =
    if nFrames >= 1 then Right(unsafe(Vector.fill(nFrames)(RigidPose.identity)))
    else Left(MotionError.InvalidInt("nFrames", nFrames, "must be positive"))

  def identity(frameCount: FrameCount): MotionTrace =
    unsafe(Vector.fill(frameCount.value)(RigidPose.identity))
