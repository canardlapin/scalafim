package scalafim.fmri.motion

final case class PoseSpline private (
    trace: MotionTrace,
    smoothIterations: Int
):
  def poseAt(frame: Int, offset: Double): Either[MotionError, RigidPose] =
    if frame < 0 || frame >= trace.length then Left(MotionError.FrameIndexOutOfBounds(frame, trace.length))
    else if !offset.isFinite then Left(MotionError.InvalidScalar("offset", offset, "must be finite"))
    else if trace.length == 1 then Right(trace.unsafeFrame(0))
    else
      val u = frame.toDouble + math.max(0.0, math.min(1.0, offset))
      val t0 = math.max(0, math.min(trace.length - 1, math.floor(u).toInt))
      val t1 = math.max(0, math.min(trace.length - 1, t0 + 1))
      val a = math.max(0.0, math.min(1.0, u - t0.toDouble))
      Right(PoseSpline.interpolate(trace.unsafeFrame(t0), trace.unsafeFrame(t1), a))

object PoseSpline:
  val defaultSmoothIterations: Int = 2

  def fromTrace(trace: MotionTrace): PoseSpline =
    new PoseSpline(trace, smoothIterations = 0)

  def smooth(
      trace: MotionTrace,
      smoothIterations: Int = defaultSmoothIterations
  ): Either[MotionError, PoseSpline] =
    if smoothIterations < 0 then Left(MotionError.InvalidInt("smoothIterations", smoothIterations, "must be non-negative"))
    else Right(new PoseSpline(smoothTrace(trace, smoothIterations), smoothIterations))

  def packetCorrectionMagnitude(
      trace: MotionTrace,
      acquisitionTiming: AcquisitionTiming,
      nSlices: Int
  ): Either[MotionError, Vector[Double]] =
    for
      offsets <- acquisitionTiming.normalizedSliceOffsets(nSlices)
      spline = fromTrace(trace)
      out <- packetCorrectionMagnitude(trace, spline, offsets)
    yield out

  private[motion] def packetCorrectionMagnitude(
      trace: MotionTrace,
      spline: PoseSpline,
      normalizedOffsets: Vector[Double]
  ): Either[MotionError, Vector[Double]] =
    val out = Vector.newBuilder[Double]
    var t = 0
    while t < trace.length do
      val base = trace.unsafeFrame(t)
      var maxCorrection = 0.0
      var k = 0
      while k < normalizedOffsets.length do
        spline.poseAt(t, normalizedOffsets(k)) match
          case Left(error) => return Left(error)
          case Right(packetPose) =>
            val correction = MotionMetrics.transformDisplacement(packetPose, reference = Some(base))
            if correction > maxCorrection then maxCorrection = correction
        k += 1
      out += maxCorrection
      t += 1
    Right(out.result())

  private[motion] def interpolate(a: RigidPose, b: RigidPose, weight: Double): RigidPose =
    val w = math.max(0.0, math.min(1.0, weight))
    val iw = 1.0 - w
    RigidPose.unsafe(
      iw * a.tx + w * b.tx,
      iw * a.ty + w * b.ty,
      iw * a.tz + w * b.tz,
      RigidPose.wrapPi(iw * a.rx + w * b.rx),
      RigidPose.wrapPi(iw * a.ry + w * b.ry),
      RigidPose.wrapPi(iw * a.rz + w * b.rz)
    )

  private def smoothTrace(trace: MotionTrace, iterations: Int): MotionTrace =
    var current = trace.poses
    var iter = 0
    while iter < iterations do
      val next = Array.ofDim[RigidPose](current.length)
      next(0) = current(0)
      var t = 1
      while t < current.length - 1 do
        next(t) = smoothPose(current(t - 1), current(t), current(t + 1))
        t += 1
      if current.length > 1 then next(current.length - 1) = current(current.length - 1)
      current = next.toVector
      iter += 1
    MotionTrace.unsafe(current)

  private def smoothPose(previous: RigidPose, current: RigidPose, next: RigidPose): RigidPose =
    RigidPose.unsafe(
      (previous.tx + 4.0 * current.tx + next.tx) / 6.0,
      (previous.ty + 4.0 * current.ty + next.ty) / 6.0,
      (previous.tz + 4.0 * current.tz + next.tz) / 6.0,
      RigidPose.wrapPi((previous.rx + 4.0 * current.rx + next.rx) / 6.0),
      RigidPose.wrapPi((previous.ry + 4.0 * current.ry + next.ry) / 6.0),
      RigidPose.wrapPi((previous.rz + 4.0 * current.rz + next.rz) / 6.0)
    )
