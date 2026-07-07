package scalafim.fmri.motion

import scalafim.image.DMat

enum MotionError:
  case InvalidScalar(name: String, value: Double, reason: String)
  case InvalidInt(name: String, value: Int, reason: String)
  case EmptyTrace
  case FrameIndexOutOfBounds(index: Int, length: Int)
  case InvalidFramePair(previous: Int, current: Int, reason: String)
  case TraceLengthMismatch(traceLength: Int, nVolumes: Int)
  case ShapeMismatch(name: String, expected: Vector[Int], actual: Vector[Int])
  case IncompleteFitCostTrace(missing: String)
  case NonFiniteData(name: String, index: Int)
  case InvalidMatrix(reason: String)
  case SingularTransform(reason: String)
  case UnsupportedInterpolation(interpolation: Interpolation)
  case NotImplemented(feature: String)

  def message: String =
    this match
      case InvalidScalar(name, value, reason) =>
        s"$name has invalid value $value: $reason"
      case InvalidInt(name, value, reason) =>
        s"$name has invalid value $value: $reason"
      case EmptyTrace =>
        "motion trace must contain at least one pose"
      case FrameIndexOutOfBounds(index, length) =>
        s"frame index $index is out of bounds for trace length $length"
      case InvalidFramePair(previous, current, reason) =>
        s"invalid frame pair ($previous, $current): $reason"
      case TraceLengthMismatch(traceLength, nVolumes) =>
        s"motion trace length $traceLength does not match run volume count $nVolumes"
      case ShapeMismatch(name, expected, actual) =>
        s"$name shape mismatch: expected $expected, actual $actual"
      case IncompleteFitCostTrace(missing) =>
        s"incomplete fit-cost trace: missing $missing"
      case NonFiniteData(name, index) =>
        s"$name contains non-finite value at linear index $index"
      case InvalidMatrix(reason) =>
        s"invalid rigid transform matrix: $reason"
      case SingularTransform(reason) =>
        s"singular rigid transform: $reason"
      case UnsupportedInterpolation(interpolation) =>
        s"unsupported interpolation for motion application: $interpolation"
      case NotImplemented(feature) =>
        s"$feature is not implemented yet"

object MotionError:
  private[motion] def validateFiniteMatrix(matrix: DMat): Either[MotionError, Unit] =
    if matrix.rows != 4 || matrix.cols != 4 then Left(MotionError.InvalidMatrix("matrix must be 4x4"))
    else
      var i = 0
      while i < matrix.data.length do
        if !matrix.data(i).isFinite then return Left(MotionError.InvalidMatrix("matrix contains non-finite values"))
        i += 1
      Right(())
