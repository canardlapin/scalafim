package scalafim.fmri.motion

final case class ApplyControl private (
    interpolation: Interpolation,
    padMode: PadMode,
    zpad: Int,
    acquisitionTiming: AcquisitionTiming
):
  require(zpad >= 0, "zpad must be non-negative")

object ApplyControl:
  val linear: ApplyControl =
    unsafe(Interpolation.Linear, PadMode.Clamp, zpad = 0, AcquisitionTiming.Volume)

  def make(
      interpolation: Interpolation = Interpolation.Linear,
      padMode: PadMode = PadMode.Clamp,
      zpad: Int = 0,
      acquisitionTiming: AcquisitionTiming = AcquisitionTiming.Volume
  ): Either[MotionError, ApplyControl] =
    if zpad < 0 then Left(MotionError.InvalidInt("zpad", zpad, "must be non-negative"))
    else Right(unsafe(interpolation, padMode, zpad, acquisitionTiming))

  def unsafe(
      interpolation: Interpolation,
      padMode: PadMode,
      zpad: Int,
      acquisitionTiming: AcquisitionTiming = AcquisitionTiming.Volume
  ): ApplyControl =
    new ApplyControl(interpolation, padMode, zpad, acquisitionTiming)
