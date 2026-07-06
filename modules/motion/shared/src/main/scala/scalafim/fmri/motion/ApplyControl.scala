package scalafim.fmri.motion

final case class ApplyControl private (
    interpolation: Interpolation,
    padMode: PadMode,
    zpad: Int
):
  require(zpad >= 0, "zpad must be non-negative")

object ApplyControl:
  val linear: ApplyControl =
    unsafe(Interpolation.Linear, PadMode.Clamp, zpad = 0)

  def make(
      interpolation: Interpolation = Interpolation.Linear,
      padMode: PadMode = PadMode.Clamp,
      zpad: Int = 0
  ): Either[MotionError, ApplyControl] =
    if zpad < 0 then Left(MotionError.InvalidInt("zpad", zpad, "must be non-negative"))
    else Right(unsafe(interpolation, padMode, zpad))

  def unsafe(interpolation: Interpolation, padMode: PadMode, zpad: Int): ApplyControl =
    new ApplyControl(interpolation, padMode, zpad)
