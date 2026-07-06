package scalafim.fmri.motion

opaque type FrameIndex = Int
opaque type Millimeters = Double
opaque type Radians = Double
opaque type Seconds = Double
opaque type HeadRadius = Double

object FrameIndex:
  def apply(value: Int): Either[MotionError, FrameIndex] =
    if value >= 0 then Right(value)
    else Left(MotionError.InvalidInt("FrameIndex", value, "must be non-negative"))

  def unsafe(value: Int): FrameIndex =
    require(value >= 0, "FrameIndex must be non-negative")
    value

  extension (index: FrameIndex)
    def value: Int = index

object Millimeters:
  def apply(value: Double): Either[MotionError, Millimeters] =
    finite("Millimeters", value)

  def unsafe(value: Double): Millimeters =
    require(value.isFinite, "Millimeters must be finite")
    value

  extension (x: Millimeters)
    def value: Double = x

object Radians:
  def apply(value: Double): Either[MotionError, Radians] =
    finite("Radians", value)

  def unsafe(value: Double): Radians =
    require(value.isFinite, "Radians must be finite")
    value

  extension (x: Radians)
    def value: Double = x

object Seconds:
  def apply(value: Double): Either[MotionError, Seconds] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(MotionError.InvalidScalar("Seconds", value, "must be positive and finite"))

  def unsafe(value: Double): Seconds =
    require(value.isFinite && value > 0.0, "Seconds must be positive and finite")
    value

  extension (x: Seconds)
    def value: Double = x

object HeadRadius:
  val default: HeadRadius = 50.0

  def apply(value: Double): Either[MotionError, HeadRadius] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(MotionError.InvalidScalar("HeadRadius", value, "must be positive and finite"))

  def unsafe(value: Double): HeadRadius =
    require(value.isFinite && value > 0.0, "HeadRadius must be positive and finite")
    value

  extension (x: HeadRadius)
    def value: Double = x

private[motion] def finite[A](name: String, value: Double): Either[MotionError, Double] =
  if value.isFinite then Right(value)
  else Left(MotionError.InvalidScalar(name, value, "must be finite"))
