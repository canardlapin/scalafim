package scalafim.fmri.motion

opaque type FrameIndex = Int
opaque type FrameCount = Int
opaque type Millimeters = Double
opaque type Radians = Double
opaque type Seconds = Double
opaque type HeadRadius = Double
opaque type FramewiseDisplacementMm = Double
opaque type DvarsRms = Double
opaque type FitCost = Double
opaque type FitCostTolerance = Double
opaque type MotionMagnitudeMm = Double
opaque type PoseScale = Double

object FrameIndex:
  def apply(value: Int): Either[MotionError, FrameIndex] =
    if value >= 0 then Right(value)
    else Left(MotionError.InvalidInt("FrameIndex", value, "must be non-negative"))

  def unsafe(value: Int): FrameIndex =
    require(value >= 0, "FrameIndex must be non-negative")
    value

  extension (index: FrameIndex)
    def value: Int = index

object FrameCount:
  def apply(value: Int): Either[MotionError, FrameCount] =
    if value > 0 then Right(value)
    else Left(MotionError.InvalidInt("FrameCount", value, "must be positive"))

  def unsafe(value: Int): FrameCount =
    require(value > 0, "FrameCount must be positive")
    value

  extension (count: FrameCount)
    def value: Int = count

final case class FramePair private (previous: FrameIndex, current: FrameIndex):
  require(current.value == previous.value + 1, "FramePair must contain adjacent frames")

object FramePair:
  def adjacent(previous: FrameIndex, current: FrameIndex): Either[MotionError, FramePair] =
    if current.value == previous.value + 1 then Right(new FramePair(previous, current))
    else Left(MotionError.InvalidFramePair(previous.value, current.value, "frames must be adjacent"))

  def forCurrent(current: FrameIndex): Either[MotionError, FramePair] =
    if current.value > 0 then Right(unsafe(current.value - 1, current.value))
    else Left(MotionError.InvalidFramePair(current.value - 1, current.value, "current frame must be positive"))

  def unsafe(previous: Int, current: Int): FramePair =
    require(previous >= 0, "FramePair previous frame must be non-negative")
    require(current == previous + 1, "FramePair must contain adjacent frames")
    new FramePair(FrameIndex.unsafe(previous), FrameIndex.unsafe(current))

final case class FrameAligned[+A] private (values: Vector[A], frameCount: FrameCount):
  require(values.length == frameCount.value, "FrameAligned length must match frameCount")

  def length: Int = frameCount.value
  def isEmpty: Boolean = values.isEmpty
  def nonEmpty: Boolean = values.nonEmpty
  def toVector: Vector[A] = values

  def apply(index: FrameIndex): Either[MotionError, A] =
    val i = index.value
    if i >= 0 && i < values.length then Right(values(i))
    else Left(MotionError.FrameIndexOutOfBounds(i, values.length))

  def unsafeFrame(index: Int): A =
    values(index)

object FrameAligned:
  def make[A](values: Vector[A]): Either[MotionError, FrameAligned[A]] =
    FrameCount(values.length).map(count => unsafe(values, count))

  def make[A](values: Vector[A], frameCount: FrameCount): Either[MotionError, FrameAligned[A]] =
    if values.length == frameCount.value then Right(unsafe(values, frameCount))
    else Left(MotionError.ShapeMismatch("frame-aligned values", Vector(frameCount.value), Vector(values.length)))

  def unsafe[A](values: Vector[A]): FrameAligned[A] =
    unsafe(values, FrameCount.unsafe(values.length))

  def unsafe[A](values: Vector[A], frameCount: FrameCount): FrameAligned[A] =
    new FrameAligned(values, frameCount)

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

object FramewiseDisplacementMm:
  val zero: FramewiseDisplacementMm = 0.0

  def apply(value: Double): Either[MotionError, FramewiseDisplacementMm] =
    nonNegativeFinite("FramewiseDisplacementMm", value)

  def unsafe(value: Double): FramewiseDisplacementMm =
    require(value.isFinite && value >= 0.0, "FramewiseDisplacementMm must be non-negative and finite")
    value

  extension (x: FramewiseDisplacementMm)
    def value: Double = x

object DvarsRms:
  def apply(value: Double): Either[MotionError, DvarsRms] =
    nonNegativeFinite("DvarsRms", value)

  def unsafe(value: Double): DvarsRms =
    require(value.isFinite && value >= 0.0, "DvarsRms must be non-negative and finite")
    value

  extension (x: DvarsRms)
    def value: Double = x

object FitCost:
  def apply(value: Double): Either[MotionError, FitCost] =
    nonNegativeFinite("FitCost", value)

  def unsafe(value: Double): FitCost =
    require(value.isFinite && value >= 0.0, "FitCost must be non-negative and finite")
    value

  extension (x: FitCost)
    def value: Double = x

object FitCostTolerance:
  val default: FitCostTolerance = 1e-8

  def apply(value: Double): Either[MotionError, FitCostTolerance] =
    nonNegativeFinite("FitCostTolerance", value)

  def unsafe(value: Double): FitCostTolerance =
    require(value.isFinite && value >= 0.0, "FitCostTolerance must be non-negative and finite")
    value

  extension (x: FitCostTolerance)
    def value: Double = x

object MotionMagnitudeMm:
  def apply(value: Double): Either[MotionError, MotionMagnitudeMm] =
    nonNegativeFinite("MotionMagnitudeMm", value)

  def unsafe(value: Double): MotionMagnitudeMm =
    require(value.isFinite && value >= 0.0, "MotionMagnitudeMm must be non-negative and finite")
    value

  extension (x: MotionMagnitudeMm)
    def value: Double = x

object PoseScale:
  def apply(value: Double): Either[MotionError, PoseScale] =
    if value.isFinite && value > 0.0 && value <= 1.0 then Right(value)
    else Left(MotionError.InvalidScalar("PoseScale", value, "must be in (0, 1]"))

  def unsafe(value: Double): PoseScale =
    require(value.isFinite && value > 0.0 && value <= 1.0, "PoseScale must be in (0, 1]")
    value

  extension (x: PoseScale)
    def value: Double = x

final case class Translation3Mm private (x: Millimeters, y: Millimeters, z: Millimeters):
  def tx: Double = x.value
  def ty: Double = y.value
  def tz: Double = z.value

object Translation3Mm:
  val zero: Translation3Mm =
    unsafe(0.0, 0.0, 0.0)

  def make(tx: Double, ty: Double, tz: Double): Either[MotionError, Translation3Mm] =
    for
      x <- Millimeters(tx)
      y <- Millimeters(ty)
      z <- Millimeters(tz)
    yield unsafeTyped(x, y, z)

  def unsafe(tx: Double, ty: Double, tz: Double): Translation3Mm =
    unsafeTyped(Millimeters.unsafe(tx), Millimeters.unsafe(ty), Millimeters.unsafe(tz))

  def unsafeTyped(x: Millimeters, y: Millimeters, z: Millimeters): Translation3Mm =
    new Translation3Mm(x, y, z)

final case class EulerZYXRad private (rxAngle: Radians, ryAngle: Radians, rzAngle: Radians):
  def rx: Double = rxAngle.value
  def ry: Double = ryAngle.value
  def rz: Double = rzAngle.value

object EulerZYXRad:
  val zero: EulerZYXRad =
    unsafe(0.0, 0.0, 0.0)

  def make(rx: Double, ry: Double, rz: Double): Either[MotionError, EulerZYXRad] =
    for
      x <- Radians(rx)
      y <- Radians(ry)
      z <- Radians(rz)
    yield unsafeTyped(x, y, z)

  def unsafe(rx: Double, ry: Double, rz: Double): EulerZYXRad =
    unsafeTyped(Radians.unsafe(RigidPose.wrapPi(rx)), Radians.unsafe(RigidPose.wrapPi(ry)), Radians.unsafe(RigidPose.wrapPi(rz)))

  def unsafeTyped(rx: Radians, ry: Radians, rz: Radians): EulerZYXRad =
    new EulerZYXRad(
      Radians.unsafe(RigidPose.wrapPi(rx.value)),
      Radians.unsafe(RigidPose.wrapPi(ry.value)),
      Radians.unsafe(RigidPose.wrapPi(rz.value))
    )

private[motion] def finite[A](name: String, value: Double): Either[MotionError, Double] =
  if value.isFinite then Right(value)
  else Left(MotionError.InvalidScalar(name, value, "must be finite"))

private[motion] def nonNegativeFinite(name: String, value: Double): Either[MotionError, Double] =
  if value.isFinite && value >= 0.0 then Right(value)
  else Left(MotionError.InvalidScalar(name, value, "must be non-negative and finite"))
