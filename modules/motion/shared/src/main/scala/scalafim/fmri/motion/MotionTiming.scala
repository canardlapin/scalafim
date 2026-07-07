package scalafim.fmri.motion

opaque type AcquisitionOffsetSeconds = Double

object AcquisitionOffsetSeconds:
  def apply(value: Double): Either[MotionError, AcquisitionOffsetSeconds] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(MotionError.InvalidScalar("AcquisitionOffsetSeconds", value, "must be non-negative and finite"))

  def unsafe(value: Double): AcquisitionOffsetSeconds =
    require(value.isFinite && value >= 0.0, "AcquisitionOffsetSeconds must be non-negative and finite")
    value

  extension (offset: AcquisitionOffsetSeconds)
    def value: Double = offset

final case class SliceTiming private (
    offsets: Vector[AcquisitionOffsetSeconds]
):
  def nSlices: Int =
    offsets.length

  def offset(slice: Int): Either[MotionError, AcquisitionOffsetSeconds] =
    if slice >= 0 && slice < offsets.length then Right(offsets(slice))
    else Left(MotionError.InvalidInt("slice", slice, s"must be in [0, ${offsets.length})"))

  def offsetSeconds: Vector[Double] =
    offsets.map(_.value)

object SliceTiming:
  def make(offsets: Vector[Double]): Either[MotionError, SliceTiming] =
    if offsets.isEmpty then Left(MotionError.InvalidInt("offsets.length", 0, "must be positive"))
    else
      val typed = Vector.newBuilder[AcquisitionOffsetSeconds]
      var i = 0
      while i < offsets.length do
        AcquisitionOffsetSeconds(offsets(i)) match
          case Left(error) => return Left(error)
          case Right(offset) => typed += offset
        i += 1
      Right(new SliceTiming(typed.result()))

  def unsafe(offsets: Vector[Double]): SliceTiming =
    make(offsets).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class SlicePacket private (
    slices: Vector[Int],
    offset: AcquisitionOffsetSeconds
):
  def offsetSeconds: Double =
    offset.value

object SlicePacket:
  def make(slices: Vector[Int], offsetSeconds: Double): Either[MotionError, SlicePacket] =
    if slices.isEmpty then Left(MotionError.InvalidInt("slices.length", 0, "must be positive"))
    else AcquisitionOffsetSeconds(offsetSeconds).map(offset => new SlicePacket(slices, offset))

  def unsafe(slices: Vector[Int], offsetSeconds: Double): SlicePacket =
    make(slices, offsetSeconds).fold(err => throw new IllegalArgumentException(err.message), identity)

final case class PacketTiming private (
    nSlices: Int,
    packets: Vector[SlicePacket]
):
  def toSliceTiming: SliceTiming =
    val offsets = Array.fill(nSlices)(0.0)
    var p = 0
    while p < packets.length do
      val packet = packets(p)
      var i = 0
      while i < packet.slices.length do
        offsets(packet.slices(i)) = packet.offset.value
        i += 1
      p += 1
    SliceTiming.unsafe(offsets.toVector)

object PacketTiming:
  def make(nSlices: Int, packets: Vector[SlicePacket]): Either[MotionError, PacketTiming] =
    if nSlices <= 0 then Left(MotionError.InvalidInt("nSlices", nSlices, "must be positive"))
    else if packets.isEmpty then Left(MotionError.InvalidInt("packets.length", 0, "must be positive"))
    else
      val seen = Array.fill(nSlices)(false)
      var covered = 0
      var p = 0
      while p < packets.length do
        val packet = packets(p)
        var i = 0
        while i < packet.slices.length do
          val slice = packet.slices(i)
          if slice < 0 || slice >= nSlices then
            return Left(MotionError.InvalidInt("slice", slice, s"must be in [0, $nSlices)"))
          if seen(slice) then
            return Left(MotionError.InvalidInt("slice", slice, "must appear in exactly one packet"))
          seen(slice) = true
          covered += 1
          i += 1
        p += 1
      if covered != nSlices then Left(MotionError.ShapeMismatch("packet slices", Vector(nSlices), Vector(covered)))
      else Right(new PacketTiming(nSlices, packets))

  def unsafe(nSlices: Int, packets: Vector[SlicePacket]): PacketTiming =
    make(nSlices, packets).fold(err => throw new IllegalArgumentException(err.message), identity)

enum AcquisitionTiming:
  case Volume
  case Slice(timing: SliceTiming)
  case Packet(timing: PacketTiming)

  def isVolume: Boolean =
    this match
      case Volume => true
      case _ => false
