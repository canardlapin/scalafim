package scalafim.connectivity

import scala.collection.mutable

final case class WindowSpec private (length: Int, step: Int):
  def description: String =
    s"window=$length:step=$step"

object WindowSpec:
  def from(length: Int, step: Int): Either[ConnectivityError, WindowSpec] =
    if length <= 0 then Left(ConnectivityError.InvalidDimension("dynamic window length", length))
    else if step <= 0 then Left(ConnectivityError.InvalidDimension("dynamic window step", step))
    else Right(WindowSpec(length, step))

  def unsafe(length: Int, step: Int): WindowSpec =
    from(length, step).fold(error => throw new IllegalArgumentException(error.message), identity)

final case class TimeWindow private (start: SampleIndex, length: Int):
  def endExclusive: SampleIndex =
    SampleIndex.unsafe(start.value + length)

  def centerSample: Double =
    start.value.toDouble + (length.toDouble - 1.0) / 2.0

  def description: String =
    s"${start.value}:${start.value + length}"

object TimeWindow:
  def from(start: SampleIndex, length: Int): Either[ConnectivityError, TimeWindow] =
    if length <= 0 then Left(ConnectivityError.InvalidDimension("time window length", length))
    else Right(TimeWindow(start, length))

  def unsafe(start: Int, length: Int): TimeWindow =
    from(SampleIndex.unsafe(start), length).fold(error => throw new IllegalArgumentException(error.message), identity)

final class WindowAxis private (val windows: Vector[TimeWindow]):
  def size: Int =
    windows.length

  def starts: Vector[SampleIndex] =
    windows.map(_.start)

object WindowAxis:
  def from(windows: Iterable[TimeWindow]): Either[ConnectivityError, WindowAxis] =
    val values = windows.toVector
    if values.isEmpty then Left(ConnectivityError.EmptyAxis("window"))
    else
      val seen = mutable.HashSet.empty[Int]
      var previousStart = -1
      var index = 0
      var error = Option.empty[ConnectivityError]
      while index < values.length && error.isEmpty do
        val window = values(index)
        val start = window.start.value
        if seen.contains(start) then
          error = Some(ConnectivityError.DuplicateId("window", start.toString))
        else if start <= previousStart then
          error = Some(ConnectivityError.AxisMismatch(s"window starts must be strictly increasing, got $start after $previousStart at position $index"))
        else
          seen += start
          previousStart = start
        index += 1
      error match
        case Some(value) => Left(value)
        case None        => Right(new WindowAxis(values))

  def sliding(timeAxis: TimeAxis, spec: WindowSpec): Either[ConnectivityError, WindowAxis] =
    if spec.length > timeAxis.sampleCount then
      Left(ConnectivityError.InvalidPlan(s"window length ${spec.length} exceeds sample count ${timeAxis.sampleCount}"))
    else
      val out = Vector.newBuilder[TimeWindow]
      var start = 0
      while start + spec.length <= timeAxis.sampleCount do
        out += TimeWindow.unsafe(start, spec.length)
        start += spec.step
      from(out.result())

  def unsafe(windows: Iterable[TimeWindow]): WindowAxis =
    from(windows).fold(error => throw new IllegalArgumentException(error.message), identity)
