package scalafim.fmri.design.event

import scalafim.fmri.design.{DesignError, EventId}
import scalafim.fmri.hrf.{NonNegativeSeconds, Seconds, TimeError, s}

final case class ScheduledEvent private (
    id: EventId,
    onset: Seconds,
    duration: NonNegativeSeconds,
    blockId: Int
):
  def durationSeconds: Seconds = duration.seconds

object ScheduledEvent:
  def apply(index: Int, onset: Seconds, duration: Seconds, blockId: Int): Either[DesignError, ScheduledEvent] =
    if blockId < 0 then Left(DesignError.InvalidSchedule(s"event ${index + 1} has negative block id $blockId"))
    else
      for
        id <- EventId(s"event_${index + 1}")
        duration0 <- NonNegativeSeconds.fromSeconds(duration, "duration").left.map(durationError(index, _))
      yield ScheduledEvent(id, onset, duration0, blockId)

  private def durationError(index: Int, error: TimeError): DesignError =
    DesignError.InvalidSchedule(s"event ${index + 1}: ${error.message}")

final case class EventSchedule private (events: Vector[ScheduledEvent]):
  def size: Int = events.length
  def onsets: Vector[Seconds] = events.map(_.onset)
  def durations: Vector[Seconds] = events.map(_.durationSeconds)
  def blockIds: Vector[Int] = events.map(_.blockId)

object EventSchedule:
  def fromParts(
      onsets: Seq[Seconds],
      durations: Seq[Seconds] = Seq.empty,
      blockIds: Seq[Int] = Seq.empty
  ): Either[DesignError, EventSchedule] =
    val n = onsets.length
    val durations0: Vector[Seconds] =
      if durations.isEmpty then Vector.fill(n)(0.0.s)
      else if durations.length == n then durations.toVector
      else return Left(DesignError.InvalidSchedule(s"durations has length ${durations.length} but expected $n"))

    val blockIds0: Vector[Int] =
      if blockIds.isEmpty then Vector.fill(n)(0)
      else if blockIds.length == n then blockIds.toVector
      else return Left(DesignError.InvalidSchedule(s"blockIds has length ${blockIds.length} but expected $n"))

    if isStrictlyDecreasing(blockIds0) then
      Left(DesignError.InvalidSchedule("'blockIds' must be non-decreasing"))
    else
      val out = Vector.newBuilder[ScheduledEvent]
      var failed: Option[DesignError] = None
      var i = 0
      val onsets0 = onsets.toVector
      while i < n && failed.isEmpty do
        ScheduledEvent(i, onsets0(i), durations0(i), blockIds0(i)) match
          case Left(err0) => failed = Some(err0)
          case Right(event0) => out += event0
        i += 1
      failed match
        case Some(err0) => Left(err0)
        case None => Right(EventSchedule(out.result()))

  private def isStrictlyDecreasing(xs: Vector[Int]): Boolean =
    var i = 1
    while i < xs.length do
      if xs(i) < xs(i - 1) then return true
      i += 1
    false
