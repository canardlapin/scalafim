package scalafim.fmri.design.event

import scalafim.fmri.design.{DesignError, EventRowProvenance, PhaseId, TrialId}
import scalafim.fmri.hrf.Seconds

/** One phase of a conceptual trial table.
  *
  * A phase owns its event schedule, but its provenance is derived from the
  * same parent/source-row axis as every other phase in a
  * [[MultiphaseEventTerm]].  This makes sample/delay/probe lowering explicit
  * without asking callers to maintain parallel, independently filtered tables.
  */
final case class EventPhase private (
    id: PhaseId,
    schedule: EventSchedule,
    provenance: Vector[EventRowProvenance]
):
  require(provenance.length == schedule.size, "phase provenance must match schedule length")
  require(provenance.forall(_.phase.contains(id)), "phase provenance must carry this phase id")

  def size: Int = schedule.size
  def onsets: Vector[Seconds] = schedule.onsets
  def durations: Vector[Seconds] = schedule.durations
  def blockIds: Vector[Int] = schedule.blockIds
  def parentTrialIds: Vector[TrialId] = provenance.map(_.parent)
  def sourceRows: Vector[Int] = provenance.map(_.sourceRow)

  /** Lower this phase against the shared event/factor values. */
  def term(events: Vector[Event], termTag: Option[String] = None): Either[DesignError, EventTerm] =
    EventTerm.fromPhase(events, this, termTag)

object EventPhase:

  def fromSchedule(
      id: PhaseId,
      schedule: EventSchedule,
      parentTrialIds: Vector[TrialId],
      sourceRows: Vector[Int]
  ): Either[DesignError, EventPhase] =
    if parentTrialIds.length != schedule.size then
      Left(DesignError.InvalidSchedule(s"phase '${id.value}' parent ids have length ${parentTrialIds.length} but expected ${schedule.size}"))
    else if parentTrialIds.distinct.length != parentTrialIds.length then
      Left(DesignError.InvalidSchedule(s"phase '${id.value}' parent trial ids must be unique"))
    else if sourceRows.length != schedule.size then
      Left(DesignError.InvalidSchedule(s"phase '${id.value}' source rows have length ${sourceRows.length} but expected ${schedule.size}"))
    else if sourceRows.distinct.length != sourceRows.length then
      Left(DesignError.InvalidSchedule(s"phase '${id.value}' source rows must be unique"))
    else if sourceRows.exists(_ < 0) then
      Left(DesignError.InvalidSchedule(s"phase '${id.value}' source rows must be non-negative"))
    else
      val provenance = schedule.events.zip(parentTrialIds.zip(sourceRows)).map { case (event, (parent, source)) =>
        EventRowProvenance(
          parent = parent,
          phase = Some(id),
          sourceRow = source,
          blockId = event.blockId,
          onset = event.onset,
          duration = event.durationSeconds
        )
      }
      Right(EventPhase(id, schedule, provenance))

  def fromParts(
      id: PhaseId,
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      blockIds: Vector[Int],
      parentTrialIds: Vector[TrialId],
      sourceRows: Vector[Int]
  ): Either[DesignError, EventPhase] =
    EventSchedule.fromParts(onsets, durations, blockIds).flatMap { schedule =>
      fromSchedule(id, schedule, parentTrialIds, sourceRows)
    }

/** A collection of phase schedules lowered from one conceptual trial axis.
  *
  * This is intentionally runtime-data oriented: factors and modulators remain
  * ordinary [[Event]] values, while phase timing is supplied by checked
  * schedules.  Every phase must retain the same parent/source-row ordering;
  * if a caller has filtered rows, that filtering must be represented before
  * constructing the collection rather than silently misaligning phases.
  */
final case class MultiphaseEventTerm private (
    events: Vector[Event],
    phases: Vector[EventPhase],
    termTag: Option[String]
):
  require(phases.nonEmpty, "a multiphase event term must contain at least one phase")
  require(phases.map(_.id).distinct.length == phases.length, "phase ids must be unique")
  require(phases.forall(_.size == events.headOption.map(_.nEvents).getOrElse(0)), "all phases must match event row count")
  require(events.forall(_.nEvents == phases.head.size), "all events must match phase row count")

  /** Lower each phase while preserving the shared parent/source-row identity. */
  def phaseTerms: Vector[EventTerm] =
    phases.map { phase =>
      phase.term(events, termTag).fold(error => throw new IllegalArgumentException(error.message), identity)
    }

object MultiphaseEventTerm:

  def validated(
      events: Vector[Event],
      phases: Vector[EventPhase],
      termTag: Option[String] = None
  ): Either[DesignError, MultiphaseEventTerm] =
    if phases.isEmpty then Left(DesignError.InvalidSchedule("a multiphase event term must contain at least one phase"))
    else if phases.map(_.id).distinct.length != phases.length then
      Left(DesignError.InvalidSchedule("phase ids must be unique"))
    else if events.exists(_.nEvents != phases.head.size) then
      Left(DesignError.InvalidSchedule("all events must match phase row count"))
    else if phases.exists(_.parentTrialIds != phases.head.parentTrialIds) then
      Left(DesignError.InvalidSchedule("all phases must preserve the same parent trial ordering"))
    else if phases.exists(_.sourceRows != phases.head.sourceRows) then
      Left(DesignError.InvalidSchedule("all phases must preserve the same source-row ordering"))
    else
      termTag match
        case Some(tag) =>
          scalafim.fmri.design.TermId(tag).map(_ => MultiphaseEventTerm(events, phases, Some(tag)))
        case None => Right(MultiphaseEventTerm(events, phases, None))
