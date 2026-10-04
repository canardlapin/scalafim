package scalafim.fmri.design

import scalafim.fmri.design.event.{CategoricalEvent, ConvolvedTerm, EventModel, EventTerm}
import scalafim.fmri.hrf.Seconds

/** Explicit timing for one conceptual parent trial. A phase onset is never
  * treated as its parent's onset: callers must supply this separately.
  *
  * Parent onsets are run-relative seconds, on the same clock as the event
  * onsets of the run named by `key.run` (not the concatenated global clock).
  * A phase that starts before its parent is rejected as
  * [[EventTrialSummaryError.PhaseBeforeParent]], which also catches parent
  * onsets accidentally given on the global clock for later runs.
  */
final case class ParentTrialKey(run: RunIndex, trial: TrialId)

final case class ParentTrialOnset(key: ParentTrialKey, onset: Seconds)

enum EventTrialSummaryError:
  case DuplicateParentTrial(key: ParentTrialKey)
  case ParentTimingRequired(term: TermId)
  case MissingParentTrial(key: ParentTrialKey, term: TermId, eventIndex: Int)
  case InvalidFactorCode(term: TermId, factor: FactorId, eventIndex: Int, code: Int)
  case InconsistentProvenance(term: TermId, eventIndex: Int, reason: String)
  case NonFiniteOffset(term: TermId, eventIndex: Int)
  case PhaseBeforeParent(term: TermId, eventIndex: Int, key: ParentTrialKey, offset: Seconds)
  case InvalidIdentifier(kind: String, value: String, reason: String)

  def message: String = this match
    case DuplicateParentTrial(key) =>
      s"Parent trial ${key.trial.value} occurs more than once in run ${key.run.oneBased}."
    case ParentTimingRequired(term) =>
      s"Term ${term.value} has phase provenance but no explicit parent-trial onset evidence."
    case MissingParentTrial(key, term, eventIndex) =>
      s"Term ${term.value} event $eventIndex has no parent-trial onset for ${key.trial.value} in run ${key.run.oneBased}."
    case InvalidFactorCode(term, factor, eventIndex, code) =>
      s"Term ${term.value} event $eventIndex has invalid code $code for factor ${factor.value}."
    case InconsistentProvenance(term, eventIndex, reason) =>
      s"Term ${term.value} event $eventIndex has inconsistent provenance: $reason"
    case NonFiniteOffset(term, eventIndex) =>
      s"Term ${term.value} event $eventIndex has a non-finite parent-relative onset offset."
    case PhaseBeforeParent(term, eventIndex, key, offset) =>
      s"Term ${term.value} event $eventIndex starts ${-offset.value} s before parent trial ${key.trial.value} in run ${key.run.oneBased}; parent onsets must be run-relative and no later than their phases."
    case InvalidIdentifier(kind, value, reason) =>
      s"Event summary cannot use '$value' as a $kind identifier: $reason"

/** Validated lookup of parent timing. This is intentionally separate from
  * event phases: earliest phase timing is not evidence of a parent onset.
  */
final class ParentTrialOnsets private (private val values: Map[ParentTrialKey, Seconds]):
  private[design] def onset(key: ParentTrialKey): Option[Seconds] = values.get(key)

object ParentTrialOnsets:
  def validated(values: Vector[ParentTrialOnset]): Either[EventTrialSummaryError, ParentTrialOnsets] =
    val duplicates = values.groupBy(_.key).collectFirst { case (key, entries) if entries.size > 1 => key }
    duplicates match
      case Some(key) => Left(EventTrialSummaryError.DuplicateParentTrial(key))
      case None => Right(new ParentTrialOnsets(values.map(value => value.key -> value.onset).toMap))

/** A min/median/max summary in physical seconds. */
final case class EventTimingSummary(minimum: Seconds, median: Seconds, maximum: Seconds)

/** One source-event summary for a compiled event-model lane. `onsetOffset` is
  * present only when this lane has phase provenance and explicit parent timing.
  */
final case class EventTrialSummary(
    term: TermId,
    cell: CellKey,
    run: RunIndex,
    phase: Option[PhaseId],
    events: Int,
    duration: EventTimingSummary,
    onsetOffset: Option[EventTimingSummary]
):
  require(events > 0, "event summaries must contain at least one event")

object EventTrialSummary:
  private final case class Key(term: TermId, cell: CellKey, run: RunIndex, phase: Option[PhaseId])
  private final case class EventValue(key: Key, duration: Seconds, onsetOffset: Option[Seconds])

  /** Summarize each convolved event term by its retained categorical cell and
    * run. Phased terms require explicit parent timing, keyed by run and trial.
    * Summaries are keyed by the model's unique term key (`model.termKeys`), so
    * two terms sharing a tag are never pooled.
    */
  def forModel(
      model: EventModel,
      parentOnsets: Option[ParentTrialOnsets] = None
  ): Either[EventTrialSummaryError, Vector[EventTrialSummary]] =
    val values = Vector.newBuilder[EventValue]
    val terms = model.terms
    var termIndex = 0
    while termIndex < terms.length do
      val (key, modelTerm) = terms(termIndex)
      modelTerm match
        case convolved: ConvolvedTerm =>
          TermId(key).left.map(error => EventTrialSummaryError.InvalidIdentifier("term", key, error.message))
            .flatMap(term => appendTerm(term, convolved.term, parentOnsets, values)) match
            case Left(error) => return Left(error)
            case Right(_) => ()
        case _ => ()
      termIndex += 1
    Right(group(values.result()))

  private def appendTerm(
      id: TermId,
      term: EventTerm,
      parentOnsets: Option[ParentTrialOnsets],
      out: scala.collection.mutable.Builder[EventValue, Vector[EventValue]]
  ): Either[EventTrialSummaryError, Unit] =
    val provenance = term.eventProvenance
    if provenance.nonEmpty && provenance.length != term.onsets.length then
      Left(EventTrialSummaryError.InconsistentProvenance(id, -1, "provenance length does not match event timing"))
    else if term.phaseId.nonEmpty && provenance.isEmpty then
      Left(EventTrialSummaryError.InconsistentProvenance(id, -1, "phase id has no event provenance"))
    else if term.phaseId.nonEmpty && parentOnsets.isEmpty then
      Left(EventTrialSummaryError.ParentTimingRequired(id))
    else
      var eventIndex = 0
      while eventIndex < term.onsets.length do
        val run = RunIndex.unsafeOneBased(term.blockIds0(eventIndex) + 1)
        cellAt(id, term, eventIndex).flatMap { cell =>
          offsetAt(id, term, eventIndex, run, parentOnsets).map { offset =>
            out += EventValue(Key(id, cell, run, term.phaseId), term.durations0(eventIndex), offset)
          }
        } match
          case Left(error) => return Left(error)
          case Right(_) => ()
        eventIndex += 1
      Right(())

  private def cellAt(id: TermId, term: EventTerm, eventIndex: Int): Either[EventTrialSummaryError, CellKey] =
    val assignments = Vector.newBuilder[CellAssignment]
    val factors = term.events.collect { case event: CategoricalEvent => event }
    var factorIndex = 0
    while factorIndex < factors.length do
      val factor = factors(factorIndex)
      val code = factor.codes(eventIndex)
      val factorId = FactorId(factor.varName) match
        case Right(value) => value
        case Left(error) => return Left(EventTrialSummaryError.InvalidIdentifier("factor", factor.varName, error.message))
      if code < 0 || code >= factor.levels.length then
        return Left(EventTrialSummaryError.InvalidFactorCode(id, factorId, eventIndex, code))
      val level = scalafim.fmri.design.contrast.LevelId(factor.levels(code)) match
        case Right(value) => value
        case Left(error) => return Left(EventTrialSummaryError.InvalidIdentifier("level", factor.levels(code), error.message))
      assignments += CellAssignment(factorId, level)
      factorIndex += 1
    CellKey.from(assignments.result()).left.map(error => EventTrialSummaryError.InconsistentProvenance(id, eventIndex, error.message))

  private def offsetAt(
      id: TermId,
      term: EventTerm,
      eventIndex: Int,
      run: RunIndex,
      parentOnsets: Option[ParentTrialOnsets]
  ): Either[EventTrialSummaryError, Option[Seconds]] =
    term.eventProvenance.lift(eventIndex) match
      case None => Right(None)
      case Some(provenance) =>
        if provenance.blockId != term.blockIds0(eventIndex) then
          Left(EventTrialSummaryError.InconsistentProvenance(id, eventIndex, "run does not match event schedule"))
        else if provenance.onset != term.onsets(eventIndex) || provenance.duration != term.durations0(eventIndex) then
          Left(EventTrialSummaryError.InconsistentProvenance(id, eventIndex, "onset or duration does not match event schedule"))
        else if term.phaseId != provenance.phase then
          Left(EventTrialSummaryError.InconsistentProvenance(id, eventIndex, "phase does not match term phase"))
        else if provenance.phase.isEmpty then Right(None)
        else
          val parent = ParentTrialKey(run, provenance.parent)
          parentOnsets.flatMap(_.onset(parent)) match
            case Some(onset) =>
              val offset = provenance.onset.value - onset.value
              if !offset.isFinite then Left(EventTrialSummaryError.NonFiniteOffset(id, eventIndex))
              else if offset < 0.0 then Left(EventTrialSummaryError.PhaseBeforeParent(id, eventIndex, parent, Seconds.unsafe(offset)))
              else Right(Some(Seconds.unsafe(offset)))
            case None => Left(EventTrialSummaryError.MissingParentTrial(parent, id, eventIndex))

  private def group(values: Vector[EventValue]): Vector[EventTrialSummary] =
    val grouped = scala.collection.mutable.LinkedHashMap.empty[Key, Vector[EventValue]]
    values.foreach { value =>
      grouped.updateWith(value.key)(previous => Some(previous.getOrElse(Vector.empty) :+ value))
    }
    grouped.iterator.map { (key, entries) =>
      EventTrialSummary(
        term = key.term,
        cell = key.cell,
        run = key.run,
        phase = key.phase,
        events = entries.size,
        duration = timing(entries.map(_.duration)),
        onsetOffset = entries.head.onsetOffset.map(_ => timing(entries.flatMap(_.onsetOffset)))
      )
    }.toVector

  private def timing(values: Vector[Seconds]): EventTimingSummary =
    val ordered = values.sortBy(_.value)
    val middle = ordered.size / 2
    val median =
      if ordered.size % 2 == 1 then ordered(middle)
      else Seconds(ordered(middle - 1).value / 2.0 + ordered(middle).value / 2.0)
    EventTimingSummary(ordered.head, median, ordered.last)
