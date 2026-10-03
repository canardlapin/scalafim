package scalafim.fmri.design

import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame

/** Selection is explicit because pre-run events can be scientifically useful.
  * DropOutsideRun selects run-relative onset times in [0, acquisition
  * duration), not response support; it must not be confused with truncating an
  * HRF at the last scan.
  */
enum EventOnsetPolicy:
  case RejectInvalid, DropOutsideRun

enum ExcludedEventOnsetReason:
  case Missing, BeforeRun, AtOrAfterRunEnd

  /** Stable reason token recorded in [[DesignAudit.excludedEvents]]. */
  def auditReason: String = this match
    case Missing => "onset-missing"
    case BeforeRun => "onset-before-run"
    case AtOrAfterRunEnd => "onset-at-or-after-run-end"

/** One dropped source row. `onset` is `None` exactly when the reason is
  * [[ExcludedEventOnsetReason.Missing]]. */
final case class ExcludedEventOnset(sourceRow: Int, run: RunIndex, onset: Option[Seconds], reason: ExcludedEventOnsetReason):
  require(sourceRow >= 0, "source row must be non-negative")
  require(onset.isEmpty == (reason == ExcludedEventOnsetReason.Missing), "only missing onsets lack an onset value")

final case class EventOnsetSelection(retainedRows: Vector[Int], excluded: Vector[ExcludedEventOnset]):
  def excludedCount: Int = excluded.length

  /** Audit entries for the dropped rows, indexed by source row.
    *
    * Selection runs before a schedule or model exists, so nothing records
    * these rows automatically: the caller must add them to the compiled
    * design's audit, e.g. `audit.copy(excludedEvents = audit.excludedEvents ++
    * selection.auditEntries(term))`.
    */
  def auditEntries(term: Option[TermId] = None): Vector[EventExclusion] =
    excluded.map(event => EventExclusion(event.sourceRow, event.reason.auditReason, term))

enum EventOnsetSelectionError:
  case RowCount(onsets: Int, runs: Int)
  case UnknownRun(row: Int, run: RunIndex, runCount: Int)
  case InvalidRunDuration(run: RunIndex)
  case Rejected(events: Vector[ExcludedEventOnset])

  def message: String = this match
    case RowCount(onsets, runs) => s"$onsets onset rows but $runs run identities"
    case UnknownRun(row, run, runCount) => s"row $row references run ${run.oneBased}; only $runCount runs are available"
    case InvalidRunDuration(run) => s"run ${run.oneBased} has a non-finite acquisition duration"
    case Rejected(events) => s"${events.length} event onsets are missing or outside their run"

object EventOnsetSelection:
  /** Apply before constructing a finite EventSchedule. Onsets are run-relative
    * and `None` marks a missing onset. Returned source indices let the caller
    * subset every aligned column; [[EventOnsetSelection.auditEntries]] turns the
    * exclusions into design-audit evidence.
    */
  def select(onsets: Vector[Option[Seconds]], runs: Vector[RunIndex], frame: SamplingFrame, policy: EventOnsetPolicy): Either[EventOnsetSelectionError, EventOnsetSelection] =
    if onsets.length != runs.length then Left(EventOnsetSelectionError.RowCount(onsets.length, runs.length))
    else runs.zipWithIndex.find((run, _) => run.oneBased > frame.nBlocks) match
      case Some((run, row)) => Left(EventOnsetSelectionError.UnknownRun(row, run, frame.nBlocks))
      case None =>
        val ends = frame.blockLens.indices.map(i => frame.blockLens(i).toDouble * frame.tr(i).value).toVector
        ends.indexWhere(value => !value.isFinite) match
          case index if index >= 0 => Left(EventOnsetSelectionError.InvalidRunDuration(RunIndex.unsafeOneBased(index + 1)))
          case _ =>
            val excluded = onsets.indices.flatMap: row =>
              val run = runs(row)
              val reason = onsets(row) match
                case None => Some(ExcludedEventOnsetReason.Missing)
                case Some(onset) if onset.value < 0.0 => Some(ExcludedEventOnsetReason.BeforeRun)
                case Some(onset) if onset.value >= ends(run.zeroBased) => Some(ExcludedEventOnsetReason.AtOrAfterRunEnd)
                case Some(_) => None
              reason.map(ExcludedEventOnset(row, run, onsets(row), _))
            .toVector
            if excluded.nonEmpty && policy == EventOnsetPolicy.RejectInvalid then Left(EventOnsetSelectionError.Rejected(excluded))
            else
              val dropped = excluded.map(_.sourceRow).toSet
              Right(EventOnsetSelection(onsets.indices.filterNot(dropped).toVector, excluded))
