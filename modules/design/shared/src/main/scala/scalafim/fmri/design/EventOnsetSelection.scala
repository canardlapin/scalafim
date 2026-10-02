package scalafim.fmri.design

import scalafim.fmri.hrf.design.SamplingFrame

/** Selection is explicit because pre-run events can be scientifically useful.
  * DropOutsideRun selects onset times in [0, acquisition duration), not response
  * support; it must not be confused with truncating an HRF at the last scan.
  */
enum EventOnsetPolicy:
  case RejectInvalid, DropOutsideRun

enum ExcludedEventOnsetReason:
  case NonFinite, BeforeRun, AtOrAfterRunEnd

final case class ExcludedEventOnset(sourceRow: Int, run: RunIndex, onset: Double, reason: ExcludedEventOnsetReason)
final case class EventOnsetSelection(retainedRows: Vector[Int], excluded: Vector[ExcludedEventOnset]):
  def excludedCount: Int = excluded.length

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
  /** Apply before constructing a finite EventSchedule. Returned source indices
    * let the caller subset every aligned column and preserve exclusion evidence.
    */
  def select(onsets: Vector[Double], runs: Vector[RunIndex], frame: SamplingFrame, policy: EventOnsetPolicy): Either[EventOnsetSelectionError, EventOnsetSelection] =
    if onsets.length != runs.length then Left(EventOnsetSelectionError.RowCount(onsets.length, runs.length))
    else runs.zipWithIndex.find((run, _) => run.oneBased > frame.nBlocks) match
      case Some((run, row)) => Left(EventOnsetSelectionError.UnknownRun(row, run, frame.nBlocks))
      case None =>
        val ends = frame.blockLens.indices.map(i => frame.blockLens(i).toDouble * frame.tr(i).value).toVector
        ends.indexWhere(value => !value.isFinite) match
          case index if index >= 0 => Left(EventOnsetSelectionError.InvalidRunDuration(RunIndex.unsafeOneBased(index + 1)))
          case _ =>
            val excluded = onsets.indices.flatMap: row =>
              val onset = onsets(row)
              val run = runs(row)
              val reason = if !onset.isFinite then Some(ExcludedEventOnsetReason.NonFinite)
                else if onset < 0.0 then Some(ExcludedEventOnsetReason.BeforeRun)
                else if onset >= ends(run.oneBased - 1) then Some(ExcludedEventOnsetReason.AtOrAfterRunEnd)
                else None
              reason.map(ExcludedEventOnset(row, run, onset, _))
            .toVector
            if excluded.nonEmpty && policy == EventOnsetPolicy.RejectInvalid then Left(EventOnsetSelectionError.Rejected(excluded))
            else
              val dropped = excluded.map(_.sourceRow).toSet
              Right(EventOnsetSelection(onsets.indices.filterNot(dropped).toVector, excluded))
