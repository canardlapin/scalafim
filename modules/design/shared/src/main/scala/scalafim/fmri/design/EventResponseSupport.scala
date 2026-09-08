package scalafim.fmri.design

import scalafim.fmri.design.event.ConvolvedTerm
import scalafim.fmri.hrf.Seconds

enum EventSupportPolicy:
  case RejectUnobservedAllowPartial, ExcludeUnobservedAllowPartial, RequireFullWindow

enum EventSupportStatus:
  case Observed, PartialWindow, Unobserved

enum EventSupportDisposition:
  case Retained, Excluded, Rejected

final case class ResponseSupportRequest(
    retainedScans: Vector[ScanIndex],
    policy: EventSupportPolicy,
    absoluteTolerance: Double = 0.0
):
  require(absoluteTolerance.isFinite && absoluteTolerance >= 0.0, "support tolerance must be finite and non-negative")
  require(retainedScans.map(_.oneBased).sliding(2).forall(pair => pair.length < 2 || pair(0) < pair(1)),
    "retained scans must be strictly increasing")

final case class EventSupportColumn(columnIndex: Int, name: String, observedSamples: Int)
final case class EventSupportDecision(
    sourceRow: Int,
    blockId: Int,
    onset: Seconds,
    duration: Seconds,
    status: EventSupportStatus,
    disposition: EventSupportDisposition,
    columns: Vector[EventSupportColumn]
)

final case class EventSupportReceipt(
    term: Option[String],
    request: ResponseSupportRequest,
    decisions: Vector[EventSupportDecision]
):
  def keepMask: Vector[Boolean] = decisions.map(_.disposition == EventSupportDisposition.Retained)
  def canonical: String =
    val rows = decisions.map { row =>
      s"source=${row.sourceRow};block=${row.blockId};onset=${java.lang.Double.doubleToLongBits(row.onset.value)};" +
        s"duration=${java.lang.Double.doubleToLongBits(row.duration.value)};status=${row.status};action=${row.disposition};" +
        row.columns.map(c => s"${c.columnIndex}:${c.name.length}:${c.name}:${c.observedSamples}").mkString("columns=[", ",", "]")
    }
    s"term=${term.getOrElse("")};policy=${request.policy};tolerance=${java.lang.Double.doubleToLongBits(request.absoluteTolerance)};" +
      s"scans=${request.retainedScans.map(_.oneBased).mkString(",")};" + rows.mkString("rows=[", "|", "]")

private[design] object EventResponseSupport:
  /** The compiler supplies a potential term: categorical codes are unchanged,
    * continuous contributions are one. Thus nonzero input denotes actual cell
    * membership, independent of a zero or missing modulator.
    */
  def assess(potential: ConvolvedTerm, request: ResponseSupportRequest): Either[DesignError, EventSupportReceipt] =
    potential.sampledSupport(request.retainedScans, request.absoluteTolerance)
      .left.map(error => DesignError.InvalidSchedule(s"Cannot assess event response support: $error"))
      .flatMap { support =>
        val rows = potential.term.onsets.indices.map { event =>
          val eligible = support.contributions.filter(c => c.eventIndex == event && c.inputAmplitude != 0.0)
          val observed = eligible.exists(_.actual.sampleCount > 0)
          val partial = eligible.exists(c => c.actual.sampleCount == 0 ||
            c.actual.window.exists(w => w.startsBeforeGrid || w.endsAfterGrid))
          val status = if !observed then EventSupportStatus.Unobserved
            else if partial then EventSupportStatus.PartialWindow else EventSupportStatus.Observed
          val disposition = (status, request.policy) match
            case (EventSupportStatus.Unobserved, EventSupportPolicy.ExcludeUnobservedAllowPartial) => EventSupportDisposition.Excluded
            case (EventSupportStatus.Unobserved, _) => EventSupportDisposition.Rejected
            case (EventSupportStatus.PartialWindow, EventSupportPolicy.RequireFullWindow) => EventSupportDisposition.Rejected
            case _ => EventSupportDisposition.Retained
          EventSupportDecision(potential.term.sourceRowIndices(event), potential.term.blockIds0(event),
            potential.term.onsets(event), potential.term.durations0(event), status, disposition,
            eligible.map(c => EventSupportColumn(c.columnIndex, c.columnName, c.actual.sampleCount)))
        }.toVector
        val receipt = EventSupportReceipt(potential.term.termTag, request, rows)
        if rows.exists(_.disposition == EventSupportDisposition.Rejected) then Left(DesignError.ResponseSupportRejected(receipt))
        else Right(receipt)
      }
