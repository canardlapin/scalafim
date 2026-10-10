package scalafim.phrfcmp.exec

/** Names that end up in file paths: ASCII letters, digits, `.`, `_`, `-`; not empty, not starting with `.` and never
  * containing `..` (cell ids such as `C-TX-.5` contain a single dot).
  */
private[exec] object SafeName:
  def valid(s: String): Boolean =
    s.nonEmpty && s.length <= 96 && s != "." && s != ".." && !s.contains("..") &&
      s.forall(c => c.isLetterOrDigit && c < 128 || c == '.' || c == '_' || c == '-') && !s.startsWith(".")

/** A pilot cell identifier (for example `T-TX-fast`). */
opaque type CellId = String

object CellId:
  def parse(s: String): Either[String, CellId] =
    if SafeName.valid(s) then Right(s) else Left(s"invalid cell id '$s'")
  extension (c: CellId) inline def value: String = c

/** A method arm identifier (for example `glmsingle`, `phrf-ml`). */
opaque type ArmId = String

object ArmId:
  def parse(s: String): Either[String, ArmId] =
    if SafeName.valid(s) then Right(s) else Left(s"invalid arm id '$s'")
  extension (a: ArmId) inline def value: String = a

/** One pilot cell and the arms that run on every dataset of it. */
final case class PilotCell(id: CellId, arms: Vector[ArmId]):
  require(arms.nonEmpty, "a cell needs at least one arm")
  require(arms.distinct.length == arms.length, "arm ids must be distinct within a cell")

/** The smallest piece of work: one arm on one dataset of one cell. */
final case class WorkUnit(cell: CellId, dataset: Int, arm: ArmId):
  require(dataset >= 0, "dataset index must be non-negative")

/** All arms of one dataset of one cell: the unit of dispatch and of completeness. */
final case class Job(cell: CellId, dataset: Int)

/** Ledger status vocabulary. `Retried` is non-terminal; the other three are final. */
enum UnitStatus(val code: String):
  case Done extends UnitStatus("done")
  case Refused extends UnitStatus("refused")
  case Failed extends UnitStatus("failed")
  case Retried extends UnitStatus("retried")

  def terminal: Boolean = this != Retried

object UnitStatus:
  def fromCode(code: String): Option[UnitStatus] = values.find(_.code == code)

/** Why a unit was committed: by the scheduler (`Scheduled`; the scored attempt is the one of the earliest
  * invocation), or as a recomputation of a job an earlier invocation completed, so that the final aggregating
  * invocation holds every kept dataset in memory (`Rerun`, decision D1; never scored, only compared).
  */
enum CommitPhase(val code: String):
  case Scheduled extends CommitPhase("scheduled")
  case Rerun extends CommitPhase("rerun")

object CommitPhase:
  def fromCode(code: String): Option[CommitPhase] = values.find(_.code == code)

/** One immutable ledger record per unit commit: status only, no rates and no comparative values. It is sealed under
  * an attempt-unique name (it carries the invocation's run id), because status, code and attempt count can differ
  * between invocations (a transient failure, a timeout); a deterministic name never carries them (finding H1).
  *
  * @param invocation 1-based ordinal of the runner invocation on this output (persisted in `cost.json`); the owner
  *                   applies "first completed attempt is scored" by taking, per unit, the `Scheduled` record with the
  *                   smallest invocation
  * @param attempts   attempts made in this invocation, the committed one included
  * @param payload    logical name of the unit's single payload blob
  * @param entries    names of the arm blobs packed in the payload, in name order
  */
final case class LedgerRecord(
    unit: WorkUnit,
    invocation: Int,
    runId: String,
    phase: CommitPhase,
    attempts: Int,
    status: UnitStatus,
    code: String,
    payload: String,
    payloadSha256: String,
    entries: Vector[String] = Vector.empty
):
  require(invocation >= 1 && attempts >= 1, "invocations and attempts are 1-based")
  require(status.terminal, "only a terminal status is committed")
  require(SafeName.valid(runId), "run id must be a safe name")
  require(payload.nonEmpty && LedgerRecord.isSha256(payloadSha256), "payload name and SHA-256")
  require(entries.forall(SafeName.valid) && entries.distinct.length == entries.length, "entry names are safe and distinct")
  require(LedgerRecord.validCode(code), "status codes are short tokens")

  /** Canonical single-line JSON (fixed key order, no timestamps). */
  def json: String =
    ujson.write(
      ujson.Obj(
        "cell" -> ujson.Str(unit.cell.value),
        "dataset" -> ujson.Num(unit.dataset.toDouble),
        "arm" -> ujson.Str(unit.arm.value),
        "invocation" -> ujson.Num(invocation.toDouble),
        "run_id" -> ujson.Str(runId),
        "phase" -> ujson.Str(phase.code),
        "attempts" -> ujson.Num(attempts.toDouble),
        "status" -> ujson.Str(status.code),
        "code" -> ujson.Str(code),
        "payload" -> ujson.Str(payload),
        "payload_sha256" -> ujson.Str(payloadSha256),
        "entries" -> ujson.Arr.from(entries.map(ujson.Str(_)))
      )
    )

object LedgerRecord:
  val Keys: Set[String] =
    Set("cell", "dataset", "arm", "invocation", "run_id", "phase", "attempts", "status", "code", "payload", "payload_sha256", "entries")

  def validCode(code: String): Boolean =
    code.length <= 64 && code.forall(c => c.isLetterOrDigit && c < 128 || c == '_' || c == '.' || c == ':' || c == '-')

  def isSha256(s: String): Boolean = s.length == 64 && s.forall(c => c.isDigit || c >= 'a' && c <= 'f')

  private def wholeNumber(v: ujson.Value): Either[String, Int] =
    val d = v.num
    if d.isWhole && d >= Int.MinValue && d <= Int.MaxValue then Right(d.toInt) else Left("non-integer field")

  def parse(text: String): Either[String, LedgerRecord] =
    try
      val o = ujson.read(text).obj
      if o.keySet != Keys then Left("ledger record keys are not exactly the status-only schema")
      else
        for
          cell <- CellId.parse(o("cell").str)
          arm <- ArmId.parse(o("arm").str)
          status <- UnitStatus.fromCode(o("status").str).filter(_.terminal).toRight("unknown or non-terminal status")
          phase <- CommitPhase.fromCode(o("phase").str).toRight("unknown phase")
          dataset <- wholeNumber(o("dataset"))
          invocation <- wholeNumber(o("invocation"))
          attempts <- wholeNumber(o("attempts"))
          runId = o("run_id").str
          code = o("code").str
          payload = o("payload").str
          sha = o("payload_sha256").str
          entries = o("entries").arr.map(_.str).toVector
          _ <-
            if dataset >= 0 && invocation >= 1 && attempts >= 1 && SafeName.valid(runId) && validCode(code) && payload.nonEmpty &&
              isSha256(sha) && entries.forall(SafeName.valid) && entries.distinct.length == entries.length
            then Right(())
            else Left("field out of range")
        yield LedgerRecord(WorkUnit(cell, dataset, arm), invocation, runId, phase, attempts, status, code, payload, sha, entries)
    catch case e: Exception => Left(s"unparsable ledger record: ${e.getClass.getSimpleName}")

/** One unit as the in-process scorer consumed it: the commit of the aggregating invocation whose payload fed the
  * scorer (finding F4). The aggregate record lists one per kept unit, so the owner can compare it, unit by unit,
  * with the first completed attempt.
  */
final case class ScorerInput(unit: WorkUnit, runId: String, phase: CommitPhase, payloadSha256: String):
  require(SafeName.valid(runId) && LedgerRecord.isSha256(payloadSha256), "run id and payload SHA-256")

/** Why the pilot runner refuses to start, to resume, or to continue. */
enum PilotRefusal(val message: String):
  case DirtyWorktree(detail: String) extends PilotRefusal(s"worktree has uncommitted changes: $detail")
  case RedirectedBuild(properties: Vector[String])
      extends PilotRefusal(s"dependency build redirected by ${properties.mkString(", ")}; a redirected build must not produce pilot evidence")
  case FrozenFileChanged(name: String, detail: String) extends PilotRefusal(s"frozen input '$name' is not as frozen: $detail")
  case StampMismatch(field: String)
      extends PilotRefusal(s"existing output was produced under a different stamp (field $field); refusing to resume")
  case ProjectionExceedsCeiling(projectedCoreHours: Double, ceilingCoreHours: Double)
      extends PilotRefusal(f"projected $projectedCoreHours%.3f core-hours exceeds the ceiling $ceilingCoreHours%.3f")
  case CpuCeilingReached(cpuSeconds: Double, ceilingCoreHours: Double)
      extends PilotRefusal(f"cumulative CPU $cpuSeconds%.1f s reached the hard ceiling of $ceilingCoreHours%.3f core-hours; stopped (resumable with an approved raised ceiling)")
  case TooFewDatasets(completed: Int, required: Int)
      extends PilotRefusal(s"partial pilot refused: $completed completed datasets per cell, at least $required required")
  case LedgerCorrupt(detail: String) extends PilotRefusal(s"ledger or data corrupt: $detail")
  case RunnerReused
      extends PilotRefusal("this PilotRunner instance has already run; a resume needs a new instance (one invocation = one run id = one instance)")
  case Interrupted extends PilotRefusal("the runner thread was interrupted; every worker was joined and the run stopped")
  case CostStateLost(detail: String)
      extends PilotRefusal(s"output holds progress or sealed blobs but the cumulative CPU state is unusable ($detail); refusing to resume, because the CPU guard would restart from zero")
  case SealFailed(detail: String) extends PilotRefusal(s"sealed store refused a write: $detail")
  case AccountingUncertain(runId: String)
      extends PilotRefusal(s"the CPU accounting of run $runId was never closed (crash, power loss or failed checkpoint); resuming needs the owner's OwnerAccountingRecovery for that run")
  case AccountingRecoveryMismatch(recoveryRunId: String, openRunId: String)
      extends PilotRefusal(s"the accounting recovery names run $recoveryRunId but the open accounting is $openRunId")
  case CeilingNotAuthorized(detail: String) extends PilotRefusal(s"CPU ceiling not authorized: $detail")
  case AccountingRecoveryInvalid(detail: String) extends PilotRefusal(s"the accounting recovery cannot be applied: $detail")
  case InvocationsExhausted(invocations: Int) extends PilotRefusal(s"cost.json already records $invocations invocations; no further ordinal can be written")
  case Failure(detail: String) extends PilotRefusal(detail)

/** Everything a pilot run depends on; a resume must reproduce it field for field. Ceilings are not stamped. */
final case class PilotStamp(fields: Vector[(String, String)]):
  require(fields.map(_._1).distinct.length == fields.length, "stamp keys must be distinct")

  def json: String = ujson.write(ujson.Obj.from(fields.map((k, v) => k -> ujson.Str(v))))

  def firstDifference(other: PilotStamp): Option[String] =
    val a = fields.toMap
    val b = other.fields.toMap
    (a.keySet ++ b.keySet).toVector.sorted.find(k => a.get(k) != b.get(k))

object PilotStamp:
  def parse(text: String): Either[String, PilotStamp] =
    try Right(PilotStamp(ujson.read(text).obj.iterator.map((k, v) => k -> v.str).toVector))
    catch case e: Exception => Left(s"unparsable stamp: ${e.getClass.getSimpleName}")
