package scalafim.phrfcmp.exec

import scala.collection.mutable.ArrayBuffer

/** The pilot root64 as received over the root pipe. It is held in memory only: no `toString`, no serializer, no
  * codec. Arms read `value` solely to derive seeds (format spec section 8).
  */
final class PilotRoot(private val bits: Long):
  def value: Long = bits
  override def toString: String = "PilotRoot(redacted)"
  override def hashCode: Int = 0
  override def equals(other: Any): Boolean = other.asInstanceOf[Matchable] match
    case o: PilotRoot => o.bits == bits
    case _ => false

/** One raw result blob before sealing (design 3.2.1): bytes are package-private and there is no `toString`. */
final class RawBlob private[exec] (val name: String, private[exec] val bytes: Array[Byte]):
  override def toString: String = "RawBlob(redacted)"

/** What an arm reports for one attempt. `childCpuSeconds` is the CPU of child processes (for GLMsingle: the sidecar
  * `cpu_s` or the polled process-tree CPU); it is added to the cumulative CPU guard. Codes are short status
  * tokens, never values.
  */
enum ArmResult:
  case Done(childCpuSeconds: Double = 0.0)
  case Refused(code: String, childCpuSeconds: Double = 0.0)
  case Failed(code: String, childCpuSeconds: Double = 0.0)

  def childCpu: Double = this match
    case Done(c) => c
    case Refused(_, c) => c
    case Failed(_, c) => c

/** Per-attempt handle given to an arm. Result bytes are handed over with [[emit]] and held in memory until the
  * scheduler seals them; nothing is written to disk in plaintext. Blobs emitted by an attempt that does not end in
  * a terminal status are dropped. Blob contents must be a deterministic function of the unit and the root seed
  * (format spec section 5, duplicate rule); `attempt` is diagnostic only and must not influence them.
  */
final class ArmContext(val unit: WorkUnit, val attempt: Int, val root: PilotRoot, abortRequested: () => Boolean):
  private val blobs = ArrayBuffer.empty[RawBlob]

  /** True once the hard CPU ceiling is reached (checked live on every call) or another unit crashed; a long arm
    * should poll it and return promptly. The attempt in progress is then discarded, whatever it returns.
    */
  def shouldAbort: Boolean = abortRequested()

  def emit(name: String, bytes: Array[Byte]): Unit = synchronized {
    require(SafeName.valid(name), "unsafe blob name")
    require(name != ScorerContribution.EntryName, "the scorer contribution is handed over with contribute")
    require(!blobs.exists(_.name == name), "blob names are distinct within an attempt")
    blobs += new RawBlob(name, bytes.clone()): Unit
  }

  /** This attempt's truth-free scorer contribution (S10): the canonical bytes from which the corpus assembler builds
    * the unit's share of the scorer's corpus. At most once per attempt. It is sealed inside the unit payload under
    * [[ScorerContribution.EntryName]], and it reaches the in-process assembler only if this attempt commits: a
    * retried or abandoned attempt's contribution is discarded with the attempt. Like every emitted blob it must be
    * deterministic in the unit and the root, since the owner rebuilds the corpus from the first completed attempts.
    */
  def contribute(bytes: Array[Byte]): Unit = synchronized {
    require(!blobs.exists(_.name == ScorerContribution.EntryName), "one scorer contribution per attempt")
    blobs += new RawBlob(ScorerContribution.EntryName, bytes.clone()): Unit
  }

  private[exec] def emitted: Vector[RawBlob] = synchronized(blobs.toVector.sortBy(_.name))

  private val noteBuf = ArrayBuffer.empty[String]
  private val timingBuf = ArrayBuffer.empty[(String, Double)]

  /** Result-adjacent free text of this attempt (a child's output tail, a refusal message). It is sealed in the
    * attempt-unique timing record only, never in a deterministic blob, never logged, and never in the ledger status.
    */
  def note(text: String): Unit = synchronized { noteBuf += text: Unit }

  /** A named measurement of this attempt (for example the GLMsingle timing CPU). Sealed in the timing record only. */
  def recordTiming(name: String, seconds: Double): Unit = synchronized {
    require(SafeName.valid(name), "unsafe timing name")
    timingBuf += name -> seconds: Unit
  }

  private[exec] def notes: Vector[String] = synchronized(noteBuf.toVector)
  private[exec] def timings: Vector[(String, Double)] = synchronized(timingBuf.toVector)

/** The reserved unit-payload entry that holds a unit's scorer contribution (S10, [[ArmContext.contribute]]). */
object ScorerContribution:
  val EntryName: String = "scorer-contribution"

/** The interface the scheduler calls for one (cell, dataset, arm) attempt. Implemented by the condition runner
  * (S3), the native trial runner (S4), the PHRF trial runner (S5) and the GLMsingle bridge (S6). Its emitted blobs
  * must be deterministic in `ctx.unit` and `ctx.root` alone: they must not depend on `ctx.attempt`, on the run id,
  * on the invocation or on timing. The owner compares `payload_sha256` across a unit's commits, and a difference is
  * recorded as nondeterminism for that unit. It must be thread-safe: datasets run in parallel, one dataset
  * single-threaded.
  */
trait ArmRunner:
  def run(ctx: ArmContext): ArmResult

  /** Called once per job after its last arm ran (or the run stopped), so per-dataset caches can be released. */
  def jobFinished(job: Job): Unit = ()

/** Logical names inside the sealed store (format spec section 4; runner layout in the format spec section 10).
  *
  * Only two names are deterministic: the root check and the stamp, which the runner itself makes identical across
  * invocations (decision F2). Every unit record (payload, ledger, timing), recomputed data and every aggregate
  * carries the invocation's run id, so a resumed run never seals a duplicate of a unit's data at all. The owner
  * detects nondeterminism by comparing `payload_sha256` across a unit's ledger records; a mismatch is recorded per
  * unit and never invalidates the store.
  *
  * Every unit commit seals exactly three blobs (payload, ledger record, timing record), whatever its status and
  * however many attempts it took.
  */
object SealedNames:
  def dataset(d: Int): String = f"d$d%04d"
  private def unitPath(u: WorkUnit): String = s"${u.cell.value}/${dataset(u.dataset)}/${u.arm.value}"

  /** The payload of a scheduled unit commit, whatever its status (status placeholder plus whatever the arm emitted). */
  def data(u: WorkUnit, runId: String): String = s"data/${unitPath(u)}/$runId"

  def ledger(u: WorkUnit, runId: String): String = s"ledger/${unitPath(u)}/$runId"

  /** One timing record per unit commit, holding every attempt of the invocation (retries are not separate blobs). */
  def timing(u: WorkUnit, runId: String): String = s"timing/${unitPath(u)}/$runId"

  /** Recomputation of an earlier invocation's completed job in the final aggregating invocation (decision D1). */
  def rerunData(u: WorkUnit, runId: String): String = s"rerun/$runId/data/${unitPath(u)}"
  def rerunLedger(u: WorkUnit, runId: String): String = s"rerun/$runId/ledger/${unitPath(u)}"
  def rerunTiming(u: WorkUnit, runId: String): String = s"rerun/$runId/timing/${unitPath(u)}"

  /** Aggregation by-products of one invocation: the S8 diagnostics and the record binding them to the corpus. */
  def aggregateDiagnostics(runId: String): String = s"aggregate/$runId/diagnostics"
  def aggregateRecord(runId: String): String = s"aggregate/$runId/record"

  val RootCheck: String = "meta/root-check"

  /** The owner ceiling raise and the owner accounting recovery an invocation used, when any. */
  def ceilingRaise(runId: String): String = s"meta/ceiling-raise/$runId"
  def accountingRecovery(runId: String): String = s"meta/accounting-recovery/$runId"
  val Stamp: String = "meta/stamp"
