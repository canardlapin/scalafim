package scalafim.phrfcmp.exec

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, DataInputStream, DataOutputStream, IOException}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.{Callable, ExecutionException, Executors, ThreadFactory, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger}
import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal

enum PilotOutcome(val decision: PartialDecision):
  /** Every planned dataset of every cell completed. */
  case Complete(d: PartialDecision) extends PilotOutcome(d)

  /** Soft stop with an accepted partial pilot (longest completed prefix, uniform reverse-index drop, D >= minimum). */
  case Partial(d: PartialDecision) extends PilotOutcome(d)

/** What one invocation returns, in process only. `rerunJobs` counts the jobs completed by earlier invocations that
  * this one recomputed so that every kept dataset reached the scorer in memory (decision D1). `scorerInputs` lists,
  * for every kept unit, the commit of this invocation whose payload the scorer consumed (finding F4); pass it with
  * `runId` to [[PilotAggregation.aggregateAndSeal]], which seals it. `contributions` holds, in the same order, the
  * scorer contribution sealed in each of those commits (S10); [[PilotAggregation.aggregateContributions]] assembles
  * the corpus from them and nothing else. Nothing here is written in plaintext.
  */
final case class PilotReport(
    outcome: PilotOutcome,
    totalCpuSeconds: Double,
    projectedCoreHours: Option[Double],
    invocation: Int,
    rerunJobs: Int,
    runId: String,
    scorerInputs: Vector[ScorerInput],
    contributions: Vector[UnitContribution] = Vector.empty
)

/** Raised inside a worker when the sealed store refuses a write; turned into a `Left` by [[PilotRunner.run]]. */
private final class RunAborted(val refusal: PilotRefusal) extends RuntimeException(refusal.message)

/** Raised at a seal when a stop flag is up (hard ceiling or another worker's crash): the commit is abandoned and the
  * unit stays incomplete. Never escapes a worker.
  */
private final class StopRequested extends RuntimeException("stop requested", null, false, false)

/** An owner ceiling raise as one invocation used it: durable in `cost.json` and sealed as `meta/ceiling-raise/<runId>`. */
private[exec] final case class CeilingRaiseRecord(invocation: Int, runId: String, hardCoreHours: Double, approver: String, reason: String):
  def json: ujson.Obj = ujson.Obj(
    "invocation" -> ujson.Num(invocation.toDouble),
    "run_id" -> ujson.Str(runId),
    "hard_core_hours" -> ujson.Num(hardCoreHours),
    "approver" -> ujson.Str(approver),
    "reason" -> ujson.Str(reason)
  )

/** An owner accounting recovery as one invocation applied it: the uncertain run and the CPU charged for it. */
private[exec] final case class RecoveryRecord(invocation: Int, uncertainRunId: String, chargedCpuSeconds: Double, approver: String, reason: String):
  def json: ujson.Obj = ujson.Obj(
    "invocation" -> ujson.Num(invocation.toDouble),
    "uncertain_run_id" -> ujson.Str(uncertainRunId),
    "charged_cpu_seconds" -> ujson.Num(chargedCpuSeconds),
    "approver" -> ujson.Str(approver),
    "reason" -> ujson.Str(reason)
  )

/** The runner's persistent plaintext state in `cost.json`: cumulative CPU (design 3.2 releases the total), the
  * number of invocations so far (the custody log records every runner launch, so the count is public too) and, when
  * any exist, the owner ceiling raises and accounting recoveries each invocation used (owner decisions, no results).
  */
private[exec] final case class CostState(
    cpuSecondsTotal: Double,
    invocations: Int,
    raises: Vector[CeilingRaiseRecord] = Vector.empty,
    recoveries: Vector[RecoveryRecord] = Vector.empty
):
  def json: String =
    val base = Vector[(String, ujson.Value)]("cpu_seconds_total" -> ujson.Num(cpuSecondsTotal), "invocations" -> ujson.Num(invocations.toDouble))
    val extra = Option.when(raises.nonEmpty)("ceiling_raises" -> ujson.Arr.from(raises.map(_.json))).toVector ++
      Option.when(recoveries.nonEmpty)("accounting_recoveries" -> ujson.Arr.from(recoveries.map(_.json))).toVector
    ujson.write(ujson.Obj.from(base ++ extra)) + "\n"

private[exec] object CostState:
  private val Base = Set("cpu_seconds_total", "invocations")
  private val Optional = Set("ceiling_raises", "accounting_recoveries")

  private def whole(v: ujson.Value, max: Int): Either[String, Int] =
    val d = v.num
    if d.isWhole && d >= 0 && d <= max then Right(d.toInt) else Left("count out of range")

  private def finite(v: ujson.Value): Either[String, Double] =
    val d = v.num
    if d.isNaN || d.isInfinite || d < 0.0 then Left("value out of range") else Right(d)

  private def sequence[A](xs: Vector[Either[String, A]]): Either[String, Vector[A]] =
    xs.collectFirst { case Left(e) => e }.toLeft(xs.collect { case Right(a) => a })

  /** Refuses every out-of-range number before narrowing; the invocation count stays below `Int.MaxValue`, so the
    * next invocation's ordinal can never overflow.
    */
  def parse(text: String): Either[String, CostState] =
    try
      val o = ujson.read(text).obj
      val keys = o.keySet.toSet
      if !Base.subsetOf(keys) || !keys.subsetOf(Base ++ Optional) then Left("unexpected keys")
      else
        for
          cpu <- finite(o("cpu_seconds_total")).left.map(_ => "CPU total out of range")
          inv <- whole(o("invocations"), Int.MaxValue - 1).left.map(_ => "invocation count out of range")
          raises <- sequence(o.get("ceiling_raises").fold(Vector.empty[ujson.Value])(_.arr.toVector).map { r =>
            for
              i <- whole(r("invocation"), inv)
              h <- finite(r("hard_core_hours"))
            yield CeilingRaiseRecord(i, r("run_id").str, h, r("approver").str, r("reason").str)
          })
          recoveries <- sequence(o.get("accounting_recoveries").fold(Vector.empty[ujson.Value])(_.arr.toVector).map { r =>
            for
              i <- whole(r("invocation"), inv)
              c <- finite(r("charged_cpu_seconds"))
            yield RecoveryRecord(i, r("uncertain_run_id").str, c, r("approver").str, r("reason").str)
          })
        yield CostState(cpu, inv, raises, recoveries)
    catch case e: Exception => Left(s"unparsable (${e.getClass.getSimpleName})")

/** The single payload blob of one unit commit: the terminal status and every blob the arm emitted, in name order.
  * When the arm emitted nothing (a refusal, a failure) the payload is the status placeholder alone, so every unit
  * commit seals the same three blobs (finding M6). Canonical: big-endian, `writeUTF` names, 8-byte lengths.
  */
object UnitPayload:
  val Tag: String = "phrf-cmp-s7-unit-payload-v1"

  def encode(status: UnitStatus, entries: Vector[RawBlob]): Array[Byte] =
    val bytes = new ByteArrayOutputStream()
    val o = new DataOutputStream(bytes)
    o.writeUTF(Tag)
    o.writeUTF(status.code)
    val sorted = entries.sortBy(_.name)
    o.writeInt(sorted.length)
    sorted.foreach { e =>
      o.writeUTF(e.name)
      o.writeLong(e.bytes.length.toLong)
      o.write(e.bytes)
    }
    o.flush()
    bytes.toByteArray

  /** Inverse of [[encode]] (owner side and tests): the status and the named entries. Every count and length is
    * checked against the bytes that remain before anything is narrowed or allocated, so a malformed payload is a
    * `Left`, never an exception.
    */
  def decode(payload: Array[Byte]): Either[String, (UnitStatus, Vector[(String, Array[Byte])])] =
    try
      val in = new DataInputStream(new ByteArrayInputStream(payload))
      if in.readUTF() != Tag then Left("not a unit payload")
      else
        UnitStatus.fromCode(in.readUTF()).toRight("unknown status").flatMap { status =>
          val n = in.readInt()
          if n < 0 || n > in.available() then Left("entry count out of range")
          else
            val entries = Vector.newBuilder[(String, Array[Byte])]
            var error: Option[String] = None
            var i = 0
            while error.isEmpty && i < n do
              val name = in.readUTF()
              val length = in.readLong()
              if length < 0L || length > in.available().toLong then error = Some("entry length out of range")
              else
                val data = new Array[Byte](length.toInt)
                in.readFully(data)
                entries += name -> data
              i += 1
            error.toLeft(entries.result()).flatMap(es => if in.available() != 0 then Left("trailing bytes") else Right((status, es)))
        }
    catch case e: IOException => Left(s"truncated payload (${e.getClass.getSimpleName})")

/** The pilot runner: stamp, resume, sealed output, CPU guard and round-robin scheduler (design 5.2, S7).
  *
  * Output layout under `output`:
  * {{{
  * stamp.json                       plaintext, releasable (hashes and versions, recipient fingerprint, store path hash)
  * cost.json                        {"cpu_seconds_total": x, "invocations": n}: totals only, releasable (design 3.2)
  * selection.json                   D, df and UCL factor only (manifest v1 records D)
  * progress/<cell>/dNNNN.{dispatched,done}(+sha) dispatch and completion markers, see [[PilotProgress]]
  * <store dir>/blobs/<hash>.enc    every payload, ledger record, timing record and meta blob, sealed at write time
  * }}}
  * Nothing result-bearing is ever written in plaintext: arms hand bytes to the scheduler in memory, and the scheduler
  * passes them straight to [[SealedStore.append]]. Sealed names: [[SealedNames]]; layout: format spec section 10.
  *
  * Resume (decision D1, "rerun-completed"): an invocation skips the jobs whose journal markers exist. The invocation
  * that reaches an accepted decision recomputes, once, every kept job an earlier invocation completed, so the
  * in-process scorer sees every kept dataset; the recomputation is sealed under `rerun/<runId>/...` and is never
  * scored by the owner (the first completed attempt is), and its CPU counts toward the guard.
  *
  * Concurrency: [[run]] returns only after every worker has stopped, and [[closeStore]] refuses while a run is in
  * progress. Pool threads are daemon threads and are joined before `run` returns.
  *
  * One instance is one invocation (finding F1): [[run]] is single-shot, so a second call is refused with
  * `PilotRefusal.RunnerReused`, and the run id is drawn inside `run`. A resume after a caught crash builds a new
  * instance, which gets a new run id and the next invocation ordinal.
  *
  * @param stamp   computed by [[StampBuilder.build]]; the runner adds `recipient_fp` and `sealed_store_path_sha256`
  *                (plaintext) and the full `sealed_store_path` (sealed `meta/stamp` only); a resume must match exactly
  * @param threads datasets run in parallel (one dataset single-threaded); results must not depend on it (F11)
  * @param guard   ceilings; deliberately not stamped, so an owner-authorized raise ([[OwnerCeilingRaise]]) can resume
  *                the same output; a ceiling above 60 core-hours cannot be built without one
  * @param runIds  draws the invocation's run id, once, inside [[run]]; it names every blob whose content may differ
  *                between invocations. Tests inject a fixed one
  * @param sweepStale called once at start: removes RAM scratch left by a dead runner (`RamScratch.sweepStale`, S6)
  * @param accountingRecovery the owner's decision to resume after an invocation whose accounting stayed open
  *                (`accounting.open` survived a crash, a power loss or a failed checkpoint); without it such a resume
  *                refuses with `AccountingUncertain`
  *
  * Accounting: before any metered work the runner durably writes `accounting.open` (its run id) and removes it only
  * after a successful final cost checkpoint, on every exit path. A marker that survives therefore means `cost.json`
  * may undercount, and a resume refuses until the owner charges the uncertain run explicitly.
  */
final class PilotRunner(
    plan: PilotPlan,
    output: Path,
    stamp: PilotStamp,
    store: SealedStore,
    root: PilotRoot,
    arms: ArmRunner,
    guard: CpuGuard = CpuGuard(),
    clock: CpuClock = CpuClock.system,
    threads: Int = 1,
    hook: CommitHook = CommitHook.none,
    log: String => Unit = _ => (),
    runIds: () => String = () => PilotRunner.freshRunId(),
    wallSeconds: () => Double = () => System.nanoTime() / 1e9,
    sweepStale: () => Unit = () => (),
    accountingRecovery: Option[OwnerAccountingRecovery] = None
):
  require(threads >= 1, "threads must be positive")
  require(!stamp.fields.exists((k, _) => PilotRunner.RunnerStampKeys.contains(k)), "recipient_fp and the store path keys are added by the runner")
  private val progress = new PilotProgress(output)
  private val retry = RetryPolicy(plan.maxRetries)
  private val cellById: Map[CellId, PilotCell] = plan.cells.map(c => c.id -> c).toMap
  private def costFile = output.resolve("cost.json")
  private def accountingFile = output.resolve(PilotRunner.AccountingOpenName)
  private def stampFile = output.resolve("stamp.json")
  private def selectionFile = output.resolve("selection.json")
  private val busy = new AtomicBoolean(false)
  private val used = new AtomicBoolean(false)
  @volatile private var invocationNo = 0
  @volatile private var currentRunId: Option[String] = None
  /** Writes the final cost and removes `accounting.open`; set once the marker is down. */
  @volatile private var closeAccounting: Option[() => Unit] = None

  private val storePath: String = store.dir.toAbsolutePath.normalize.toString

  /** The plaintext stamp this runner enforces: the caller's fields plus the owner key fingerprint and the SHA-256 of
    * the sealed store path (findings L2, F7), so a resume against another key or another store refuses without the
    * releasable `stamp.json` naming the custodian's directory.
    */
  val effectiveStamp: PilotStamp =
    PilotStamp(stamp.fields ++ Vector(
      "recipient_fp" -> store.recipientFingerprint,
      "sealed_store_path_sha256" -> Fs.sha256(storePath.getBytes(UTF_8))
    ))

  /** The sealed `meta/stamp`: the plaintext stamp plus the full store path. */
  val sealedStamp: PilotStamp = PilotStamp(effectiveStamp.fields :+ ("sealed_store_path" -> storePath))

  /** The 1-based invocation ordinal of this instance's [[run]] (0 before it). */
  def invocation: Int = invocationNo

  /** This invocation's run id, drawn when [[run]] starts. */
  def runId: String = currentRunId.getOrElse(throw new IllegalStateException("the run id is drawn when run() starts"))

  /** The persisted cost state. A missing `cost.json` is a fresh start only when the output holds no progress and no
    * sealed blob; otherwise, or when it is unreadable, the run refuses (finding M3), because the cumulative CPU guard
    * would silently restart from zero.
    */
  private def loadCost(): Either[PilotRefusal, CostState] =
    val hasWork = progress.exists || store.hasBlobs || Files.exists(accountingFile)
    if !Files.exists(costFile) then
      if hasWork then Left(PilotRefusal.CostStateLost("cost.json is missing")) else Right(CostState(0.0, 0))
    else
      val text =
        try Right(new String(Files.readAllBytes(costFile), UTF_8))
        catch case e: IOException => Left(s"unreadable (${e.getClass.getSimpleName})")
      text.flatMap(CostState.parse).left.map(PilotRefusal.CostStateLost(_))

  /** An open accounting marker from an earlier invocation needs the owner's recovery for exactly that run; the
    * charged CPU is added to the durable total and recorded. A recovery without an open marker is refused too.
    */
  private def reconcileAccounting(prior: CostState, inv: Int): Either[PilotRefusal, CostState] =
    val open =
      if !Files.exists(accountingFile) then Right(None)
      else
        val id = new String(Files.readAllBytes(accountingFile), UTF_8).trim
        if SafeName.valid(id) then Right(Some(id)) else Left(PilotRefusal.AccountingUncertain("unreadable"))
    open.flatMap {
      case None =>
        accountingRecovery.fold(Right(prior))(r => Left(PilotRefusal.AccountingRecoveryMismatch(r.runId, "none")))
      case Some(id) =>
        accountingRecovery match
          case None => Left(PilotRefusal.AccountingUncertain(id))
          case Some(r) if r.runId != id => Left(PilotRefusal.AccountingRecoveryMismatch(r.runId, id))
          case Some(r) =>
            log(s"PILOT_ACCOUNTING_RECOVERY,uncertain_run_id=$id,charged_cpu_s=${r.chargedCpuSeconds},approver=${r.approver},reason=${ujson.write(ujson.Str(r.reason))}")
            Right(prior.copy(
              cpuSecondsTotal = prior.cpuSecondsTotal + r.chargedCpuSeconds,
              recoveries = prior.recoveries :+ RecoveryRecord(inv, id, r.chargedCpuSeconds, r.approver, r.reason)
            ))
    }

  /** The runner's own admission of its guard: invariants re-checked, and a raise only for this very invocation. */
  private def admitGuard(inv: Int): Either[PilotRefusal, Unit] =
    if !CpuGuard.admissible(guard) then Left(PilotRefusal.CeilingNotAuthorized("the guard's ceiling is not authorized"))
    else
      guard.raise match
        case Some(r) if r.invocation != inv => Left(PilotRefusal.CeilingNotAuthorized(s"the raise authorizes invocation ${r.invocation}, not $inv"))
        case _ => Right(())

  private def checkStamp(): Either[PilotRefusal, Unit] =
    Files.createDirectories(output)
    if Files.exists(stampFile) then
      val text = new String(Files.readAllBytes(stampFile), UTF_8).trim
      if text == effectiveStamp.json then Right(())
      else
        PilotStamp.parse(text) match
          case Right(old) => Left(PilotRefusal.StampMismatch(old.firstDifference(effectiveStamp).getOrElse("stamp")))
          case Left(_) => Left(PilotRefusal.StampMismatch("stamp"))
    else if progress.exists || store.hasBlobs then Left(PilotRefusal.Failure("output holds data but no stamp.json; refusing to adopt it"))
    else
      Fs.writeAtomic(stampFile, effectiveStamp.json + "\n")
      Right(())

  /** One invocation. Single-shot: a second call on the same instance is refused (`RunnerReused`), because the run id
    * and the invocation ordinal belong to the instance and a repeat would seal differing records under one name.
    */
  def run(): Either[PilotRefusal, PilotReport] =
    if !busy.compareAndSet(false, true) then Left(PilotRefusal.Failure("the runner is busy (a run or a close is in progress)"))
    else if !used.compareAndSet(false, true) then
      busy.set(false)
      Left(PilotRefusal.RunnerReused)
    else
      try
        val rid = runIds()
        require(SafeName.valid(rid), "run id must be a safe name")
        currentRunId = Some(rid)
        accounted {
          for
            _ <- checkStamp()
            loaded <- loadCost()
            inv = loaded.invocations + 1
            prior <- reconcileAccounting(loaded, inv)
            _ <- admitGuard(inv)
            raise = guard.raise.map(r => CeilingRaiseRecord(inv, rid, r.hardCoreHours, r.approver, r.reason))
            state = prior.copy(invocations = inv, raises = prior.raises ++ raise)
            _ = open(state, rid)
            journal <- progress.recover(cellById.keySet)
            _ = try sweepStale() catch case NonFatal(_) => () // stale RAM scratch of a dead earlier runner (S6)
            _ <- sealMeta(rid, state)
            _ = raise.foreach(r => log(s"PILOT_CEILING_RAISE,run_id=$rid,invocation=$inv,hard_core_hours=${r.hardCoreHours},approver=${r.approver},reason=${ujson.write(ujson.Str(r.reason))}"))
            report <- schedule(journal, state)
          yield report
        }
      finally busy.set(false)

  /** Bumps the invocation in `cost.json`, then durably opens the accounting (`accounting.open` holds the run id) before
    * any metered work; until [[schedule]] takes over, closing rewrites the same state.
    */
  private def open(state: CostState, rid: String): Unit =
    invocationNo = state.invocations
    Fs.writeAtomic(costFile, state.json)
    Fs.writeAtomic(accountingFile, rid + "\n")
    closeAccounting = Some(() => { Fs.writeAtomic(costFile, state.json); closeMarker() })

  private def closeMarker(): Unit =
    Files.deleteIfExists(accountingFile): Unit
    Fs.fsyncDir(output.toAbsolutePath)

  /** Runs `body` and, on every exit, closes the accounting: the final cost checkpoint, then the marker removal. If
    * that fails the marker stays (the next resume refuses) and the failure propagates; an exception of `body` is
    * rethrown after a best-effort close.
    */
  private def accounted(body: => Either[PilotRefusal, PilotReport]): Either[PilotRefusal, PilotReport] =
    val result =
      try
        try body
        catch case e: RunAborted => Left(e.refusal)
      catch
        case t: Throwable =>
          try closeUninterrupted() catch case NonFatal(_) => ()
          throw t
    closeUninterrupted()
    result

  /** The final close with the caller's interrupt flag cleared (an interrupted thread cannot write through a
    * `FileChannel`) and restored afterwards, so an interrupted run still closes its accounting.
    */
  private def closeUninterrupted(): Unit =
    val wasInterrupted = Thread.interrupted()
    try closeAccounting.foreach(_())
    finally if wasInterrupted then Thread.currentThread().interrupt()

  /** Seals the root check (the ack hash, never the root) and the stamp. Both are deterministic: resuming under
    * another root makes the owner's reader refuse a differing duplicate of `meta/root-check`, and `meta/stamp` binds
    * the plaintext `stamp.json` to the sealed tree.
    */
  private def sealMeta(rid: String, state: CostState): Either[PilotRefusal, Unit] =
    val check = Fs.hex(MessageDigest.getInstance("SHA-256").digest(s"${root.value}\n".getBytes(UTF_8)))
    def mine[A](xs: Vector[A])(inv: A => Int): Option[A] = xs.find(inv(_) == state.invocations)
    for
      _ <- seal(SealedNames.RootCheck, check.getBytes(UTF_8))
      _ <- seal(SealedNames.Stamp, (sealedStamp.json + "\n").getBytes(UTF_8))
      _ <- mine(state.raises)(_.invocation).fold(Right(""))(r => seal(SealedNames.ceilingRaise(rid), (ujson.write(r.json) + "\n").getBytes(UTF_8)))
      _ <- mine(state.recoveries)(_.invocation).fold(Right(""))(r => seal(SealedNames.accountingRecovery(rid), (ujson.write(r.json) + "\n").getBytes(UTF_8)))
    yield ()

  /** Closes the sealed store (CLOSE record and SEALED receipt): once, after the last run and the aggregation. It
    * refuses while a run is in progress, so it never races the workers.
    */
  def closeStore(): Either[PilotRefusal, SealReceipt] =
    if !busy.compareAndSet(false, true) then Left(PilotRefusal.Failure("the runner is busy (a run or a close is in progress)"))
    else
      try store.close().left.map(e => PilotRefusal.SealFailed(e.message))
      finally busy.set(false)

  private def seal(name: String, bytes: Array[Byte]): Either[PilotRefusal, String] =
    store.append(name, bytes).left.map(e => PilotRefusal.SealFailed(e.message))

  /** One attempt of one unit, kept in memory until the unit commits; sealed in the unit's single timing record. */
  private final class AttemptLog(
      val attempt: Int,
      val status: UnitStatus,
      val code: String,
      val wall: Double,
      val child: Double,
      val timings: Vector[(String, Double)],
      val notes: Vector[String]
  )

  private def schedule(journal: JournalState, state: CostState): Either[PilotRefusal, PilotReport] =
    val inv = state.invocations
    val alreadyDone = journal.completed
    val meter = new CpuMeter(clock, state.cpuSecondsTotal)
    val hard = new AtomicBoolean(guard.check(meter.totalSeconds) == CpuGuard.State.HardStop)
    val crashed = new AtomicBoolean(false)
    val costLock = new Object
    val completed = java.util.concurrent.ConcurrentHashMap.newKeySet[Job]()
    alreadyDone.foreach(j => completed.add(j): Unit)
    val rerunDone = java.util.concurrent.ConcurrentHashMap.newKeySet[Job]()
    val consumed = new java.util.concurrent.ConcurrentHashMap[WorkUnit, (ScorerInput, UnitContribution)]()
    val order = Dispatch.order(plan.cells, plan.datasets)
    val (probe, rest) = Dispatch.waves(order, plan.probeDatasets)
    val mustFinish = Dispatch.mustFinish(order, alreadyDone, journal.dispatched)

    def checkpoint(): Unit = costLock.synchronized {
      val total = meter.totalSeconds
      Fs.writeAtomic(costFile, state.copy(cpuSecondsTotal = total).json)
      if guard.check(total) == CpuGuard.State.HardStop then hard.set(true)
    }
    // the schedule-wide final checkpoint: on every exit of the invocation, after every worker has stopped
    closeAccounting = Some(() => { checkpoint(); closeMarker() })

    def stopped: Boolean = hard.get() || crashed.get()

    /** Live hard-ceiling check (finding L3): trips the hard flag as soon as the cumulative CPU reaches the ceiling. */
    def stopNow(): Boolean =
      if !hard.get() && guard.check(meter.totalSeconds) == CpuGuard.State.HardStop then hard.set(true)
      stopped

    /** Persists the CPU spent so far on a crash path (re-review failure 2). A failure here is not lost either:
      * `accounting.open` stays down until the final checkpoint succeeds, so a resume after it refuses.
      */
    def checkpointOnCrash(): Unit =
      try checkpoint()
      catch case NonFatal(_) => ()

    /** A stop flag up at a seal abandons the commit (finding M4): nothing is written after a crash elsewhere. */
    def sealOrAbort(name: String, bytes: Array[Byte]): Unit =
      if stopped then throw new StopRequested
      seal(name, bytes).fold(r => throw new RunAborted(r), _ => ())

    def timingJson(u: WorkUnit, phase: CommitPhase, attempts: Vector[AttemptLog]): Array[Byte] =
      ujson
        .write(
          ujson.Obj(
            "cell" -> ujson.Str(u.cell.value),
            "dataset" -> ujson.Num(u.dataset.toDouble),
            "arm" -> ujson.Str(u.arm.value),
            "invocation" -> ujson.Num(inv.toDouble),
            "run_id" -> ujson.Str(runId),
            "phase" -> ujson.Str(phase.code),
            "attempts" -> ujson.Arr.from(attempts.map { a =>
              ujson.Obj(
                "attempt" -> ujson.Num(a.attempt.toDouble),
                "status" -> ujson.Str(a.status.code),
                "code" -> ujson.Str(a.code),
                "wall_s" -> ujson.Num(a.wall),
                "child_cpu_s" -> ujson.Num(a.child),
                "timings" -> ujson.Obj.from(a.timings.map((k, v) => k -> ujson.Num(v))),
                "notes" -> ujson.Arr.from(a.notes.map(ujson.Str(_)))
              )
            })
          )
        )
        .getBytes(UTF_8)

    /** Seals the unit's fixed blob set: payload, ledger record, timing record (finding M6). */
    def commit(job: Job, u: WorkUnit, phase: CommitPhase, status: UnitStatus, code: String, raws: Vector[RawBlob], attempts: Vector[AttemptLog]): Unit =
      val (payloadName, ledgerName, timingName) = phase match
        case CommitPhase.Scheduled => (SealedNames.data(u, runId), SealedNames.ledger(u, runId), SealedNames.timing(u, runId))
        case CommitPhase.Rerun => (SealedNames.rerunData(u, runId), SealedNames.rerunLedger(u, runId), SealedNames.rerunTiming(u, runId))
      val payload = UnitPayload.encode(status, raws)
      val payloadSha = Fs.sha256(payload)
      sealOrAbort(payloadName, payload)
      hook.at(CommitStage.DataSealed, job)
      val record = LedgerRecord(u, inv, runId, phase, attempts.length, status, code, payloadName, payloadSha, raws.map(_.name).sorted)
      sealOrAbort(ledgerName, (record.json + "\n").getBytes(UTF_8))
      hook.at(CommitStage.LedgerSealed, job)
      sealOrAbort(timingName, timingJson(u, phase, attempts))
      // only the committed attempt reaches the scorer: its contribution is the one sealed in this payload (S10)
      val contribution = raws.find(_.name == ScorerContribution.EntryName).map(_.bytes)
      consumed.put(u, (ScorerInput(u, runId, phase, payloadSha), new UnitContribution(u, contribution))): Unit

    /** Runs one unit to a commit; false when it stopped without committing. Retries seal nothing: their timings and
      * notes are buffered and sealed in the one timing record at commit.
      */
    def runUnit(job: Job, u: WorkUnit, phase: CommitPhase): Boolean =
      val attempts = ArrayBuffer.empty[AttemptLog]
      var finished = false
      var committed = false
      while !finished && !stopNow() do
        val attempt = attempts.length + 1
        val ctx = new ArmContext(u, attempt, root, () => stopNow())
        val t0 = wallSeconds()
        val result =
          try arms.run(ctx)
          catch
            case _: InterruptedException =>
              Thread.currentThread().interrupt()
              ArmResult.Failed("interrupted")
            case NonFatal(e) => ArmResult.Failed("executor_exception:" + e.getClass.getSimpleName.take(40).filter(_.isLetterOrDigit))
        val wall = wallSeconds() - t0
        meter.addChild(result.childCpu)
        if Thread.currentThread().isInterrupted then
          // the measured child CPU is in the meter; persist it (with the flag cleared, or the write would fail), restore
          // the flag and end the run
          crashed.set(true)
          val _ = Thread.interrupted()
          checkpointOnCrash()
          Thread.currentThread().interrupt()
          throw new RunAborted(PilotRefusal.Interrupted)
        if stopNow() then
          // the attempt ran into a stop (an arm that saw shouldAbort, or the ceiling crossed during it): discarded
          checkpoint()
          finished = true
        else
          try
            retry.decide(attempts.length, result) match
              case RetryPolicy.Step.Retry(code) =>
                attempts += AttemptLog(attempt, UnitStatus.Retried, safeCode(code), wall, result.childCpu, ctx.timings, ctx.notes)
              case RetryPolicy.Step.Commit(status, code) =>
                val c = safeCode(code)
                attempts += AttemptLog(attempt, status, c, wall, result.childCpu, ctx.timings, ctx.notes)
                commit(job, u, phase, status, c, ctx.emitted, attempts.toVector)
                finished = true
                committed = true
          catch
            case _: StopRequested => finished = true
            case t: Throwable =>
              crashed.set(true)
              checkpointOnCrash()
              throw t
          checkpoint()
      committed

    /** Durable dispatch marker before the job's first arm (blocker probe M2); a failure to write it is a crash. */
    def dispatch(job: Job): Unit =
      try progress.markDispatched(job)
      catch
        case t: Throwable =>
          crashed.set(true)
          throw t

    def runJob(job: Job, phase: CommitPhase): Unit =
      val cell = cellById(job.cell)
      var allCommitted = true
      try cell.arms.foreach(arm => allCommitted = allCommitted && !stopped && runUnit(job, WorkUnit(job.cell, job.dataset, arm), phase))
      finally arms.jobFinished(job)
      if allCommitted && !crashed.get() then
        phase match
          case CommitPhase.Rerun => rerunDone.add(job): Unit
          case CommitPhase.Scheduled =>
            try
              progress.markComplete(job, hook)
              completed.add(job): Unit
            catch
              case t: Throwable =>
                crashed.set(true)
                throw t

    /** Runs `jobs` on a pool of daemon threads and returns only after every worker has stopped (finding M4); the
      * first worker failure is rethrown after that. An interrupt of the calling thread (finding F5) sets the crash
      * flag, so workers stop at their next check, keeps joining every future, restores the interrupt flag and ends
      * the run with `PilotRefusal.Interrupted`.
      */
    def runWave(jobs: Vector[Job], phase: CommitPhase): Unit =
      val pending = if phase == CommitPhase.Scheduled then jobs.filterNot(completed.contains) else jobs
      if pending.nonEmpty then
        val next = new AtomicInteger(0)
        val n = math.min(threads, pending.size)
        val pool = Executors.newFixedThreadPool(n, PilotRunner.workerThreads)
        try
          val futures = (0 until n).map { _ =>
            pool.submit(new Callable[Unit]:
              def call(): Unit =
                var go = true
                while go do
                  val i = next.getAndIncrement()
                  if i >= pending.length || stopNow() then go = false
                  else
                    val job = pending(i)
                    phase match
                      case CommitPhase.Rerun => runJob(job, phase) // the soft stop does not apply: the scorer needs every kept job
                      case CommitPhase.Scheduled =>
                        guard.check(meter.totalSeconds) match
                          case CpuGuard.State.Ok =>
                            dispatch(job)
                            runJob(job, phase)
                          case CpuGuard.State.SoftStop =>
                            // no new datasets past the soft stop; jobs an earlier invocation dispatched and abandoned
                            // are finished (D2, M2)
                            if mustFinish.contains(job) then
                              dispatch(job)
                              runJob(job, phase)
                          case CpuGuard.State.HardStop =>
                            hard.set(true)
                            go = false
            )
          }
          var failure: Option[Throwable] = None
          var interrupted = false
          futures.foreach { f =>
            var joined = false
            while !joined do
              try
                f.get()
                joined = true
              catch
                case e: ExecutionException =>
                  crashed.set(true)
                  if failure.isEmpty then failure = Some(Option(e.getCause).getOrElse(e))
                  joined = true
                case _: InterruptedException =>
                  crashed.set(true) // workers stop at their next check; keep joining
                  interrupted = true
          }
          if interrupted || failure.nonEmpty then checkpointOnCrash() // every worker has stopped: the final CPU
          if interrupted then
            Thread.currentThread().interrupt()
            throw new RunAborted(PilotRefusal.Interrupted)
          failure.foreach(t => throw t)
        finally
          pool.shutdown()
          val wasInterrupted = Thread.interrupted() // every future is joined; let the pool drain uninterrupted
          val _ = pool.awaitTermination(1, TimeUnit.MINUTES)
          if wasInterrupted then Thread.currentThread().interrupt()

    def hardRefusal: PilotRefusal = PilotRefusal.CpuCeilingReached(meter.totalSeconds, guard.hardCoreHours)

    def completedSets(): Map[CellId, Set[Int]] =
      plan.cells.map(c => c.id -> (0 until plan.datasets).filter(d => completed.contains(Job(c.id, d))).toSet).toMap

    /** Accepts the decision, then recomputes once every kept job completed by an earlier invocation (D1). */
    def finish(projected: Option[Double]): Either[PilotRefusal, PilotReport] =
      val sets = completedSets()
      val full = sets.values.forall(_.size == plan.datasets)
      PartialPilot.decide(sets, if full then 2 else plan.minDatasets).flatMap { decision =>
        val reruns = order.filter(j => alreadyDone.contains(j) && decision.kept.get(j.cell).exists(_.contains(j.dataset)))
        runWave(reruns, CommitPhase.Rerun)
        checkpoint()
        if hard.get() then Left(hardRefusal)
        else if reruns.exists(j => !rerunDone.contains(j)) then Left(PilotRefusal.Failure("recomputation of earlier jobs did not complete"))
        else
          val keptUnits = for
            c <- plan.cells
            d <- decision.kept.getOrElse(c.id, Vector.empty)
            a <- c.arms
          yield WorkUnit(c.id, d, a)
          val inputs = keptUnits.flatMap(u => Option(consumed.get(u)))
          if inputs.length != keptUnits.length then Left(PilotRefusal.Failure("a kept unit has no commit in this invocation"))
          else
            Fs.writeAtomic(selectionFile, selectionJson(decision))
            val outcome = if full then PilotOutcome.Complete(decision) else PilotOutcome.Partial(decision)
            Right(PilotReport(outcome, meter.totalSeconds, projected, inv, reruns.length, runId, inputs.map(_._1), inputs.map(_._2)))
      }

    if hard.get() then
      checkpoint()
      Left(hardRefusal)
    else
      runWave(probe, CommitPhase.Scheduled)
      if hard.get() then
        checkpoint()
        Left(hardRefusal)
      else
        val doneJobs = completedSets().values.map(_.size).sum
        val projected = Option.when(doneJobs >= probe.length)(CpuGuard.projectCoreHours(meter.totalSeconds, doneJobs, order.length))
        projected.filter(_ > guard.hardCoreHours) match
          case Some(p) =>
            checkpoint()
            Left(PilotRefusal.ProjectionExceedsCeiling(p, guard.hardCoreHours))
          case None =>
            projected.foreach(p => log(f"PILOT_PROJECTION,core_hours=$p%.3f,ceiling=${guard.hardCoreHours}%.3f"))
            runWave(rest, CommitPhase.Scheduled) // past the soft stop only the jobs in `mustFinish` run
            checkpoint()
            if hard.get() then Left(hardRefusal) else finish(projected)

  private def safeCode(code: String): String =
    val cleaned = code.filter(c => c.isLetterOrDigit && c < 128 || c == '_' || c == '.' || c == ':' || c == '-').take(64)
    if LedgerRecord.validCode(cleaned) then cleaned else ""

  private def selectionJson(d: PartialDecision): String =
    ujson.write(ujson.Obj("D" -> ujson.Num(d.D.toDouble), "df" -> ujson.Num(d.df.toDouble), "ucl_factor" -> ujson.Str(f"${d.uclFactor}%.12f"))) + "\n"

object PilotRunner:
  /** Plaintext marker present while an invocation's CPU accounting is open (content: its run id). */
  val AccountingOpenName: String = "accounting.open"

  /** Stamp keys the runner adds itself; a caller's stamp must not carry them. */
  val RunnerStampKeys: Set[String] = Set("recipient_fp", "sealed_store_path", "sealed_store_path_sha256")

  def freshRunId(): String =
    val b = new Array[Byte](8)
    new java.security.SecureRandom().nextBytes(b)
    Fs.hex(b)

  private val workerCount = new AtomicInteger(0)

  /** Daemon worker threads: a worker can never keep the JVM alive after `run` returned or the main thread died. */
  private[exec] val workerThreads: ThreadFactory = r =>
    val t = new Thread(r, s"phrf-pilot-worker-${workerCount.incrementAndGet()}")
    t.setDaemon(true)
    t
