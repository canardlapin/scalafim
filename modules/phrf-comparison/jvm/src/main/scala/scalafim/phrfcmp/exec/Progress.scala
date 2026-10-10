package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

/** Commit stages at which tests may inject a simulated crash. */
enum CommitStage:
  case DataSealed, LedgerSealed, MarkerWritten, MarkerSealed

/** Fault-injection seam for the commit sequence; production uses [[CommitHook.none]]. */
trait CommitHook:
  def at(stage: CommitStage, job: Job): Unit

object CommitHook:
  val none: CommitHook = (_, _) => ()

/** What the resume journal holds: the completed jobs and the jobs some invocation dispatched. */
final case class JournalState(completed: Set[Job], dispatched: Set[Job])

/** The resume journal: plaintext constant-content markers per (cell, dataset) job, and nothing else.
  *
  *   - `dNNNN.dispatched` is written, durably, before the first arm of the job runs. A resume past the soft stop
  *     finishes every dispatched job that has not completed: the runner "finishes in-flight datasets" (design 5.2)
  *     even when the interruption hit a job at or above the highest completed index (blocker probe M2).
  *   - `dNNNN.done` says that every arm of the job reached a terminal state, whatever that state was.
  *
  * No marker carries a status, code, attempt count, hash, size or timing. Their modification times do record when a
  * job was dispatched and completed, so the output directory must sit inside the custodian work directory, under the
  * no-peeking rule (runbook section 3). All results (payloads, ledger records, timings) live only in the sealed
  * store, which the runner cannot read back. A marker counts only when its `.sha256` exists too; the done marker is
  * written after every blob of the job was sealed and verified.
  *
  * The journal stays under decision D1: without it every resume would recompute every job (the runner cannot read
  * the store), multiplying the CPU by the number of resumes, and the longest-completed-prefix rule (D2) and the
  * choice of the jobs to recompute once at the end both need the completed set.
  * {{{
  * progress/<cell>/dNNNN.dispatched + .sha256
  * progress/<cell>/dNNNN.done + .sha256
  * }}}
  */
final class PilotProgress(root: Path):
  private val Content = "complete\n"
  private val DispatchedContent = "dispatched\n"
  private def dir(job: Job): Path = root.resolve("progress").resolve(job.cell.value)
  private def file(job: Job): Path = dir(job).resolve(SealedNames.dataset(job.dataset) + ".done")
  private def dispatchFile(job: Job): Path = dir(job).resolve(SealedNames.dataset(job.dataset) + ".dispatched")

  def exists: Boolean = Files.exists(root.resolve("progress"))

  /** Deletes crash debris (temp files, a marker without its sha) and returns the completed and the dispatched jobs.
    * A sha that disagrees with its marker, or an unrecognised file, refuses.
    */
  def recover(known: Set[CellId]): Either[PilotRefusal, JournalState] =
    Fs.listFiles(root.resolve("progress")).filter(_.getFileName.toString.endsWith(".tmp")).foreach(p => Files.deleteIfExists(p): Unit)
    Fs.listFiles(root.resolve("progress")).foreach { p =>
      val name = p.getFileName.toString
      if name.endsWith(".sha256") && !Files.exists(p.resolveSibling(name.stripSuffix(".sha256"))) then Files.deleteIfExists(p): Unit
      else if (name.endsWith(".done") || name.endsWith(".dispatched")) && !Files.exists(Fs.shaPath(p)) then Files.deleteIfExists(p): Unit
    }
    val base = root.resolve("progress")
    val markers = Fs.listFiles(base).filterNot(_.getFileName.toString.endsWith(".sha256"))
    val done = """d(\d{4})\.done""".r
    val dispatched = """d(\d{4})\.dispatched""".r
    def valid(p: Path, content: String): Boolean = new String(Files.readAllBytes(p), UTF_8) == content && Fs.shaState(p) == Fs.ShaState.Valid
    val parsed = markers.map { p =>
      val rel = base.relativize(p)
      val cell = Option(rel.getParent).map(_.toString).flatMap(c => CellId.parse(c).toOption).filter(known.contains)
      (rel.getFileName.toString, cell) match
        case (done(n), Some(c)) if rel.getNameCount == 2 && valid(p, Content) => Right(Left(Job(c, n.toInt)))
        case (dispatched(n), Some(c)) if rel.getNameCount == 2 && valid(p, DispatchedContent) => Right(Right(Job(c, n.toInt)))
        case _ => Left(PilotRefusal.LedgerCorrupt(s"unrecognised or inconsistent journal entry ${root.relativize(p)}"))
    }
    parsed.collectFirst { case Left(e) => e }.toLeft {
      val jobs = parsed.collect { case Right(j) => j }
      JournalState(jobs.collect { case Left(j) => j }.toSet, jobs.collect { case Right(j) => j }.toSet)
    }

  /** Records, durably, that `job` is about to run (marker, then its sha); idempotent. A crash between the two leaves
    * an orphan that `recover` deletes: the job had not started.
    */
  def markDispatched(job: Job): Unit =
    val f = dispatchFile(job)
    if Fs.shaState(f) != Fs.ShaState.Valid then
      Fs.writeAtomic(f, DispatchedContent)
      Fs.writeSha(f)

  /** Marker data first, its sha after; a crash between the two leaves an orphan that `recover` deletes. */
  def markComplete(job: Job, hook: CommitHook): Unit =
    val f = file(job)
    Fs.writeAtomic(f, Content)
    hook.at(CommitStage.MarkerWritten, job)
    Fs.writeSha(f)
    hook.at(CommitStage.MarkerSealed, job)
