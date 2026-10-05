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

/** The resume journal: one plaintext marker per completed (cell, dataset) job, and nothing else. A marker says
  * only that every arm of the job reached a terminal state, whatever that state was; its content carries no status,
  * code, attempt count, hash, size or timing. Its modification time does record when the job completed, so the output
  * directory must sit inside the custodian work directory, under the no-peeking rule (runbook section 3). All results
  * (payloads, ledger records, timings) live only in the sealed store, which the runner cannot read back. A job is
  * complete only when its marker and its `.sha256` both exist, written after every blob of the job was sealed and
  * verified.
  *
  * The journal stays under decision D1: without it every resume would recompute every job (the runner cannot read
  * the store), multiplying the CPU by the number of resumes, and the longest-completed-prefix rule (D2) and the
  * choice of the jobs to recompute once at the end both need the completed set.
  * {{{
  * progress/<cell>/dNNNN.done + .sha256
  * }}}
  */
final class PilotProgress(root: Path):
  private val Content = "complete\n"
  private def file(job: Job): Path = root.resolve("progress").resolve(job.cell.value).resolve(SealedNames.dataset(job.dataset) + ".done")

  def exists: Boolean = Files.exists(root.resolve("progress"))

  /** Deletes crash debris (temp files, a marker without its sha) and returns the completed jobs. A sha that
    * disagrees with its marker, or an unrecognised file, refuses.
    */
  def recover(known: Set[CellId]): Either[PilotRefusal, Set[Job]] =
    Fs.listFiles(root.resolve("progress")).filter(_.getFileName.toString.endsWith(".tmp")).foreach(p => Files.deleteIfExists(p): Unit)
    Fs.listFiles(root.resolve("progress")).foreach { p =>
      val name = p.getFileName.toString
      if name.endsWith(".sha256") && !Files.exists(p.resolveSibling(name.stripSuffix(".sha256"))) then Files.deleteIfExists(p): Unit
      else if name.endsWith(".done") && !Files.exists(Fs.shaPath(p)) then Files.deleteIfExists(p): Unit
    }
    val base = root.resolve("progress")
    val markers = Fs.listFiles(base).filter(_.getFileName.toString.endsWith(".done"))
    val ds = """d(\d{4})\.done""".r
    val parsed = markers.map { p =>
      val rel = base.relativize(p)
      val cell = Option(rel.getParent).map(_.toString).flatMap(c => CellId.parse(c).toOption).filter(known.contains)
      (rel.getFileName.toString, cell) match
        case (ds(n), Some(c)) if rel.getNameCount == 2 && new String(Files.readAllBytes(p), UTF_8) == Content && Fs.shaState(p) == Fs.ShaState.Valid =>
          Right(Job(c, n.toInt))
        case _ => Left(PilotRefusal.LedgerCorrupt(s"unrecognised or inconsistent journal entry ${root.relativize(p)}"))
    }
    parsed.collectFirst { case Left(e) => e }.toLeft(parsed.collect { case Right(j) => j }.toSet)

  /** Marker data first, its sha after; a crash between the two leaves an orphan that `recover` deletes. */
  def markComplete(job: Job, hook: CommitHook): Unit =
    val f = file(job)
    Fs.writeAtomic(f, Content)
    hook.at(CommitStage.MarkerWritten, job)
    Fs.writeSha(f)
    hook.at(CommitStage.MarkerSealed, job)
