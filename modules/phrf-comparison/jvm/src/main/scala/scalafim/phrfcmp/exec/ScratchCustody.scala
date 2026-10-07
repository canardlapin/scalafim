package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.util.control.NonFatal

import scalafim.phrfcmp.run.{RamScratch, ScratchEvent, ScratchHook}

/** Receives non-result-bearing custody events: a short event token and short ASCII fields. */
trait CustodySink:
  def record(event: String, fields: Vector[(String, String)]): Unit

object CustodySink:
  val none: CustodySink = (_, _) => ()

/** Hash-chained, fsynced JSONL log of the runner's own custody events (scratch lifecycle). Each line carries the
  * previous line's hash, so truncation or rewriting is detectable by [[ChainedCustodyLog.verify]]; the head hash is
  * for the custodian to anchor, as with the session log. It holds no results: paths, device names, residue flags and
  * fixed tokens only.
  */
final class ChainedCustodyLog(path: Path, now: () => Long = () => System.currentTimeMillis() / 1000) extends CustodySink:
  private var seq = 0L
  private var head = ChainedCustodyLog.Genesis

  locally {
    if Files.exists(path) then
      ChainedCustodyLog.verify(path) match
        case Right((n, h)) => seq = n; head = h
        case Left(e) => throw new IllegalStateException(s"custody log not verifiable: $e")
  }

  def headHash: String = synchronized(head)

  def record(event: String, fields: Vector[(String, String)]): Unit = synchronized {
    val body = ujson.Obj(
      "seq" -> ujson.Num(seq.toDouble),
      "ts" -> ujson.Num(now().toDouble),
      "event" -> ujson.Str(event),
      "data" -> ujson.Obj.from(fields.map((k, v) => k -> ujson.Str(v)))
    )
    val hash = ChainedCustodyLog.link(head, ujson.write(body))
    val line = ujson.write(ujson.Obj("body" -> body, "prev" -> ujson.Str(head), "hash" -> ujson.Str(hash))) + "\n"
    Files.createDirectories(path.toAbsolutePath.getParent)
    val ch = java.nio.channels.FileChannel.open(path, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
    try
      val bb = java.nio.ByteBuffer.wrap(line.getBytes(UTF_8))
      while bb.hasRemaining do ch.write(bb): Unit
      ch.force(true)
    finally ch.close()
    seq += 1
    head = hash
  }

object ChainedCustodyLog:
  val Genesis: String = "0" * 64
  private[exec] def link(prev: String, bodyJson: String): String = Fs.sha256((prev + "\n" + bodyJson).getBytes(UTF_8))

  /** Entry count and head hash if the whole chain verifies. */
  def verify(path: Path): Either[String, (Long, String)] =
    val lines = new String(Files.readAllBytes(path), UTF_8).linesIterator.filter(_.nonEmpty).toVector
    lines.zipWithIndex.foldLeft[Either[String, (Long, String)]](Right((0L, Genesis))) { (acc, li) =>
      acc.flatMap { (n, prev) =>
        try
          val o = ujson.read(li._1).obj
          val body = o("body")
          if o("prev").str != prev then Left(s"broken chain at ${li._2}")
          else if o("hash").str != link(prev, ujson.write(body)) then Left(s"bad hash at ${li._2}")
          else if body("seq").num.toLong != n then Left(s"bad sequence at ${li._2}")
          else Right((n + 1, o("hash").str))
        catch case NonFatal(_) => Left(s"unparsable entry ${li._2}")
      }
    }

/** Maps S6 scratch events onto the custody sink, including `StaleSwept`. Never throws. `Wiped` carries only the
  * residue count: file and byte counts of the wiped GLMsingle output are dropped, because the size of a result file
  * is metadata under the peeking ban.
  */
final class ScratchCustodyHook(sink: CustodySink) extends ScratchHook:
  private def clean(s: String): String = s.map(c => if c >= ' ' && c < 127.toChar then c else '?').take(200)

  def event(e: ScratchEvent): Unit =
    try
      val (name, fields): (String, Vector[(String, String)]) = e match
        case ScratchEvent.Created(k, p)            => ("scratch-created", Vector("kind" -> k, "path" -> p))
        case ScratchEvent.DeviceAttached(d, s)     => ("scratch-device-attached", Vector("device" -> d, "sectors" -> s.toString))
        case ScratchEvent.Formatted(d, f)          => ("scratch-formatted", Vector("device" -> d, "filesystem" -> f))
        case ScratchEvent.Mounted(p, o)            => ("scratch-mounted", Vector("path" -> p, "options" -> o))
        case ScratchEvent.IndexingGuard(n)         => ("scratch-indexing-guard", Vector("note" -> n))
        case ScratchEvent.ChildStarted(pid, pgid)  => ("scratch-child-started", Vector("pid" -> pid.toString, "pgid" -> pgid.toString))
        case ScratchEvent.ProcessGroupKilled(g, s) => ("scratch-group-killed", Vector("pgid" -> g.toString, "survivors" -> s.toString))
        case ScratchEvent.Wiped(_, _, r)           => ("scratch-wiped", Vector("residue" -> r.toString))
        case ScratchEvent.Unmounted(p)             => ("scratch-unmounted", Vector("path" -> p))
        case ScratchEvent.Detached(d)              => ("scratch-detached", Vector("device" -> d))
        case ScratchEvent.Removed(p)               => ("scratch-removed", Vector("path" -> p))
        case ScratchEvent.Verified(p, d)           => ("scratch-verified", Vector("path_gone" -> p.toString, "device_gone" -> d.toString))
        case ScratchEvent.StepFailed(s, d)         => ("scratch-step-failed", Vector("step" -> s, "detail" -> d))
        case ScratchEvent.StaleSwept(p, d, o, r)   => ("scratch-stale-swept", Vector("path" -> p, "device" -> d.getOrElse("-"), "owner_pid" -> o.toString, "outcome" -> r))
      sink.record(name, fields.map((k, v) => k -> clean(v)))
    catch case NonFatal(_) => ()

object ScratchCustody:
  /** The start-of-run sweep (S6 `RamScratch.sweepStale`) reporting `StaleSwept` through the custody hook. */
  def sweepAtStart(sink: CustodySink): () => Unit =
    val hook = new ScratchCustodyHook(sink)
    () => { RamScratch.sweepStale(hook); () }
