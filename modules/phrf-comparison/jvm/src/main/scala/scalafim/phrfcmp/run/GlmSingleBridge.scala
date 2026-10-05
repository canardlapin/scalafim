package scalafim.phrfcmp.run

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import java.util.concurrent.TimeUnit

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*
import scala.util.Try
import scala.util.control.NonFatal

import scalafim.phrfcmp.exec.ChildEnvironment
import scalafim.phrfcmp.ingest.{NpyArray, Npz}
import scalafim.phrfcmp.score.{TrialEstimate, VoxelOutcome}

// ------------------------------------------------------------------------------------------------------------
// Slice S6 (Scala side): the GLMsingle bridge (design sections 1.1, 2.2, 2.3, 3.2, 4, 5.2; owner decision
// 2026-10-03: RAM-backed scratch).
//
// The bridge runs `tools/phrf-comparison/glmsingle/from_generator.py` (v0-frozen) as a child process whose working
// directory, output directory, TMPDIR, HOME and every Python, numpy, numba, matplotlib and joblib cache are inside
// a RAM-backed scratch created for the invocation. The result-bearing output (the STORED `.glmsingle.npz` and the
// sidecar) is read into memory, the scratch is wiped and detached, and the bridge returns `VoxelOutcome`s with the
// E-trial in INPUT trial order. GLMsingle's result-bearing output never exists on a persistent file system.
//
// Input binding: GLMsingle does its own preprocessing (no whitening, no common nuisance model), so the bridge does
// NOT bind to `CommonPreparation`. It binds to the dataset: the SHA-256 of the generator `.npz` is computed here,
// optionally compared with the caller's expected hash (S7 passes `BoundDataset.inputSha256`), and compared with the
// `input.npz_sha256` the Python side recorded in the sidecar. The frozen script hash and the pinned GLMsingle
// commit are checked the same way.
// ------------------------------------------------------------------------------------------------------------

/** One non-result-bearing lifecycle event of a scratch: paths, device names, counts, never contents. Consumed by
  * S7's custody log through a [[ScratchHook]].
  */
enum ScratchEvent:
  case Created(kind: String, path: String)
  case DeviceAttached(device: String, sectors: Long)
  case Formatted(device: String, filesystem: String)
  case Mounted(path: String, options: String)
  case IndexingGuard(note: String)
  case ChildStarted(pid: Long, pgid: Long)
  case ProcessGroupKilled(pgid: Long, survivorsAfterKill: Int)
  case Wiped(files: Int, bytes: Long, residueAfterWipe: Int)
  case Unmounted(path: String)
  case Detached(device: String)
  case Removed(path: String)
  case Verified(pathGone: Boolean, deviceGone: Boolean)
  case StepFailed(step: String, detail: String)
  case StaleSwept(path: String, device: Option[String], ownerPid: Long, outcome: String)

  def describe: String = this match
    case Created(k, p)         => s"scratch created kind=$k path=$p"
    case DeviceAttached(d, s)  => s"ram device attached $d sectors=$s"
    case Formatted(d, f)       => s"formatted $d as $f"
    case Mounted(p, o)         => s"mounted $p options=$o"
    case IndexingGuard(n)      => s"indexing guard: $n"
    case ChildStarted(p, g)    => s"child started pid=$p pgid=$g"
    case ProcessGroupKilled(g, s) => s"process group $g killed, survivors=$s"
    case Wiped(f, b, r)        => s"wiped files=$f bytes=$b residue=$r"
    case Unmounted(p)          => s"unmounted $p"
    case Detached(d)           => s"detached $d"
    case Removed(p)            => s"removed $p"
    case Verified(p, d)        => s"verified pathGone=$p deviceGone=$d"
    case StepFailed(s, d)      => s"step failed $s: $d"
    case StaleSwept(p, d, o, r) => s"stale scratch $p device=${d.getOrElse("-")} owner=$o: $r"

/** Receives scratch events. Must not throw; exceptions from a hook are swallowed so cleanup always proceeds. */
trait ScratchHook:
  def event(e: ScratchEvent): Unit

object ScratchHook:
  val none: ScratchHook = _ => ()

/** Why the bridge returned no estimate.
  *
  * Custody: `InputRefusedByPython.tail`, `ChildFailed.tail`, and the `message` of `Timeout` and `ChildFailed` are
  * RESULT-ADJACENT (they carry the child's output tail and its CPU); they go only to the sealed ledger, never to
  * the log, a hook, an error channel or the whitelist. The other cases carry only fixed text, paths, hashes and
  * type names (the JSON parser message is deliberately not carried, since it can quote sidecar bytes).
  */
enum GlmSingleRefusal:
  case NoRamScratch(reason: String)
  case ScratchSetupFailed(step: String, detail: String)
  case LauncherMissing(what: String)
  case ScriptHashMismatch(expected: String, found: String)
  case InputUnreadable(path: String, detail: String)
  case InputHashMismatch(expected: String, found: String)
  /** `from_generator.py` exit 2: it refused the dataset (typed Python `Refusal`). */
  case InputRefusedByPython(tail: String)
  /** Exit 3 or any other non-zero exit. */
  case ChildFailed(exitCode: Int, tail: String)
  case Timeout(seconds: Double, cpuSecondsObserved: Double)
  case LaunchFailed(detail: String)
  case MissingOutput(file: String)
  case OutputUnreadable(file: String, detail: String)
  case OutputInvalid(detail: String)
  case ProvenanceMismatch(what: String, expected: String, found: String)
  /** Cleanup left residue or could not complete. `prior` is the refusal the run would otherwise have returned. */
  case ScratchResidue(step: String, detail: String, prior: Option[String])
  /** The calling thread was interrupted; the child was killed and the scratch released. The flag is restored. */
  case Interrupted

  def message: String = this match
    case NoRamScratch(r)           => s"no RAM-backed scratch available: $r"
    case ScratchSetupFailed(s, d)  => s"scratch setup failed at $s: $d"
    case LauncherMissing(w)        => s"required launcher missing: $w"
    case ScriptHashMismatch(e, f)  => s"from_generator.py hash $f != expected $e"
    case InputUnreadable(p, d)     => s"cannot read $p: $d"
    case InputHashMismatch(e, f)   => s"dataset sha256 $f != expected $e"
    case InputRefusedByPython(t)   => s"from_generator.py refused the dataset: $t"
    case ChildFailed(c, t)         => s"GLMsingle child exited $c: $t"
    case Timeout(s, c)             => f"GLMsingle exceeded the $s%.1f s timeout (child CPU observed $c%.2f s)"
    case LaunchFailed(d)           => s"could not launch the child: $d"
    case MissingOutput(f)          => s"GLMsingle produced no $f"
    case OutputUnreadable(f, d)    => s"cannot read GLMsingle output $f: $d"
    case OutputInvalid(d)          => s"invalid GLMsingle output: $d"
    case ProvenanceMismatch(w, e, f) => s"$w is $f, expected $e"
    case Interrupted               => "interrupted"
    case ScratchResidue(s, d, p)   => s"scratch cleanup failed at $s: $d" + p.fold("")(x => s" (run result discarded: $x)")

  /** Per design 2.3: GLMsingle refusal, exit, timeout, missing or invalid output is `Failed` for every voxel. */
  def asVoxels(nVoxels: Int): Vector[VoxelOutcome[TrialEstimate]] = Vector.fill(nVoxels)(VoxelOutcome.Failed)

/** What the caller supplies per dataset. `datasetNpz` is the generator `<CELL>__dNNNN.npz` (manifest alongside). */
final case class GlmSingleInputs(datasetNpz: Path, expectedInputSha256: Option[String]):
  override def toString: String = s"GlmSingleInputs(${datasetNpz.getFileName})"

/** Frozen pins of the v0 set (receipt `docs/verification/phrf-cmp-s6-20261001.md`). */
object GlmSinglePins:
  val FromGeneratorSha256 = "c5a8c0dcdbf9320978b0e794f8b4ff04524c6c0ef9e3731eb9fdcf9506eb89ac"
  val GlmSingleCommit = "1ab54a65edd3ea41a6133d4b4ecb78a9c7296684"

/** Thread pinning and cache confinement for the child (design 5.2 and the RAM-scratch decision). */
object GlmSingleEnv:
  val Threads: Int = 1
  val ThreadPins: Map[String, String] = Map(
    "OMP_NUM_THREADS" -> "1",
    "MKL_NUM_THREADS" -> "1",
    "OPENBLAS_NUM_THREADS" -> "1",
    "NUMEXPR_NUM_THREADS" -> "1",
    "VECLIB_MAXIMUM_THREADS" -> "1",
    "NUMBA_NUM_THREADS" -> "1"
  )

  /** A complete, minimal environment. Nothing is inherited, so the root pipe variables (format spec section 8) and
    * any ambient cache or temp setting cannot reach the child.
    */
  def forScratch(scratch: Path, python: Path): Map[String, String] =
    def p(rel: String): String = scratch.resolve(rel).toString
    ThreadPins ++ Map(
      "PATH" -> (Option(python.getParent).map(_.toString + ":").getOrElse("") + "/usr/bin:/bin:/usr/sbin:/sbin"),
      "LANG" -> "en_US.UTF-8",
      "HOME" -> p("home"),
      "TMPDIR" -> p("tmp"),
      "TMP" -> p("tmp"),
      "TEMP" -> p("tmp"),
      "XDG_CACHE_HOME" -> p("home/.cache"),
      "XDG_CONFIG_HOME" -> p("home/.config"),
      "XDG_DATA_HOME" -> p("home/.local/share"),
      "MPLCONFIGDIR" -> p("home/mpl"),
      "MPLBACKEND" -> "Agg",
      "NUMBA_CACHE_DIR" -> p("home/numba"),
      "JOBLIB_TEMP_FOLDER" -> p("tmp"),
      "PYTHONDONTWRITEBYTECODE" -> "1",
      "PYTHONUSERBASE" -> p("home/.local"),
      "PYTHONHASHSEED" -> "0"
    )

  def description: String = s"threads=$Threads;" + ThreadPins.toVector.sorted.map((k, v) => s"$k=$v").mkString(";")

/** A live RAM-backed scratch. `release` is idempotent, exception-safe (every teardown step runs even if another
  * fails or the thread is interrupted; the interrupt flag is restored at the end) and returns `ScratchResidue` if
  * anything could not be verified gone. `release` never throws.
  */
trait Scratch:
  def root: Path
  /** Wipe, unmount and detach (macOS) or remove (Linux); then assert nothing survives. */
  def release(): Either[GlmSingleRefusal, Unit]

/** Creates RAM-backed scratches. */
trait ScratchProvider:
  def create(hook: ScratchHook): Either[GlmSingleRefusal, Scratch]

/** How the bridge kills a child's process group. Production kills with SIGKILL; tests inject a no-op to simulate
  * an unkillable member and prove the survivor verification is exercised.
  */
trait GroupKiller:
  def kill(pgid: Long, process: Process): Unit

object GroupKiller:
  val real: GroupKiller = (pgid, process) =>
    val before = Try(process.descendants().iterator().asScala.toVector).getOrElse(Vector.empty)
    val _ = RamScratch.cmd(Seq("/bin/kill", "-KILL", "--", s"-$pgid"), 10L)
    process.destroyForcibly(): Unit
    before.foreach(h => h.destroyForcibly(): Unit)

/** Live scratches of this JVM. A JVM shutdown hook (SIGINT, SIGTERM, normal exit) kills each registered child
  * process group (Ctrl-C does not reach a `setpgrp`'d child) and releases the scratch. `SIGKILL` and a JVM crash
  * cannot run it: then the child group is killed by its parent-death watchdog (`GlmSingleBridge.WatchdogScript`,
  * within `WatchdogPollSeconds`), and the RAM scratch itself is released by `RamScratch.sweepStale` at the next start.
  */
private[run] object ScratchRegistry:
  private val live = new java.util.concurrent.ConcurrentHashMap[Scratch, java.util.concurrent.atomic.AtomicLong]
  private var installed = false

  /** Test seam: when set, `register` throws it (models `addShutdownHook` during JVM shutdown). */
  @volatile private[run] var registerFault: Option[RuntimeException] = None

  def register(s: Scratch): Unit = synchronized {
    registerFault.foreach(e => throw e)
    if !installed then
      Runtime.getRuntime.addShutdownHook(new Thread(() => shutdownAll(), "phrfcmp-scratch-shutdown"))
      installed = true
    live.put(s, new java.util.concurrent.atomic.AtomicLong(-1L)): Unit
  }
  def unregister(s: Scratch): Unit = live.remove(s): Unit
  def setGroup(s: Scratch, pgid: Long): Unit = Option(live.get(s)).foreach(_.set(pgid))
  def liveCount: Int = live.size()

  def shutdownAll(): Unit =
    live.entrySet().asScala.toVector.foreach { e =>
      val g = e.getValue.get
      if g > 0 then Try(RamScratch.cmd(Seq("/bin/kill", "-KILL", "--", s"-$g"), 10L)): Unit
      try e.getKey.release(): Unit
      catch case _: Throwable => ()
    }

/** Paths of the macOS tools, so a test can substitute a failing or hanging one. */
private[run] final case class MacTools(
    hdiutil: String = "/usr/bin/hdiutil",
    newfs: String = "/sbin/newfs_hfs",
    mount: String = "/sbin/mount",
    umount: String = "/sbin/umount",
    cmdTimeoutSeconds: Long = 60L,
    retries: Int = 8,
    retryPauseMillis: Long = 250L
)

/** A scratch found by the startup sweep. */
final case class StaleScratch(path: String, device: Option[String], ownerPid: Long, outcome: String)

/** Host RAM scratch: `hdiutil attach -nomount ram://` plus HFS+ on macOS; a private 0700 directory under
  * `/dev/shm` (tmpfs) on Linux; anything else is a typed refusal (never a disk fallback). Mount points are named
  * `phrfcmp-ram-<ownerPid>-<random>` (Linux: `/dev/shm/phrfcmp-<ownerPid>-<random>`) so a later JVM can tell a
  * scratch whose owner died from one still in use.
  */
object RamScratch:
  val DefaultMegabytes: Int = 128
  private val MacPrefix = "phrfcmp-ram-"
  private val ShmPrefix = "phrfcmp-"

  /** The scratch for the host JVM. */
  def host(megabytes: Int = DefaultMegabytes): ScratchProvider = forOs(System.getProperty("os.name", ""), megabytes)

  def forOs(osName: String, megabytes: Int = DefaultMegabytes): ScratchProvider =
    forOsWith(osName, megabytes, MacTools(), ProcessHandle.current().pid())

  private[run] def forOsWith(osName: String, megabytes: Int, tools: MacTools, ownerPid: Long): ScratchProvider =
    val os = osName.toLowerCase
    if os.contains("mac") then hook => MacRam.create(megabytes, hook, tools, ownerPid)
    else if os.contains("linux") then hook => LinuxShm.create(hook, ownerPid)
    else _ => Left(GlmSingleRefusal.NoRamScratch(s"unsupported OS '$osName' (macOS and Linux only)"))

  private[run] final case class CmdResult(exit: Int, out: String)

  /** Test seam: variables added to every `cmd` child's inherited environment BEFORE the root-pipe scrub, standing
    * in for a runner JVM started with `PHRF_ROOT_FD`/`PHRF_ROOT_ACK_FD` set (a test cannot set its own environment).
    */
  @volatile private[run] var inheritedEnvForTest: Map[String, String] = Map.empty

  /** Runs a short tool. The output is read on its own thread and the wait is bounded, so a hung tool is killed
    * (`exit = -1`) rather than blocking. Interrupts never cut the wait short (teardown must finish); the flag is
    * restored on return. The root-pipe variables (format spec section 8) are scrubbed from the tool's environment.
    */
  private[run] def cmd(args: Seq[String], timeoutSeconds: Long = 60L): CmdResult =
    var interrupted = false
    try
      val pb = new ProcessBuilder(args*).redirectErrorStream(true)
      inheritedEnvForTest.foreach((k, v) => pb.environment().put(k, v))
      val pr = ChildEnvironment.scrub(pb).start()
      val out = new java.io.ByteArrayOutputStream
      val reader = new Thread(() => try { pr.getInputStream.transferTo(out): Unit } catch case _: java.io.IOException => (), "cmd-output")
      reader.setDaemon(true)
      reader.start()
      val deadline = System.nanoTime() + timeoutSeconds * 1_000_000_000L
      var done = false
      while !done do
        val remaining = deadline - System.nanoTime()
        if remaining <= 0L then done = true
        else
          try { if pr.waitFor(remaining, TimeUnit.NANOSECONDS) then done = true }
          catch case _: InterruptedException => interrupted = true
      val finished = !pr.isAlive
      if !finished then
        Try(pr.descendants().iterator().asScala.foreach(h => h.destroyForcibly(): Unit)): Unit
        pr.destroyForcibly(): Unit
        try { pr.waitFor(2, TimeUnit.SECONDS): Unit } catch case _: InterruptedException => interrupted = true
      try reader.join(2000L) catch case _: InterruptedException => interrupted = true
      if finished then CmdResult(pr.exitValue(), new String(out.toByteArray, UTF_8).trim) else CmdResult(-1, "timed out")
    catch case e: Exception => CmdResult(-2, e.getClass.getSimpleName)
    finally if interrupted then Thread.currentThread().interrupt()

  private[run] def emit(hook: ScratchHook, e: ScratchEvent): Unit =
    try hook.event(e)
    catch case _: Throwable => ()

  /** Entries the file system itself creates on an empty HFS+ volume; they hold names and counters, never data. */
  private val VolumeMetadata = Set(".fseventsd", ".Spotlight-V100", ".Trashes", ".metadata_never_index", ".DS_Store", ".TemporaryItems")

  /** Deletes everything under `dir` (keeping `dir`); returns files removed, bytes, and regular files left. */
  private[run] def wipe(dir: Path): (Int, Long, Vector[Path]) =
    var files = 0
    var bytes = 0L
    val entries =
      try
        val s = Files.walk(dir)
        try s.iterator().asScala.filter(_ != dir).toVector finally s.close()
      catch case _: Exception => Vector.empty[Path]
    entries.sortBy(-_.getNameCount).foreach { p =>
      try
        if Files.isRegularFile(p, java.nio.file.LinkOption.NOFOLLOW_LINKS) then
          bytes += Try(Files.size(p)).getOrElse(0L)
          files += 1
        Files.deleteIfExists(p): Unit
      catch case _: Exception => ()
    }
    val residue =
      try
        val s = Files.walk(dir)
        try s.iterator().asScala.filter(p => p != dir && !Files.isDirectory(p, java.nio.file.LinkOption.NOFOLLOW_LINKS))
          .filterNot(p => VolumeMetadata.contains(dir.relativize(p).getName(0).toString)).toVector
        finally s.close()
      catch case _: Exception => Vector.empty[Path]
    (files, bytes, residue)

  private def mountTable(tools: MacTools): Vector[(String, String)] =
    val re = """^(/dev/disk\S*) on (.+) \(([^)]*)\)$""".r
    cmd(Seq(tools.mount), tools.cmdTimeoutSeconds).out.linesIterator.flatMap {
      case re(dev, path, opts) if opts.startsWith("hfs") => Some(dev -> path)
      case _                                             => None
    }.toVector

  private def isMounted(path: Path, tools: MacTools): Boolean =
    val real = Try(path.toRealPath().toString).getOrElse(path.toString)
    cmd(Seq(tools.mount), tools.cmdTimeoutSeconds).out.linesIterator.exists(l => l.contains(s" on $real (") || l.contains(s" on ${path.toString} ("))

  /** Devices `hdiutil info` reports as `ram://` images. */
  private def ramDevices(tools: MacTools): Set[String] =
    val lines = cmd(Seq(tools.hdiutil, "info"), tools.cmdTimeoutSeconds).out.linesIterator.toVector
    val blocks = lines.foldLeft(Vector(Vector.empty[String])) { (acc, l) =>
      if l.startsWith("====") then acc :+ Vector.empty[String] else acc.init :+ (acc.last :+ l)
    }
    blocks.filter(_.exists(l => l.contains("image-path") && l.contains("ram://")))
      .flatMap(_.flatMap(l => """^(/dev/disk\d+)\b""".r.findFirstIn(l))).toSet

  private def subdirs(root: Path): Unit =
    Seq("out", "tmp", "home", "home/.cache", "home/.config", "home/.local/share", "home/mpl", "home/numba")
      .foreach(d => Files.createDirectories(root.resolve(d)): Unit)

  private val rwx = PosixFilePermissions.fromString("rwx------")

  /** True iff `/proc/mounts` lists `/dev/shm` as tmpfs (or devtmpfs). */
  private[run] def shmIsTmpfs(): Boolean =
    Try(Files.readAllLines(Paths.get("/proc/mounts")).asScala.exists { l =>
      val f = l.split(" ")
      f.lift(1).contains("/dev/shm") && f.lift(2).exists(Set("tmpfs", "devtmpfs").contains)
    }).getOrElse(false)

  private def ownerOf(name: String, prefix: String): Option[Long] =
    s"""^${java.util.regex.Pattern.quote(prefix)}(\\d+)-.*$$""".r.findFirstMatchIn(name).flatMap(m => Try(m.group(1).toLong).toOption)

  private def ownerDead(pid: Long): Boolean =
    pid != ProcessHandle.current().pid() && !ProcessHandle.of(pid).map(_.isAlive).orElse(false)

  /** Releases scratches whose owning JVM is dead, found by their `phrfcmp-ram-<pid>-*` mount path in `mount(8)`
    * (confirmed as a `ram://` device in `hdiutil info`), and removes empty stale mountpoint directories (macOS), or
    * stale `/dev/shm/phrfcmp-<pid>-*` directories (Linux). A mount whose path is not ours, or whose owner is alive,
    * is never touched. Returns what it found and did; also emits a `StaleSwept` event per item.
    */
  def sweepStale(hook: ScratchHook = ScratchHook.none): Vector[StaleScratch] =
    sweepStaleWith(System.getProperty("os.name", ""), hook, MacTools())

  private[run] def sweepStaleWith(osName: String, hook: ScratchHook, tools: MacTools): Vector[StaleScratch] =
    val os = osName.toLowerCase
    val found = Vector.newBuilder[StaleScratch]
    def report(s: StaleScratch): Unit =
      found += s
      emit(hook, ScratchEvent.StaleSwept(s.path, s.device, s.ownerPid, s.outcome))
    if os.contains("mac") then
      val tmp = Try(Paths.get(System.getProperty("java.io.tmpdir")).toRealPath()).toOption
      val rams = ramDevices(tools)
      val table = mountTable(tools)
      table.foreach { (dev, path) =>
        val p = Paths.get(path)
        val name = Option(p.getFileName).map(_.toString).getOrElse("")
        for
          owner <- ownerOf(name, MacPrefix)
          t <- tmp
          if Option(p.getParent).flatMap(x => Try(x.toRealPath()).toOption).contains(t)
          if ownerDead(owner) && rams.contains(dev)
        do
          val s = new MacScratch(dev, Some(p), hook, tools)
          s.markMounted()
          val out = s.release().fold(r => s"release failed: ${r.message}", _ => "released")
          report(StaleScratch(path, Some(dev), owner, out))
      }
      tmp.foreach { t =>
        val s = Files.newDirectoryStream(t, MacPrefix + "*")
        val dirs = try s.iterator().asScala.toVector finally s.close()
        dirs.foreach { d =>
          val owner = ownerOf(d.getFileName.toString, MacPrefix)
          val mountedNow = table.exists((_, p) => Try(Paths.get(p) == d.toRealPath()).getOrElse(false))
          if owner.exists(ownerDead) && !mountedNow && Files.isDirectory(d) then
            val empty = Try { val ds = Files.newDirectoryStream(d); try !ds.iterator().hasNext finally ds.close() }.getOrElse(false)
            if empty then
              val ok = Try(Files.deleteIfExists(d)).isSuccess
              report(StaleScratch(d.toString, None, owner.get, if ok then "removed empty stale mountpoint" else "could not remove empty mountpoint"))
        }
      }
    else if os.contains("linux") then
      val shm = Paths.get("/dev/shm")
      // Never sweep a /dev/shm that is not a tmpfs mount; the owner-pid test also assumes one pid namespace (receipt).
      if Files.isDirectory(shm) && shmIsTmpfs() then
        val s = Files.newDirectoryStream(shm, ShmPrefix + "*")
        val dirs = try s.iterator().asScala.toVector finally s.close()
        dirs.foreach { d =>
          ownerOf(d.getFileName.toString, ShmPrefix).filter(ownerDead).foreach { owner =>
            val sc = new ShmScratch(d, hook)
            report(StaleScratch(d.toString, None, owner, sc.release().fold(r => s"release failed: ${r.message}", _ => "released")))
          }
        }
    found.result()

  // ---- teardown helper shared by both platforms ------------------------------------------------------------
  /** Collects interrupts and failures so that every teardown step runs. */
  private final class Teardown(hook: ScratchHook):
    var interrupted: Boolean = Thread.interrupted()
    var problems: Vector[(String, String)] = Vector.empty
    def note(step: String, detail: String): Unit =
      problems :+= (step -> detail)
      emit(hook, ScratchEvent.StepFailed(step, detail))
    def pause(ms: Long): Unit =
      if Thread.interrupted() then interrupted = true
      try Thread.sleep(ms) catch case _: InterruptedException => interrupted = true
    def step(name: String)(f: => Unit): Unit =
      if Thread.interrupted() then interrupted = true
      try f catch case t: Throwable => note(name, t.getClass.getSimpleName)
      if Thread.interrupted() then interrupted = true
    def finish(): Either[GlmSingleRefusal, Unit] =
      if interrupted then Thread.currentThread().interrupt()
      if problems.isEmpty then Right(())
      else Left(GlmSingleRefusal.ScratchResidue(problems.map(_._1).distinct.mkString(","), problems.map(_._2).mkString("; "), None))

  // ---- macOS --------------------------------------------------------------------------------------------------
  private object MacRam:
    def create(megabytes: Int, hook: ScratchHook, tools: MacTools, ownerPid: Long): Either[GlmSingleRefusal, Scratch] =
      val sectors = megabytes.toLong * 2048L
      def fail(step: String, detail: String): GlmSingleRefusal =
        emit(hook, ScratchEvent.StepFailed(step, detail))
        GlmSingleRefusal.ScratchSetupFailed(step, detail)
      if !Files.isExecutable(Paths.get(tools.hdiutil)) then
        Left(GlmSingleRefusal.NoRamScratch("hdiutil not found"))
      else
        val attach = cmd(Seq(tools.hdiutil, "attach", "-nomount", s"ram://$sectors"), tools.cmdTimeoutSeconds)
        val dev = attach.out.trim
        if attach.exit != 0 || !dev.startsWith("/dev/disk") then
          Left(GlmSingleRefusal.NoRamScratch(s"hdiutil attach -nomount failed (exit ${attach.exit}): ${attach.out.take(200)}"))
        else
          emit(hook, ScratchEvent.DeviceAttached(dev, sectors))
          val mountDir =
            try Right(Files.createTempDirectory(s"$MacPrefix$ownerPid-", PosixFilePermissions.asFileAttribute(rwx)))
            catch case e: Exception => Left(fail("mountpoint", e.getClass.getSimpleName))
          val s = new MacScratch(dev, mountDir.toOption, hook, tools)
          try
            ScratchRegistry.register(s)
            val up = for
              md <- mountDir
              _ <- Right(emit(hook, ScratchEvent.Created("macos-ram-disk", md.toString)))
              _ <- {
                val f = cmd(Seq(tools.newfs, "-v", "phrfscratch", dev), tools.cmdTimeoutSeconds)
                if f.exit != 0 then Left(fail("newfs_hfs", f.out.take(200)))
                else Right(emit(hook, ScratchEvent.Formatted(dev, "HFS+")))
              }
              _ <- {
                val opts = "nobrowse,nosuid,nodev"
                val m = cmd(Seq(tools.mount, "-t", "hfs", "-o", opts, dev, md.toString), tools.cmdTimeoutSeconds)
                if m.exit != 0 then Left(fail("mount", m.out.take(200)))
                else
                  s.markMounted()
                  emit(hook, ScratchEvent.Mounted(md.toString, opts))
                  Right(())
              }
              // The 0700 mode on the volume root is mandatory and checked before any child can start.
              _ <- Try {
                Files.setPosixFilePermissions(md, rwx)
                if Files.getPosixFilePermissions(md).asScala.toSet != rwx.asScala.toSet then throw new java.io.IOException("mode not applied")
              }.toEither.left.map(e => fail("chmod-0700", e.getClass.getSimpleName))
              _ <- Try {
                // mdutil -i off needs root; the volume is mounted nobrowse and carries the never-index marker.
                Files.createFile(md.resolve(".metadata_never_index")): Unit
                subdirs(md)
                emit(hook, ScratchEvent.IndexingGuard("nobrowse mount plus .metadata_never_index (mdutil -i off requires root and is not used)"))
              }.toEither.left.map(e => fail("layout", e.getClass.getSimpleName))
            yield s
            up.left.map { r =>
              s.release() match
                case Left(c)  => GlmSingleRefusal.ScratchResidue("setup-cleanup", c.message, Some(r.message))
                case Right(_) => r
            }
          catch
            case t: Throwable =>
              s.release(): Unit
              throw t

  private[run] final class MacScratch(device: String, dir: Option[Path], hook: ScratchHook, tools: MacTools) extends Scratch:
    private var mountedNow = false
    private var released = false
    def markMounted(): Unit = mountedNow = true
    def root: Path = dir.get

    def release(): Either[GlmSingleRefusal, Unit] = synchronized {
      if released then Right(())
      else
        val td = new Teardown(hook)
        // 1. wipe
        td.step("wipe") {
          dir.foreach { d =>
            if mountedNow then
              val (n, b, residue) = wipe(d)
              emit(hook, ScratchEvent.Wiped(n, b, residue.length))
              if residue.nonEmpty then td.note("wipe", s"${residue.length} file(s) survive the wipe")
          }
        }
        // 2. unmount, retried while briefly busy (fseventsd); never forced
        td.step("unmount") {
          dir.foreach { d =>
            if mountedNow then
              var ok = false
              var i = 0
              while !ok && i < tools.retries do
                ok = cmd(Seq(tools.umount, d.toString), tools.cmdTimeoutSeconds).exit == 0
                if !ok then td.pause(tools.retryPauseMillis)
                i += 1
              if ok then
                mountedNow = false
                emit(hook, ScratchEvent.Unmounted(d.toString))
              else td.note("unmount", "umount failed (not forced)")
          }
        }
        // 3. detach the RAM device (this discards the memory); attempted regardless, never forced
        td.step("detach") {
          var ok = !Files.exists(Paths.get(device))
          var i = 0
          while !ok && i < tools.retries do
            ok = cmd(Seq(tools.hdiutil, "detach", device), tools.cmdTimeoutSeconds).exit == 0 || !Files.exists(Paths.get(device))
            if !ok then td.pause(tools.retryPauseMillis)
            i += 1
          if ok then emit(hook, ScratchEvent.Detached(device)) else td.note("detach", s"hdiutil detach $device failed (not forced)")
        }
        // 4. remove the mountpoint (fails harmlessly while still mounted)
        td.step("remove-mountpoint") {
          dir.foreach { d =>
            if Files.exists(d) then
              Files.deleteIfExists(d): Unit
              emit(hook, ScratchEvent.Removed(d.toString))
          }
        }
        // 5. verify
        td.step("verify") {
          val pathGone = dir.forall(d => !Files.exists(d) && !isMounted(d, tools))
          val deviceGone = !Files.exists(Paths.get(device))
          emit(hook, ScratchEvent.Verified(pathGone, deviceGone))
          if !pathGone then td.note("verify", "scratch path still exists or is mounted")
          if !deviceGone then td.note("verify", s"device $device still exists")
        }
        val r = td.finish()
        if r.isRight then
          released = true
          ScratchRegistry.unregister(this)
        r
    }

  // ---- Linux --------------------------------------------------------------------------------------------------
  private object LinuxShm:
    def create(hook: ScratchHook, ownerPid: Long): Either[GlmSingleRefusal, Scratch] =
      val shm = Paths.get("/dev/shm")
      if !Files.isDirectory(shm) || !Files.isWritable(shm) then Left(GlmSingleRefusal.NoRamScratch("/dev/shm is absent or not writable"))
      else
        if !shmIsTmpfs() then Left(GlmSingleRefusal.NoRamScratch("/dev/shm is not a tmpfs mount"))
        else
          try
            val d = Files.createTempDirectory(shm, s"$ShmPrefix$ownerPid-", PosixFilePermissions.asFileAttribute(rwx))
            val s = new ShmScratch(d, hook)
            ScratchRegistry.register(s)
            try
              subdirs(d)
              emit(hook, ScratchEvent.Created("linux-dev-shm", d.toString))
              Right(s)
            catch
              case t: Throwable =>
                s.release(): Unit
                throw t
          catch case e: Exception => Left(GlmSingleRefusal.ScratchSetupFailed("mkdir", e.getClass.getSimpleName))

  private[run] final class ShmScratch(dir: Path, hook: ScratchHook) extends Scratch:
    private var released = false
    def root: Path = dir
    def release(): Either[GlmSingleRefusal, Unit] = synchronized {
      if released then Right(())
      else
        val td = new Teardown(hook)
        td.step("wipe") {
          val (n, b, residue) = wipe(dir)
          emit(hook, ScratchEvent.Wiped(n, b, residue.length))
          if residue.nonEmpty then td.note("wipe", s"${residue.length} file(s) survive the wipe")
        }
        td.step("remove") {
          Files.deleteIfExists(dir): Unit
          emit(hook, ScratchEvent.Removed(dir.toString))
        }
        td.step("verify") {
          val gone = !Files.exists(dir)
          emit(hook, ScratchEvent.Verified(gone, deviceGone = true))
          if !gone then td.note("verify", "scratch path still exists")
        }
        val r = td.finish()
        if r.isRight then
          released = true
          ScratchRegistry.unregister(this)
        r
    }

/** How the bridge is run. `python` is the GLMsingle virtualenv interpreter; `fromGenerator` the frozen script.
  * `killer` and `probe` are test seams (the production values are the defaults).
  */
final case class GlmSingleConfig(
    python: Path,
    fromGenerator: Path,
    timeoutSeconds: Double,
    expectGeneratorSha: Option[String] = None,
    expectNPool: Option[Int] = None,
    expectNVoxels: Option[Int] = None,
    expectedFromGeneratorSha256: String = GlmSinglePins.FromGeneratorSha256,
    expectedGlmSingleCommit: String = GlmSinglePins.GlmSingleCommit,
    scratch: ScratchProvider = RamScratch.host(),
    hook: ScratchHook = ScratchHook.none,
    killer: GroupKiller = GroupKiller.real,
    probe: String => Unit = _ => ()
):
  require(timeoutSeconds > 0.0, "timeout must be positive")

/** What one invocation cost and did, whether or not it produced an estimate (design 2.3, 5.1: every attempt is
  * retained with exit metadata, wall and CPU seconds, output hash).
  *
  * CPU figures (two purposes, never mixed):
  *  - `guardCpuSeconds`: for the section 5.2 runtime guard only. The child's rusage (`/usr/bin/time`, includes
  *    interpreter start-up, imports and JIT), or the polled process-tree CPU if no rusage was captured (killed
  *    child). This is what the machine actually spent.
  *  - `sidecarCpuSeconds`: for the timing endpoint only. The sidecar `cpu_s`, the GLMsingle call alone, which
  *    matches the in-process native arms whose timing excludes process start-up.
  */
final case class GlmSingleAttempt(
    exitCode: Option[Int],
    timedOut: Boolean,
    wallSeconds: Double,
    rusageCpuSeconds: Option[Double],
    polledCpuSeconds: Double,
    sidecarCpuSeconds: Option[Double],
    outputNpzSha256: Option[String],
    groupSurvivors: Int
):
  def guardCpuSeconds: Double = rusageCpuSeconds.getOrElse(polledCpuSeconds)

object GlmSingleAttempt:
  val none: GlmSingleAttempt = GlmSingleAttempt(None, false, 0.0, None, 0.0, None, None, 0)

/** The attempt record plus its result; both are present on every branch. */
final case class GlmSingleRun(attempt: GlmSingleAttempt, result: Either[GlmSingleRefusal, GlmSingleOutcome])

/** The in-memory result of one GLMsingle invocation. `voxels` holds the E-trial of the scored voxels in INPUT trial
  * order (`TrialEstimate` carries a redacted `toString`). See [[GlmSingleAttempt]] for the two CPU figures:
  * `guardCpuSeconds` (section 5.2 guard) and `timingCpuSeconds` (timing endpoint).
  */
final case class GlmSingleOutcome(
    voxels: Vector[VoxelOutcome[TrialEstimate]],
    nTrials: Int,
    guardCpuSeconds: Double,
    timingCpuSeconds: Double,
    cpuRusageSeconds: Option[Double],
    cpuPolledSeconds: Double,
    wallSeconds: Double,
    outputNpzSha256: String,
    inputNpzSha256: String,
    realizedPoolSize: Int,
    glmSingleCommit: String,
    fromGeneratorSha256: String,
    threadPinning: String
):
  override def toString: String = s"GlmSingleOutcome(voxels=${voxels.length}, trials=$nTrials, <redacted>)"

object GlmSingleBridge:
  private val swept = new java.util.concurrent.atomic.AtomicBoolean(false)

  /** Run GLMsingle on one generator dataset (see [[runAttempt]] for the attempt metadata on every branch). */
  def run(inputs: GlmSingleInputs, config: GlmSingleConfig): Either[GlmSingleRefusal, GlmSingleOutcome] =
    runAttempt(inputs, config).result

  /** Like `run`, but also returns the attempt (exit, wall, CPU, output hash) when the run is refused. Cleanup of
    * the scratch runs in a `finally` on every path including `Error`; a cleanup failure, or a process-group member
    * that survives the kill, overrides the run result (a result is never returned from a host whose scratch or
    * child could not be verified gone).
    */
  def runAttempt(inputs: GlmSingleInputs, config: GlmSingleConfig): GlmSingleRun =
    if swept.compareAndSet(false, true) then Try(RamScratch.sweepStale(config.hook)): Unit
    preflight(inputs, config) match
      case Left(r) => GlmSingleRun(GlmSingleAttempt.none, Left(r))
      case Right(inputSha) =>
        config.scratch.create(config.hook) match
          case Left(r) => GlmSingleRun(GlmSingleAttempt.none, Left(r))
          case Right(scratch) =>
            var run = GlmSingleRun(GlmSingleAttempt.none, Left(GlmSingleRefusal.LaunchFailed("not run")))
            var cleaned: Either[GlmSingleRefusal, Unit] = Right(())
            var interrupted = false
            // Holds the attempt as soon as the child has been measured, so an exception after that point (output
            // reading, parsing) keeps the child's exit, wall and CPU for the section 5.2 guard.
            val sink = new AttemptSink
            try
              run =
                try runInScratch(inputs, config, inputSha, scratch, sink)
                catch
                  case _: InterruptedException =>
                    interrupted = true
                    GlmSingleRun(sink.attempt, Left(GlmSingleRefusal.Interrupted))
                  case NonFatal(e) =>
                    GlmSingleRun(sink.attempt, Left(GlmSingleRefusal.LaunchFailed(e.getClass.getSimpleName)))
            finally
              // Runs for Error too (OutOfMemoryError, StackOverflowError, LinkageError): the Error then propagates.
              cleaned = scratch.release()
              if interrupted then Thread.currentThread().interrupt()
            val prior = run.result.left.toOption.map(_.message)
            if run.attempt.groupSurvivors > 0 then
              val d = s"${run.attempt.groupSurvivors} process(es) of the child group survive SIGKILL; the unmount was not forced"
              GlmSingleRun(run.attempt, Left(GlmSingleRefusal.ScratchResidue("kill", d + cleaned.left.toOption.fold("")(c => "; " + c.message), prior)))
            else
              cleaned match
                case Right(_) => run
                case Left(GlmSingleRefusal.ScratchResidue(s, d, _)) =>
                  GlmSingleRun(run.attempt, Left(GlmSingleRefusal.ScratchResidue(s, d, prior.orElse(Some("result discarded")))))
                case Left(other) => GlmSingleRun(run.attempt, Left(other))

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  private def preflight(inputs: GlmSingleInputs, config: GlmSingleConfig): Either[GlmSingleRefusal, String] =
    for
      script <- readFile(config.fromGenerator)
      _ <- Either.cond(sha256(script) == config.expectedFromGeneratorSha256, (),
        GlmSingleRefusal.ScriptHashMismatch(config.expectedFromGeneratorSha256, sha256(script)))
      data <- readFile(inputs.datasetNpz)
      sha = sha256(data)
      _ <- inputs.expectedInputSha256 match
        case Some(e) if e != sha => Left(GlmSingleRefusal.InputHashMismatch(e, sha))
        case _                   => Right(())
      manifest = inputs.datasetNpz.resolveSibling(inputs.datasetNpz.getFileName.toString.stripSuffix(".npz") + ".manifest.json")
      _ <- if Files.isReadable(manifest) then Right(()) else Left(GlmSingleRefusal.InputUnreadable(manifest.toString, "manifest not readable"))
      _ <- Either.cond(Files.isExecutable(config.python), (), GlmSingleRefusal.LauncherMissing(s"python ${config.python}"))
      _ <- Either.cond(Files.isExecutable(Paths.get("/usr/bin/perl")), (), GlmSingleRefusal.LauncherMissing("/usr/bin/perl (process-group launcher)"))
      _ <- Either.cond(Files.isExecutable(Paths.get("/usr/bin/pgrep")), (), GlmSingleRefusal.LauncherMissing("/usr/bin/pgrep (group verifier)"))
    yield sha

  private def readFile(p: Path): Either[GlmSingleRefusal, Array[Byte]] =
    Try(Files.readAllBytes(p)).toEither.left.map(e => GlmSingleRefusal.InputUnreadable(p.toString, e.getClass.getSimpleName))

  private val UserSys = """(?m)^(user|sys)\s+([0-9.]+)""".r

  /** The latest attempt record of one invocation (see `runAttempt`). */
  private[run] final class AttemptSink:
    @volatile var attempt: GlmSingleAttempt = GlmSingleAttempt.none

  /** Parent-death watchdog poll interval; the bound on how long the child group outlives its JVM is this plus one
    * `kill(2)` (format spec section 8).
    */
  val WatchdogPollSeconds: Double = 0.1

  /** The launcher (`perl -e WatchdogScript <parentPid> -- command...`). It makes itself the leader of a new process
    * group (pid = pgid, so the bridge can kill the group without killing the runner), forks the command into that
    * group, and then polls:
    *  - when the command exits, it SIGKILLs any member of the group other than itself (background stragglers) and
    *    exits with the command's status (128 + signal for a signalled command);
    *  - when its parent is no longer `<parentPid>` (the runner JVM died, including a SIGKILL of the runner's own
    *    process group, which the launcher has left), or on SIGTERM, SIGINT or SIGHUP, it SIGKILLs its whole group,
    *    itself included. It also does so at once if the parent is already gone when it starts.
    * The bridge's own timeout and kill paths are unchanged: `kill -KILL -- -<pgid>` takes the launcher with it.
    */
  val WatchdogScript: String =
    s"""use strict; use POSIX ();
       |my $$parent = shift @ARGV; shift @ARGV if @ARGV && $$ARGV[0] eq '--';
       |die "usage: <parent-pid> -- command...\\n" unless defined $$parent && $$parent =~ /^\\d+$$/ && @ARGV;
       |setpgrp(0, 0) or die "setpgrp failed: $$!\\n";
       |my $$gone = sub { kill 'KILL', -$$$$; POSIX::_exit(137) };
       |$$SIG{$$_} = $$gone for qw(TERM INT HUP);
       |$$gone->() if getppid() != $$parent;
       |my $$pid = fork();
       |die "fork failed: $$!\\n" unless defined $$pid;
       |if ($$pid == 0) { $$SIG{$$_} = 'DEFAULT' for qw(TERM INT HUP); exec { $$ARGV[0] } @ARGV; POSIX::_exit(127) }
       |my $$status;
       |while (1) {
       |  if (waitpid($$pid, POSIX::WNOHANG()) == $$pid) { $$status = $$?; last }
       |  $$gone->() if getppid() != $$parent;
       |  select(undef, undef, undef, $WatchdogPollSeconds);
       |}
       |for (1 .. 50) {
       |  my @m = grep { $$_ != $$$$ } map { /(\\d+)/ ? $$1 : () } `/usr/bin/pgrep -g $$$$`;
       |  last unless @m;
       |  kill 'KILL', @m;
       |  $$gone->() if getppid() != $$parent;
       |  select(undef, undef, undef, 0.02);
       |}
       |POSIX::_exit(($$status & 127) ? 128 + ($$status & 127) : ($$status >> 8));
       |""".stripMargin

  /** The launcher command line for `command`, guarded against the death of `parentPid`. */
  private[run] def watchdogCommand(parentPid: Long, command: Seq[String]): Seq[String] =
    Seq("/usr/bin/perl", "-e", WatchdogScript, parentPid.toString, "--") ++ command

  /** Starts `command` under the parent-death watchdog, whose pid is the new process group id (pid = pgid). */
  private[run] def startGroupLeader(command: Seq[String], dir: Path, env: Map[String, String]): Process =
    val cmd = watchdogCommand(ProcessHandle.current().pid(), command)
    val pb = new ProcessBuilder(cmd*).redirectErrorStream(true).directory(dir.toFile)
    pb.environment().clear()
    env.foreach((k, v) => pb.environment().put(k, v))
    pb.start()

  private def runInScratch(
      inputs: GlmSingleInputs,
      config: GlmSingleConfig,
      inputSha: String,
      scratch: Scratch,
      sink: AttemptSink
  ): GlmSingleRun =
    val root = scratch.root
    val outDir = root.resolve("out")
    val timeFile = root.resolve("tmp").resolve("child.time")
    val haveTime = Files.isExecutable(Paths.get("/usr/bin/time"))
    val pyArgs = Seq(config.python.toString, config.fromGenerator.toAbsolutePath.toString, inputs.datasetNpz.toAbsolutePath.toString,
      "--out", outDir.toString, "--timeout", config.timeoutSeconds.toString) ++
      config.expectGeneratorSha.toSeq.flatMap(s => Seq("--expect-generator-sha", s)) ++
      config.expectNPool.toSeq.flatMap(n => Seq("--expect-n-pool", n.toString)) ++
      config.expectNVoxels.toSeq.flatMap(n => Seq("--expect-n-voxels", n.toString))
    val timed = if haveTime then Seq("/usr/bin/time", "-p", "-o", timeFile.toString) ++ pyArgs else pyArgs
    val t0 = System.nanoTime()
    val started =
      try Right(startGroupLeader(timed, root, GlmSingleEnv.forScratch(root, config.python)))
      catch case e: java.io.IOException => Left(GlmSingleRefusal.LaunchFailed(e.getClass.getSimpleName))
    started match
      case Left(r) => GlmSingleRun(GlmSingleAttempt.none, Left(r))
      case Right(pr) =>
        val pgid = pr.pid()
        ScratchRegistry.setGroup(scratch, pgid)
        val tail = new TailBuffer(8192)
        val reader = new Thread(() => tail.drain(pr.getInputStream), "glmsingle-output")
        reader.setDaemon(true)
        reader.start()
        var polledCpu = 0.0
        var lastPs = 0L
        var timedOut = false
        var interrupted = false
        var survivors = 0
        try
          RamScratch.emit(config.hook, ScratchEvent.ChildStarted(pr.pid(), pgid))
          def poll(): Unit =
            val handles = Iterator(pr.toHandle) ++ Try(pr.descendants().iterator().asScala).getOrElse(Iterator.empty)
            val total = handles.map(h => h.info().totalCpuDuration().toScala.map(_.toNanos / 1e9).getOrElse(0.0)).sum
            if total > polledCpu then polledCpu = total
            // `totalCpuDuration` is empty on some hosts (macOS), so the group's `ps` CPU time is a second source.
            val now = System.nanoTime()
            if now - lastPs > 200_000_000L then
              lastPs = now
              val g = groupCpuSeconds(pgid)
              if g > polledCpu then polledCpu = g
          val deadline = t0 + (config.timeoutSeconds * 1e9).toLong
          while pr.isAlive && !timedOut && !interrupted do
            poll()
            if System.nanoTime() >= deadline then
              lastPs = 0L
              poll()
              timedOut = true
            else
              try { pr.waitFor(25, TimeUnit.MILLISECONDS): Unit }
              catch case _: InterruptedException => interrupted = true
        finally
          // Process-group kill comes first on every path (also when the child already exited: stragglers).
          survivors = killGroup(pgid, pr, config.hook, config.killer)
          try { pr.waitFor(10, TimeUnit.SECONDS): Unit } catch case _: InterruptedException => interrupted = true
          try reader.join(5000L) catch case _: InterruptedException => interrupted = true
          if interrupted then Thread.currentThread().interrupt()
          // Recorded here so that the measurement survives an exception thrown from the loop or after it.
          val exit0 = if timedOut || interrupted || pr.isAlive then None else Some(pr.exitValue())
          sink.attempt = GlmSingleAttempt(exit0, timedOut, (System.nanoTime() - t0) / 1e9, readRusage(timeFile), polledCpu, None, None, survivors)
        val text = tail.text
        val base = sink.attempt
        val exit = base.exitCode
        if interrupted then GlmSingleRun(base, Left(GlmSingleRefusal.Interrupted))
        else if timedOut then GlmSingleRun(base, Left(GlmSingleRefusal.Timeout(config.timeoutSeconds, base.guardCpuSeconds)))
        else
          exit match
            case Some(0) => readOutcome(inputs, config, inputSha, outDir, base, sink)
            case Some(2) => GlmSingleRun(base, Left(GlmSingleRefusal.InputRefusedByPython(text.takeRight(600))))
            case Some(c) => GlmSingleRun(base, Left(GlmSingleRefusal.ChildFailed(c, text.takeRight(600))))
            case None    => GlmSingleRun(base, Left(GlmSingleRefusal.ChildFailed(-1, text.takeRight(600))))

  /** CPU seconds of the live members of process group `pgid` from `ps` (`[[dd-]hh:]mm:ss[.ss]` cumulative time). */
  private[run] def groupCpuSeconds(pgid: Long): Double =
    val r = RamScratch.cmd(Seq("/bin/ps", "-axo", "pgid=,time="), 10L)
    if r.exit != 0 then 0.0
    else
      r.out.linesIterator.flatMap { l =>
        l.trim.split("\\s+") match
          case Array(g, t) if g == pgid.toString => parsePsTime(t)
          case _                                  => None
      }.sum

  private[run] def parsePsTime(t: String): Option[Double] =
    val (days, rest) = t.split("-", 2) match
      case Array(d, r) => (Try(d.toDouble).getOrElse(0.0), r)
      case _           => (0.0, t)
    Try(rest.split(":").foldLeft(0.0)((acc, p) => acc * 60.0 + p.toDouble) + days * 86400.0).toOption

  private def readRusage(f: Path): Option[Double] =
    Try(new String(Files.readAllBytes(f), UTF_8)).toOption.flatMap { t =>
      val m = UserSys.findAllMatchIn(t).map(x => x.group(1) -> x.group(2).toDouble).toMap
      for u <- m.get("user"); s <- m.get("sys") yield u + s
    }

  /** SIGKILL the whole process group through `killer`, confirm with `pgrep -g` that it is empty, retrying; returns
    * the number of members still present (0 = verified empty). If `pgrep` cannot give an answer the group is
    * counted as surviving.
    */
  private[run] def killGroup(pgid: Long, process: Process, hook: ScratchHook, killer: GroupKiller): Int =
    var intr = Thread.interrupted()
    def members: Int =
      val r = RamScratch.cmd(Seq("/usr/bin/pgrep", "-g", pgid.toString), 10L)
      if r.exit == 0 then r.out.linesIterator.count(_.trim.nonEmpty).max(1)
      else if r.exit == 1 then 0
      else 1
    killer.kill(pgid, process)
    var left = members
    var i = 0
    while left > 0 && i < 20 do
      if Thread.interrupted() then intr = true
      try Thread.sleep(50L) catch case _: InterruptedException => intr = true
      if i % 5 == 4 then killer.kill(pgid, process)
      left = members
      i += 1
    RamScratch.emit(hook, ScratchEvent.ProcessGroupKilled(pgid, left))
    if intr then Thread.currentThread().interrupt()
    left

  private final class TailBuffer(cap: Int):
    private val buf = new java.io.ByteArrayOutputStream
    def drain(in: java.io.InputStream): Unit =
      val chunk = new Array[Byte](4096)
      var n = in.read(chunk)
      while n >= 0 do
        synchronized {
          buf.write(chunk, 0, n)
          if buf.size > 4 * cap then
            val keep = buf.toByteArray.takeRight(cap)
            buf.reset()
            buf.write(keep, 0, keep.length)
        }
        n = in.read(chunk)
    def text: String = synchronized(new String(buf.toByteArray.takeRight(cap), UTF_8))

  private def readOutcome(
      inputs: GlmSingleInputs,
      config: GlmSingleConfig,
      inputSha: String,
      outDir: Path,
      base: GlmSingleAttempt,
      sink: AttemptSink
  ): GlmSingleRun =
    val stem = inputs.datasetNpz.getFileName.toString.stripSuffix(".npz")
    val npzPath = outDir.resolve(s"$stem.glmsingle.npz")
    val metaPath = outDir.resolve(s"$stem.glmsingle.meta.json")
    var outShaSeen: Option[String] = None
    var cpuSeen: Option[Double] = None
    def read(p: Path): Either[GlmSingleRefusal, Array[Byte]] =
      if !Files.isRegularFile(p) then Left(GlmSingleRefusal.MissingOutput(p.getFileName.toString))
      else Try(Files.readAllBytes(p)).toEither.left.map(e => GlmSingleRefusal.OutputUnreadable(p.getFileName.toString, e.getClass.getSimpleName))
    def attempt = base.copy(sidecarCpuSeconds = cpuSeen, outputNpzSha256 = outShaSeen)
    def readValidated(): Either[GlmSingleRefusal, GlmSingleOutcome] =
      for
        npzBytes <- read(npzPath)
        _ = { outShaSeen = Some(sha256(npzBytes)) }
        metaBytes <- read(metaPath)
        npz <- Npz.parse(npzBytes).left.map(e => GlmSingleRefusal.OutputUnreadable(npzPath.getFileName.toString, e.message))
        // The parser message can quote the offending bytes, which are result bytes: it is not carried.
        side <- Try(ujson.read(new String(metaBytes, UTF_8))).toEither.left.map(_ => GlmSingleRefusal.OutputUnreadable(metaPath.getFileName.toString, "sidecar is not parseable JSON"))
        outSha = sha256(npzBytes)
        _ <- str(side, "output", "npz_sha256").flatMap(s => check("output npz sha256", outSha, s))
        _ <- str(side, "input", "npz_sha256").flatMap(s => check("input npz sha256", inputSha, s))
        _ <- str(side, "meta", "from_generator_sha256").flatMap(s => check("from_generator.py sha256", config.expectedFromGeneratorSha256, s))
        commit <- str(side, "meta", "glmsingle_commit")
        _ <- check("GLMsingle commit", config.expectedGlmSingleCommit, commit)
        pool <- num(side, "meta", "realized_pool_size")
        cpuS <- num(side, "timing", "cpu_s")
        _ = { cpuSeen = Some(cpuS) }
        beta <- matrix(npz, "d_beta_data")
        evIdx <- vector(npz, "trial_event_index")
        ev <- eventIndex(evIdx, beta._2)
        voxels <- Right(toVoxelOutcomes(beta._1, beta._2, beta._3, ev))
      yield
        val guard = base.guardCpuSeconds
        GlmSingleOutcome(voxels, beta._2, guard, cpuS, base.rusageCpuSeconds, base.polledCpuSeconds, base.wallSeconds, outSha, inputSha,
          pool.toInt, commit, config.expectedFromGeneratorSha256, GlmSingleEnv.description)
    // The sink is updated on every exit, so a throw while reading keeps the output hash seen so far.
    val res =
      try
        config.probe("read-output")
        readValidated()
      finally sink.attempt = attempt
    GlmSingleRun(attempt, res)

  private def check(what: String, expected: String, found: String): Either[GlmSingleRefusal, Unit] =
    Either.cond(expected == found, (), GlmSingleRefusal.ProvenanceMismatch(what, expected, found))

  private def at(v: ujson.Value, keys: Seq[String]): Option[ujson.Value] =
    keys.foldLeft(Option(v))((acc, k) => acc.flatMap(x => Try(x.obj).toOption.flatMap(_.get(k))))

  private def str(side: ujson.Value, keys: String*): Either[GlmSingleRefusal, String] =
    at(side, keys).flatMap(x => Try(x.str).toOption).toRight(GlmSingleRefusal.OutputInvalid(s"sidecar key ${keys.mkString(".")} missing"))

  private def num(side: ujson.Value, keys: String*): Either[GlmSingleRefusal, Double] =
    at(side, keys).flatMap(x => Try(x.num).toOption).toRight(GlmSingleRefusal.OutputInvalid(s"sidecar key ${keys.mkString(".")} missing"))

  private def matrix(npz: Npz, name: String): Either[GlmSingleRefusal, (Int, Int, Array[Double])] =
    npz.get(name).map(_.array) match
      case Some(NpyArray.F64(Vector(v, n), data)) if v > 0 && n > 0 => Right((v, n, data))
      case _ => Left(GlmSingleRefusal.OutputInvalid(s"$name is missing or not a non-empty 2-d float64 array"))

  private def vector(npz: Npz, name: String): Either[GlmSingleRefusal, Array[Double]] =
    npz.get(name).map(_.array) match
      case Some(NpyArray.F64(Vector(_), data)) => Right(data)
      case _                                   => Left(GlmSingleRefusal.OutputInvalid(s"$name is missing or not a 1-d float64 array"))

  /** `trial_event_index[k]` is the generator event index of chronological trial `k`: it must be a permutation. */
  private[run] def eventIndex(raw: Array[Double], nTrials: Int): Either[GlmSingleRefusal, Vector[Int]] =
    if raw.length != nTrials then Left(GlmSingleRefusal.OutputInvalid(s"trial_event_index length ${raw.length} != $nTrials trials"))
    else if raw.exists(x => x != math.rint(x) || x < 0 || x >= nTrials) then Left(GlmSingleRefusal.OutputInvalid("trial_event_index has a non-integer or out-of-range entry"))
    else
      val ev = raw.iterator.map(_.toInt).toVector
      if ev.distinct.length != nTrials then Left(GlmSingleRefusal.OutputInvalid("trial_event_index is not a permutation"))
      else Right(ev)

  /** Chronological betas (row-major `voxels x trials`) to per-voxel estimates in input (event) order:
    * `inputOrder(v)(eventIndex(k)) = chrono(v, k)`. A voxel with a non-finite beta is `Failed` alone.
    */
  private[phrfcmp] def toVoxelOutcomes(voxels: Int, trials: Int, chrono: Array[Double], eventIndex: Vector[Int]): Vector[VoxelOutcome[TrialEstimate]] =
    Vector.tabulate(voxels) { v =>
      val row = new Array[Double](trials)
      var k = 0
      while k < trials do
        row(eventIndex(k)) = chrono(v * trials + k)
        k += 1
      TrialEstimate.of(row.toVector).fold(_ => VoxelOutcome.Failed, VoxelOutcome.Estimated(_))
    }
