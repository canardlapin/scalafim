package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import scala.jdk.CollectionConverters.*

/** Why the pilot runner refuses to start, to resume, or to continue. */
enum PilotRefusal(val message: String):
  case DirtyWorktree(detail: String) extends PilotRefusal(s"worktree has uncommitted changes: $detail")
  case RedirectedBuild(properties: Vector[String])
      extends PilotRefusal(s"dependency build redirected by ${properties.mkString(", ")}; a redirected build must not produce pilot evidence")
  case CellsManifestChanged(found: String) extends PilotRefusal(s"cells.json sha256 $found is not the frozen manifest")
  case StampMismatch(field: String) extends PilotRefusal(s"existing output was produced under a different stamp (field $field); refusing to resume")
  case ProjectionExceedsCeiling(projected: Double, ceiling: Double)
      extends PilotRefusal(f"projected $projected%.3f core-hours exceeds the ceiling $ceiling%.3f")
  case CpuCeilingReached(cpuSeconds: Double, ceilingCoreHours: Double)
      extends PilotRefusal(f"process CPU time $cpuSeconds%.1f s reached the ceiling of $ceilingCoreHours%.3f core-hours; stopped (resumable)")
  case Failure(detail: String) extends PilotRefusal(detail)

/** Everything a pilot run depends on; a resume must reproduce it field for field. */
final case class PilotStamp(fields: Vector[(String, String)]):
  def json: String = fields.map((k, v) => s"\"$k\":\"$v\"").mkString("{", ",", "}")

  def firstDifference(other: PilotStamp): Option[String] =
    val a = fields.toMap
    val b = other.fields.toMap
    (a.keySet ++ b.keySet).toVector.sorted.find(k => a.get(k) != b.get(k))

/** The pilot configuration. `PilotConfig.declared` is the §6 pilot; tests use tiny harness-seed configs.
  * `ceilingCoreHours` bounds the projection before launch; `runtimeCeilingCoreHours` bounds the measured process
  * CPU time of the whole run, summed over resumed invocations (owner rule: stop and ask if a run would exceed the
  * ceiling). Both are 15 when declared. Ceilings are not stamped (they do not change results), so an owner-approved
  * raise can resume the same output.
  */
final case class PilotConfig(
    repo: Path,
    output: Path,
    cells: Vector[Cell],
    powerCells: Set[CellId],
    studies: Int,
    draws: Int,
    phase: Phase,
    nullSchemes: Vector[Scheme],
    powerSchemes: Vector[Scheme],
    threads: Int,
    ceilingCoreHours: Double,
    calibrationStudies: Int,
    requireCleanWorktree: Boolean,
    selectConfirmation: Boolean,
    runtimeCeilingCoreHours: Double = 15.0
):
  require(studies >= 1 && draws >= 1 && threads >= 1 && calibrationStudies >= 1, "positive sizes")

object PilotConfig:
  val DeclaredOutput: Path = Paths.get("/private/tmp/scalafim-execution-20260929/bootstrap-pilot-20260930")

  /** Declaration §6 pilot: 117 cells, R = 2000, B = 499, pilot roots 2026100101-05, power stream on the 90 core cells. */
  def declared(repo: Path, threads: Int): PilotConfig = PilotConfig(
    repo = repo,
    output = DeclaredOutput,
    cells = Cell.all,
    powerCells = Cell.core.map(_.id).toSet,
    studies = 2000,
    draws = 499,
    phase = Phase.Pilot,
    nullSchemes = Scheme.values.toVector,
    powerSchemes = Scheme.values.toVector.filter(_.role == SchemeRole.Candidate),
    threads = threads,
    ceilingCoreHours = 15.0,
    calibrationStudies = 2,
    requireCleanWorktree = true,
    selectConfirmation = true,
    runtimeCeilingCoreHours = 15.0
  )

final case class PilotReport(projectedCoreHours: Double, cellsRun: Int, cellsSkipped: Int, selectionFile: Option[Path], cpuSeconds: Double, wallSeconds: Double)

/** Opt-in pilot runner (declaration §6). It never prints rates: stdout gets only the projection, progress
  * counts, cost numbers and file paths. Every file is written atomically (temp file + ATOMIC_MOVE). Outputs,
  * under `config.output`:
  *   stamp.json                         the run stamp (git SHA, manifest-v2, cells.json, production group sources,
  *                                      build.sbt, JVM, run configuration); a resume must match it exactly
  *   cells/<cell>.<null|power>.jsonl    one header line (stamp) + one canonical JSON line per study and scheme
  *   summaries/<cell>.<stream>.json     descriptive per-scheme counts (files only, never printed), written
  *                                      before the cell's .sha256, and regenerated on resume when missing
  *   cells/<cell>.<stream>.jsonl.sha256 written last: its presence means the cell stream is complete
  *   run-cost.json                      process CPU seconds and wall seconds of the latest invocation
  *   selection.json (+ .sha256)         the six confirmation cells chosen by ConfirmationSelection.select
  */
object PilotRunner:
  val Alpha = 0.05

  private def sha(bytes: Array[Byte]): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  private def shaFile(p: Path): String = sha(Files.readAllBytes(p))

  /** Writes via a temp file in the same directory and an atomic move. */
  def writeAtomic(path: Path, text: String): Unit =
    val tmp = Files.createTempFile(path.getParent, path.getFileName.toString + ".", ".tmp")
    Files.write(tmp, text.getBytes(UTF_8))
    val _ = Files.move(tmp, path, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)

  private def git(repo: Path, args: String*): Either[PilotRefusal, String] =
    try
      val process = new ProcessBuilder(("git" +: "-C" +: repo.toString +: args)*).redirectErrorStream(true).start()
      val out = new String(process.getInputStream.readAllBytes(), UTF_8).trim
      if process.waitFor() == 0 then Right(out) else Left(PilotRefusal.Failure(s"git ${args.mkString(" ")} failed: $out"))
    catch case e: java.io.IOException => Left(PilotRefusal.Failure(s"git unavailable: ${e.getMessage}"))

  private def listFiles(root: Path, suffix: String): Vector[Path] =
    if !Files.exists(root) then Vector.empty
    else
      val stream = Files.walk(root)
      try stream.iterator().asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(suffix)).toVector.sortBy(_.toString)
      finally stream.close()

  /** System properties that redirect a source dependency to a local checkout (build.sbt `scalafim.<dep>.build`). */
  def redirectedBuildProperties: Vector[String] =
    sys.props.keySet.toVector.filter(k => k.startsWith("scalafim.") && k.endsWith(".build")).sorted

  /** The run stamp. Production sources: every .scala file under modules/group/{shared,jvm,js}/src/main. */
  def stamp(config: PilotConfig): Either[PilotRefusal, PilotStamp] =
    val repo = config.repo
    val tools = repo.resolve("tools/group-bootstrap-research")
    val redirected = redirectedBuildProperties
    for
      _ <- if redirected.nonEmpty then Left(PilotRefusal.RedirectedBuild(redirected)) else Right(())
      head <- git(repo, "rev-parse", "HEAD")
      status <- git(repo, "status", "--porcelain")
      _ <- if config.requireCleanWorktree && status.nonEmpty then Left(PilotRefusal.DirtyWorktree(status.linesIterator.take(5).mkString("; "))) else Right(())
      cells = shaFile(tools.resolve("cells.json"))
      _ <- if cells != CellManifest.sha256 then Left(PilotRefusal.CellsManifestChanged(cells)) else Right(())
    yield
      val production = Vector("shared", "jvm", "js").flatMap(p => listFiles(repo.resolve(s"modules/group/$p/src/main"), ".scala"))
      val productionDigest = sha(production.map(p => s"${repo.relativize(p)} ${shaFile(p)}\n").mkString.getBytes(UTF_8))
      PilotStamp(Vector(
        "git_sha" -> head,
        "git_clean" -> status.isEmpty.toString,
        "manifest_v2_sha256" -> shaFile(tools.resolve("manifest-v2.json")),
        "cells_json_sha256" -> cells,
        "group_main_sources_sha256" -> productionDigest,
        "group_main_sources_count" -> production.length.toString,
        "build_sbt_sha256" -> shaFile(repo.resolve("build.sbt")),
        "java_version" -> sys.props.getOrElse("java.version", "unknown"),
        "java_vendor" -> sys.props.getOrElse("java.vendor", "unknown"),
        "java_vm_version" -> sys.props.getOrElse("java.vm.version", "unknown"),
        "redirected_builds" -> "none",
        "phase" -> config.phase.toString,
        "roots" -> StreamKind.values.map(config.phase.root).mkString("-"),
        "studies" -> config.studies.toString,
        "draws" -> config.draws.toString,
        "cells" -> sha(config.cells.map(_.id.value).mkString(",").getBytes(UTF_8)),
        "power_cells" -> sha(config.powerCells.toVector.map(_.value).sorted.mkString(",").getBytes(UTF_8)),
        "null_schemes" -> config.nullSchemes.map(_.code).mkString("+"),
        "power_schemes" -> config.powerSchemes.map(_.code).mkString("+"),
        "selection_rule" -> SelectionRule.Version
      ))

  /** Process CPU time in ns (all JVM threads, conservative), or None when the platform bean is unavailable. */
  def processCpuNanos(): Option[Long] =
    java.lang.management.ManagementFactory.getOperatingSystemMXBean match
      case bean: com.sun.management.OperatingSystemMXBean => Option(bean.getProcessCpuTime).filter(_ >= 0L)
      case _ => None

  private def num(d: Double): String = if d.isFinite then java.lang.Double.toString(d) else "null"
  private def str(s: String): String = "\"" + s.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  private def baseline(study: Int, scheme: String, r: Either[String, BaselineResult]): String = r match
    case Right(b) =>
      val verdict = if b.pValue <= Alpha then "Reject" else "Retain"
      s"""{"study":$study,"scheme":"$scheme","verdict":"$verdict","T":${num(b.statistic)},"p":${num(b.pValue)},"failure":null}"""
    case Left(e) => s"""{"study":$study,"scheme":"$scheme","verdict":"Failed","T":null,"p":null,"failure":${str(e)}}"""

  /** Canonical record lines for one study (one line per scheme, baseline and the study itself). */
  def records(record: StudyRecord, draws: Int): Vector[String] =
    val s = record.study.index
    val schemes = record.schemes.map { (scheme, result) =>
      val verdict = StudyVerdict.of(result, Alpha).toString
      result match
        case Right(o) =>
          s"""{"study":$s,"scheme":"${scheme.code}","verdict":"$verdict","k":${o.exceed},"f":${o.failed},"B":$draws,"T":${num(o.statistic)},"failure":null}"""
        case Left(e) =>
          s"""{"study":$s,"scheme":"${scheme.code}","verdict":"$verdict","k":null,"f":null,"B":$draws,"T":null,"failure":${str(e.message)}}"""
    }
    val native = record.native match
      case Right(c) =>
        val verdict = ComparatorVerdict.of(record, Alpha).toString
        s"""{"study":$s,"scheme":"native-pm-mkh","verdict":"$verdict","T":${num(c.statistic)},"p":${num(c.pValue)},"failure":${if c.failed then str("native sample failure") else "null"}}"""
      case Left(e) => s"""{"study":$s,"scheme":"native-pm-mkh","verdict":"Failed","T":null,"p":null,"failure":${str(e)}}"""
    val signFlip = record.signFlip.map { p =>
      s"""{"study":$s,"scheme":"sign-flip","verdict":"${if p <= Alpha then "Reject" else "Retain"}","T":null,"p":${num(p)},"failure":null}"""
    }
    val info = s"""{"study":$s,"scheme":"study","mean_log_v_ratio":${num(record.study.meanLogVarianceRatio)}}"""
    schemes ++ Vector(native, baseline(s, "hc3-equal", record.hc3Equal), baseline(s, "hc3-inverse-v", record.hc3InverseV),
      baseline(s, "oracle-z", record.oracle)) ++ signFlip.toVector :+ info

  /** Single-threaded per-study cost on harness seeds (never pilot roots), projected to the configured run. */
  def projectCoreHours(config: PilotConfig): Double =
    val nanos = config.cells.map { cell =>
      val engine = new BootstrapEngine(cell.researchDesign)
      def cost(purpose: StudyPurpose, schemes: Vector[Scheme]): Double =
        StudyRunner.run(cell, Phase.Harness, purpose, 900000, config.draws, schemes, engine) // warm-up, not timed
        val start = System.nanoTime()
        (0 until config.calibrationStudies).foreach(i => StudyRunner.run(cell, Phase.Harness, purpose, 900001 + i, config.draws, schemes, engine))
        (System.nanoTime() - start).toDouble / config.calibrationStudies
      val nullCost = cost(StudyPurpose.Null, config.nullSchemes) * config.studies
      val powerCost = if config.powerCells.contains(cell.id) then cost(StudyPurpose.Power, config.powerSchemes) * config.studies else 0.0
      nullCost + powerCost
    }.sum
    nanos / 3.6e12


  private def cellFile(config: PilotConfig, cell: Cell, stream: String): Path =
    config.output.resolve("cells").resolve(s"${cell.id.value}.$stream.jsonl")

  private def summaryFile(config: PilotConfig, cell: Cell, stream: String): Path =
    config.output.resolve("summaries").resolve(s"${cell.id.value}.$stream.json")

  private def header(stamp: PilotStamp, cell: Cell, stream: String): String =
    s"""{"stamp":${stamp.json},"cell":"${cell.id.value}","stream":"$stream"}"""

  /** Descriptive per-scheme counts, rebuilt from the durable records (so a missing summary can be regenerated). */
  private def writeSummary(config: PilotConfig, stamp: PilotStamp, cell: Cell, stream: String): Unit =
    val line = "\"scheme\":\"([^\"]+)\",\"verdict\":\"([A-Za-z]+)\",\"k\":".r
    val counts = scala.collection.mutable.LinkedHashMap.empty[String, (Int, Int)]
    val lines = Files.readAllLines(cellFile(config, cell, stream), UTF_8).asScala.iterator.drop(1)
    lines.flatMap(l => line.findFirstMatchIn(l)).foreach { m =>
      val v = StudyVerdict.valueOf(m.group(2))
      val (n, k) = counts.getOrElse(m.group(1), (0, 0))
      counts(m.group(1)) = (n + 1, k + (if stream == "null" then (if v.levelRejects then 1 else 0) else (if v.powerRejects then 1 else 0)))
    }
    val label = if stream == "null" then "level_rejections" else "power_rejections"
    val body = counts.map((code, nk) => s""""$code":{"studies":${nk._1},"$label":${nk._2}}""")
    writeAtomic(summaryFile(config, cell, stream), s"""{"stamp":${stamp.json},"cell":"${cell.id.value}","stream":"$stream","schemes":{${body.mkString(",")}}}\n""")

  /** A cell stream is complete when its file and sha256 exist and agree; a stamp difference refuses; a missing
    * summary of a complete cell is regenerated from its records.
    */
  private def complete(config: PilotConfig, stamp: PilotStamp, cell: Cell, stream: String): Either[PilotRefusal, Boolean] =
    val file = cellFile(config, cell, stream)
    val shaPath = Paths.get(file.toString + ".sha256")
    if !Files.exists(file) || !Files.exists(shaPath) then Right(false)
    else if new String(Files.readAllBytes(shaPath), UTF_8).trim.split("\\s+").head != shaFile(file) then Right(false)
    else
      val first = Files.lines(file)
      val headerOk = try first.findFirst().orElse("") == header(stamp, cell, stream) finally first.close()
      if !headerOk then Left(PilotRefusal.StampMismatch(s"${cell.id.value}.$stream header"))
      else
        if !Files.exists(summaryFile(config, cell, stream)) then writeSummary(config, stamp, cell, stream)
        Right(true)

  /** Runs one cell stream. Returns false, leaving no partial data, if `abort` turns true between studies. */
  private def runCell(config: PilotConfig, stamp: PilotStamp, cell: Cell, stream: String, abort: () => Boolean): Boolean =
    val (purpose, schemes) = stream match
      case "null" => (StudyPurpose.Null, config.nullSchemes)
      case _ => (StudyPurpose.Power, config.powerSchemes)
    val engine = new BootstrapEngine(cell.researchDesign)
    val file = cellFile(config, cell, stream)
    val partial = Files.createTempFile(file.getParent, file.getFileName.toString + ".", ".partial")
    var aborted = false
    val writer = Files.newBufferedWriter(partial, UTF_8)
    try
      writer.write(header(stamp, cell, stream))
      writer.write("\n")
      var i = 0
      while i < config.studies && !aborted do
        if abort() then aborted = true
        else
          val record = StudyRunner.run(cell, config.phase, purpose, i, config.draws, schemes, engine)
          records(record, config.draws).foreach { line =>
            writer.write(line)
            writer.write("\n")
          }
          i += 1
    catch
      case e: Throwable =>
        writer.close()
        Files.deleteIfExists(partial): Unit
        throw e
    finally writer.close()
    if aborted then
      Files.deleteIfExists(partial): Unit
      false
    else
      val _ = Files.move(partial, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
      writeSummary(config, stamp, cell, stream)
      writeAtomic(Paths.get(file.toString + ".sha256"), s"${shaFile(file)}  ${file.getFileName}\n")
      true

  /** Candidate verdicts of every core cell's null stream, read back from the durable records. */
  def pilotEvidence(config: PilotConfig, cells: Vector[Cell]): Vector[PilotEvidence] =
    val line = "\"scheme\":\"([^\"]+)\",\"verdict\":\"([A-Za-z]+)\"".r
    val candidates = Scheme.values.toVector.filter(_.role == SchemeRole.Candidate)
    cells.map { cell =>
      val parsed = Files.readAllLines(cellFile(config, cell, "null"), UTF_8).asScala.toVector.drop(1).flatMap(l => line.findFirstMatchIn(l))
      val verdicts = candidates.map { s =>
        s -> parsed.filter(_.group(1) == s.code).map(m => StudyVerdict.valueOf(m.group(2)))
      }.toMap
      PilotEvidence(cell, verdicts)
    }

  def run(config: PilotConfig, log: String => Unit): Either[PilotRefusal, PilotReport] =
    val wallStart = System.nanoTime()
    val cpuStart = processCpuNanos()
    val threadBean = java.lang.management.ManagementFactory.getThreadMXBean
    val workerCpu = new AtomicLong(0L)
    // Process CPU (every JVM thread, conservative) when available; otherwise CPU summed over the pool threads.
    def invocationCpuNanos(): Long = cpuStart.flatMap(s => processCpuNanos().map(_ - s)).getOrElse(workerCpu.get)
    // CPU of earlier invocations of this output (from run-cost.json), so the ceiling bounds the whole run.
    var priorCpuNanos = 0L
    def cpuUsedNanos(): Long = priorCpuNanos + invocationCpuNanos()
    val cpuSource = if cpuStart.isDefined then "process" else "pool-threads"
    val ceilingNanos = config.runtimeCeilingCoreHours * 3.6e12
    def seconds(ns: Double): String = f"${ns / 1e9}%.3f"
    def writeCost(status: String, cellsRun: Int): (Double, Double) =
      val cpu = invocationCpuNanos().toDouble
      val total = priorCpuNanos.toDouble + cpu
      val wall = (System.nanoTime() - wallStart).toDouble
      writeAtomic(config.output.resolve("run-cost.json"),
        s"""{"status":"$status","cpu_source":"$cpuSource","cpu_seconds":${seconds(cpu)},"cpu_seconds_total":${seconds(total)},"wall_seconds":${seconds(wall)},"cells_run":$cellsRun,"runtime_ceiling_core_hours":${config.runtimeCeilingCoreHours}}""" + "\n")
      (cpu / 1e9, wall / 1e9)
    stamp(config).flatMap { current =>
      val projected = projectCoreHours(config)
      log(f"PILOT_PROJECTION,core_hours=$projected%.3f,ceiling=${config.ceilingCoreHours}%.3f,cells=${config.cells.length},studies=${config.studies},draws=${config.draws},threads=${config.threads}")
      if projected > config.ceilingCoreHours then Left(PilotRefusal.ProjectionExceedsCeiling(projected, config.ceilingCoreHours))
      else
        Files.createDirectories(config.output.resolve("cells"))
        Files.createDirectories(config.output.resolve("summaries"))
        val stampPath = config.output.resolve("stamp.json")
        val existing =
          if Files.exists(stampPath) then
            val text = new String(Files.readAllBytes(stampPath), UTF_8).trim
            if text == current.json then Right(())
            else
              val pairs = "\"([^\"]+)\":\"([^\"]*)\"".r.findAllMatchIn(text).map(m => m.group(1) -> m.group(2)).toVector
              Left(PilotRefusal.StampMismatch(PilotStamp(pairs).firstDifference(current).getOrElse("stamp")))
          else
            writeAtomic(stampPath, current.json + "\n")
            Right(())
        existing.flatMap { _ =>
          val costPath = config.output.resolve("run-cost.json")
          if Files.exists(costPath) then
            val prior = "\"cpu_seconds_total\":([0-9.]+)".r.findFirstMatchIn(new String(Files.readAllBytes(costPath), UTF_8))
            priorCpuNanos = prior.fold(0L)(m => (m.group(1).toDouble * 1e9).toLong)
          val work = config.cells.flatMap(c => Vector(c -> "null") ++ (if config.powerCells.contains(c.id) then Vector(c -> "power") else Vector.empty))
          work.foldLeft[Either[PilotRefusal, Vector[(Cell, String)]]](Right(Vector.empty)) { (acc, cs) =>
            acc.flatMap(todo => complete(config, current, cs._1, cs._2).map(done => if done then todo else todo :+ cs))
          }.flatMap { todo =>
            val skipped = work.length - todo.length
            val pool = Executors.newFixedThreadPool(config.threads)
            val next = new AtomicInteger(0)
            val done = new AtomicInteger(0)
            val stop = new AtomicBoolean(false)
            val costLock = new Object
            val failures = new java.util.concurrent.ConcurrentLinkedQueue[String]()
            def overCeiling(): Boolean =
              if cpuUsedNanos() > ceilingNanos then stop.set(true)
              stop.get
            val worker: Runnable = () =>
              var mine = threadBean.getCurrentThreadCpuTime
              var more = true
              while more do
                if overCeiling() then more = false
                else
                  val i = next.getAndIncrement()
                  if i >= todo.length then more = false
                  else
                    val (cell, stream) = todo(i)
                    val finished =
                      try runCell(config, current, cell, stream, () => overCeiling())
                      catch
                        case e: Throwable =>
                          failures.add(s"${cell.id.value}.$stream: $e"): Unit
                          false
                    val now = threadBean.getCurrentThreadCpuTime
                    workerCpu.addAndGet(now - mine): Unit
                    mine = now
                    if finished then
                      log(s"PILOT_PROGRESS,done=${done.incrementAndGet()}/${todo.length},skipped=$skipped")
                      // Checkpoint the cost after every completed cell, so a killed run still bounds a resume.
                      costLock.synchronized(writeCost("running", done.get)): Unit
            (0 until config.threads).foreach(_ => pool.submit(worker): Unit)
            pool.shutdown()
            val _ = pool.awaitTermination(7, TimeUnit.DAYS)
            if !failures.isEmpty then
              writeCost("failed", done.get): Unit
              Left(PilotRefusal.Failure(failures.asScala.mkString("; ")))
            else if stop.get then
              val (cpu, wall) = writeCost("stopped-at-ceiling", done.get)
              log(f"PILOT_STOPPED,cpu_seconds=$cpu%.3f,cpu_seconds_total=${cpuUsedNanos() / 1e9}%.3f,wall_seconds=$wall%.3f,cells_run=${done.get},cells_skipped=$skipped")
              Left(PilotRefusal.CpuCeilingReached(cpuUsedNanos() / 1e9, config.runtimeCeilingCoreHours))
            else
              val selection =
                if !config.selectConfirmation then Right(None)
                else
                  val poolCells = SelectionRule.Owner.pool.cells.filter(c => config.cells.contains(c))
                  ConfirmationSelection.select(pilotEvidence(config, poolCells), SelectionRule.Owner, config.studies)
                    .left.map(e => PilotRefusal.Failure(s"selection: ${e.message}"))
                    .map { ids =>
                      val path = config.output.resolve("selection.json")
                      writeAtomic(path, s"""{"stamp":${current.json},"rule":"${SelectionRule.Version}","selected":[${ids.map(i => "\"" + i.value + "\"").mkString(",")}]}\n""")
                      writeAtomic(Paths.get(path.toString + ".sha256"), s"${shaFile(path)}  selection.json\n")
                      log(s"PILOT_SELECTION_WRITTEN,$path")
                      Some(path)
                    }
              selection.map { sel =>
                val (cpu, wall) = writeCost("complete", todo.length)
                log(f"PILOT_DONE,cells_run=${todo.length},cells_skipped=$skipped,cpu_seconds=$cpu%.3f,cpu_seconds_total=${cpuUsedNanos() / 1e9}%.3f,wall_seconds=$wall%.3f,output=${config.output}")
                PilotReport(projected, todo.length, skipped, sel, cpu, wall)
              }
          }
        }
    }

/** Opt-in entry points. Projection only (harness-seed calibration, no pilot roots, no output):
  *   -Dscalafim.group.bootstrapPilot.projectOnly=true
  * The pilot itself (declared config; requires a clean worktree and no redirected build; ceilings 15 core-hours):
  *   -Dscalafim.group.bootstrapPilot.run=true [-Dscalafim.group.bootstrapPilot.threads=4]
  */
class PilotLaunch extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(7, "d")

  private def repo: Path = Paths.get(sys.props.getOrElse("user.dir", "."))
  private def threads: Int = sys.props.get("scalafim.group.bootstrapPilot.threads").map(_.toInt).getOrElse(4)

  test("pilot core-hour projection from a harness-seed calibration (opt-in)"):
    assume(sys.props.get("scalafim.group.bootstrapPilot.projectOnly").contains("true"), "pass -Dscalafim.group.bootstrapPilot.projectOnly=true")
    val config = PilotConfig.declared(repo, threads)
    println(f"PILOT_PROJECTION,core_hours=${PilotRunner.projectCoreHours(config)}%.3f,ceiling=${config.ceilingCoreHours}%.3f,cells=${config.cells.length},studies=${config.studies},draws=${config.draws}")

  test("run the declared pilot (opt-in; never run by default)"):
    assume(sys.props.get("scalafim.group.bootstrapPilot.run").contains("true"), "pass -Dscalafim.group.bootstrapPilot.run=true")
    val result = PilotRunner.run(PilotConfig.declared(repo, threads), line => println(line))
    assert(result.isRight, result.left.map(_.message).left.getOrElse(""))
