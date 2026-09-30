package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import java.util.concurrent.{Executors, TimeUnit}
import java.util.concurrent.atomic.AtomicInteger
import scala.jdk.CollectionConverters.*

/** Why the pilot runner refuses to start or to resume. */
enum PilotRefusal(val message: String):
  case DirtyWorktree(detail: String) extends PilotRefusal(s"worktree has uncommitted changes: $detail")
  case CellsManifestChanged(found: String) extends PilotRefusal(s"cells.json sha256 $found is not the frozen manifest")
  case StampMismatch(field: String) extends PilotRefusal(s"existing output was produced under a different stamp (field $field); refusing to resume")
  case ProjectionExceedsCeiling(projected: Double, ceiling: Double)
      extends PilotRefusal(f"projected $projected%.3f core-hours exceeds the ceiling $ceiling%.3f")
  case Failure(detail: String) extends PilotRefusal(detail)

/** Everything a pilot run depends on; a resume must reproduce it field for field. */
final case class PilotStamp(fields: Vector[(String, String)]):
  def json: String = fields.map((k, v) => s"\"$k\":\"$v\"").mkString("{", ",", "}")

  def firstDifference(other: PilotStamp): Option[String] =
    val a = fields.toMap
    val b = other.fields.toMap
    (a.keySet ++ b.keySet).toVector.sorted.find(k => a.get(k) != b.get(k))

/** The pilot configuration. `PilotConfig.declared` is the §6 pilot; tests use tiny harness-seed configs. */
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
    selectConfirmation: Boolean
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
    selectConfirmation = true
  )

final case class PilotReport(projectedCoreHours: Double, cellsRun: Int, cellsSkipped: Int, selectionFile: Option[Path])

/** Opt-in pilot runner (declaration §6). It never prints rates: stdout gets only the projection, progress
  * counts and file paths. Outputs, under `config.output`:
  *   stamp.json                         the run stamp (git SHA, manifest-v2, cells.json, production group sources,
  *                                      build.sbt, run configuration); a resume must match it exactly
  *   cells/<cell>.<null|power>.jsonl    one header line (stamp) + one canonical JSON line per study and scheme
  *   cells/<cell>.<stream>.jsonl.sha256
  *   summaries/<cell>.<stream>.json     descriptive per-scheme counts (files only, never printed)
  *   selection.json (+ .sha256)         the six confirmation cells chosen by ConfirmationSelection.select
  */
object PilotRunner:
  val Alpha = 0.05

  private def sha(bytes: Array[Byte]): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString

  private def shaFile(p: Path): String = sha(Files.readAllBytes(p))

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

  /** The run stamp. Production sources: every .scala file under modules/group/{shared,jvm,js}/src/main. */
  def stamp(config: PilotConfig): Either[PilotRefusal, PilotStamp] =
    val repo = config.repo
    val tools = repo.resolve("tools/group-bootstrap-research")
    for
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

  private def header(stamp: PilotStamp, cell: Cell, stream: String): String =
    s"""{"stamp":${stamp.json},"cell":"${cell.id.value}","stream":"$stream"}"""

  /** A cell stream is complete when its file and sha256 exist and agree; a stamp difference refuses. */
  private def complete(config: PilotConfig, stamp: PilotStamp, cell: Cell, stream: String): Either[PilotRefusal, Boolean] =
    val file = cellFile(config, cell, stream)
    val shaPath = Paths.get(file.toString + ".sha256")
    if !Files.exists(file) || !Files.exists(shaPath) then Right(false)
    else if new String(Files.readAllBytes(shaPath), UTF_8).trim.split("\\s+").head != shaFile(file) then Right(false)
    else
      val first = Files.lines(file)
      try
        if first.findFirst().orElse("") == header(stamp, cell, stream) then Right(true)
        else Left(PilotRefusal.StampMismatch(s"${cell.id.value}.$stream header"))
      finally first.close()

  private def runCell(config: PilotConfig, stamp: PilotStamp, cell: Cell, stream: String): Unit =
    val (purpose, schemes) = stream match
      case "null" => (StudyPurpose.Null, config.nullSchemes)
      case _ => (StudyPurpose.Power, config.powerSchemes)
    val engine = new BootstrapEngine(cell.researchDesign)
    val file = cellFile(config, cell, stream)
    val partial = Paths.get(file.toString + ".partial")
    val counts = scala.collection.mutable.LinkedHashMap.empty[String, (Int, Int)]
    val writer = Files.newBufferedWriter(partial, UTF_8)
    try
      writer.write(header(stamp, cell, stream))
      writer.write("\n")
      (0 until config.studies).foreach { i =>
        val record = StudyRunner.run(cell, config.phase, purpose, i, config.draws, schemes, engine)
        records(record, config.draws).foreach { line =>
          writer.write(line)
          writer.write("\n")
        }
        record.schemes.foreach { (s, r) =>
          val v = StudyVerdict.of(r, Alpha)
          val (n, k) = counts.getOrElse(s.code, (0, 0))
          counts(s.code) = (n + 1, k + (if purpose == StudyPurpose.Null then (if v.levelRejects then 1 else 0) else (if v.powerRejects then 1 else 0)))
        }
      }
    finally writer.close()
    val _ = Files.move(partial, file, StandardCopyOption.REPLACE_EXISTING, StandardCopyOption.ATOMIC_MOVE)
    Files.write(Paths.get(file.toString + ".sha256"), s"${shaFile(file)}  ${file.getFileName}\n".getBytes(UTF_8))
    val summary = config.output.resolve("summaries").resolve(s"${cell.id.value}.$stream.json")
    val body = counts.map((code, nk) => s""""$code":{"studies":${nk._1},"${if purpose == StudyPurpose.Null then "level_rejections" else "power_rejections"}":${nk._2}}""")
    val _ = Files.write(summary, s"""{"stamp":${stamp.json},"cell":"${cell.id.value}","stream":"$stream","schemes":{${body.mkString(",")}}}\n""".getBytes(UTF_8))

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
            Files.write(stampPath, (current.json + "\n").getBytes(UTF_8))
            Right(())
        existing.flatMap { _ =>
          val work = config.cells.flatMap(c => Vector(c -> "null") ++ (if config.powerCells.contains(c.id) then Vector(c -> "power") else Vector.empty))
          work.foldLeft[Either[PilotRefusal, Vector[(Cell, String)]]](Right(Vector.empty)) { (acc, cs) =>
            acc.flatMap(todo => complete(config, current, cs._1, cs._2).map(done => if done then todo else todo :+ cs))
          }.flatMap { todo =>
            val skipped = work.length - todo.length
            val pool = Executors.newFixedThreadPool(config.threads)
            val done = new AtomicInteger(0)
            val failures = new java.util.concurrent.ConcurrentLinkedQueue[String]()
            todo.foreach { (cell, stream) =>
              val task: Runnable = () =>
                try runCell(config, current, cell, stream)
                catch case e: Throwable => failures.add(s"${cell.id.value}.$stream: $e"): Unit
                log(s"PILOT_PROGRESS,done=${done.incrementAndGet()}/${todo.length},skipped=$skipped")
              val _ = pool.submit(task)
            }
            pool.shutdown()
            val _ = pool.awaitTermination(7, TimeUnit.DAYS)
            if !failures.isEmpty then Left(PilotRefusal.Failure(failures.asScala.mkString("; ")))
            else
              val selection =
                if !config.selectConfirmation then Right(None)
                else
                  val pool = SelectionRule.Owner.pool.cells.filter(c => config.cells.contains(c))
                  ConfirmationSelection.select(pilotEvidence(config, pool), SelectionRule.Owner, config.studies)
                    .left.map(e => PilotRefusal.Failure(s"selection: ${e.message}"))
                    .map { ids =>
                      val path = config.output.resolve("selection.json")
                      val text = s"""{"stamp":${current.json},"rule":"${SelectionRule.Version}","selected":[${ids.map(i => "\"" + i.value + "\"").mkString(",")}]}\n"""
                      Files.write(path, text.getBytes(UTF_8))
                      Files.write(Paths.get(path.toString + ".sha256"), s"${shaFile(path)}  selection.json\n".getBytes(UTF_8))
                      log(s"PILOT_SELECTION_WRITTEN,$path")
                      Some(path)
                    }
              selection.map { sel =>
                log(s"PILOT_DONE,cells_run=${todo.length},cells_skipped=$skipped,output=${config.output}")
                PilotReport(projected, todo.length, skipped, sel)
              }
          }
        }
    }

/** Opt-in entry points. Projection only (harness-seed calibration, no pilot roots, no output):
  *   -Dscalafim.group.bootstrapPilot.projectOnly=true
  * The pilot itself (declared config; requires a clean worktree, the ceiling of 15 core-hours):
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
