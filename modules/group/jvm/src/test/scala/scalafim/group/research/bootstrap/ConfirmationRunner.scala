package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Confirmation mode (declaration §6). It reuses `PilotRunner` with `PilotConfig.confirmation` (Phase.Confirmation
  * roots, 12 core cells, R = 20000, B = 999, power stream on the four declared power cells), then aggregates the
  * durable per-study records into `CellEvidence` / `PowerEvidence` and applies `Decision.outcome` to each candidate.
  * It never runs `ConfirmationSelection`. Output, in addition to the runner's files under `config.output`:
  *   outcomes.json (+ .sha256)   per candidate: outcome class and per-cell Pass/Fail/Unresolved labels
  * Stdout carries only the runner's progress lines and `CONFIRMATION_OUTCOMES_WRITTEN,<path>`; no rates.
  */
object ConfirmationRunner:
  type Decide = (Vector[CellEvidence], Vector[PowerEvidence]) => Either[DecisionRefusal, CandidateOutcome]

  val Candidates: Vector[Scheme] = Scheme.values.toVector.filter(_.role == SchemeRole.Candidate)

  private val Line = "\"study\":(\\d+),\"scheme\":\"([^\"]+)\",\"verdict\":\"([A-Za-z]+)\"".r

  private def records(file: Path): Vector[(Int, String, String)] =
    val lines = Files.readAllLines(file, UTF_8).asScala.toVector.drop(1)
    lines.flatMap(l => Line.findFirstMatchIn(l).map(m => (m.group(1).toInt, m.group(2), m.group(3))))

  private def cellFile(config: PilotConfig, cell: Cell, stream: String): Path =
    config.output.resolve("cells").resolve(s"${cell.id.value}.$stream.jsonl")

  /** Per candidate scheme: one `CellEvidence` per cell from the null stream. */
  def cellEvidence(config: PilotConfig): Map[Scheme, Vector[CellEvidence]] =
    val perCell = config.cells.map { cell =>
      val parsed = records(cellFile(config, cell, "null"))
      cell -> Candidates.map(s => s -> parsed.filter(_._2 == s.code).sortBy(_._1).map(r => StudyVerdict.valueOf(r._3))).toMap
    }
    Candidates.map(s => s -> perCell.map((cell, v) => CellEvidence.aggregate(cell, v(s)))).toMap

  /** Per candidate scheme: one `PowerEvidence` per power cell, pairing the candidate with the native comparator. */
  def powerEvidence(config: PilotConfig): Map[Scheme, Vector[PowerEvidence]] =
    val powerCells = config.cells.filter(c => config.powerCells.contains(c.id))
    val perCell = powerCells.map { cell =>
      val parsed = records(cellFile(config, cell, "power"))
      val comparator = parsed.filter(_._2 == "native-pm-mkh").map(r => r._1 -> ComparatorVerdict.valueOf(r._3)).toMap
      cell -> Candidates.map { s =>
        s -> parsed.filter(_._2 == s.code).sortBy(_._1).map(r => (StudyVerdict.valueOf(r._3), comparator(r._1)))
      }.toMap
    }
    Candidates.map(s => s -> perCell.map((cell, v) => PowerEvidence.aggregate(cell, v(s)))).toMap

  private def q(s: String): String = "\"" + s + "\""

  private def outcomeJson(o: CandidateOutcome): String = o match
    case CandidateOutcome.Adopt => """{"class":"Adopt"}"""
    case CandidateOutcome.Bound(fs) => s"""{"class":"Bound","subfamilies":[${fs.map(f => q(f.label)).mkString(",")}]}"""
    case CandidateOutcome.Decline => """{"class":"Decline"}"""
    case CandidateOutcome.Unresolved => """{"class":"Unresolved"}"""

  /** Pass / Fail / Unresolved for one confirmation cell: Fail if the null criterion fails, Pass if the null and
    * failure criteria both pass, Unresolved otherwise. Labels only; counts and rates are never written.
    */
  def cellLabel(e: CellEvidence): String =
    if Decision.nullVerdict(e.nullRejections, e.studies) == NullVerdict.Fail then "Fail"
    else if Decision.passes(e) then "Pass"
    else "Unresolved"

  private def powerLabels(e: PowerEvidence): String =
    val v = Decision.powerVerdict(e)
    s"""{"gain":${v.gain},"non_loss":${v.nonLoss},"definite_loss":${v.definiteLoss}}"""

  /** outcomes.json body for the given evidence; `decide` is `Decision.outcome` in production. */
  def outcomesJson(stamp: PilotStamp, cells: Map[Scheme, Vector[CellEvidence]], power: Map[Scheme, Vector[PowerEvidence]], decide: Decide): Either[PilotRefusal, String] =
    Candidates.foldLeft[Either[PilotRefusal, Vector[String]]](Right(Vector.empty)) { (acc, s) =>
      acc.flatMap { done =>
        decide(cells(s), power(s)).left.map(r => PilotRefusal.Failure(s"decision refused for ${s.code}: ${r.message}")).map { o =>
          val cellPart = cells(s).map(e => s"${q(e.cell.id.value)}:${q(cellLabel(e))}").mkString("{", ",", "}")
          val powerPart = power(s).map(e => s"${q(e.cell.id.value)}:${powerLabels(e)}").mkString("{", ",", "}")
          done :+ s"""${q(s.code)}:{"outcome":${outcomeJson(o)},"cells":$cellPart,"power_cells":$powerPart}"""
        }
      }
    }.map(parts => s"""{"stamp":${stamp.json},"rule":"decision-rule/v2","candidates":{${parts.mkString(",")}}}""" + "\n")

  /** Aggregates the completed output and writes outcomes.json (+ .sha256) atomically. */
  def writeOutcomes(config: PilotConfig, stamp: PilotStamp, decide: Decide = Decision.outcome): Either[PilotRefusal, Path] =
    outcomesJson(stamp, cellEvidence(config), powerEvidence(config), decide).map { text =>
      val path = config.output.resolve("outcomes.json")
      PilotRunner.writeAtomic(path, text)
      val digest = java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path)).map(b => f"${b & 0xff}%02x").mkString
      PilotRunner.writeAtomic(Paths.get(path.toString + ".sha256"), s"$digest  outcomes.json\n")
      path
    }

  /** Runs (or resumes) the cells, then decides. `decide` is a test hook; production uses `Decision.outcome`. */
  def run(config: PilotConfig, log: String => Unit, decide: Decide = Decision.outcome): Either[PilotRefusal, Path] =
    for
      _ <- PilotRunner.run(config, log)
      stamp <- PilotRunner.stamp(config)
      path <- writeOutcomes(config, stamp, decide)
    yield
      log(s"CONFIRMATION_OUTCOMES_WRITTEN,$path")
      path

/** Opt-in entry points (never run by default). Projection only (harness-seed calibration, no output):
  *   -Dscalafim.group.bootstrapConfirmation.projectOnly=true
  * The confirmation (clean worktree, no redirected build, ceilings 15 core-hours):
  *   -Dscalafim.group.bootstrapConfirmation.run=true [-Dscalafim.group.bootstrapConfirmation.threads=4]
  */
class ConfirmationLaunch extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(7, "d")

  private def repo: Path = Paths.get(sys.props.getOrElse("user.dir", "."))
  private def threads: Int = sys.props.get("scalafim.group.bootstrapConfirmation.threads").map(_.toInt).getOrElse(4)
  private def config: PilotConfig =
    PilotConfig.confirmation(repo, threads, PilotConfig.DeclaredOutput.resolve("selection.json")).fold(r => fail(r.message), identity)

  test("confirmation core-hour projection from a harness-seed calibration (opt-in)"):
    assume(sys.props.get("scalafim.group.bootstrapConfirmation.projectOnly").contains("true"), "pass -Dscalafim.group.bootstrapConfirmation.projectOnly=true")
    val c = config
    println(f"CONFIRMATION_PROJECTION,core_hours=${PilotRunner.projectCoreHours(c)}%.3f,ceiling=${c.ceilingCoreHours}%.3f,cells=${c.cells.length},studies=${c.studies},draws=${c.draws}")

  test("run the declared confirmation (opt-in; never run by default)"):
    assume(sys.props.get("scalafim.group.bootstrapConfirmation.run").contains("true"), "pass -Dscalafim.group.bootstrapConfirmation.run=true")
    val result = ConfirmationRunner.run(config, line => println(line))
    assert(result.isRight, result.left.map(_.message).left.getOrElse(""))
