package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** Confirmation mode on tiny synthetic configurations: harness seeds only; the real confirmation roots are
  * asserted through the configuration and the stamp, never used to draw a study here.
  */
class ConfirmationRunnerSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val repo = Paths.get(sys.props.getOrElse("user.dir", "."))
  private val tiny = Vector("C-n8-DI-Vspread-T2-N8", "C-n20-DG-Vrev-T0-N8").map(ResearchTestSupport.cell)

  private def sha(bytes: Array[Byte]): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
  private def sha(p: Path): String = sha(Files.readAllBytes(p))
  private def lines(p: Path): Vector[String] = Files.readAllLines(p, UTF_8).asScala.toVector

  private def selectionFile(ids: Vector[String]): Path =
    val dir = Files.createTempDirectory("confirmation-selection-test")
    val p = dir.resolve("selection.json")
    Files.write(p, s"""{"rule":"x","selected":[${ids.map(i => "\"" + i + "\"").mkString(",")}]}\n""".getBytes(UTF_8))
    p

  private val sixPool: Vector[String] = SelectionPool.CoreMinusFixed.cells.take(6).map(_.id.value)

  /** A valid synthetic selection, accepted through the expected-hash hook. */
  private def syntheticConfig(): PilotConfig =
    val f = selectionFile(sixPool)
    PilotConfig.confirmation(repo, 2, f, sha(f)).fold(r => fail(r.message), identity)

  /** The synthetic confirmation config shrunk to a runnable tiny run on harness seeds. */
  private def tinyConfig(out: Path): PilotConfig =
    syntheticConfig().copy(
      output = out,
      cells = tiny,
      powerCells = Set(tiny.head.id),
      studies = 3,
      draws = 19,
      phase = Phase.Harness,
      calibrationStudies = 1,
      requireCleanWorktree = false
    )

  /** Test hook for the strict R = 20000 decision: format and aggregation are checked without weakening `outcome`. */
  private val stub: ConfirmationRunner.Decide = (c, p) =>
    Right(if Decision.qualifies(_ => true, c, p) then CandidateOutcome.Adopt else CandidateOutcome.Decline)

  test("a selection file with the wrong sha256 is refused"):
    val f = selectionFile(sixPool)
    val refused = PilotConfig.confirmation(repo, 2, f)
    assertEquals(refused.left.toOption, Some(PilotRefusal.SelectionHashMismatch(sha(f), PilotConfig.PilotSelectionSha256)))
    assert(PilotConfig.confirmation(repo, 2, Paths.get("/nonexistent/selection.json")).isLeft)

  test("anything but 12 distinct core cells is refused"):
    def refusal(ids: Vector[String]) =
      val f = selectionFile(ids)
      PilotConfig.confirmation(repo, 2, f, sha(f)).left.toOption
    val fixed = CellManifest.FixedConfirmation.head.value
    Vector(sixPool.take(5), sixPool :+ sixPool.head, sixPool.take(5) :+ fixed, sixPool.take(5) :+ "C-not-a-cell", Vector.empty[String]).foreach { ids =>
      assert(refusal(ids).exists(_.isInstanceOf[PilotRefusal.WrongConfirmationCells]), ids.toString)
    }
    assert(refusal(sixPool).isEmpty)

  test("the configuration is the confirmation: roots, cells, R, B, power cells, ceilings, stamp fields"):
    val c = syntheticConfig()
    assertEquals(c.phase, Phase.Confirmation)
    assertEquals(StreamKind.values.toVector.map(c.phase.root), Vector(2026100201L, 2026100202L, 2026100203L, 2026100204L, 2026100205L))
    assertEquals((c.cells.length, c.cells.map(_.id).distinct.length, c.studies, c.draws), (12, 12, 20000, 999))
    assertEquals(c.cells.take(6).map(_.id), CellManifest.FixedConfirmation)
    assertEquals(c.powerCells, CellManifest.PowerCells.toSet)
    assertEquals(c.powerCells.size, 4)
    assertEquals((c.ceilingCoreHours, c.runtimeCeilingCoreHours), (15.0, 15.0))
    assertEquals(c.output, Paths.get("/private/tmp/scalafim-execution-20260929/bootstrap-confirmation-20260930"))
    assert(c.requireCleanWorktree && !c.selectConfirmation)
    assertEquals(c.nullSchemes, Scheme.values.toVector)
    assertEquals(c.powerSchemes, ConfirmationRunner.Candidates)
    val stamp = PilotRunner.stamp(c.copy(requireCleanWorktree = false)).fold(r => fail(r.message), identity).fields.toMap
    assertEquals(stamp("phase"), "Confirmation")
    assertEquals(stamp("roots"), "2026100201-2026100202-2026100203-2026100204-2026100205")
    assertEquals(stamp("pilot_output_tree_digest"), PilotConfig.PilotOutputTreeDigest)
    assertEquals(stamp("pilot_output_tree_digest"), "67bd26ba2edfc75539db4f541fe673782b0e798230a973f101cd075c61f92430")
    assert(stamp("selection_sha256").length == 64)
    assertNotEquals(stamp("roots"), PilotRunner.stamp(PilotConfig.declared(repo, 2).copy(requireCleanWorktree = false)).toOption.get.fields.toMap.apply("roots"))

  test("the pilot's real selection.json yields the declared 12 cells (when present)"):
    val real = PilotConfig.DeclaredOutput.resolve("selection.json")
    assume(Files.exists(real), "pilot selection not on this machine")
    val c = PilotConfig.confirmation(repo, 4, real).fold(r => fail(r.message), identity)
    assertEquals(c.cells.drop(6).map(_.id.value), Vector("C-n20-DG-Vrev-T0-N8", "C-n80-DG-Vflat-T0-N8", "C-n8-DG-Vspread-T2-N40",
      "C-n20-DI-Vflat-T0-N8", "C-n80-DI-Vflat-T0-N8", "C-n20-DG-Vflat-T0-N8"))
    assertEquals(c.extraStamp.toMap.apply("selection_sha256"), PilotConfig.PilotSelectionSha256)

  private val allowed = Vector("PILOT_PROJECTION,", "PILOT_PROGRESS,", "PILOT_DONE,", "PILOT_STOPPED,", "CONFIRMATION_OUTCOMES_WRITTEN,")

  test("outcomes.json is produced with the right format; stdout has only paths and progress"):
    val out = Files.createTempDirectory("confirmation-runner-test")
    val log = Vector.newBuilder[String]
    val written = ConfirmationRunner.run(tinyConfig(out), l => log += l, stub)
    assert(written.isRight, written.left.map(_.message).left.getOrElse(""))
    val path = written.toOption.get
    assertEquals(path, out.resolve("outcomes.json"))
    assertEquals(lines(Paths.get(path.toString + ".sha256")).head, s"${sha(path)}  outcomes.json")
    assert(!Files.list(out).iterator().asScala.exists(_.toString.endsWith(".tmp")))
    assert(!Files.exists(out.resolve("selection.json")), "confirmation never runs ConfirmationSelection")
    // No rate output: only allowed line kinds, and the last line is the outcome path.
    val all = log.result()
    all.foreach { l =>
      assert(allowed.exists(l.startsWith), s"unexpected output line: $l")
      val lower = l.toLowerCase
      assert(!lower.contains("rate") && !lower.contains("reject") && !lower.contains("retain"), l)
    }
    assertEquals(all.last, s"CONFIRMATION_OUTCOMES_WRITTEN,$path")
    assertEquals(all.count(_.startsWith("CONFIRMATION_OUTCOMES_WRITTEN,")), 1)
    // Format.
    val text = lines(path).mkString
    assertEquals(lines(path).length, 1)
    assert(text.startsWith("{\"stamp\":{"), text.take(40))
    assert(text.contains("\"rule\":\"decision-rule/v2\""))
    ConfirmationRunner.Candidates.foreach { s =>
      val block = s"\"${s.code}\":\\{\"outcome\":\\{\"class\":\"(Adopt|Decline)\"\\},\"cells\":\\{(.*?)\\},\"power_cells\":\\{(.*?)\\}\\}".r
      val m = block.findFirstMatchIn(text).getOrElse(fail(s"no block for ${s.code}: $text"))
      tiny.foreach(c => assert(m.group(2).contains(s"\"${c.id.value}\":\"")))
      assert("""^("[^"]+":"(Pass|Fail|Unresolved)"(,|$))+$""".r.matches(m.group(2)), m.group(2))
      assert(m.group(3).contains(s"\"${tiny.head.id.value}\":{\"gain\":") && !m.group(3).contains(tiny(1).id.value))
    }
    // No counts or decimals: the only numbers are in the stamp.
    assert(!text.substring(text.indexOf("\"candidates\"")).matches("(?s).*[0-9]+\\.[0-9]+.*"))
    assert(!text.contains("\"k\"") && !text.toLowerCase.contains("rate"))

  test("the production decision stays strict: tiny R is refused and nothing is written"):
    val out = Files.createTempDirectory("confirmation-runner-strict")
    val refused = ConfirmationRunner.run(tinyConfig(out), _ => ())
    assert(refused.left.exists(_.message.contains("R = 20000")), refused.toString)
    assert(!Files.exists(out.resolve("outcomes.json")))

  test("aggregation uses the typed aggregators: failures and Unresolved count as level rejections, comparator failures as rejections"):
    val out = Files.createTempDirectory("confirmation-runner-agg")
    assert(PilotRunner.run(tinyConfig(out), _ => ()).isRight)
    val cfg = tinyConfig(out)
    val cells = ConfirmationRunner.cellEvidence(cfg)
    ConfirmationRunner.Candidates.foreach { s =>
      assertEquals(cells(s).map(_.cell.id), tiny.map(_.id))
      assert(cells(s).forall(_.studies == 3))
    }
    val power = ConfirmationRunner.powerEvidence(cfg)
    ConfirmationRunner.Candidates.foreach(s => assertEquals(power(s).map(_.cell.id), Vector(tiny.head.id)))
    val expected = PilotRunner.pilotEvidence(cfg, tiny).map(e => e.cell.id -> e.verdicts)
    expected.foreach { (id, v) =>
      ConfirmationRunner.Candidates.foreach { s =>
        val ev = cells(s).find(_.cell.id == id).get
        assertEquals(ev.nullRejections, v(s).count(_.levelRejects))
        assertEquals(ev.studyFailures, v(s).count(_.isStudyFailure))
      }
    }

  test("paired power discordance equals an independent re-parse of the durable per-study records (same study index)"):
    // R = 12 (not the suite's 3) so that a mis-pairing of studies changes n10/n01; harness seeds only.
    val out = Files.createTempDirectory("confirmation-runner-power")
    val cfg = tinyConfig(out).copy(studies = 12)
    assert(PilotRunner.run(cfg, _ => ()).isRight)
    val file = out.resolve("cells").resolve(s"${tiny.head.id.value}.power.jsonl")
    // Independent parse: plain string search per line, no shared regex or aggregation code.
    def field(line: String, key: String): Option[String] =
      val tag = "\"" + key + "\":\""
      val i = line.indexOf(tag)
      if i < 0 then None else Some(line.substring(i + tag.length, line.indexOf('"', i + tag.length)))
    def study(line: String): Int =
      val i = line.indexOf("\"study\":") + 8
      line.substring(i, line.indexOf(',', i)).toInt
    val rows = lines(file).tail.filter(l => field(l, "verdict").isDefined)
    val power = ConfirmationRunner.powerEvidence(cfg)
    ConfirmationRunner.Candidates.foreach { s =>
      val cand = rows.filter(l => field(l, "scheme").contains(s.code)).map(l => study(l) -> field(l, "verdict").get).toMap
      val comp = rows.filter(l => field(l, "scheme").contains("native-pm-mkh")).map(l => study(l) -> field(l, "verdict").get).toMap
      assertEquals(cand.keySet, (0 until 12).toSet)
      assertEquals(comp.keySet, cand.keySet)
      // Section 6: a candidate failure (or Unresolved bound) is a non-rejection; a comparator failure is a rejection.
      val n10 = cand.keys.count(i => cand(i) == "Reject" && comp(i) == "Retain")
      val n01 = cand.keys.count(i => cand(i) != "Reject" && comp(i) != "Retain")
      val e = power(s).head
      assertEquals((e.studies, e.candidateOnly, e.comparatorOnly), (12, n10, n01), s.code)
    }

  test("resume reuses complete cells and reproduces outcomes.json; a different stamp is refused"):
    val out = Files.createTempDirectory("confirmation-runner-resume")
    val cfg = tinyConfig(out)
    assert(ConfirmationRunner.run(cfg, _ => (), stub).isRight)
    val first = sha(out.resolve("outcomes.json"))
    val log = Vector.newBuilder[String]
    assert(ConfirmationRunner.run(cfg, l => log += l, stub).isRight)
    assert(log.result().exists(_.contains("cells_run=0,cells_skipped=3")), log.result().toString)
    assertEquals(sha(out.resolve("outcomes.json")), first)
    val otherSelection = cfg.copy(extraStamp = cfg.extraStamp.map((k, v) => if k == "selection_sha256" then (k, "0" * 64) else (k, v)))
    val mismatch = ConfirmationRunner.run(otherSelection, _ => (), stub)
    assertEquals(mismatch.left.toOption, Some(PilotRefusal.StampMismatch("selection_sha256")))
    val otherPilot = cfg.copy(extraStamp = cfg.extraStamp.map((k, v) => if k == "pilot_output_tree_digest" then (k, "1" * 64) else (k, v)))
    assertEquals(ConfirmationRunner.run(otherPilot, _ => (), stub).left.toOption, Some(PilotRefusal.StampMismatch("pilot_output_tree_digest")))
    assertEquals(sha(out.resolve("outcomes.json")), first, "a refused resume leaves outcomes.json untouched")
