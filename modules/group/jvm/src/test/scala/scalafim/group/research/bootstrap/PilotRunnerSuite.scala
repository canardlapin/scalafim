package scalafim.group.research.bootstrap

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The pilot runner on a tiny synthetic configuration: harness seeds only, two cells, R = 3. */
class PilotRunnerSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val repo = Paths.get(sys.props.getOrElse("user.dir", "."))
  private val cells = Vector("C-n8-DI-Vspread-T2-N8", "C-n20-DG-Vrev-T0-N8").map(ResearchTestSupport.cell)

  private def config(output: Path, draws: Int = 19, ceiling: Double = 15.0, runtimeCeiling: Double = 15.0): PilotConfig = PilotConfig(
    repo = repo,
    output = output,
    cells = cells,
    powerCells = Set(cells.head.id),
    studies = 3,
    draws = draws,
    phase = Phase.Harness,
    nullSchemes = Scheme.values.toVector,
    powerSchemes = Scheme.values.toVector.filter(_.role == SchemeRole.Candidate),
    threads = 2,
    ceilingCoreHours = ceiling,
    calibrationStudies = 1,
    requireCleanWorktree = false,
    selectConfirmation = false,
    runtimeCeilingCoreHours = runtimeCeiling
  )

  private def fresh(): Path = Files.createTempDirectory("bootstrap-pilot-runner-test")
  private def lines(p: Path): Vector[String] = Files.readAllLines(p, UTF_8).asScala.toVector
  private def sha(p: Path): String =
    java.security.MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(p)).map(b => f"${b & 0xff}%02x").mkString

  /** Stdout of the runner may carry only these line kinds, and never a rate. */
  private def assertNoRates(log: Vector[String]): Unit =
    val allowed = Vector("PILOT_PROJECTION,", "PILOT_PROGRESS,", "PILOT_DONE,", "PILOT_STOPPED,", "PILOT_SELECTION_WRITTEN,")
    log.foreach { l =>
      assert(allowed.exists(l.startsWith), s"unexpected output line: $l")
      val lower = l.toLowerCase
      assert(!lower.contains("rate") && !lower.contains("reject") && !lower.contains("retain"), s"rate-like output: $l")
    }

  test("record format, stamps, sha256 files and descriptive summaries (files only)"):
    val out = fresh()
    val log = Vector.newBuilder[String]
    val report = PilotRunner.run(config(out), l => log += l)
    assert(report.isRight, report.left.map(_.message).left.getOrElse(""))
    assertEquals(report.toOption.get.cellsRun, 3)
    assertNoRates(log.result())
    val stamp = lines(out.resolve("stamp.json")).head
    Vector("git_sha", "manifest_v2_sha256", "cells_json_sha256", "group_main_sources_sha256", "build_sbt_sha256",
      "java_version", "java_vendor", "java_vm_version", "redirected_builds").foreach(k => assert(stamp.contains(s"\"$k\":\""), k))
    assert(stamp.contains(s"\"cells_json_sha256\":\"${CellManifest.sha256}\""))
    val files = Vector(s"${cells(0).id.value}.null", s"${cells(0).id.value}.power", s"${cells(1).id.value}.null")
    files.foreach { f =>
      val p = out.resolve("cells").resolve(s"$f.jsonl")
      val body = lines(p)
      assert(body.head.startsWith(s"{\"stamp\":$stamp"), s"$f header carries the stamp")
      assertEquals(lines(Paths.get(p.toString + ".sha256")).head.split("\\s+").head, sha(p))
      val schemeLines = body.tail.filter(_.contains("\"k\":"))
      val expectedSchemes = if f.endsWith("power") then 3 else Scheme.values.length
      assertEquals(schemeLines.length, 3 * expectedSchemes, f)
      schemeLines.foreach { l =>
        assert("""^\{"study":\d+,"scheme":"[^"]+","verdict":"(Reject|Retain|Unresolved|Failed)","k":(\d+|null),"f":(\d+|null),"B":19,"T":[^,]+,"failure":.*\}$""".r.matches(l), l)
      }
      Vector("native-pm-mkh", "hc3-equal", "hc3-inverse-v", "oracle-z", "study").foreach(s => assertEquals(body.count(_.contains(s"\"scheme\":\"$s\"")), 3, s"$f $s"))
      assert(Files.exists(out.resolve("summaries").resolve(s"$f.json")))
    }
    // The intercept-only cell carries the sign-flip reference; the G cell does not.
    assertEquals(lines(out.resolve("cells").resolve(s"${cells(0).id.value}.null.jsonl")).count(_.contains("\"scheme\":\"sign-flip\"")), 3)
    assertEquals(lines(out.resolve("cells").resolve(s"${cells(1).id.value}.null.jsonl")).count(_.contains("\"scheme\":\"sign-flip\"")), 0)
    // The selection input can be rebuilt from the durable records.
    val evidence = PilotRunner.pilotEvidence(config(out), cells)
    assert(evidence.forall(_.verdicts.values.forall(_.length == 3)))

  test("resume skips complete cells without rewriting them, and recomputes a damaged one"):
    val out = fresh()
    assert(PilotRunner.run(config(out), _ => ()).isRight)
    val files = Files.list(out.resolve("cells")).iterator().asScala.toVector.filter(_.toString.endsWith(".jsonl"))
    val before = files.map(p => p -> sha(p)).toMap
    val again = PilotRunner.run(config(out), _ => ())
    assertEquals(again.map(r => (r.cellsRun, r.cellsSkipped)), Right((0, 3)))
    files.foreach(p => assertEquals(sha(p), before(p)))
    val damaged = files.head
    Files.write(damaged, "garbage\n".getBytes(UTF_8))
    val repaired = PilotRunner.run(config(out), _ => ())
    assertEquals(repaired.map(r => (r.cellsRun, r.cellsSkipped)), Right((1, 2)))
    assertEquals(sha(damaged), before(damaged), "a recomputed cell reproduces the same records")

  test("resume refuses when any stamp differs (run configuration or a cell header)"):
    val out = fresh()
    assert(PilotRunner.run(config(out), _ => ()).isRight)
    val changed = PilotRunner.run(config(out, draws = 21), _ => ())
    assert(changed.left.exists { case PilotRefusal.StampMismatch("draws") => true; case _ => false }, changed.toString)
    val cellFile = out.resolve("cells").resolve(s"${cells(1).id.value}.null.jsonl")
    val body = lines(cellFile)
    val forged = (body.head.replace("\"phase\":\"Harness\"", "\"phase\":\"Pilot\"") +: body.tail).mkString("", "\n", "\n")
    Files.write(cellFile, forged.getBytes(UTF_8))
    Files.write(Paths.get(cellFile.toString + ".sha256"), s"${sha(cellFile)}  x\n".getBytes(UTF_8))
    val refused = PilotRunner.run(config(out), _ => ())
    assert(refused.left.exists(_.isInstanceOf[PilotRefusal.StampMismatch]), refused.toString)

  test("the runner refuses to start when the projection exceeds the ceiling, before writing anything"):
    val out = fresh().resolve("never-created")
    val log = Vector.newBuilder[String]
    val refused = PilotRunner.run(config(out, ceiling = 1e-12), l => log += l)
    assert(refused.left.exists(_.isInstanceOf[PilotRefusal.ProjectionExceedsCeiling]), refused.toString)
    assert(!Files.exists(out))
    assertNoRates(log.result())

  test("the declared configuration is the §6 pilot and refuses a dirty worktree"):
    val declared = PilotConfig.declared(repo, 4)
    assertEquals((declared.cells.length, declared.studies, declared.draws, declared.phase), (117, 2000, 499, Phase.Pilot))
    assertEquals(declared.powerCells, Cell.core.map(_.id).toSet)
    assertEquals(StreamKind.values.toVector.map(declared.phase.root), Vector(2026100101L, 2026100102L, 2026100103L, 2026100104L, 2026100105L))
    assertEquals(declared.ceilingCoreHours, 15.0)
    assertEquals(declared.output, Paths.get("/private/tmp/scalafim-execution-20260929/bootstrap-pilot-20260930"))
    val dirty = PilotRunner.stamp(declared)
    // The stamp check itself never touches the output root.
    assert(!Files.exists(declared.output) || Files.exists(declared.output.resolve("stamp.json")))
    dirty.left.foreach(e => assert(e.isInstanceOf[PilotRefusal.DirtyWorktree], e.message))

  private def leftovers(out: Path): Vector[Path] =
    val stream = Files.walk(out)
    try stream.iterator().asScala.toVector.filter(p => p.toString.endsWith(".tmp") || p.toString.endsWith(".partial"))
    finally stream.close()

  test("atomic writes leave no temp or partial files, and every sha256 has its summary"):
    val out = fresh()
    val log = Vector.newBuilder[String]
    assert(PilotRunner.run(config(out), l => log += l).isRight)
    assertEquals(leftovers(out), Vector.empty)
    val shas = Files.list(out.resolve("cells")).iterator().asScala.toVector.filter(_.toString.endsWith(".sha256"))
    assertEquals(shas.length, 3)
    shas.foreach { p =>
      val name = p.getFileName.toString.stripSuffix(".jsonl.sha256")
      assert(Files.exists(out.resolve("summaries").resolve(s"$name.json")), name)
    }
    val done = log.result().find(_.startsWith("PILOT_DONE,")).get
    assert("""cpu_seconds=[0-9.]+,cpu_seconds_total=[0-9.]+,wall_seconds=[0-9.]+""".r.findFirstIn(done).isDefined, done)
    val cost = lines(out.resolve("run-cost.json")).head
    assert(cost.contains("\"status\":\"complete\"") && cost.contains("\"cpu_seconds\":") && cost.contains("\"wall_seconds\":"), cost)

  test("resume regenerates a missing summary from the durable records"):
    val out = fresh()
    assert(PilotRunner.run(config(out), _ => ()).isRight)
    val summary = out.resolve("summaries").resolve(s"${cells(1).id.value}.null.json")
    val before = sha(summary)
    Files.delete(summary)
    val again = PilotRunner.run(config(out), _ => ())
    assertEquals(again.map(r => (r.cellsRun, r.cellsSkipped)), Right((0, 3)))
    assertEquals(sha(summary), before)

  test("the runtime CPU guard stops at the ceiling without partial data, and the run stays resumable"):
    val out = fresh()
    val log = Vector.newBuilder[String]
    val stopped = PilotRunner.run(config(out, runtimeCeiling = 1e-9), l => log += l)
    assert(stopped.left.exists(_.isInstanceOf[PilotRefusal.CpuCeilingReached]), stopped.toString)
    assertNoRates(log.result())
    val stop = log.result().find(_.startsWith("PILOT_STOPPED,")).get
    assert("""^PILOT_STOPPED,cpu_seconds=[0-9.]+,cpu_seconds_total=[0-9.]+,wall_seconds=[0-9.]+,cells_run=\d+,cells_skipped=\d+$""".r.matches(stop), stop)
    assertEquals(Files.list(out.resolve("cells")).iterator().asScala.toVector, Vector.empty, "no cell output past the ceiling")
    assertEquals(leftovers(out), Vector.empty)
    val cost = lines(out.resolve("run-cost.json")).head
    assert(cost.contains("\"status\":\"stopped-at-ceiling\""), cost)
    val priorTotal = "\"cpu_seconds_total\":([0-9.]+)".r.findFirstMatchIn(cost).get.group(1).toDouble
    // Same stamp (ceilings are not stamped): an owner-approved raise resumes the same output.
    val resumed = PilotRunner.run(config(out), _ => ())
    assertEquals(resumed.map(_.cellsRun), Right(3))
    val total = "\"cpu_seconds_total\":([0-9.]+)".r.findFirstMatchIn(lines(out.resolve("run-cost.json")).head).get.group(1).toDouble
    assert(total > priorTotal, "the CPU total accumulates across invocations")

  test("a redirected dependency build (scalafim.*.build) is refused and never produces pilot evidence"):
    val out = fresh().resolve("never-created")
    val key = "scalafim.gale.build"
    assume(!sys.props.contains(key), "this JVM already has a redirected build")
    System.setProperty(key, "/tmp/some-local-gale")
    try
      assertEquals(PilotRunner.redirectedBuildProperties, Vector(key))
      val refused = PilotRunner.run(config(out), _ => ())
      assertEquals(refused.left.toOption, Some(PilotRefusal.RedirectedBuild(Vector(key))))
      assert(!Files.exists(out))
    finally System.clearProperty(key): Unit
    assertEquals(PilotRunner.redirectedBuildProperties, Vector.empty)
