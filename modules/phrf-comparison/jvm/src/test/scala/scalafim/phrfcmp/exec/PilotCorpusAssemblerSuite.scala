package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files
import scalafim.phrfcmp.ingest.ScoringSynth
import scalafim.phrfcmp.score.{PilotCorpus, ScoreSynth}
import scalafim.phrfcmp.score.PilotCell as ScoreCell

class PilotCorpusAssemblerSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "min")
  private def ok[A](e: Either[String, A]): A = e.fold(fail(_), identity)
  private val cells = ScoreCell.values.toVector
  private val plan = PilotPlan(cells.map(c => PilotCell(CellId.parse(c.id).toOption.get,
    PilotCorpusAssembler.arms(c).map((a, _) => ArmId.parse(a).toOption.get))), 15)
  private val truth: PilotTruthSource = job =>
    val c = cells.find(_.id == job.cell.value).get
    PilotScoreTruth.fromBound(ScoringSynth.bound(c, job.dataset))
  private val assembler = new PilotCorpusAssembler(truth, GaussianCoverageTarget.Tau)
  private lazy val contributions = (for
    c <- cells; d <- 0 until 15; (a, _) <- PilotCorpusAssembler.arms(c)
  yield
    val u = ScoringSynth.unit(c, d, a)
    new UnitContribution(u, Some(ScoringSynth.contribution(u))))

  test("complete production contributions assemble every cell and preserve canonical outcomes under reordering"):
    val a = ok(assembler.outcomes(15, contributions))
    val b = ok(assembler.outcomes(15, contributions.reverse))
    assertEquals(CorpusDigest.outcomes(15, a), CorpusDigest.outcomes(15, b))
    assert(PilotCorpus.of(a.condition, a.trial, a.coverage, ScoreSynth.timing, 0.0).isRight)
    assertEquals(a.condition.values.map(_.length).toSet, Set(15))
    assertEquals(a.trial.values.map(_.length).toSet, Set(15))
    assertEquals(a.coverage.length, 15)

  test("missing, duplicate, swapped, corrupt, wrong-input and wrong-truth records refuse"):
    assertEquals(assembler.outcomes(15, contributions.tail), Left("contribution_inventory"))
    assertEquals(assembler.outcomes(15, contributions.tail :+ contributions(1)), Left("contribution_inventory"))
    val u = contributions.head.unit
    for bytes <- Vector(None, Some(Array[Byte](1, 2)), contributions(1).payload) do
      assert(assembler.outcomes(15, new UnitContribution(u, bytes) +: contributions.tail).isLeft)
    val json = ujson.read(new String(contributions.head.payload.get, UTF_8)); json(2) = "f" * 64
    assertEquals(assembler.outcomes(15, new UnitContribution(u, Some(ujson.write(json).getBytes(UTF_8))) +: contributions.tail), Left("contribution_input"))
    val wrong: PilotTruthSource = _ => PilotScoreTruth.fromBound(ScoringSynth.bound(ScoreCell.CTX5, 99))
    assertEquals(new PilotCorpusAssembler(wrong, GaussianCoverageTarget.Tau).outcomes(15, contributions), Left("truth_identity"))

  test("an unavailable committing attempt stays in the scored failure denominator"):
    val u = contributions.head.unit
    val xs = new UnitContribution(u, Some(PilotContribution.unavailable(u))) +: contributions.tail
    val out = ok(assembler.outcomes(15, xs))
    assert(out.condition(ScoreCell.CTX5).head.arms(scalafim.phrfcmp.score.Method.Can).forall(_.isMissing))
    assertEquals(out.condition(ScoreCell.CTX5).head.voxels, ScoringSynth.voxels)

  test("two-dataset rehearsal maps the exact inventory while pilot aggregation still refuses"):
    val small = contributions.filter(_.unit.dataset < 2)
    val result = ok(assembler.rehearsalOutcomes(small))
    assertEquals(result.coverage.length, 2)
    assertEquals(assembler.outcomes(2, small), Left("dataset_count"))
    assert(PilotCorpus.of(result.condition, result.trial, result.coverage, ScoreSynth.timing, 0.0).isLeft)
    assertEquals(assembler.rehearsalOutcomes(small.tail), Left("contribution_inventory"))

  private final case class Rehearsal(report: PilotReport, items: Map[String, Array[Byte]], whitelist: String)
  private lazy val rehearsal: Rehearsal =
    val owner = TestOwner.random() // ephemeral test fixture; never an owner custody key
    val out = Files.createTempDirectory("phrf-s10-production-rehearsal-")
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(e => fail(e.message), identity)
    val retry = ScoringSynth.unit(ScoreCell.CTX5, 2, "can")
    val arms: ArmRunner = ctx =>
      val transient = ctx.unit == retry && ctx.attempt == 1
      ctx.contribute(ScoringSynth.contribution(ctx.unit, if transient then 10.0 else 0.0))
      if transient then ArmResult.Failed("synthetic_retry") else ArmResult.Done()
    val stamp = PilotStamp(Vector("role" -> "synthetic-harness-rehearsal", "contribution" -> assembler.schema))
    val root = new PilotRoot(7L)
    val crash: CommitHook = (s, j) =>
      if s == CommitStage.MarkerWritten && j.cell.value == ScoreCell.CTX5.id && j.dataset == 7 then throw new SimulatedCrash
    intercept[SimulatedCrash](new PilotRunner(plan, out, stamp, store, root, arms, CpuGuard(), new FakeClock, hook = crash).run())
    val report = new PilotRunner(plan, out, stamp, store, root, arms, CpuGuard(), new FakeClock).run().fold(e => fail(e.message), identity)
    assert(report.scorerInputs.exists(_.phase == CommitPhase.Rerun))
    assertEquals(report.contributions.find(_.unit == retry).get.payload.get.toVector, ScoringSynth.contribution(retry).toVector)
    val result = PilotAggregation.aggregateContributions(report, assembler, ScoreSynth.timing, store).fold(e => fail(e.message), identity)
    val items = ok(OwnerReader.readAll(store.dir, owner.priv, allowPartial = true))
    Rehearsal(report, items, result.whitelistSha256)

  test("production codec and assembler survive retries, crash and resume; the owner rebuild is Valid"):
    val r = rehearsal
    assertEquals(OwnerVerification.verify(r.items, plan, r.whitelist, Vector(assembler)),
      OwnerVerdict.Valid(r.report.runId, 15, 450, Vector.empty))
    val record = ujson.read(new String(r.items(SealedNames.aggregateRecord(r.report.runId)), UTF_8))
    assertEquals(record("contribution_schema").str, assembler.schema)
    val rebuilt = ok(assembler.outcomes(15, r.report.contributions))
    assertEquals(record("corpus_outcomes_sha256").str, CorpusDigest.outcomes(15, rebuilt))

  test("tampering with a sealed contribution invalidates the production rehearsal"):
    val r = rehearsal
    val name = r.items.keys.filter(_.startsWith("data/")).toVector.sorted.head
    val changed = r.items(name).clone(); changed(changed.length - 1) = (changed.last ^ 1).toByte
    assert(!OwnerVerification.verify(r.items.updated(name, changed), plan, r.whitelist, Vector(assembler)).isInstanceOf[OwnerVerdict.Valid])

  test("synthetic low-ceiling rehearsal discards an interrupted contribution and resumes only with bound authorization"):
    val owner = TestOwner.random()
    val out = Files.createTempDirectory("phrf-s10-low-ceiling-")
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(e => fail(e.message), identity)
    val small = PilotPlan(Vector(plan.cells.head), 2, probeDatasets = 1, minDatasets = 2)
    val stamp = PilotStamp(Vector("role" -> "synthetic-low-ceiling"))
    val root = new PilotRoot(7L)
    val clock = new FakeClock
    var calls = 0
    val arms: ArmRunner = ctx =>
      calls += 1
      ctx.contribute(ScoringSynth.contribution(ctx.unit))
      ArmResult.Done(childCpuSeconds = 10.0)
    def runner(guard: CpuGuard) = new PilotRunner(small, out, stamp, store, root, arms, guard, clock)
    val tiny = CpuGuard(1.0 / 3600, 5.0 / 3600)
    assert(runner(tiny).run().left.exists(_.isInstanceOf[PilotRefusal.CpuCeilingReached]))
    assertEquals(calls, 1)
    assert(runner(tiny).run().isLeft)
    assertEquals(calls, 1)
    val outputId = runner(CpuGuard()).outputIdentity.get
    val raise = ok(OwnerCeilingRaise.of(61.0, "synthetic-test-owner", "synthetic CPU-guard rehearsal", outputId, 3))
    val report = runner(CpuGuard.raised(45.0, raise)).run().fold(e => fail(e.message), identity)
    assertEquals(report.contributions.length, 8)
    assert(report.contributions.forall(c => c.payload.get.toVector == ScoringSynth.contribution(c.unit).toVector))
    assertEquals(assembler.outcomes(2, report.contributions), Left("dataset_count"), "a two-dataset rehearsal cannot become a scored pilot")

  test("GLMsingle estimates must bind the actual NPZ read by the child to the held generator truth"):
    val target = ScoringSynth.unit(ScoreCell.TTXFast, 0, "glmsingle")
    val original = contributions.find(_.unit == target).get
    for file <- Vector[ujson.Value](ujson.Null, ujson.Str("f" * 64)) do
      val json = ujson.read(new String(original.payload.get, UTF_8)); json(5) = file
      val changed = contributions.map(c => if c.unit == target then new UnitContribution(target, Some(ujson.write(json).getBytes(UTF_8))) else c)
      assertEquals(assembler.outcomes(15, changed), Left("glmsingle_source_file"))
