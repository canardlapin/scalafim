package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.atomic.AtomicInteger

import scalafim.phrfcmp.ingest.{CellKind, ConditionSynth}
import scalafim.phrfcmp.ingest.ConditionSynth.{Spec, Truth}
import scalafim.phrfcmp.prep.{CommonPreparation, NativeArm}
import scalafim.phrfcmp.run.*
import scalafim.phrfcmp.score.{ConditionResponse, Method, ResponseGrid, TimingQuantity, TrialEstimate, VoxelOutcome}

class PilotArmsSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private def cell(s: String): CellId = CellId.parse(s).fold(sys.error, identity)
  private def arm(s: String): ArmId = ArmId.parse(s).fold(sys.error, identity)
  private val root = new PilotRoot(7L)

  private val spec = Spec(Vector(Truth(5.5, 1.6, Vector(3.0, 5.0, 2.0)), Truth(4.2, 1.1, Vector(4.0, 2.0, 3.0))), noiseSd = 1.0, seed = 3L)
  private lazy val inputs = ConditionSynth.inputs(spec)
  private lazy val prep = CommonPreparation.prepare(inputs, CellKind.Condition).fold(e => fail(e.message), identity)
  private lazy val loaded = new LoadedDataset(inputs, prep, Some(GlmSingleInputs(Path.of("/nonexistent/x.npz"), None)))
  private val voxels = 2

  private final class Source extends PilotDatasetSource:
    val loads = new AtomicInteger(0)
    def load(job: Job): Either[String, LoadedDataset] = { loads.incrementAndGet(); Right(loaded) }

  private final class Feed extends ScoreFeed:
    var conditions = Vector.empty[ConditionArmResult]
    var trials = Vector.empty[(Method, Vector[VoxelOutcome[TrialEstimate]])]
    var timings = Vector.empty[(TimingQuantity, Double)]
    var profiles = Vector.empty[Vector[Double]]
    def condition(job: Job, r: ConditionArmResult): Unit = conditions :+= r
    def trial(job: Job, m: Method, o: Vector[VoxelOutcome[TrialEstimate]]): Unit = trials :+= ((m, o))
    def timing(job: Job, q: TimingQuantity, s: Double): Unit = timings :+= ((q, s))
    def alphaProfile(job: Job, s: Vector[Double]): Unit = profiles :+= s

  private val grid = ResponseGrid.standard().fold(e => fail(e.message), identity)
  private def resp = ConditionResponse(grid, Vector(Array.fill(grid.size)(1.0), Array.fill(grid.size)(2.0)))
  private def condResult(status: ConditionArmStatus, a: ConditionArm) =
    ConditionArmResult(a, "sha", Vector.tabulate(voxels)(v => ConditionVoxel(v, status, Option.when(status.isEstimated)(resp), None)), None, None)

  /** Fake engines; every method is overridable per test. */
  private class Fake extends PilotEngines:
    var cond: ConditionArm => ConditionArmResult = a => condResult(ConditionArmStatus.Estimated, a)
    var native: (NativeArm, Option[RlssSettings]) => Either[NativeTrialRefusal, NativeTrialOutcome] = (_, _) => Left(NativeTrialRefusal.Inconsistent("unset"))
    var rlss: Either[PhrfTrialRefusal, RlssSettings] = Left(PhrfTrialRefusal.WrongKind)
    var phrf: Either[PhrfTrialRefusal, PhrfTrialOutcome] = Left(PhrfTrialRefusal.WrongKind)
    var glmRun: GlmSingleRun = GlmSingleRun(GlmSingleAttempt.none, Left(GlmSingleRefusal.LaunchFailed("unset")))
    val nativeCalls = new AtomicInteger(0)
    val rlssSeen = scala.collection.mutable.ArrayBuffer.empty[Option[RlssSettings]]
    def condition(a: ConditionArm, d: LoadedDataset): ConditionArmResult = cond(a)
    def trialNative(a: NativeArm, d: LoadedDataset, r: Option[RlssSettings]): Either[NativeTrialRefusal, NativeTrialOutcome] =
      nativeCalls.incrementAndGet(); rlssSeen += r; native(a, r)
    def rlssSettings(d: LoadedDataset): Either[PhrfTrialRefusal, RlssSettings] = rlss
    def phrfTrial(d: LoadedDataset): Either[PhrfTrialRefusal, PhrfTrialOutcome] = phrf
    def glm(i: GlmSingleInputs): GlmSingleRun = glmRun

  private def unit(a: String, d: Int = 0) = WorkUnit(cell("C-X"), d, arm(a))
  private def ctx(a: String, attempt: Int = 1) = new ArmContext(unit(a), attempt, root, () => false)

  test("arm ids parse to the nine pilot arms; unknown ids fail without touching an engine") {
    assertEquals(PilotArmKind.all.map(_.id), Vector("can", "inf3", "fir", "phrf-gauss", "lsa", "lss", "rlss", "phrf-ml", "glmsingle"))
    val e = new Fake
    val c = ctx("nope")
    assertEquals(new PilotArmRunner(new Source, e).run(c), ArmResult.Failed("unknown_arm"))
  }

  test("condition arm: sealed blob emitted, feed called, voxel-level refusals do not make the unit refuse") {
    val e = new Fake; val f = new Feed
    e.cond = a => ConditionArmResult(a, "sha", Vector(
      ConditionVoxel(0, ConditionArmStatus.Estimated, Some(resp), None),
      ConditionVoxel(1, ConditionArmStatus.Refused(RefusalKind.Decode(scalafim.fmri.fit.profile.DecodeStatus.Boundary)), None, None)), None, None)
    val c = ctx("can")
    assertEquals(new PilotArmRunner(new Source, e, f).run(c), ArmResult.Done())
    assertEquals(c.emitted.map(_.name), Vector("result"))
    assert(c.emitted.head.bytes.length > 100)
    assertEquals(f.conditions.length, 1)
  }

  test("condition arm: dataset-level refusal or failure becomes the unit status with a token code") {
    val e = new Fake
    e.cond = a => condResult(ConditionArmStatus.Refused(RefusalKind.DatasetLevel("plan", "NoCompact")), a)
    val r = new PilotArmRunner(new Source, e).run(ctx("phrf-gauss"))
    assertEquals(r, ArmResult.Refused("refused:dataset.plan.NoCompact"))
    e.cond = a => condResult(ConditionArmStatus.Failed(FailureKind.InputsNotPrepared), a)
    assertEquals(new PilotArmRunner(new Source, e).run(ctx("can")), ArmResult.Failed("failed:inputs-not-prepared"))
    e.cond = a => condResult(ConditionArmStatus.Failed(FailureKind.NonFiniteCoefficient), a)
    assertEquals(new PilotArmRunner(new Source, e).run(ctx("can")), ArmResult.Done(), "a numerical failure of every voxel stays voxel-level")
  }

  private def nativeOutcome(arm: NativeArm): NativeTrialOutcome =
    val m = scalafim.phrfcmp.ingest.Matrix.of(3, voxels, Array.tabulate(3 * voxels)(_.toDouble)).fold(x => fail(x.message), identity)
    NativeTrialOutcome(NativeTrialFit(arm, Array(0, 1, 2), m, None, Vector.fill(voxels)(TrialArmStatus.Estimated)), None)

  test("native trial arms: LSA runs without settings; a typed refusal maps to the ledger status; the feed gets per-voxel outcomes") {
    val e = new Fake; val f = new Feed
    e.native = (a, _) => Right(nativeOutcome(a))
    val c = ctx("lsa")
    assertEquals(new PilotArmRunner(new Source, e, f).run(c), ArmResult.Done())
    assertEquals(e.rlssSeen.toVector, Vector(None))
    assertEquals(c.emitted.map(_.name), Vector("fit"))
    assertEquals(f.trials.map(_._1), Vector(Method.Lsa))
    e.native = (_, _) => Left(NativeTrialRefusal.Fit(scalafim.fmri.fit.FitError.EmptyDesign))
    assertEquals(new PilotArmRunner(new Source, e, f).run(ctx("lss")), ArmResult.Failed("fit_EmptyDesign"))
    assert(f.trials.last._2.forall(_ == VoxelOutcome.Failed))
    e.native = (_, _) => Left(NativeTrialRefusal.WrongKind(NativeArm.Can))
    assertEquals(new PilotArmRunner(new Source, e, f).run(ctx("lss")), ArmResult.Refused("native_wrong_kind"))
  }

  test("rLSS always receives EdfTargets: settings without targets refuse before the engine runs; with targets they are passed through") {
    val e = new Fake
    val noTargets = RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity, None)
    e.rlss = Right(noTargets)
    e.native = (a, _) => Right(nativeOutcome(a))
    assertEquals(new PilotArmRunner(new Source, e).run(ctx("rlss")), ArmResult.Refused("native_no_edf_targets"))
    assertEquals(e.nativeCalls.get(), 0)
    val withTargets = RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity, Some(EdfTargets(_ => Right(Vector.fill(AlphaGrid.Pilot.size)(1.0)))))
    e.rlss = Right(withTargets)
    assertEquals(new PilotArmRunner(new Source, e).run(ctx("rlss")), ArmResult.Done())
    assertEquals(e.rlssSeen.toVector.map(_.exists(_.targets.isDefined)), Vector(true))
    e.rlss = Left(PhrfTrialRefusal.Setup("edf", "Boom"))
    assertEquals(new PilotArmRunner(new Source, e).run(ctx("rlss")), ArmResult.Refused("native_inconsistent_inputs"))
  }

  test("PHRF trial refusal maps by status; PHRF-ML never emits a blob for a refusal") {
    val e = new Fake; val f = new Feed
    e.phrf = Left(PhrfTrialRefusal.Dataset("plan", "X"))
    val c = ctx("phrf-ml")
    assertEquals(new PilotArmRunner(new Source, e, f).run(c), ArmResult.Refused("phrf_dataset_plan_X"))
    assertEquals(c.emitted, Vector.empty)
    assertEquals(f.trials.map(_._1), Vector(Method.Phrf))
    e.phrf = Left(PhrfTrialRefusal.Setup("basis", "Y"))
    assertEquals(new PilotArmRunner(new Source, e, f).run(ctx("phrf-ml")), ArmResult.Failed("phrf_setup_basis_Y"))
  }

  private def glmOutcome(guard: Double, timing: Double) =
    GlmSingleOutcome(Vector(VoxelOutcome.Estimated(TrialEstimate.of(Vector(1.0, 2.0)).toOption.get), VoxelOutcome.Failed), 2, guard, timing, Some(guard), guard, 3.0, "o" * 64, "i" * 64, 10, "commit", "script", "threads=1")

  test("GLMsingle: guard CPU goes to the scheduler, timing CPU to the timing endpoint; refusal text stays in the sealed note") {
    val e = new Fake; val f = new Feed
    val attempt = GlmSingleAttempt(Some(0), false, 3.0, Some(9.5), 9.0, Some(7.25), Some("o" * 64), 0)
    e.glmRun = GlmSingleRun(attempt, Right(glmOutcome(9.5, 7.25)))
    val c = ctx("glmsingle")
    assertEquals(new PilotArmRunner(new Source, e, f).run(c), ArmResult.Done(9.5))
    assertEquals(f.timings, Vector((TimingQuantity.GlmsingleDataset, 7.25)))
    assertEquals(c.timings.toMap, Map("glmsingle_timing_cpu_s" -> 7.25, "glmsingle_guard_cpu_s" -> 9.5))
    assertEquals(f.trials.map(_._1), Vector(Method.GlmsD))
    assertEquals(c.emitted.map(_.name), Vector("result"))
    // refusal: the tail is a note, the result is Failed with the child's CPU for the guard
    val failed = GlmSingleAttempt(Some(3), false, 2.0, Some(4.0), 3.5, None, None, 0)
    e.glmRun = GlmSingleRun(failed, Left(GlmSingleRefusal.ChildFailed(3, "SECRET-TAIL 0.123456")))
    val c2 = ctx("glmsingle")
    assertEquals(new PilotArmRunner(new Source, e, f).run(c2), ArmResult.Failed("glmsingle_ChildFailed", 4.0))
    assert(c2.notes.exists(_.contains("SECRET-TAIL")))
    assertEquals(c2.emitted, Vector.empty)
    assert(f.trials.last._2.forall(_ == VoxelOutcome.Failed))
  }

  test("a job's dataset is loaded once for all its arms and released in jobFinished") {
    val src = new Source; val e = new Fake
    val r = new PilotArmRunner(src, e)
    r.run(ctx("can")); r.run(ctx("inf3")); r.run(ctx("fir"))
    assertEquals(src.loads.get(), 1)
    r.jobFinished(Job(cell("C-X"), 0))
    r.run(ctx("can"))
    assertEquals(src.loads.get(), 2)
  }

  test("a dataset that cannot be loaded fails every arm with a token") {
    val bad: PilotDatasetSource = _ => Left("NpzRefused")
    assertEquals(new PilotArmRunner(bad, new Fake).run(ctx("can")), ArmResult.Failed("dataset_NpzRefused"))
  }

  test("end to end through the scheduler: fake arms are sealed, GLMsingle tail only in the sealed timing record, guard CPU counts") {
    val owner = TestOwner.random()
    val out = Files.createTempDirectory("phrf-s7-arms-")
    val e = new Fake
    val attempt = GlmSingleAttempt(Some(3), false, 2.0, Some(4.0), 3.5, None, None, 0)
    e.glmRun = GlmSingleRun(attempt, Left(GlmSingleRefusal.ChildFailed(3, "SECRET-TAIL-xyz")))
    e.cond = a => condResult(ConditionArmStatus.Estimated, a)
    val plan = PilotPlan(Vector(PilotCell(cell("C-X"), Vector(arm("can"), arm("glmsingle")))), 3, maxRetries = 0)
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(x => fail(x.message), identity)
    val clk = new FakeClock
    val r = new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), store, root, new PilotArmRunner(new Source, e), CpuGuard(), clk)
    val rep = r.run().fold(x => fail(x.message), identity)
    assertEquals(rep.outcome.decision.D, 3)
    assertEquals(rep.totalCpuSeconds, 3 * 4.0, 1e-9) // child CPU of the three failed GLMsingle units
    val items = OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).fold(x => fail(x), identity)
    val tail = items.filter((_, d) => new String(d, UTF_8).contains("SECRET-TAIL-xyz")).keys.toVector
    assertEquals(tail.length, 3)
    assert(tail.forall(_.startsWith("timing/")), tail.toString)
    val ledger = LedgerRecord.parse(new String(items(SealedNames.ledger(WorkUnit(cell("C-X"), 0, arm("glmsingle")), r.runId)), UTF_8)).toOption.get
    assertEquals((ledger.status, ledger.code), (UnitStatus.Failed, "glmsingle_ChildFailed"))
    Fs.listFiles(out).foreach(p => assert(!new String(Files.readAllBytes(p), "ISO-8859-1").contains("SECRET-TAIL"), p.toString))
  }

  test("review 3: a feed exception after GLMsingle ran keeps the measured child CPU, on every attempt") {
    val owner = TestOwner.random()
    val throwing = new ScoreFeed:
      def condition(job: Job, r: ConditionArmResult): Unit = ()
      def trial(job: Job, m: Method, o: Vector[VoxelOutcome[TrialEstimate]]): Unit = throw new IllegalStateException("feed broke")
      def timing(job: Job, q: TimingQuantity, s: Double): Unit = throw new IllegalStateException("feed broke")
      def alphaProfile(job: Job, s: Vector[Double]): Unit = ()
    val e = new Fake
    e.glmRun = GlmSingleRun(GlmSingleAttempt(Some(3), false, 2.0, Some(37.0), 3.5, None, None, 0), Left(GlmSingleRefusal.ChildFailed(3, "tail")))
    val direct = new PilotArmRunner(new Source, e, throwing).run(ctx("glmsingle"))
    assertEquals(direct.childCpu, 37.0)
    for retries <- Vector(0, 2) do
      val out = Files.createTempDirectory("phrf-s7-feedcpu-")
      val plan = PilotPlan(Vector(PilotCell(cell("C-X"), Vector(arm("glmsingle")))), 2, probeDatasets = 1, maxRetries = retries)
      val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(x => fail(x.message), identity)
      val rep = new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), store, root, new PilotArmRunner(new Source, e, throwing), CpuGuard(), new FakeClock)
        .run().fold(x => fail(x.message), identity)
      assertEqualsDouble(rep.totalCpuSeconds, 2 * (retries + 1) * 37.0, 1e-9)
  }

  test("re-review 2: an InterruptedException from the feed after GLMsingle ran still meters its CPU, then ends the run as Interrupted") {
    val owner = TestOwner.random()
    val interrupting = new ScoreFeed:
      def condition(job: Job, r: ConditionArmResult): Unit = ()
      def trial(job: Job, m: Method, o: Vector[VoxelOutcome[TrialEstimate]]): Unit = throw new InterruptedException("feed interrupted")
      def timing(job: Job, q: TimingQuantity, s: Double): Unit = ()
      def alphaProfile(job: Job, s: Vector[Double]): Unit = ()
    val e = new Fake
    e.glmRun = GlmSingleRun(GlmSingleAttempt(Some(3), false, 2.0, Some(37.0), 3.5, None, None, 0), Left(GlmSingleRefusal.ChildFailed(3, "tail")))
    val direct = new PilotArmRunner(new Source, e, interrupting).run(ctx("glmsingle"))
    assertEquals(direct.childCpu, 37.0)
    assert(Thread.interrupted(), "the interrupt flag is restored for the caller")
    val out = Files.createTempDirectory("phrf-s7-intcpu-")
    val plan = PilotPlan(Vector(PilotCell(cell("C-X"), Vector(arm("glmsingle")))), 2, probeDatasets = 1, maxRetries = 2)
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(x => fail(x.message), identity)
    val r = new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), store, root, new PilotArmRunner(new Source, e, interrupting), CpuGuard(), new FakeClock)
    assertEquals(r.run().left.toOption, Some(PilotRefusal.Interrupted))
    assertEquals(ujson.read(Files.readString(out.resolve("cost.json")))("cpu_seconds_total").num, 37.0)
    assert(!Files.exists(out.resolve("accounting.open")))
  }

  test("review3 H1: a fatal throwable after GLMsingle ran leaves accounting.open, so the resume refuses uncertain accounting") {
    val owner = TestOwner.random()
    val fatal = new ScoreFeed:
      def condition(job: Job, r: ConditionArmResult): Unit = ()
      def trial(job: Job, m: Method, o: Vector[VoxelOutcome[TrialEstimate]]): Unit = throw new OutOfMemoryError("synthetic")
      def timing(job: Job, q: TimingQuantity, s: Double): Unit = ()
      def alphaProfile(job: Job, s: Vector[Double]): Unit = ()
    val e = new Fake
    e.glmRun = GlmSingleRun(GlmSingleAttempt(Some(3), false, 2.0, Some(37.0), 3.5, None, None, 0), Left(GlmSingleRefusal.ChildFailed(3, "tail")))
    val out = Files.createTempDirectory("phrf-s7-fatal-")
    val plan = PilotPlan(Vector(PilotCell(cell("C-X"), Vector(arm("glmsingle")))), 2, probeDatasets = 1, maxRetries = 0)
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(x => fail(x.message), identity)
    val stamp = PilotStamp(Vector("k" -> "v"))
    // munit's intercept does not catch fatal throwables
    val thrown =
      try
        new PilotRunner(plan, out, stamp, store, root, new PilotArmRunner(new Source, e, fatal), CpuGuard(), new FakeClock, runIds = () => "fatal1").run()
        None
      catch case t: OutOfMemoryError => Some(t)
    assert(thrown.isDefined, "the fatal throwable propagates")
    assertEquals(Files.readString(out.resolve("accounting.open")).trim, "fatal1", "the measured 37 s may be missing: accounting stays open")
    val resume = new PilotRunner(plan, out, stamp, store, root, new PilotArmRunner(new Source, e), CpuGuard(), new FakeClock).run()
    assertEquals(resume.left.toOption, Some(PilotRefusal.AccountingUncertain("fatal1")))
  }

  test("real S3 engine: CAN on synthetic condition data, two datasets in parallel, sealed result decrypts and feed receives estimates") {
    val owner = TestOwner.random()
    val out = Files.createTempDirectory("phrf-s7-real-")
    val dummy = GlmSingleConfig(Path.of("/nonexistent/python"), Path.of("/nonexistent/from_generator.py"), 1.0)
    val feed = new Feed
    val clock = ThreadCpuClock.system.fold(m => fail(m), identity)
    val arms = new PilotArmRunner(new Source, PilotEngines.real(dummy, clock), feed)
    val plan = PilotPlan(Vector(PilotCell(cell("C-X"), Vector(arm("can")))), 2, probeDatasets = 1)
    val store = SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint).fold(x => fail(x.message), identity)
    new PilotRunner(plan, out, PilotStamp(Vector("k" -> "v")), store, root, arms, CpuGuard(), CpuClock.system, threads = 2).run().fold(x => fail(x.message), identity)
    assert(feed.conditions.nonEmpty && feed.conditions.forall(_.statuses.forall(_.isEstimated)))
    val items = OwnerReader.readAll(out.resolve("sealed"), owner.priv, allowPartial = true).fold(x => fail(x), identity)
    assertEquals(items.keys.count(_.startsWith("data/C-X/")), 2)
    assert(items.filter(_._1.startsWith("data/")).values.forall(_.length > 100))
  }

  test("ThreadCpuClock advances on CPU work and not on sleep") {
    val clock = ThreadCpuClock.system.fold(m => fail(m), identity)
    val t0 = clock.nanos()
    Thread.sleep(300)
    val slept = clock.nanos() - t0
    assert(slept < 100_000_000L, s"sleep counted as CPU: $slept ns")
    val t1 = clock.nanos()
    var x = 0.0
    var i = 0
    while i < 30_000_000 do { x += math.sqrt(i.toDouble); i += 1 }
    assert(x > 0.0 && clock.nanos() - t1 > 5_000_000L)
  }
