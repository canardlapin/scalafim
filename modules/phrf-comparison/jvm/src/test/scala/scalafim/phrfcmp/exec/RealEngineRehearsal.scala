package scalafim.phrfcmp.exec

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentHashMap
import scalafim.phrfcmp.ingest.*
import scalafim.phrfcmp.prep.{CommonPreparation, NativeArm}
import scalafim.phrfcmp.run.*
import scalafim.phrfcmp.score.PilotCell as ScoreCell

/** Explicit harness-root S10 executable. Throwaway test owner only; no launch or pilot whitelist. */
object RealEngineRehearsal:
  private def ok[A](e: Either[?, A]): A = e.fold(x => throw new IllegalStateException(x.toString), identity)
  private val harness = "7a3c91d50b44e2f1"
  private val cells = ScoreCell.values.toVector
  private def id(s: String) = ok(CellId.parse(s))
  private def arm(s: String) = ok(ArmId.parse(s))

  def main(args: Array[String]): Unit =
    require(args.length == 3, "RealEngineRehearsal <inputs> <new-output> <GLMsingle-python>")
    val input = Path.of(args(0)).toAbsolutePath
    val out = Path.of(args(1)).toAbsolutePath
    require(!Files.exists(out), "rehearsal output must be new")
    Files.createDirectories(out)
    val inventory = ujson.read(Files.readString(input.resolve("rehearsal-inputs.json")))
    require(inventory("root_kind").str == "harness" && inventory("root_hex").str == harness)
    require(inventory("datasets_per_cell").num == 2 && inventory("scored_voxels").num == 40)
    require(inventory("cells").arr.map(_.str).toSet == cells.map(_.id).toSet)
    val generator = inventory("generator_code_sha256").str
    val inputsHash = Fs.sha256File(input.resolve("rehearsal-inputs.json"))
    val plan = PilotPlan(cells.map(c => PilotCell(id(c.id), PilotCorpusAssembler.arms(c).map((a, _) => arm(a)))),
      2, minDatasets = 2, maxRetries = 0)
    val bound = new ConcurrentHashMap[Job, BoundDataset]()
    def load(job: Job): BoundDataset = bound.computeIfAbsent(job, j =>
      val c = cells.find(_.id == j.cell.value).get
      val pool = if Set("T-TX-fast", "T-TX-jit").contains(c.id) then 4000 else 0
      val spec = CellSpec(c.id, if c.isCondition then CellKind.Condition else CellKind.Trial,
        40, pool, 1.0, pool > 0)
      val expectation = IngestExpectation(RootKind.Harness, harness, generator, spec, j.dataset, Seeds.ProtocolDenylist)
      val stem = f"${c.id}__d${j.dataset}%04d"
      val b = ok(PhrfDatasetLoader.load(input, stem, expectation))
      val entry = inventory("records").arr.find(r => r("cell").str == c.id && r("dataset").num.toInt == j.dataset).get
      require(b.inputSha256 == entry("npz_sha256").str)
      b)
    val jobs = new ConcurrentHashMap[LoadedDataset, Job]()
    val source: PilotDatasetSource = job =>
      val b = load(job)
      val prep = ok(CommonPreparation.prepare(b.fit, b.manifest.cell.kind))
      val npz = input.resolve(f"${job.cell.value}__d${job.dataset}%04d.npz")
      val d = new LoadedDataset(b.fit, prep, Some(GlmSingleInputs(npz, Some(b.inputSha256))))
      jobs.put(d, job)
      Right(d)
    val truth: PilotTruthSource = job => PilotScoreTruth.fromBound(load(job))
    for c <- cells; d <- 0 until 2 do
      val job = Job(id(c.id), d)
      require(truth.load(job).isRight, s"harness scoring truth refused: ${c.id}/$d")
    val production = new PilotCorpusAssembler(truth, GaussianCoverageTarget.Tau)
    val assembler = new CorpusAssembler:
      val schema = "phrf-s10-real-harness-two-dataset-tau-v1"
      def outcomes(n: Int, cs: Vector[UnitContribution]): Either[String, CorpusOutcomes] =
        if n != 2 then Left("rehearsal_dataset_count") else production.rehearsalOutcomes(cs)
    val clock = ok(ThreadCpuClock.system)
    val glm = GlmSingleConfig(Path.of(args(2)), Path.of("tools/phrf-comparison/glmsingle/from_generator.py").toAbsolutePath, 900.0,
      expectGeneratorSha = Some(generator), expectNPool = Some(4000), expectNVoxels = Some(40))
    val real = PilotEngines.real(glm, clock)
    def log(value: ujson.Value): Unit = synchronized:
      Files.writeString(out.resolve("engine-events.jsonl"), ujson.write(value) + "\n", UTF_8,
        java.nio.file.StandardOpenOption.CREATE, java.nio.file.StandardOpenOption.APPEND)
      println(ujson.write(value))
    def measured[A](name: String, d: LoadedDataset)(f: => A): A =
      val started = CpuClock.system.processCpuSeconds()
      val value = f
      val job = jobs.get(d)
      log(ujson.Obj("cell" -> job.cell.value, "dataset" -> job.dataset, "arm" -> name,
        "jvm_cpu_seconds" -> (CpuClock.system.processCpuSeconds() - started)))
      value
    val engines = new PilotEngines:
      def condition(a: ConditionArm, d: LoadedDataset): ConditionArmResult = measured(a.id, d)(real.condition(a, d))
      def trialNative(a: NativeArm, d: LoadedDataset, r: Option[RlssSettings]): Either[NativeTrialRefusal, NativeTrialOutcome] =
        val result = measured(a.toString, d)(real.trialNative(a, d, r))
        result.toOption.flatMap(_.tuning).foreach(t => endpoint("rlss", d, t.selection))
        result
      def rlssSettings(d: LoadedDataset): Either[PhrfTrialRefusal, RlssSettings] = real.rlssSettings(d)
      def phrfTrial(d: LoadedDataset): Either[PhrfTrialRefusal, PhrfTrialOutcome] =
        val result = measured("phrf-ml", d)(real.phrfTrial(d))
        result.toOption.foreach(t => endpoint("phrf-ml", d, t.tuning.selection))
        result.left.toOption.foreach: refusal =>
          val job = jobs.get(d)
          log(ujson.Obj("cell" -> job.cell.value, "dataset" -> job.dataset, "arm" -> "phrf-ml", "refusal" -> refusal.toString))
        result
      def glm(i: GlmSingleInputs): GlmSingleRun =
        val result = real.glm(i)
        log(ujson.Obj("input" -> i.datasetNpz.getFileName.toString, "arm" -> "glmsingle",
          "succeeded" -> result.result.isRight, "child_cpu_seconds" -> result.attempt.guardCpuSeconds,
          "refusal_kind" -> result.result.left.toOption.fold[ujson.Value](ujson.Null)(r => ujson.Str(r.productPrefix))))
        result
      private def endpoint(name: String, d: LoadedDataset, selection: LorocvSelection): Unit =
        val job = jobs.get(d)
        log(ujson.Obj("cell" -> job.cell.value, "dataset" -> job.dataset, "arm" -> name,
          "selected_index" -> selection.selected, "selected_alpha" -> selection.selectedAlpha,
          "lower_endpoint" -> selection.atLowerEnd, "upper_endpoint" -> selection.atUpperEnd))
    val owner = TestOwner.random()
    val store = ok(SealedStore.open(out.resolve("sealed"), owner.recipient, owner.fingerprint))
    val stamp = PilotStamp(Vector("role" -> "harness-real-engine-rehearsal", "input_inventory_sha256" -> inputsHash,
      "coverage" -> "tau-rehearsal-only", "plan" -> plan.canonical))
    val root = new PilotRoot(java.lang.Long.parseUnsignedLong(harness, 16))
    val runner = new PilotArmRunner(source, engines)
    val first = plan.cells(1).id // keep one completed job before interrupting the next
    val crash: CommitHook = (stage, job) =>
      if stage == CommitStage.MarkerWritten && job.cell == first && job.dataset == 0 then throw new SimulatedCrash
    var crashed = false
    try new PilotRunner(plan, out, stamp, store, root, runner, hook = crash, log = println).run()
    catch case _: SimulatedCrash => crashed = true
    require(crashed, "marker-written interruption was not exercised")
    val report = ok(new PilotRunner(plan, out, stamp, store, root, runner, log = println).run())
    require(report.rerunJobs > 0 && report.contributions.length == 60,
      s"reruns=${report.rerunJobs}, contributions=${report.contributions.length}")
    val mapped = ok(assembler.outcomes(2, report.contributions))
    require(production.outcomes(2, report.contributions).isLeft, "rehearsal must not be a pilot")
    val digest = CorpusDigest.outcomes(2, mapped)
    // This is a test-only verification record, not a PilotAggregator output or
    // released whitelist. It lets the real owner verifier rebuild first commits.
    val markerHash = Fs.sha256("S10 HARNESS RECONSTRUCTION ONLY; NO WHITELIST".getBytes(UTF_8))
    val units = report.scorerInputs.sortBy(i => OwnerVerification.path(i.unit)).map(i => ujson.Obj(
      "cell" -> i.unit.cell.value, "dataset" -> i.unit.dataset, "arm" -> i.unit.arm.value,
      "run_id" -> i.runId, "phase" -> i.phase.code, "payload_sha256" -> i.payloadSha256))
    val record = ujson.Obj("run_id" -> report.runId, "datasets" -> 2, "whitelist_sha256" -> markerHash,
      "corpus_outcomes_sha256" -> digest, "corpus_timing_sha256" -> ("0" * 64),
      "contribution_schema" -> assembler.schema, "units" -> ujson.Arr.from(units))
    ok(store.append(SealedNames.aggregateRecord(report.runId), ujson.write(record).getBytes(UTF_8)))
    val items = ok(OwnerReader.readAll(store.dir, owner.priv, allowPartial = true))
    val verdict = OwnerVerification.verify(items, plan, markerHash, Vector(assembler))
    require(verdict.isInstanceOf[OwnerVerdict.Valid], verdict.toString)
    val result = ujson.Obj("schema" -> "phrf-s10-real-rehearsal/v1", "root_kind" -> "harness",
      "datasets_per_cell" -> 2, "cells" -> ujson.Arr.from(cells.map(_.id)), "units" -> report.contributions.length,
      "process_and_child_cpu_seconds" -> report.totalCpuSeconds, "rerun_jobs" -> report.rerunJobs,
      "owner_verdict" -> verdict.toString, "corpus_outcomes_sha256" -> digest,
      "input_inventory_sha256" -> inputsHash, "coverage_target" -> "tau-rehearsal-only",
      "pilot_launch_authority" -> false, "scientific_admission" -> false, "whitelist_released" -> false)
    Files.writeString(out.resolve("receipt.json"), ujson.write(result, indent = 2) + "\n")
    println(ujson.write(result))
