package scalafim.phrfcmp.exec

import java.io.ByteArrayOutputStream
import java.lang.management.ManagementFactory
import java.util.concurrent.ConcurrentHashMap

import scalafim.phrfcmp.ingest.FitInputs
import scalafim.phrfcmp.prep.{CommonPrep, NativeArm}
import scalafim.phrfcmp.run.*
import scalafim.phrfcmp.score.{Method, TimingQuantity, TrialEstimate, VoxelOutcome}

/** The thread-CPU `PhrfClock` for the JVM driver (design 5.1: timings are single-thread CPU, in core-seconds). Only
  * valid on the thread that reads it; the S5 runner reads it from the one thread that runs a dataset.
  */
object ThreadCpuClock:
  /** `Left` if the JVM cannot report thread CPU time; there is no wall-clock fallback for timing samples. */
  def system: Either[String, PhrfClock] =
    val bean = ManagementFactory.getThreadMXBean
    if !bean.isCurrentThreadCpuTimeSupported then Left("thread CPU time is not supported on this JVM")
    else
      if !bean.isThreadCpuTimeEnabled then bean.setThreadCpuTimeEnabled(true)
      Right(new PhrfClock:
        def nanos(): Long = bean.getCurrentThreadCpuTime)

/** Everything the arms of one (cell, dataset) job need: the fitter-visible inputs, the common preparation, and (for
  * GLMsingle) the generator `.npz` with its bound hash. It never holds truth.
  */
final class LoadedDataset(val inputs: FitInputs, val prep: CommonPrep, val glm: Option[GlmSingleInputs]):
  override def toString: String = "LoadedDataset(<redacted>)"

/** Loads and prepares one dataset. The error is a short token (a refusal constructor name), never a value. */
trait PilotDatasetSource:
  def load(job: Job): Either[String, LoadedDataset]

/** Where per-voxel arm outcomes go for scoring (S8). Truth lives on the other side of this seam: the S10 harness
  * implements it with the generator truth and assembles the `PilotCorpus`. The scheduler calls it once per attempt;
  * the LAST call per (job, method) is the terminal attempt, so implementations overwrite.
  */
trait ScoreFeed:
  def condition(job: Job, result: ConditionArmResult): Unit
  def trial(job: Job, method: Method, outcomes: Vector[VoxelOutcome[TrialEstimate]]): Unit
  def timing(job: Job, quantity: TimingQuantity, seconds: Double): Unit
  def alphaProfile(job: Job, secondsPerAlpha: Vector[Double]): Unit

object ScoreFeed:
  val none: ScoreFeed = new ScoreFeed:
    def condition(job: Job, result: ConditionArmResult): Unit = ()
    def trial(job: Job, method: Method, outcomes: Vector[VoxelOutcome[TrialEstimate]]): Unit = ()
    def timing(job: Job, quantity: TimingQuantity, seconds: Double): Unit = ()
    def alphaProfile(job: Job, secondsPerAlpha: Vector[Double]): Unit = ()

/** The S3 to S6 entry points behind one seam, so the adapter is testable with fakes. */
trait PilotEngines:
  def condition(arm: ConditionArm, d: LoadedDataset): ConditionArmResult
  def trialNative(arm: NativeArm, d: LoadedDataset, rlss: Option[RlssSettings]): Either[NativeTrialRefusal, NativeTrialOutcome]
  def rlssSettings(d: LoadedDataset): Either[PhrfTrialRefusal, RlssSettings]
  def phrfTrial(d: LoadedDataset): Either[PhrfTrialRefusal, PhrfTrialOutcome]
  def glm(inputs: GlmSingleInputs): GlmSingleRun

object PilotEngines:
  /** Production wiring: S3 `ConditionRunner.run`, S4 `TrialNativeRunner.run`, S5 `PhrfTrialRunner.run` with the
    * injected thread-CPU clock, and S6 `GlmSingleBridge.runAttempt` with the caller's `GlmSingleConfig` (its `python`,
    * scratch and `ScratchHook`).
    */
  def real(glmConfig: GlmSingleConfig, clock: PhrfClock): PilotEngines = new PilotEngines:
    def condition(arm: ConditionArm, d: LoadedDataset): ConditionArmResult = ConditionRunner.run(arm, d.prep, d.inputs)
    def trialNative(arm: NativeArm, d: LoadedDataset, rlss: Option[RlssSettings]): Either[NativeTrialRefusal, NativeTrialOutcome] =
      // LSA and LSS ignore the settings; only rLSS gets the real ones
      TrialNativeRunner.run(arm, d.inputs, d.prep, rlss.getOrElse(RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity)))
    def rlssSettings(d: LoadedDataset): Either[PhrfTrialRefusal, RlssSettings] = PhrfTrialRunner.rlssSettings(d.inputs, d.prep)
    def phrfTrial(d: LoadedDataset): Either[PhrfTrialRefusal, PhrfTrialOutcome] = PhrfTrialRunner.run(d.inputs, d.prep, clock = clock)
    def glm(inputs: GlmSingleInputs): GlmSingleRun = GlmSingleBridge.runAttempt(inputs, glmConfig)

/** The pilot arm ids a plan may use (the `ArmId` strings of the cell table). */
enum PilotArmKind(val id: String):
  case Condition(arm: ConditionArm) extends PilotArmKind(arm.id)
  case Native(arm: NativeArm, method: Method) extends PilotArmKind(method.code.toLowerCase)
  case PhrfMl extends PilotArmKind("phrf-ml")
  case GlmSingle extends PilotArmKind("glmsingle")

object PilotArmKind:
  val all: Vector[PilotArmKind] =
    ConditionArm.All.map(PilotArmKind.Condition(_)) ++
      Vector(Native(NativeArm.Lsa, Method.Lsa), Native(NativeArm.Lss, Method.Lss), Native(NativeArm.Rlss, Method.Rlss), PhrfMl, GlmSingle)

  def parse(arm: ArmId): Option[PilotArmKind] = all.find(_.id == arm.value)

/** Little-endian-free, big-endian canonical byte builder for raw result blobs. */
private final class Canon:
  private val out = new ByteArrayOutputStream()
  def i32(v: Int): Canon = { out.write(v >>> 24); out.write(v >>> 16); out.write(v >>> 8); out.write(v); this }
  def i64(v: Long): Canon = { i32((v >>> 32).toInt); i32(v.toInt) }
  def f64(d: Double): Canon = i64(java.lang.Double.doubleToLongBits(d))
  def str(s: String): Canon = { val b = s.getBytes("UTF-8"); i32(b.length); out.write(b, 0, b.length); this }
  def bool(b: Boolean): Canon = { out.write(if b then 1 else 0); this }
  def doubles(xs: Array[Double]): Canon = { i32(xs.length); xs.foreach(f64); this }
  def bytes: Array[Byte] = out.toByteArray

/** Deterministic encodings of arm results (the plaintext of the sealed raw blobs). No wall times or counters that
  * vary between runs.
  */
object RawEncoding:
  def condition(r: ConditionArmResult): Array[Byte] =
    val c = new Canon().str("phrf-cmp-s7-condition-v1").str(r.arm.id).str(r.inputSha256).i32(r.voxels.length)
    r.voxels.foreach { v =>
      c.i32(v.voxel).str(v.status.code)
      v.response match
        case None => c.bool(false)
        case Some(resp) =>
          c.bool(true).i32(resp.grid.size).i32(resp.curves.length)
          resp.curves.foreach(c.doubles)
      v.phrf match
        case None => c.bool(false)
        case Some(p) =>
          c.bool(true).doubles(p.coordinates.toArray).doubles(p.conditionMeans.toArray).str(p.decode.productPrefix).f64(p.penalizedEnergy)
    }
    r.eventCoefficients match
      case None => c.bool(false)
      case Some(m) => c.bool(true).i32(m.rows).i32(m.cols).doubles(m.data)
    c.str(r.route.getOrElse(""))
    c.bytes

  def glm(o: GlmSingleOutcome): Array[Byte] =
    val c = new Canon().str("phrf-cmp-s7-glmsingle-v1").i32(o.nTrials).i32(o.realizedPoolSize).str(o.outputNpzSha256).str(o.inputNpzSha256).i32(o.voxels.length)
    o.voxels.foreach {
      case VoxelOutcome.Estimated(e) => c.i32(0).doubles(e.values.toArray)
      case VoxelOutcome.Refused => c.i32(1)
      case VoxelOutcome.Failed => c.i32(2)
    }
    c.bytes

  def tuning(t: RlssTuning): Array[Byte] =
    val s = t.selection
    val c = new Canon().str("phrf-cmp-s7-rlss-tuning-v1").i32(s.selected).f64(s.selectedAlpha).f64(t.selectedRidge)
    c.doubles(s.meanScores.toArray).i32(s.foldScores.length)
    s.foldScores.foreach(f => c.doubles(f.toArray))
    c.bytes

object PilotArmRunner:
  def isDatasetLevel(s: ConditionArmStatus): Boolean = s match
    case ConditionArmStatus.Refused(RefusalKind.DatasetLevel(_, _)) => true
    case ConditionArmStatus.Failed(FailureKind.Prep(_) | FailureKind.KindMismatch | FailureKind.InputsNotPrepared | FailureKind.Inconsistent(_) | FailureKind.Setup(_, _)) => true
    case _ => false

/** The scheduler's [[ArmRunner]] for the real pilot arms: loads each dataset once per job (released in
  * `jobFinished`), runs the arm through its typed entry point, emits the sealed raw blob, feeds the scorer, and maps
  * the typed refusal to the ledger status. Result-adjacent text (GLMsingle tails, messages) goes to `ctx.note` only,
  * which the scheduler seals in the attempt-unique timing record.
  */
final class PilotArmRunner(source: PilotDatasetSource, engines: PilotEngines, feed: ScoreFeed = ScoreFeed.none) extends ArmRunner:
  private final class Cached(val loaded: Either[String, LoadedDataset]):
    lazy val rlss: Either[PhrfTrialRefusal, RlssSettings] = loaded.fold(c => Left(PhrfTrialRefusal.Setup("load", c)), engines.rlssSettings)

  private val cache = new ConcurrentHashMap[Job, Cached]()

  private def dataset(job: Job): Cached = cache.computeIfAbsent(job, j => new Cached(source.load(j)))

  override def jobFinished(job: Job): Unit = { cache.remove(job); () }

  private def token(s: String): String = s.filter(c => c.isLetterOrDigit && c < 128 || c == '_' || c == '.' || c == ':' || c == '-').take(64)

  private def fromStatus(s: TrialArmStatus): ArmResult = s match
    case TrialArmStatus.Failed(c) => ArmResult.Failed(token(c))
    case TrialArmStatus.Refused(c) => ArmResult.Refused(token(c))
    case TrialArmStatus.Estimated => ArmResult.Done()

  def run(ctx: ArmContext): ArmResult =
    val job = Job(ctx.unit.cell, ctx.unit.dataset)
    PilotArmKind.parse(ctx.unit.arm) match
      case None => ArmResult.Failed("unknown_arm")
      case Some(kind) =>
        val cached = dataset(job)
        cached.loaded match
          case Left(code) => ArmResult.Failed(token("dataset_" + code))
          case Right(d) => dispatch(kind, job, ctx, cached, d)

  private def dispatch(kind: PilotArmKind, job: Job, ctx: ArmContext, cached: Cached, d: LoadedDataset): ArmResult = kind match
    case PilotArmKind.Condition(arm) => condition(arm, job, ctx, d)
    case PilotArmKind.Native(arm, method) => native(arm, method, job, ctx, cached, d)
    case PilotArmKind.PhrfMl => phrf(job, ctx, d)
    case PilotArmKind.GlmSingle => glm(job, ctx, d)

  /** A voxel-wise result is `Done`; only a dataset-level refusal or failure (every voxel the same non-estimated
    * status of a dataset-level kind) becomes a unit-level `Refused` or `Failed`. The full per-voxel statuses are in the
    * sealed blob either way.
    */
  private def condition(arm: ConditionArm, job: Job, ctx: ArmContext, d: LoadedDataset): ArmResult =
    val r = engines.condition(arm, d)
    ctx.emit("result", RawEncoding.condition(r))
    feed.condition(job, r)
    val datasetLevel = r.voxels.nonEmpty && r.statuses.distinct.length == 1 && PilotArmRunner.isDatasetLevel(r.statuses.head)
    if !datasetLevel then ArmResult.Done()
    else
      r.statuses.head match
        case s @ ConditionArmStatus.Refused(_) => ArmResult.Refused(token(s.code))
        case s => ArmResult.Failed(token(s.code))

  private def native(arm: NativeArm, method: Method, job: Job, ctx: ArmContext, cached: Cached, d: LoadedDataset): ArmResult =
    val voxels = d.inputs.y.rows
    val rlss: Either[NativeTrialRefusal, Option[RlssSettings]] =
      if arm != NativeArm.Rlss then Right(None)
      else
        cached.rlss match
          case Left(r) => Left(NativeTrialRefusal.Inconsistent(r.code))
          case Right(s) if s.targets.isEmpty => Left(NativeTrialRefusal.NoTargets) // the pilot path must always supply EdfTargets
          case Right(s) => Right(Some(s))
    val result = rlss.flatMap(engines.trialNative(arm, d, _))
    feed.trial(job, method, TrialNativeScoring.outcomes(result, voxels))
    result match
      case Right(o) =>
        ctx.emit("fit", o.fit.canonicalBytes)
        o.tuning.foreach(t => ctx.emit("tuning", RawEncoding.tuning(t)))
        ArmResult.Done()
      case Left(r) => fromStatus(r.status)

  private def phrf(job: Job, ctx: ArmContext, d: LoadedDataset): ArmResult =
    val result = engines.phrfTrial(d)
    feed.trial(job, Method.Phrf, PhrfTrialScoring.outcomes(result, d.inputs.y.rows))
    result match
      case Right(o) =>
        ctx.emit("outcome", o.canonicalBytes)
        o.timings.trialMlSecondsPerVoxel.foreach(feed.timing(job, TimingQuantity.TrialMlPerVoxel, _))
        o.timings.trialPreparationSecondsPerFit.foreach(feed.timing(job, TimingQuantity.TrialPreparationPerAlpha, _))
        feed.alphaProfile(job, o.timings.alphaPreparationProfile)
        o.timings.trialMlSecondsPerVoxel.headOption.foreach(s => ctx.recordTiming("trial_ml_core_s_per_voxel_first", s))
        ArmResult.Done()
      case Left(r) => fromStatus(r.status)

  private def glm(job: Job, ctx: ArmContext, d: LoadedDataset): ArmResult =
    d.glm match
      case None => ArmResult.Failed("glmsingle_no_input")
      case Some(inputs) =>
        val run = engines.glm(inputs)
        val guard = run.attempt.guardCpuSeconds // what the machine spent, for the section 5.2 guard
        run.result match
          case Right(o) =>
            ctx.emit("result", RawEncoding.glm(o))
            ctx.recordTiming("glmsingle_timing_cpu_s", o.timingCpuSeconds) // the GLMsingle call alone, for the timing endpoint
            ctx.recordTiming("glmsingle_guard_cpu_s", o.guardCpuSeconds)
            feed.trial(job, Method.GlmsD, o.voxels)
            feed.timing(job, TimingQuantity.GlmsingleDataset, o.timingCpuSeconds)
            ArmResult.Done(guard)
          case Left(refusal) =>
            ctx.note(refusal.message) // result-adjacent (tail, CPU): sealed timing record only
            run.attempt.sidecarCpuSeconds.foreach(s => ctx.recordTiming("glmsingle_timing_cpu_s", s))
            ctx.recordTiming("glmsingle_guard_cpu_s", guard)
            feed.trial(job, Method.GlmsD, refusal.asVoxels(d.inputs.y.rows))
            ArmResult.Failed(token("glmsingle_" + refusal.productPrefix), guard)
