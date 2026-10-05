package scalafim.phrfcmp.run

import scala.collection.mutable.ArrayBuffer

import scalafim.dataset.{DataSelection, SynchronousFmriDataset}
import scalafim.fmri.design.hrf.HrfKernelBasis
import scalafim.fmri.fit.CanonicalTemporalWhitening
import scalafim.fmri.fit.profile.*
import scalafim.fmri.hrf.family.{NormalizationRule, ParametricHrfFamily}
import scalafim.fmri.model.{ProfileCriterion, ProfileHrfPlan}
import scalafim.phrfcmp.ingest.{CellKind, Digests, FitInputs, Matrix}
import scalafim.phrfcmp.prep.CommonPrep
import scalafim.phrfcmp.score.{Method, TrialEstimate, VoxelOutcome}

/** Deterministic big-endian byte builder for canonical forms (no timings, no wall clock, ever). */
private[run] final class PhrfBytes:
  private val out = new java.io.ByteArrayOutputStream()
  def i32(v: Int): PhrfBytes =
    out.write(v >>> 24); out.write(v >>> 16); out.write(v >>> 8); out.write(v)
    this
  def i64(v: Long): PhrfBytes =
    i32((v >>> 32).toInt); i32(v.toInt)
  def f64(d: Double): PhrfBytes = i64(java.lang.Double.doubleToLongBits(d))
  def str(s: String): PhrfBytes =
    val b = s.getBytes("UTF-8")
    i32(b.length)
    out.write(b, 0, b.length)
    this
  def bytes: Array[Byte] = out.toByteArray

/**
  * The counted work of one PHRF execution (design: "work receipts"): the executor's own counters, never wall time. Equal
  * inputs give equal receipts, so the receipt is part of the deterministic canonical form.
  */
final case class PhrfWorkReceipt(
    kind: String,
    route: String,
    attempted: Int,
    delivered: Int,
    statuses: Vector[(String, Long)],
    decoder: ProfileDecoderWork,
    mlSetup: Option[TrialMlWork],
    mlRun: Option[TrialMlWork],
    evidenceVoxels: Int,
    criterionForm: Boolean,
    publicAttempts: Long,
    publicSuccesses: Long,
    publicFailures: Long,
    publicDecodeRefusals: Long
):
  private[run] def write(b: PhrfBytes): PhrfBytes =
    b.str(kind).str(route).i32(attempted).i32(delivered).i32(statuses.length)
    statuses.foreach((s, n) => b.str(s).i64(n))
    val d = decoder
    b.i64(d.voxels).i64(d.nodeScores).i64(d.jets).i64(d.exactEvaluations).i64(d.candidateAttempts).i64(d.terminalVerifications).i64(d.newtonSteps).i64(d.fallbacks)
    def ml(w: Option[TrialMlWork]): PhrfBytes = w match
      case None => b.i32(0)
      case Some(x) =>
        b.i32(1).i64(x.referenceAttempts).i64(x.nFactorAttempts).i64(x.nFactorFailures).i64(x.solveAttempts).i64(x.rightHandSideAttempts)
          .i64(x.membershipRightHandSides).i64(x.derivativeRightHandSides).i64(x.smallFactorAttempts).i64(x.logDetRecursionAttempts).i64(x.failures)
    ml(mlSetup)
    ml(mlRun)
    b.i32(evidenceVoxels).i32(if criterionForm then 1 else 0).i64(publicAttempts).i64(publicSuccesses).i64(publicFailures).i64(publicDecodeRefusals)

  def canonical: Array[Byte] = write(new PhrfBytes).bytes

object PhrfWorkReceipt:
  def from(kind: String, summary: ProfileRunSummary, evidenceVoxels: Int): PhrfWorkReceipt =
    val p = summary.progress
    val pub = p.publicReadout
    PhrfWorkReceipt(
      kind,
      summary.setup.route,
      p.attemptedVoxels,
      p.deliveredVoxels,
      p.decodeStatuses.toVector.map((s, n) => s.productPrefix -> n).sortBy(_._1),
      p.decoder,
      summary.setup.mlSetup,
      p.trialMl,
      evidenceVoxels,
      summary.provenance.contains("criterion-form=J=E+sigma2*D"),
      pub.fold(0L)(_.attempts),
      pub.fold(0L)(_.successes),
      pub.fold(0L)(_.failures),
      pub.fold(0L)(_.decodeRefusals)
    )

/** Wall (or injected CPU) seconds of the two stages of one fit. Never hashed. */
final case class PhrfStageSeconds(prepare: Double, run: Double)

/**
  * A training-fold fit of one grid alpha: per voxel the decoded status and shape coordinates, and the raw trial
  * coefficients (`trials x voxels`, in the units of the unnormalised family kernel, which is what the held-out prediction
  * multiplies back). A non-`Accepted` voxel is still delivered (its decoded point is a point); only the final fit turns
  * a status into a refusal.
  */
final class PhrfTrainFit(
    val alpha: Double,
    val decode: Vector[DecodeStatus],
    val coordinates: Vector[Vector[Double]],
    val amplitudes: Matrix,
    val receipt: PhrfWorkReceipt
):
  def voxels: Int = coordinates.length

  def canonicalBytes: Array[Byte] =
    val b = new PhrfBytes().str("phrf-cmp-s5-train-fit-v1").f64(alpha).i32(amplitudes.rows).i32(amplitudes.cols)
    decode.foreach(s => b.i32(s.ordinal))
    coordinates.foreach(_.foreach(b.f64))
    amplitudes.data.foreach(b.f64)
    b.i32(receipt.canonical.length)
    receipt.write(b)
    b.bytes

  def sha256: String = Digests.sha256Hex(canonicalBytes)

/** One voxel of the final fit. `eTrial` is `Some` exactly when `status` is `Estimated` (E-trial, input trial order). */
final case class PhrfVoxelEstimate(
    status: TrialArmStatus,
    decode: Option[DecodeStatus],
    coordinates: Vector[Double],
    eTrial: Option[Vector[Double]],
    evidence: Boolean
)

/** The final fit on every run at the selected alpha, with its two work receipts (the legacy run and the public outputs). */
final case class PhrfTrialFit(alpha: Double, voxels: Vector[PhrfVoxelEstimate], runReceipt: PhrfWorkReceipt, outputReceipt: PhrfWorkReceipt):

  def canonicalBytes: Array[Byte] =
    val b = new PhrfBytes().str("phrf-cmp-s5-final-fit-v1").f64(alpha).i32(voxels.length)
    voxels.foreach { v =>
      b.str(v.status match
        case TrialArmStatus.Estimated  => "estimated"
        case TrialArmStatus.Refused(c) => s"refused:$c"
        case TrialArmStatus.Failed(c)  => s"failed:$c")
      b.i32(v.decode.fold(-1)(_.ordinal)).i32(if v.evidence then 1 else 0)
      b.i32(v.coordinates.length)
      v.coordinates.foreach(b.f64)
      v.eTrial match
        case None    => b.i32(-1)
        case Some(e) => b.i32(e.length); e.foreach(b.f64)
    }
    runReceipt.write(b)
    outputReceipt.write(b)
    b.bytes

  def sha256: String = Digests.sha256Hex(canonicalBytes)

/** Everything one PHRF trial dataset produces. `timings` are measurements and are excluded from [[sha256]]. */
final case class PhrfTrialOutcome(
    fit: PhrfTrialFit,
    tuning: PhrfTuningRecord,
    finalSigma2: Double,
    timings: PhrfTimings
):
  def canonicalBytes: Array[Byte] =
    val b = new PhrfBytes().str("phrf-cmp-s5-outcome-v1").f64(finalSigma2)
    val f = fit.canonicalBytes
    val t = tuning.canonicalBytes
    b.i32(f.length)
    f.foreach(x => b.i32(x))
    b.i32(t.length)
    t.foreach(x => b.i32(x))
    b.bytes

  def sha256: String = Digests.sha256Hex(canonicalBytes)

/** E-trial conversion (protocol section 3): the trial peak height `a_i * max|h|`, keeping the sign. */
object PhrfETrial:

  /**
    * `max_t |k(t; theta)|` of the family's raw kernel on a `step` grid over `[0, horizon]`, refined by the parabola
    * through the three samples around the maximum (exact for a locally quadratic peak).
    */
  def peakHeight(family: ParametricHrfFamily, coordinates: Vector[Double], horizon: Double, step: Double): Either[String, Double] =
    if !(horizon > 0.0 && step > 0.0 && step < horizon) then Left("invalid peak grid")
    else
      family.chart.point(coordinates).left.map(_.message).flatMap { point =>
        val n = (horizon / step).toInt + 1
        val lags = Array.tabulate(n)(_ * step)
        val k = new Array[Double](n)
        family.evalInto(lags, point, k)
        var m = 0
        var i = 1
        while i < n do
          if math.abs(k(i)) > math.abs(k(m)) then m = i
          i += 1
        val a = math.abs(k(m))
        if !a.isFinite then Left("non-finite kernel")
        else if m == 0 || m == n - 1 then Right(a)
        else
          val ym = math.abs(k(m - 1))
          val yp = math.abs(k(m + 1))
          val denom = ym - 2.0 * a + yp
          if denom == 0.0 then Right(a)
          else
            val delta = 0.5 * (ym - yp) / denom
            Right(a - 0.25 * (ym - yp) * delta)
      }

  /**
    * E-trial from the public ExactShape output. `normalized` are the amplitudes in the family's library normalisation
    * (`ProfileTrialReadoutResult.trialAmplitudes`), `scale` the normalisation scale at the decoded point, so
    * `normalized * scale` is the coefficient of the raw kernel and the trial's peak height is that times `peak`.
    */
  def convert(normalized: Vector[Double], scale: Double, peak: Double): Vector[Double] = normalized.map(a => a * scale * peak)

/**
  * The PHRF trial arm (design 2.2): positive-alpha trial-banded ML, tuned by the shared LOROCV shell
  * ([[PhrfTrialTuning]]), refitted on every run at the selected alpha with ExactShape public outputs, converted to
  * E-trial. Alpha 0 is the condition-means route and is never routed to ML ([[mlPlan]]).
  */
object PhrfTrialRunner:

  /** The decode policy S0 pinned for the trial route (no observed-family admission: that is the condition route). */
  def decodePolicy(c: PhrfConditionConfig): ProfileDecodePolicy =
    ProfileDecodePolicy(Vector(c.decodeNodes, c.decodeNodes), c.budget, prior = None, execution = ExecutionBudget(c.block, 1))

  /**
    * `ProfileHrfPlan.fromTrialEvents(..., alpha, TrialRandomEffectsML(sigma2))`. The only way to an ML plan: a
    * non-positive alpha is refused here, before any plan is built, so alpha 0 can never reach the ML executor.
    */
  def mlPlan(problem: PhrfTrialProblem, basis: HrfKernelBasis, alpha: Double): Either[PhrfTrialRefusal, ProfileHrfPlan] =
    if !(alpha > 0.0) || !alpha.isFinite then Left(PhrfTrialRefusal.AlphaNotPositive(alpha))
    else
      ProfileHrfPlan
        .fromTrialEvents(
          problem.dataset,
          problem.drive,
          problem.baseline,
          problem.spec.phrfConfig,
          basis,
          alpha,
          ProfileCriterion.TrialRandomEffectsML(problem.sigma2)
        )
        .left.map(e => PhrfTrialRefusal.Dataset("plan", PhrfTrialAssembly.fmt(e)))

  private def seconds(clock: PhrfClock, from: Long): Double = (clock.nanos() - from) / 1e9

  private def collecting(into: ArrayBuffer[ProfileFitBlock]) = new BlockSink[ProfileFitBlock, ProfileFitReceipt]:
    def accept(block: VoxelBlock, payload: ProfileFitBlock): Either[String, ProfileFitReceipt] =
      into += payload
      Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))

  private def collectingOutputs(into: ArrayBuffer[ProfileTrialOutputBlock]) = new BlockSink[ProfileTrialOutputBlock, ProfileFitReceipt]:
    def accept(block: VoxelBlock, payload: ProfileTrialOutputBlock): Either[String, ProfileFitReceipt] =
      into += payload
      Right(ProfileFitReceipt(payload.ordinal, payload.voxelIds))

  private def mat(rows: Int, cols: Int, data: Array[Double]): Either[PhrfTrialRefusal, Matrix] =
    Matrix.of(rows, cols, data).left.map(e => PhrfTrialRefusal.Inconsistent(e.message))

  /** The legacy run's per-voxel results in voxel order; a voxel that was not delivered is a typed failure. */
  private def byVoxel(problem: PhrfTrialProblem, blocks: Seq[ProfileFitBlock]): Either[PhrfTrialRefusal, Vector[ProfileVoxelResult]] =
    val byId = blocks.flatMap(_.results).map(r => r.voxelId -> r).toMap
    val all = Vector.tabulate(problem.voxels)(byId.get)
    if all.exists(_.isEmpty) then Left(PhrfTrialRefusal.Setup("run", "VoxelNotDelivered")) else Right(all.map(_.get))

  /** A training-fold fit: prepare at positive `alpha`, run (legacy path, no public outputs), collect decoded shape and amplitudes. */
  def trainFit(
      problem: PhrfTrialProblem,
      basis: HrfKernelBasis,
      alpha: Double,
      config: PhrfTrialConfig,
      clock: PhrfClock
  ): Either[PhrfTrialRefusal, (PhrfTrainFit, PhrfStageSeconds)] =
    mlPlan(problem, basis, alpha).flatMap { plan =>
      val t0 = clock.nanos()
      ProfileHrfFit
        .prepare(plan, DataSelection.All, CanonicalTemporalWhitening.Shared(problem.spec.plan), decodePolicy(config.phrf))
        .left.map(e => PhrfTrialRefusal.Dataset("prepare", PhrfTrialAssembly.fmt(e)))
        .flatMap { prepared =>
          val prepSeconds = seconds(clock, t0)
          val t1 = clock.nanos()
          val blocks = ArrayBuffer.empty[ProfileFitBlock]
          for
            reader <- SynchronousFmriDataset.readerFor(problem.dataset).left.map(e => PhrfTrialRefusal.Setup("reader", PhrfTrialAssembly.fmt(e)))
            summary <- prepared.run(reader, collecting(blocks)).left.map(e => PhrfTrialRefusal.Dataset("run", PhrfTrialAssembly.fmt(e)))
            runSeconds = seconds(clock, t1)
            results <- byVoxel(problem, blocks.toSeq)
            fit <- trainFitOf(problem, alpha, results, PhrfWorkReceipt.from("fold-fit", summary, results.count(_.criterionEvidence.nonEmpty)))
          yield (fit, PhrfStageSeconds(prepSeconds, runSeconds))
        }
    }

  private def trainFitOf(
      problem: PhrfTrialProblem,
      alpha: Double,
      results: Vector[ProfileVoxelResult],
      receipt: PhrfWorkReceipt
  ): Either[PhrfTrialRefusal, PhrfTrainFit] =
    val n = problem.trials
    val v = problem.voxels
    val data = new Array[Double](n * v)
    var bad: Option[PhrfTrialRefusal] = None
    var j = 0
    while bad.isEmpty && j < v do
      results(j).readout match
        case ProfileAmplitudeReadout.AdaptiveTrial(rd) if rd.trialAmplitudes.length == n && rd.trialAmplitudes.forall(_.isFinite) =>
          var i = 0
          while i < n do
            data(i * v + j) = rd.trialAmplitudes(i)
            i += 1
        case _ => bad = Some(PhrfTrialRefusal.Setup("run", "NoTrialReadout"))
      j += 1
    bad match
      case Some(e) => Left(e)
      case None =>
        mat(n, v, data).map(m => new PhrfTrainFit(alpha, results.map(_.status), results.map(_.coordinates), m, receipt))

  /** Final-fit seconds: preparation, the legacy run, and the public outputs. */
  final case class FinalSeconds(prepare: Double, run: Double, outputs: Double)

  /**
    * The final fit on `problem` (every run) at `alpha`: the legacy run for terminal ML evidence and the receipt, then
    * `prepared.trialOutputs` and `view.run(reader, TrialAmplitudes(Density), ExactShape, sink)` for the amplitudes. A
    * voxel whose decode is not `Accepted` is a typed `Refused("decode_<status>")`; the rest are converted to E-trial.
    */
  def finalFit(
      problem: PhrfTrialProblem,
      basis: HrfKernelBasis,
      alpha: Double,
      config: PhrfTrialConfig,
      clock: PhrfClock
  ): Either[PhrfTrialRefusal, (PhrfTrialFit, FinalSeconds)] =
    mlPlan(problem, basis, alpha).flatMap { plan =>
      val t0 = clock.nanos()
      ProfileHrfFit
        .prepare(plan, DataSelection.All, CanonicalTemporalWhitening.Shared(problem.spec.plan), decodePolicy(config.phrf))
        .left.map(e => PhrfTrialRefusal.Dataset("prepare", PhrfTrialAssembly.fmt(e)))
        .flatMap { prepared =>
          val prepSeconds = seconds(clock, t0)
          val t1 = clock.nanos()
          val blocks = ArrayBuffer.empty[ProfileFitBlock]
          val outs = ArrayBuffer.empty[ProfileTrialOutputBlock]
          val request = OutputRequest.TrialAmplitudes(NormalizationRule.Density)
          for
            reader <- SynchronousFmriDataset.readerFor(problem.dataset).left.map(e => PhrfTrialRefusal.Setup("reader", PhrfTrialAssembly.fmt(e)))
            summary <- prepared.run(reader, collecting(blocks)).left.map(e => PhrfTrialRefusal.Dataset("run", PhrfTrialAssembly.fmt(e)))
            runSeconds = seconds(clock, t1)
            results <- byVoxel(problem, blocks.toSeq)
            t2 = clock.nanos()
            view <- prepared.trialOutputs.left.map(e => PhrfTrialRefusal.Dataset("outputs", PhrfTrialAssembly.fmt(e)))
            outSummary <- view
              .run(reader, request, ProfileTrialReadoutMode.ExactShape, collectingOutputs(outs))
              .left.map(e => PhrfTrialRefusal.Dataset("outputs", PhrfTrialAssembly.fmt(e)))
            outSeconds = seconds(clock, t2)
            voxels <- voxelEstimates(problem, basis, config, results, outs.toSeq)
          yield
            val evidence = results.count(_.criterionEvidence.nonEmpty)
            (
              PhrfTrialFit(alpha, voxels, PhrfWorkReceipt.from("final-run", summary, evidence), PhrfWorkReceipt.from("final-outputs", outSummary, evidence)),
              FinalSeconds(prepSeconds, runSeconds, outSeconds)
            )
        }
    }

  private def voxelEstimates(
      problem: PhrfTrialProblem,
      basis: HrfKernelBasis,
      config: PhrfTrialConfig,
      results: Vector[ProfileVoxelResult],
      outs: Seq[ProfileTrialOutputBlock]
  ): Either[PhrfTrialRefusal, Vector[PhrfVoxelEstimate]] =
    val byId = outs.flatMap(_.results).map(r => r.voxelId -> r).toMap
    val family = basis.family
    Right(Vector.tabulate(problem.voxels) { v =>
      val legacy = results(v)
      val evidence = legacy.criterionEvidence.nonEmpty
      byId.get(v) match
        case None => PhrfVoxelEstimate(TrialArmStatus.Failed("voxel_not_delivered"), Some(legacy.status), legacy.coordinates, None, evidence)
        case Some(o) =>
          o.output match
            case ProfileTrialOutputOutcome.DecodeRefused(s) =>
              PhrfVoxelEstimate(TrialArmStatus.Refused(s"decode_${s.productPrefix}"), Some(s), legacy.coordinates, None, evidence)
            case ProfileTrialOutputOutcome.Emitted(_, value) =>
              val converted = for
                a <- value.trialAmplitudes.toRight("no_trial_amplitudes")
                peak <- PhrfETrial.peakHeight(family, value.actualCoordinates, config.peakHorizonSeconds, config.peakStepSeconds).left.map(_ => "peak_unavailable")
                e = PhrfETrial.convert(a, value.normalizationScale, peak)
                _ <- Either.cond(e.length == problem.trials && e.forall(_.isFinite), (), "nonfinite_e_trial")
              yield e
              converted match
                case Right(e) => PhrfVoxelEstimate(TrialArmStatus.Estimated, Some(o.selection.status), value.actualCoordinates, Some(e), evidence)
                case Left(c)  => PhrfVoxelEstimate(TrialArmStatus.Failed(c), Some(o.selection.status), value.actualCoordinates, None, evidence)
    })

  /** Basis of the configuration, as a typed refusal. */
  def basisOf(config: PhrfTrialConfig): Either[PhrfTrialRefusal, HrfKernelBasis] =
    config.phrf.basis.left.map(m => PhrfTrialRefusal.Setup("basis", m.take(60).filter(c => c.isLetterOrDigit).mkString))

  /** The inputs-match contract of design 2.0: whitening `inputs.y` with the shared plan reproduces the prepared arrays. */
  private[run] def checked(inputs: FitInputs, prep: CommonPrep): Either[PhrfTrialRefusal, Unit] =
    if prep.kind != CellKind.Trial then Left(PhrfTrialRefusal.WrongKind)
    else
      // exact SHA-256 over every raw input the preparation was built from (S2 `requireMatches`), then the structural checks
      prep.requireMatches(inputs).left.map(PhrfTrialRefusal.Prep(_)).flatMap { _ =>
        if prep.runId.length != inputs.runId.length || !prep.runId.sameElements(inputs.runId) || prep.whitened.y.rows != inputs.y.rows then
          Left(PhrfTrialRefusal.Inconsistent("preparation and inputs describe different datasets"))
        else if !ConditionRunner.whitenedMatches(prep, inputs) then Left(PhrfTrialRefusal.InputsNotPrepared)
        else ConditionRunner.sharedInput(prep).left.map(k => PhrfTrialRefusal.Inconsistent(k.code)).map(_ => ())
      }

  /**
    * The whole PHRF trial arm on one dataset: LOROCV over `grid` (36 fold fits on a 4-run dataset), then the final fit
    * (37 fits), E-trial in input trial order. Never throws.
    */
  def run(
      inputs: FitInputs,
      prep: CommonPrep,
      grid: AlphaGrid = AlphaGrid.Pilot,
      config: PhrfTrialConfig = PhrfTrialConfig(),
      clock: PhrfClock = PhrfClock.Wall
  ): Either[PhrfTrialRefusal, PhrfTrialOutcome] =
    for
      _ <- checked(inputs, prep)
      basis <- basisOf(config)
      native <- TrialNativeInputs.from(inputs, prep).left.map(e => PhrfTrialRefusal.Inconsistent(e.code))
      tuned <- PhrfTrialTuning.tune(inputs, prep, native, grid, config, clock, PhrfTrialTuning.mlEngine(basis, config, clock))
      problem <- PhrfTrialAssembly.problem(inputs, prep, prep.segments.map(_.runIndex), prep.sigma2All.sigma2, config)
      fin <- finalFit(problem, basis, tuned.record.selection.selectedAlpha, config, clock)
      (fit, secs) = fin
    yield PhrfTrialOutcome(fit, tuned.record, prep.sigma2All.sigma2, tuned.timings.withFinal(secs))

  /**
    * The rLSS settings of the pilot path: the shared grid, PHRF's TRUE effective-df targets ([[PhrfEdf]], the trace
    * `tr(S X)` of the amplitude smoother at the canonical shape, computed per design from the design alone: `forRuns`
    * builds the problem of exactly the runs asked for, a training fold or all runs), and the derived penalty scale
    * ([[PhrfPenaltyScale]]), which is a REPORTED DIAGNOSTIC only. The pilot path must always pass these targets;
    * `targets = None` and `PenaltyScale.Identity` are test-only.
    */
  def rlssSettings(inputs: FitInputs, prep: CommonPrep, grid: AlphaGrid = AlphaGrid.Pilot, config: PhrfTrialConfig = PhrfTrialConfig()): Either[PhrfTrialRefusal, RlssSettings] =
    PhrfPenaltyScale.derive(inputs, prep, config).map { r =>
      val targets = EdfTargets(runs =>
        PhrfEdf.forRuns(inputs, prep, runs.toVector.sorted, grid, config).map(_.edf).left.map(_.message))
      RlssSettings(grid, r.scale, Some(targets))
    }

/** Adapter to the scorer's input contract (S8, `score.Inputs`): E-trial in input trial order, a typed status per voxel. */
object PhrfTrialScoring:

  val method: Method = Method.Phrf

  def outcomes(fit: PhrfTrialFit): Vector[VoxelOutcome[TrialEstimate]] =
    fit.voxels.map { v =>
      v.status match
        case TrialArmStatus.Estimated =>
          v.eTrial.map(TrialEstimate.of) match
            case Some(Right(e)) => VoxelOutcome.Estimated(e)
            case _              => VoxelOutcome.Failed
        case TrialArmStatus.Refused(_) => VoxelOutcome.Refused
        case TrialArmStatus.Failed(_)  => VoxelOutcome.Failed
    }

  /** A dataset-level refusal or failure marks every voxel alike (design 2.3). */
  def outcomes(result: Either[PhrfTrialRefusal, PhrfTrialOutcome], voxels: Int): Vector[VoxelOutcome[TrialEstimate]] =
    result match
      case Right(o) => outcomes(o.fit)
      case Left(r) =>
        Vector.fill(voxels)(r.status match
          case TrialArmStatus.Failed(_) => VoxelOutcome.Failed
          case _                        => VoxelOutcome.Refused)
