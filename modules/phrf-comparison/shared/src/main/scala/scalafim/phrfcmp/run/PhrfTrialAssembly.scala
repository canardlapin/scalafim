package scalafim.phrfcmp.run

import gale.linalg.{DMat, Matrix as GMatrix}

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.ar.{TimeSegment, WhiteningPlan}
import scalafim.fmri.design.{ConditionId, TrialId}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept, NuisanceCheck}
import scalafim.fmri.design.event.EventSchedule
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, TrialMembership}
import scalafim.fmri.hrf.Seconds
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.ProfileTrialDrive
import scalafim.image.SampleSpaces
import scalafim.phrfcmp.ingest.{CellKind, FitInputs, Matrix}
import scalafim.phrfcmp.prep.{CommonPrep, PrepRefusal, WhiteningSpec}

/** Typed reason the PHRF trial arm refused or failed; no exception crosses this boundary (design 2.3). */
enum PhrfTrialRefusal:
  case WrongKind
  /** The inputs are not the ones the common preparation was built from (whitened `y` differs bit for bit). */
  case InputsNotPrepared
  case Inconsistent(detail: String)
  /** Alpha 0 is the condition-means route and is never an ML trial fit. */
  case AlphaNotPositive(alpha: Double)
  /** A harness or assembly failure at `stage`; `error` is a constructor name. */
  case Setup(stage: String, error: String)
  case Prep(refusal: PrepRefusal)
  /** PHRF itself refused the dataset at `stage` (a `ProfileFitError` or plan error constructor name). */
  case Dataset(stage: String, error: String)
  case Cv(error: LorocvRefusal)
  case Df(error: DfMappingRefusal)

  def code: String = this match
    case WrongKind          => "phrf_wrong_kind"
    case InputsNotPrepared  => "phrf_inputs_not_prepared"
    case Inconsistent(_)    => "phrf_inconsistent_inputs"
    case AlphaNotPositive(_) => "phrf_alpha_not_positive"
    case Setup(s, e)        => s"phrf_setup_${s}_$e"
    case Prep(r)            => s"phrf_prep_${r.productPrefix}"
    case Dataset(s, e)      => s"phrf_dataset_${s}_$e"
    case Cv(e)              => e.code
    case Df(e)              => e.code

  def message: String = this match
    case WrongKind          => "the common preparation is not a trial cell"
    case InputsNotPrepared  => "the inputs are not the ones the common preparation was built from"
    case Inconsistent(d)    => s"PHRF trial inputs: $d"
    case AlphaNotPositive(a) => s"alpha $a is not positive; alpha 0 is the condition-means route and is never routed to ML"
    case Setup(s, e)        => s"PHRF setup failed at $s: $e"
    case Prep(r)            => r.message
    case Dataset(s, e)      => s"PHRF refused the dataset at $s: $e"
    case Cv(e)              => e.message
    case Df(e)              => e.message

  /**
    * Design 2.3. A dataset-level `ProfileFitError` is a typed refusal of PHRF (all voxels `Refused`); harness and
    * preparation failures, and a LOROCV evaluation that could not be formed, are `Failed`.
    */
  def status: TrialArmStatus = this match
    case Dataset(_, _) | AlphaNotPositive(_) | WrongKind => TrialArmStatus.Refused(code)
    case Cv(LorocvRefusal.NoTrainingCondition(_) | LorocvRefusal.DegenerateVariance(_)) => TrialArmStatus.Refused(code)
    case _ => TrialArmStatus.Failed(code)

/** A wall-clock source for stage timings. The JVM driver injects a thread-CPU clock; timings never enter a hash. */
trait PhrfClock:
  def nanos(): Long

object PhrfClock:
  val Wall: PhrfClock = new PhrfClock:
    def nanos(): Long = System.nanoTime()

/**
  * Configuration of the PHRF trial arm: the S0-pinned Gaussian setup shared with the condition arm (0.2 s lowering, 9x9
  * decode grid, 16 Newton steps, 20 jets, 40 exact evaluations, no prior) and the lag grid on which the trial peak
  * height is evaluated (design: E-trial is `a_i * max|h|`).
  *
  * `fixedShapeNodes` is the node grid of the PHRF-can fixed-shape readout; it has no decoder to feed, so the smallest
  * bank that brackets the canonical point is built.
  */
final case class PhrfTrialConfig(
    phrf: PhrfConditionConfig = PhrfTrialConfig.DefaultBase,
    peakHorizonSeconds: Double = 32.0,
    peakStepSeconds: Double = 0.01,
    fixedShapeNodes: Int = 3,
    /** sealed diagnostic: PHRF's edf at each voxel's decoded shape (not a decision input; costs about 0.1-0.3 s per voxel) */
    recordDecodedShapeEdf: Boolean = true
)

object PhrfTrialConfig:
  /**
    * The lowering grid is the generator's onset grid, 0.1 s (T-TS cells place onsets on 0.1 s multiples; T-TX cells are
    * TR-aligned). At 0.2 s an odd-0.1 s onset falls between lowering nodes and carries an O(h^2) interpolation error of
    * about 0.3% of the peak (S3 finding), about 200x the on-grid error; at 0.1 s it is about 1e-5. The cost is a larger
    * kernel basis build and lowering (receipt).
    */
  val DefaultBase: PhrfConditionConfig = PhrfConditionConfig(loweringSeconds = 0.1)

/**
  * One PHRF problem: a subset of runs (the training runs of a fold, or all runs for the final fit), assembled for
  * `ProfileHrfFit` from the RAW responses (PHRF whitens internally with `spec.plan`, the same plan and phi as every
  * native arm) with the frame placed at the generator's sample times.
  *
  * @param runs         original run ids, ascending
  * @param rows         original sample indices of those runs, ascending
  * @param trialIndex   original trial index of each local trial (input order)
  * @param spec         whitening plan and `FitConfig` for these runs only; the phi is the shared pre-fit rho bit for bit
  * @param sigma2       frozen noise variance, computed from the problem's own runs only
  */
final class PhrfTrialProblem private[run] (
    val runs: Vector[Int],
    val rows: Array[Int],
    val trialIndex: Array[Int],
    val trialCond: Array[Int],
    val trialStim: Array[Int],
    val conditions: Int,
    val spec: WhiteningSpec,
    val frame: SamplingFrame,
    val dataset: FmriDataset,
    val baseline: BaselineModel,
    val drive: ProfileTrialDrive,
    val sigma2: Double,
    val voxels: Int
):
  def trials: Int = trialIndex.length
  def timepoints: Int = rows.length

/** The held-out run's own design: its trials lowered on a one-run frame, and the one-run whitening plan. */
final class PhrfHeldOutDesign private[run] (
    val run: Int,
    val rows: Array[Int],
    val trialIndex: Array[Int],
    val expanded: ExpandedTrialDesign,
    val plan: WhiteningPlan
)

object PhrfTrialAssembly:

  private[run] def fmt(e: Any): String = e match
    case p: Product => p.productPrefix
    case other      => other.getClass.getSimpleName

  private val SampleTimeTolerance = 1e-9

  private def segmentsOf(lengths: Vector[Int]): Vector[TimeSegment] =
    val starts = lengths.scanLeft(0)(_ + _)
    Vector.tabulate(lengths.length)(r => TimeSegment(starts(r), starts(r + 1), r))

  private def rowsOf(inputs: FitInputs, runs: Vector[Int]): Array[Int] =
    val keep = runs.toSet
    Array.tabulate(inputs.runId.length)(identity).filter(t => keep.contains(inputs.runId(t)))

  /** Constant spacing of the first run; every run must then sit at `start + k * tr` (checked against the frame later). */
  private def trOf(inputs: FitInputs, rows: Array[Int]): Either[PhrfTrialRefusal, Double] =
    if rows.length < 2 then Left(PhrfTrialRefusal.Inconsistent("fewer than two samples"))
    else
      val tr = inputs.sampleTime(rows(1)) - inputs.sampleTime(rows(0))
      if tr > 0.0 && tr.isFinite then Right(tr) else Left(PhrfTrialRefusal.Setup("frame", "NonPositiveTr"))

  /**
    * The frame whose scan `k` of run `r` sits at `start_r + k * tr`, with `start_r` the run's first sample time. The
    * library default is `(k + 1/2) tr`, which is NOT the generator's convention; the frame's sample times are compared
    * with `FitInputs.sampleTime` before any fit and a mismatch refuses.
    */
  private[run] def frameOf(inputs: FitInputs, rows: Array[Int], lengths: Vector[Int]): Either[PhrfTrialRefusal, SamplingFrame] =
    trOf(inputs, rows).flatMap { tr =>
      val starts = lengths.scanLeft(0)(_ + _)
      val frame = SamplingFrame(
        blockLens = lengths,
        tr = lengths.map(_ => tr),
        startTime = lengths.indices.toVector.map(r => inputs.sampleTime(rows(starts(r))))
      )
      val times = frame.samples().map(_.value)
      val ok = times.length == rows.length && times.indices.forall(k => math.abs(times(k) - inputs.sampleTime(rows(k))) <= SampleTimeTolerance)
      if ok then Right(frame) else Left(PhrfTrialRefusal.Setup("frame", "SampleTimesDiffer"))
    }

  private def lengthsOf(inputs: FitInputs, runs: Vector[Int]): Vector[Int] =
    runs.map(r => inputs.runId.count(_ == r))

  /** Per-run baseline blocks: the generator nuisance without its intercepts (columns confined to one run), as S0 B8 requires. */
  private def baselineOf(nuisance: Matrix, rows: Array[Int], lengths: Vector[Int], frame: SamplingFrame): Either[PhrfTrialRefusal, BaselineModel] =
    val starts = lengths.scanLeft(0)(_ + _)
    val owners = (0 until nuisance.cols).map { j =>
      val ownedBy = lengths.indices.filter(r => (starts(r) until starts(r + 1)).exists(k => nuisance(rows(k), j) != 0.0))
      ownedBy.length match
        case 0 => Right(None) // supported only by runs outside this problem
        case 1 => Right(Some((j, ownedBy.head)))
        case _ => Left(PhrfTrialRefusal.Setup("baseline", "NuisanceColumnSpansRuns"))
    }
    owners.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None =>
        val own = owners.collect { case Right(Some(x)) => x }
        val mats = lengths.indices.map { r =>
          val cols = own.filter(_._2 == r).map(_._1)
          val n = lengths(r)
          Mat.unsafe(n, cols.length, Array.tabulate(n * cols.length)(k => nuisance(rows(starts(r) + k / cols.length), cols(k % cols.length))))
        }
        scala.util
          .Try(BaselineModel.build(frame, BaselineBasis.Constant, intercept = Intercept.Runwise, nuisanceList = Some(mats), nuisanceCheck = NuisanceCheck.None))
          .toEither
          .left
          .map(e => PhrfTrialRefusal.Setup("baseline", fmt(e)))

  private def datasetOf(inputs: FitInputs, rows: Array[Int], frame: SamplingFrame): Either[PhrfTrialRefusal, FmriDataset] =
    val v = inputs.y.rows
    val t = rows.length
    try
      val data = GMatrix.dense(t, v, (0 until t).flatMap(r => (0 until v).map(k => inputs.y(k, rows(r)))))
      Right(FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("phrf-cmp-trial"), data, SampleSpaces(Vector(v, 1, 1))), frame).dataset)
    catch case e: Exception => Left(PhrfTrialRefusal.Setup("dataset", fmt(e)))

  private def eventsOf(inputs: FitInputs, runs: Vector[Int]): Array[Int] =
    val keep = runs.toSet
    Array.tabulate(inputs.evOnset.length)(identity).filter(e => keep.contains(inputs.evRun(e)))

  private def validRuns(inputs: FitInputs, runs: Vector[Int]): Either[PhrfTrialRefusal, Unit] =
    if runs.isEmpty || runs != runs.distinct.sorted || !runs.forall(inputs.runId.contains) then
      Left(PhrfTrialRefusal.Inconsistent("runs must be distinct, ascending and present in run_id"))
    else Right(())

  /**
    * The training-side PHRF problem for `runs`. Nothing of any other run is read: rows, events, baseline columns,
    * whitening segments and (through `sigma2`) the noise variance all come from `runs` alone. `sigma2` is passed in so
    * that the caller states where it came from (`prep.sigma2Training(runs)` for a fold, `prep.sigma2All` for the final
    * fit).
    */
  def problem(
      inputs: FitInputs,
      prep: CommonPrep,
      runs: Vector[Int],
      sigma2: Double,
      config: PhrfTrialConfig
  ): Either[PhrfTrialRefusal, PhrfTrialProblem] =
    for
      _ <- validRuns(inputs, runs)
      _ <- Either.cond(prep.kind == CellKind.Trial, (), PhrfTrialRefusal.WrongKind)
      rows = rowsOf(inputs, runs)
      lengths = lengthsOf(inputs, runs)
      frame <- frameOf(inputs, rows, lengths)
      all = runs == prep.segments.map(_.runIndex)
      spec <-
        if all then Right(prep.spec)
        else WhiteningSpec.build(prep.rho, segmentsOf(lengths)).left.map(PhrfTrialRefusal.Prep(_))
      baseline <- baselineOf(prep.baselineNuisance, rows, lengths, frame)
      dataset <- datasetOf(inputs, rows, frame)
      events = eventsOf(inputs, runs)
      nCond = inputs.evCond.max + 1
      conds = events.map(inputs.evCond(_))
      _ <- Either.cond(events.nonEmpty && (0 until nCond).forall(c => conds.contains(c)), (), PhrfTrialRefusal.Inconsistent("a condition owns no trial in these runs"))
      drive <- driveOf(inputs, events, runs, nCond)
      _ <- Either.cond(sigma2.isFinite && sigma2 > 0.0, (), PhrfTrialRefusal.Setup("sigma2", "NonPositive"))
      _ <- Either.cond(onGrid(inputs, events, config.phrf.loweringSeconds), (), PhrfTrialRefusal.Setup("lowering", "OnsetsOffGrid"))
    yield new PhrfTrialProblem(
      runs, rows, events, conds, events.map(inputs.evStim(_)), nCond, spec, frame, dataset, baseline, drive, sigma2, inputs.y.rows
    )

  /** Every onset is a multiple of the lowering step: an off-grid onset would be interpolated, not evaluated. */
  private def onGrid(inputs: FitInputs, events: Array[Int], step: Double): Boolean =
    events.forall { e =>
      val k = inputs.evOnset(e) / step
      math.abs(k - math.rint(k)) < 1e-6
    }

  private def driveOf(inputs: FitInputs, events: Array[Int], runs: Vector[Int], nCond: Int): Either[PhrfTrialRefusal, ProfileTrialDrive] =
    val blocks = events.toVector.map(e => runs.indexOf(inputs.evRun(e)))
    val conds = events.toVector.map(inputs.evCond(_))
    for
      schedule <- EventSchedule
        .fromParts(events.toVector.map(e => Seconds(inputs.evOnset(e))), events.toVector.map(e => Seconds(inputs.evDuration(e))), blocks)
        .left.map(e => PhrfTrialRefusal.Setup("schedule", fmt(e)))
      membership <- TrialMembership.make(conds, nCond).left.map(e => PhrfTrialRefusal.Setup("membership", fmt(e)))
      labels = Vector.tabulate(nCond)(i => ConditionId.unsafe(s"c$i"))
      drive <- ProfileTrialDrive
        .make(schedule, membership, labels, events.toVector.map(e => TrialId.unsafe(s"trial$e")), conds.map(labels(_)))
        .left.map(e => PhrfTrialRefusal.Setup("drive", fmt(e)))
    yield drive

  /** The held-out run lowered for prediction: one block, the run's own first sample time, the shared kernel basis. */
  def heldOut(inputs: FitInputs, prep: CommonPrep, run: Int, basis: HrfKernelBasis): Either[PhrfTrialRefusal, PhrfHeldOutDesign] =
    for
      _ <- validRuns(inputs, Vector(run))
      rows = rowsOf(inputs, Vector(run))
      lengths = lengthsOf(inputs, Vector(run))
      frame <- frameOf(inputs, rows, lengths)
      events = eventsOf(inputs, Vector(run))
      nCond = inputs.evCond.max + 1
      conds = events.toVector.map(inputs.evCond(_))
      membership <- TrialMembership.make(conds, nCond).left.map(e => PhrfTrialRefusal.Setup("membership", fmt(e)))
      expanded <- ExpandedTrialDesign
        .lower(
          events.toVector.map(e => Seconds(inputs.evOnset(e))),
          Vector.fill(events.length)(0),
          events.toVector.map(e => Seconds(inputs.evDuration(e))),
          membership,
          frame,
          basis,
          basis.spec.fineStep.seconds
        )
        .left.map(e => PhrfTrialRefusal.Setup("lowering", fmt(e)))
      spec <- WhiteningSpec.build(prep.rho, segmentsOf(lengths)).left.map(PhrfTrialRefusal.Prep(_))
    yield new PhrfHeldOutDesign(run, rows, events, expanded, spec.plan)

  /** The expanded trial design of a problem, lowered exactly as `ProfileHrfFit.prepare` does. */
  private[run] def expandedOf(problem: PhrfTrialProblem, basis: HrfKernelBasis): Either[PhrfTrialRefusal, ExpandedTrialDesign] =
    val s = problem.drive.schedule
    ExpandedTrialDesign
      .lower(s.onsets, s.blockIds, s.durations, problem.drive.membership, problem.frame, basis, basis.spec.fineStep.seconds)
      .left.map(e => PhrfTrialRefusal.Setup("lowering", fmt(e)))

  private[run] def baselineMat(problem: PhrfTrialProblem): DMat =
    val d = problem.baseline.designMatrix
    DMat.tabulate(d.rows, d.cols)((r, c) => d(r, c))
