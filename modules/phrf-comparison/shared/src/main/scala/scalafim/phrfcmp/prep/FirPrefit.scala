package scalafim.phrfcmp.prep

import gale.linalg.DMat

import scalafim.fmri.ar.{
  ArError,
  ArFitOptions,
  ArOrder,
  AcvfBias,
  CorrectionBudget,
  EstimationPolicy,
  NoiseEstimationLayout,
  NoiseFit,
  NoisePooling,
  RunCorrection,
  WhiteningPlan
}
import scalafim.fmri.hrf.{HrfFunctions, Lag}
import scalafim.phrfcmp.ingest.{CellKind, FitInputs, Matrix}

/**
  * Per-dataset AR-heterogeneity RECORD (design 2.0.3, F7). It documents how far the one global rho is from the data and
  * never gates anything; it is sealed with the ledger, not whitelisted.
  *
  * @param perRunRho      pooled-over-voxels lag-1 estimate of each run, same estimator as the global one
  * @param perRunStatus   conditioning outcome of each run's fit; a value is a CORRECTED estimate only when `Applied`
  * @param voxelRhoSd     sample SD across the voxelwise estimates whose runs were all `Applied` (0 with fewer than two)
  * @param voxelRho       the voxelwise estimates themselves
  * @param voxelCorrected per voxel: true only if every run of that voxel's fit was `Applied`; false entries are raw-path
  *                       values and are excluded from `voxelRhoSd`
  */
final case class ArHeterogeneityRecord(
    perRunRho: Vector[Double],
    perRunStatus: Vector[RunCorrection],
    voxelRhoSd: Double,
    voxelRho: Vector[Double],
    voxelCorrected: Vector[Boolean]
):
  /** Always false: this record never gates. */
  val gates: Boolean = false

  /** Deterministic text form (doubles as IEEE-754 bit patterns) so it can be hashed and sealed byte-identically. */
  def canonical: String =
    def bits(d: Double) = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(d))
    Vector(
      "phrf-cmp-s2-ar-heterogeneity-v2",
      "role=record-only",
      s"per_run_rho=${perRunRho.map(bits).mkString(",")}",
      s"per_run_status=${perRunStatus.map(ArFitProvenance.statusText).mkString(",")}",
      s"voxel_rho_sd=${bits(voxelRhoSd)}",
      s"voxel_rho=${voxelRho.map(bits).mkString(",")}",
      s"voxel_corrected=${voxelCorrected.map(b => if b then "1" else "0").mkString("")}"
    ).mkString("\n") + "\n"

  def sha256: String = scalafim.phrfcmp.ingest.Digests.sha256Hex(canonical.getBytes("UTF-8"))

/** Which pre-fit design the AR estimate comes from; chosen by [[CellKind]] only (owner decision rev 3, 2026-10-01). */
enum ArDesignLevel:
  /** FIR condition columns plus nuisance (condition cells). */
  case ConditionLevel

  /** Condition design plus one canonical column per trial, variant A (trial cells). */
  case TrialLevel

object ArDesignLevel:
  def forKind(kind: CellKind): ArDesignLevel = kind match
    case CellKind.Condition => ConditionLevel
    case CellKind.Trial     => TrialLevel

/** The estimation policy of the pilot's AR fit, as a recorded value (the AR module's policy carries a matrix). */
enum ArFitPolicy:
  case DesignCorrected

/**
  * Provenance of the one AR fit behind the whitening (owner decision 2026-10-01, rev 3). Nothing here is silent: the policy,
  * the lag budget actually used, the conditioning outcome of every run, phi and the pooled autocovariance are all
  * recorded and sealed with the ledger.
  *
  * @param level         the pre-fit design the residuals come from
  * @param budget        the requested [[CorrectionBudget]]
  * @param requestedLag  lag budget the policy resolved to (fmrireg's adaptive rule), before any residual-df cap
  * @param lagUsed       lag the bias system was actually solved over
  * @param residualDf    residual degrees of freedom of the AR design
  * @param corrections   per-run conditioning outcome; the pilot refuses unless every run is `Applied`
  * @param phi           the pooled AR(1) coefficient, exactly the value placed on the plan and the `FitConfig`
  * @param gamma         the corrected pooled autocovariance the coefficient came from
  * @param stationarityBound the AR module's stationarity bound; a pooled |phi| at the bound means it was clamped
  * @param pooledClamped whether the pooled phi sits at the stationarity clamp (always false in a returned fit: a clamp is refused)
  * @param clampedRuns   runs whose per-run (record-only) phi sits at the clamp
  * @param rhoRaw        the same AR module's uncorrected AR(1) estimate (diagnostic only; never used downstream)
  */
final case class ArFitProvenance(
    level: ArDesignLevel,
    policy: ArFitPolicy,
    budget: CorrectionBudget,
    requestedLag: Int,
    lagUsed: Int,
    residualDf: Int,
    corrections: Vector[RunCorrection],
    phi: Double,
    gamma: Vector[Double],
    rhoRaw: Double,
    stationarityBound: Double,
    pooledClamped: Boolean,
    clampedRuns: Vector[Int]
):
  def budgetCapped: Boolean = lagUsed < requestedLag

  /** Deterministic text form (doubles as IEEE-754 bit patterns) for sealing. */
  def canonical: String =
    def bits(d: Double) = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(d))
    Vector(
      "phrf-cmp-s2-ar-fit-v4",
      s"design_level=$level",
      s"policy=$policy",
      s"budget=$budget",
      s"requested_lag=$requestedLag",
      s"lag_used=$lagUsed",
      s"residual_df=$residualDf",
      s"corrections=${corrections.map(ArFitProvenance.statusText).mkString(",")}",
      s"phi=${bits(phi)}",
      s"gamma=${gamma.map(bits).mkString(",")}",
      s"rho_raw=${bits(rhoRaw)}",
      s"stationarity_bound=${bits(stationarityBound)}",
      s"pooled_clamped=$pooledClamped",
      s"clamped_runs=${clampedRuns.mkString(",")}"
    ).mkString("\n") + "\n"

  def sha256: String = scalafim.phrfcmp.ingest.Digests.sha256Hex(canonical.getBytes("UTF-8"))

object ArFitProvenance:
  private[prep] def statusText(c: RunCorrection): String =
    def bits(d: Double) = java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(d))
    c match
      case RunCorrection.Uncorrected       => "uncorrected"
      case RunCorrection.Applied(r)        => s"applied:${bits(r)}"
      case RunCorrection.AppliedWithTailAnchor(r, directions) => s"tail-applied:${bits(r)}:$directions"
      case RunCorrection.IllConditioned(r) => s"ill-conditioned:${bits(r)}"
      case RunCorrection.SolveFallback(w)  => s"solve-fallback:${w.toString}"
      case RunCorrection.NotAttempted(w)   => s"not-attempted:${w.toString}"

/**
  * @param rho            global pooled AR(1), estimated by `ArEstimation.fitNoise` on the residuals of the AR design of the
  *                       cell kind with the design-corrected policy
  * @param rank           numerical rank `p` of the AR design
  * @param nSamples       `n`, the number of rows
  * @param designCols     columns of the AR design (also the sigma2 design)
  */
final case class PrefitResult(
    rho: Double,
    rank: Int,
    nSamples: Int,
    designCols: Int,
    heterogeneity: ArHeterogeneityRecord,
    provenance: ArFitProvenance
)

/**
  * AR(1) pre-fit (design 2.0.2, owner decision rev 3, 2026-10-01). One global rho per dataset, estimated from residuals
  * of the pre-fit design of the cell kind ([[ArDesignLevel.forKind]]): the condition design in condition cells, the
  * variant-A trial design in trial cells; always with fmriAR's residual-bias correction
  * (`EstimationPolicy.DesignCorrected`, bias matrices built once per dataset and design and reused by every record fit) and fmrireg's adaptive lag budget. This object owns no AR estimator. sigma2
  * uses the same design as the AR fit.
  *
  * The trial design is rank deficient by exactly one per condition (264 columns, rank 261 in T-TX-fast with 3
  * conditions). With onsets and sample times on the integer 1 s grid (`tr_aligned_onsets`), the sum of a condition's
  * trial columns, `sum_e h(lag_e)`, equals `sum_b h(b) * FIR(c, b)`: the canonical response at integer lags lies in the
  * span of that condition's 1 s FIR bins. This is intended; the pivoted QR and the AR module both use the numerical
  * rank, and [[checkRank]] refuses if they ever disagree.
  */
object FirPrefit:

  /** FIR window and bin count: 1 s bins over [0, H), H = 32 s. */
  val FirBins: Int = 32

  /** Ceiling of fmrireg's adaptive budget, `CorrectionBudget.DefaultMaxLag` (fmriAR's `correction_max_lag` default). */
  val BudgetCeiling: Int = CorrectionBudget.DefaultMaxLag

  val Budget: CorrectionBudget = CorrectionBudget.Adaptive(BudgetCeiling)

  /** Order 1, global pooling, exact first-sample AR(1): the pilot's O1 options. */
  val Options: ArFitOptions = ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Global, exactFirstAr1 = true)

  /** The same options with per-run pooling (heterogeneity record only). */
  private val RunOptions: ArFitOptions = ArFitOptions(order = ArOrder.Fixed(1), pooling = NoisePooling.Run, exactFirstAr1 = true)

  /** Raw SPMG1 peak (`HrfFunctions.spmg1` maximum), used only to put the LSA columns on a peak-1 scale. */
  private val Spmg1Peak: Double = 0.17544119701700001

  private def nConditions(i: FitInputs): Either[PrepRefusal, Int] =
    if i.evCond.isEmpty then Left(PrepRefusal.Design("no events"))
    else if i.evCond.exists(_ < 0) then Left(PrepRefusal.Design("negative condition index"))
    else Right(i.evCond.max + 1)

  private def validate(i: FitInputs, kind: CellKind): Either[PrepRefusal, Int] =
    val t = i.y.cols
    if i.sampleTime.length != t || i.runId.length != t || i.nuisance.rows != t then
      Left(PrepRefusal.Inconsistent(s"time axes disagree (y $t, sample_time ${i.sampleTime.length}, run_id ${i.runId.length}, nuisance ${i.nuisance.rows})"))
    else if i.evOnset.length != i.evCond.length || i.evOnset.length != i.evRun.length then
      Left(PrepRefusal.Inconsistent("event arrays differ in length"))
    else if (kind == CellKind.Trial) != i.yPool.isDefined then
      Left(PrepRefusal.Inconsistent(s"cell kind $kind but y_pool is ${if i.yPool.isDefined then "present" else "absent"}"))
    else nConditions(i)

  /** Condition-level pre-fit design, T x p: `[condition FIR columns] [generator nuisance]`. The AR fit uses this in condition cells. */
  def conditionDesign(i: FitInputs, kind: CellKind): Either[PrepRefusal, Matrix] =
    validate(i, kind).flatMap(nCond => build(i, nCond, withTrialColumns = false))

  /**
    * Trial-level (variant A) design; the AR fit and sigma2 use it in trial cells (S2.7): `[trial LSA columns (trial cells only)] [condition FIR] [generator nuisance]`.
    * For condition cells it equals [[conditionDesign]].
    */
  def design(i: FitInputs, kind: CellKind): Either[PrepRefusal, Matrix] =
    validate(i, kind).flatMap(nCond => build(i, nCond, withTrialColumns = kind == CellKind.Trial))

  private def build(i: FitInputs, nCond: Int, withTrialColumns: Boolean): Either[PrepRefusal, Matrix] =
    val t = i.y.cols
    val nEv = i.evOnset.length
    val nLsa = if withTrialColumns then nEv else 0
    val nFir = nCond * FirBins
    val p = nLsa + nFir + i.nuisance.cols
    val x = new Array[Double](t * p)
    var e = 0
    while e < nEv do
      var s = 0
      while s < t do
        if i.runId(s) == i.evRun(e) then
          val lag = i.sampleTime(s) - i.evOnset(e)
          val bin = math.floor(lag + 1e-9).toInt
          if lag > -1e-9 && bin >= 0 && bin < FirBins then x(s * p + nLsa + i.evCond(e) * FirBins + bin) += 1.0
          if nLsa > 0 && lag > 0.0 && lag < FirBins.toDouble then
            x(s * p + e) = HrfFunctions.spmg1(Lag(lag)) / Spmg1Peak
        s += 1
      e += 1
    var s = 0
    while s < t do
      System.arraycopy(i.nuisance.data, s * i.nuisance.cols, x, s * p + nLsa + nFir, i.nuisance.cols)
      s += 1
    Matrix.of(t, p, x).left.map(err => PrepRefusal.Design(err.message))

  /** Pre-fit result and the sigma2 design (the latter so `CommonPreparation` builds it once). */
  def prefit(i: FitInputs, kind: CellKind): Either[PrepRefusal, (PrefitResult, Matrix)] =
    val level = ArDesignLevel.forKind(kind)
    for
      x <- arDesign(i, kind)
      result <- fit(i, x, level)
    yield (result, x)

  /** The design the AR fit and sigma2 use for `kind`. */
  def arDesign(i: FitInputs, kind: CellKind): Either[PrepRefusal, Matrix] =
    ArDesignLevel.forKind(kind) match
      case ArDesignLevel.ConditionLevel => conditionDesign(i, kind)
      case ArDesignLevel.TrialLevel     => design(i, kind)

  private def arError(e: ArError): PrepRefusal = PrepRefusal.ArEstimation(e.message)

  private def phiOf(plan: WhiteningPlan, index: Int = 0): Double = plan.coefficients(index).phi.head

  /**
    * Residuals of `y` on the AR design `x` of `level`, then `NoiseFit.estimate` (which calls
    * `ArEstimation.fitNoise`) under `DesignCorrected(x, Adaptive(ceiling))`. Refuses unless every run's bias matrix passed
    * the conditioning gate: a pre-registered pilot must not silently fall back to the raw estimator.
    */
  def fit(i: FitInputs, x: Matrix, level: ArDesignLevel): Either[PrepRefusal, PrefitResult] =
    fitWith(i, x, level, prepared = true)

  /** The previous path: every fit rebuilds the bias matrices. Kept so a test can pin bit equality with [[fit]]. */
  private[prep] def fitUnprepared(i: FitInputs, x: Matrix, level: ArDesignLevel): Either[PrepRefusal, PrefitResult] =
    fitWith(i, x, level, prepared = false)

  private def fitWith(i: FitInputs, x: Matrix, level: ArDesignLevel, prepared: Boolean): Either[PrepRefusal, PrefitResult] =
    val t = i.y.cols
    val nRuns = i.runId.max + 1
    val xd = Linear.toDMat(x)
    val (res, rank) = Linear.residuals(xd, Linear.transposeToDMat(i.y))
    if t - rank <= 0 then Left(PrepRefusal.Design(s"design rank $rank leaves no residual degrees of freedom for $t samples"))
    else
      val policy = EstimationPolicy.DesignCorrected(xd, Budget)
      for
        segments <- Runs.segments(i.runId)
        layout <- NoiseEstimationLayout.excludingRows(segments, t, Set.empty).left.map(arError)
        // One bias-matrix build per dataset and design (target order 1 for every fit below, so binding accepts it).
        prep <-
          if prepared then AcvfBias.prepare(xd, layout, Budget, Options.order.maxRequested).left.map(arError).map(Some(_))
          else Right(None)
        estimate = (r: DMat, o: ArFitOptions) =>
          prep match
            case Some(pc) => NoiseFit.estimate(r, layout, o, xd, pc)
            case None     => NoiseFit.estimate(r, layout, o, policy)
        nf <- estimate(res, Options).left.map(arError)
        _ <- refuseNotApplied(nf.corrections)
        matrices <- nf.biasMatrices.orElse(prep.map(_.matrices)).toRight(PrepRefusal.ArEstimation("design-corrected fit returned no bias matrices"))
        _ <- checkRank(t, rank, matrices.residualDf)
        raw <- NoiseFit.estimate(res, layout, Options, EstimationPolicy.Raw).left.map(arError)
        rho = phiOf(nf.plan)
        _ <- if rho.isFinite && math.abs(rho) < 1.0 then Right(()) else Left(PrepRefusal.Rho(rho))
        _ <- refuseClamped(rho)
        runFit <- estimate(res, RunOptions).left.map(arError)
        perRun = (0 until nRuns).toVector.map(r => phiOf(runFit.plan, r))
        perVoxFits <- voxelwise(res, estimate)
      yield
        val perVox = perVoxFits.map(_._1)
        val corrected = perVoxFits.map(_._2)
        val used = perVox.zip(corrected).collect { case (r, true) => r }
        val mean = if used.isEmpty then 0.0 else used.sum / used.length
        val sd =
          if used.length < 2 then 0.0
          else math.sqrt(used.map(r => (r - mean) * (r - mean)).sum / (used.length - 1))
        val perRunStatus = runFit.corrections
        val clampedRuns = perRun.zipWithIndex.collect { case (r, run) if atClamp(r) => run }
        val prov = ArFitProvenance(
          level,
          ArFitPolicy.DesignCorrected,
          Budget,
          matrices.requestedLag,
          matrices.lag,
          matrices.residualDf,
          nf.corrections,
          rho,
          nf.acvf.headOption.getOrElse(Vector.empty),
          phiOf(raw.plan),
          Options.stationarityBound,
          pooledClamped = false,
          clampedRuns
        )
        PrefitResult(rho, rank, t, x.cols, ArHeterogeneityRecord(perRun, perRunStatus, sd, perVox, corrected), prov)

  /** The pooled fit must have used the bias correction on every run: any status other than `Applied` is a refusal. */
  private[prep] def refuseNotApplied(corrections: Vector[RunCorrection]): Either[PrepRefusal, Unit] =
    corrections.zipWithIndex.collectFirst {
      case (RunCorrection.IllConditioned(rc), run) => PrepRefusal.ArIllConditioned(run, rc)
      case (RunCorrection.SolveFallback(w), run)   => PrepRefusal.ArSolveFallback(run, w)
      case (RunCorrection.NotAttempted(w), run)    => PrepRefusal.ArNotAttempted(run, w)
      case (RunCorrection.Uncorrected, run)        => PrepRefusal.ArNotCorrected(run)
    } match
      case Some(refusal) => Left(refusal)
      case None          => Right(())

  /** An AR(1) coefficient sitting at the stationarity bound was clamped by the AR module. */
  private def atClamp(phi: Double): Boolean = math.abs(phi) >= Options.stationarityBound - 1e-12

  /** A clamped pooled phi at a true rho of about 0.3 means the fit is badly wrong: refuse, never whiten with it. */
  private[prep] def refuseClamped(pooledPhi: Double): Either[PrepRefusal, Unit] =
    if atClamp(pooledPhi) then Left(PrepRefusal.StationarityClamp(pooledPhi, Options.stationarityBound)) else Right(())

  /** The residual-forming fit (pivoted QR) and the AR module must agree on the design rank. */
  private[prep] def checkRank(rows: Int, linearRank: Int, arResidualDf: Int): Either[PrepRefusal, Unit] =
    if rows - arResidualDf == linearRank then Right(()) else Left(PrepRefusal.RankMismatch(linearRank, rows - arResidualDf))

  /** Voxelwise AR(1) with the same preparation and options (record only; the flag is true when every run was `Applied`). */
  private def voxelwise(
      res: DMat,
      estimate: (DMat, ArFitOptions) => Either[ArError, NoiseFit]
  ): Either[PrepRefusal, Vector[(Double, Boolean)]] =
    val out = Vector.newBuilder[(Double, Boolean)]
    var c = 0
    var failure: Option[PrepRefusal] = None
    while failure.isEmpty && c < res.cols do
      val col = res.slice(0, res.rows, c, c + 1)
      estimate(col, Options) match
        case Left(e) => failure = Some(arError(e))
        case Right(nf) =>
          out += ((phiOf(nf.plan), nf.corrections.forall { case RunCorrection.Applied(_) => true; case _ => false }))
      c += 1
    failure.toLeft(out.result())
