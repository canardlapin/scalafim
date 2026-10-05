package scalafim.phrfcmp.run

import gale.linalg.DMat

import scalafim.fmri.fit.{
  CoefficientBlock,
  DesignMatrix,
  FitError,
  LeastSquaresSeparate,
  LssFixedDesign,
  LssOptions,
  LssTrialDesign,
  Ols,
  ResponseBlock,
  RidgeLeastSquaresSeparate,
  RidgeLssGrouping,
  RidgeLssPrepared
}
import scalafim.phrfcmp.ingest.{CellKind, Digests, FitInputs, Matrix}
import scalafim.phrfcmp.prep.{CommonPrep, FirPrefit, NativeArm}

/** Typed reason a native trial arm refused or failed (design 2.3); no exception crosses this boundary. */
enum NativeTrialRefusal:
  case WrongKind(arm: NativeArm)
  case Inconsistent(detail: String)
  case Fit(error: FitError)
  case Df(error: DfMappingRefusal)
  case Cv(error: LorocvRefusal)
  case NoTargets
  case Targets(detail: String)

  def message: String = this match
    case WrongKind(a)     => s"$a is not a trial arm"
    case Inconsistent(d)  => s"trial inputs: $d"
    case Fit(e)           => e.message
    case Df(e)            => e.message
    case Cv(e)            => e.message
    case NoTargets        => "rLSS needs PHRF effective-df targets (RlssSettings.targets)"
    case Targets(d)       => s"effective-df targets: $d"

  /** Short status token (letters, digits, `_`), safe for ledger codes. */
  def code: String = this match
    case WrongKind(_)    => "native_wrong_kind"
    case Inconsistent(_) => "native_inconsistent_inputs"
    case Fit(e)          => s"fit_${e.productPrefix}"
    case Df(e)           => e.code
    case Cv(e)           => e.code
    case NoTargets       => "native_no_edf_targets"
    case Targets(_)      => "native_edf_targets"

  /** Design 2.3: a rank-deficient or non-finite fit is `Failed`; everything else is a `Refused` input or configuration. */
  def status: TrialArmStatus = this match
    case Fit(_) => TrialArmStatus.Failed(code)
    case _      => TrialArmStatus.Refused(code)

/** Per-voxel (or per-arm) status vocabulary of design 2.3, restricted to what a native trial arm can produce. */
enum TrialArmStatus:
  case Estimated
  case Refused(code: String)
  case Failed(code: String)

/**
  * Everything a native trial arm reads, whitened with the one shared plan: responses `T x V`, the canonical single-trial
  * regressors `T x N`, the fixed nuisance `T x F` (generator nuisance without intercepts plus the run intercepts), the run
  * of every sample and, per trial, its run, condition and stimulus. It carries no truth: it is built from [[FitInputs]]
  * and the common preparation only.
  */
final class TrialNativeInputs private (
    val y: Matrix,
    val trialDesign: Matrix,
    val fixed: Matrix,
    val runId: Array[Int],
    val trialRun: Array[Int],
    val trialCond: Array[Int],
    val trialStim: Array[Int],
    val trialIds: Array[Int]
):
  def timepoints: Int = y.rows
  def voxels: Int = y.cols
  def trials: Int = trialDesign.cols

  /** Rows `rows` (ascending) and trials `trials` (indices into this object); fixed columns that are all zero on the rows are dropped. */
  def restrict(rows: Array[Int], trials: Array[Int]): Either[NativeTrialRefusal, TrialNativeInputs] =
    val keep = fixedColumnsWithSupport(rows)
    val yR = TrialNativeInputs.selectRows(y, rows)
    val xR = TrialNativeInputs.selectBlock(trialDesign, rows, trials)
    val fR = TrialNativeInputs.selectBlock(fixed, rows, keep)
    TrialNativeInputs.of(
      yR,
      xR,
      fR,
      rows.map(runId(_)),
      trials.map(trialRun(_)),
      trials.map(trialCond(_)),
      trials.map(trialStim(_)),
      trials.map(trialIds(_))
    )

  /** Columns of `fixed` that have a non-zero entry on `rows`. */
  private def fixedColumnsWithSupport(rows: Array[Int]): Array[Int] =
    val out = Array.newBuilder[Int]
    var c = 0
    while c < fixed.cols do
      var any = false
      var i = 0
      while !any && i < rows.length do
        if fixed(rows(i), c) != 0.0 then any = true
        i += 1
      if any then out += c
      c += 1
    out.result()

object TrialNativeInputs:

  private def finiteIn(m: Matrix): Boolean =
    var i = 0
    while i < m.data.length do
      if !m.data(i).isFinite then return false
      i += 1
    true

  private[run] def selectRows(m: Matrix, rows: Array[Int]): Matrix =
    selectBlock(m, rows, Array.tabulate(m.cols)(identity))

  private[run] def selectBlock(m: Matrix, rows: Array[Int], cols: Array[Int]): Matrix =
    val out = new Array[Double](rows.length * cols.length)
    var r = 0
    while r < rows.length do
      var c = 0
      while c < cols.length do
        out(r * cols.length + c) = m(rows(r), cols(c))
        c += 1
      r += 1
    Matrix.of(rows.length, cols.length, out).fold(e => throw new IllegalStateException(e.message), identity)

  /** Validating constructor. */
  def of(
      y: Matrix,
      trialDesign: Matrix,
      fixed: Matrix,
      runId: Array[Int],
      trialRun: Array[Int],
      trialCond: Array[Int],
      trialStim: Array[Int],
      trialIds: Array[Int]
  ): Either[NativeTrialRefusal, TrialNativeInputs] =
    def bad(d: String) = Left(NativeTrialRefusal.Inconsistent(d))
    val t = y.rows
    val n = trialDesign.cols
    if trialDesign.rows != t || fixed.rows != t || runId.length != t then
      bad(s"time axes disagree (y $t, trial design ${trialDesign.rows}, fixed ${fixed.rows}, run_id ${runId.length})")
    else if Vector(trialRun, trialCond, trialStim, trialIds).exists(_.length != n) then bad("per-trial arrays differ from the number of trial columns")
    else if y.cols == 0 || n == 0 || t == 0 then bad("empty responses or trial design")
    else if !finiteIn(y) || !finiteIn(trialDesign) || !finiteIn(fixed) then bad("non-finite input")
    else if trialRun.exists(r => !runId.contains(r)) then bad("a trial belongs to a run with no samples")
    else if trialCond.exists(_ < 0) then bad("negative condition id")
    else Right(new TrialNativeInputs(y, trialDesign, fixed, runId, trialRun, trialCond, trialStim, trialIds))

  /**
    * From the dataset's [[FitInputs]] and its [[CommonPrep]]. The canonical single-trial regressors are the leading
    * `nEvents` columns of the whitened variant-A pre-fit design ([[FirPrefit.design]] layout `[trial][FIR][nuisance]`,
    * the same `Spmg1`-over-peak columns the AR fit used), so every trial arm reads the shared whitened arrays.
    */
  def from(inputs: FitInputs, prep: CommonPrep): Either[NativeTrialRefusal, TrialNativeInputs] =
    if prep.kind != CellKind.Trial then Left(NativeTrialRefusal.Inconsistent("common preparation is not a trial cell"))
    else
      val w = prep.whitened
      val nEv = inputs.evOnset.length
      val nCond = inputs.evCond.max + 1
      val expectedCols = nEv + nCond * FirPrefit.FirBins + inputs.nuisance.cols
      if w.prefitDesign.cols != expectedCols then
        Left(NativeTrialRefusal.Inconsistent(s"pre-fit design has ${w.prefitDesign.cols} columns, the trial layout needs $expectedCols"))
      else
        val t = w.y.cols
        val yTv = Matrix.of(t, w.y.rows, Array.tabulate(t * w.y.rows)(k => w.y(k % w.y.rows, k / w.y.rows)))
        val fixedCols = w.nuisance.cols + w.runIntercepts.cols
        val fixed = Array.tabulate(t * fixedCols)(k =>
          val r = k / fixedCols
          val c = k % fixedCols
          if c < w.nuisance.cols then w.nuisance(r, c) else w.runIntercepts(r, c - w.nuisance.cols))
        val x = selectBlock(w.prefitDesign, Array.tabulate(t)(identity), Array.tabulate(nEv)(identity))
        for
          yM <- yTv.left.map(e => NativeTrialRefusal.Inconsistent(e.message))
          fM <- Matrix.of(t, fixedCols, fixed).left.map(e => NativeTrialRefusal.Inconsistent(e.message))
          in <- of(yM, x, fM, inputs.runId, inputs.evRun, inputs.evCond, inputs.evStim, Array.tabulate(nEv)(identity))
        yield in

/**
  * One native arm's trial amplitudes (`N x V`, rows ordered as `trialIds`), the ridge used (rLSS only) and a status per
  * voxel: a voxel whose amplitudes are not all finite is `Failed("nonfinite_beta")`.
  */
final case class NativeTrialFit(
    arm: NativeArm,
    trialIds: Array[Int],
    amplitudes: Matrix,
    ridge: Option[Double],
    voxelStatus: Vector[TrialArmStatus]
):
  def voxels: Int = amplitudes.cols

  /** Deterministic byte form: arm name, dimensions, ridge bits and the amplitude bits (big endian). */
  def canonicalBytes: Array[Byte] =
    val head = s"phrf-cmp-s4-native-trial-v1\n$arm\n${amplitudes.rows}x${amplitudes.cols}\nridge=${ridge.fold("none")(r => java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(r)))}\n"
      .getBytes("UTF-8")
    val out = new Array[Byte](head.length + 4 * trialIds.length + 8 * amplitudes.data.length)
    System.arraycopy(head, 0, out, 0, head.length)
    var o = head.length
    var i = 0
    while i < trialIds.length do
      val v = trialIds(i)
      out(o) = (v >>> 24).toByte; out(o + 1) = (v >>> 16).toByte; out(o + 2) = (v >>> 8).toByte; out(o + 3) = v.toByte
      o += 4
      i += 1
    i = 0
    while i < amplitudes.data.length do
      val b = java.lang.Double.doubleToLongBits(amplitudes.data(i))
      var s = 56
      while s >= 0 do
        out(o) = (b >>> s).toByte
        o += 1
        s -= 8
      i += 1
    out

  def sha256: String = Digests.sha256Hex(canonicalBytes)

/**
  * The PHRF side's effective-df targets (owner decision 2026-10-02, see [[DfMapping]]): `forRuns(runs)` returns PHRF's
  * edf at every grid point's `lambda_k` for the design that uses exactly `runs` (the training runs of a fold, or all runs
  * for the final fit). It is computed from the design alone, with no data.
  */
final case class EdfTargets(forRuns: Set[Int] => Either[String, Vector[Double]])

/**
  * The rLSS settings: the shared grid, the PHRF-side edf targets and the penalty-scale factor, which is REPORTED ONLY
  * (a diagnostic in the [[DfMap]]; the mapping uses `targets`). Without `targets` the rLSS arm refuses.
  */
final case class RlssSettings(grid: AlphaGrid, scale: PenaltyScale, targets: Option[EdfTargets] = None)

/** The LOROCV record of the tuned rLSS arm: the selection, one df map per fold design and the final-design map. */
final case class RlssTuning(
    selection: LorocvSelection,
    foldMaps: Vector[DfMap],
    finalMap: DfMap,
    selectedRidge: Double
)

/** What a native trial arm returns: the fit, and for rLSS the tuning record. */
final case class NativeTrialOutcome(fit: NativeTrialFit, tuning: Option[RlssTuning])

/**
  * Trial-native arms (design 2.2): LSA, LSS, and condition-centred rLSS. All three read the shared whitened arrays; LSA
  * and LSS have no hyperparameter; rLSS is tuned by the shared LOROCV shell on the same grid, folds, score and tie rule as
  * PHRF. Entry points return typed refusals; the scheduler adapter (S7 `ArmRunner`) maps `refusal.status` to its result.
  */
object TrialNativeRunner:

  /** One entry point per arm. rLSS needs `rlss` settings; LSA and LSS ignore them. */
  def run(arm: NativeArm, in: TrialNativeInputs, rlss: RlssSettings): Either[NativeTrialRefusal, NativeTrialOutcome] =
    arm match
      case NativeArm.Lsa  => lsa(in).map(NativeTrialOutcome(_, None))
      case NativeArm.Lss  => lss(in).map(NativeTrialOutcome(_, None))
      case NativeArm.Rlss =>
        rlss.targets match
          case None    => Left(NativeTrialRefusal.NoTargets)
          case Some(t) => tuneRlss(in, rlss.grid, t, rlss.scale).map(r => NativeTrialOutcome(r.fit, Some(r.tuning)))
      case other          => Left(NativeTrialRefusal.WrongKind(other))

  /** Convenience: from the dataset's inputs and common preparation. */
  def run(arm: NativeArm, inputs: FitInputs, prep: CommonPrep, rlss: RlssSettings): Either[NativeTrialRefusal, NativeTrialOutcome] =
    TrialNativeInputs.from(inputs, prep).flatMap(run(arm, _, rlss))

  // ---------------------------------------------------------------------------------------------------- bridges

  private def toDMat(m: Matrix): DMat = DMat.tabulate(m.rows, m.cols)((r, c) => m(r, c))

  private def fromDMat(d: DMat): Either[NativeTrialRefusal, Matrix] =
    val out = new Array[Double](d.rows * d.cols)
    var r = 0
    while r < d.rows do
      var c = 0
      while c < d.cols do
        out(r * d.cols + c) = d(r, c)
        c += 1
      r += 1
    Matrix.of(d.rows, d.cols, out).left.map(e => NativeTrialRefusal.Inconsistent(e.message))

  private def fixedDesign(in: TrialNativeInputs): Either[NativeTrialRefusal, LssFixedDesign] =
    if in.fixed.cols == 0 then Right(LssFixedDesign.empty(in.timepoints))
    else LssFixedDesign.fromMatrix(toDMat(in.fixed)).left.map(NativeTrialRefusal.Fit(_))

  private def response(in: TrialNativeInputs): Either[NativeTrialRefusal, ResponseBlock] =
    ResponseBlock.fromMatrix(toDMat(in.y)).left.map(NativeTrialRefusal.Fit(_))

  private def statuses(m: Matrix): Vector[TrialArmStatus] =
    Vector.tabulate(m.cols) { v =>
      var ok = true
      var r = 0
      while ok && r < m.rows do
        if !m(r, v).isFinite then ok = false
        r += 1
      if ok then TrialArmStatus.Estimated else TrialArmStatus.Failed("nonfinite_beta")
    }

  private def packaged(arm: NativeArm, in: TrialNativeInputs, block: CoefficientBlock, ridge: Option[Double]): Either[NativeTrialRefusal, NativeTrialFit] =
    fromDMat(block.value).map(m => NativeTrialFit(arm, in.trialIds, m, ridge, statuses(m)))

  // ---------------------------------------------------------------------------------------------------- LSA, LSS

  /**
    * LSA: one regression of every voxel on all trial regressors plus the fixed nuisance. The inputs are already whitened
    * with the shared plan, so ordinary least squares on them is the GLS fit; the trial rows of the coefficients are the
    * amplitudes.
    */
  def lsa(in: TrialNativeInputs): Either[NativeTrialRefusal, NativeTrialFit] =
    val n = in.trials
    val f = in.fixed.cols
    val design = DMat.tabulate(in.timepoints, n + f)((r, c) => if c < n then in.trialDesign(r, c) else in.fixed(r, c - n))
    for
      d <- DesignMatrix.fromMatrix(design).left.map(NativeTrialRefusal.Fit(_))
      y <- response(in)
      fit <- Ols.fit(d, y).left.map(NativeTrialRefusal.Fit(_))
      top = DMat.tabulate(n, fit.coefficients.voxels)((r, c) => fit.coefficients(r, c))
      out <- packaged(NativeArm.Lsa, in, CoefficientBlock(top), None)
    yield out

  /** LSS: `LeastSquaresSeparate.fit` on the whitened inputs, defaults, no tuning. */
  def lss(in: TrialNativeInputs): Either[NativeTrialRefusal, NativeTrialFit] =
    for
      x <- LssTrialDesign.fromMatrix(toDMat(in.trialDesign)).left.map(NativeTrialRefusal.Fit(_))
      fixed <- fixedDesign(in)
      y <- response(in)
      fit <- LeastSquaresSeparate.fit(x, y, fixed, LssOptions()).left.map(NativeTrialRefusal.Fit(_))
      out <- packaged(NativeArm.Lss, in, fit.coefficients, None)
    yield out

  // ---------------------------------------------------------------------------------------------------- rLSS

  /** Dense condition ids `0 until G` in order of first appearance sorted by id (so the mapping is canonical). */
  private def denseConditions(cond: Array[Int]): Array[Int] =
    val ids = cond.distinct.sorted
    cond.map(c => ids.indexOf(c))

  def prepareRlss(in: TrialNativeInputs, grouping: RidgeLssGrouping): Either[NativeTrialRefusal, RidgeLssPrepared] =
    for
      x <- LssTrialDesign.fromMatrix(toDMat(in.trialDesign)).left.map(NativeTrialRefusal.Fit(_))
      fixed <- fixedDesign(in)
      p <- RidgeLeastSquaresSeparate.prepare(x, fixed, grouping).left.map(NativeTrialRefusal.Fit(_))
    yield p

  /** The condition-centred grouping of the inputs' trials. */
  def conditionGrouping(in: TrialNativeInputs): RidgeLssGrouping = RidgeLssGrouping.ByGroup(denseConditions(in.trialCond).toVector)

  /** rLSS at one ridge with an explicit grouping. */
  def rlss(in: TrialNativeInputs, grouping: RidgeLssGrouping, ridge: Double): Either[NativeTrialRefusal, NativeTrialFit] =
    for
      p <- prepareRlss(in, grouping)
      y <- response(in)
      proj <- p.project(y).left.map(NativeTrialRefusal.Fit(_))
      fit <- proj.solve(ridge).left.map(NativeTrialRefusal.Fit(_))
      out <- packaged(NativeArm.Rlss, in, fit.amplitudes, Some(ridge))
    yield out

  /** The LSS-equivalence flag: one pooled group at ridge 0 reproduces [[lss]]. */
  def rlssAsLss(in: TrialNativeInputs): Either[NativeTrialRefusal, NativeTrialFit] = rlss(in, RidgeLssGrouping.Pooled, 0.0)

  /** The tuned arm's amplitudes at one ridge, condition-centred (the form the pilot uses). */
  def rlssCentred(in: TrialNativeInputs, ridge: Double): Either[NativeTrialRefusal, NativeTrialFit] =
    rlss(in, conditionGrouping(in), ridge)

  /**
    * The nine ridges of `grid` on the design of `runs` (all runs when `None`): PHRF's edf targets for those runs are matched
    * on the rLSS amplitude edf of the design restricted to them. The penalty-scale ridges are attached as a diagnostic.
    */
  def dfMap(in: TrialNativeInputs, grid: AlphaGrid, targets: EdfTargets, scale: PenaltyScale, runs: Option[Set[Int]] = None): Either[NativeTrialRefusal, DfMap] =
    val used = runs.getOrElse(in.runId.toSet)
    for
      design <- if runs.isEmpty then Right(in) else restrictToRuns(in, used)
      prepared <- prepareRlss(design, conditionGrouping(design))
      t <- targets.forRuns(used).left.map(NativeTrialRefusal.Targets(_))
      m <- DfMapping.map(grid, t, EdfCurve.rlss(prepared), Some(scaleDiagnostic(design, grid, scale))).left.map(NativeTrialRefusal.Df(_))
    yield m

  /** `kappa^2 (1 - 1/n_c) lambda_k` with `n_c` recomputed on the design actually fitted (diagnostic only). */
  private def scaleDiagnostic(design: TrialNativeInputs, grid: AlphaGrid, scale: PenaltyScale): Vector[Double] =
    val centring = PenaltyScale.centringByTrial(design.trialCond).toOption.map(c => c.sum / c.length).getOrElse(scale.centring)
    Vector.tabulate(grid.size)(k => scale.shape * centring * grid.lambda(k))

  private def restrictToRuns(in: TrialNativeInputs, runs: Set[Int]): Either[NativeTrialRefusal, TrialNativeInputs] =
    in.restrict(Array.tabulate(in.timepoints)(identity).filter(t => runs.contains(in.runId(t))), Array.tabulate(in.trials)(identity).filter(i => runs.contains(in.trialRun(i))))

  /** The result of [[tuneRlss]]: the final fit on every run at the selected ridge and the tuning record. */
  final case class Tuned(fit: NativeTrialFit, tuning: RlssTuning)

  /**
    * Tune rLSS by leave-one-run-out cross-validation over `grid` and refit on every run at the selected alpha.
    *
    * Per fold (training runs only: rows, trials and fixed columns of the held-out run never enter the fit, the `q` or the
    * df map): prepare the condition-centred design, map the grid to ridges by the df mapping, solve all nine ridges from one
    * projection, predict each held-out trial by the training amplitude of its stimulus (condition mean if unseen), form the
    * predicted held-out signal `X_test * amplitude` and score it with the shared [[FoldScore]].
    */
  def tuneRlss(in: TrialNativeInputs, grid: AlphaGrid, targets: EdfTargets, scale: PenaltyScale): Either[NativeTrialRefusal, Tuned] =
    for
      folds <- Lorocv.folds(in.runId).left.map(NativeTrialRefusal.Cv(_))
      scored <- scoreFolds(in, folds, grid, targets, scale)
      (maps, table) = scored
      selection <- Lorocv.select(grid, table).left.map(NativeTrialRefusal.Cv(_))
      finalPrep <- prepareRlss(in, conditionGrouping(in))
      finalMap <- dfMap(in, grid, targets, scale)
      ridge = finalMap.ridges(selection.selected)
      y <- response(in)
      proj <- finalPrep.project(y).left.map(NativeTrialRefusal.Fit(_))
      fit <- proj.solve(ridge).left.map(NativeTrialRefusal.Fit(_))
      out <- packaged(NativeArm.Rlss, in, fit.amplitudes, Some(ridge))
    yield Tuned(out, RlssTuning(selection, maps, finalMap, ridge))

  private def scoreFolds(
      in: TrialNativeInputs,
      folds: Vector[Fold],
      grid: AlphaGrid,
      targets: EdfTargets,
      scale: PenaltyScale
  ): Either[NativeTrialRefusal, (Vector[DfMap], Vector[Vector[Double]])] =
    val maps = Vector.newBuilder[DfMap]
    val table = Vector.newBuilder[Vector[Double]]
    var failure: Option[NativeTrialRefusal] = None
    var f = 0
    while failure.isEmpty && f < folds.length do
      scoreFold(in, folds(f), f, grid, targets, scale) match
        case Left(e) => failure = Some(e)
        case Right((m, s)) =>
          maps += m
          table += s
      f += 1
    failure.toLeft((maps.result(), table.result()))

  private def scoreFold(
      in: TrialNativeInputs,
      fold: Fold,
      index: Int,
      grid: AlphaGrid,
      targets: EdfTargets,
      scale: PenaltyScale
  ): Either[NativeTrialRefusal, (DfMap, Vector[Double])] =
    val trainTrials = Array.tabulate(in.trials)(identity).filter(i => fold.trainRuns.contains(in.trialRun(i)))
    val testTrials = Array.tabulate(in.trials)(identity).filter(i => in.trialRun(i) == fold.heldOut)
    if trainTrials.isEmpty || testTrials.isEmpty then
      Left(NativeTrialRefusal.Cv(LorocvRefusal.Inconsistent(s"fold $index has no training or no held-out trial")))
    else
      for
        train <- in.restrict(fold.trainRows, trainTrials)
        test <- in.restrict(fold.testRows, testTrials)
        prepared <- prepareRlss(train, conditionGrouping(train))
        t <- targets.forRuns(fold.trainRuns.toSet).left.map(NativeTrialRefusal.Targets(_))
        map <- DfMapping.map(grid, t, EdfCurve.rlss(prepared), Some(scaleDiagnostic(train, grid, scale))).left.map(NativeTrialRefusal.Df(_))
        y <- response(train)
        proj <- prepared.project(y).left.map(NativeTrialRefusal.Fit(_))
        scores <- scoreGrid(proj, map, train, test, index)
      yield (map, scores)

  private def scoreGrid(
      proj: scalafim.fmri.fit.RidgeLssProjection,
      map: DfMap,
      train: TrialNativeInputs,
      test: TrialNativeInputs,
      fold: Int
  ): Either[NativeTrialRefusal, Vector[Double]] =
    val out = Vector.newBuilder[Double]
    var failure: Option[NativeTrialRefusal] = None
    var k = 0
    while failure.isEmpty && k < map.ridges.length do
      val one: Either[NativeTrialRefusal, Double] =
        for
          fit <- proj.solve(map.ridges(k)).left.map(NativeTrialRefusal.Fit(_))
          amp <- fromDMat(fit.amplitudes.value)
          predicted <- AmplitudePrediction.byStimulus(train.trialStim, train.trialCond, amp, test.trialStim, test.trialCond)
            .left.map(NativeTrialRefusal.Cv(_))
          signal <- FoldScore.predictedSignal(test.trialDesign, predicted).left.map(NativeTrialRefusal.Cv(_))
          score <- FoldScore.predictedR2(test.y, signal, test.fixed).left.map {
            case LorocvRefusal.DegenerateVariance(_) => NativeTrialRefusal.Cv(LorocvRefusal.DegenerateVariance(fold))
            case other                               => NativeTrialRefusal.Cv(other)
          }
        yield score.r2
      one match
        case Left(e)  => failure = Some(e)
        case Right(s) => out += s
      k += 1
    failure.toLeft(out.result())

/**
  * Adapter to the scorer's input contract (S8, `score.Inputs`). Amplitudes of the native arms are already E-trial: the
  * canonical regressors are `Spmg1` over its peak (peak-1 scale), so an amplitude is the trial's peak height and the
  * conversion "through the arm's own HRF" is the identity. Rows are in input trial order (`trialIds` ascending).
  */
object TrialNativeScoring:
  import scalafim.phrfcmp.score.{Method, TrialEstimate, VoxelOutcome}

  def method(arm: NativeArm): Option[Method] = arm match
    case NativeArm.Lsa  => Some(Method.Lsa)
    case NativeArm.Lss  => Some(Method.Lss)
    case NativeArm.Rlss => Some(Method.Rlss)
    case _              => None

  /** One outcome per voxel; a voxel flagged `Failed`, or whose values the scorer rejects, is `Failed`. */
  def outcomes(fit: NativeTrialFit): Vector[VoxelOutcome[TrialEstimate]] =
    Vector.tabulate(fit.voxels) { v =>
      fit.voxelStatus(v) match
        case TrialArmStatus.Estimated =>
          TrialEstimate.of(Vector.tabulate(fit.amplitudes.rows)(i => fit.amplitudes(i, v))) match
            case Right(e) => VoxelOutcome.Estimated(e)
            case Left(_)  => VoxelOutcome.Failed
        case TrialArmStatus.Refused(_) => VoxelOutcome.Refused
        case TrialArmStatus.Failed(_)  => VoxelOutcome.Failed
    }

  /** A dataset-level result: every voxel carries the same refusal or failure. */
  def outcomes(result: Either[NativeTrialRefusal, NativeTrialOutcome], voxels: Int): Vector[VoxelOutcome[TrialEstimate]] =
    result match
      case Right(o) => outcomes(o.fit)
      case Left(r) =>
        Vector.fill(voxels)(r.status match
          case TrialArmStatus.Failed(_) => VoxelOutcome.Failed
          case _                        => VoxelOutcome.Refused)
