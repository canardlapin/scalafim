package scalafim.phrfcmp.run

import gale.linalg.{DMat, QROptions, QRPivoting}

import scalafim.fmri.fit.{DesignMatrix, Ols, ResponseBlock}
import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.hrf.family.GaussianFamily
import scalafim.fmri.fit.profile.TrialHeldOutPrediction
import scalafim.phrfcmp.ingest.{CellKind, FitInputs, Matrix, PhrfTrialSynth}
import scalafim.phrfcmp.ingest.PhrfTrialSynth.{Spec, Truth}
import scalafim.phrfcmp.prep.{CommonPrep, CommonPreparation, InputFingerprint, Whiten}
import scalafim.phrfcmp.score.VoxelOutcome

/**
  * S5 theory-grounded checks (JVM and JS), each against an independent computation:
  *  - E-trial conversion against direct evaluation of the true kernel (noise-free);
  *  - ExactShape amplitudes against a dense penalised solve at the decoded shape;
  *  - PHRF-can at alpha -> 0 (condition means) and alpha -> infinity (LSA) against native fits at the canonical shape;
  *  - the held-out prediction against the held-out data itself (units of the raw coefficients);
  *  - the real PHRF-to-rLSS penalty scale against directly convolved regressors.
  */
class PhrfTrialTheorySuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(30, "min")

  private val truths = Vector(Truth(5.5, 1.6, Vector(3.0, 5.0, 2.0)), Truth(4.4, 1.2, Vector(4.0, 2.0, 3.0)), Truth(6.5, 2.0, Vector(2.0, 3.0, 5.0)))
  private val noisySpec = Spec(truths, devSd = 0.0, noiseSd = 0.3, seed = 4L)
  private val config = PhrfTrialConfig()
  private lazy val basis = PhrfTrialRunner.basisOf(config).fold(e => fail(e.message), identity)

  /**
    * TEST-ONLY. `CommonPreparation` is ill-posed on exactly noise-free data (the residual rho is estimated from is
    * numerically empty), so the plan, rho and sigma2 come from a noisy sibling with identical events, nuisance and runs, and
    * the whitened response is recomputed bit for bit from the exact data with that plan (the construction S3 uses).
    */
  private def exactPrepFor(sibling: CommonPrep, exact: FitInputs): CommonPrep =
    val wy = Whiten.series(sibling.plan, exact.y).fold(e => fail(e.message), identity)
    sibling.copy(whitened = sibling.whitened.copy(y = wy), inputFingerprint = InputFingerprint.of(exact))

  private final case class World(spec: Spec, noisy: PhrfTrialSynth.Data, prep: CommonPrep, exact: PhrfTrialSynth.Data, exactPrep: CommonPrep):
    def allRuns: Vector[Int] = prep.segments.map(_.runIndex)
    /**
      * `sigma2` is the frozen noise variance of the ML criterion `J = E + sigma2 D`. On noise-free data the borrowed value
      * of the noisy sibling is inconsistent with the data and the determinant term pulls the decoded shape (observed: sd
      * off by 0.28), so the exact-data checks use a negligible variance, which makes the shape the profile minimum. 1e-5 is
      * chosen because the decoder's terminal test is knife-edge on exact data: with sigma2 = 1e-8 the JVM refuses one voxel
      * (BudgetExceeded), with 1e-6 and 1e-4 JS does, while 1e-5, 1e-3 and 1e-2 are Accepted on both (measured).
      */
    def exactProblem(runs: Vector[Int] = allRuns, sigma2: Double = 1e-5): PhrfTrialProblem =
      PhrfTrialAssembly.problem(exact.inputs, exactPrep, runs, sigma2, config).fold(e => fail(e.message), identity)
    def noisyProblem(runs: Vector[Int] = allRuns): PhrfTrialProblem =
      PhrfTrialAssembly.problem(noisy.inputs, prep, runs, prep.sigma2All.sigma2, config).fold(e => fail(e.message), identity)

  private def world(spec: Spec): World =
    val noisy = PhrfTrialSynth.generate(spec)
    val prep = CommonPreparation.prepare(noisy.inputs, CellKind.Trial).fold(e => fail(e.message), identity)
    val exact = PhrfTrialSynth.generate(spec.copy(noiseSd = 0.0))
    World(spec, noisy, prep, exact, exactPrep = exactPrepFor(prep, exact.inputs))

  // zero deviation: every trial of a condition has the condition amplitude, so no estimator is biased by shrinkage
  private lazy val flat = world(noisySpec)
  // stimulus-persistent deviations
  private lazy val persistent = world(noisySpec.copy(devSd = 0.6, seed = 5L))

  private def bits(d: Double) = java.lang.Double.doubleToLongBits(d)

  // ---------------------------------------------------------------------------------------------- E-trial

  test("peak height of the raw Gaussian kernel is 1 at any shape (refined past the 0.01 s grid); sign is kept by the conversion"):
    val fam = GaussianFamily.Default
    var worst = 0.0
    for tau <- Seq(3.0, 4.123, 5.0, 5.55, 7.987, 8.0); sd <- Seq(0.8, 1.234, 2.0, 3.0) do
      val p = PhrfETrial.peakHeight(fam, Vector(tau, math.log(sd)), 32.0, 0.01).fold(m => fail(m), identity)
      worst = math.max(worst, math.abs(p - 1.0))
    println(f"S5-PEAK max |peak - 1| over the chart = $worst%.3e")
    assert(worst < 1e-7, s"worst $worst")
    assertEquals(PhrfETrial.convert(Vector(-2.0, 3.0, 0.0), 0.5, 2.0), Vector(-2.0, 3.0, 0.0))
    assert(PhrfETrial.peakHeight(fam, Vector(50.0, 0.0), 32.0, 0.01).isLeft)
    assert(PhrfETrial.peakHeight(fam, Vector(5.0, 0.0), 32.0, 0.0).isLeft)

  test("E-trial equals direct evaluation of the true trial peak height (noise-free, zero deviation)"):
    val w = flat
    val (fit, _) = PhrfTrialRunner.finalFit(w.exactProblem(), basis, 1.0, config, PhrfClock.Wall).fold(e => fail(e.message), identity)
    assertEquals(fit.voxels.map(_.status).distinct, Vector(TrialArmStatus.Estimated))
    var worst = 0.0
    var scale = 0.0
    for (v, vox) <- fit.voxels.zipWithIndex if v.eTrial.isDefined do
      val truth = w.exact.eTrial(vox)
      val est = v.eTrial.get
      assertEquals(est.length, truth.length)
      scale = math.max(scale, truth.map(math.abs).max)
      for i <- truth.indices do worst = math.max(worst, math.abs(est(i) - truth(i)))
    println(f"S5-ETRIAL noise-free max |E-trial - direct| = $worst%.3e (largest true E-trial $scale%.3f)")
    // the 0.2 s lowering and rank-40 basis bound the error (S3 saw 1.6e-5 of the peak on the condition route)
    assert(worst < 2e-3 * scale, s"worst $worst of $scale")

  test("E-trial equals the raw (legacy) coefficient times the dense-grid peak of the raw kernel at the decoded shape"):
    val w = flat
    val problem = w.exactProblem()
    val (fit, _) = PhrfTrialRunner.finalFit(problem, basis, 1.0, config, PhrfClock.Wall).fold(e => fail(e.message), identity)
    val (train, _) = PhrfTrialRunner.trainFit(problem, basis, 1.0, config, PhrfClock.Wall).fold(e => fail(e.message), identity)
    var worst = 0.0
    for (v, vox) <- fit.voxels.zipWithIndex if v.eTrial.isDefined do
      // the public ExactShape output and the legacy run decode the same point
      assertEquals(v.coordinates.map(bits), train.coordinates(vox).map(bits), "decoded shape differs between the run and the public outputs")
      val tau = v.coordinates(0)
      val sd = math.exp(v.coordinates(1))
      // dense direct maximum of |exp(-(t - tau)^2 / (2 sd^2))| on a 1 ms grid
      var peak = 0.0
      var k = 0
      while k <= 32000 do
        val t = k / 1000.0
        peak = math.max(peak, math.abs(math.exp(-0.5 * (t - tau) * (t - tau) / (sd * sd))))
        k += 1
      for i <- v.eTrial.get.indices do worst = math.max(worst, math.abs(v.eTrial.get(i) - train.amplitudes(i, vox) * peak))
    println(f"S5-ETRIAL public (normalised x scale x peak) vs raw x dense peak: max abs diff $worst%.3e")
    assert(worst < 1e-6, s"worst $worst")

  // ---------------------------------------------------------------------------------------------- lowering grid

  private lazy val odd = world(noisySpec.copy(first = 4.1, gap = 8.3, seed = 7L)) // every onset an odd multiple of 0.1 s
  private lazy val config02 = PhrfTrialConfig(scalafim.phrfcmp.run.PhrfConditionConfig(loweringSeconds = 0.2))

  /**
    * Max |E-trial - true E-trial| / largest true E-trial of a noise-free, zero-deviation fixed-shape fit at each voxel's TRUE
    * shape with no shrinkage (alpha 2^20), lowered on `cfg`'s grid. Nothing but the lowering and the kernel basis can
    * contribute (the ML decoder is bypassed: on exact data its terminal Newton test is numerically unstable).
    */
  private def oddError(cfg: PhrfTrialConfig): Double =
    val w = odd
    val b = PhrfTrialRunner.basisOf(cfg).fold(e => fail(e.message), identity)
    val problem = w.exactProblem()
    var worst = 0.0
    var scale = 0.0
    for (t, vox) <- truths.zipWithIndex do
      val shape = PhrfCanonicalShape(Vector(t.tau, math.log(t.sd)), 0.0)
      val (fit, _) = PhrfCanProbe.fixedShapeFit(problem, b, shape, math.pow(2.0, 20), cfg, PhrfClock.Wall, w.exact.inputs.y).fold(e => fail(e.message), identity)
      val truth = w.exact.eTrial(vox)
      scale = math.max(scale, truth.map(math.abs).max)
      for i <- truth.indices do worst = math.max(worst, math.abs(fit.amplitudes(i, vox) - truth(i)))
    worst / scale

  test("the default lowering grid is the generator's 0.1 s onset grid; odd-0.1 s onsets meet the existing bound"):
    assertEqualsDouble(config.phrf.loweringSeconds, 0.1, 0.0)
    assert(odd.exact.inputs.evOnset.exists(o => math.abs(o * 10 - 2 * math.rint(o * 5)) > 0.5), "onsets must include odd multiples of 0.1 s")
    val e = oddError(config)
    println(f"S5-LOWERING 0.1 s grid, odd-0.1 s onsets: max relative E-trial error $e%.3e")
    assert(e < 2e-3, s"error $e")
    assert(e < 1e-4, s"error $e")

  test("contrast: lowering on 0.2 s fails the same bound with odd-0.1 s onsets, and an off-grid problem is refused"):
    val e = oddError(config02)
    val e01 = oddError(config)
    println(f"S5-LOWERING 0.2 s grid, odd-0.1 s onsets: max relative E-trial error $e%.3e (0.1 s grid: $e01%.3e, ratio ${e / e01}%.0f)")
    assert(e > 2e-3, s"0.2 s error $e should exceed the bound the 0.1 s grid meets")
    assert(e > 20 * e01)
    assertEquals(PhrfTrialAssembly.problem(odd.exact.inputs, odd.exactPrep, odd.allRuns, 1e-6, config02).left.map(_.code), Left("phrf_setup_lowering_OnsetsOffGrid"))

  // ---------------------------------------------------------------------------------------------- exact shape

  test("ExactShape amplitudes equal a dense penalised least-squares solve at the decoded shape"):
    val w = persistent
    val problem = w.noisyProblem()
    val alpha = 0.5
    val (train, _) = PhrfTrialRunner.trainFit(problem, basis, alpha, config, PhrfClock.Wall).fold(e => fail(e.message), identity)
    val expanded = PhrfTrialAssembly.expandedOf(problem, basis).fold(e => fail(e.message), identity)
    val n = problem.trials
    val t = problem.timepoints
    val plan = problem.spec.plan
    val nuis = WhiteningTransform.matrix(plan, PhrfTrialAssembly.baselineMat(problem)).fold(e => fail(e.toString), identity)
    val f = nuis.cols
    val lambda = 1.0 / alpha
    var worst = 0.0
    var scale = 0.0
    for vox <- 0 until problem.voxels do
      val theta = train.coordinates(vox)
      val c = TrialHeldOutPrediction.basisCoefficients(expanded, theta).fold(e => fail(e.message), identity)
      val raw = DMat.tabulate(t, n)((r, i) => (0 until expanded.rank).map(j => c(j) * expanded.term.data(r, j * n + i)).sum)
      val x = WhiteningTransform.matrix(plan, raw).fold(e => fail(e.toString), identity)
      val y = WhiteningTransform.matrix(plan, DMat.tabulate(t, 1)((r, _) => w.noisy.inputs.y(vox, problem.rows(r)))).fold(e => fail(e.toString), identity)
      // penalty lambda u'(I - M)u = lambda |(I - M) u|^2 with M the within-condition averaging projector
      val counts = Array.tabulate(problem.conditions)(k => problem.trialCond.count(_ == k))
      val penalty = DMat.tabulate(n, n)((i, j) =>
        math.sqrt(lambda) * ((if i == j then 1.0 else 0.0) - (if problem.trialCond(i) == problem.trialCond(j) then 1.0 / counts(problem.trialCond(i)) else 0.0)))
      val a = DMat.tabulate(t + n, n + f)((r, col) =>
        if r < t then (if col < n then x(r, col) else nuis(r, col - n)) else if col < n then penalty(r - t, col) else 0.0)
      val rhs = DMat.tabulate(t + n, 1)((r, _) => if r < t then y(r, 0) else 0.0)
      val beta = a.qr(QROptions(QRPivoting.Column, Some(1e-12))).solveLeastSquares(rhs).fold(e => fail(e.toString), identity)
      for i <- 0 until n do
        scale = math.max(scale, math.abs(beta(i, 0)))
        worst = math.max(worst, math.abs(beta(i, 0) - train.amplitudes(i, vox)))
    println(f"S5-EXACTSHAPE max |dense penalised solve - PHRF amplitudes| = $worst%.3e (largest amplitude $scale%.3f)")
    assert(worst < 1e-6 * math.max(1.0, scale), s"worst $worst")

  test("on noise-free zero-deviation data the decoded shape and the condition amplitudes are recovered"):
    val w = flat
    val (fit, _) = PhrfTrialRunner.finalFit(w.exactProblem(), basis, 1.0, config, PhrfClock.Wall).fold(e => fail(e.message), identity)
    var worstTau = 0.0
    var worstSd = 0.0
    for (v, vox) <- fit.voxels.zipWithIndex if v.eTrial.isDefined do
      worstTau = math.max(worstTau, math.abs(v.coordinates(0) - truths(vox).tau))
      worstSd = math.max(worstSd, math.abs(math.exp(v.coordinates(1)) - truths(vox).sd))
    println(f"S5-SHAPE noise-free tau error $worstTau%.3e, sd error $worstSd%.3e")
    assert(worstTau < 5e-3 && worstSd < 1e-2, s"tau $worstTau sd $worstSd")

  // ---------------------------------------------------------------------------------------------- limits at the canonical shape

  /** Direct unit-peak Gaussian regressor of the events selected by `pick`, sampled at the generator's sample times (lag in [0, 24]). */
  private def direct(in: FitInputs, tau: Double, sd: Double, pick: Int => Boolean): Array[Double] =
    val out = new Array[Double](in.sampleTime.length)
    var s = 0
    while s < out.length do
      var e = 0
      while e < in.evOnset.length do
        if pick(e) && in.evRun(e) == in.runId(s) then
          val lag = in.sampleTime(s) - in.evOnset(e)
          if lag >= 0.0 && lag <= 24.0 then out(s) += math.exp(-0.5 * (lag - tau) * (lag - tau) / (sd * sd))
        e += 1
      s += 1
    out

  /** Native fit: whiten `design` (T x k) with the shared plan, add the shared whitened nuisance, OLS; returns the leading k rows (k x V). */
  private def nativeFit(w: World, design: DMat): DMat =
    val p = w.prep
    val xw = WhiteningTransform.matrix(p.plan, design).fold(e => fail(e.toString), identity)
    val k = xw.cols
    val t = xw.rows
    val nu = p.whitened.nuisance
    val ri = p.whitened.runIntercepts
    val full = DMat.tabulate(t, k + nu.cols + ri.cols)((r, c) => if c < k then xw(r, c) else if c < k + nu.cols then nu(r, c - k) else ri(r, c - k - nu.cols))
    val v = p.whitened.y.rows
    val ols = Ols
      .fit(DesignMatrix.fromMatrix(full).fold(e => fail(e.toString), identity), ResponseBlock.fromMatrix(DMat.tabulate(t, v)((r, j) => p.whitened.y(j, r))).fold(e => fail(e.toString), identity))
      .fold(e => fail(e.toString), identity)
    DMat.tabulate(k, v)((r, c) => ols.coefficients(r, c))

  private lazy val canonical = PhrfCanonicalShape.derive(basis.family).fold(m => fail(m), identity)

  test("PHRF-can at alpha -> 0 equals the native condition-means GLS fit at the canonical shape"):
    val w = persistent
    val problem = w.noisyProblem()
    val (fit, _) = PhrfCanProbe
      .fixedShapeFit(problem, basis, canonical, math.pow(2.0, -24), config, PhrfClock.Wall, w.noisy.inputs.y)
      .fold(e => fail(e.message), identity)
    val tau = canonical.coordinates(0)
    val sd = math.exp(canonical.coordinates(1))
    val conds = w.noisy.inputs.evCond
    val nCond = conds.max + 1
    val design = DMat.tabulate(w.noisy.inputs.sampleTime.length, nCond)((r, c) => direct(w.noisy.inputs, tau, sd, e => conds(e) == c)(r))
    val native = nativeFit(w, design)
    var worst = 0.0
    var scale = 0.0
    var within = 0.0
    for vox <- 0 until problem.voxels; i <- 0 until problem.trials do
      val cOfTrial = problem.trialCond(i)
      scale = math.max(scale, math.abs(native(cOfTrial, vox)))
      worst = math.max(worst, math.abs(fit.amplitudes(i, vox) - native(cOfTrial, vox)))
      // all trials of a condition share one amplitude in the limit
      val first = problem.trialCond.indexOf(cOfTrial)
      within = math.max(within, math.abs(fit.amplitudes(i, vox) - fit.amplitudes(first, vox)))
    println(f"S5-LIMIT0 PHRF-can alpha=2^-24 vs native condition-means fit: max abs diff $worst%.3e (scale $scale%.3f), within-condition spread $within%.3e")
    // the 0.2 s lowering and rank-40 basis, plus the finite penalty, bound the agreement
    assert(worst < 2e-3 * scale, s"worst $worst of $scale")
    assert(within < 1e-5 * scale, s"within $within")

  test("PHRF-can at alpha -> infinity equals native LSA at the canonical shape"):
    val w = persistent
    val problem = w.noisyProblem()
    val (fit, _) = PhrfCanProbe
      .fixedShapeFit(problem, basis, canonical, math.pow(2.0, 24), config, PhrfClock.Wall, w.noisy.inputs.y)
      .fold(e => fail(e.message), identity)
    val tau = canonical.coordinates(0)
    val sd = math.exp(canonical.coordinates(1))
    val n = w.noisy.inputs.evOnset.length
    val cols = Array.tabulate(n)(i => direct(w.noisy.inputs, tau, sd, _ == i))
    val design = DMat.tabulate(w.noisy.inputs.sampleTime.length, n)((r, i) => cols(i)(r))
    val native = nativeFit(w, design)
    var worst = 0.0
    var scale = 0.0
    for vox <- 0 until problem.voxels; i <- 0 until problem.trials do
      scale = math.max(scale, math.abs(native(i, vox)))
      worst = math.max(worst, math.abs(fit.amplitudes(i, vox) - native(i, vox)))
    println(f"S5-LIMITINF PHRF-can alpha=2^24 vs native LSA at the canonical shape: max abs diff $worst%.3e (scale $scale%.3f)")
    assert(worst < 5e-3 * scale, s"worst $worst of $scale")

  // ---------------------------------------------------------------------------------------------- held-out prediction

  test("the held-out prediction from the true shape and true amplitudes reproduces the held-out data (units of the raw coefficients)"):
    val w = persistent
    val in = w.exact.inputs
    val ep = w.exactPrep
    val native = TrialNativeInputs.from(in, ep).fold(e => fail(e.message), identity)
    val fam = basis.family
    val heldOutRun = 2
    val folds = Lorocv.folds(in.runId).fold(e => fail(e.message), identity)
    val fold = folds(heldOutRun)
    val testTrials = Array.tabulate(native.trials)(identity).filter(i => native.trialRun(i) == heldOutRun)
    val trainProblem = w.exactProblem(fold.trainRuns)
    val held = PhrfTrialAssembly.heldOut(in, ep, heldOutRun, basis).fold(e => fail(e.message), identity)
    val test = native.restrict(fold.testRows, testTrials).fold(e => fail(e.code), identity)
    // truth in raw-coefficient units: amplitude (density) times the density scale 1 / (sd sqrt(2 pi))
    val raw = Vector.tabulate(truths.length)(v => w.exact.trialAmp(v).map(a => a / (truths(v).sd * math.sqrt(2.0 * math.Pi))))
    val coords = truths.map(t => Vector(t.tau, math.log(t.sd)))
    coords.foreach(c => assert(fam.chart.point(c).isRight))
    val amp = Array.tabulate(trainProblem.trials * truths.length)(k => raw(k % truths.length)(trainProblem.trialIndex(k / truths.length)))
    val trainFit = new PhrfTrainFit(
      1.0,
      Vector.fill(truths.length)(scalafim.fmri.fit.profile.DecodeStatus.Accepted),
      coords,
      Matrix.of(trainProblem.trials, truths.length, amp).fold(e => fail(e.message), identity),
      PhrfWorkReceipt("test", "none", 0, 0, Vector.empty, scalafim.fmri.fit.profile.ProfileDecoderWork(0, 0, 0, 0, 0, 0, 0, 0), None, None, 0, false, 0, 0, 0, 0)
    )
    val predicted = PhrfTrialTuning.predictWhitened(trainFit, trainProblem, held, test).fold(e => fail(e.message), identity)
    val score = FoldScore.predictedR2(test.y, predicted, test.fixed).fold(e => fail(e.message), identity)
    println(f"S5-HELDOUT noise-free held-out R2 from true shape and amplitudes = ${score.r2}%.8f (rss ${score.rss}%.3e of tss ${score.tss}%.3e)")
    assert(score.r2 > 0.999, s"R2 ${score.r2}")

  test("noise-free LOROCV: larger alpha (weaker shrinkage) predicts persistent deviations better than the strongest shrinkage"):
    val w = persistent
    val native = TrialNativeInputs.from(w.exact.inputs, w.exactPrep).fold(e => fail(e.message), identity)
    val grid = AlphaGrid.from(Vector(1.0 / 64.0, 4.0)).fold(m => fail(m), identity)
    val tuned = PhrfTrialTuning
      .tune(w.exact.inputs, w.exactPrep, native, grid, config, PhrfClock.Wall, PhrfTrialTuning.mlEngine(basis, config, PhrfClock.Wall))
      .fold(e => fail(e.message), identity)
    val s = tuned.record.folds.map(_.scores)
    println(s"S5-LOROCV noise-free fold scores (alpha 1/64, 4): ${s.map(_.map(x => f"$x%.4f").mkString("/")).mkString(" ")}")
    assert(s.forall(f => f(1) > f(0)), s"scores $s")
    assertEquals(tuned.record.selection.selected, 1)
    assert(tuned.record.selection.atUpperEnd)

  // ---------------------------------------------------------------------------------------------- penalty scale

  test("the canonical shape is the squared-error minimum of the SPM canonical response within the chart (local-minimum check)"):
    val fam = basis.family
    val c = canonical
    assert(fam.chart.point(c.coordinates).isRight)
    val here = PhrfCanonicalShape.objective(fam, c.coordinates)
    assertEqualsDouble(here, c.objective, 1e-12)
    for (dt, ds) <- Seq((1e-3, 0.0), (-1e-3, 0.0), (0.0, 1e-3), (0.0, -1e-3)) do
      val moved = Vector(c.coordinates(0) + dt, c.coordinates(1) + ds)
      if fam.chart.point(moved).isRight then assert(PhrfCanonicalShape.objective(fam, moved) >= here, s"moved by ($dt, $ds)")
    println(f"S5-CANONICAL tau=${c.coordinates(0)}%.5f sd=${math.exp(c.coordinates(1))}%.5f (logSd ${c.coordinates(1)}%.5f), squared error ${c.objective}%.5f")
    assert(c.coordinates(0) > 3.0 && c.coordinates(0) < 8.0)

  test("the penalty scale: PHRF squared norms against directly convolved, whitened regressors; centring 1 - 1/n_c; used by the rLSS pilot path"):
    val w = persistent
    val r = PhrfPenaltyScale.derive(w.noisy.inputs, w.prep, config).fold(e => fail(e.message), identity)
    val in = w.noisy.inputs
    val tau = r.shape.coordinates(0)
    val sd = math.exp(r.shape.coordinates(1))
    val n = in.evOnset.length
    var direct2 = 0.0
    for i <- 0 until n do
      val col = direct(in, tau, sd, _ == i)
      val wcol = WhiteningTransform.matrix(w.prep.plan, DMat.tabulate(col.length, 1)((row, _) => col(row))).fold(e => fail(e.toString), identity)
      var s = 0.0
      var row = 0
      while row < wcol.rows do
        s += wcol(row, 0) * wcol(row, 0)
        row += 1
      direct2 += s
    val rel = math.abs(r.phrfNorm2 - direct2) / direct2
    println(f"S5-PENALTY phrf norm2 sum ${r.phrfNorm2}%.4f vs direct $direct2%.4f (rel $rel%.2e); rlss norm2 sum ${r.rlssNorm2}%.4f; kappa2 ${r.scale.shape}%.5f centring ${r.scale.centring}%.5f; spreads shape ${r.scale.shapeSpread}%.3f centring ${r.scale.centringSpread}%.3f; factor ${r.scale.factor}%.5f")
    assert(rel < 2e-3, s"relative $rel")
    assertEqualsDouble(r.scale.centring, 1.0 - 1.0 / 12.0, 1e-12) // 12 trials per condition (4 runs x 3 stimuli)
    assertEqualsDouble(r.scale.shape, r.rlssNorm2 / r.phrfNorm2, 1e-12)
    assert(r.scale.shape > 0.2 && r.scale.shape < 5.0)
    assert(r.scale != PenaltyScale.Identity)
    // the pilot path: rLSS tuned with the settings this arm derives. The mapping is on PHRF's TRUE edf (supplied here as
    // targets); the scale is a reported diagnostic.
    val settings = PhrfTrialRunner.rlssSettings(in, w.prep).fold(e => fail(e.message), identity)
    assertEquals(settings.scale, r.scale)
    assert(settings.targets.isDefined, "the pilot path always passes PHRF edf targets")
    val nativeIn = TrialNativeInputs.from(in, w.prep).fold(e => fail(e.code), identity)
    val out = TrialNativeRunner.run(scalafim.phrfcmp.prep.NativeArm.Rlss, nativeIn, settings).fold(e => fail(e.message), identity)
    val tuning = out.tuning.getOrElse(fail("rLSS has a tuning record"))
    val phrf = PhrfEdf.forRuns(in, w.prep, w.allRuns, settings.grid, config).fold(e => fail(e.message), identity)
    for k <- 0 until settings.grid.size do assertEqualsDouble(tuning.finalMap.targetEdf(k), phrf.edf(k), 1e-12 * phrf.edf(k), s"target $k")
    if !tuning.finalMap.flagged then assert(tuning.finalMap.maxEdfResidual < 1e-6, s"residual ${tuning.finalMap.maxEdfResidual}")
    val diagnostic = tuning.finalMap.scaleDiagnosticRidges.getOrElse(fail("diagnostic ridges are reported"))
    for k <- 0 until settings.grid.size do assertEqualsDouble(diagnostic(k), r.scale.penalty(settings.grid.lambda(k)), 1e-12 * diagnostic(k), s"diagnostic $k")

  // ---------------------------------------------------------------------------------------------- typed refusal at the decoder

  test("a voxel whose decode is not Accepted is a typed per-voxel refusal; its neighbour is estimated"):
    val w = world(noisySpec.copy(truths = Vector(Truth(5.5, 1.6, Vector(0.0, 0.0, 0.0)), truths(0)), seed = 6L))
    val (fit, _) = PhrfTrialRunner.finalFit(w.exactProblem(), basis, 1.0, config, PhrfClock.Wall).fold(e => fail(e.message), identity)
    println(s"S5-REFUSAL statuses ${fit.voxels.map(v => (v.status, v.decode, v.coordinates))}")
    assert(fit.voxels(0).status.isInstanceOf[TrialArmStatus.Refused], fit.voxels(0).status.toString)
    assert(fit.voxels(0).eTrial.isEmpty)
    assertEquals(fit.voxels(1).status, TrialArmStatus.Estimated)
    val outs = PhrfTrialScoring.outcomes(fit)
    assertEquals(outs(0), VoxelOutcome.Refused)
    assert(outs(1).toOption.isDefined)
