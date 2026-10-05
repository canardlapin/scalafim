package scalafim.phrfcmp.run

import gale.linalg.{DMat, QROptions, QRPivoting}

import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.fit.profile.ProfileFitError
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{ProfileCriterion, ProfileHrfPlan}
import scalafim.phrfcmp.ingest.{CellKind, FitInputs, Matrix, PhrfTrialSynth}
import scalafim.phrfcmp.ingest.PhrfTrialSynth.{Spec, Truth}
import scalafim.phrfcmp.prep.{CommonPrep, CommonPreparation, FirPrefit, InputFingerprint, PrepSynth, Sigma2, Whiten}
import scalafim.phrfcmp.score.VoxelOutcome

/**
  * S5 mechanics on small PHRF-model data (JVM and JS): the assembly reads only the runs it is given, per-fold sigma2, alpha 0
  * never reaches ML, a poisoned held-out run changes only the score, hash-equal reruns, work receipts, typed refusals.
  */
class PhrfTrialSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(30, "min")

  private val truths = Vector(Truth(5.5, 1.6, Vector(3.0, 5.0, 2.0)), Truth(4.4, 1.2, Vector(4.0, 2.0, 3.0)), Truth(6.5, 2.0, Vector(2.0, 3.0, 5.0)))
  private val spec = Spec(truths, devSd = 0.6, noiseSd = 0.3, seed = 2L)
  private lazy val data = PhrfTrialSynth.generate(spec)
  private lazy val inputs: FitInputs = data.inputs
  private lazy val prep: CommonPrep = CommonPreparation.prepare(inputs, CellKind.Trial).fold(e => fail(e.message), identity)
  private lazy val config = PhrfTrialConfig()
  private lazy val basis = PhrfTrialRunner.basisOf(config).fold(e => fail(e.message), identity)
  private lazy val allRuns = prep.segments.map(_.runIndex)
  private val grid3: AlphaGrid = AlphaGrid.from(Vector(0.25, 1.0, 4.0)).fold(m => throw new AssertionError(m), identity)
  private def bits(d: Double) = java.lang.Double.doubleToLongBits(d)

  private def problemOf(runs: Vector[Int], sigma2: Double): PhrfTrialProblem =
    PhrfTrialAssembly.problem(inputs, prep, runs, sigma2, config).fold(e => fail(e.message), identity)

  private lazy val tuned: PhrfTuned =
    val native = TrialNativeInputs.from(inputs, prep).fold(e => fail(e.message), identity)
    PhrfTrialTuning.tune(inputs, prep, native, grid3, config, PhrfClock.Wall, PhrfTrialTuning.mlEngine(basis, config, PhrfClock.Wall)).fold(e => fail(e.message), identity)

  private lazy val whole: PhrfTrialOutcome = PhrfTrialRunner.run(inputs, prep, grid3, config).fold(e => fail(e.message), identity)

  // ---------------------------------------------------------------------------------------------- assembly

  test("every problem's frame puts scan k at the generator's sample time; the library default frame would not"):
    val trap = SamplingFrame(blockLens = Vector(100), tr = Vector(1.0)).samples().map(_.value)
    assertEquals(trap.head, 0.5) // (k + 1/2) TR: the S0 spike's mistake
    for runs <- Seq(allRuns, Vector(0, 1, 3), Vector(2)) do
      val p = problemOf(runs, 0.5)
      val got = p.frame.samples().map(_.value)
      assertEquals(got.length, p.rows.length)
      assert(got.indices.forall(k => got(k) == inputs.sampleTime(p.rows(k))), s"runs $runs")
    // a frame that cannot reproduce the sample times (irregular spacing) is refused before any fit
    val jittered = PhrfTrialSynth.withSampleTime(inputs, inputs.sampleTime.zipWithIndex.map((t, k) => if k == 5 then t + 0.3 else t))
    assertEquals(PhrfTrialAssembly.problem(jittered, prep, allRuns, 0.5, config).left.map(_.code), Left("phrf_setup_frame_SampleTimesDiffer"))

  test("a training problem reads only its own runs: rows, events, baseline columns, whitening segments, phi bit for bit"):
    val p = problemOf(Vector(0, 2, 3), 0.5)
    assert(p.rows.forall(t => Set(0, 2, 3).contains(inputs.runId(t))))
    assertEquals(p.rows.length, 300)
    assertEquals(p.trials, 27)
    assert(p.trialIndex.forall(e => Set(0, 2, 3).contains(inputs.evRun(e))))
    assertEquals(p.drive.schedule.blockIds.distinct, Vector(0, 1, 2)) // local block indices
    // the baseline owns the intercept and the five nuisance columns of the three runs only
    assertEquals(p.baseline.designMatrix.cols, 3 * 6)
    assertEquals(p.baseline.designMatrix.rows, 300)
    assertEquals(bits(p.spec.plan.coefficients.head.phi.head), bits(prep.rho))
    assertEquals(bits(p.spec.phrfConfig.autocorrelation.rho.get), bits(prep.rho))
    assert(scalafim.phrfcmp.prep.WhiteningSpec.agreement(p.spec).isRight)
    assertEquals(p.spec.plan.nTimepoints, 300)
    // the all-runs problem uses the shared spec itself
    val all = problemOf(allRuns, 0.5)
    assert(all.spec eq prep.spec)

  test("the training plan whitens its rows exactly as the shared plan whitens them (per-run whitening)"):
    val p = problemOf(Vector(1, 2, 3), 0.5)
    val yRows = Matrix.of(inputs.y.rows, p.rows.length, Array.tabulate(inputs.y.rows * p.rows.length)(k => inputs.y(k / p.rows.length, p.rows(k % p.rows.length)))).fold(e => fail(e.message), identity)
    val local = Whiten.series(p.spec.plan, yRows).fold(e => fail(e.message), identity)
    var worst = 0.0
    var r = 0
    while r < p.rows.length do
      var v = 0
      while v < inputs.y.rows do
        worst = math.max(worst, math.abs(local(v, r) - prep.whitened.y(v, p.rows(r))))
        v += 1
      r += 1
    println(f"S5-WHITEN max |training-plan whitening - shared whitening| = $worst%.3e")
    assert(worst < 1e-12, s"worst $worst")

  test("the held-out design is the held-out run alone, on its own one-block frame and one-segment plan"):
    val h = PhrfTrialAssembly.heldOut(inputs, prep, 2, basis).fold(e => fail(e.message), identity)
    assertEquals(h.rows.length, 100)
    assertEquals(h.expanded.rows, 100)
    assertEquals(h.expanded.trials, 9)
    assert(h.trialIndex.forall(e => inputs.evRun(e) == 2))
    assertEquals(h.plan.nTimepoints, 100)
    assert(PhrfTrialAssembly.heldOut(inputs, prep, 9, basis).isLeft)

  // ---------------------------------------------------------------------------------------------- sigma2

  /** sigma2 by an independent route: raw design, plan built for the training runs, whitened refit, rss / (V (n - rank)). */
  private def directSigma2(trainRuns: Vector[Int]): Double =
    val x = FirPrefit.design(inputs, CellKind.Trial).fold(e => fail(e.message), identity)
    val rows = Array.tabulate(inputs.runId.length)(identity).filter(t => trainRuns.contains(inputs.runId(t)))
    val lengths = trainRuns.map(r => inputs.runId.count(_ == r))
    val starts = lengths.scanLeft(0)(_ + _)
    val segs = lengths.indices.toVector.map(i => TimeSegment(starts(i), starts(i + 1), i))
    val plan = WhiteningPlan.global(ArmaCoefficients.ar(prep.rho), segs, exactFirstAr1 = true)
    val v = inputs.y.rows
    val wx = WhiteningTransform.matrix(plan, DMat.tabulate(rows.length, x.cols)((r, c) => x(rows(r), c))).fold(e => fail(e.toString), identity)
    val wy = WhiteningTransform.matrix(plan, DMat.tabulate(rows.length, v)((r, j) => inputs.y(j, rows(r)))).fold(e => fail(e.toString), identity)
    val qr = wx.qr(QROptions(pivoting = QRPivoting.Column, rankTolerance = Some(1e-10)))
    val rank = qr.diagnostics.rank.getOrElse(wx.cols)
    val q = qr.q.slice(0, wx.rows, 0, rank)
    val res = wy - q * (q.t * wy)
    var rss = 0.0
    var r = 0
    while r < res.rows do
      var j = 0
      while j < res.cols do
        rss += res(r, j) * res(r, j)
        j += 1
      r += 1
    rss / (v.toDouble * (rows.length - rank))

  test("per-fold sigma2 equals a direct computation on the training runs, and is not the all-run value"):
    val folds = Lorocv.folds(inputs.runId).fold(e => fail(e.message), identity)
    var worstRel = 0.0
    for (f, rec) <- folds.zip(tuned.record.folds) do
      val direct = directSigma2(f.trainRuns)
      worstRel = math.max(worstRel, math.abs(rec.sigma2 - direct) / direct)
      assertEquals(rec.heldOutRun, f.heldOut)
      assertEquals(bits(rec.sigma2), bits(prep.sigma2Training(f.trainRuns.toSet).fold(e => fail(e.message), _.sigma2)))
      assert(rec.sigma2 != prep.sigma2All.sigma2)
    println(f"S5-SIGMA2 max relative |fold sigma2 - direct| = $worstRel%.3e")
    assert(worstRel < 1e-9, s"worst $worstRel")
    val allDirect = directSigma2(allRuns)
    assert(math.abs(prep.sigma2All.sigma2 - allDirect) / allDirect < 1e-9)

  // ---------------------------------------------------------------------------------------------- alpha 0

  test("alpha 0 (and any non-positive alpha) is refused before an ML plan exists; the executor itself would refuse it too"):
    val p = problemOf(allRuns, prep.sigma2All.sigma2)
    for a <- Seq(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      val r = PhrfTrialRunner.mlPlan(p, basis, a)
      assertEquals(r.left.map(_.code), Left("phrf_alpha_not_positive"), s"alpha $a")
      assertEquals(PhrfTrialRunner.trainFit(p, basis, a, config, PhrfClock.Wall).left.map(_.code), Left("phrf_alpha_not_positive"))
      assertEquals(PhrfCanProbe.fixedShapeFit(p, basis, PhrfCanonicalShape(Vector(5.0, 0.5), 0.0), a, config, PhrfClock.Wall, inputs.y).left.map(_.code), Left("phrf_alpha_not_positive"))
    // what the guard prevents: the library builds an alpha-0 "ML" plan and the executor then refuses it
    val raw = ProfileHrfPlan
      .fromTrialEvents(p.dataset, p.drive, p.baseline, p.spec.phrfConfig, basis, 0.0, ProfileCriterion.TrialRandomEffectsML(p.sigma2))
      .fold(e => fail(e.toString), identity)
    val refused = scalafim.fmri.fit.profile.ProfileHrfFit.prepare(
      raw,
      scalafim.dataset.DataSelection.All,
      scalafim.fmri.fit.CanonicalTemporalWhitening.Shared(p.spec.plan),
      PhrfTrialRunner.decodePolicy(config.phrf)
    )
    assert(refused.left.exists(_.isInstanceOf[ProfileFitError.Unsupported]))
    // the grid cannot hold alpha 0, and the tuning attempted exactly the grid's positive alphas in every fold
    assert(AlphaGrid.from(Vector(0.0, 0.5)).isLeft)
    assertEquals(tuned.record.alphasFitted, Vector.fill(4)(grid3.alphas).flatten)
    assert(tuned.record.alphasFitted.forall(_ > 0.0))
    assert(whole.fit.alpha > 0.0 && grid3.alphas.contains(whole.fit.alpha))

  // ---------------------------------------------------------------------------------------------- isolation

  /** The held-out run's response replaced by something wild; everything else, including rho, is untouched. */
  private def poisoned(run: Int): (FitInputs, CommonPrep) =
    val y = inputs.y
    val d = y.data.clone()
    var s = 0
    while s < y.cols do
      if inputs.runId(s) == run then
        var v = 0
        while v < y.rows do
          d(v * y.cols + s) += 20.0 + 7.0 * math.cos(0.3 * s) + v
          v += 1
      s += 1
    val ym = Matrix.of(y.rows, y.cols, d).fold(e => fail(e.message), identity)
    val in2 = PhrfTrialSynth.withY(inputs, ym)
    val wa = prep.whitened.copy(y = Whiten.series(prep.plan, ym).fold(e => fail(e.message), identity))
    val s2 = Sigma2.pooled(wa, inputs.runId, None).fold(e => fail(e.message), identity)
    (in2, prep.copy(whitened = wa, sigma2All = s2, inputFingerprint = InputFingerprint.of(in2)))

  test("a poisoned held-out run leaves the training fit unchanged and changes only the score"):
    val poisonRun = 1
    val (in2, prep2) = poisoned(poisonRun)
    assert(prep2.sigma2All.sigma2 != prep.sigma2All.sigma2) // the poison is visible to anything that reads the held-out run
    val native2 = TrialNativeInputs.from(in2, prep2).fold(e => fail(e.message), identity)
    val tuned2 = PhrfTrialTuning
      .tune(in2, prep2, native2, grid3, config, PhrfClock.Wall, PhrfTrialTuning.mlEngine(basis, config, PhrfClock.Wall))
      .fold(e => fail(e.message), identity)
    val a = tuned.record.folds(poisonRun)
    val b = tuned2.record.folds(poisonRun)
    assertEquals(a.heldOutRun, poisonRun)
    assertEquals(b.trainSha256, a.trainSha256, "training fit of the poisoned fold changed")
    assertEquals(bits(b.sigma2), bits(a.sigma2))
    assert(a.scores.zip(b.scores).forall((x, y) => x != y), s"scores ${a.scores} vs ${b.scores}")
    // every other fold trains on the poisoned run, so its training fit does change
    for f <- tuned.record.folds.indices if f != poisonRun do
      assert(tuned.record.folds(f).trainSha256.zip(tuned2.record.folds(f).trainSha256).forall((x, y) => x != y), s"fold $f")

  // ---------------------------------------------------------------------------------------------- determinism and receipts

  test("a rerun is hash-equal (outcome, tuning and final fit); timings are not part of the hash"):
    val again = PhrfTrialRunner.run(inputs, prep, grid3, config).fold(e => fail(e.message), identity)
    assertEquals(again.sha256, whole.sha256)
    assertEquals(again.tuning.sha256, whole.tuning.sha256)
    assertEquals(again.fit.sha256, whole.fit.sha256)
    assertEquals(again.tuning.folds.map(_.trainSha256), whole.tuning.folds.map(_.trainSha256))
    assertEquals(whole.sha256.length, 64)
    // the tuned record of a separate tune call agrees as well
    assertEquals(tuned.record.sha256, whole.tuning.sha256)
    // a different dataset hashes differently
    val other = PhrfTrialSynth.generate(spec.copy(seed = 3L))
    val otherPrep = CommonPreparation.prepare(other.inputs, CellKind.Trial).fold(e => fail(e.message), identity)
    val o = PhrfTrialRunner.run(other.inputs, otherPrep, grid3, config).fold(e => fail(e.message), identity)
    assert(o.sha256 != whole.sha256)

  test("work receipts: route, terminal ML evidence on every voxel, counted decoder work, public outputs accounted"):
    val v = inputs.y.rows
    val run = whole.fit.runReceipt
    val out = whole.fit.outputReceipt
    assertEquals(run.route, "trial-banded-ml")
    assertEquals(run.delivered, v)
    assertEquals(run.attempted, v)
    assertEquals(run.evidenceVoxels, v)
    assert(run.criterionForm, "provenance must carry criterion-form=J=E+sigma2*D")
    assertEquals(run.statuses.map(_._2).sum, v.toLong)
    assert(run.decoder.voxels == v.toLong && run.decoder.nodeScores > 0L && run.decoder.jets > 0L)
    assert(run.mlSetup.exists(_.referenceAttempts > 0L) && run.mlRun.exists(_.solveAttempts > 0L))
    assertEquals(out.publicAttempts + out.publicDecodeRefusals, v.toLong)
    assertEquals(out.publicSuccesses + out.publicFailures, out.publicAttempts)
    assert(whole.fit.voxels.forall(_.evidence))
    // every fold fit carries its own receipt, none of them a public-output run
    for f <- whole.tuning.folds; r <- f.receipts do
      assertEquals(r.route, "trial-banded-ml")
      assertEquals(r.delivered, v)
      assertEquals(r.evidenceVoxels, v)
      assertEquals(r.publicAttempts, 0L)
    assertEquals(whole.tuning.folds.flatMap(_.receipts).length, 4 * grid3.size)

  test("timings are recorded per stage and are not part of any hash"):
    val t = whole.timings
    assertEquals(t.foldPrepare.length, 4)
    assert(t.foldPrepare.forall(_.length == 3) && t.foldRun.forall(_.length == 3))
    assertEquals(t.alphaPreparationProfile.length, 3)
    assertEquals(t.trialMlSecondsPerVoxel.length, 12)
    assert(t.finalPrepare >= 0.0 && t.finalRun >= 0.0 && t.finalOutputs >= 0.0)

  // ---------------------------------------------------------------------------------------------- refusals

  test("typed refusals: inputs not prepared, wrong cell kind, status mapping, scorer adapter"):
    val exact = PhrfTrialSynth.generate(spec.copy(noiseSd = 0.0)).inputs
    assertEquals(PhrfTrialRunner.run(exact, prep, grid3, config).left.map(_.code), Left("phrf_prep_InputsDiffer"))
    val condIn = PrepSynth.inputs(PrepSynth.Spec(CellKind.Condition, seed = 3L))
    val condPrep = CommonPreparation.prepare(condIn, CellKind.Condition).fold(e => fail(e.message), identity)
    assertEquals(PhrfTrialRunner.run(inputs, condPrep, grid3, config).left.map(_.code), Left("phrf_wrong_kind"))
    assert(PhrfTrialRefusal.Dataset("prepare", "TrialMlPreparation").status == TrialArmStatus.Refused("phrf_dataset_prepare_TrialMlPreparation"))
    assert(PhrfTrialRefusal.Setup("frame", "SampleTimesDiffer").status == TrialArmStatus.Failed("phrf_setup_frame_SampleTimesDiffer"))
    assert(PhrfTrialRefusal.AlphaNotPositive(0.0).status.isInstanceOf[TrialArmStatus.Refused])
    val refused = PhrfTrialScoring.outcomes(Left(PhrfTrialRefusal.Dataset("run", "Backend")), 3)
    assertEquals(refused, Vector.fill(3)(VoxelOutcome.Refused))
    val failed = PhrfTrialScoring.outcomes(Left(PhrfTrialRefusal.Setup("frame", "SampleTimesDiffer")), 3)
    assertEquals(failed, Vector.fill(3)(VoxelOutcome.Failed))

  test("tamper: a one-ulp change to y, an onset, the nuisance or a sample time is refused by every entry point (exact input fingerprint)"):
    assert(prep.matches(inputs))
    val ulp = java.lang.Math.nextUp(inputs.y.data(7))
    val yData = inputs.y.data.clone()
    yData(7) = ulp
    val yT = PhrfTrialSynth.withY(inputs, Matrix.of(inputs.y.rows, inputs.y.cols, yData).fold(e => fail(e.message), identity))
    val onsets = inputs.evOnset.clone()
    onsets(3) = java.lang.Math.nextUp(onsets(3))
    val nuis = inputs.nuisance.data.clone()
    nuis(11) = java.lang.Math.nextUp(nuis(11))
    val times = inputs.sampleTime.clone()
    times(5) = java.lang.Math.nextUp(times(5))
    val tampered = Vector(
      "y" -> yT,
      "onset" -> PhrfTrialSynth.withOnset(inputs, onsets),
      "nuisance" -> PhrfTrialSynth.withNuisance(inputs, Matrix.of(inputs.nuisance.rows, inputs.nuisance.cols, nuis).fold(e => fail(e.message), identity)),
      "sample_time" -> PhrfTrialSynth.withSampleTime(inputs, times)
    )
    val native = TrialNativeInputs.from(inputs, prep).fold(e => fail(e.code), identity)
    for (what, t) <- tampered do
      assert(!prep.matches(t), what)
      assertEquals(PhrfTrialRunner.run(t, prep, grid3, config).left.map(_.code), Left("phrf_prep_InputsDiffer"), what)
      assertEquals(PhrfTrialTuning.tune(t, prep, native, grid3, config, PhrfClock.Wall, PhrfTrialTuning.mlEngine(basis, config, PhrfClock.Wall)).left.map(_.code), Left("phrf_prep_InputsDiffer"), what)
      assertEquals(PhrfPenaltyScale.derive(t, prep, config).left.map(_.code), Left("phrf_prep_InputsDiffer"), what)
      assertEquals(PhrfEdf.forRuns(t, prep, allRuns, grid3, config).left.map(_.code), Left("phrf_prep_InputsDiffer"), what)
      assertEquals(PhrfCanProbe.run(t, prep, grid3, config).left.map(_.code), Left("phrf_prep_InputsDiffer"), what)
      assertEquals(PhrfTrialRunner.rlssSettings(t, prep, grid3, config).left.map(_.code), Left("phrf_prep_InputsDiffer"), what)

  test("a rank-deficient baseline is a dataset-level typed PHRF refusal: every voxel Refused, no exception"):
    val dup = PhrfTrialSynth.generate(spec.copy(duplicateLinear = true))
    val dupPrep = CommonPreparation.prepare(dup.inputs, CellKind.Trial).fold(e => fail(e.message), identity)
    val r = PhrfTrialRunner.run(dup.inputs, dupPrep, grid3, config)
    println(s"S5-RANKDEF ${r.left.map(e => (e.code, e.message))}")
    assert(r.left.exists(_.isInstanceOf[PhrfTrialRefusal.Dataset]), r.toString)
    assert(r.left.exists(_.status.isInstanceOf[TrialArmStatus.Refused]))
    assertEquals(PhrfTrialScoring.outcomes(r, 3), Vector.fill(3)(VoxelOutcome.Refused))

  test("scorer adapter: Estimated voxels carry E-trial in input trial order, one value per trial"):
    val outs = PhrfTrialScoring.outcomes(Right(whole), inputs.y.rows)
    assertEquals(outs.length, 3)
    for (o, v) <- outs.zip(whole.fit.voxels) do
      v.status match
        case TrialArmStatus.Estimated =>
          val e = o.toOption.getOrElse(fail("estimated voxel lost"))
          assertEquals(e.toString, "TrialEstimate(<redacted>)")
          assertEquals(v.eTrial.get.length, inputs.evOnset.length)
        case _ => assert(o.isMissing)
