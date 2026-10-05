package scalafim.phrfcmp.run

import gale.linalg.DMat

import scalafim.fmri.hrf.{HrfFunctions, Lag}
import scalafim.phrfcmp.ingest.{CellKind, Matrix}
import scalafim.phrfcmp.prep.{CommonPreparation, NativeArm, PrepSynth, Whiten}

/** The trial-native arms and the tuned rLSS runner, on constructed whitened inputs and on a genuinely prepared dataset. */
class TrialNativeRunnerSuite extends munit.FunSuite:

  private def ok[A](r: Either[NativeTrialRefusal, A]): A = r.fold(e => fail(e.message), identity)
  private def dmat(m: Matrix): DMat = DMat.tabulate(m.rows, m.cols)((r, c) => m(r, c))

  private def relErr(a: Matrix, b: Matrix): Double =
    assertEquals((a.rows, a.cols), (b.rows, b.cols))
    var diff = 0.0
    var scale = 0.0
    var i = 0
    while i < a.data.length do
      diff = math.max(diff, math.abs(a.data(i) - b.data(i)))
      scale = math.max(scale, math.abs(b.data(i)))
      i += 1
    diff / scale

  /** Normal-equations oracle: solve (A'A) b = A'y by Cholesky, an algorithm independent of the QR paths under test. */
  private def normalSolve(a: DMat, y: DMat): DMat =
    val gram = a.t * a
    gram.cholesky.fold(e => fail(e.toString), identity).solve(a.t * y).fold(e => fail(e.toString), identity)

  private def toMatrix(d: DMat): Matrix =
    Matrix.of(d.rows, d.cols, Array.tabulate(d.rows * d.cols)(k => d(k / d.cols, k % d.cols))).fold(e => fail(e.message), identity)

  private val spec = TrialSynth.Spec(seed = 3L)
  private lazy val inputs = TrialSynth.inputs(spec)

  // ------------------------------------------------------------------------------------------------- LSA, LSS

  test("LSA equals the normal-equations solve on [trials, fixed] at 1e-10"):
    val in = inputs
    val a = DMat.tabulate(in.timepoints, in.trials + in.fixed.cols)((r, c) =>
      if c < in.trials then in.trialDesign(r, c) else in.fixed(r, c - in.trials))
    val full = normalSolve(a, dmat(in.y))
    val expected = toMatrix(DMat.tabulate(in.trials, in.voxels)((i, v) => full(i, v)))
    val got = ok(TrialNativeRunner.lsa(in))
    val e = relErr(got.amplitudes, expected)
    println(f"S4-LSA-NORMAL-EQUATIONS relative error $e%.3e")
    assert(e <= 1e-10, s"LSA: $e")
    assertEquals(got.arm, NativeArm.Lsa)
    assertEquals(got.ridge, None)
    assert(got.voxelStatus.forall(_ == TrialArmStatus.Estimated))

  test("LSS equals the explicit per-trial regression on [x_i, sum of the other trials, fixed] at 1e-10"):
    val in = inputs
    val out = Array.ofDim[Double](in.trials, in.voxels)
    var i = 0
    while i < in.trials do
      val a = DMat.tabulate(in.timepoints, 2 + in.fixed.cols): (r, c) =>
        if c == 0 then in.trialDesign(r, i)
        else if c == 1 then (0 until in.trials).filter(_ != i).map(j => in.trialDesign(r, j)).sum
        else in.fixed(r, c - 2)
      val b = normalSolve(a, dmat(in.y))
      var v = 0
      while v < in.voxels do
        out(i)(v) = b(0, v)
        v += 1
      i += 1
    val expected = toMatrix(DMat.tabulate(in.trials, in.voxels)((a, b) => out(a)(b)))
    val got = ok(TrialNativeRunner.lss(in))
    val e = relErr(got.amplitudes, expected)
    println(f"S4-LSS-EXPLICIT-LOOP relative error $e%.3e")
    assert(e <= 1e-10, s"LSS: $e")

  test("the LSS-equivalence flag (pooled rLSS at ridge 0) reproduces LSS at 1e-10"):
    val in = inputs
    val e = relErr(ok(TrialNativeRunner.rlssAsLss(in)).amplitudes, ok(TrialNativeRunner.lss(in)).amplitudes)
    println(f"S4-RLSS-AS-LSS relative error $e%.3e")
    assert(e <= 1e-10, s"pooled rLSS at ridge 0 vs LSS: $e")

  test("condition-centred rLSS at ridge 0 is not LSS, and shrinks toward the condition level as the ridge grows"):
    val in = inputs
    val lss = ok(TrialNativeRunner.lss(in)).amplitudes
    val r0 = ok(TrialNativeRunner.rlssCentred(in, 0.0)).amplitudes
    assert(relErr(r0, lss) > 1e-3)
    // spread of trial amplitudes within a condition decreases with the ridge
    def spread(m: Matrix): Double =
      (0 until 3).map { c =>
        val idx = (0 until in.trials).filter(in.trialCond(_) == c)
        (0 until in.voxels).map { v =>
          val mean = idx.map(m(_, v)).sum / idx.length
          idx.map(i => (m(i, v) - mean) * (m(i, v) - mean)).sum
        }.sum
      }.sum
    val spreads = Vector(0.0, 0.1, 1.0, 10.0, 1e3, 1e9).map(r => spread(ok(TrialNativeRunner.rlssCentred(in, r)).amplitudes))
    assert(spreads.zip(spreads.tail).forall((a, b) => a > b), s"within-condition spread $spreads")
    assert(spreads.last < 1e-6 * spreads.head, "an enormous ridge collapses every trial onto its condition level")

  test("native fits are deterministic: reruns have identical canonical bytes, and the digest depends on the arm and the ridge"):
    val in = inputs
    assertEquals(ok(TrialNativeRunner.lss(in)).sha256, ok(TrialNativeRunner.lss(in)).sha256)
    assertEquals(ok(TrialNativeRunner.lsa(in)).sha256, ok(TrialNativeRunner.lsa(in)).sha256)
    assertNotEquals(ok(TrialNativeRunner.lsa(in)).sha256, ok(TrialNativeRunner.lss(in)).sha256)
    assertNotEquals(ok(TrialNativeRunner.rlssCentred(in, 1.0)).sha256, ok(TrialNativeRunner.rlssCentred(in, 2.0)).sha256)

  test("typed failures and refusals: rank deficiency is Failed, a non-trial arm is Refused, inputs are validated"):
    val in = inputs
    val dup = Matrix
      .of(in.timepoints, in.trials, Array.tabulate(in.timepoints * in.trials)(k => in.trialDesign(k / in.trials, if k % in.trials == 1 then 0 else k % in.trials)))
      .fold(e => fail(e.message), identity)
    val broken = TrialNativeInputs
      .of(in.y, dup, in.fixed, in.runId, in.trialRun, in.trialCond, in.trialStim, in.trialIds)
      .fold(e => fail(e.message), identity)
    val lsa = TrialNativeRunner.lsa(broken)
    assert(lsa.isLeft)
    lsa.left.foreach { e =>
      assert(e.code.startsWith("fit_"), e.code)
      assert(e.status.isInstanceOf[TrialArmStatus.Failed])
      assert(e.code.length <= 64 && e.code.forall(c => c.isLetterOrDigit || c == '_'))
    }
    val settings = pilotSettings(in)
    NativeArm.values.filterNot(a => a == NativeArm.Lsa || a == NativeArm.Lss || a == NativeArm.Rlss).foreach { arm =>
      val r = TrialNativeRunner.run(arm, in, settings)
      assertEquals(r.left.map(_.status), Left(TrialArmStatus.Refused("native_wrong_kind")))
    }
    val m = Matrix.of(2, 1, Array(1.0, 2.0)).fold(e => fail(e.message), identity)
    assert(TrialNativeInputs.of(in.y, m, in.fixed, in.runId, in.trialRun, in.trialCond, in.trialStim, in.trialIds).isLeft)
    val nan = Matrix.of(1, 1, Array(Double.NaN)).fold(e => fail(e.message), identity)
    assert(TrialNativeInputs.of(nan, Matrix.of(1, 1, Array(1.0)).fold(e => fail(e.message), identity), nan, Array(0), Array(0), Array(0), Array(0), Array(0)).isLeft)

  // ------------------------------------------------------------------------------------------------- LOROCV on rLSS

  private def pilotSettings(in: TrialNativeInputs): RlssSettings =
    RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity, Some(JointEdf.targets(in, AlphaGrid.Pilot)))

  private def tune(in: TrialNativeInputs) =
    TrialNativeRunner.tuneRlss(in, AlphaGrid.Pilot, JointEdf.targets(in, AlphaGrid.Pilot), PenaltyScale.Identity)

  test("tuned rLSS: one fold per run, each leaving exactly that run out, nine scores per fold, a selected ridge from the final map"):
    val in = inputs
    val t = ok(tune(in))
    assertEquals(t.tuning.foldMaps.length, spec.runs)
    assertEquals(t.tuning.selection.foldScores.length, spec.runs)
    assert(t.tuning.selection.foldScores.forall(_.length == 9))
    assertEquals(t.fit.ridge, Some(t.tuning.finalMap.ridges(t.tuning.selection.selected)))
    assertEquals(t.tuning.selectedRidge, t.fit.ridge.get)
    assertEquals((t.fit.amplitudes.rows, t.fit.amplitudes.cols), (in.trials, in.voxels))
    assert(t.fit.amplitudes.data.forall(_.isFinite))
    val folds = Lorocv.folds(in.runId).fold(e => fail(e.message), identity)
    folds.foreach { f =>
      val train = ok(in.restrict(f.trainRows, in.trialRun.indices.filter(i => f.trainRuns.contains(in.trialRun(i))).toArray))
      assert(train.runId.forall(_ != f.heldOut))
      assert(train.trialRun.forall(_ != f.heldOut))
      assertEquals(train.trials, in.trials - in.trialRun.count(_ == f.heldOut))
      assertEquals(train.fixed.cols, in.fixed.cols - 3, "the held-out run's three nuisance columns have no training support")
    }

  test("held-out data never enter fitting, q or the df map: poisoning the held-out run changes only that fold's score"):
    val in = inputs
    val base = ok(tune(in))
    val held = 2
    val poisonedY = Matrix
      .of(in.y.rows, in.y.cols, Array.tabulate(in.y.rows * in.y.cols)(k => if in.runId(k / in.y.cols) == held then 1e6 * math.sin(k.toDouble) else in.y.data(k)))
      .fold(e => fail(e.message), identity)
    val poisonedX = Matrix
      .of(in.timepoints, in.trials, Array.tabulate(in.timepoints * in.trials)(k => if in.runId(k / in.trials) == held then 77.0 * math.cos(k.toDouble) else in.trialDesign.data(k)))
      .fold(e => fail(e.message), identity)
    val poisoned = TrialNativeInputs
      .of(poisonedY, poisonedX, in.fixed, in.runId, in.trialRun, in.trialCond, in.trialStim, in.trialIds)
      .fold(e => fail(e.message), identity)
    val after = ok(tune(poisoned))
    // fold index == held-out run here (runs 0 .. 3, ascending): its df map is built from the other three runs only
    assertEquals(after.tuning.foldMaps(held), base.tuning.foldMaps(held))
    assertNotEquals(after.tuning.foldMaps((held + 1) % spec.runs), base.tuning.foldMaps((held + 1) % spec.runs))
    assertNotEquals(after.tuning.selection.foldScores(held), base.tuning.selection.foldScores(held), "the held-out score does read the held-out data")

  test("tuning is deterministic: two runs give bit-identical selections, ridges and final amplitudes"):
    val in = inputs
    val a = ok(tune(in))
    val b = ok(tune(in))
    assertEquals(a.fit.sha256, b.fit.sha256)
    assertEquals(a.tuning.selection.foldScores, b.tuning.selection.foldScores)
    assertEquals(a.tuning.selection.selected, b.tuning.selection.selected)

  test("endpoint behaviour follows the truth: no deviations select strong shrinkage, strong persistent deviations select weak shrinkage"):
    // alpha is the deviation variance ratio, so a small alpha is a strong penalty. The scale is chosen so that the grid
    // brackets the information q (penalty 64 .. 0.25 against q of order 1).
    val none = (1 to 4).map(s => ok(tune(TrialSynth.inputs(TrialSynth.Spec(devSd = 0.0, noiseSd = 1.0, seed = s.toLong)))).tuning.selection.selected)
    val strong = (1 to 4).map(s => ok(tune(TrialSynth.inputs(TrialSynth.Spec(devSd = 3.0, noiseSd = 0.2, seed = s.toLong)))).tuning.selection.selected)
    println(s"S4-ENDPOINTS no-deviation selected indices $none; strong-deviation selected indices $strong")
    assert(none.forall(_ <= 2), s"no deviations should be shrunk hard: $none")
    assert(strong.forall(_ >= 6), s"strong persistent deviations should be left almost unshrunk: $strong")

  test("run(Rlss) returns the tuning record; LSA and LSS return none"):
    val in = inputs
    val s = pilotSettings(in)
    assert(ok(TrialNativeRunner.run(NativeArm.Rlss, in, s)).tuning.isDefined)
    assert(ok(TrialNativeRunner.run(NativeArm.Lsa, in, s)).tuning.isEmpty)
    assert(ok(TrialNativeRunner.run(NativeArm.Lss, in, s)).tuning.isEmpty)

  // ------------------------------------------------------------------------------------------------- real preparation

  private val trialPrep = PrepSynth.Spec(CellKind.Trial, seed = 5L)

  test("trial regressors read from the prepared dataset are the canonical columns on the generator's sample times, whitened"):
    val fit = PrepSynth.inputs(trialPrep)
    val prep = CommonPreparation.prepare(fit, CellKind.Trial).fold(e => fail(e.message), identity)
    val in = ok(TrialNativeInputs.from(fit, prep))
    // the generator's sample times: k * TR from 0 in each run (TR = 1), never (k + 0.5) * TR
    assertEquals(fit.sampleTime.toVector, Vector.tabulate(fit.sampleTime.length)(s => (s % trialPrep.runLen).toDouble))
    val peak = 0.17544119701700001
    val raw = Array.tabulate(fit.sampleTime.length * fit.evOnset.length) { k =>
      val s = k / fit.evOnset.length
      val e = k % fit.evOnset.length
      val lag = fit.sampleTime(s) - fit.evOnset(e)
      if fit.runId(s) == fit.evRun(e) && lag > 0.0 && lag < 32.0 then HrfFunctions.spmg1(Lag(lag)) / peak else 0.0
    }
    val rawM = Matrix.of(fit.sampleTime.length, fit.evOnset.length, raw).fold(e => fail(e.message), identity)
    val expected = Whiten.columns(prep.plan, rawM).fold(e => fail(e.message), identity)
    assertEquals((in.trialDesign.rows, in.trialDesign.cols), (expected.rows, expected.cols))
    assertEquals(in.trialDesign.data.toVector, expected.data.toVector)
    // trials and fixed nuisance come from the shared whitened arrays, never a private copy
    assertEquals(in.fixed.cols, prep.whitened.nuisance.cols + prep.whitened.runIntercepts.cols)
    assertEquals(in.trialIds.toVector, Vector.tabulate(fit.evOnset.length)(identity))
    assertEquals(in.y.rows, fit.y.cols)
    assertEquals(in.y(7, 2), prep.whitened.y(2, 7))

  test("native trial arms run end to end on a prepared dataset and are deterministic"):
    val fit = PrepSynth.inputs(trialPrep)
    val prep = CommonPreparation.prepare(fit, CellKind.Trial).fold(e => fail(e.message), identity)
    val settings = RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity, Some(JointEdf.targets(TrialNativeInputs.from(fit, prep).fold(e => fail(e.message), identity), AlphaGrid.Pilot)))
    Vector(NativeArm.Lsa, NativeArm.Lss, NativeArm.Rlss).foreach { arm =>
      val a = ok(TrialNativeRunner.run(arm, fit, prep, settings))
      val b = ok(TrialNativeRunner.run(arm, fit, prep, settings))
      assertEquals(a.fit.sha256, b.fit.sha256, arm.toString)
      assertEquals((a.fit.amplitudes.rows, a.fit.amplitudes.cols), (fit.evOnset.length, fit.y.rows))
    }

  test("a condition cell is not a trial cell"):
    val fit = PrepSynth.inputs(PrepSynth.Spec(CellKind.Condition, seed = 3L))
    val prep = CommonPreparation.prepare(fit, CellKind.Condition).fold(e => fail(e.message), identity)
    assert(TrialNativeInputs.from(fit, prep).isLeft)

  test("scorer adapter: one outcome per voxel in input trial order, accepted by TrialDataset; refusals map to Refused/Failed"):
    import scalafim.phrfcmp.score.{Method, TrialDataset, TrialTruth, VoxelOutcome}
    val in = inputs
    val fit = ok(TrialNativeRunner.lss(in))
    val outs = TrialNativeScoring.outcomes(fit)
    assertEquals(outs.length, in.voxels)
    assertEquals(TrialNativeScoring.method(NativeArm.Rlss), Some(Method.Rlss))
    assertEquals(TrialNativeScoring.method(NativeArm.PhrfInput), None)
    val e = outs(1) match
      case VoxelOutcome.Estimated(x) => x
      case other                     => fail(s"expected an estimate, got $other")
    val truthAmp = Vector.tabulate(in.voxels)(v => Vector.tabulate(in.trials)(i => fit.amplitudes(i, v)))
    val truth = TrialTruth.of(in.trialCond.toVector, truthAmp).fold(x => fail(x.toString), identity)
    val ds = TrialDataset.of(0, truth, Map(Method.Lss -> outs)).fold(x => fail(x.toString), identity)
    assert(ds != null && e != null)
    val bad = fit.copy(amplitudes = Matrix.of(fit.amplitudes.rows, fit.amplitudes.cols, fit.amplitudes.data.updated(0, Double.NaN)).fold(x => fail(x.message), identity),
      voxelStatus = fit.voxelStatus.updated(0, TrialArmStatus.Failed("nonfinite_beta")))
    assertEquals(TrialNativeScoring.outcomes(bad).head, VoxelOutcome.Failed)
    val refused = TrialNativeScoring.outcomes(Left(NativeTrialRefusal.WrongKind(NativeArm.Can)), 3)
    assertEquals(refused, Vector.fill(3)(VoxelOutcome.Refused))
    val failed = TrialNativeScoring.outcomes(Left(NativeTrialRefusal.Fit(scalafim.fmri.fit.FitError.EmptyDesign)), 3)
    assertEquals(failed, Vector.fill(3)(VoxelOutcome.Failed))

  // ------------------------------------------------------------------------------------------------- edf matching

  test("fold and final maps match PHRF's joint edf on the rLSS amplitude edf; the ridges are not the penalties"):
    val in = inputs
    val t = ok(tune(in))
    val grid = AlphaGrid.Pilot
    val folds = Lorocv.folds(in.runId).fold(e => fail(e.message), identity)
    folds.zip(t.tuning.foldMaps).foreach { (f, m) =>
      val runs = f.trainRuns.toSet
      val train = ok(in.restrict(f.trainRows, in.trialRun.indices.filter(i => runs.contains(in.trialRun(i))).toArray))
      assertEquals(m.targetEdf, JointEdf.at(train, grid.lambdas))
      val p = ok(TrialNativeRunner.prepareRlss(train, TrialNativeRunner.conditionGrouping(train)))
      m.ridges.zip(m.targetEdf).foreach((r, target) => assertEqualsDouble(p.amplitudeEdf(r), target, 1e-9))
      assert(!m.flagged)
      assertEqualsDouble(p.amplitudeEdf(0.0), train.trials.toDouble, 1e-9, "edf is N with no penalty")
      assertEqualsDouble(p.amplitudeEdf(1e15 * p.q.max), 3.0, 1e-6, "edf is G (conditions) at an enormous penalty")
    }
    assertEquals(t.tuning.finalMap.targetEdf, JointEdf.at(in, grid.lambdas))
    assert(t.tuning.finalMap.ridges.zip(grid.lambdas).exists((r, l) => math.abs(r - l) > 0.05 * l), "matching on true edf is not the identity on lambda")
    assert(t.tuning.finalMap.ridges.zip(t.tuning.finalMap.ridges.tail).forall((a, b) => a > b), "ridge decreases with alpha")

  test("without PHRF edf targets the rLSS arm refuses with a typed reason"):
    val in = inputs
    val r = TrialNativeRunner.run(NativeArm.Rlss, in, RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity))
    assertEquals(r.left.map(_.status), Left(TrialArmStatus.Refused("native_no_edf_targets")))
    val bad = TrialNativeRunner.run(NativeArm.Rlss, in, RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity, Some(EdfTargets(_ => Left("no design")))))
    assertEquals(bad.left.map(_.code), Left("native_edf_targets"))

  test("the penalty-scale diagnostic is recomputed on each training fold's n_c and never changes the mapping"):
    val in = inputs
    val scale = PenaltyScale(2.0, 0.5, 0.0, 0.0)
    val t = ok(TrialNativeRunner.tuneRlss(in, AlphaGrid.Pilot, JointEdf.targets(in, AlphaGrid.Pilot), scale))
    val base = ok(tune(in))
    assertEquals(t.tuning.foldMaps.map(_.ridges), base.tuning.foldMaps.map(_.ridges))
    t.tuning.foldMaps.foreach { m =>
      // training folds hold 3 runs x 4 stimuli = 12 trials per condition: centring 1 - 1/12
      assertEqualsDouble(m.scaleDiagnosticRidges.get.head, 2.0 * (1.0 - 1.0 / 12.0) * AlphaGrid.Pilot.lambda(0), 1e-9)
    }
    // the final design holds 4 runs x 4 stimuli = 16 per condition
    assertEqualsDouble(t.tuning.finalMap.scaleDiagnosticRidges.get.head, 2.0 * (1.0 - 1.0 / 16.0) * AlphaGrid.Pilot.lambda(0), 1e-9)

  test("the fold score is unchanged by a baseline-heavy response: the held-out nuisance projection is kept at runner level"):
    val in = inputs
    val base = ok(tune(in))
    val shifted = Matrix
      .of(in.y.rows, in.y.cols, Array.tabulate(in.y.rows * in.y.cols) { k =>
        val r = k / in.y.cols
        val v = k % in.y.cols
        in.y.data(k) + (0 until in.fixed.cols).map(j => in.fixed(r, j) * 800.0 * math.sin(1.0 + j + 3.0 * v)).sum
      })
      .fold(e => fail(e.message), identity)
    val heavy = TrialNativeInputs
      .of(shifted, in.trialDesign, in.fixed, in.runId, in.trialRun, in.trialCond, in.trialStim, in.trialIds)
      .fold(e => fail(e.message), identity)
    val after = ok(TrialNativeRunner.tuneRlss(heavy, AlphaGrid.Pilot, JointEdf.targets(in, AlphaGrid.Pilot), PenaltyScale.Identity))
    base.tuning.selection.foldScores.zip(after.tuning.selection.foldScores).foreach { (a, b) =>
      a.zip(b).foreach((x, y) => assertEqualsDouble(x, y, 1e-6))
    }
    assertEquals(after.tuning.selection.selected, base.tuning.selection.selected)

  test("the shared held-out signal is regressors times amplitudes, with a typed refusal on mismatched trials"):
    val x = Matrix.of(2, 3, Array(1.0, 0.0, 2.0, 0.0, 3.0, 0.5)).fold(e => fail(e.message), identity)
    val a = Matrix.of(3, 2, Array(1.0, 2.0, 3.0, 4.0, 5.0, 6.0)).fold(e => fail(e.message), identity)
    val sig = FoldScore.predictedSignal(x, a).fold(e => fail(e.message), identity)
    assertEquals(sig.data.toVector, Vector(11.0, 14.0, 11.5, 15.0))
    assert(FoldScore.predictedSignal(x, Matrix.of(2, 2, Array(1.0, 2.0, 3.0, 4.0)).fold(e => fail(e.message), identity)).isLeft)

  test("tamper: inputs that differ from the prepared dataset (0.1 s onset shift) are refused with a typed reason"):
    val fit = PrepSynth.inputs(trialPrep)
    val prep = CommonPreparation.prepare(fit, CellKind.Trial).fold(e => fail(e.message), identity)
    val settings = RlssSettings(AlphaGrid.Pilot, PenaltyScale.Identity)
    assert(TrialNativeRunner.run(NativeArm.Lss, fit, prep, settings).isRight)
    val shifted = PrepSynth.inputs(trialPrep.copy(onsetShift = 0.1))
    val r = TrialNativeRunner.run(NativeArm.Lss, shifted, prep, settings)
    assertEquals(r.left.map(_.code), Left("native_inputs_differ"))
    assertEquals(r.left.map(_.status), Left(TrialArmStatus.Refused("native_inputs_differ")))
