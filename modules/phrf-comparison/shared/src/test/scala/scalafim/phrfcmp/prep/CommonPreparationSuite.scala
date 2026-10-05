package scalafim.phrfcmp.prep

import scalafim.fmri.ar.{CorrectionFallback, CorrectionSkip, InitialConditionPolicy, RunCorrection}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig}
import scalafim.phrfcmp.ingest.{CellKind, Matrix}

class CommonPreparationSuite extends munit.FunSuite:

  private val condSpec = PrepSynth.Spec(CellKind.Condition, seed = 3L)
  private val trialSpec = PrepSynth.Spec(CellKind.Trial, seed = 5L)

  private def ok[A](r: Either[PrepRefusal, A]): A = r.fold(e => fail(e.message), identity)
  private def bits(d: Double): Long = java.lang.Double.doubleToLongBits(d)
  private def maxAbs(a: Matrix, b: Matrix): Double =
    assertEquals((a.rows, a.cols), (b.rows, b.cols))
    a.data.indices.map(i => math.abs(a.data(i) - b.data(i))).max

  // ---- 1. intercept handling ------------------------------------------------------------------------------------
  test("generator intercepts are dropped, one per run; intercepts plus remainder span the generator nuisance"):
    val in = PrepSynth.inputs(trialSpec)
    val split = ok(Nuisance.dropIntercepts(in.nuisance, in.runId))
    assertEquals(split.droppedColumns, Vector(0, 6, 12))
    assertEquals((split.dropped.rows, split.dropped.cols), (300, 15))
    assertEquals((split.runIntercepts.rows, split.runIntercepts.cols), (300, 3))
    val (pGen, rGen) = Linear.projector(Linear.toDMat(in.nuisance))
    val full = Matrix.of(300, 18, Array.tabulate(300 * 18)(k =>
      val (t, j) = (k / 18, k % 18)
      if j < 3 then split.runIntercepts(t, j) else split.dropped(t, j - 3))).fold(e => fail(e.message), identity)
    val (pNew, rNew) = Linear.projector(Linear.toDMat(full))
    assertEquals((rGen, rNew), (18, 18))
    var worst = 0.0
    for r <- 0 until 300; c <- 0 until 300 do worst = math.max(worst, math.abs(pGen(r, c) - pNew(r, c)))
    assert(worst < 1e-10, s"projector difference $worst")

  test("the rank trap: generator nuisance plus Constant intercepts is rank deficient, the dropped form is full rank"):
    val in = PrepSynth.inputs(trialSpec)
    val split = ok(Nuisance.dropIntercepts(in.nuisance, in.runId))
    val doubled = Matrix.of(300, 21, Array.tabulate(300 * 21)(k =>
      val (t, j) = (k / 21, k % 21)
      if j < 3 then split.runIntercepts(t, j) else in.nuisance(t, j - 3))).fold(e => fail(e.message), identity)
    assertEquals(Linear.projector(Linear.toDMat(doubled))._2, 18)
    val ok18 = Matrix.of(300, 18, Array.tabulate(300 * 18)(k =>
      val (t, j) = (k / 18, k % 18)
      if j < 3 then split.runIntercepts(t, j) else split.dropped(t, j - 3))).fold(e => fail(e.message), identity)
    assertEquals(Linear.projector(Linear.toDMat(ok18))._2, 18)

  test("a nuisance without exactly one intercept per run is refused"):
    val in = PrepSynth.inputs(condSpec)
    val noInt = Matrix.of(300, 2, Array.tabulate(600)(k => if k % 2 == 0 then math.sin(k.toDouble) else k.toDouble)).fold(e => fail(e.message), identity)
    assertEquals(Nuisance.dropIntercepts(noInt, in.runId), Left(PrepRefusal.NuisanceIntercepts(0, 0)))
    val twice = Matrix.of(300, 2, Array.tabulate(600)(k => if k / 2 < 100 then 1.0 else 0.0)).fold(e => fail(e.message), identity)
    assertEquals(Nuisance.dropIntercepts(twice, in.runId), Left(PrepRefusal.NuisanceIntercepts(0, 2)))

  // ---- 2. specified pre-fit --------------------------------------------------------------------------------------
  test("pre-fit designs: condition FIR, trial LSA plus FIR"):
    val c = ok(FirPrefit.design(PrepSynth.inputs(condSpec), CellKind.Condition))
    assertEquals((c.rows, c.cols), (300, 3 * 32 + 18))
    val t = ok(FirPrefit.design(PrepSynth.inputs(trialSpec), CellKind.Trial))
    assertEquals((t.rows, t.cols), (300, 24 + 3 * 32 + 18))

  test("pre-fit refuses a cell kind that contradicts the inputs"):
    assert(FirPrefit.design(PrepSynth.inputs(condSpec), CellKind.Trial).isLeft)
    assert(FirPrefit.design(PrepSynth.inputs(trialSpec), CellKind.Condition).isLeft)

  test("pre-fit rho is finite, in range, deterministic; the AR design is selected by cell kind"):
    for spec <- Seq(condSpec, trialSpec) do
      val in = PrepSynth.inputs(spec)
      val (a, sigmaDesign) = ok(FirPrefit.prefit(in, spec.kind))
      val (b, _) = ok(FirPrefit.prefit(in, spec.kind))
      val cond = ok(FirPrefit.conditionDesign(in, spec.kind))
      val trial = ok(FirPrefit.design(in, spec.kind))
      assertEquals(bits(a.rho), bits(b.rho))
      assert(math.abs(a.rho - 0.3) < 0.15, s"rho ${a.rho}")
      assertEquals(a.provenance.sha256, b.provenance.sha256)
      assertEquals(a.provenance.level, ArDesignLevel.forKind(spec.kind))
      assertEquals(a.designCols, sigmaDesign.cols)
      spec.kind match
        case CellKind.Condition =>
          assertEquals(a.provenance.level, ArDesignLevel.ConditionLevel)
          assertEquals(a.designCols, cond.cols)
        case CellKind.Trial =>
          assertEquals(a.provenance.level, ArDesignLevel.TrialLevel)
          assertEquals(a.designCols, trial.cols)
          assert(trial.cols > cond.cols)

  test("AR fit provenance records policy, budget, per-run conditioning, phi and gamma; every run is Applied"):
    for spec <- Seq(condSpec, trialSpec) do
      val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(spec), spec.kind))
      val p = prep.arFit
      assertEquals(p.budget, FirPrefit.Budget)
      assertEquals(bits(p.phi), bits(prep.rho))
      assertEquals(p.corrections.length, prep.segments.length)
      assert(p.corrections.forall { case RunCorrection.Applied(_) => true; case _ => false }, p.corrections.toString)
      assert(p.lagUsed >= 1 && p.lagUsed <= p.requestedLag)
      assert(p.gamma.length > 1 && p.gamma.head > 0.0)
      assert(p.canonical.contains("phrf-cmp-s2-ar-fit-v4"))
      assertEquals(p.policy, ArFitPolicy.DesignCorrected)
      assert(!p.pooledClamped && p.clampedRuns.isEmpty)
      assertEquals(prep.prefit.nSamples - p.residualDf, prep.prefit.rank)

  test("a pooled phi at the stationarity clamp is a refusal; a rank mismatch between the QR and the AR module is a refusal"):
    assertEquals(FirPrefit.refuseClamped(0.3), Right(()))
    assertEquals(FirPrefit.refuseClamped(0.99), Left(PrepRefusal.StationarityClamp(0.99, 0.99)))
    assertEquals(FirPrefit.refuseClamped(-0.99), Left(PrepRefusal.StationarityClamp(-0.99, 0.99)))
    assertEquals(FirPrefit.checkRank(600, 261, 339), Right(()))
    assertEquals(FirPrefit.checkRank(600, 261, 340), Left(PrepRefusal.RankMismatch(261, 260)))

  test("an ill-conditioned run is a hard refusal, never a silent fall back to raw"):
    assertEquals(FirPrefit.refuseNotApplied(Vector(RunCorrection.Applied(0.1), RunCorrection.Applied(0.2))), Right(()))
    assertEquals(
      FirPrefit.refuseNotApplied(Vector(RunCorrection.Applied(0.1), RunCorrection.IllConditioned(1e-14))),
      Left(PrepRefusal.ArIllConditioned(1, 1e-14))
    )
    assert(PrepRefusal.ArIllConditioned(1, 1e-14).message.contains("refuses"))
    val ok = RunCorrection.Applied(0.1)
    assertEquals(
      FirPrefit.refuseNotApplied(Vector(ok, RunCorrection.SolveFallback(CorrectionFallback.SingularSystem))),
      Left(PrepRefusal.ArSolveFallback(1, CorrectionFallback.SingularSystem))
    )
    assertEquals(
      FirPrefit.refuseNotApplied(Vector(RunCorrection.NotAttempted(CorrectionSkip.NoLagZeroPairs), ok)),
      Left(PrepRefusal.ArNotAttempted(0, CorrectionSkip.NoLagZeroPairs))
    )
    assertEquals(FirPrefit.refuseNotApplied(Vector(ok, RunCorrection.Uncorrected)), Left(PrepRefusal.ArNotCorrected(1)))

  test("the prepared path is bit-identical to the unprepared path on synthetic data"):
    for spec <- Seq(condSpec, trialSpec) do
      val in = PrepSynth.inputs(spec)
      val x = ok(FirPrefit.arDesign(in, spec.kind))
      val level = ArDesignLevel.forKind(spec.kind)
      val a = ok(FirPrefit.fit(in, x, level))
      val b = ok(FirPrefit.fitUnprepared(in, x, level))
      assertEquals(a.rho.toString, b.rho.toString)
      assertEquals(a.provenance.canonical, b.provenance.canonical)
      assertEquals(a.heterogeneity.canonical, b.heterogeneity.canonical)

  // ---- 3. plan and FitConfig -------------------------------------------------------------------------------------
  test("plan phi equals the FitConfig phi bit for bit; segments and initial condition agree"):
    for spec <- Seq(condSpec, trialSpec) do
      val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(spec), spec.kind))
      val cfg = prep.phrfConfig.autocorrelation
      assertEquals(prep.plan.coefficients.length, 1)
      assertEquals(bits(prep.plan.coefficients.head.phi.head), bits(prep.rho))
      assertEquals(cfg.rho.map(bits), Some(bits(prep.rho)))
      assertEquals(cfg.structure, ArStructure.Ar(1))
      assert(cfg.global)
      assert(prep.plan.exactFirstAr1 && cfg.exactFirst)
      assertEquals(prep.plan.initialCondition, InitialConditionPolicy.ExactAr1)
      assertEquals(prep.plan.segments, prep.segments)
      assertEquals(WhiteningSpec.agreement(prep.spec), Right(()))

  test("a one-ulp phi difference, a different initial condition or a non-global config is refused"):
    val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(condSpec), CondKind))
    val s = prep.spec
    def withCfg(c: ArOptions) = s.copy(phrfConfig = FitConfig(autocorrelation = c))
    val nudged = math.nextUp(s.rho)
    assert(WhiteningSpec.agreement(withCfg(ArOptions(ArStructure.Ar(1), global = true, rho = Some(nudged)))).isLeft)
    assert(WhiteningSpec.agreement(withCfg(ArOptions(ArStructure.Ar(1), global = true, exactFirst = false, rho = Some(s.rho)))).isLeft)
    assert(WhiteningSpec.agreement(withCfg(ArOptions(ArStructure.Ar(1), global = false, rho = Some(s.rho)))).isLeft)

  private val CondKind = CellKind.Condition

  // ---- 4. whitened arrays ----------------------------------------------------------------------------------------
  test("whitened Y is the exact-first AR(1) filter applied per run"):
    val in = PrepSynth.inputs(condSpec)
    val prep = ok(CommonPreparation.prepare(in, CellKind.Condition))
    val phi = prep.rho
    var worst = 0.0
    for v <- 0 until in.y.rows; t <- 0 until in.y.cols do
      val first = t == 0 || in.runId(t) != in.runId(t - 1)
      val expect = if first then math.sqrt(1.0 - phi * phi) * in.y(v, t) else in.y(v, t) - phi * in.y(v, t - 1)
      worst = math.max(worst, math.abs(expect - prep.whitened.y(v, t)))
    assert(worst < 1e-12, s"worst $worst")

  test("whitened arrays are hash-identical across native arms and built from one source"):
    for spec <- Seq(condSpec, trialSpec) do
      val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(spec), spec.kind))
      val arms = prep.armInputs
      assertEquals(arms.map(_.arm), NativeArm.forKind(spec.kind))
      assert(arms.forall(_.arrays eq prep.whitened))
      assertEquals(prep.armHashes.values.toSet.size, 1)
      assertEquals(prep.armHashes.keySet, NativeArm.forKind(spec.kind).toSet)
      // rebuilt from the same FitInputs: same hash; a different rho or different data: different hash
      val again = ok(CommonPreparation.prepare(PrepSynth.inputs(spec), spec.kind))
      assertEquals(again.whitened.sha256, prep.whitened.sha256)
      assertEquals(bits(again.rho), bits(prep.rho))
      val other = ok(CommonPreparation.prepare(PrepSynth.inputs(spec.copy(seed = spec.seed + 1)), spec.kind))
      assertNotEquals(other.whitened.sha256, prep.whitened.sha256)

  test("PHRF input and native arms agree on the whitened response (the plan applied to raw Y reproduces it)"):
    val in = PrepSynth.inputs(trialSpec)
    val prep = ok(CommonPreparation.prepare(in, CellKind.Trial))
    val viaPlan = ok(Whiten.series(prep.plan, in.y))
    assertEquals(maxAbs(viaPlan, prep.armInput(NativeArm.PhrfInput).arrays.y), 0.0)
    assertEquals(maxAbs(viaPlan, prep.armInput(NativeArm.Lsa).arrays.y), 0.0)

  test("an arm design is whitened with the same plan"):
    val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(condSpec), CellKind.Condition))
    val w = ok(prep.whitenDesign(prep.split.dropped))
    assertEquals(maxAbs(w, prep.whitened.nuisance), 0.0)

  // ---- 5. AR heterogeneity record --------------------------------------------------------------------------------
  test("heterogeneity is a record: per-run rho for every run, never gating, canonical form deterministic"):
    val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(trialSpec), CellKind.Trial))
    val h = prep.heterogeneity
    assertEquals(h.perRunRho.length, 3)
    assertEquals(h.voxelRho.length, 8)
    assert(!h.gates)
    assert(h.voxelRhoSd >= 0.0 && h.voxelRhoSd.isFinite)
    assert(h.perRunRho.forall(r => r.isFinite && math.abs(r) < 1.0))
    assertEquals(h.perRunStatus.length, 3)
    assertEquals(h.voxelCorrected.length, 8)
    assert(h.perRunStatus.forall { case RunCorrection.Applied(_) => true; case _ => false })
    assert(h.canonical.contains("per_run_status=applied:"))
    val notApplied = h.copy(perRunStatus = Vector(RunCorrection.IllConditioned(1e-15)))
    assert(notApplied.canonical.contains("per_run_status=ill-conditioned:"))
    val again = ok(CommonPreparation.prepare(PrepSynth.inputs(trialSpec), CellKind.Trial)).heterogeneity
    assertEquals(h.canonical, again.canonical)
    assertEquals(h.sha256, again.sha256)
    assert(h.canonical.contains("role=record-only"))

  // ---- 6. input fingerprint --------------------------------------------------------------------------------------
  test("input fingerprint: identical across rebuilds; a 0.1 s onset shift and a one-ulp sample-time change are detected"):
    for spec <- Seq(condSpec, trialSpec) do
      val in = PrepSynth.inputs(spec)
      val prep = ok(CommonPreparation.prepare(in, spec.kind))
      val again = ok(CommonPreparation.prepare(PrepSynth.inputs(spec), spec.kind))
      assertEquals(prep.inputFingerprint, again.inputFingerprint)
      assertEquals(prep.inputFingerprint.length, 64)
      assert(prep.matches(PrepSynth.inputs(spec)))
      assertEquals(prep.requireMatches(in), Right(()))
      val shifted = PrepSynth.inputs(spec.copy(onsetShift = 0.1))
      assert(!prep.matches(shifted))
      assert(prep.requireMatches(shifted).isLeft)
      val ulp = PrepSynth.inputs(spec.copy(sampleTimeUlp = true))
      assert(!prep.matches(ulp))
      assertNotEquals(prep.inputFingerprint, InputFingerprint.of(shifted))
      assertNotEquals(InputFingerprint.of(shifted), InputFingerprint.of(ulp))

  // ---- 7. sigma2 -------------------------------------------------------------------------------------------------
  test("sigma2 is finite and near the noise variance; fold values use training runs only"):
    val prep = ok(CommonPreparation.prepare(PrepSynth.inputs(trialSpec), CellKind.Trial))
    val all = prep.sigma2All
    assert(all.sigma2 > 0.3 && all.sigma2 < 2.0, s"sigma2 ${all.sigma2}")
    assertEquals(all.rows, 300)
    val fold = ok(prep.sigma2Training(Set(0, 1)))
    assertEquals(fold.rows, 200)
    assert(fold.sigma2.isFinite && fold.sigma2 > 0.0)
    assertNotEquals(bits(fold.sigma2), bits(all.sigma2))
    assert(prep.sigma2Training(Set.empty).isLeft)
