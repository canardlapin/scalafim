package scalafim.phrfcmp.run

import gale.linalg.DMat

import scalafim.fmri.ar.WhiteningTransform
import scalafim.fmri.fit.{CanonicalTemporalWhitening, FitError}
import scalafim.fmri.fit.profile.DecodeStatus
import scalafim.fmri.hrf.Lag
import scalafim.fmri.hrf.family.{GaussianFamily, ShapePoint}
import scalafim.phrfcmp.ingest.{CellKind, ConditionSynth, FitInputs}
import scalafim.phrfcmp.ingest.ConditionSynth.{Spec, Truth}
import scalafim.phrfcmp.prep.{CommonPrep, CommonPreparation, InputFingerprint, PrepSynth, Whiten}
import scalafim.phrfcmp.score.ResponseGrid

/** The condition runner end to end (JVM and JS): PHRF compact route, CAN, INF3, FIR, shared inputs, typed refusals. */
class PhrfConditionSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private val truths = Vector(Truth(5.5, 1.6, Vector(3.0, 5.0, 2.0)), Truth(4.2, 1.1, Vector(4.0, 2.0, 3.0)), Truth(6.8, 2.2, Vector(2.0, 3.0, 5.0)))
  private val noisySpec = Spec(truths, noiseSd = 1.0, seed = 3L)
  private val exactSpec = noisySpec.copy(noiseSd = 0.0)

  private def prepOf(in: FitInputs): CommonPrep = CommonPreparation.prepare(in, CellKind.Condition).fold(e => fail(e.message), identity)
  private def bits(d: Double) = java.lang.Double.doubleToLongBits(d)

  private lazy val noisy = ConditionSynth.inputs(noisySpec)
  private lazy val prep = prepOf(noisy)
  private lazy val exact = ConditionSynth.inputs(exactSpec)

  test("all arms share one whitened-array hash; phi is bit-equal between plan and PHRF FitConfig"):
    val all = ConditionRunner.runAll(prep, noisy)
    assertEquals(all.results.map(_.arm), ConditionArm.All)
    assert(all.results.forall(_.inputSha256 == all.sharedInputSha256))
    assertEquals(all.results.map(_.inputSha256).distinct.size, 1)
    assertEquals(prep.armHashes.values.toSet, Set(all.sharedInputSha256))
    assertEquals(ConditionRunner.sharedInput(prep), Right(all.sharedInputSha256))
    val cfg = prep.phrfConfig.autocorrelation
    assertEquals(prep.plan.coefficients.head.phi.map(bits), cfg.rho.toVector.map(bits))
    assertEquals(bits(prep.rho), bits(cfg.rho.get))
    // the response arrays handed to arms are the one shared instance, not copies
    assert(prep.armInput(ConditionArm.Can.nativeArm).arrays eq prep.armInput(ConditionArm.Phrf.nativeArm).arrays)

  test("native arms estimate every voxel on the 0.1 s grid with finite coefficients"):
    // 200-scan runs with 16 events per run: 48 events identify FIR's 3 x 32 bins (cf. the 8-event case below)
    val big = ConditionSynth.inputs(noisySpec.copy(runLen = 200, eventsPerRun = 16, conds = Vector.tabulate(16)(_ % 3)))
    val bigPrep = prepOf(big)
    for arm <- Seq(ConditionArm.Can, ConditionArm.Inf3, ConditionArm.Fir) do
      val r = ConditionRunner.run(arm, bigPrep, big)
      assertEquals(r.statuses.distinct, Vector(ConditionArmStatus.Estimated), arm.id)
      assert(r.voxels.forall(_.response.exists(c => c.grid.size == 321 && c.conditions == 3 && c.curves.forall(_.forall(_.isFinite)))))
      assert(r.eventCoefficients.exists(m => m.cols == 3 && m.data.forall(_.isFinite)))
    assertEquals(ConditionRunner.run(ConditionArm.Fir, bigPrep, big).eventCoefficients.get.rows, 3 * 32)

  test("FIR with 8 events per run cannot identify 3 x 32 bins: a typed rank-deficiency failure, while CAN and INF3 estimate"):
    // 24 events in total against 96 FIR columns plus 18 nuisance columns: the design is genuinely rank deficient
    val r = ConditionRunner.run(ConditionArm.Fir, prep, noisy)
    assertEquals(r.statuses.map(_.code).distinct, Vector("failed:fit.RankDeficientDesign"))
    assert(r.voxels.forall(_.response.isEmpty) && r.eventCoefficients.isEmpty)
    for arm <- Seq(ConditionArm.Can, ConditionArm.Inf3) do
      assertEquals(ConditionRunner.run(arm, prep, noisy).statuses.distinct, Vector(ConditionArmStatus.Estimated))

  /**
    * TEST-ONLY. `CommonPreparation` is ill-posed on exactly noise-free data (the pre-fit residual that rho is estimated from
    * is numerically empty), so the plan, rho and sigma2 come from a noisy sibling with identical events, nuisance and runs,
    * and the whitened response is recomputed bit for bit from the exact data with that plan. The result is consistent in
    * the sense the runner verifies (`whitenedMatches`); only rho and sigma2 are borrowed.
    */
  private def testOnlyPrepFor(p: CommonPrep, in: FitInputs): CommonPrep =
    val wy = Whiten.series(p.plan, in.y).fold(e => fail(e.message), identity)
    p.copy(whitened = p.whitened.copy(y = wy), inputFingerprint = InputFingerprint.of(in))

  private lazy val exactPrep = testOnlyPrepFor(prep, exact)

  test("the runner refuses inputs that are not the ones the preparation was built from"):
    assert(ConditionRunner.whitenedMatches(prep, noisy))
    assert(!ConditionRunner.whitenedMatches(prep, exact))
    for arm <- ConditionArm.All do
      assertEquals(ConditionRunner.run(arm, prep, exact).statuses.map(_.code).distinct, Vector("failed:inputs-not-prepared"), arm.id)
    assert(ConditionRunner.whitenedMatches(exactPrep, exact))

  test("PHRF E-resp equals direct evaluation of the true response on noise-free data"):
    val r = ConditionRunner.run(ConditionArm.Phrf, exactPrep, exact)
    assertEquals(r.route, Some("direct-condition-compact"))
    assertEquals(r.statuses.distinct, Vector(ConditionArmStatus.Estimated), r.statuses.map(_.code).toString)
    val grid = ResponseGrid.standard().fold(e => fail(e.message), identity)
    var worstRel = 0.0
    var worstTau = 0.0
    var worstSd = 0.0
    for (tr, o) <- truths.zip(r.voxels) do
      val resp = o.response.get
      val peak = tr.amps.map(math.abs).max / (tr.sd * math.sqrt(2 * math.Pi))
      for c <- tr.amps.indices; i <- 0 until grid.size do
        val direct = tr.amps(c) * scalafim.fmri.hrf.HrfFunctions.gaussianPdf(Lag(grid.lag(i)), tr.tau, tr.sd)
        worstRel = math.max(worstRel, math.abs(resp.curves(c)(i) - direct) / peak)
      worstTau = math.max(worstTau, math.abs(o.phrf.get.coordinates(0) - tr.tau))
      worstSd = math.max(worstSd, math.abs(math.exp(o.phrf.get.coordinates(1)) - tr.sd))
    println(f"S3-PHRF noise-free: max |E-resp - direct| / peak = $worstRel%.3e, tau err $worstTau%.3e, sd err $worstSd%.3e")
    // achieved 1.6e-5 of the peak (tau 1.4e-6, sd 3.6e-5): the 0.2 s lowering and rank-40 kernel basis bound it, not noise
    assert(worstRel < 1e-4, s"E-resp error $worstRel")
    assert(worstTau < 1e-4 && worstSd < 1e-3, s"tau $worstTau sd $worstSd")

  test("reconstruction is exactly amplitude times the library kernel at the decoded coordinates"):
    val r = ConditionRunner.run(ConditionArm.Phrf, exactPrep, exact)
    val fam = GaussianFamily.Default
    val grid = ResponseGrid.standard().fold(e => fail(e.message), identity)
    var worst = 0.0
    for o <- r.voxels if o.status.isEstimated do
      val d = o.phrf.get
      val hrf = fam.toHrf(ShapePoint.unsafe(d.coordinates))
      for c <- d.conditionMeans.indices; i <- 0 until grid.size do
        worst = math.max(worst, math.abs(o.response.get.curves(c)(i) - d.conditionMeans(c) * hrf(Lag(grid.lag(i))).data(0)))
    assert(worst < 1e-12, s"worst $worst")

  test("a response far outside the latency chart decodes onto its edge: typed Boundary refusal, neighbour unaffected"):
    // Boundary rule (ShapeDecoder.scala:325-330, used at :614): a decoded coordinate x_i with
    //   x_i <= lower_i + 1e-12  or  x_i >= upper_i - 1e-12.
    // The Gaussian chart is tau in [3, 8] s (GaussianFamily.scala:107). Boundary is a property of the DECODED point, not
    // of the truth: tau = 11 is outside the chart but the free log-sd axis lets an interior (tau, sd) fit it (observed
    // decode tau 5.38), so it is not a boundary case. tau = 20 puts the whole response beyond the 24 s span, so the best
    // chart point pins tau at the upper bound 8 (observed decode tau 8.00).
    val spec = noisySpec.copy(truths = Vector(Truth(20.0, 1.5, Vector(3.0, 5.0, 2.0)), truths(0)))
    val out = ConditionSynth.inputs(spec.copy(noiseSd = 0.0))
    val outPrep = testOnlyPrepFor(prepOf(ConditionSynth.inputs(spec)), out)
    val r = ConditionRunner.run(ConditionArm.Phrf, outPrep, out)
    val first = r.voxels.head
    assertEquals(first.status, ConditionArmStatus.Refused(RefusalKind.Decode(DecodeStatus.Boundary)))
    assertEquals(first.status.code, "refused:decode.Boundary")
    assert(first.response.isEmpty)
    val tau = first.phrf.get.coordinates(0)
    assert(tau >= GaussianFamily.DefaultChart.upper(0) - 1e-12 || tau <= GaussianFamily.DefaultChart.lower(0) + 1e-12, s"tau $tau")
    assertEquals(r.voxels(1).status, ConditionArmStatus.Estimated)

  test("a condition with no events is a typed rank-deficiency failure in every native arm and a typed outcome in PHRF"):
    // conditions 0 and 2 only: condition 1 exists by index but never occurs, so its design columns are zero
    val spec = noisySpec.copy(conds = Vector(0, 2, 2, 0, 0, 2, 2, 0))
    val in = ConditionSynth.inputs(spec)
    val p = prepOf(in)
    for arm <- Seq(ConditionArm.Can, ConditionArm.Inf3, ConditionArm.Fir) do
      val r = ConditionRunner.run(arm, p, in)
      assertEquals(r.voxels.length, 3)
      assert(r.statuses.forall {
        case ConditionArmStatus.Failed(FailureKind.Fit(_: FitError.RankDeficientDesign)) => true
        case _ => false
      }, s"${arm.id}: ${r.statuses.map(_.code).distinct}")
      assert(r.statuses.forall(_.code == "failed:fit.RankDeficientDesign"))
    val phrf = ConditionRunner.run(ConditionArm.Phrf, p, in)
    assert(phrf.statuses.forall(s => s != ConditionArmStatus.Estimated && s != ConditionArmStatus.NotRun), phrf.statuses.map(_.code).toString)

  test("two conditions with identical events are collinear: typed failure, no exception"):
    val spec = noisySpec.copy(conds = Vector(0, 1, 0, 1, 0, 1, 0, 1))
    val in = ConditionSynth.inputs(spec)
    val collinear = ConditionSynth.pairedOnsets(in)
    // below the input binding (the collinear events are not the events `p` was prepared from): the native design and fit
    val p = prepOf(in)
    assert(!ConditionRunner.inputsBound(p, collinear))
    val events = ConditionEvents.fromInputs(collinear).fold(k => fail(k.code), identity)
    val x = ConditionDesigns.build(ConditionDesigns.designHrf(ConditionArm.Can, 32).get, events, collinear.sampleTime, p.segments).fold(k => fail(k.code), identity)
    val fit = NativeConditionFit.fit(x, p.plan, p.armInput(ConditionArm.Can.nativeArm).arrays)
    assertEquals(fit.left.toOption.map(_.code), Some("fit.RankDeficientDesign"))

  test("a trial-cell preparation is a typed kind mismatch"):
    val trialIn = PrepSynth.inputs(PrepSynth.Spec(CellKind.Trial, seed = 5L))
    val trialPrep = CommonPreparation.prepare(trialIn, CellKind.Trial).fold(e => fail(e.message), identity)
    val r = ConditionRunner.run(ConditionArm.Can, trialPrep, trialIn)
    assert(r.statuses.nonEmpty && r.statuses.forall(_.code == "failed:kind-mismatch"))

  // ---- input binding (F3) ---------------------------------------------------------------------------------------------
  test("every input array is bound to the preparation: a one-bit change in nuisance, onsets, conditions or sample times is refused"):
    assert(ConditionRunner.inputsBound(prep, noisy))
    val tampered = Seq(
      "nuisance" -> ConditionSynth.withNuisance(noisy), "onset" -> ConditionSynth.withOnset(noisy),
      "condition" -> ConditionSynth.withCond(noisy), "sample time" -> ConditionSynth.withSampleTime(noisy))
    for (what, in) <- tampered do
      assert(!ConditionRunner.inputsBound(prep, in), what)
      for arm <- ConditionArm.All do
        assertEquals(ConditionRunner.run(arm, prep, in).statuses.map(_.code).distinct, Vector("failed:inputs-not-prepared"), s"$what ${arm.id}")

  test("a sub-bin change (0.1 s within the same 1 s FIR bin) of an onset or a scan time is refused by the exact input fingerprint"):
    val onset = ConditionSynth.withOnsetSubBin(noisy)
    val scan = ConditionSynth.withSampleTimeSubBin(noisy)
    // the FIR pre-fit design alone cannot see these: it resolves to 1 s bins (the S3 rev 2 residual gap)
    assert(prep.requireMatches(noisy).isRight)
    for (what, in) <- Seq("onset" -> onset, "sample time" -> scan) do
      assert(prep.requireMatches(in).isLeft, what)
      assert(!ConditionRunner.inputsBound(prep, in), what)
      for arm <- ConditionArm.All do
        assertEquals(ConditionRunner.run(arm, prep, in).statuses.map(_.code).distinct, Vector("failed:inputs-not-prepared"), s"$what ${arm.id}")

  test("PHRF's internal shared whitening of the response is bit-equal to the common whitened arrays"):
    // PHRF whitens the raw response block (T x V, row major) through WhiteningTransform.matrix with the plan inside
    // CanonicalTemporalWhitening.Shared; S2's arrays use Whiten.series on the same plan. Same function, same plan.
    val shared = CanonicalTemporalWhitening.Shared(prep.plan)
    val plan = shared match
      case CanonicalTemporalWhitening.Shared(p) => p
      case _                                    => fail("not a shared plan")
    assert(plan eq prep.plan)
    val t = noisy.y.cols
    val v = noisy.y.rows
    val raw = DMat.tabulate(t, v)((r, k) => noisy.y(k, r))
    val internal = WhiteningTransform.matrix(plan, raw).fold(e => fail(e.toString), identity)
    var worst = 0L
    for r <- 0 until t; k <- 0 until v do
      worst = math.max(worst, java.lang.Math.abs(java.lang.Double.doubleToLongBits(internal(r, k)) - java.lang.Double.doubleToLongBits(prep.whitened.y(k, r))))
    assertEquals(worst, 0L, "bit difference between PHRF's whitening and Whiten.series")

  // ---- off-grid onsets (F1) -------------------------------------------------------------------------------------------
  // Kernels that are ~1e-7 at lag 0 (like a real HRF), so an onset that coincides with a scan does not meet the Gaussian
  // family's jump at lag 0 (a separate, documented convention: a response with a kernel that is not zero at lag 0).
  private val cleanTruths = Vector(Truth(6.5, 1.2, Vector(3.0, 5.0, 2.0)), Truth(6.0, 1.0, Vector(4.0, 2.0, 3.0)), Truth(7.0, 1.3, Vector(2.0, 3.0, 5.0)))

  private def noiseFreeError(spec: Spec, config: ConditionRunConfig = ConditionRunConfig()): Double =
    val sibling = ConditionSynth.inputs(spec.copy(noiseSd = 1.0, seed = 3L))
    val ex = ConditionSynth.inputs(spec.copy(noiseSd = 0.0))
    val r = ConditionRunner.run(ConditionArm.Phrf, testOnlyPrepFor(prepOf(sibling), ex), ex, config)
    assertEquals(r.statuses.distinct, Vector(ConditionArmStatus.Estimated), r.statuses.map(_.code).toString)
    val grid = ResponseGrid.standard().fold(e => fail(e.message), identity)
    var worst = 0.0
    for (tr, o) <- spec.truths.zip(r.voxels) do
      val peak = tr.amps.map(math.abs).max / (tr.sd * math.sqrt(2 * math.Pi))
      for c <- tr.amps.indices; i <- 0 until grid.size do
        worst = math.max(worst, math.abs(o.response.get.curves(c)(i) - tr.amps(c) * scalafim.fmri.hrf.HrfFunctions.gaussianPdf(Lag(grid.lag(i)), tr.tau, tr.sd)) / peak)
    worst

  for (first, gap, runLen, tr) <- Seq((4.0, 11.2, 100, 1.0), (4.1, 11.3, 100, 1.0), (4.1, 11.1, 100, 1.0), (4.0, 11.2, 200, 0.5), (4.1, 11.3, 200, 0.5)) do
    test(s"PHRF E-resp on noise-free data, onsets first=$first gap=$gap, TR=$tr: within 1e-4 of the peak at the default 0.1 s lowering"):
      val e = noiseFreeError(Spec(cleanTruths, first = first, gap = gap, runLen = runLen, tr = tr))
      println(f"S3-OFFGRID first=$first gap=$gap TR=$tr lowering=0.1: max |E-resp - direct| / peak = $e%.3e")
      assert(e < 1e-4, s"E-resp error $e")

  // The response at lag exactly 0 (an onset on a scan) is counted in full by the generator (kernels.py: `t >= 0`; the
  // truth is `f(lag) / pk`), by the native exact-lag Regressor (`lag >= 0`) and by the PHRF compact route (the impulse
  // sits on a lowering node and its fine kernel starts at lag 0). A kernel that is not ~0 at lag 0 makes the convention
  // visible: the Gaussian family at the chart edges (density 1e-3 of the peak at lag 0), onsets on every scan (T-TX).
  private val edgeTruths = Vector(Truth(3.1, 0.85, Vector(3.0, 5.0, 2.0)), Truth(7.8, 0.85, Vector(4.0, 2.0, 3.0)), Truth(3.2, 1.0, Vector(2.0, 3.0, 5.0)))

  test("T-TX-like: every onset exactly on a scan (TR 1), truths at the chart edges where the lag-0 density is largest: within 1e-4"):
    val e = noiseFreeError(Spec(edgeTruths, first = 4.0, gap = 11.0))
    println(f"S3-LAG0 onsets on scans, edge truths: max |E-resp - direct| / peak = $e%.3e")
    assert(e < 1e-4, s"E-resp error $e")

  test("the lag-0 convention has power: counting the lag-0 sample at half weight or not at all in the truth is detected"):
    val half = noiseFreeError(Spec(edgeTruths, first = 4.0, gap = 11.0, lag0Weight = 0.5))
    val none = noiseFreeError(Spec(edgeTruths, first = 4.0, gap = 11.0, lag0Weight = 0.0))
    println(f"S3-LAG0 truth with the lag-0 sample at half weight: $half%.3e, dropped: $none%.3e")
    assert(half > 1e-4 && none > half, s"half $half none $none")

  test("original truths (kernel 2.7e-3 of the peak at lag 0), odd-0.1 onsets that are exact decimals: within 1e-4"):
    val e = noiseFreeError(Spec(truths, first = 4.1, gap = 11.3))
    println(f"S3-LAG0 original truths first=4.1 gap=11.3: $e%.3e")
    assert(e < 1e-4, s"E-resp error $e")

  test("at a 0.2 s lowering grid an odd-0.1 onset costs a systematic error 100x larger (why the default is 0.1 s)"):
    val spec = Spec(cleanTruths, first = 4.1, gap = 11.3)
    val coarse = noiseFreeError(spec, ConditionRunConfig(phrf = PhrfConditionConfig(loweringSeconds = 0.2)))
    val fine = noiseFreeError(spec)
    println(f"S3-OFFGRID 4.1/11.3 TR=1: lowering 0.2 -> $coarse%.3e, lowering 0.1 -> $fine%.3e")
    assert(coarse > 1e-3 && coarse > 100 * fine, s"coarse $coarse fine $fine")

  // ---- refusals other than Boundary (M1) and admission reaching the policy (M4) -----------------------------------------
  test("a voxel with no signal is refused with a non-Boundary decode status, and a refusal is never an estimate"):
    val spec = noisySpec.copy(truths = Vector(Truth(5.5, 1.6, Vector(0.0, 0.0, 0.0)), truths(0)))
    val ex = ConditionSynth.inputs(spec.copy(noiseSd = 0.0))
    val r = ConditionRunner.run(ConditionArm.Phrf, testOnlyPrepFor(prepOf(ConditionSynth.inputs(spec)), ex), ex)
    val first = r.voxels.head
    val nonBoundary = Set(DecodeStatus.WeaklyIdentified, DecodeStatus.BudgetExceeded, DecodeStatus.CurvatureNotPositive, DecodeStatus.AmbiguousCells, DecodeStatus.NoAdmissibleNode)
    first.status match
      case ConditionArmStatus.Refused(RefusalKind.Decode(s)) => assert(nonBoundary.contains(s), s"status $s")
      case other => fail(s"expected a non-Boundary decode refusal, got ${other.code}")
    assert(first.response.isEmpty)
    assert(first.phrf.exists(_.decode != DecodeStatus.Accepted))
    assertEquals(r.voxels(1).status, ConditionArmStatus.Estimated)

  test("the observed admission and the held-out points reach the PHRF policy"):
    val r = ConditionRunner.run(ConditionArm.Phrf, prep, noisy)
    val a = r.admission.getOrElse(fail("no admission summary"))
    assertEquals(a.heldOutPoints, 3)
    assert(a.fingerprint.exists(_.nonEmpty), "the prepared run reports no observed-admission fingerprint")
    assert(a.maxProjectorError.isFinite)
    val two = ConditionRunConfig(phrf = PhrfConditionConfig(heldOut = PhrfConditionConfig().heldOut.take(2)))
    val r2 = ConditionRunner.run(ConditionArm.Phrf, prep, noisy, two)
    assertEquals(r2.admission.map(_.heldOutPoints), Some(2))
    assertNotEquals(r2.admission.map(_.maxProjectorError), r.admission.map(_.maxProjectorError))

  test("a dataset with a different voxel count than the preparation is a typed inconsistency"):
    val r = ConditionRunner.run(ConditionArm.Can, prep, ConditionSynth.inputs(noisySpec.copy(truths = truths.take(2))))
    assert(r.statuses.forall(_.code == "failed:inconsistent"))
