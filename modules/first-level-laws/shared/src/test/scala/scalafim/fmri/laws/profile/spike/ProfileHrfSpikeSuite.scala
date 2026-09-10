package scalafim.fmri.laws.profile.spike

/** PHRF-25 spike: kernel-basis condition fit end to end, against a dense
  * time-domain oracle, with strict work counters and phase timings. This is
  * an experiment, not a production estimator; see
  * docs/plans/profile-hrf-work-items-v2.md.
  */
class ProfileHrfSpikeSuite extends munit.FunSuite:

  override val munitTimeout: scala.concurrent.duration.Duration = scala.concurrent.duration.Duration(20, "min")

  private val domain = SpikeGaussianFamily.Domain(3.0, 8.0, math.log(0.8), math.log(3.0))
  private val fineDt = 0.1
  private val horizon = 24.0
  private val tr = 1.0
  private val stride = math.round(tr / fineDt).toInt
  private val nT = 600
  private val conditions = 3
  private val eventsPerCondition = 100
  private val arPhi = 0.3
  private val nuisanceCols = 6
  private val basisTolerance = 1e-5
  private val truthMargin = 0.5

  private def now(): Double = System.nanoTime() / 1e6

  private lazy val fullBasis = SpikeKernelBasis.compile(domain, fineDt, horizon, tauNodes = 26, logSdNodes = 21, includeDerivatives = true, maxRank = 48)
  private lazy val errors = SpikeKernelBasis.heldOutErrors(fullBasis, domain, points = 300, seed = 11L)
  private lazy val basisRank = errors.rankFor(basisTolerance).getOrElse(fail(s"no rank <= 48 reaches $basisTolerance"))
  private lazy val basis = fullBasis.truncated(basisRank)
  private lazy val nFine = basis.nFine
  private lazy val schedule = SpikeSchedule.random(conditions, eventsPerCondition, nT * stride, nFine, nT, stride, new scala.util.Random(20260910L))
  private lazy val nuisance = SpikeNuisance.columns(nT)
  private lazy val whitener = new SpikeWhitener(nT, arPhi)
  private lazy val prep = SpikePreparation.compile(schedule, basis, nuisance, nuisanceCols, whitener)

  private def percentile(values: Array[Double], p: Double): Double =
    if values.isEmpty then Double.NaN
    else
      val sorted = values.sorted
      sorted(math.min(sorted.length - 1, math.ceil(p * sorted.length).toInt - 1).max(0))

  test("family jets agree with central finite differences"):
    val times = Array.tabulate(240)(i => i * fineDt)
    val jet = new Array[Double](6 * times.length)
    val plus = new Array[Double](times.length)
    val minus = new Array[Double](times.length)
    val h = 1e-4
    for (tau, v) <- Seq((4.5, math.log(1.2)), (6.0, math.log(2.5)), (3.2, math.log(0.9))) do
      SpikeGaussianFamily.jetInto(times, tau, v, jet)
      def fd(comp: Int, dTau: Double, dV: Double): Unit =
        SpikeGaussianFamily.valueInto(times, tau + dTau, v + dV, plus)
        SpikeGaussianFamily.valueInto(times, tau - dTau, v - dV, minus)
        var i = 0
        var worst = 0.0
        while i < times.length do
          val fdv = (plus(i) - minus(i)) / (2.0 * math.hypot(dTau, dV))
          worst = math.max(worst, math.abs(fdv - jet(comp * times.length + i)))
          i += 1
        assert(worst < 1e-6, s"component $comp at ($tau, $v): max abs discrepancy $worst")
      fd(1, h, 0.0)
      fd(2, 0.0, h)
      // second derivatives from first-derivative differences
      val jp = new Array[Double](6 * times.length)
      val jm = new Array[Double](6 * times.length)
      def fd2(comp: Int, first: Int, dTau: Double, dV: Double): Unit =
        SpikeGaussianFamily.jetInto(times, tau + dTau, v + dV, jp)
        SpikeGaussianFamily.jetInto(times, tau - dTau, v - dV, jm)
        var i = 0
        var worst = 0.0
        while i < times.length do
          val fdv = (jp(first * times.length + i) - jm(first * times.length + i)) / (2.0 * math.hypot(dTau, dV))
          worst = math.max(worst, math.abs(fdv - jet(comp * times.length + i)))
          i += 1
        assert(worst < 1e-6, s"second-order component $comp at ($tau, $v): max abs discrepancy $worst")
      fd2(3, 1, h, 0.0)
      fd2(4, 1, 0.0, h)
      fd2(5, 2, 0.0, h)

  test("kernel basis rank report"):
    val report = Seq(1e-3, 1e-4, 1e-5).map { tol =>
      val mv = errors.rankFor(tol).map(_.toString).getOrElse(">48")
      val m1 = errors.firstDerivativeError.indexWhere(_ <= tol) + 1
      val m2 = errors.secondDerivativeError.indexWhere(_ <= tol) + 1
      s"tol=$tol value m=$mv firstDeriv m=${if m1 > 0 then m1 else ">48"} secondDeriv m=${if m2 > 0 then m2 else ">48"}"
    }
    println("[spike] kernel basis (Gaussian, tau in [3,8], sd in [0.8,3], T_h=24 s, dt=0.1 s):")
    report.foreach(line => println("[spike]   " + line))
    println(s"[spike]   chosen m=$basisRank for value tolerance $basisTolerance; K=${prep.rank} (C*m=${conditions * basisRank}), nuisance rank=${prep.nuisanceRank}")
    assert(basisRank <= 32, s"basis rank $basisRank exceeds 32")
    assertEquals(prep.rank, conditions * basisRank, "expanded design should be full rank for this schedule")

  test("compact energy and amplitudes agree with the direct time-domain fit at a prepared shape"):
    val rng = new scala.util.Random(7L)
    val cohort = SpikeCohort.generate(schedule, nuisance, nuisanceCols, basis.fineTimes, domain.tauMin + truthMargin, domain.tauMax - truthMargin, domain.logSdMin + 0.2, domain.logSdMax - 0.2, voxels = 8, snr = 1.0, arPhi, rng)
    val wy = whitener(cohort.voxels, transpose(cohort.y, cohort.voxels, nT))
    val oracle = new SpikeOracle(schedule, prep, whitener, basis.fineTimes, domain, tauGrid = 3, logSdGrid = 3, compassLevels = 0)
    val solver = new SpikeProfileSolver(prep, basis, domain, 8, 8)
    val counters = new SpikeCounters
    val z = new Array[Double](prep.rank)
    val qy = new Array[Double](prep.nuisanceRank)
    val column = new Array[Double](nT)
    val oracleOut = new SpikeOracleResult(conditions)
    var v = 0
    var worstEnergy = 0.0
    var worstBeta = 0.0
    while v < cohort.voxels do
      extractColumn(wy, cohort.voxels, nT, v, column)
      val e = prep.project(column, 0, z, qy)
      // Direct fit at the true shape through the oracle's exact evaluator.
      val direct = oracleDirect(oracle, column, cohort.trueTau(v), cohort.trueLogSd(v), oracleOut)
      val compact = solver.exactEnergyForTest(cohort.trueTau(v), cohort.trueLogSd(v), z, e, counters)
      worstEnergy = math.max(worstEnergy, math.abs(compact - direct) / e)
      var i = 0
      while i < conditions do
        worstBeta = math.max(worstBeta, math.abs(solver.lastAmplitudes(i) - oracleOut.beta(i)) / math.max(1e-9, math.abs(oracleOut.beta(i))))
        i += 1
      v += 1
    println(f"[spike] prepared-shape agreement: max |E_compact - E_direct|/e = $worstEnergy%.3e, max rel amplitude diff = $worstBeta%.3e")
    assert(worstEnergy < 1e-5, s"energy discrepancy $worstEnergy")
    assert(worstBeta < 1e-3, s"amplitude discrepancy $worstBeta")

  private def oracleDirect(oracle: SpikeOracle, wy: Array[Double], tau: Double, logSd: Double, out: SpikeOracleResult): Double =
    oracle.energyAtForTest(wy, tau, logSd, out)

  private def transpose(voxelMajor: Array[Double], voxels: Int, nT: Int): Array[Double] =
    val out = new Array[Double](voxelMajor.length)
    var v = 0
    while v < voxels do
      var t = 0
      while t < nT do
        out(t * voxels + v) = voxelMajor(v * nT + t)
        t += 1
      v += 1
    out

  private def extractColumn(timeMajor: Array[Double], voxels: Int, nT: Int, v: Int, out: Array[Double]): Unit =
    var t = 0
    while t < nT do
      out(t) = timeMajor(t * voxels + v)
      t += 1

  private final case class Variant(name: String, tauNodes: Int, logSdNodes: Int, steps: Int, exact: Int, mode: Int, coarse: Int = 1):
    def solver(prep: SpikePreparation, basis: SpikeKernelBasis): SpikeProfileSolver =
      new SpikeProfileSolver(prep, basis, domain, tauNodes, logSdNodes, steps, exact, mode, coarse)
  private val variants = Seq(
    Variant("8x8 full x2", 8, 8, 2, 6, SpikeProfileSolver.FullNewton),
    Variant("8x8 full x3", 8, 8, 3, 8, SpikeProfileSolver.FullNewton),
    Variant("12x12 full x2", 12, 12, 2, 6, SpikeProfileSolver.FullNewton),
    Variant("12x12 node+GN x3", 12, 12, 3, 8, SpikeProfileSolver.GaussNewton),
    Variant("12x12 frozenH x3", 12, 12, 3, 8, SpikeProfileSolver.FrozenHessian),
    Variant("15x15 hier full x2", 15, 15, 2, 6, SpikeProfileSolver.FullNewton, coarse = 2),
    Variant("15x15 hier frozenH x3", 15, 15, 3, 8, SpikeProfileSolver.FrozenHessian, coarse = 2),
    Variant("23x23 hier full x2", 23, 23, 2, 6, SpikeProfileSolver.FullNewton, coarse = 3),
    Variant("23x23 hier frozenH x2", 23, 23, 2, 6, SpikeProfileSolver.FrozenHessian, coarse = 3)
  )
  private val defaultVariant = 5

  test("decoded shapes agree with the dense oracle across SNR levels"):
    val oracle = new SpikeOracle(schedule, prep, whitener, basis.fineTimes, domain, tauGrid = 101, logSdGrid = 21)
    val solvers = variants.map(vn => vn.solver(prep, basis))
    val voxels = 200
    val gates = Seq(0.02, 0.05, 1e-3)
    var seed = 100L
    var overall = true
    for snr <- Seq(1.0, 0.5, 0.25) do
      seed += 1
      val cohort = SpikeCohort.generate(schedule, nuisance, nuisanceCols, basis.fineTimes, domain.tauMin + truthMargin, domain.tauMax - truthMargin, domain.logSdMin + 0.2, domain.logSdMax - 0.2, voxels, snr, arPhi, new scala.util.Random(seed))
      val wy = whitener(voxels, transpose(cohort.y, voxels, nT))
      val z = new Array[Double](prep.rank * voxels)
      val es = new Array[Double](voxels)
      val qy = new Array[Double](prep.nuisanceRank)
      val column = new Array[Double](nT)
      val zv = new Array[Double](prep.rank)
      val orTau = new Array[Double](voxels)
      val orLogSd = new Array[Double](voxels)
      val orBeta = new Array[Double](voxels * conditions)
      val orEnergy = new Array[Double](voxels)
      val or = new SpikeOracleResult(conditions)
      var oracleEvals = 0L
      val t0 = now()
      var v = 0
      while v < voxels do
        extractColumn(wy, voxels, nT, v, column)
        es(v) = prep.project(column, 0, zv, qy)
        System.arraycopy(zv, 0, z, v * prep.rank, prep.rank)
        oracle.solve(column, 0, or)
        orTau(v) = or.tau
        orLogSd(v) = or.logSd
        orEnergy(v) = or.energy
        System.arraycopy(or.beta, 0, orBeta, v * conditions, conditions)
        oracleEvals += or.exactEvaluations
        v += 1
      val oracleMs = now() - t0
      println(f"[spike] SNR $snr%.2f: oracle ${oracleMs / voxels}%.2f ms/voxel, ${oracleEvals.toDouble / voxels}%.1f exact time-domain evaluations/voxel")
      variants.zipWithIndex.foreach { case (vn, vi) =>
        val solver = solvers(vi)
        val counters = new SpikeCounters
        val res = new SpikeVoxelResult(conditions)
        val tauErr = Array.newBuilder[Double]
        val fwhmErr = Array.newBuilder[Double]
        val ampErr = Array.newBuilder[Double]
        val tauTruthErr = Array.newBuilder[Double]
        var maxGap = 0.0
        var admitted = 0
        var newtonAccepted = 0
        val s0 = now()
        v = 0
        while v < voxels do
          System.arraycopy(z, v * prep.rank, zv, 0, prep.rank)
          solver.solve(zv, es(v), counters, res)
          if res.status == SpikeStatus.Accepted then
            admitted += 1
            tauErr += math.abs(res.tau - orTau(v))
            fwhmErr += math.abs(SpikeGaussianFamily.fwhm(res.logSd) - SpikeGaussianFamily.fwhm(orLogSd(v)))
            var num = 0.0
            var den = 0.0
            var i = 0
            while i < conditions do
              val d = res.beta(i) - orBeta(v * conditions + i)
              num += d * d
              den += orBeta(v * conditions + i) * orBeta(v * conditions + i)
              i += 1
            ampErr += math.sqrt(num / den)
            tauTruthErr += math.abs(res.tau - cohort.trueTau(v))
            maxGap = math.max(maxGap, (res.energy - orEnergy(v)) / math.max(1e-12, orEnergy(v)))
            if res.newtonSteps > 0 then newtonAccepted += 1
          v += 1
        val solverMs = now() - s0
        val te = tauErr.result()
        val fe = fwhmErr.result()
        val ae = ampErr.result()
        val tt = tauTruthErr.result()
        val p95Tau = percentile(te, 0.95)
        val p95F = percentile(fe, 0.95)
        val p95A = percentile(ae, 0.95)
        val ok = admitted >= 0.95 * voxels && p95Tau <= gates(0) && p95F <= gates(1) && p95A <= gates(2)
        println(f"[spike]   ${vn.name}%-22s admitted ${admitted}%3d/$voxels, p95 |tau-or| ${p95Tau}%.4f s, p95 |FWHM-or| ${p95F}%.4f s, p95 rel amp ${p95A}%.2e, max energy gap ${maxGap}%.1e, ${solverMs / voxels}%.3f ms/voxel, jets ${counters.perVoxel(counters.jets)}%.2f (GN ${counters.perVoxel(counters.gaussNewtonJets)}%.2f), exact ${counters.perVoxel(counters.exactEvaluations)}%.2f, Newton in $newtonAccepted, median |tau-truth| ${percentile(tt, 0.5)}%.3f s ${if ok then "GATES MET" else "gates unmet"}")
        if snr >= 0.5 && vi == defaultVariant then overall = overall && ok
      }
    assert(overall, s"accuracy gates unmet at SNR >= 0.5 for the default variant ${variants(defaultVariant).name}; see [spike] output")

  test("basis tolerance sweep for the default variant"):
    val oracle = new SpikeOracle(schedule, prep, whitener, basis.fineTimes, domain, tauGrid = 101, logSdGrid = 21)
    val voxels = 200
    val vn = variants(defaultVariant)
    for tol <- Seq(1e-3, 1e-4) do
      val rank = errors.rankFor(tol).getOrElse(fail(s"no rank for $tol"))
      val b = fullBasis.truncated(rank)
      val pr = SpikePreparation.compile(schedule, b, nuisance, nuisanceCols, whitener)
      val solver = vn.solver(pr, b)
      for snr <- Seq(1.0, 0.5) do
        val cohort = SpikeCohort.generate(schedule, nuisance, nuisanceCols, b.fineTimes, domain.tauMin + truthMargin, domain.tauMax - truthMargin, domain.logSdMin + 0.2, domain.logSdMax - 0.2, voxels, snr, arPhi, new scala.util.Random(101L + (if snr == 1.0 then 0 else 1)))
        val wy = whitener(voxels, transpose(cohort.y, voxels, nT))
        val z = new Array[Double](pr.rank)
        val qy = new Array[Double](pr.nuisanceRank)
        val column = new Array[Double](nT)
        val res = new SpikeVoxelResult(conditions)
        val or = new SpikeOracleResult(conditions)
        val counters = new SpikeCounters
        val tauErr = Array.newBuilder[Double]
        val ampErr = Array.newBuilder[Double]
        var admitted = 0
        var v = 0
        var solverMs = 0.0
        while v < voxels do
          extractColumn(wy, voxels, nT, v, column)
          val e = pr.project(column, 0, z, qy)
          val s0 = now()
          solver.solve(z, e, counters, res)
          solverMs += now() - s0
          oracle.solve(column, 0, or)
          if res.status == SpikeStatus.Accepted then
            admitted += 1
            tauErr += math.abs(res.tau - or.tau)
            var num = 0.0
            var den = 0.0
            var i = 0
            while i < conditions do
              val dd = res.beta(i) - or.beta(i)
              num += dd * dd
              den += or.beta(i) * or.beta(i)
              i += 1
            ampErr += math.sqrt(num / den)
          v += 1
        println(f"[spike] basis tol $tol m=$rank K=${pr.rank} SNR $snr%.2f ${vn.name}: admitted $admitted/$voxels, p95 |tau-or| ${percentile(tauErr.result(), 0.95)}%.4f s, p95 rel amp ${percentile(ampErr.result(), 0.95)}%.2e, ${solverMs / voxels}%.3f ms/voxel")

  test("benchmark: V=10,000 condition fit with phase timings"):
    val voxels = 10000
    val t0 = now()
    val cohort = SpikeCohort.generate(schedule, nuisance, nuisanceCols, basis.fineTimes, domain.tauMin + truthMargin, domain.tauMax - truthMargin, domain.logSdMin + 0.2, domain.logSdMax - 0.2, voxels, 0.5, arPhi, new scala.util.Random(5L))
    val tGen = now()
    val timeMajor = transpose(cohort.y, voxels, nT)
    val wy = whitener(voxels, timeMajor)
    val tWhiten = now()
    val z = new Array[Double](prep.rank * voxels)
    val es = new Array[Double](voxels)
    val qy = new Array[Double](4 * prep.nuisanceRank)
    val column = new Array[Double](nT)
    val zv = new Array[Double](prep.rank)
    var v = 0
    while v < voxels do
      extractColumn(wy, voxels, nT, v, column)
      es(v) = prep.project(column, 0, zv, qy)
      System.arraycopy(zv, 0, z, v * prep.rank, prep.rank)
      v += 1
    val tProject = now()
    // 4-wide blocked projection straight from the time-major block
    val z4 = new Array[Double](prep.rank * voxels)
    val es4 = new Array[Double](voxels)
    v = 0
    while v + 4 <= voxels do
      prep.projectFour(wy, voxels, v, z4, v * prep.rank, es4, qy)
      v += 4
    val tProject4 = now()
    var maxDiff = 0.0
    var i = 0
    while i < z.length do
      maxDiff = math.max(maxDiff, math.abs(z(i) - z4(i)))
      i += 1
    assert(maxDiff < 1e-9, s"blocked projection differs by $maxDiff")
    val gen = tGen - t0
    val wh = tWhiten - tGen
    val pr = tProject - tWhiten
    val pr4 = tProject4 - tProject
    println(f"[spike] benchmark V=$voxels T=$nT C=$conditions K=${prep.rank} m=${basis.m}: generate ${gen}%.0f ms, whiten (ar) ${wh}%.0f ms, project ${pr}%.0f ms (per-voxel loop) / ${pr4}%.0f ms (4-wide blocked)")
    variants.foreach { vn =>
      val solver = vn.solver(prep, basis)
      val counters = new SpikeCounters
      val res = new SpikeVoxelResult(conditions)
      val out = new Array[Double](voxels * (2 + conditions))
      var accepted = 0
      // warm up on the first 1000 voxels, then time all
      v = 0
      while v < 1000 do
        System.arraycopy(z, v * prep.rank, zv, 0, prep.rank)
        solver.solve(zv, es(v), new SpikeCounters, res)
        v += 1
      val s0 = now()
      v = 0
      while v < voxels do
        System.arraycopy(z, v * prep.rank, zv, 0, prep.rank)
        solver.solve(zv, es(v), counters, res)
        out(v * (2 + conditions)) = res.tau
        out(v * (2 + conditions) + 1) = res.logSd
        System.arraycopy(res.beta, 0, out, v * (2 + conditions) + 2, conditions)
        if res.status == SpikeStatus.Accepted then accepted += 1
        v += 1
      val so = now() - s0
      println(f"[spike]   ${vn.name}%-18s post-solve ${so}%.0f ms = ${so / voxels}%.4f ms/voxel (ratio to 4-wide projection ${so / pr4}%.1f): scan ${counters.scanNanos / 1e6 / voxels}%.4f, jets ${counters.jetNanos / 1e6 / voxels}%.4f, exact ${counters.exactNanos / 1e6 / voxels}%.4f ms; per voxel nodes ${counters.perVoxel(counters.nodeScores)}%.0f, jets ${counters.perVoxel(counters.jets)}%.2f (full ${counters.perVoxel(counters.fullJets)}%.2f, GN ${counters.perVoxel(counters.gaussNewtonJets)}%.2f), exact ${counters.perVoxel(counters.exactEvaluations)}%.2f; accepted $accepted; V=100k extrapolation ${(wh + pr4 + so) * 10 / 1000}%.1f s")
      assert(counters.perVoxel(counters.nodeScores) <= (vn.tauNodes * vn.logSdNodes).toDouble)
      assert(counters.perVoxel(counters.jets) <= vn.steps + 1e-9, s"${vn.name}: jets ${counters.perVoxel(counters.jets)}")
      assert(counters.perVoxel(counters.exactEvaluations) <= vn.exact + 1e-9)
      assert(out.forall(_.isFinite))
    }
