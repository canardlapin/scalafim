package scalafim.group.research.bootstrap

import ResearchTestSupport.*

/** Theory-mandated properties of declaration §7 (items 1 and 2) and v1's
  * bootstrap-of-z exactness. Every check uses fixed seeds, so the outcome is deterministic.
  */
class BootstrapTheorySuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  val Alpha = 0.05
  val Candidates: Vector[Scheme] = Scheme.values.toVector.filter(_.role == SchemeRole.Candidate)

  /** §7.1 exchangeable-rank control: observed data and all B draws come from one fixed
    * P(theta0) (known sigma^2, tau^2, nu; no generator refit), ties broken by an
    * independent uniform. The rank is exactly discrete-uniform on 1..B+1.
    */
  private def exchangeableRanks(cellId: String, outer: Int, draws: Int): Array[Int] =
    val c = cell(cellId)
    val d = c.researchDesign
    val fitter = new StudyFitter(d)
    val sigma2 = c.sigma2.get
    val tau = math.sqrt(c.tau2.value)
    val nu = c.trueNu.value
    val y = new Array[Double](c.n)
    val v = new Array[Double](c.n)
    val ranks = new Array[Int](outer)
    var r = 0
    while r < outer do
      val key = StreamKey(Phase.Harness.root(StreamKind.Outcome), c.id, r, StreamKind.Outcome)
      val lane = key.lane(Lane.SubjectE)
      val ties = key.lane(Lane.TieBreak)
      def statistic(): (Double, Double) =
        var i = 0
        while i < c.n do
          y(i) = tau * lane.nextGaussian() + math.sqrt(sigma2(i)) * lane.nextGaussian()
          v(i) = if nu.isInfinite then sigma2(i) else sigma2(i) * lane.nextChiSquare(nu) / nu
          i += 1
        val status = fitter.fitFull(y, v, TauPolicy.PauleMandel)
        (if status.ok then math.abs(fitter.mkhStatistic(0.0)) else 0.0, ties.nextDouble())
      val (t0, u0) = statistic()
      var above = 0
      var b = 0
      while b < draws do
        val (t, u) = statistic()
        if t > t0 || (t == t0 && u >= u0) then above += 1
        b += 1
      ranks(r) = above + 1
      r += 1
    ranks

  private def checkDiscreteUniform(label: String, ranks: Array[Int], draws: Int): Unit =
    val m = draws + 1
    val exact = math.floor(Alpha * m) / m
    val rejections = ranks.count(rank => rank.toDouble / m <= Alpha)
    val chi = rankChiSquare(ranks, m, 50)
    val ks = rankKs(ranks, m)
    println(f"RANK_CONTROL,$label,R=${ranks.length},B=$draws,rejections=$rejections,rate=${rejections.toDouble / ranks.length}%.4f,exact=$exact%.4f,chi2_49=$chi%.2f,ks=$ks%.4f")
    assert(consistentWithRate(rejections, ranks.length, exact, 0.001), s"$label: CP 99.9% interval excludes the exact rate $exact ($rejections/${ranks.length})")
    assert(chi < ChiSquare49At999, s"$label: rank chi-square $chi exceeds the .001 critical value")
    assert(ks <= ksTolerance(ranks.length), s"$label: KS distance $ks exceeds the pre-set tolerance ${ksTolerance(ranks.length)}")

  test("§7.1 exchangeable-rank control: P(p <= alpha) = floor(alpha(B+1))/(B+1), 50-bin chi-square and KS"):
    checkDiscreteUniform("n8-I-spread-T2-N8", exchangeableRanks("C-n8-DI-Vspread-T2-N8", 1000, 99), 99)
    checkDiscreteUniform("n20-G-rev-T0-N8", exchangeableRanks("C-n20-DG-Vrev-T0-N8", 500, 99), 99)

  test("§7.1 heavy: exchangeable-rank control at R = 5000, B = 199 (opt-in)"):
    assume(heavy, howToEnableHeavy)
    checkDiscreteUniform("heavy-n20-G-spread-T2-N8", exchangeableRanks("C-n20-DG-Vspread-T2-N8", 5000, 199), 199)

  /** Bootstrap-of-z: known v, tau^2 fixed at 0, restricted world. The z statistic is an exact
    * N(0,1) pivot under the null in the data and in every bootstrap world, so the Monte
    * Carlo test is exact: P(p <= alpha) = floor(alpha(B+1))/(B+1).
    */
  private def bootstrapOfZRanks(cellId: String, outer: Int, draws: Int): Array[Int] =
    val c = cell(cellId)
    val engine = new BootstrapEngine(c.researchDesign)
    Array.tabulate(outer) { r =>
      val sim = ModelJ.draw(c, Phase.Harness, StudyPurpose.Null, r, engine.design)
      val key = StreamKey.of(Phase.Harness, StudyPurpose.Null, c.id, r, StreamKind.Bootstrap)
      val outcome = value(engine.run(sim.data, Scheme.FixV, draws, BootstrapEngine.streamVariates(key), StatisticKind.KnownVarianceZ))
      assertEquals(outcome.failed, 0)
      outcome.exceed + 1
    }

  test("bootstrap-of-z exactness: the restricted z bootstrap has exactly the discrete-uniform p law"):
    checkDiscreteUniform("z-n8-I-spread", bootstrapOfZRanks("C-n8-DI-Vspread-T0-Ninf", 2000, 99), 99)
    checkDiscreteUniform("z-n20-G-rev", bootstrapOfZRanks("C-n20-DG-Vrev-T0-Ninf", 1000, 99), 99)

  test("bootstrap-of-z heavy: R = 20000, B = 99 (opt-in)"):
    assume(heavy, howToEnableHeavy)
    checkDiscreteUniform("heavy-z-n20-G-spread", bootstrapOfZRanks("C-n20-DG-Vspread-T0-Ninf", 20000, 99), 99)

  test("p bounds: the +1 rule, failures widen the upper bound, straddling alpha is Unresolved"):
    val base = BootstrapOutcome(Scheme.Plug, 2.0, 99, 3, 0, 0, 0.0, 0.0, None)
    assertEqualsDouble(base.pLower, 0.04, 1e-15)
    assertEqualsDouble(base.pUpper, 0.04, 1e-15)
    assertEquals(base.decision(Alpha), StudyDecision.Reject)
    val zero = base.copy(exceed = 0)
    assertEqualsDouble(zero.pLower, 0.01, 1e-15)
    val straddle = base.copy(failed = 2)
    assertEqualsDouble(straddle.pUpper, 0.06, 1e-15)
    assertEquals(straddle.decision(Alpha), StudyDecision.Unresolved)
    assertEquals(base.copy(exceed = 5).decision(Alpha), StudyDecision.Retain)

  test("draw failures are counted against the fixed B, never replaced"):
    val sim = study("C-n20-DI-Vspread-T2-N8", 0)
    val engine = new BootstrapEngine(sim.data.design)
    val good = variates(sim, engine, Scheme.Plug, 49)
    // Poison draws 3 and 17 with a non-finite normal: those refits must fail and be counted.
    val poisoned = good.copy(ze = good.ze.clone())
    poisoned.ze(3 * sim.cell.n) = Double.NaN
    poisoned.ze(17 * sim.cell.n + 4) = Double.PositiveInfinity
    val sink = new Array[Double](49)
    val out = value(engine.run(sim.data, Scheme.Plug, 49, BootstrapEngine.fixedVariates(poisoned), sink = Some(sink)))
    assertEquals(out.failed, 2)
    assertEquals(out.draws, 49)
    assert(sink(3).isNaN && sink(17).isNaN)
    assertEqualsDouble(out.pUpper - out.pLower, 2.0 / 50.0, 1e-15)

  /** Runs one scheme with explicit variates and returns (outcome, T* vector). */
  private def runWith(data: StudyData, scheme: Scheme, v: MatrixDrawVariates): (BootstrapOutcome, Array[Double]) =
    val engine = new BootstrapEngine(data.design)
    val sink = new Array[Double](v.draws)
    (value(engine.run(data, scheme, v.draws, BootstrapEngine.fixedVariates(v), sink = Some(sink))), sink)

  private def assertSameP(label: String, a: (BootstrapOutcome, Array[Double]), b: (BootstrapOutcome, Array[Double]), sign: Double, tol: Double): Unit =
    assertEquals(b._1.exceed, a._1.exceed, s"$label: k")
    assertEquals(b._1.failed, a._1.failed, s"$label: f")
    assertEqualsDouble(b._1.statistic, sign * a._1.statistic, tol * math.max(1.0, math.abs(a._1.statistic)), s"$label: T")
    a._2.indices.foreach(i => assertEqualsDouble(b._2(i), sign * a._2(i), tol * math.max(1.0, math.abs(a._2(i))), s"$label: T*_$i"))

  private val Draws = 99
  private def invarianceStudies: Vector[SimulatedStudy] =
    Vector(study("C-n20-DG-Vspread-T2-N8", 11), study("C-n8-DI-Vspread-T2-N8", 7), study("C-n20-DG-Vrev-T0-N40", 3))

  test("§7.2 scale invariance: (y, v, b0) -> (c y, c^2 v, c b0) with the same innovations gives identical p"):
    for sim <- invarianceStudies; scheme <- Scheme.values do
      val engine = new BootstrapEngine(sim.data.design)
      val vs = variates(sim, engine, scheme, Draws)
      val base = sim.data.copy(b0 = 0.1)
      val ref = runWith(base, scheme, vs)
      assertSameP(s"${sim.cell.id.value} ${scheme.code} c=2", ref, runWith(base.scaled(2.0), scheme, vs), 1.0, 1e-12)
      assertSameP(s"${sim.cell.id.value} ${scheme.code} c=3.7", ref, runWith(base.scaled(3.7), scheme, vs), 1.0, 1e-9)

  test("§7.2 design invariance: X -> X L with c -> L'c gives identical p"):
    val l3 = Array(1.0, 0.0, 0.0, 0.5, 2.0, 0.0, -0.3, 0.1, 0.7)
    for sim <- invarianceStudies; scheme <- Scheme.values do
      val engine = new BootstrapEngine(sim.data.design)
      val vs = variates(sim, engine, scheme, Draws)
      val l = if sim.cell.p == 3 then l3 else Array(2.5)
      val moved = value(sim.data.design.reparameterized(l))
      val ref = runWith(sim.data, scheme, vs)
      assertSameP(s"${sim.cell.id.value} ${scheme.code} XL", ref, runWith(sim.data.copy(design = moved), scheme, vs), 1.0, 1e-9)

  test("§7.2 order invariance: permuting subjects together with their variates gives identical p"):
    for sim <- invarianceStudies; scheme <- Scheme.values do
      val engine = new BootstrapEngine(sim.data.design)
      val vs = variates(sim, engine, scheme, Draws)
      val n = sim.cell.n
      val perm = Array.tabulate(n)(i => (7 * i + 3) % n) match
        case p if p.distinct.length == n => p
        case _ => Array.tabulate(n)(i => n - 1 - i)
      val ref = runWith(sim.data, scheme, vs)
      assertSameP(s"${sim.cell.id.value} ${scheme.code} perm", ref, runWith(value(sim.data.permuted(perm)), scheme, vs.permuted(perm)), 1.0, 1e-9)

  test("§7.2 sign symmetry: p(y) = p(-y) at b0 = 0 with negated normals and identical chi-square draws"):
    for sim <- invarianceStudies; scheme <- Scheme.values do
      val engine = new BootstrapEngine(sim.data.design)
      val vs = variates(sim, engine, scheme, Draws)
      val ref = runWith(sim.data, scheme, vs)
      assertSameP(s"${sim.cell.id.value} ${scheme.code} -y", ref, runWith(sim.data.negated, scheme, vs.negatedNormals), -1.0, 0.0)

  test("paired randomness: candidates share the u*, e* and chi lanes of one study"):
    val sim = study("C-n20-DG-Vspread-T2-N8", 11)
    val engine = new BootstrapEngine(sim.data.design)
    val plug = variates(sim, engine, Scheme.Plug, 9)
    val eb = variates(sim, engine, Scheme.EmpiricalBayes, 9)
    val swap = variates(sim, engine, Scheme.NuSwap, 9)
    assertEquals(plug.zu.toVector, eb.zu.toVector)
    assertEquals(plug.ze.toVector, eb.ze.toVector)
    assertEquals(plug.chi.toVector, eb.chi.toVector)
    assertEquals(plug.zu.toVector, swap.zu.toVector)
    assertNotEquals(plug.chi.toVector, swap.chi.toVector)
