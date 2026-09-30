package scalafim.group.research.bootstrap

import ResearchTestSupport.*

/** Native theory checks of the candidate draw formulas (declaration §1), which the
  * R parity also covers, so that no candidate formula rests on one reference alone.
  */
class CandidateTheorySuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  val Alpha = 0.05

  private def runSink(data: StudyData, scheme: Scheme, v: MatrixDrawVariates, statistic: StatisticKind = StatisticKind.PmMkh): (BootstrapOutcome, Array[Double]) =
    val engine = new BootstrapEngine(data.design)
    val out = new Array[Double](v.draws)
    (value(engine.run(data, scheme, v.draws, BootstrapEngine.fixedVariates(v), statistic, Some(out))), out)

  test("B-fixV and the known-v control never read the chi lane: T* is bit-identical under arbitrary chi values"):
    for id <- Vector("C-n20-DG-Vspread-T2-N8", "C-n8-DI-Vspread-T2-N8"); scheme <- Vector(Scheme.FixV, Scheme.KnownVFrozenTau) do
      val sim = study(id, 5)
      val engine = new BootstrapEngine(sim.data.design)
      val v = variates(sim, engine, scheme, 49)
      val g = SplitMix64.fromSeed(2026093077L)
      val scrambled = v.copy(chi = Array.fill(v.chi.length)(0.05 + 20.0 * g.nextDouble()))
      val (a, ta) = runSink(sim.data, scheme, v)
      val (b, tb) = runSink(sim.data, scheme, scrambled)
      assertEquals(tb.toVector, ta.toVector, s"$id ${scheme.code}")
      assertEquals((b.exceed, b.failed), (a.exceed, a.failed))

  /** Conditional exactness at finite nu: given v, y ~ N(X beta0, diag v) with tau^2 = 0. The z statistic
    * with v as known variances is an exact N(0,1) pivot in the data and in every B-fixV world
    * (e* ~ N(0, v), v* = v), so the rank of T among the draws is exactly discrete-uniform.
    */
  private def conditionalRanks(cellId: String, outer: Int, draws: Int): Array[Int] =
    val c = cell(cellId)
    val engine = new BootstrapEngine(c.researchDesign)
    val sigma2 = c.sigma2.get
    val nu = c.trueNu.value
    Array.tabulate(outer) { r =>
      val key = StreamKey(Phase.Harness.root(StreamKind.Outcome), c.id, 5000 + r, StreamKind.Outcome)
      val chi = key.lane(Lane.FirstLevelChi)
      val e = key.lane(Lane.SubjectE)
      val v = Array.tabulate(c.n)(i => sigma2(i) * chi.nextChiSquare(nu) / nu)
      val y = Array.tabulate(c.n)(i => math.sqrt(v(i)) * e.nextGaussian())
      val data = StudyData(engine.design, y, v, Array.fill(c.n)(nu), 0.0)
      val bootKey = StreamKey(Phase.Harness.root(StreamKind.Bootstrap), c.id, 5000 + r, StreamKind.Bootstrap)
      val out = value(engine.run(data, Scheme.FixV, draws, BootstrapEngine.streamVariates(bootKey), StatisticKind.KnownVarianceZ))
      assertEquals(out.failed, 0)
      out.exceed + 1
    }

  private def checkUniform(label: String, ranks: Array[Int], draws: Int): Unit =
    val m = draws + 1
    val exact = math.floor(Alpha * m) / m
    val rejections = ranks.count(_.toDouble / m <= Alpha)
    val chi = rankChiSquare(ranks, m, 50)
    println(f"CONDITIONAL_EXACT,$label,R=${ranks.length},B=$draws,rejections=$rejections,chi2_49=$chi%.2f,ks=${rankKs(ranks, m)}%.4f")
    assert(consistentWithRate(rejections, ranks.length, exact, 0.001), s"$label: $rejections/${ranks.length} vs exact $exact")
    assert(chi < ChiSquare49At999, s"$label: chi-square $chi")
    assert(rankKs(ranks, m) <= ksTolerance(ranks.length), s"$label: KS")

  test("conditional exactness at finite nu: KnownVarianceZ with B-fixV has an exactly discrete-uniform rank"):
    checkUniform("n8-I-spread-N8", conditionalRanks("C-n8-DI-Vspread-T0-N8", 1500, 99), 99)
    checkUniform("n20-G-rev-N8", conditionalRanks("C-n20-DG-Vrev-T0-N8", 1000, 99), 99)

  test("B-EB posterior draw: chi at its mean d0 + nu gives sigma*^2 = s~^2; the d0 -> 0 limit gives nu v / chi2_nu"):
    val fit = SmythFit(6.5, 0.4)
    for v <- Vector(0.03, 0.4, 2.7); nu <- Vector(8.0, 40.0) do
      val s2 = fit.posteriorScale(v, nu)
      assertEqualsDouble(fit.posteriorDraw(v, nu, fit.d0 + nu), s2, 2e-16 * s2, s"v=$v nu=$nu")
      assertEqualsDouble(s2, (6.5 * 0.4 + nu * v) / (6.5 + nu), 1e-15 * s2)
      val limit = SmythFit(1e-12, 0.4)
      for chi <- Vector(3.1, 8.0, 17.4) do
        assertEqualsDouble(limit.posteriorDraw(v, nu, chi), nu * v / chi, 1e-10 * nu * v / chi, s"d0->0 v=$v nu=$nu chi=$chi")
    assertEquals(SmythFit(Double.PositiveInfinity, 0.4).posteriorDraw(3.0, 8.0, 5.0), 0.4)
    assertEquals(fit.posteriorDraw(3.0, Double.PositiveInfinity, 5.0), 3.0)

  test("the B-EB engine loop uses the posterior draw: with chi = d0 + nu, B-EB equals a B-plug run on s~^2"):
    val sim = study("C-n20-DG-Vspread-T2-N8", 11)
    val engine = new BootstrapEngine(sim.data.design)
    val fit = value(engine.hyperparameters(sim.data)).get
    assert(fit.d0.isFinite, "needs a finite d0")
    val v = variates(sim, engine, Scheme.EmpiricalBayes, 29)
    val atMean = v.copy(post = Array.tabulate(v.post.length)(k => fit.d0 + sim.data.nu(k % sim.cell.n)))
    val (_, eb) = runSink(sim.data, Scheme.EmpiricalBayes, atMean)
    // B-plug on the same observed data would use v; instead rebuild the world with sigma^2 = s~^2 by hand.
    val scale = Array.tabulate(sim.cell.n)(i => fit.posteriorScale(sim.data.v(i), sim.data.nu(i)))
    val manual = new Array[Double](29)
    val fitter = new StudyFitter(sim.data.design)
    val mean = new Array[Double](sim.cell.n)
    assert(fitter.fitRestricted(sim.data.y, sim.data.v, 0.0, TauPolicy.PauleMandel, mean).ok)
    val tau = math.sqrt(fitter.restricted.tau2)
    (0 until 29).foreach { b =>
      val o = b * sim.cell.n
      val ys = Array.tabulate(sim.cell.n)(i => mean(i) + tau * v.zu(o + i) + math.sqrt(scale(i)) * v.ze(o + i))
      val vs = Array.tabulate(sim.cell.n)(i => scale(i) * v.chi(o + i))
      assert(fitter.fitFull(ys, vs, TauPolicy.PauleMandel).ok)
      manual(b) = fitter.mkhStatistic(0.0)
    }
    (0 until 29).foreach(b => assertEqualsDouble(eb(b), manual(b), 1e-12 * math.max(1.0, math.abs(manual(b)))))

  test("PM NoConvergence is a typed status and a counted draw failure"):
    val interior = (0 until 40).map(i => study("C-n20-DI-Vspread-T2-N8", i)).find { s =>
      val f = new StudyFitter(s.data.design)
      f.fitFull(s.data.y, s.data.v, TauPolicy.PauleMandel).ok && f.full.tau2 > 0.0 && f.full.iterations > 1
    }.get
    val capped = new StudyFitter(interior.data.design, maxIterations = 1)
    assertEquals(capped.fitFull(interior.data.y, interior.data.v, TauPolicy.PauleMandel), FitStatus.NoConvergence)
    val observedFailure = new BootstrapEngine(interior.data.design, maxIterations = 1)
      .run(interior.data, Scheme.Plug, 9, BootstrapEngine.streamVariates(StreamKey(Phase.Harness.root(StreamKind.Bootstrap), interior.cell.id, 1, StreamKind.Bootstrap)))
    assertEquals(observedFailure, Left(StudyFailure.ObservedFit(FitStatus.NoConvergence)))
    assertEquals(StudyVerdict.of(observedFailure, Alpha), StudyVerdict.Failed)
    // Observed and restricted fits on the boundary need no iteration; interior draws then fail and are counted.
    val boundary = (0 until 200).map(i => study("C-n20-DI-Vspread-T0-N8", i)).find { s =>
      val f = new StudyFitter(s.data.design)
      val mean = new Array[Double](s.cell.n)
      f.fitFull(s.data.y, s.data.v, TauPolicy.PauleMandel).ok && f.full.tau2 == 0.0 &&
        f.fitRestricted(s.data.y, s.data.v, 0.0, TauPolicy.PauleMandel, mean).ok && f.restricted.tau2 == 0.0
    }.get
    val engine = new BootstrapEngine(boundary.data.design)
    val v = variates(boundary, engine, Scheme.Plug, 99)
    val capOne = value(new BootstrapEngine(boundary.data.design, maxIterations = 1).run(boundary.data, Scheme.Plug, 99, BootstrapEngine.fixedVariates(v)))
    val free = value(engine.run(boundary.data, Scheme.Plug, 99, BootstrapEngine.fixedVariates(v)))
    assertEquals(free.failed, 0)
    assert(capOne.failed > 0, "some draws have interior roots that need more than one Newton step")
    assertEquals(capOne.draws, 99)
    assertEqualsDouble(capOne.pUpper - capOne.pLower, capOne.failed / 100.0, 1e-15)
