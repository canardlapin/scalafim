package scalafim.group.research.bootstrap

import ResearchTestSupport.*

/** Declaration §7.6: only controls whose direction is fixed by an analytic argument
  * are discriminating. The unrestricted uncentred bootstrap is the one such control
  * (T* is centred at T), and it must fail exactly as predicted. The other controls
  * are frozen-direction hypotheses: they are wired and checked here; their rates
  * belong to the pilot (descriptive), which this harness does not run.
  */
class ControlsSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  val Alpha = 0.05

  private def rejections(cellId: String, purpose: StudyPurpose, studies: Int, draws: Int, schemes: Vector[Scheme]): Map[Scheme, Int] =
    val c = cell(cellId)
    val engine = new BootstrapEngine(c.researchDesign)
    val counts = scala.collection.mutable.Map.from(schemes.map(_ -> 0))
    (0 until studies).foreach { r =>
      val sim = ModelJ.draw(c, Phase.Harness, purpose, r, engine.design)
      val key = StreamKey.of(Phase.Harness, purpose, c.id, r, StreamKind.Bootstrap)
      schemes.foreach { s =>
        val rejected = engine.run(sim.data, s, draws, BootstrapEngine.streamVariates(key)) match
          case Right(o) =>
            purpose match
              case StudyPurpose.Null => o.decision(Alpha) != StudyDecision.Retain
              case StudyPurpose.Power => o.decision(Alpha) == StudyDecision.Reject
          case Left(_) => purpose == StudyPurpose.Null
        if rejected then counts(s) += 1
      }
    }
    counts.toMap

  test("discriminating control: the unrestricted uncentred bootstrap is conservative at the null and loses its power"):
    val schemes = Vector(Scheme.Plug, Scheme.UnrestrictedUncentred)
    val studies = 150
    val nul = rejections("C-n20-DI-Vflat-T0-N8", StudyPurpose.Null, studies, 99, schemes)
    val pow = rejections("C-n20-DI-Vflat-T0-N8", StudyPurpose.Power, studies, 99, schemes)
    println(s"CONTROL_UNCENTRED,null=${nul(Scheme.UnrestrictedUncentred)}/$studies (B-plug ${nul(Scheme.Plug)}),power=${pow(Scheme.UnrestrictedUncentred)}/$studies (B-plug ${pow(Scheme.Plug)})")
    // Analytic: |T*| >= |T| holds for about half the draws, so p is rarely <= .05.
    assert(nul(Scheme.UnrestrictedUncentred) <= 2, "the uncentred control must (almost) never reject under the null")
    assert(pow(Scheme.UnrestrictedUncentred) <= 3, "the uncentred control's power must collapse")
    assert(pow(Scheme.Plug) >= 30, "the restricted candidate keeps power at the oracle-z 50% alternative")
    assert(pow(Scheme.Plug) - pow(Scheme.UnrestrictedUncentred) >= 25)

  test("discriminating control holds for a covariate design with heterogeneity too"):
    val schemes = Vector(Scheme.Plug, Scheme.UnrestrictedUncentred)
    val pow = rejections("C-n20-DG-Vspread-T2-N40", StudyPurpose.Power, 100, 99, schemes)
    println(s"CONTROL_UNCENTRED_G,power=${pow(Scheme.UnrestrictedUncentred)}/100 (B-plug ${pow(Scheme.Plug)})")
    assert(pow(Scheme.UnrestrictedUncentred) <= 3)
    assert(pow(Scheme.Plug) >= 15)

  test("hypothesis controls are wired: omit-u drops u*, nu-swap redraws chi at n - p, known-v freezes tau^2"):
    val sim = study("C-n20-DG-Vspread-T2-N8", 11)
    val engine = new BootstrapEngine(sim.data.design)
    val draws = 19
    def sink(s: Scheme): (BootstrapOutcome, Array[Double]) =
      val out = new Array[Double](draws)
      val v = variates(sim, engine, s, draws)
      (value(engine.run(sim.data, s, draws, BootstrapEngine.fixedVariates(v), sink = Some(out))), out)
    val (plug, plugT) = sink(Scheme.Plug)
    val (omit, omitT) = sink(Scheme.OmitU)
    val (swap, swapT) = sink(Scheme.NuSwap)
    val (frozen, frozenT) = sink(Scheme.KnownVFrozenTau)
    val (fixv, fixvT) = sink(Scheme.FixV)
    assert(plug.tau2World > 0.0, "the check needs a positive restricted tau^2")
    assertEquals(omit.tau2World, plug.tau2World)
    assert(!plugT.sameElements(omitT) && !plugT.sameElements(swapT) && !fixvT.sameElements(frozenT))
    assertEquals(Vector(plug, omit, swap, frozen, fixv).map(_.statistic).distinct.length, 1)
    assertEquals(engine.chiDf(sim.data, Scheme.NuSwap).toVector, Vector.fill(sim.cell.n)(sim.data.design.residualDf.toDouble))

  test("B-EB degenerates correctly: known variances give B-fixV draws; d0 = inf gives sigma*^2 = s0^2"):
    val known = study("C-n8-DI-Vspread-T2-Ninf", 4)
    val engine = new BootstrapEngine(known.data.design)
    val v = variates(known, engine, Scheme.EmpiricalBayes, 29)
    val a = new Array[Double](29)
    val b = new Array[Double](29)
    val eb = value(engine.run(known.data, Scheme.EmpiricalBayes, 29, BootstrapEngine.fixedVariates(v), sink = Some(a)))
    value(engine.run(known.data, Scheme.FixV, 29, BootstrapEngine.fixedVariates(v), sink = Some(b)))
    assertEquals(eb.smyth, None)
    assertEquals(a.toVector, b.toVector)
    val flat = SmythFit(Double.PositiveInfinity, 0.5)
    assertEquals(flat.posteriorScale(3.0, 8.0), 0.5)
    val finite = SmythFit(10.0, 0.5)
    assertEqualsDouble(finite.posteriorScale(3.0, 8.0), (10.0 * 0.5 + 8.0 * 3.0) / 18.0, 1e-15)

  test("the B-fixV known-truth control stratifies by terciles of mean log(v/sigma^2)"):
    val c = cell("C-n8-DI-Vflat-T0-N8")
    val engine = new BootstrapEngine(c.researchDesign)
    val records = (0 until 30).toVector.map(r => StudyRunner.run(c, Phase.Harness, StudyPurpose.Null, r, 19, Vector(Scheme.FixV), engine))
    val terciles = FixVTerciles.tabulate(records, Scheme.FixV, Alpha)
    assertEquals(terciles.map(_.studies), Vector(10, 10, 10))
    assert(terciles(0).upper <= terciles(1).lower && terciles(1).upper <= terciles(2).lower)

  test("a study record carries every scheme, the native and HC3 baselines, the oracle z and the sign-flip p"):
    Vector("C-n8-DI-Vspread-T2-N8", "C-n20-DG-Vrev-T0-N8", "H-n8-DI-Vsichi-T2-N8", "S-ar1-n20-DI-Vflat-T0-N8").foreach { id =>
      val c = cell(id)
      val engine = new BootstrapEngine(c.researchDesign)
      val record = StudyRunner.run(c, Phase.Harness, StudyPurpose.Null, 0, 19, Scheme.values.toVector, engine)
      assertEquals(record.schemes.length, Scheme.values.length)
      assert(record.schemes.forall(_._2.isRight), id)
      assert(record.native.isRight && record.hc3Equal.isRight && record.hc3InverseV.isRight && record.oracle.isRight, id)
      assertEquals(record.signFlip.isDefined, c.p == 1)
      record.signFlip.foreach(p => assert(p > 0.0 && p <= 1.0))
    }
