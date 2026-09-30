package scalafim.group.research.bootstrap

import ResearchTestSupport.*

class PmKernelSuite extends munit.FunSuite:
  /** epsilon_q from the native 1e-9 relative root tolerance: |Q/(n-p) - 1| <= 1e-9 at a native root. */
  val NativeEpsilonQ = 1e-9

  private def interiorAndBoundary: (SimulatedStudy, SimulatedStudy) =
    val sims = (0 until 40).map(i => study("C-n20-DG-Vspread-T2-N8", i))
    val fits = sims.map { s =>
      val f = new StudyFitter(s.data.design)
      assert(f.fitFull(s.data.y, s.data.v, TauPolicy.PauleMandel).ok)
      s -> f.full.tau2
    }
    (fits.find(_._2 > 0.0).get._1, fits.find(_._2 == 0.0).get._1)

  test("PM identity: q = 1 within epsilon at an interior root, q <= 1 at the boundary, so max(1, q) = 1"):
    val (interior, boundary) = interiorAndBoundary
    val f = new StudyFitter(interior.data.design)
    assert(f.fitFull(interior.data.y, interior.data.v, TauPolicy.PauleMandel).ok)
    val df = interior.data.design.residualDf
    assert(f.full.tau2 > 0.0)
    assertEqualsDouble(f.full.qStatistic / df, 1.0, f.full.RootTolerance * 4)
    assertEqualsDouble(f.mkhScale, 1.0, f.full.RootTolerance * 4)
    val g = new StudyFitter(boundary.data.design)
    assert(g.fitFull(boundary.data.y, boundary.data.v, TauPolicy.PauleMandel).ok)
    assertEquals(g.full.tau2, 0.0)
    assert(g.full.qStatistic / df <= 1.0)
    assertEquals(g.mkhScale, 1.0)

  test("the harness PM/mKH statistic matches the native modules/group fit on one study of every core cell"):
    var interior = 0
    Cell.core.foreach { c =>
      val sim = study(c.id.value, 0)
      val d = sim.data.design
      val f = new StudyFitter(d)
      assert(f.fitFull(sim.data.y, sim.data.v, TauPolicy.PauleMandel).ok, c.id.value)
      if f.full.tau2 > 0.0 then interior += 1
      val t = f.mkhStatistic(0.0)
      val native = value(NativeBaseline.pmMkh(d, Vector(sim.data.y), Vector(sim.data.v), 0.0)).head
      assert(!native.failed, c.id.value)
      // Both roots stop within their tolerances (native 1e-9 relative on Q); declaration §7.5 bound 1e-7.
      assertEqualsDouble(t, native.statistic, 1e-7 * math.max(1.0, math.abs(t)), c.id.value)
    }
    assert(interior >= 20, s"the parity sweep must include interior roots, got $interior")

  test("the native mKH statistic equals the unscaled Wald statistic at an interior root (max(1, q) = 1)"):
    val (interior, _) = interiorAndBoundary
    val d = interior.data.design
    val f = new StudyFitter(d)
    assert(f.fitFull(interior.data.y, interior.data.v, TauPolicy.PauleMandel).ok)
    val native = value(NativeBaseline.pmMkh(d, Vector(interior.data.y), Vector(interior.data.v), 0.0)).head
    // Native T = T_wald / sqrt(max(1, q_native)). The native root has |q - 1| <= epsilon_q = 1e-9, and the two
    // roots differ within their tolerances, so the implied scale is 1 within the §7.5 bound of 1e-7.
    val impliedScale = math.pow(f.waldStatistic(0.0) / native.statistic, 2)
    assert(math.abs(impliedScale - 1.0) <= 1e-7, s"implied native scale $impliedScale")
    assert(math.abs(f.full.qStatistic / d.residualDf - 1.0) <= NativeEpsilonQ, "harness q is inside epsilon_q")

  test("restricted PM solves Q0(tau^2) = n - p0 on the null-space design and satisfies c'beta0 = b0"):
    val sims = (0 until 30).map(i => study("C-n20-DG-Vflat-T2-N40", i))
    var interior = 0
    sims.foreach { s =>
      val d = s.data.design
      val f = new StudyFitter(d)
      val mean = new Array[Double](d.n)
      val b0 = 0.3
      assert(f.fitRestricted(s.data.y, s.data.v, b0, TauPolicy.PauleMandel, mean).ok)
      val beta0 = f.restrictedCoefficients(b0)
      assertEqualsDouble(d.contrast.zip(beta0).map(_ * _).sum, b0, 1e-14)
      val fitted = Array.tabulate(d.n)(i => (0 until d.p).map(j => d.row(i, j) * beta0(j)).sum)
      fitted.indices.foreach(i => assertEqualsDouble(fitted(i), mean(i), 1e-12))
      val t2 = f.restricted.tau2
      val q0 = (0 until d.n).map(i => math.pow(s.data.y(i) - mean(i), 2) / (s.data.v(i) + t2)).sum
      if t2 > 0.0 then
        interior += 1
        assertEqualsDouble(q0 / d.restrictedDf, 1.0, 1e-11)
      else assert(q0 <= d.restrictedDf)
    }
    assert(interior > 5)

  test("intercept-only restricted fit (p0 = 0): r0 = y - b0 and Q0 targets n"):
    val s = study("C-n8-DI-Vspread-T2-N8", 2)
    val d = s.data.design
    assertEquals(d.p0, 0)
    val f = new StudyFitter(d)
    val mean = new Array[Double](d.n)
    assert(f.fitRestricted(s.data.y, s.data.v, 0.4, TauPolicy.PauleMandel, mean).ok)
    mean.foreach(m => assertEqualsDouble(m, 0.4, 1e-15))
    (0 until d.n).foreach(i => assertEqualsDouble(f.restricted.residual(i), s.data.y(i) - 0.4, 1e-15))
    val q0 = (0 until d.n).map(i => math.pow(s.data.y(i) - 0.4, 2) / (s.data.v(i) + f.restricted.tau2)).sum
    if f.restricted.tau2 > 0.0 then assertEqualsDouble(q0, d.n.toDouble, 1e-10) else assert(q0 <= d.n)

  test("restricted fits agree with the explicit R uniroot reference"):
    BootstrapReferenceFixtures.Cases.foreach { c =>
      val d = value(ResearchDesign.of(c.n, c.p, c.designRowMajor, c.terms, c.contrast))
      val f = new StudyFitter(d)
      val mean = new Array[Double](c.n)
      assert(f.fitRestricted(c.y, c.v, c.b0, TauPolicy.PauleMandel, mean).ok, c.id)
      assertEqualsDouble(f.restricted.tau2, c.restrictedTau2, 1e-9, s"${c.id} restricted tau2")
      mean.indices.foreach(i => assertEqualsDouble(mean(i), c.restrictedMean(i), 1e-9 * math.max(1.0, math.abs(mean(i))), s"${c.id} mean $i"))
    }

  test("fit failures are typed statuses: non-positive or non-finite variances, singular weighted design"):
    val d = value(ResearchDesign.of(4, 1, Array.fill(4)(1.0), Vector("intercept"), Array(1.0)))
    val f = new StudyFitter(d)
    assertEquals(f.fitFull(Array(1.0, 2.0, 3.0, 4.0), Array(1.0, 0.0, 1.0, 1.0), TauPolicy.PauleMandel), FitStatus.NonPositiveVariance)
    assertEquals(f.fitFull(Array(1.0, Double.NaN, 3.0, 4.0), Array(1.0, 1.0, 1.0, 1.0), TauPolicy.PauleMandel), FitStatus.NonFiniteInput)
    val x = Array(1.0, 0.0, 1.0, 0.0, 1.0, 1.0, 1.0, 1.0)
    val g = value(ResearchDesign.of(4, 2, x, Vector("a", "b"), Array(0.0, 1.0)))
    val tiny = new StudyFitter(g)
    // The second column is carried only by subjects whose weights underflow the Cholesky tolerance.
    assertEquals(tiny.fitFull(Array(1.0, 2.0, 3.0, 4.0), Array(1.0, 1.0, 1e300, 1e300), TauPolicy.Fixed(0.0)), FitStatus.SingularWeightedDesign)
    assertEquals(ResearchDesign.of(4, 2, Array(1.0, 2.0, 1.0, 2.0, 1.0, 2.0, 1.0, 2.0), Vector("a", "b"), Array(1.0, 0.0)), Left(DesignError.RankDeficient))
