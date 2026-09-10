package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.JetLayout
import scalafim.fmri.model.ProfileCriterion

class ProfileReductionSuite extends munit.FunSuite:

  private val d = 2
  private val c = 3
  private val comps = JetLayout.components(d)

  /** A synthetic smooth `(s, b, G)(theta)` with analytic jets, and its exact energy. */
  private object Synthetic:
    private val rng = new scala.util.Random(3L)
    private val base = Array.fill(c * c)(rng.nextGaussian())
    private val gram0 = Array.tabulate(c * c) { idx =>
      val i = idx / c; val j = idx % c
      var acc = 0.0
      var k = 0
      while k < c do
        acc += base(k * c + i) * base(k * c + j)
        k += 1
      acc + (if i == j then 2.0 else 0.0)
    }
    private val b0 = Array.fill(c)(rng.nextGaussian())
    private val b1 = Array.fill(c)(rng.nextGaussian())
    private val b2 = Array.fill(c)(rng.nextGaussian())
    private val g1 = Array.fill(c * c)(0.0)
    locally {
      var i = 0
      while i < c do
        g1(i * c + i) = 0.3 + 0.1 * i
        i += 1
    }

    // s(theta) = 5 + 0.7 t1 + 0.4 t2^2 + 0.2 t1 t2 ; b(theta) = b0 + t1 b1 + sin(t2) b2 ;
    // G(theta) = gram0 + t1^2 g1 (diagonal, keeps G SPD near the origin)
    def energy(t1: Double, t2: Double): Double =
      val s = 5.0 + 0.7 * t1 + 0.4 * t2 * t2 + 0.2 * t1 * t2
      val b = Array.tabulate(c)(i => b0(i) + t1 * b1(i) + math.sin(t2) * b2(i))
      val g = Array.tabulate(c * c)(i => gram0(i) + t1 * t1 * g1(i))
      val l = java.util.Arrays.copyOf(g, c * c)
      assert(SmallCholesky.factorInPlace(c, l))
      val w = java.util.Arrays.copyOf(b, c)
      SmallCholesky.solveInPlace(c, l, w)
      s - (0 until c).map(i => b(i) * w(i)).sum

    def jets(t1: Double, t2: Double, withEnergyDerivatives: Boolean): (Array[Double], Array[Double], Array[Double]) =
      val s = new Array[Double](comps)
      s(JetLayout.Value) = 5.0 + 0.7 * t1 + 0.4 * t2 * t2 + 0.2 * t1 * t2
      if withEnergyDerivatives then
        s(JetLayout.first(0)) = 0.7 + 0.2 * t2
        s(JetLayout.first(1)) = 0.8 * t2 + 0.2 * t1
        s(JetLayout.second(d, 0, 1)) = 0.2
        s(JetLayout.second(d, 1, 1)) = 0.8
      val b = new Array[Double](comps * c)
      val g = new Array[Double](comps * c * c)
      var i = 0
      while i < c do
        b(JetLayout.Value * c + i) = b0(i) + t1 * b1(i) + math.sin(t2) * b2(i)
        b(JetLayout.first(0) * c + i) = b1(i)
        b(JetLayout.first(1) * c + i) = math.cos(t2) * b2(i)
        b(JetLayout.second(d, 1, 1) * c + i) = -math.sin(t2) * b2(i)
        i += 1
      i = 0
      while i < c * c do
        g(JetLayout.Value * c * c + i) = gram0(i) + t1 * t1 * g1(i)
        g(JetLayout.first(0) * c * c + i) = 2.0 * t1 * g1(i)
        g(JetLayout.second(d, 0, 0) * c * c + i) = 2.0 * g1(i)
        i += 1
      (s, b, g)

  private def fdGradient(f: (Double, Double) => Double, t1: Double, t2: Double, h: Double): (Double, Double) =
    ((f(t1 + h, t2) - f(t1 - h, t2)) / (2 * h), (f(t1, t2 + h) - f(t1, t2 - h)) / (2 * h))

  private def fdHessian(f: (Double, Double) => Double, t1: Double, t2: Double, h: Double): Array[Double] =
    val f0 = f(t1, t2)
    val h11 = (f(t1 + h, t2) - 2 * f0 + f(t1 - h, t2)) / (h * h)
    val h22 = (f(t1, t2 + h) - 2 * f0 + f(t1, t2 - h)) / (h * h)
    val h12 = (f(t1 + h, t2 + h) - f(t1 + h, t2 - h) - f(t1 - h, t2 + h) + f(t1 - h, t2 - h)) / (4 * h * h)
    Array(h11, h12, h12, h22)

  test("the reduction matches finite differences of the exact energy, including energy-term derivatives"):
    val reduction = new ProfileReduction(d, c)
    val out = new ProfileJetBuffer(d, c)
    for (t1, t2) <- Seq((0.3, -0.4), (-0.6, 0.9), (0.1, 0.2)) do
      val (s, b, g) = Synthetic.jets(t1, t2, withEnergyDerivatives = true)
      assert(reduction.reduce(s, b, g, out))
      assertEqualsDouble(out.energy, Synthetic.energy(t1, t2), 1e-12)
      val (g1, g2) = fdGradient(Synthetic.energy, t1, t2, 1e-5)
      assertEqualsDouble(out.gradient(0), g1, 1e-7, s"dE/dt1 at ($t1,$t2)")
      assertEqualsDouble(out.gradient(1), g2, 1e-7, s"dE/dt2 at ($t1,$t2)")
      val h = fdHessian(Synthetic.energy, t1, t2, 1e-3)
      var i = 0
      while i < 4 do
        assertEqualsDouble(out.hessian(i), h(i), 1e-5, s"Hessian entry $i at ($t1,$t2)")
        i += 1

  test("omitting the energy-term derivatives is detected"):
    val reduction = new ProfileReduction(d, c)
    val out = new ProfileJetBuffer(d, c)
    val (s, b, g) = Synthetic.jets(0.3, -0.4, withEnergyDerivatives = false)
    assert(reduction.reduce(s, b, g, out))
    val (g1, _) = fdGradient(Synthetic.energy, 0.3, -0.4, 1e-5)
    assert(math.abs(out.gradient(0) - g1) > 0.1, "a shape-dependent energy term with zero derivatives must not pass")

  test("a non-positive-definite Gram is reported, not inverted"):
    val reduction = new ProfileReduction(d, c)
    val out = new ProfileJetBuffer(d, c)
    val s = new Array[Double](comps)
    val b = new Array[Double](comps * c)
    val g = new Array[Double](comps * c * c) // zero Gram
    assert(!reduction.reduce(s, b, g, out))
    assertEquals(out.curvature, CurvatureStatus.GramNotPositiveDefinite)
    assert(out.energy.isPosInfinity)

  test("compact condition jets agree with finite differences of the direct compact energy and are sign-invariant"):
    val rng = new scala.util.Random(9L)
    val k = 12
    val m = 4
    val rHat = Array.fill(k * c * m)(rng.nextGaussian())
    val z = Array.fill(k)(rng.nextGaussian())
    val e = z.map(x => x * x).sum + 1.0
    // c_j(theta) = cos(a_j t1) + b_j t2^2 with analytic jets
    val a = Array.tabulate(m)(j => 0.5 + 0.3 * j)
    val bb = Array.tabulate(m)(j => 0.2 * (j + 1))
    def coefficientJet(t1: Double, t2: Double): Array[Double] =
      val out = new Array[Double](comps * m)
      var j = 0
      while j < m do
        out(JetLayout.Value * m + j) = math.cos(a(j) * t1) + bb(j) * t2 * t2
        out(JetLayout.first(0) * m + j) = -a(j) * math.sin(a(j) * t1)
        out(JetLayout.first(1) * m + j) = 2.0 * bb(j) * t2
        out(JetLayout.second(d, 0, 0) * m + j) = -a(j) * a(j) * math.cos(a(j) * t1)
        out(JetLayout.second(d, 1, 1) * m + j) = 2.0 * bb(j)
        j += 1
      out
    val jets = new CompactConditionJets(rHat, k, c, m, d)
    val reduction = new ProfileReduction(d, c)
    val out = new ProfileJetBuffer(d, c)
    def energyAt(t1: Double, t2: Double, response: Array[Double]): Double =
      jets.assemble(response, e, coefficientJet(t1, t2), 1)
      assert(reduction.reduce(jets.s, jets.b, jets.g, out))
      out.energy
    for (t1, t2) <- Seq((0.4, 0.3), (-0.7, 0.5)) do
      jets.assemble(z, e, coefficientJet(t1, t2), comps)
      assert(reduction.reduce(jets.s, jets.b, jets.g, out))
      val energy = out.energy
      val grad = out.gradient.clone()
      val hess = out.hessian.clone()
      val amplitudes = out.amplitudes.clone()
      val (g1, g2) = fdGradient((x, y) => energyAt(x, y, z), t1, t2, 1e-5)
      assertEqualsDouble(grad(0), g1, 1e-6)
      assertEqualsDouble(grad(1), g2, 1e-6)
      val h = fdHessian((x, y) => energyAt(x, y, z), t1, t2, 1e-3)
      var i = 0
      while i < 4 do
        assertEqualsDouble(hess(i), h(i), 1e-4, s"Hessian $i")
        i += 1
      // sign reversal of the response leaves shape evidence unchanged and negates amplitudes
      val negated = z.map(-_)
      jets.assemble(negated, e, coefficientJet(t1, t2), comps)
      assert(reduction.reduce(jets.s, jets.b, jets.g, out))
      assertEqualsDouble(out.energy, energy, 1e-12)
      i = 0
      while i < 4 do
        assertEqualsDouble(out.hessian(i), hess(i), 1e-10)
        i += 1
      i = 0
      while i < c do
        assertEqualsDouble(out.amplitudes(i), -amplitudes(i), 1e-12)
        i += 1

  test("criterion assembly scales the energy jet and demands a determinant for trial ML"):
    val jet = ProfileJet(4.0, Vector(1.0, -2.0), Vector(2.0, 0.5, 0.5, 3.0), Vector(1.0, 2.0, 3.0), CurvatureStatus.PositiveDefinite)
    val penalised = CriterionJet.assemble(ProfileCriterion.PenalizedProfile(2.0), jet, None).fold(e => fail(e.message), identity)
    assertEqualsDouble(penalised.score, -1.0, 1e-15)
    assertEquals(penalised.gradient, Vector(-0.25, 0.5))
    assertEquals(penalised.hessian, Vector(-0.5, -0.125, -0.125, -0.75))
    assert(CriterionJet.assemble(ProfileCriterion.TrialRandomEffectsML(2.0), jet, None).isLeft)
    assert(CriterionJet.assemble(ProfileCriterion.PenalizedProfile(0.0), jet, None).isLeft)
    val logDet = ProfileJet(2.0, Vector(0.2, 0.0), Vector(0.0, 0.0, 0.0, 0.0), Vector.empty, CurvatureStatus.PositiveDefinite)
    val ml = CriterionJet.assemble(ProfileCriterion.TrialRandomEffectsML(2.0), jet, Some(logDet)).fold(e => fail(e.message), identity)
    assertEqualsDouble(ml.score, -1.0 - 1.0, 1e-15)
    assertEqualsDouble(ml.gradient(0), -0.25 - 0.1, 1e-15)
