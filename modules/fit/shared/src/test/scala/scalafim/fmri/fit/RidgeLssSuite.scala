package scalafim.fmri.fit

import gale.linalg.{DMat, QROptions, QRPivoting}

/**
  * rLSS against an independent dense penalised solve (augmented least squares, one trial at a time), against LSS
  * (pooled grouping at ridge 0), and against its two analytic limits. Pure Gale, shared between JVM and JS.
  */
class RidgeLssSuite extends munit.FunSuite:

  private final class Lcg(seed: Long):
    private var s = seed
    def uniform(): Double =
      s = s * 6364136223846793005L + 1442695040888963407L
      (s >>> 11).toDouble / (1L << 53).toDouble
    def normal(): Double = uniform() + uniform() + uniform() + uniform() - 2.0

  private val Rows = 96
  private val Trials = 12
  private val Voxels = 4

  private final case class Fixture(x: DMat, f: DMat, y: DMat, groups: Vector[Int])

  private def bump(lag: Double): Double =
    if lag < 0.0 || lag >= 24.0 then 0.0 else lag * lag * math.exp(-lag / 3.0) / 10.0

  private def fixture(seed: Long): Fixture =
    val rng = new Lcg(seed)
    val onsets = Vector.tabulate(Trials)(i => 3.0 + 7.0 * i + 3.0 * rng.uniform())
    val x = DMat.tabulate(Rows, Trials)((t, i) => bump(t - onsets(i)))
    val f = DMat.tabulate(Rows, 3)((t, c) =>
      c match
        case 0 => 1.0
        case 1 => (t - Rows / 2.0) / Rows
        case _ => math.cos(0.21 * t))
    val amp = DMat.tabulate(Trials, Voxels)((i, v) => (i % 3) * 1.5 + v + 0.8 * rng.normal())
    val signal = x * amp
    val y = DMat.tabulate(Rows, Voxels)((t, v) => signal(t, v) + 0.4 * rng.normal() + 0.3 * f(t, 1) * (v + 1))
    Fixture(x, f, y, Vector.tabulate(Trials)(_ % 3))

  private def prepared(fx: Fixture, grouping: RidgeLssGrouping): RidgeLssPrepared =
    RidgeLeastSquaresSeparate
      .prepare(LssTrialDesign.unsafe(fx.x), LssFixedDesign.unsafe(fx.f), grouping)
      .fold(e => fail(e.message), identity)

  private def relErr(a: DMat, b: DMat): Double =
    assertEquals((a.rows, a.cols), (b.rows, b.cols))
    var diff = 0.0
    var scale = 0.0
    var r = 0
    while r < a.rows do
      var c = 0
      while c < a.cols do
        diff = math.max(diff, math.abs(a(r, c) - b(r, c)))
        scale = math.max(scale, math.abs(b(r, c)))
        c += 1
      r += 1
    diff / scale

  /** Dense oracle: for each trial, least squares on `[D; sqrt(r) e_delta]` against `[Y; 0]`, `D = [X_g, x_i, F]`. */
  private def dense(fx: Fixture, groups: Vector[Int], ridge: Double): (DMat, DMat) =
    val g = groups.max + 1
    val amp = Array.ofDim[Double](Trials, Voxels)
    val dev = Array.ofDim[Double](Trials, Voxels)
    var i = 0
    while i < Trials do
      val cols = g + 1 + fx.f.cols
      val d = DMat.tabulate(Rows + 1, cols): (t, c) =>
        if t == Rows then (if c == g then math.sqrt(ridge) else 0.0)
        else if c < g then (0 until Trials).filter(groups(_) == c).map(j => fx.x(t, j)).sum
        else if c == g then fx.x(t, i)
        else fx.f(t, c - g - 1)
      val rhs = DMat.tabulate(Rows + 1, Voxels)((t, v) => if t == Rows then 0.0 else fx.y(t, v))
      val beta = d.qr(QROptions(QRPivoting.Column, Some(1e-12))).solveLeastSquares(rhs).fold(e => fail(e.toString), identity)
      var v = 0
      while v < Voxels do
        amp(i)(v) = beta(groups(i), v) + beta(g, v)
        dev(i)(v) = beta(g, v)
        v += 1
      i += 1
    (DMat.tabulate(Trials, Voxels)((a, b) => amp(a)(b)), DMat.tabulate(Trials, Voxels)((a, b) => dev(a)(b)))

  private def project(p: RidgeLssPrepared, fx: Fixture): RidgeLssProjection =
    p.project(ResponseBlock.unsafe(fx.y)).fold(e => fail(e.message), identity)

  private def fitAt(proj: RidgeLssProjection, ridge: Double): RidgeLssFit =
    proj.solve(ridge).fold(e => fail(e.message), identity)

  test("rLSS closed form equals the dense penalised solve at 1e-10 across the ridge range"):
    val fx = fixture(11L)
    val p = prepared(fx, RidgeLssGrouping.ByGroup(fx.groups))
    val proj = project(p, fx)
    val meanQ = p.q.sum / Trials
    var worst = 0.0
    for mult <- Vector(0.0, 1e-2, 1e-1, 1.0, 10.0, 100.0, 1e6) do
      val r = mult * meanQ
      val got = fitAt(proj, r)
      val (amp, dev) = dense(fx, fx.groups, r)
      val e = math.max(relErr(got.amplitudes.value, amp), relErr(got.deviations.value, dev))
      worst = math.max(worst, e)
      assert(e <= 1e-10, s"rLSS vs dense at r = $mult * mean q: $e")
    println(f"RIDGELSS-DENSE worst relative error $worst%.3e")

  test("q_i equals the residual of x_i on [X_g, F] (dense projection)"):
    val fx = fixture(12L)
    val p = prepared(fx, RidgeLssGrouping.ByGroup(fx.groups))
    val g = 3
    val z = DMat.tabulate(Rows, g + 3)((t, c) =>
      if c < g then (0 until Trials).filter(fx.groups(_) == c).map(j => fx.x(t, j)).sum else fx.f(t, c - g))
    val qr = z.qr(QROptions(QRPivoting.Column, Some(1e-12)))
    val res = qr.residualize(fx.x).fold(e => fail(e.toString), identity)
    var i = 0
    while i < Trials do
      val expected = (0 until Rows).map(t => res(t, i) * res(t, i)).sum
      assertEqualsDouble(p.q(i), expected, 1e-10 * math.max(1.0, expected))
      i += 1

  test("pooled grouping at ridge 0 reproduces LSS (the LSS-equivalence flag)"):
    val fx = fixture(13L)
    val proj = project(prepared(fx, RidgeLssGrouping.Pooled), fx)
    val got = fitAt(proj, 0.0)
    val lss = LeastSquaresSeparate
      .fit(LssTrialDesign.unsafe(fx.x), ResponseBlock.unsafe(fx.y), LssFixedDesign.unsafe(fx.f), LssOptions())
      .fold(e => fail(e.message), identity)
    val e = relErr(got.amplitudes.value, lss.coefficients.value)
    println(f"RIDGELSS-LSS-EQUIVALENCE relative error $e%.3e")
    assert(e <= 1e-10, s"pooled rLSS at 0 vs LSS: $e")

  test("ridge to 0 recovers LSS (pooled), and the condition-grouped form differs from it"):
    val fx = fixture(14L)
    val pooled = prepared(fx, RidgeLssGrouping.Pooled)
    val tiny = fitAt(project(pooled, fx), 1e-9 * pooled.q.sum / Trials)
    val lss = LeastSquaresSeparate
      .fit(LssTrialDesign.unsafe(fx.x), ResponseBlock.unsafe(fx.y), LssFixedDesign.unsafe(fx.f), LssOptions())
      .fold(e => fail(e.message), identity)
    assert(relErr(tiny.amplitudes.value, lss.coefficients.value) < 1e-8)
    val grouped = fitAt(project(prepared(fx, RidgeLssGrouping.ByGroup(fx.groups)), fx), 0.0)
    assert(relErr(grouped.amplitudes.value, lss.coefficients.value) > 1e-3, "grouped and pooled forms must differ")

  test("ridge to infinity gives the condition-mean fit; deviation norm is strictly decreasing in the ridge"):
    val fx = fixture(15L)
    val p = prepared(fx, RidgeLssGrouping.ByGroup(fx.groups))
    val proj = project(p, fx)
    val meanQ = p.q.sum / Trials
    val big = fitAt(proj, 1e14 * meanQ)
    val g = 3
    val z = DMat.tabulate(Rows, g + 3)((t, c) =>
      if c < g then (0 until Trials).filter(fx.groups(_) == c).map(j => fx.x(t, j)).sum else fx.f(t, c - g))
    val theta = z.qr(QROptions(QRPivoting.Column, Some(1e-12))).solveLeastSquares(fx.y).fold(e => fail(e.toString), identity)
    val target = DMat.tabulate(Trials, Voxels)((i, v) => theta(fx.groups(i), v))
    assert(relErr(big.amplitudes.value, target) < 1e-9)
    val norms = Vector(0.0, 0.01, 0.1, 1.0, 10.0, 100.0).map { m =>
      val d = fitAt(proj, m * meanQ).deviations.value
      math.sqrt((for i <- 0 until Trials; v <- 0 until Voxels yield d(i, v) * d(i, v)).sum)
    }
    assert(norms.zip(norms.tail).forall((a, b) => a > b), s"deviation norms $norms")

  test("information ratio is monotone decreasing from 1 toward 0 and equals the closed form on a disjoint-support design"):
    // Disjoint supports, no F, two groups: x~_i = x_i - X_g |x_i|^2 / S_g, so q_i = d_i^2 (1 - d_i^2 / S_g) exactly.
    val d = Vector(1.0, 2.0, 1.5, 0.5, 3.0, 1.0)
    val groups = Vector(0, 0, 0, 1, 1, 1)
    val x = DMat.tabulate(d.length * 3, d.length)((t, i) => if t / 3 == i then d(i) * (1.0 + 0.1 * (t % 3)) else 0.0)
    val norm2 = Vector.tabulate(d.length)(i => (0 until x.rows).map(t => x(t, i) * x(t, i)).sum)
    val s = Vector(0, 1).map(g => (0 until d.length).filter(groups(_) == g).map(norm2).sum)
    val p = RidgeLeastSquaresSeparate
      .prepare(LssTrialDesign.unsafe(x), LssFixedDesign.empty(x.rows), RidgeLssGrouping.ByGroup(groups))
      .fold(e => fail(e.message), identity)
    val qExpected = Vector.tabulate(d.length)(i => norm2(i) * (1.0 - norm2(i) / s(groups(i))))
    p.q.zip(qExpected).foreach((a, b) => assertEqualsDouble(a, b, 1e-12 * math.max(1.0, b)))
    val penalties = Vector(0.0, 1e-3, 0.1, 1.0, 10.0, 1e3, 1e9)
    val edf = penalties.map(p.informationRatio)
    assertEqualsDouble(edf.head, 1.0, 1e-15)
    assert(edf.zip(edf.tail).forall((a, b) => a > b), s"edf not strictly decreasing: $edf")
    penalties.zip(edf).foreach((pen, e) =>
      assertEqualsDouble(e, qExpected.map(q => q / (q + pen)).sum / d.length, 1e-14))

  test("typed refusals: grouping, rows, ridge, rank and non-estimable trials"):
    val fx = fixture(16L)
    def prep(grouping: RidgeLssGrouping) =
      RidgeLeastSquaresSeparate.prepare(LssTrialDesign.unsafe(fx.x), LssFixedDesign.unsafe(fx.f), grouping)
    assert(prep(RidgeLssGrouping.ByGroup(Vector(0, 1))).isLeft)
    assert(prep(RidgeLssGrouping.ByGroup(Vector.fill(Trials)(2))).isLeft, "non-dense ids")
    assert(prep(RidgeLssGrouping.ByGroup(Vector.fill(Trials)(-1))).isLeft)
    val p = prepared(fx, RidgeLssGrouping.ByGroup(fx.groups))
    assert(p.project(ResponseBlock.unsafe(DMat.zeros(Rows - 1, 2))).isLeft)
    val proj = project(p, fx)
    assert(proj.solve(-1.0).isLeft)
    assert(proj.solve(Double.NaN).isLeft)
    // A singleton group makes x_i collinear with its group column: q_i = 0, so ridge 0 is refused and ridge > 0 is not.
    val single = prepared(fx, RidgeLssGrouping.ByGroup(0 +: Vector.fill(Trials - 1)(1)))
    val sp = project(single, fx)
    assert(sp.solve(0.0).isLeft)
    val shrunk = fitAt(sp, 1.0)
    assertEqualsDouble(shrunk.deviations.value(0, 0), 0.0, 1e-9)
    // rank-deficient Z (a duplicated fixed column) is a typed refusal, never an exception
    val dup = DMat.tabulate(Rows, 2)((t, _) => fx.f(t, 0))
    assert(RidgeLeastSquaresSeparate.prepare(LssTrialDesign.unsafe(fx.x), LssFixedDesign.unsafe(dup), RidgeLssGrouping.Pooled).isLeft)

  // ---------------------------------------------------------------------------------------------- amplitude edf

  /** Dense oracle for tr(S X): per trial, the self-influence of its amplitude through the explicit penalised normal solve. */
  private def denseAmplitudeEdf(fx: Fixture, groups: Vector[Int], ridge: Double): Double =
    val g = groups.max + 1
    var total = 0.0
    var i = 0
    while i < Trials do
      val cols = g + 1 + fx.f.cols
      def col(t: Int, c: Int): Double =
        if c < g then (0 until Trials).filter(groups(_) == c).map(j => fx.x(t, j)).sum
        else if c == g then fx.x(t, i)
        else fx.f(t, c - g - 1)
      val d = DMat.tabulate(Rows, cols)(col)
      val normal = DMat.tabulate(cols, cols)((a, b) => (0 until Rows).map(t => d(t, a) * d(t, b)).sum + (if a == g && b == g then ridge else 0.0))
      val rhs = DMat.tabulate(cols, 1)((a, _) => (0 until Rows).map(t => d(t, a) * fx.x(t, i)).sum)
      val beta = normal.cholesky.fold(e => fail(e.toString), identity).solve(rhs).fold(e => fail(e.toString), identity)
      total += beta(groups(i), 0) + beta(g, 0)
      i += 1
    total

  test("rLSS amplitude edf equals the dense explicit trace tr(S X) at 1e-10, from N at no penalty down to G"):
    val fx = fixture(21L)
    val p = prepared(fx, RidgeLssGrouping.ByGroup(fx.groups))
    val meanQ = p.q.sum / Trials
    var worst = 0.0
    for mult <- Vector(0.0, 1e-2, 1e-1, 1.0, 10.0, 100.0, 1e8) do
      val r = mult * meanQ
      val closed = p.amplitudeEdf(r)
      val dense = denseAmplitudeEdf(fx, fx.groups, r)
      worst = math.max(worst, math.abs(closed - dense) / dense)
      assertEqualsDouble(closed, dense, 1e-10 * dense, s"ridge $mult * mean q")
    println(f"RIDGELSS-EDF-DENSE worst relative error $worst%.3e")
    assertEqualsDouble(p.amplitudeEdf(0.0), Trials.toDouble, 1e-10)
    assertEqualsDouble(p.amplitudeEdf(1e16 * meanQ), 3.0, 1e-6)
    val edf = Vector(0.0, 0.01, 0.1, 1.0, 10.0, 100.0).map(m => p.amplitudeEdf(m * meanQ))
    assert(edf.zip(edf.tail).forall((a, b) => a > b), s"edf not strictly decreasing: $edf")

  test("self-loadings sum to one within every group, and the edf also equals the Jacobian of the amplitudes (finite differences)"):
    val fx = fixture(22L)
    val p = prepared(fx, RidgeLssGrouping.ByGroup(fx.groups))
    (0 until 3).foreach(g => assertEqualsDouble((0 until Trials).filter(fx.groups(_) == g).map(p.selfLoading).sum, 1.0, 1e-12))
    // d ahat_i / d a_i by perturbing y along x_i: ahat is linear, so one step is exact
    val r = 0.7 * p.q.sum / Trials
    val base = fitAt(project(p, fx), r).amplitudes.value
    var jac = 0.0
    var i = 0
    while i < Trials do
      val bumped = DMat.tabulate(Rows, Voxels)((t, v) => fx.y(t, v) + (if v == 0 then fx.x(t, i) else 0.0))
      val moved = p.project(ResponseBlock.unsafe(bumped)).flatMap(_.solve(r)).fold(e => fail(e.message), identity).amplitudes.value
      jac += moved(i, 0) - base(i, 0)
      i += 1
    assertEqualsDouble(jac, p.amplitudeEdf(r), 1e-9 * p.amplitudeEdf(r))

  test("closed form on a balanced orthogonal design: edf = N [1/n + (1 - 1/n) q/(q + r)]"):
    val n = 4
    val groups = Vector(0, 0, 0, 0, 1, 1, 1, 1)
    val x = DMat.tabulate(24, 8)((t, i) => if t / 3 == i then 1.5 * (1.0 + 0.2 * (t % 3)) else 0.0)
    val d2 = (0 until 24).map(t => x(t, 0) * x(t, 0)).sum
    val q = d2 * (1.0 - 1.0 / n)
    val p = RidgeLeastSquaresSeparate
      .prepare(LssTrialDesign.unsafe(x), LssFixedDesign.empty(24), RidgeLssGrouping.ByGroup(groups))
      .fold(e => fail(e.message), identity)
    for r <- Vector(0.0, 0.1, 1.0, 10.0, 1e6) do
      assertEqualsDouble(p.amplitudeEdf(r), 8.0 * (1.0 / n + (1.0 - 1.0 / n) * q / (q + r)), 1e-12)
