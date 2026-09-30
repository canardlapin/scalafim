package scalafim.fmri.fit.profile

import gale.linalg.DMat
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{Cascade34Family, GaussianFamily, ParametricHrfFamily, ShapePoint}

/** Native backend against an independent original-time dense model.
  *
  * The oracle assembles X(theta) from the design source and compiled basis
  * coefficients at the actual shape, whitens X, F and y with an explicit two-run
  * AR(1) recurrence, and forms K = I + (XP)(XP)ᵀ/lambda and Z = [F, XM] with
  * Gale dense solves. D = log|K| comes before any nuisance projection, and
  * E = r'K⁻¹r is the GLS profile. It shares only the compiled basis and Gale's
  * dense kernels with production (not a direct convolved original-time truth),
  * and never reuses the production accepted band, release or factor.
  */
class TrialBandedMlBackendSuite extends munit.FunSuite:
  private val step = PositiveSeconds(0.2).fold(error => fail(error.message), identity)
  private lazy val gaussian = HrfKernelBasis.compile(
    KernelBasisSpec(GaussianFamily.Default, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)
  private lazy val cascade = HrfKernelBasis.compile(
    KernelBasisSpec(Cascade34Family.Default, step, Vector(9, 7, 5), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private val sigma2 = 2.5

  private final case class Fixture(
      expanded: ExpandedTrialDesign,
      nuisance: Option[DMat],
      whitening: Option[WhiteningPlan],
      lambda: Double,
      response: Array[Double]):
    def rows: Int = expanded.rows
    def trials: Int = expanded.trials
    def nuisanceColumns: Int = nuisance.fold(0)(_.cols)

  private val onsets = Vector(2.5, 8.0, 15.0, 21.0, 4.0, 14.0)
  private val runs = Vector(0, 0, 0, 0, 1, 1)
  // Unequal counts with a singleton condition 0.
  private val membership = Vector(0, 1, 1, 1, 2, 2)

  private def fixture(
      basis: HrfKernelBasis,
      lambda: Double = 2.0,
      nuisanceColumns: Int = 2,
      coincident: Boolean = false,
      aliasNuisance: Boolean = false): Fixture =
    val rows = if basis.family.dimension == 3 then 48 else 80
    val run = rows / 2
    val members = TrialMembership.make(membership, 3).fold(error => fail(error.message), identity)
    val times = if coincident then onsets.updated(2, onsets(1)) else onsets
    val expanded = ExpandedTrialDesign.lower(
      times.map(Seconds(_)), runs, Vector.fill(times.length)(Seconds(0.0)), members,
      SamplingFrame(blockLens = Seq(run, run), tr = Seq(1.0, 1.0)),
      basis, Seconds(0.2)).fold(error => fail(error.message), identity)
    val nuisance = if nuisanceColumns == 0 then None else Some(DMat.tabulate(rows, nuisanceColumns): (t, j) =>
      val local = if t < run then t else t - run
      if j == 0 || (aliasNuisance && j == nuisanceColumns - 1) then 1.0 else (local - (run - 1) * 0.5) / run)
    val whitening = Some(WhiteningPlan.global(ArmaCoefficients.ar(0.31),
      Vector(TimeSegment(0, run, 0), TimeSegment(run, rows, 1))))
    val shape = offNode(basis.family, Vector(0.55, 0.45, 0.5).take(basis.family.dimension))
    val x = designAt(expanded, ShapePoint.unsafe(shape))
    val signed = Array(0.8, -0.3, 0.45, 0.2, -0.6, 0.35)
    val raw = Array.tabulate(rows): t =>
      var value = 0.05 * math.sin(0.37 * t) + 0.03 * math.cos(0.11 * t * t)
      var i = 0
      while i < expanded.trials do
        value += x(t * expanded.trials + i) * signed(i)
        i += 1
      nuisance.foreach(matrix => value += 0.4 * matrix(t, 0))
      value
    val base = Fixture(expanded, nuisance, whitening, lambda, Array.emptyDoubleArray)
    base.copy(response = whiten(base, 1, raw))

  private def offNode(family: ParametricHrfFamily, fractions: Vector[Double]): Vector[Double] =
    Vector.tabulate(family.dimension): axis =>
      family.chart.lower(axis) + fractions(axis) * (family.chart.upper(axis) - family.chart.lower(axis))

  private def prepared(fx: Fixture): TrialBandedPreparation =
    TrialBandedPreparation.prepare(fx.expanded, fx.whitening, fx.nuisance, fx.lambda)
      .fold(error => fail(error.message), identity)

  private def backend(fx: Fixture, nodes: Int = 3): TrialBandedMlBackend =
    val prep = prepared(fx)
    val grid = NodeGrid(prep.basis.family.chart, Vector.fill(prep.basis.family.dimension)(nodes))
    val objective = prep.objective(grid).fold(error => fail(error.message), identity)
    val made = TrialBandedMlBackend.make(objective)
    val value = made.result.fold(error => fail(error.message), identity)
    assert(value.pointAt(fx.response).result.isRight)
    value

  private def success[A](attempt: TrialMlAttempt[A]): A = attempt.result.fold(error => fail(error.message), identity)

  // ----- Independent dense oracle -----

  private def designAt(expanded: ExpandedTrialDesign, point: ShapePoint): Array[Double] =
    val basis = expanded.basis
    val coefficients = new Array[Double](basis.rank)
    basis.coefficientsInto(point, new Array[Double](basis.fineCount), coefficients)
    val rows = expanded.rows
    val n = expanded.trials
    val m = basis.rank
    val source = expanded.term.data.data
    Array.tabulate(rows * n): index =>
      val row = index / n
      val trial = index % n
      var value = 0.0
      var p = 0
      while p < m do
        value += source(row * n * m + p * n + trial) * coefficients(p)
        p += 1
      value

  /** Explicit two-run AR(1) recurrence with exact first-sample scaling and reset. */
  private def whiten(fx: Fixture, columns: Int, data: Array[Double]): Array[Double] =
    fx.whitening match
      case None => java.util.Arrays.copyOf(data, data.length)
      case Some(plan) =>
        val out = new Array[Double](data.length)
        plan.segments.foreach: segment =>
          val phi = plan.coefficientsFor(segment).phi.head
          var col = 0
          while col < columns do
            out(segment.start * columns + col) = data(segment.start * columns + col) *
              (if plan.exactFirstAr1 then math.sqrt(1.0 - phi * phi) else 1.0)
            var row = segment.start + 1
            while row < segment.endExclusive do
              out(row * columns + col) = data(row * columns + col) - phi * data((row - 1) * columns + col)
              row += 1
            col += 1
        out

  private final case class Dense(energy: Double, logDet: Double, means: Vector[Double]):
    def criterion: Double = energy + sigma2 * logDet

  private def dense(fx: Fixture, coordinates: Vector[Double]): Dense =
    val rows = fx.rows
    val n = fx.trials
    val f = fx.nuisanceColumns
    val members = fx.expanded.membership
    val c = members.conditionCount
    val x = whiten(fx, n, designAt(fx.expanded, ShapePoint.unsafe(coordinates)))
    val nuisanceRaw = new Array[Double](rows * f)
    fx.nuisance.foreach(_.copyRowMajorTo(nuisanceRaw))
    val nuisance = whiten(fx, f, nuisanceRaw)
    val projected = Array.tabulate(rows * n): index =>
      val t = index / n
      val i = index % n
      val group = members.trialsOf(members.conditionOfTrial(i))
      x(t * n + i) - group.map(j => x(t * n + j)).sum / group.length
    val k = DMat.tabulate(rows, rows): (t, s) =>
      var sum = 0.0
      var i = 0
      while i < n do
        sum += projected(t * n + i) * projected(s * n + i)
        i += 1
      (if t == s then 1.0 else 0.0) + sum / fx.lambda
    val chol = k.cholesky.fold(error => throw error, identity)
    val logDet = 2.0 * (0 until rows).map(i => math.log(chol.lower(i, i))).sum
    // Z = [F, XM]: nuisance columns then per-condition sums of trial columns.
    val z = DMat.tabulate(rows, f + c): (t, j) =>
      if j < f then nuisance(t * f + j)
      else members.trialsOf(j - f).map(i => x(t * n + i)).sum
    val y = DMat.tabulate(rows, 1)((t, _) => fx.response(t))
    val kz = chol.solve(z).fold(error => throw error, identity)
    val ky = chol.solve(y).fold(error => throw error, identity)
    val gram = z.t * kz
    val cross = z.t * ky
    val coefficients = gram.cholesky.fold(error => throw error, identity).solve(cross).fold(error => throw error, identity)
    val yKy = (0 until rows).map(t => fx.response(t) * ky(t, 0)).sum
    val fit = (0 until f + c).map(j => cross(j, 0) * coefficients(j, 0)).sum
    Dense(yKy - fit, logDet, Vector.tabulate(c)(j => coefficients(f + j, 0)))

  private final case class Differences(gradient: Vector[Double], hessian: Vector[Double])

  private def differences(value: Vector[Double] => Double, at: Vector[Double], h: Double): Differences =
    val d = at.length
    def f(shift: Vector[Double]): Double = value(at.zip(shift).map(_ + _))
    def e(axis: Int, size: Double): Vector[Double] = Vector.tabulate(d)(k => if k == axis then size else 0.0)
    val center = f(Vector.fill(d)(0.0))
    val gradient = Vector.tabulate(d)(p => (f(e(p, h)) - f(e(p, -h))) / (2.0 * h))
    val hessian = Vector.tabulate(d * d): index =>
      val p = index / d
      val q = index % d
      if p == q then (f(e(p, h)) - 2.0 * center + f(e(p, -h))) / (h * h)
      else
        def both(sp: Double, sq: Double): Vector[Double] = e(p, sp).zip(e(q, sq)).map(_ + _)
        (f(both(h, h)) - f(both(h, -h)) - f(both(-h, h)) + f(both(-h, -h))) / (4.0 * h * h)
    Differences(gradient, hessian)

  private def maxError(a: Vector[Double], b: Vector[Double]): Double = a.zip(b).map((x, y) => math.abs(x - y)).max

  // ----- Tests -----

  test("off-node Gaussian 2D and Cascade 3D raw E, D and J match the dense model and its finite differences"):
    for (basis, fractions) <- Vector(gaussian -> Vector(0.43, 0.61), cascade -> Vector(0.47, 0.38, 0.56)) do
      val fx = fixture(basis)
      val ml = backend(fx)
      val at = offNode(basis.family, fractions)
      val pair = success(ml.jetAt(at))
      assert(pair.raw.reference eq pair.determinant.reference)
      val jet = CriterionJet.assemble(sigma2, CriterionJet.Input.TrialMl(pair)).fold(error => fail(error.message), identity)
      val truth = dense(fx, at)
      val name = basis.family.name
      assertEqualsDouble(pair.raw.jet.energy, truth.energy, 1e-9 * math.max(1.0, math.abs(truth.energy)), clues(name))
      assertEqualsDouble(pair.determinant.value, truth.logDet, 1e-9 * math.max(1.0, math.abs(truth.logDet)), clues(name))
      assertEqualsDouble(jet.minimizationJet.energy, truth.criterion, 1e-9 * math.max(1.0, math.abs(truth.criterion)))
      assertEqualsDouble(jet.score, -truth.criterion / (2.0 * sigma2), 1e-9 * math.max(1.0, math.abs(truth.criterion)))
      for j <- truth.means.indices do
        assertEqualsDouble(pair.raw.jet.amplitudes(j), truth.means(j), 1e-7 * math.max(1.0, math.abs(truth.means(j))), clues(name, j))
      val steps = Vector(2e-3, 1e-3)
      val errors = steps.map: h =>
        val e = differences(c => dense(fx, c).energy, at, h)
        val d = differences(c => dense(fx, c).logDet, at, h)
        val j = differences(c => dense(fx, c).criterion, at, h)
        Vector(
          maxError(pair.raw.jet.gradient, e.gradient), maxError(pair.raw.jet.hessian, e.hessian),
          maxError(pair.determinant.gradient, d.gradient), maxError(pair.determinant.hessian, d.hessian),
          maxError(jet.minimizationJet.gradient, j.gradient), maxError(jet.minimizationJet.hessian, j.hessian))
      println(s"$name E=${pair.raw.jet.energy} D=${pair.determinant.value} J=${jet.minimizationJet.energy} " +
        s"dense J=${truth.criterion}; FD errors [gE,HE,gD,HD,gJ,HJ] by step $steps: $errors")
      val scale = math.max(1.0, (pair.raw.jet.hessian ++ pair.raw.jet.gradient).map(math.abs).max)
      for stepErrors <- errors; (error, index) <- stepErrors.zipWithIndex do
        assert(error < 5e-5 * scale, clues(name, index, errors, scale))
      // Centered differences are O(h²): every entry family must shrink on halving.
      for index <- 0 until 6 do
        assert(errors(1)(index) < errors(0)(index) * 0.6 || errors(1)(index) < 1e-8 * scale, clues(name, index, errors))
      if basis.family.dimension == 3 then
        val separation = math.abs(pair.determinant.hessian(2) - pair.determinant.hessian(4))
        assert(separation > 1e-3 && separation > 1000.0 * errors(1)(3), clues(pair.determinant.hessian, errors))

  test("value-only and full evaluations at the same coordinates agree, from one reference each"):
    for (basis, fractions) <- Vector(gaussian -> Vector(0.43, 0.61), cascade -> Vector(0.47, 0.38, 0.56)) do
      val ml = backend(fixture(basis))
      val at = offNode(basis.family, fractions)
      val value = ml.valueAt(at)
      val full = ml.jetAt(at)
      val v = success(value)
      val j = success(full)
      assert(v.raw.reference eq v.determinant.reference)
      assertEquals(v.raw.reference.order, CriterionDerivativeOrder.Value)
      assertEquals(j.raw.reference.order, CriterionDerivativeOrder.Full)
      assert(!(v.raw.reference eq j.raw.reference), "each evaluation mints its own reference")
      assertEqualsDouble(v.raw.energy, j.raw.jet.energy, 1e-10 * math.max(1.0, math.abs(j.raw.jet.energy)))
      assertEqualsDouble(v.determinant.value, j.determinant.value, 1e-10 * math.max(1.0, math.abs(j.determinant.value)))
      assertEquals(v.raw.amplitudes.length, j.raw.jet.amplitudes.length)
      for i <- v.raw.amplitudes.indices do
        assertEqualsDouble(v.raw.amplitudes(i), j.raw.jet.amplitudes(i), 1e-9 * math.max(1.0, math.abs(j.raw.jet.amplitudes(i))))
      // One N-sized accepted-band factorisation per reference; no hidden determinant jet in the value call.
      val c = ml.amplitudeCount.toLong
      val d = basis.family.dimension
      val pairs = d * (d + 1) / 2
      assertEquals(value.work.referenceAttempts, 1L)
      assertEquals(value.work.nFactorAttempts, 1L)
      assertEquals(value.work.logDetRecursionAttempts, 0L)
      assertEquals(value.work.derivativeRightHandSides, 0L)
      assertEquals(value.work.membershipRightHandSides, c)
      assertEquals(value.work.smallFactorAttempts, 2L)
      assertEquals(full.work.referenceAttempts, 1L)
      assertEquals(full.work.nFactorAttempts, 1L)
      assertEquals(full.work.logDetRecursionAttempts, 2L)
      // The legacy scalar determinant and the helper both solve the membership columns: counted twice.
      assertEquals(full.work.membershipRightHandSides, 2L * c)
      assertEquals(full.work.derivativeRightHandSides, (d + pairs) * c)
      assertEquals(full.work.smallFactorAttempts, 3L)
      assertEquals(full.work.failures, 0L)

  test("nodes use the cached determinant jet: setup charges it once, workers never refactor"):
    val fx = fixture(cascade)
    val ml = backend(fx)
    val count = ml.grid.count.toLong
    val c = ml.amplitudeCount.toLong
    assertEquals(ml.setupReceipt.logDetRecursionAttempts, 2L * count)
    assertEquals(ml.setupReceipt.nFactorAttempts, count)
    assertEquals(ml.setupReceipt.referenceAttempts, count)
    assertEquals(ml.setupReceipt.membershipRightHandSides, 2L * count * c)
    val node = ml.grid.count / 2
    val coordinates = ml.grid.point(node).coordinates
    val before = ml.workerWorkSnapshot
    val nodeJet = ml.jetAtNode(node)
    val nodeValue = ml.valueAtNode(node)
    val j = success(nodeJet)
    val v = success(nodeValue)
    assert(j.raw.reference eq j.determinant.reference)
    assert(v.raw.reference eq j.raw.reference, "node values reuse the cached full node reference")
    assertEquals(nodeJet.work.nFactorAttempts + nodeValue.work.nFactorAttempts, 0L)
    assertEquals(nodeJet.work.logDetRecursionAttempts + nodeValue.work.logDetRecursionAttempts, 0L)
    assertEquals(ml.workerWorkSnapshot, before + nodeJet.work + nodeValue.work)
    // The continuous path at the node's coordinates builds its own reference and agrees.
    val continuous = success(ml.jetAt(coordinates))
    assertEqualsDouble(j.raw.jet.energy, continuous.raw.jet.energy, 1e-10 * math.max(1.0, math.abs(continuous.raw.jet.energy)))
    assertEqualsDouble(j.determinant.value, continuous.determinant.value, 1e-10 * math.max(1.0, math.abs(continuous.determinant.value)))
    for i <- j.determinant.hessian.indices do
      assertEqualsDouble(j.determinant.hessian(i), continuous.determinant.hessian(i), 1e-9)
    // Raw value and raw jet use different reductions of the same reference; the cached D is shared exactly.
    assertEqualsDouble(v.raw.energy, j.raw.jet.energy, 1e-10 * math.max(1.0, math.abs(j.raw.jet.energy)))
    assertEquals(v.determinant.value, j.determinant.value)
    // A new worker shares the owner and node cache but not the response epoch.
    val other = ml.newWorker()
    assert(other.owner eq ml.owner)
    assertEquals(other.setupReceipt, ml.setupReceipt)
    assert(other.jetAtNode(node).result.isLeft, "evaluation before pointAt is refused")

  test("epochs, response and coordinate validation are typed refusals before numerical work"):
    val fx = fixture(gaussian)
    val prep = prepared(fx)
    val grid = NodeGrid(prep.basis.family.chart, Vector.fill(2)(3))
    val ml = success(TrialBandedMlBackend.make(prep.objective(grid).fold(error => fail(error.message), identity)))
    val at = offNode(gaussian.family, Vector(0.43, 0.61))
    val early = ml.jetAt(at)
    assert(early.result.isLeft)
    assertEquals(early.work.nFactorAttempts, 0L)
    assertEquals(early.work.failures, 1L)
    for bad <- Vector(fx.response.take(fx.rows - 1), fx.response.updated(3, Double.NaN)) do
      val refused = ml.pointAt(bad)
      assert(refused.result.isLeft)
      assertEquals(refused.work.failures, 1L)
    val first = success(ml.pointAt(fx.response))
    val second = success(ml.pointAt(fx.response))
    assert(second.sequence > first.sequence)
    assert(first.owner eq ml.owner)
    val stale = success(ml.jetAt(at))
    assert(stale.raw.epoch eq second, "raw evidence carries the current epoch")
    for coordinates <- Vector(at :+ 0.1, Vector(at(0), Double.NaN), Vector.empty[Double]) do
      val refused = ml.jetAt(coordinates)
      assert(refused.result.isLeft, clues(coordinates))
      assertEquals(refused.work.nFactorAttempts, 0L)
      assertEquals(refused.work.referenceAttempts, 0L)
    assert(ml.valueAtNode(-1).result.isLeft && ml.jetAtNode(ml.grid.count).result.isLeft)

  test("a pre-factor construction refusal charges one reference refusal and no factor, solve or determinant work"):
    // Outside-chart accounting control only, not an admissible decoder candidate: at finite
    // (5, -1000) the Gaussian kernel overflows, the value band has nonfinite active entries, and
    // the accepted-band factory refuses before any Gale factorisation.
    val fx = fixture(gaussian)
    val ml = backend(fx)
    val outside = Vector(5.0, -1000.0)
    def legacyDelta(before: TrialBandedWorkSnapshot, after: TrialBandedWorkSnapshot) =
      (after.attempted.referenceAttempts - before.attempted.referenceAttempts,
        after.attempted.referenceFailures - before.attempted.referenceFailures,
        after.attempted.factorAttempts - before.attempted.factorAttempts,
        after.attempted.factorFailures - before.attempted.factorFailures,
        after.attempted.solveAttempts - before.attempted.solveAttempts)
    for (label, evaluate) <- Vector[(String, () => TrialMlAttempt[?])](
        "value" -> (() => ml.valueAt(outside)), "full" -> (() => ml.jetAt(outside))) do
      val before = ml.legacyWork
      val attempt = evaluate()
      val after = ml.legacyWork
      assert(attempt.result.isLeft, clues(label))
      val w = attempt.work
      assertEquals(
        (w.referenceAttempts, w.nFactorAttempts, w.nFactorFailures, w.smallFactorAttempts, w.solveAttempts,
          w.rightHandSideAttempts, w.logDetRecursionAttempts, w.failures),
        (1L, 0L, 0L, 0L, 0L, 0L, 0L, 1L), clues(label, w))
      assertEquals(legacyDelta(before, after), (1L, 1L, 0L, 0L, 0L), clues(label))
    // A subsequent in-chart evaluation still succeeds with exactly one N factor.
    val valid = ml.jetAt(offNode(gaussian.family, Vector(0.43, 0.61)))
    assert(valid.result.isRight)
    assertEquals((valid.work.nFactorAttempts, valid.work.nFactorFailures), (1L, 0L))

  test("aliased nuisance is refused by the release rank before any ML backend exists"):
    val fx = fixture(gaussian, nuisanceColumns = 2, aliasNuisance = true)
    val prepared = TrialBandedPreparation.prepare(fx.expanded, fx.whitening, fx.nuisance, fx.lambda)
    prepared match
      case Left(error) => assert(error.message.nonEmpty)
      case Right(prep) =>
        val grid = NodeGrid(prep.basis.family.chart, Vector.fill(2)(3))
        prep.objective(grid) match
          case Left(TrialBandedError.ReleaseRank(_, _)) => ()
          case other => fail(s"expected a typed release-rank refusal, got $other")

  test("coincident trials and predeclared lambda values are admitted against the dense model or refused by type"):
    for lambda <- Vector(1e-6, 1.0, 1e4) do
      val fx = fixture(gaussian, lambda = lambda, coincident = true)
      val at = offNode(gaussian.family, Vector(0.43, 0.61))
      val ml = backend(fx)
      val attempt = ml.jetAt(at)
      attempt.result match
        case Right(pair) =>
          val truth = dense(fx, at)
          println(s"lambda=$lambda admitted: E=${pair.raw.jet.energy} dense=${truth.energy} D=${pair.determinant.value} dense=${truth.logDet}")
          assertEqualsDouble(pair.raw.jet.energy, truth.energy, 1e-7 * math.max(1.0, math.abs(truth.energy)), clues(lambda))
          assertEqualsDouble(pair.determinant.value, truth.logDet, 1e-7 * math.max(1.0, math.abs(truth.logDet)), clues(lambda))
        case Left(error) =>
          println(s"lambda=$lambda refused: ${error.message}")
          assertEquals(attempt.work.failures, 1L)

  test("unchanged decoder ranks, refines and verifies with J, never with raw E"):
    // A decode-scale noise variance comparable to this fixture's residual variance (E/T ~ 0.01);
    // the dense tests above use sigma2 = 2.5 to make D dominate the derivative checks.
    val sigma2 = 0.05
    val fx = fixture(gaussian)
    val ml = backend(fx, nodes = 5)
    val budget = DecodeBudget()
    val decoder = TrialMlDecoder.checked(Some(ml), sigma2, budget, None).fold(error => fail(error.message), identity)
    val counters = new DecoderCounters
    val attempt = decoder.decode(fx.response, counters)
    val result = attempt.result.fold(error => fail(error.message), identity)
    println(s"default decode counters: nodes=${counters.nodeScores} jets=${counters.jets} exact=${counters.exactEvaluations} " +
      s"candidates=${counters.candidateAttempts} terminal=${counters.terminalVerifications} evidence=${result.terminalEvidence}")
    // The default budget takes a scalar candidate and its reserved terminal verification, which
    // yields exact-coordinate terminal evidence (no budget change, no stationarity claim).
    assert(counters.exactEvaluations >= 1L && counters.candidateAttempts >= 1L, clues(counters.exactEvaluations))
    assert(counters.terminalVerifications >= 1L, clues(counters.terminalVerifications))
    result.terminalEvidence match
      case TerminalEvidence.Available(jet) =>
        assertEquals(jet.raw.reference.coordinates, result.decoded.coordinates)
        assertEquals(jet.minimizationJet.hessian, result.decoded.dataHessian)
      case TerminalEvidence.Unavailable => fail("the default-budget decode reserved a terminal jet; evidence must be available")
    // Every scored node energy is J = E + sigma2 D of that node, not raw E.
    val scored = (0 until ml.grid.count).filter(node => result.nodeEnergies(node).isFinite)
    assert(scored.length >= 4, clues(result.nodeEnergies))
    for node <- scored do
      val v = success(ml.valueAtNode(node))
      val j = v.raw.energy + sigma2 * v.determinant.value
      assertEqualsDouble(result.nodeEnergies(node), j, 1e-9 * math.max(1.0, math.abs(j)), clues(node))
      assert(math.abs(result.nodeEnergies(node) - v.raw.energy) > 1e-6, clues(node))
    // The returned energy is J at the returned coordinates, from a fresh coherent evaluation.
    val returned = result.decoded.coordinates
    val check = success(ml.valueAt(returned))
    val jAtReturned = check.raw.energy + sigma2 * check.determinant.value
    assertEqualsDouble(result.decoded.energy, jAtReturned, 1e-8 * math.max(1.0, math.abs(jAtReturned)))
    result.terminalEvidence match
      case TerminalEvidence.Available(jet) =>
        assertEquals(jet.raw.reference.coordinates, returned)
        assert(jet.determinant.exists(_.reference eq jet.raw.reference))
      case TerminalEvidence.Unavailable => ()
    assertEquals(result.setupReceipt, ml.setupReceipt)
    println(s"decode status=${result.decoded.status} at=$returned J=${result.decoded.energy} work=${attempt.work}")
    // A fixture-specific budget that reaches acceptance exercises refinement and terminal evidence.
    val generous = DecodeBudget(maxNewtonSteps = 8, maxJets = 10, maxExactEvaluations = 20, maxCandidateAttempts = 8)
    val accepted = TrialMlDecoder.checked(Some(ml), sigma2, generous, None).fold(error => fail(error.message), identity)
      .decode(fx.response, new DecoderCounters).result.fold(error => fail(error.message), identity)
    println(s"generous decode status=${accepted.decoded.status} at=${accepted.decoded.coordinates} J=${accepted.decoded.energy} " +
      s"retained=${accepted.retainedJetCount}")
    assertEquals(accepted.decoded.status, DecodeStatus.Accepted)
    accepted.terminalEvidence match
      case TerminalEvidence.Available(jet) =>
        assertEquals(jet.raw.reference.coordinates, accepted.decoded.coordinates)
        assertEquals(jet.minimizationJet.hessian, accepted.decoded.dataHessian)
        assert(jet.determinant.exists(_.reference eq jet.raw.reference))
      case TerminalEvidence.Unavailable => fail("an accepted decode must carry terminal evidence")
    // The same unchanged decoder on raw E alone (legacy objective) lands elsewhere: D moves the optimum.
    val prep = prepared(fx)
    val legacy = prep.objective(NodeGrid(prep.basis.family.chart, Vector.fill(2)(5))).fold(error => fail(error.message), identity)
    legacy.pointAt(prep.encodeWhitened(fx.response).fold(error => fail(error.message), identity))
    val energyOnly = new ShapeDecoder(legacy, generous, None, sigma2).decode(new DecoderCounters)
    println(s"E-only decode status=${energyOnly.status} at=${energyOnly.coordinates}")
    val shift = accepted.decoded.coordinates.zip(energyOnly.coordinates).map((a, b) => math.abs(a - b)).max
    assert(shift > 1e-3, clues(accepted.decoded.coordinates, energyOnly.coordinates))
    // Missing capability and invalid sigma2 are refused before any response work.
    assertEquals(TrialMlDecoder.checked(None, sigma2, budget, None).left.toOption, Some(TrialMlFailure.MissingCapability))
    for bad <- Vector(0.0, Double.NaN, Double.PositiveInfinity) do
      assert(TrialMlDecoder.checked(Some(ml), bad, budget, None).isLeft, clues(bad))
