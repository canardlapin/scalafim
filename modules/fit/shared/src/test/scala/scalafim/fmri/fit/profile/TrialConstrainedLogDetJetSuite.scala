package scalafim.fmri.fit.profile

import gale.linalg.{BandedCholesky, DMat, DMatBuilder}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, WhiteningPlan}
import scalafim.fmri.design.hrf.{ExpandedTrialDesign, HrfKernelBasis, KernelBasisSpec, TrialMembership}
import scalafim.fmri.hrf.{PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{Cascade34Family, GaussianFamily, JetLayout, ParametricHrfFamily, ShapePoint}

/** The oracle is the full-data covariance `K = I + X P Xᵀ / lambda` built from
  * whitened time-domain design columns and the membership projector, with
  * `log|K|` from Gale's dense Cholesky and centered finite differences of that
  * dense value for every gradient and Hessian entry. The helper's input bands
  * are assembled here from the packed Gram blocks, as the owning reference will
  * do; the oracle shares only the compiled basis coefficients with production.
  */
class TrialConstrainedLogDetJetSuite extends munit.FunSuite:
  private val step = PositiveSeconds(0.2).fold(error => fail(error.message), identity)
  private lazy val gaussian = HrfKernelBasis.compile(
    KernelBasisSpec(GaussianFamily.Default, step, Vector(26, 21), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)
  private lazy val cascade = HrfKernelBasis.compile(
    KernelBasisSpec(Cascade34Family.Default, step, Vector(9, 7, 5), tolerance = 1e-4, maxRank = 40))
    .fold(error => fail(error.message), identity)

  private final case class Fixture(
      expanded: ExpandedTrialDesign,
      nuisance: Option[DMat],
      whitening: Option[WhiteningPlan],
      lambda: Double):
    def rows: Int = expanded.rows
    def trials: Int = expanded.trials

  // Unequal counts with a singleton condition (0) and, optionally, two coincident onsets.
  private val defaultOnsets = Vector(2.5, 8.0, 15.0, 21.0, 4.0, 14.0)
  private val defaultRuns = Vector(0, 0, 0, 0, 1, 1)
  private val defaultMembership = Vector(0, 1, 1, 1, 2, 2)

  private def fixture(
      basis: HrfKernelBasis,
      lambda: Double = 2.0,
      nuisanceColumns: Int = 2,
      ar: Boolean = true,
      coincident: Boolean = false,
      onsets: Vector[Double] = defaultOnsets,
      runs: Vector[Int] = defaultRuns,
      membership: Vector[Int] = defaultMembership): Fixture =
    val rows = if basis.family.dimension == 3 then 48 else 80
    val run = rows / 2
    val conditions = membership.max + 1
    val members = TrialMembership.make(membership, conditions).fold(error => fail(error.message), identity)
    val times = if coincident then onsets.updated(2, onsets(1)) else onsets
    val expanded = ExpandedTrialDesign.lower(
      times.map(Seconds(_)), runs, Vector.fill(times.length)(Seconds(0.0)), members,
      SamplingFrame(blockLens = Seq(run, run), tr = Seq(1.0, 1.0)),
      basis, Seconds(0.2)).fold(error => fail(error.message), identity)
    val nuisance = if nuisanceColumns == 0 then None else Some(DMat.tabulate(rows, nuisanceColumns): (t, j) =>
      val local = if t < run then t else t - run
      if j == 0 then 1.0 else (local - (run - 1) * 0.5) / run)
    val whitening =
      if ar then Some(WhiteningPlan.global(ArmaCoefficients.ar(0.31),
        Vector(TimeSegment(0, run, 0), TimeSegment(run, rows, 1))))
      else None
    Fixture(expanded, nuisance, whitening, lambda)

  private def prepare(fx: Fixture): TrialBandedPreparation =
    TrialBandedPreparation.prepare(fx.expanded, fx.whitening, fx.nuisance, fx.lambda)
      .fold(error => fail(error.message), identity)

  private def offNode(family: ParametricHrfFamily, fractions: Vector[Double]): Vector[Double] =
    Vector.tabulate(family.dimension): axis =>
      family.chart.lower(axis) + fractions(axis) * (family.chart.upper(axis) - family.chart.lower(axis))

  /** Owner-style bands at one point: value (with lambda), first and row-packed second jets. */
  private final case class Bands(value: DMat, first: Vector[DMat], second: Vector[DMat])

  private def bands(prep: TrialBandedPreparation, coordinates: Vector[Double], scale: Double = 1.0): Bands =
    val basis = prep.basis
    val d = basis.family.dimension
    val m = basis.rank
    val n = prep.trials
    val width = prep.bandWidth
    val comps = JetLayout.components(d)
    val coefficients = new Array[Double](comps * m)
    basis.coefficientJetInto(ShapePoint.unsafe(coordinates), new Array[Double](comps * basis.fineCount), coefficients, comps)
    def c(component: Int, p: Int): Double = coefficients(component * m + p)
    def assemble(weight: (Int, Int) => Double, withLambda: Boolean): DMat =
      val out = DMatBuilder.zeros(n, width)
      var q = 0
      while q < m do
        var p = 0
        while p <= q do
          val w = weight(p, q)
          if w != 0.0 then
            val block = TrialBandedPreparation.pairIndex(p, q) * prep.packedBandSize
            var i = 0
            while i < n do
              var delta = 0
              while delta <= math.min(i, width - 1) do
                out(i, delta) = out(i, delta) + scale * w * prep.gramBlocksData(block + i * width + delta)
                delta += 1
              i += 1
          p += 1
        q += 1
      if withLambda then
        var i = 0
        while i < n do
          out(i, 0) = out(i, 0) + scale * prep.lambda
          i += 1
      out.result()
    val value = assemble((p, q) => c(0, p) * c(0, q), withLambda = true)
    val first = Vector.tabulate(d): a =>
      val f = JetLayout.first(a)
      assemble((p, q) => c(f, p) * c(0, q) + c(0, p) * c(f, q), withLambda = false)
    // Row-packed order: second(JetLayout.second(d, a, b) - 1 - d) for a <= b.
    val second = for a <- (0 until d).toVector; b <- (a until d).toVector yield
      val s = JetLayout.second(d, a, b)
      val fa = JetLayout.first(a)
      val fb = JetLayout.first(b)
      assemble((p, q) => c(s, p) * c(0, q) + c(fa, p) * c(fb, q) + c(fb, p) * c(fa, q) + c(0, p) * c(s, q),
        withLambda = false)
    Bands(value, first, second)

  private def accept(prep: TrialBandedPreparation, coordinates: Vector[Double], value: DMat, lambda: Double): TrialAcceptedTrialBand =
    TrialAcceptedTrialBand.factorize(prep, coordinates, lambda, value).outcome.fold(error => fail(error.message), identity)

  private def input(prep: TrialBandedPreparation, coordinates: Vector[Double], b: Bands, lambda: Double): TrialDeterminantInput =
    TrialDeterminantInput(accept(prep, coordinates, b.value, lambda), b.first, b.second)
      .fold(error => fail(error.message), identity)

  private def evaluate(prep: TrialBandedPreparation, coordinates: Vector[Double]): TrialDeterminantAttempt =
    TrialConstrainedLogDetJet.evaluate(input(prep, coordinates, bands(prep, coordinates), prep.lambda),
      TrialDeterminantNumerics.Default)

  private def jetOf(attempt: TrialDeterminantAttempt): TrialDeterminantJet =
    attempt.outcome.fold(error => fail(error.message), identity)

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

  private def whiten(fx: Fixture, columns: Int, data: Array[Double]): Array[Double] =
    fx.whitening match
      case None => java.util.Arrays.copyOf(data, data.length)
      case Some(plan) =>
        val out = new Array[Double](data.length)
        // Explicit two-run AR(1) recurrence, reset at each run boundary.
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

  /** log|I + (XP)(XP)ᵀ / lambda| from a dense Gale Cholesky. */
  private def denseLogDetK(fx: Fixture, coordinates: Vector[Double], lambda: Double): Double =
    val rows = fx.rows
    val n = fx.trials
    val x = whiten(fx, n, designAt(fx.expanded, ShapePoint.unsafe(coordinates)))
    val membership = fx.expanded.membership
    val projected = Array.tabulate(rows * n): index =>
      val t = index / n
      val i = index % n
      val group = membership.trialsOf(membership.conditionOfTrial(i))
      x(t * n + i) - group.map(j => x(t * n + j)).sum / group.length
    val k = DMat.tabulate(rows, rows): (t, s) =>
      var sum = 0.0
      var i = 0
      while i < n do
        sum += projected(t * n + i) * projected(s * n + i)
        i += 1
      (if t == s then 1.0 else 0.0) + sum / lambda
    val lower = k.cholesky.fold(error => throw error, identity).lower
    2.0 * (0 until rows).map(i => math.log(lower(i, i))).sum

  private final case class FiniteDifferences(gradient: Vector[Double], hessian: Vector[Double])

  private def finiteDifferences(fx: Fixture, at: Vector[Double], h: Double): FiniteDifferences =
    val d = at.length
    def f(shift: Vector[Double]): Double = denseLogDetK(fx, at.zip(shift).map(_ + _), fx.lambda)
    def e(axis: Int, size: Double): Vector[Double] = Vector.tabulate(d)(k => if k == axis then size else 0.0)
    val center = f(Vector.fill(d)(0.0))
    val gradient = Vector.tabulate(d)(p => (f(e(p, h)) - f(e(p, -h))) / (2.0 * h))
    val hessian = Vector.tabulate(d * d): k =>
      val p = k / d
      val q = k % d
      if p == q then (f(e(p, h)) - 2.0 * center + f(e(p, -h))) / (h * h)
      else
        def both(sp: Double, sq: Double): Vector[Double] = e(p, sp).zip(e(q, sq)).map(_ + _)
        (f(both(h, h)) - f(both(h, -h)) - f(both(-h, h)) + f(both(-h, -h))) / (4.0 * h * h)
    FiniteDifferences(gradient, hessian)

  // ----- Tests -----

  test("value equals the accepted scalar constrained determinant at a prepared node"):
    for basis <- Vector(gaussian, cascade) do
      val prep = prepare(fixture(basis))
      val grid = NodeGrid(basis.family.chart, Vector.fill(basis.family.dimension)(3))
      val objective = prep.objective(grid).fold(error => fail(error.message), identity)
      val node = grid.indexOf(Array.fill(grid.dimension)(1))
      val coordinates = grid.point(node).coordinates
      val jet = jetOf(evaluate(prep, coordinates))
      val scalar = objective.logDetAtNode(node).fold(error => fail(error.message), identity)
      assertEqualsDouble(jet.value, scalar, 1e-9 * math.max(1.0, math.abs(scalar)), clues(basis.family.name))

  test("off-node Gaussian 2D and Cascade 3D match the dense full-data covariance and its finite differences"):
    for (basis, fractions) <- Vector(
        gaussian -> Vector(0.43, 0.61),
        cascade -> Vector(0.47, 0.38, 0.56)) do
      val fx = fixture(basis)
      val prep = prepare(fx)
      val at = offNode(basis.family, fractions)
      val bound = input(prep, at, bands(prep, at), prep.lambda)
      val jet = jetOf(TrialConstrainedLogDetJet.evaluate(bound, TrialDeterminantNumerics.Default))
      val dense = denseLogDetK(fx, at, fx.lambda)
      assertEqualsDouble(jet.value, dense, 1e-9 * math.max(1.0, math.abs(dense)), clues(basis.family.name))
      val d = at.length
      val steps = Vector(2e-3, 1e-3)
      val errors = steps.map: h =>
        val fd = finiteDifferences(fx, at, h)
        val gradientError = (0 until d).map(p => math.abs(fd.gradient(p) - jet.gradient(p))).max
        val hessianError = (0 until d * d).map(k => math.abs(fd.hessian(k) - jet.hessian(k))).max
        (gradientError, hessianError)
      println(s"${basis.family.name} D=${jet.value} dense=$dense gradient=${jet.gradient} hessian=${jet.hessian} " +
        s"FD errors (gradient, hessian) by step $steps: $errors")
      // Absolute tolerances: the entries here are O(1e-2..1); observed O(h²) errors are ~1e-6 at h = 2e-3.
      for (gradientError, hessianError) <- errors do
        assert(gradientError < 2e-5, clues(basis.family.name, errors, jet.gradient))
        assert(hessianError < 2e-5, clues(basis.family.name, errors, jet.hessian))
      // Centered differences are O(h²): halving h must cut both errors, not merely stay small.
      assert(errors(1)._1 < errors(0)._1 * 0.6 && errors(1)._2 < errors(0)._2 * 0.6, clues(errors))
      for p <- 0 until d; q <- 0 until d do assertEquals(jet.hessianAt(p, q), jet.hessianAt(q, p))
      if d == 3 then
        // Discriminates a copied (unremapped) second-derivative suffix: Gale packs 02 where ScalaFIM packs 11.
        // A swap would move both entries by |H02 - H11|; require that to dwarf the oracle's own error.
        val separation = math.abs(jet.hessianAt(0, 2) - jet.hessianAt(1, 1))
        assert(separation > 1e-3 && separation > 1000.0 * errors(1)._2,
          clues(jet.hessianAt(0, 2), jet.hessianAt(1, 1), errors))
      assert(jet.factor eq bound.factor)
      assert(jet.preparation eq prep)
      assertEquals(jet.coordinates, at)

  test("nuisance design does not enter the constrained determinant"):
    for basis <- Vector(gaussian, cascade) do
      val at = offNode(basis.family, Vector(0.52, 0.44, 0.61).take(basis.family.dimension))
      val without = jetOf(evaluate(prepare(fixture(basis, nuisanceColumns = 0)), at))
      val withNuisance = jetOf(evaluate(prepare(fixture(basis, nuisanceColumns = 2)), at))
      assertEqualsDouble(withNuisance.value, without.value, 1e-10 * math.max(1.0, math.abs(without.value)))
      for k <- without.gradient.indices do
        assertEqualsDouble(withNuisance.gradient(k), without.gradient(k), 1e-9 * math.max(1.0, math.abs(without.gradient(k))))
      for k <- without.hessian.indices do
        assertEqualsDouble(withNuisance.hessian(k), without.hessian(k), 1e-8 * math.max(1.0, math.abs(without.hessian(k))))

  test("coincident trials and a complete-record permutation preserve the determinant jet"):
    val basis = gaussian
    val at = offNode(basis.family, Vector(0.38, 0.57))
    val fx = fixture(basis, coincident = true)
    val original = jetOf(evaluate(prepare(fx), at))
    val dense = denseLogDetK(fx, at, fx.lambda)
    assertEqualsDouble(original.value, dense, 1e-9 * math.max(1.0, math.abs(dense)))
    val order = Vector(4, 0, 5, 2, 1, 3)
    val coincidentOnsets = defaultOnsets.updated(2, defaultOnsets(1))
    val permuted = fixture(basis, onsets = order.map(coincidentOnsets), runs = order.map(defaultRuns),
      membership = order.map(defaultMembership))
    assertEquals(permuted.expanded.membership.conditionOfTrial, order.map(defaultMembership))
    val moved = jetOf(evaluate(prepare(permuted), at))
    assertEqualsDouble(moved.value, original.value, 1e-9 * math.max(1.0, math.abs(original.value)))
    for k <- original.gradient.indices do
      assertEqualsDouble(moved.gradient(k), original.gradient(k), 1e-8 * math.max(1.0, math.abs(original.gradient(k))))
    for k <- original.hessian.indices do
      assertEqualsDouble(moved.hessian(k), original.hessian(k), 1e-7 * math.max(1.0, math.abs(original.hessian(k))))

  test("one trial per condition gives P = 0 and a vanishing determinant jet"):
    val basis = cascade
    val fx = fixture(basis, membership = Vector(0, 1, 2, 3, 4, 5))
    val at = offNode(basis.family, Vector(0.5, 0.45, 0.55))
    val jet = jetOf(evaluate(prepare(fx), at))
    // D = log|A| + log|A⁻¹| exactly; the residue is the rounding of two O(10) logs and of the jet recursions.
    assertEqualsDouble(jet.value, 0.0, 1e-10)
    jet.gradient.foreach(g => assertEqualsDouble(g, 0.0, 1e-8))
    jet.hessian.foreach(h => assertEqualsDouble(h, 0.0, 1e-6))

  test("consistent rescaling X* = sX with lambda* = s² lambda preserves the determinant jet"):
    val basis = cascade
    val prep = prepare(fixture(basis))
    val at = offNode(basis.family, Vector(0.47, 0.38, 0.56))
    val base = jetOf(evaluate(prep, at))
    val s = 3.0
    val scaled = TrialConstrainedLogDetJet.evaluate(
      input(prep, at, bands(prep, at, scale = s * s), prep.lambda * s * s), TrialDeterminantNumerics.Default)
    val jet = jetOf(scaled)
    assertEqualsDouble(jet.value, base.value, 1e-10 * math.max(1.0, math.abs(base.value)))
    for k <- base.gradient.indices do
      assertEqualsDouble(jet.gradient(k), base.gradient(k), 1e-9 * math.max(1.0, math.abs(base.gradient(k))))
    for k <- base.hessian.indices do
      assertEqualsDouble(jet.hessian(k), base.hessian(k), 1e-8 * math.max(1.0, math.abs(base.hessian(k))))

  test("predeclared lambda controls are admitted against the dense covariance or refused by residual"):
    for lambda <- Vector(1e-6, 1.0, 1e4) do
      val fx = fixture(gaussian, lambda = lambda, coincident = true)
      val at = offNode(gaussian.family, Vector(0.43, 0.61))
      val attempt = evaluate(prepare(fx), at)
      attempt.outcome match
        case Right(jet) =>
          val dense = denseLogDetK(fx, at, lambda)
          println(s"lambda=$lambda admitted: D=${jet.value} dense=$dense maxResidual=${jet.maxNormalizedResidual}")
          assert(jet.maxNormalizedResidual <= TrialDeterminantNumerics.Default.residualTolerance)
          assertEqualsDouble(jet.value, dense, 1e-7 * math.max(1.0, math.abs(dense)), clues(lambda))
        case Left(error @ TrialDeterminantError.Residual(_, _, _)) =>
          println(s"lambda=$lambda refused: ${error.message}")
          assertEquals(attempt.work.residualCheckFailures, 1L)
        case Left(other) => fail(s"lambda=$lambda: unexpected refusal ${other.message}")

  test("work receipt counts every solve, RHS column, residual check, product and log-determinant jet"):
    for basis <- Vector(gaussian, cascade) do
      val prep = prepare(fixture(basis))
      val at = offNode(basis.family, Vector(0.43, 0.61, 0.5).take(basis.family.dimension))
      val attempt = evaluate(prep, at)
      assert(attempt.outcome.isRight, clues(attempt.outcome))
      val d = basis.family.dimension
      val pairs = d * (d + 1) / 2
      val solves = 1L + d + pairs
      val c = prep.conditions.toLong
      val w = attempt.work
      assertEquals(w.bandedSolveAttempts, solves)
      assertEquals(w.bandedSolveFailures, 0L)
      assertEquals(w.rightHandSideAttempts, solves * c)
      assertEquals(w.residualCheckAttempts, solves)
      assertEquals(w.residualCheckFailures, 0L)
      // Residual check per solve, one RHS product per first axis, three per second pair.
      assertEquals(w.bandProductColumns, (solves + d + 3L * pairs) * c)
      assertEquals(w.conditionFactorAttempts, 1L)
      assertEquals(w.logDetJetAttempts, 2L)
      assertEquals(w.logDetJetFailures, 0L)
      assertEquals(w.scratchDoubles, (d + 4L) * prep.trials * c)

  test("malformed construction and derivative inputs are refused before numerical work, with charged receipts"):
    val basis = gaussian
    val prep = prepare(fixture(basis))
    val at = offNode(basis.family, Vector(0.43, 0.61))
    val b = bands(prep, at)
    def refusedBundle(coordinates: Vector[Double] = at, lambda: Double = prep.lambda, value: DMat = b.value) =
      val attempt = TrialAcceptedTrialBand.factorize(prep, coordinates, lambda, value)
      assertEquals(attempt.work, TrialBandFactorWork(1L, 1L, 0L, 0L, 0L), clues(attempt.outcome))
      attempt.outcome.left.toOption
    for lambda <- Vector(0.0, -1.0, Double.NaN, Double.PositiveInfinity) do
      assert(refusedBundle(lambda = lambda).exists(_.isInstanceOf[TrialDeterminantError.InvalidLambda]), clues(lambda))
    assert(refusedBundle(coordinates = at :+ 0.5).exists(_.isInstanceOf[TrialDeterminantError.InvalidDimension]))
    assert(refusedBundle(coordinates = Vector(at(0), Double.NaN)).exists(_.isInstanceOf[TrialDeterminantError.NonFiniteCoordinate]))
    assert(refusedBundle(value = DMat.zeros(prep.trials, prep.bandWidth + 1)).exists(_.isInstanceOf[TrialDeterminantError.BandShape]))
    val activeValueNaN = DMat.tabulate(prep.trials, prep.bandWidth)((i, d) => if i == 2 && d == 0 then Double.NaN else b.value(i, d))
    assert(refusedBundle(value = activeValueNaN).exists(_.isInstanceOf[TrialDeterminantError.NonFiniteBand]))

    // A non-SPD band reaches the one factorisation, which is charged and refused; no bundle exists to evaluate.
    val indefinite = DMat.tabulate(prep.trials, prep.bandWidth)((_, d) => if d == 0 then -1.0 else 0.0)
    val notSpd = TrialAcceptedTrialBand.factorize(prep, at, prep.lambda, indefinite)
    assertEquals(notSpd.work, TrialBandFactorWork(1L, 0L, 1L, 1L, 0L))
    assert(notSpd.outcome.left.toOption.exists:
      case TrialDeterminantError.Factor(TrialDeterminantStage.TrialFactor, _) => true
      case _ => false)
    val admitted = TrialAcceptedTrialBand.factorize(prep, at, prep.lambda, b.value)
    assertEquals(admitted.work, TrialBandFactorWork(1L, 0L, 1L, 0L, prep.trials.toLong * prep.bandWidth))

    val accepted = accept(prep, at, b.value, prep.lambda)
    def refusedInput(first: Vector[DMat] = b.first, second: Vector[DMat] = b.second) =
      TrialDeterminantInput(accepted, first, second).left.toOption
    assert(refusedInput(first = b.first.take(1)).exists(_.isInstanceOf[TrialDeterminantError.DerivativeCount]))
    assert(refusedInput(second = b.second.drop(1)).exists(_.isInstanceOf[TrialDeterminantError.DerivativeCount]))
    assert(refusedInput(first = b.first.updated(0, DMat.zeros(prep.trials, 1))).exists(_.isInstanceOf[TrialDeterminantError.BandShape]))
    val source = b.first(0)
    val poisoned = DMat.tabulate(source.rows, source.cols)((i, d) => if i == 3 && d == 1 then Double.NaN else source(i, d))
    assert(refusedInput(first = b.first.updated(0, poisoned)).exists(_.isInstanceOf[TrialDeterminantError.NonFiniteBand]))
    val padded = DMat.tabulate(source.rows, source.cols)((i, d) => if d > i then Double.NaN else source(i, d))
    assert(refusedInput(first = b.first.updated(0, padded)).isEmpty, "NaN padding is ignored")
    assert(TrialDeterminantNumerics(0.0).isLeft && TrialDeterminantNumerics(Double.NaN).isLeft)

  test("an independently supplied factor or band cannot be paired: the pre-repair constructor shape does not compile"):
    val basis = gaussian
    val prep = prepare(fixture(basis))
    val at = offNode(basis.family, Vector(0.43, 0.61))
    val b = bands(prep, at)
    val factor = BandedCholesky.factorLower(b.value).fold(error => fail(error.getMessage), identity)
    val (value, first, second, lambda) = (b.value, b.first, b.second, prep.lambda)
    // 28382eae accepted exactly this call with any same-shaped factor.
    assert(compileErrors("TrialDeterminantInput(prep, at, lambda, factor, value, first, second)").nonEmpty)
    assert(compileErrors("new TrialAcceptedTrialBand(prep, at, lambda, value, factor)").nonEmpty)
    // The only producer factors its own copy; the admitted input still evaluates.
    assert(TrialDeterminantInput(accept(prep, at, value, lambda), first, second).isRight)
    assert(factor.size == prep.trials)

  test("a close same-shape SPD band passes the residual check, so identity comes only from the stamped bundle"):
    val basis = gaussian
    val prep = prepare(fixture(basis))
    val at = offNode(basis.family, Vector(0.43, 0.61))
    val b = bands(prep, at)
    val n = prep.trials
    val c = prep.conditions
    // A2 = A1 + eps I, same N and bandwidth, still SPD.
    val eps = 1e-13
    val a2 = DMat.tabulate(n, prep.bandWidth)((i, d) => if d == 0 then b.value(i, d) + eps else b.value(i, d))
    val first = accept(prep, at, b.value, prep.lambda)
    val second = accept(prep, at, a2, prep.lambda)
    // The old hybrid: U from A1's factor, residual against A2. It passes the 1e-10 admission.
    val m = DMatBuilder.zeros(n, c)
    for i <- 0 until n do m(i, prep.membership.conditionOfTrial(i)) = 1.0
    val u = first.factor.solve(m.result()).fold(error => fail(error.getMessage), identity)
    val width = prep.bandWidth
    // Symmetric lower-band product and infinity norm, independent of the production helper.
    def entry(i: Int, j: Int): Double =
      val (hi, lo) = if i >= j then (i, j) else (j, i)
      if hi - lo < width then a2(hi, hi - lo) else 0.0
    val norm = (0 until n).map(i => (0 until n).map(j => math.abs(entry(i, j))).sum).max
    val worst = (0 until c).map: j =>
      val residual = (0 until n).map: i =>
        val product = (0 until n).map(k => entry(i, k) * u(k, j)).sum
        math.abs(product - (if prep.membership.conditionOfTrial(i) == j then 1.0 else 0.0))
      residual.max / (norm * (0 until n).map(i => math.abs(u(i, j))).max + 1.0)
    assert(worst.max < TrialDeterminantNumerics.Default.residualTolerance, clues(worst))
    // Each bundle holds the factor of its own frozen band, so no such hybrid can be constructed.
    assert(second.factor ne first.factor)
    assertEquals(second.factor.logDet, BandedCholesky.factorLower(second.band).fold(error => fail(error.getMessage), identity).logDet)
    for i <- 0 until n; d <- 0 until prep.bandWidth if d <= i do assertEquals(second.band(i, d), a2(i, d))
    val jet = jetOf(TrialConstrainedLogDetJet.evaluate(
      TrialDeterminantInput(second, b.first, b.second).fold(error => fail(error.message), identity),
      TrialDeterminantNumerics.Default))
    assert(jet.factor eq second.factor)

  test("the bundle keeps a canonical copy: source padding is not retained and a closed source builder refuses writes"):
    val basis = cascade
    val prep = prepare(fixture(basis))
    val at = offNode(basis.family, Vector(0.47, 0.38, 0.56))
    val b = bands(prep, at)
    val source = DMatBuilder.zeros(prep.trials, prep.bandWidth)
    for i <- 0 until prep.trials; d <- 0 until prep.bandWidth do
      source(i, d) = if d > i then Double.NaN else b.value(i, d)
    val frozenSource = source.result()
    intercept[Throwable](source(0, 0) = 99.0)
    val accepted = accept(prep, at, frozenSource, prep.lambda)
    assert(accepted.band ne frozenSource)
    for i <- 0 until prep.trials; d <- 0 until prep.bandWidth do
      if d > i then assertEquals(accepted.band(i, d), 0.0)
      else assertEquals(accepted.band(i, d), frozenSource(i, d))
    val before = jetOf(TrialConstrainedLogDetJet.evaluate(
      TrialDeterminantInput(accepted, b.first, b.second).fold(error => fail(error.message), identity),
      TrialDeterminantNumerics.Default))
    val again = jetOf(TrialConstrainedLogDetJet.evaluate(
      TrialDeterminantInput(accepted, b.first, b.second).fold(error => fail(error.message), identity),
      TrialDeterminantNumerics.Default))
    assertEquals(again.value, before.value)
    assertEquals(again.gradient, before.gradient)
    assertEquals(again.hessian, before.hessian)
