package scalafim.fmri.ar

import gale.linalg.{DMat, DMatBuilder, Matrix}

class WhiteningTransposeSuite extends munit.FunSuite:

  private def plan(
      scope: CoefficientScope,
      segments: Vector[TimeSegment],
      initial: InitialConditionPolicy
  ): WhiteningPlan =
    WhiteningPlan.withScope(scope, segments, initial).fold(error => fail(error.message), identity)

  private def fixture(rows: Int, cols: Int, seed: Double): DMat =
    Matrix.tabulate(rows, cols)((i, j) => math.sin(seed + 1.3 * i + 0.7 * j) + 0.25 * math.cos(seed * i - j))

  /** Independent dense W from the defining equations of each segment:
    * e = S (A y - B e), so W = (I + S B)⁻¹ S A, with A = I - Σ φl shift_l,
    * B = Σ θl shift_l restricted to the segment, and S = diag(first scale, 1, ...).
    * The inverse comes from Gale's dense solve, not the production recurrence.
    */
  private def denseW(plan: WhiteningPlan): DMat =
    val n = plan.nTimepoints
    val segmentOf = Array.fill(n)(-1)
    plan.segments.zipWithIndex.foreach((segment, index) => (segment.start until segment.endExclusive).foreach(segmentOf(_) = index))
    def scale(t: Int): Double =
      val segment = plan.segments(segmentOf(t))
      if t == segment.start then plan.coefficientsFor(segment).firstScale(plan.initialCondition).toOption.get else 1.0
    def lag(t: Int, u: Int): Int =
      if u < t && segmentOf(u) == segmentOf(t) then t - u else 0
    val scaledA = Matrix.tabulate(n, n): (t, u) =>
      val coefficients = plan.coefficientsFor(plan.segments(segmentOf(t)))
      val a =
        if t == u then 1.0
        else
          val l = lag(t, u)
          if l > 0 && l <= coefficients.phi.length then -coefficients.phi(l - 1) else 0.0
      scale(t) * a
    val lhs = Matrix.tabulate(n, n): (t, u) =>
      val coefficients = plan.coefficientsFor(plan.segments(segmentOf(t)))
      val l = lag(t, u)
      val b = if l > 0 && l <= coefficients.theta.length then coefficients.theta(l - 1) else 0.0
      (if t == u then 1.0 else 0.0) + scale(t) * b
    lhs.solve(scaledA).fold(error => throw error, identity)

  private def assertClose(actual: DMat, expected: DMat, tolerance: Double, label: String = "")(using
      munit.Location
  ): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols), label)
    for i <- 0 until actual.rows; j <- 0 until actual.cols do
      assertEqualsDouble(actual(i, j), expected(i, j), tolerance, clues(label, i, j))

  private def inner(left: DMat, right: DMat): Double =
    (for i <- 0 until left.rows; j <- 0 until left.cols yield left(i, j) * right(i, j)).sum

  private val twoRuns = Vector(TimeSegment(0, 7, 0), TimeSegment(7, 13, 1))
  // Run 0 is split by a censoring reset; run 1 is continuous.
  private val censored = Vector(TimeSegment(0, 4, 0), TimeSegment(4, 9, 0), TimeSegment(9, 15, 1))

  private val cases: Vector[(String, WhiteningPlan)] = Vector(
    "IID identity" -> plan(CoefficientScope.Global(ArmaCoefficients.Iid), twoRuns, InitialConditionPolicy.Identity),
    "AR(1) exact first sample" ->
      plan(CoefficientScope.Global(ArmaCoefficients.ar(0.62)), twoRuns, InitialConditionPolicy.ExactAr1),
    "AR(3) identity" ->
      plan(CoefficientScope.Global(ArmaCoefficients.ar(0.4, -0.2, 0.1)), censored, InitialConditionPolicy.Identity),
    "MA(2) identity" ->
      plan(CoefficientScope.Global(ArmaCoefficients.arma(Nil, Seq(0.5, -0.3))), twoRuns, InitialConditionPolicy.Identity),
    "ARMA(2,1) nonunit first scale" ->
      plan(CoefficientScope.Global(ArmaCoefficients.arma(Seq(0.5, -0.25), Seq(0.4))), censored,
        InitialConditionPolicy.PrecomputedScale(0.6)),
    "ARMA(1,2) zero first scale" ->
      plan(CoefficientScope.Global(ArmaCoefficients.arma(Seq(0.3), Seq(0.45, 0.2))), twoRuns,
        InitialConditionPolicy.PrecomputedScale(0.0)),
    "by-run AR(1)/ARMA exact first sample" ->
      plan(CoefficientScope.ByRun(Vector(ArmaCoefficients.ar(-0.45), ArmaCoefficients.arma(Seq(0.35), Seq(-0.5)))),
        censored, InitialConditionPolicy.ExactAr1)
  )

  test("dense oracle reproduces the production forward map"):
    for (name, p) <- cases do
      val x = fixture(p.nTimepoints, 3, 0.3)
      val forward = WhiteningTransform.matrix(p, x).fold(error => fail(error.message), identity)
      assertClose(forward, denseW(p) * x, 1e-12, name)

  test("transpose equals the dense W transpose for every policy, scope and segment layout"):
    for (name, p) <- cases do
      val z = fixture(p.nTimepoints, 3, 1.7)
      val adjoint = WhiteningTransform.transposeMatrix(p, z).fold(error => fail(error.message), identity)
      assertClose(adjoint, denseW(p).t * z, 1e-12, name)

  test("dot-product adjoint law against the production forward map"):
    for (name, p) <- cases; seed <- List(0.1, 2.9) do
      val x = fixture(p.nTimepoints, 2, seed)
      val z = fixture(p.nTimepoints, 2, seed + 4.4)
      val wx = WhiteningTransform.matrix(p, x).fold(error => fail(error.message), identity)
      val wtz = WhiteningTransform.transposeMatrix(p, z).fold(error => fail(error.message), identity)
      assertEqualsDouble(inner(wx, z), inner(x, wtz), 1e-11, clues(name, seed))

  test("a nonunit first scale with MA terms is not interchangeable with the IID scale"):
    // Guards the reverse-scaling order: scaling the segment start after MA
    // propagation, or using the input rather than the accumulated adjoint,
    // changes the start rows whenever theta and the scale are both nontrivial.
    val p = cases.find(_._1 == "ARMA(2,1) nonunit first scale").get._2
    val z = fixture(p.nTimepoints, 1, 0.9)
    val adjoint = WhiteningTransform.transposeMatrix(p, z).fold(error => fail(error.message), identity)
    val expected = denseW(p).t * z
    for segment <- p.segments do
      assertEqualsDouble(adjoint(segment.start, 0), expected(segment.start, 0), 1e-12)
      assert(math.abs(adjoint(segment.start, 0) - z(segment.start, 0) * 0.6) > 1e-3)

  test("zero scale is a singular forward map with a valid adjoint"):
    val p = cases.find(_._1 == "ARMA(1,2) zero first scale").get._2
    val w = denseW(p)
    for segment <- p.segments; u <- 0 until p.nTimepoints do assertEquals(w(segment.start, u), 0.0)
    val z = Matrix.tabulate(p.nTimepoints, 1)((i, _) => if p.segments.exists(_.start == i) then 1.0 else 0.0)
    val adjoint = WhiteningTransform.transposeMatrix(p, z).fold(error => fail(error.message), identity)
    for i <- 0 until p.nTimepoints do assertEquals(adjoint(i, 0), 0.0)

  test("strided and zero-column inputs are read logically and never modified"):
    val p = cases.find(_._1 == "by-run AR(1)/ARMA exact first sample").get._2
    val n = p.nTimepoints
    val source = fixture(n, 3, 0.55)
    val transposedStorage = DMatBuilder.from(source.t).result().t
    val padded = Matrix.tabulate(n + 4, 6)((i, j) => if i >= 2 && i < n + 2 && j >= 1 && j < 4 then source(i - 2, j - 1) else Double.NaN)
    val slice = padded.slice(2, n + 2, 1, 4)
    val snapshot = DMatBuilder.from(slice).result()
    val contiguous = WhiteningTransform.transposeMatrix(p, source).fold(error => fail(error.message), identity)
    for view <- List(transposedStorage, slice) do
      val result = WhiteningTransform.transposeMatrix(p, view).fold(error => fail(error.message), identity)
      assertClose(result, contiguous, 0.0)
    assertClose(slice, snapshot, 0.0)
    val empty = WhiteningTransform.transposeMatrix(p, DMat.zeros(n, 0)).fold(error => fail(error.message), identity)
    assertEquals((empty.rows, empty.cols), (n, 0))

  test("row layout and initial-condition policy are refused with the forward map's typed errors"):
    val p = cases.head._2
    val wrong = fixture(p.nTimepoints + 1, 2, 0.2)
    assertEquals(WhiteningTransform.transposeMatrix(p, wrong), WhiteningTransform.matrix(p, wrong).map(identity))
    assertEquals(WhiteningTransform.transposeMatrix(p, wrong).left.toOption,
      Some(ArError.SegmentCoverageMismatch(p.nTimepoints, p.nTimepoints + 1)))
    assertEquals(WhiteningTransform.transposeMatrix(p, DMat.zeros(0, 1)).left.toOption, Some(ArError.NonPositiveRows(0)))
    assertEquals(
      WhiteningPlan.withScope(CoefficientScope.Global(ArmaCoefficients.ar(0.2)), twoRuns,
        InitialConditionPolicy.PrecomputedScale(-0.1)).left.toOption,
      Some(ArError.InvalidInitialScale(-0.1)))
    assertEquals(
      WhiteningPlan.withScope(CoefficientScope.ByRun(Vector(ArmaCoefficients.ar(0.2))), twoRuns,
        InitialConditionPolicy.Identity).left.toOption,
      Some(ArError.CoefficientScopeMismatch(CoefficientScopeKind.ByRun, 1, 2)))
