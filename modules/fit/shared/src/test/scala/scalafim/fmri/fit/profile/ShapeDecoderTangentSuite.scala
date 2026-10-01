package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.ShapeChart

class ShapeDecoderTangentSuite extends munit.FunSuite:
  /** Exact convex control with constrained minimum (0, 1[, 0]). */
  private final class Coupled(mirrored: Boolean = false, three: Boolean = false, atMinimum: Boolean = false) extends ShapeObjective:
    private val axes = Vector(("x", 0.0, 1.0), ("y", if atMinimum then 1.0 else 0.0, 4.0)) ++
      (if three then Vector(("z", -1.0, 1.0)) else Vector.empty)
    val grid = NodeGrid(ShapeChart(axes*), if three then Vector(2, 2, 3) else Vector(2, 2))
    def amplitudeCount: Int = 1
    var candidateCalls = 0
    private def fill(coords: Array[Double], out: ProfileJetBuffer): Unit =
      val x = if mirrored then 1.0 - coords(0) else coords(0)
      val y = coords(1)
      val z = if three then coords(2) else 0.0
      val sign = if mirrored then -1.0 else 1.0
      out.energy = x * x + x * y + y * y - 0.5 * x - 2.0 * y + z * z
      out.gradient(0) = sign * (2.0 * x + y - 0.5)
      out.gradient(1) = x + 2.0 * y - 2.0
      java.util.Arrays.fill(out.hessian, 0.0)
      out.hessian(0) = 2.0
      out.hessian(1) = sign
      out.hessian(grid.dimension) = sign
      out.hessian(grid.dimension + 1) = 2.0
      if three then
        out.gradient(2) = 2.0 * z
        out.hessian(8) = 2.0
      out.amplitudes(0) = 7.0 + y
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double =
      val out = new ProfileJetBuffer(grid.dimension, 1)
      fill(grid.point(node).coordinates.toArray, out)
      out.energy
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node).coordinates.toArray, out)
      true
    def jetAt(coords: Array[Double], out: ProfileJetBuffer): Boolean =
      candidateCalls += 1
      fill(coords, out)
      true
    def energyAt(coords: Array[Double], out: ProfileJetBuffer): Double =
      candidateCalls += 1
      fill(coords, out)
      out.energy

  private def checkCoupled(mirrored: Boolean, three: Boolean): Unit =
    val objective = new Coupled(mirrored, three)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Boundary)
    assertEqualsDouble(result.coordinates(0), if mirrored then 1.0 else 0.0, 1e-12)
    assertEqualsDouble(result.coordinates(1), 1.0, 1e-9)
    if three then assertEqualsDouble(result.coordinates(2), 0.0, 1e-12)
    assertEqualsDouble(result.energy, -1.0, 1e-12)
    assertEqualsDouble(result.amplitudes.head, 8.0, 1e-9)
    assert(counters.candidateAttempts > 0L)
    assertEquals(objective.candidateCalls.toLong, counters.jets - 1L + counters.exactEvaluations)
    assert(counters.jets <= 8L && counters.exactEvaluations <= 2L && counters.newtonSteps <= 6L)
    assertEquals(result.budgetExit, None)
    val terminal = new ProfileJetBuffer(objective.grid.dimension, 1)
    objective.jetAt(result.coordinates.toArray, terminal)
    assertEqualsDouble(terminal.gradient(1), 0.0, 2e-9)
    terminal.hessian.toVector.zip(result.dataHessian).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))

  test("coupled lower-bound Newton direction follows feasible descent to the exact constrained optimum"):
    checkCoupled(false, false)

  test("coupled upper-bound Newton direction follows feasible descent to the mirrored optimum"):
    checkCoupled(true, false)

  test("tangent correction handles a third independent coordinate"):
    checkCoupled(false, true)

  test("a genuine constrained stationary bank point needs no candidate"):
    val objective = new Coupled(atMinimum = true)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, DecodeBudget(coarseStride = 1), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Boundary)
    assertEqualsDouble(result.energy, -1.0, 1e-12)
    assertEquals(counters.candidateAttempts, 0L)
    assertEquals(objective.candidateCalls, 0)

  private final class Line(val grid: NodeGrid, origin: Double, offset: Double, slope: Double, reject: Boolean = false, curvature: Double = 2.0) extends ShapeObjective:
    def amplitudeCount: Int = 1
    var calls = 0
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      val u = x - origin
      out.energy = offset + 0.5 * curvature * u * u - slope * u
      out.gradient(0) = curvature * u - slope
      out.hessian(0) = curvature
      out.amplitudes(0) = 3.0 + u
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double =
      val out = new ProfileJetBuffer(1, 1)
      fill(grid.point(node)(0), out)
      out.energy
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      true
    def jetAt(coords: Array[Double], out: ProfileJetBuffer): Boolean =
      calls += 1
      fill(coords(0), out)
      if reject then out.energy = Double.PositiveInfinity
      true
    def energyAt(coords: Array[Double], out: ProfileJetBuffer): Double =
      jetAt(coords, out)
      out.energy

  test("a tiny representable clipped step is evaluated rather than called stationary"):
    val objective = new Line(NodeGrid(ShapeChart(("x", 1.0 - 5e-10, 1.0)), Vector(2)), 2.0, 1e15, 0.0)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 2, maxJets = 3), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Boundary)
    assertEqualsDouble(result.coordinates.head, 1.0, 1e-15)
    assertEquals(counters.candidateAttempts, 1L)
    assertEquals(objective.calls, 1)

  test("an unrepresentable raw nonstationary correction preserves terminal state without an evaluation"):
    val b = 1e8
    val objective = new Line(NodeGrid(ShapeChart(("x", b - 1.0, b + 1.0)), Vector(3)), b, 0.0, 8e-9)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, DecodeBudget(coarseStride = 1), None, 1.0).decode(counters)
    assertEquals(result.status.toString, "Stalled")
    assertEquals(result.budgetExit, None)
    assertEqualsDouble(result.coordinates.head, b, 0.0)
    assertEqualsDouble(result.energy, 0.0, 0.0)
    assertEqualsDouble(result.amplitudes.head, 3.0, 0.0)
    assertEqualsDouble(result.dataHessian.head, 2.0, 0.0)
    assertEquals(counters.candidateAttempts, 0L)
    assertEquals(counters.jets, 1L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(objective.calls, 0)

  test("halving to an unchanged coordinate stalls only before the attempt cap is exhausted"):
    for (cap, expected) <- Vector((1, "BudgetExceeded"), (4, "Stalled")) do
      val b = 1e8
      val objective = new Line(NodeGrid(ShapeChart(("x", b - 1.0, b + 1.0)), Vector(3)), b, 0.0, 3e-8, reject = true)
      val counters = new DecoderCounters
      val result = new ShapeDecoder(objective,
        DecodeBudget(coarseStride = 1, maxNewtonSteps = 2, maxJets = 4, maxExactEvaluations = 2, maxCandidateAttempts = cap),
        None, 1.0).decode(counters)
      assertEquals(result.status.toString, expected)
      assertEquals(result.budgetExit, if cap == 1 then Some(DecodeBudgetExit.CandidateAttemptCap) else None)
      assertEquals(counters.candidateAttempts, 2L.min(cap.toLong))
      assertEquals(objective.calls.toLong, counters.candidateAttempts)
      assertEqualsDouble(result.coordinates.head, b, 0.0)
      assertEqualsDouble(result.amplitudes.head, 3.0, 0.0)
      assertEqualsDouble(result.dataHessian.head, 2.0, 0.0)

  test("a finite jet whose Newton solve overflows stalls without an objective callback"):
    val objective = new Line(NodeGrid(ShapeChart(("x", 0.0, 1.0)), Vector(2)), 0.0, 1e308, 1.0,
      curvature = Double.MinPositiveValue)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, DecodeBudget(coarseStride = 1), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Stalled)
    assertEquals(result.budgetExit, None)
    assertEqualsDouble(result.coordinates.head, 0.0, 0.0)
    assertEqualsDouble(result.energy, 1e308, 0.0)
    assertEqualsDouble(result.amplitudes.head, 3.0, 0.0)
    assertEqualsDouble(result.dataHessian.head, Double.MinPositiveValue, 0.0)
    assertEquals(counters.candidateAttempts, 0L)
    assertEquals(counters.jets, 1L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(objective.calls, 0)
