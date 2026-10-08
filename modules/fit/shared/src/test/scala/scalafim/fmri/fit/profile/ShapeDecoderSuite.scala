package scalafim.fmri.fit.profile

import scalafim.fmri.design.hrf.{HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.hrf.PositiveSeconds
import scalafim.fmri.hrf.family.{GaussianFamily, ShapeChart}

class ShapeDecoderSuite extends munit.FunSuite:

  test("all prepared grid nodes stay in their chart including cancellation-prone upper endpoints"):
    val chart = scalafim.fmri.hrf.family.Cascade34Family.Default.chart
    Vector(2, 3, 7).foreach: count =>
      val grid = NodeGrid(chart, Vector.fill(chart.dimension)(count))
      (0 until grid.count).foreach: node =>
        assert(chart.point(grid.point(node).coordinates).isRight, s"count=$count node=$node")
      chart.names.indices.foreach: axis =>
        assertEqualsDouble(grid.coordinateOf(0, axis), chart.lower(axis), 0.0)
        assertEqualsDouble(grid.coordinateOf(count - 1, axis), chart.upper(axis), 0.0)

  /** Smooth synthetic objective: `E = (x - x*)' A (x - x*) + 0.3 sum cos(2 x_i) + 5`, no amplitudes. */
  private final class Bowl(val grid: NodeGrid, target: Array[Double], a: Array[Double], val indefinite: Boolean = false) extends ShapeObjective:
    private val d = grid.dimension
    def amplitudeCount: Int = 1
    def energy(x: Array[Double]): Double =
      var acc = 5.0
      var i = 0
      while i < d do
        var j = 0
        while j < d do
          acc += (x(i) - target(i)) * a(i * d + j) * (x(j) - target(j))
          j += 1
        acc += 0.3 * math.cos(2.0 * x(i))
        i += 1
      if indefinite then -acc else acc
    private def fill(x: Array[Double], out: ProfileJetBuffer): Unit =
      out.energy = energy(x)
      out.amplitudes(0) = 1.0
      var i = 0
      while i < d do
        var g = 0.0
        var j = 0
        while j < d do
          g += 2.0 * a(i * d + j) * (x(j) - target(j))
          out.hessian(i * d + j) = 2.0 * a(i * d + j) * (if indefinite then -1.0 else 1.0)
          j += 1
        g -= 0.6 * math.sin(2.0 * x(i))
        out.gradient(i) = if indefinite then -g else g
        out.hessian(i * d + i) += -1.2 * math.cos(2.0 * x(i)) * (if indefinite then -1.0 else 1.0)
        i += 1
      out.curvature = CurvatureStatus.PositiveDefinite
    private val scratch = new Array[Double](3)
    def scoreNode(node: Int): Double =
      grid.coordinatesInto(node, scratch)
      energy(scratch)
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      grid.coordinatesInto(node, scratch)
      fill(scratch, out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      fill(coordinates, out)
      true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      fill(coordinates, out)
      out.energy

  private class Quartic(nodeCurvature: Option[Double] = None) extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    def amplitudeCount: Int = 1
    def energy(x: Double): Double =
      val z = x - 0.2
      5.0 + z * z + 10.0 * z * z * z * z
    def curvature(x: Double): Double = 2.0 + 120.0 * (x - 0.2) * (x - 0.2)
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      val z = x - 0.2
      out.energy = energy(x)
      out.gradient(0) = 2.0 * z + 40.0 * z * z * z
      out.hessian(0) = curvature(x)
      out.amplitudes(0) = 1.0
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double = energy(grid.point(node)(0))
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      nodeCurvature.foreach(out.hessian(0) = _)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      fill(coordinates(0), out)
      true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      fill(coordinates(0), out)
      out.energy

  private final class RejectingLineSearch extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    def amplitudeCount: Int = 1
    private def exact(x: Double): Double = 5.0 + x * x
    private def misleadingJet(x: Double, out: ProfileJetBuffer): Unit =
      out.energy = exact(x)
      out.gradient(0) = -1.0
      out.hessian(0) = 2.0
      out.amplitudes(0) = 1.0
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double = exact(grid.point(node)(0))
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      misleadingJet(grid.point(node)(0), out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      misleadingJet(coordinates(0), out)
      true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      out.energy = exact(coordinates(0))
      out.amplitudes(0) = 1.0
      out.energy

  private final class InfiniteObjective extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    def amplitudeCount: Int = 1
    def scoreNode(node: Int): Double = Double.PositiveInfinity
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean = false
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean = false
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double = Double.PositiveInfinity

  private final class RefusingObjective extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    var refuse: Boolean = true
    def amplitudeCount: Int = 1
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      out.energy = 5.0 + x * x
      out.gradient(0) = 2.0 * x
      out.hessian(0) = 2.0
      out.amplitudes(0) = 2.0
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double =
      val x = grid.point(node)(0)
      5.0 + x * x
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      if refuse then false
      else
        fill(grid.point(node)(0), out)
        true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      if refuse then false
      else
        fill(coordinates(0), out)
        true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      if refuse then Double.PositiveInfinity
      else
        fill(coordinates(0), out)
        out.energy

  private final class OffsetQuadratic(
      candidateJet: String = "valid", energyHigher: Boolean = false, priorTarget: Double = 0.2
  ) extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    val offset = 1e15
    val energyProbes = scala.collection.mutable.ArrayBuffer.empty[Double]
    def amplitudeCount: Int = 1
    private def data(x: Double): Double = offset + (x - priorTarget) * (x - priorTarget)
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      out.energy = data(x)
      out.gradient(0) = 2.0 * (x - priorTarget)
      out.hessian(0) = 2.0
      out.amplitudes(0) = x + 1.0
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double = data(grid.point(node)(0))
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      fill(coordinates(0), out)
      candidateJet match
        case "refused" => false
        case "nan" =>
          out.gradient(0) = Double.NaN
          true
        case "indefinite" =>
          out.hessian(0) = -2.0
          true
        case "higher" =>
          out.energy = java.lang.Math.nextUp(offset)
          true
        case _ => true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      energyProbes += coordinates(0)
      fill(coordinates(0), out)
      if energyHigher then out.energy = java.lang.Math.nextUp(offset)
      out.energy

  private final class OffsetQuartic extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    val energyProbes = scala.collection.mutable.ArrayBuffer.empty[Double]
    def amplitudeCount: Int = 1
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      val z = x - 0.2
      out.energy = 1e15 + z * z + 10.0 * z * z * z * z
      out.gradient(0) = 2.0 * z + 40.0 * z * z * z
      out.hessian(0) = 2.0 + 120.0 * z * z
      out.amplitudes(0) = x + 1.0
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double =
      val out = new ProfileJetBuffer(1, 1)
      fill(grid.point(node)(0), out)
      out.energy

    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      fill(coordinates(0), out)
      true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      energyProbes += coordinates(0)
      fill(coordinates(0), out)
      out.energy

  private final class ObservedObjective(val underlying: ShapeObjective) extends ShapeObjective:
    var nodeScores = 0L
    var nodeJets = 0L
    var jets = 0L
    var exact = 0L
    def grid: NodeGrid = underlying.grid
    def amplitudeCount: Int = underlying.amplitudeCount
    def scoreNode(node: Int): Double =
      nodeScores += 1L
      underlying.scoreNode(node)
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      nodeJets += 1L
      underlying.jetAtNode(node, out)
    def jetAt(coords: Array[Double], out: ProfileJetBuffer): Boolean =
      jets += 1L
      underlying.jetAt(coords, out)
    def energyAt(coords: Array[Double], out: ProfileJetBuffer): Double =
      exact += 1L
      underlying.energyAt(coords, out)

  private val chart2 = ShapeChart(("a", -2.0, 3.0), ("b", -1.0, 4.0))
  private val grid2 = NodeGrid(chart2, Vector(15, 15))

  test("node grid indexing round-trips"):
    val idx = new Array[Int](2)
    var node = 0
    while node < grid2.count do
      grid2.indicesInto(node, idx)
      assertEquals(grid2.indexOf(idx), node)
      node += 1
    assertEqualsDouble(grid2.coordinateOf(14, 0), 3.0, 1e-12)

  test("an interior optimum is found to high precision within the budget"):
    val bowl = new Bowl(grid2, Array(0.7, 1.9), Array(3.0, 0.4, 0.4, 2.0))
    val decoder = new ShapeDecoder(bowl, DecodeBudget(coarseStride = 2, maxNewtonSteps = 5, maxJets = 6, maxExactEvaluations = 6), None, 1.0)
    val counters = new DecoderCounters
    val result = decoder.decode(counters)
    // The true minimiser is perturbed from the target by the cosine term; verify stationarity instead.
    val buf = new ProfileJetBuffer(2, 1)
    bowl.jetAt(result.coordinates.toArray, buf)
    assert(math.abs(buf.gradient(0)) < 1e-6 && math.abs(buf.gradient(1)) < 1e-6, s"gradient ${buf.gradient.toVector} at ${result.coordinates}")
    assertEquals(result.status, DecodeStatus.Accepted)
    assert(counters.nodeScores <= 64 + 9, s"node scores ${counters.nodeScores}")
    assert(counters.jets <= 6)
    assert(counters.exactEvaluations <= 6)
    assert(result.conditionalSd.forall(_ > 0.0))

  test("an optimum outside the box lands on the boundary with a projected step"):
    val bowl = new Bowl(grid2, Array(5.0, 1.0), Array(2.0, 0.0, 0.0, 2.0))
    val decoder = new ShapeDecoder(
      bowl,
      DecodeBudget(coarseStride = 2, maxNewtonSteps = 5, maxJets = 6, stationarityStepTolerance = 1e-8),
      None,
      1.0
    )
    val counters = new DecoderCounters
    val result = decoder.decode(counters)
    val buf = new ProfileJetBuffer(2, 1)
    bowl.jetAt(result.coordinates.toArray, buf)
    assertEquals(result.status, DecodeStatus.Boundary, s"coordinates=${result.coordinates}, gradient=${buf.gradient.toVector}, steps=${result.newtonSteps}, jets=${counters.jets}, exact=${counters.exactEvaluations}")
    assertEqualsDouble(result.coordinates(0), 3.0, 1e-12)
    assert(math.abs(buf.gradient(1)) < 1e-6, "free coordinate is stationary on the face")

  test("indefinite curvature is reported and the parabolic fallback still improves on the node"):
    val bowl = new Bowl(grid2, Array(0.7, 1.9), Array(3.0, 0.4, 0.4, 2.0), indefinite = true)
    val decoder = new ShapeDecoder(bowl, DecodeBudget(coarseStride = 1), None, 1.0)
    val counters = new DecoderCounters
    val result = decoder.decode(counters)
    assertEquals(result.status, DecodeStatus.CurvatureNotPositive)
    assertEquals(result.newtonSteps, 0)
    assert(counters.fallbacks == 1)

  test("a prior pulls the estimate and both curvatures are reported"):
    val bowl = new Bowl(grid2, Array(0.7, 1.9), Array(1.0, 0.0, 0.0, 1.0))
    val prior = ShapePrior(Vector(2.0, 2.0), Vector(4.0, 0.0, 0.0, 4.0))
    val withPrior = new ShapeDecoder(bowl, DecodeBudget(maxNewtonSteps = 4, maxJets = 5), Some(prior), 1.0).decode(new DecoderCounters)
    val without = new ShapeDecoder(bowl, DecodeBudget(maxNewtonSteps = 4, maxJets = 5), None, 1.0).decode(new DecoderCounters)
    assert(withPrior.coordinates(0) > without.coordinates(0), "prior pulls a towards 2")
    assertEqualsDouble(withPrior.augmentedHessian(0) - withPrior.dataHessian(0), 8.0, 1e-12)
    assertEquals(without.augmentedHessian, without.dataHessian)

  test("weak identification and ambiguity are separate statuses"):
    val flat = new Bowl(grid2, Array(0.7, 1.9), Array(1e-4, 0.0, 0.0, 1e-4))
    val weak = new ShapeDecoder(flat, DecodeBudget(maxNewtonSteps = 4, maxJets = 5, weakSdLimit = Vector(0.5, 0.5)), None, 1.0).decode(new DecoderCounters)
    assertEquals(weak.status, DecodeStatus.WeaklyIdentified)
    val bowl = new Bowl(grid2, Array(0.7, 1.9), Array(3.0, 0.4, 0.4, 2.0))
    val ambiguous = new ShapeDecoder(bowl, DecodeBudget(maxNewtonSteps = 4, maxJets = 5, ambiguityEnergy = 1e9), None, 1.0).decode(new DecoderCounters)
    assertEquals(ambiguous.status, DecodeStatus.AmbiguousCells)
    assert(ambiguous.ambiguityGap > 0.0)

  test("three-dimensional charts decode with a hierarchical scan"):
    val chart3 = ShapeChart(("a", -1.0, 1.0), ("b", -1.0, 1.0), ("c", -1.0, 1.0))
    val grid3 = NodeGrid(chart3, Vector(7, 7, 7))
    val bowl = new Bowl(grid3, Array(0.2, -0.3, 0.4), Array(2.0, 0.1, 0.0, 0.1, 3.0, 0.2, 0.0, 0.2, 2.5))
    val counters = new DecoderCounters
    val result = new ShapeDecoder(bowl, DecodeBudget(coarseStride = 2, maxNewtonSteps = 4, maxJets = 5), None, 1.0).decode(counters)
    val buf = new ProfileJetBuffer(3, 1)
    bowl.jetAt(result.coordinates.toArray, buf)
    assert(buf.gradient.forall(g => math.abs(g) < 1e-6), s"gradient ${buf.gradient.toVector}")
    assertEquals(result.status, DecodeStatus.Accepted)
    assert(counters.nodeScores <= 64 + 27, s"node scores ${counters.nodeScores}")

  test("an exact accepted point without a terminal jet cannot use stale curvature for admission"):
    val objective = new Quartic
    val counters = new DecoderCounters
    val result = new ShapeDecoder(
      objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 1, weakSdLimit = Vector(0.7)),
      None,
      1.0
    ).decode(counters)
    assertEqualsDouble(result.coordinates(0), 0.10588235294117648, 1e-12)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assert(result.dataHessian.forall(_.isNaN))
    assert(result.augmentedHessian.forall(_.isNaN))
    assert(result.conditionalSd.forall(_.isNaN))
    assertEquals(counters.exactEvaluations, 1L)
    assertEquals(counters.candidateAttempts, 1L)
    assertEquals(counters.terminalVerifications, 0L)

  test("an accepted jet binds both reported curvatures to the returned point"):
    val objective = new Quartic
    val result = new ShapeDecoder(
      objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2),
      Some(ShapePrior(Vector(0.0), Vector(3.0))),
      1.0
    ).decode(new DecoderCounters)
    val actualDataHessian = objective.curvature(result.coordinates(0))
    assertEqualsDouble(result.dataHessian(0), actualDataHessian, 1e-12)
    assertEqualsDouble(result.augmentedHessian(0), actualDataHessian + 6.0, 1e-12)

  test("a reserved terminal jet verifies an energy-only move before admission"):
    val target = 0.2
    val objective = new ShapeObjective:
      val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
      def amplitudeCount: Int = 1
      private def fill(value: Double, out: ProfileJetBuffer): Unit =
        out.energy = 5.0 + (value - target) * (value - target)
        out.gradient(0) = 2.0 * (value - target)
        out.hessian(0) = 2.0
        out.amplitudes(0) = value + 1.0
        out.curvature = CurvatureStatus.PositiveDefinite
      def scoreNode(node: Int): Double =
        val value = grid.point(node)(0)
        5.0 + (value - target) * (value - target)
      def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
        fill(grid.point(node)(0), out)
        true
      def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
        fill(coordinates(0), out)
        true
      def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
        out.energy = 5.0 + (coordinates(0) - target) * (coordinates(0) - target)
        out.amplitudes(0) = coordinates(0) + 1.0
        out.energy
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 1),
      None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.coordinates(0), target, 1e-12)
    assertEqualsDouble(result.amplitudes(0), 1.2, 1e-12)
    assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
    assertEqualsDouble(result.conditionalSd(0), 1.0, 1e-12)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.exactEvaluations, 1L)
    assertEquals(counters.terminalVerifications, 1L)
    assertEquals(counters.candidateAttempts, 1L)

  test("a final full jet reaches a stationary candidate when exact evaluation is unavailable"):
    val counters = new DecoderCounters
    val result = new ShapeDecoder(
      new OffsetQuadratic,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 0),
      None,
      1.0
    ).decode(counters)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.coordinates(0), 0.2, 1e-12)
    assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(counters.candidateAttempts, 1L)
    assertEquals(counters.terminalVerifications, 0L)

  test("a rejected final full candidate preserves accepted state and caps"):
    val counters = new DecoderCounters
    val result = new ShapeDecoder(
      new OffsetQuadratic(candidateJet = "higher"),
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 0,
        maxCandidateAttempts = 1),
      None,
      1.0
    ).decode(counters)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(result.coordinates(0), 0.0, 1e-12)
    assertEqualsDouble(result.amplitudes(0), 1.0, 1e-12)
    assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(counters.candidateAttempts, 1L)
    assertEquals(counters.terminalVerifications, 0L)

  test("terminal verification retains a nonstationary budget refusal and current curvature"):
    val objective = new Quartic
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 1),
      None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(result.dataHessian(0), objective.curvature(result.coordinates(0)), 1e-12)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.terminalVerifications, 1L)

  test("equal rounded energy accepts a stationary candidate through either jet route"):
    for maxJets <- List(2, 3) do
      val objective = new OffsetQuadratic
      val counters = new DecoderCounters
      val result = new ShapeDecoder(objective,
        DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = maxJets, maxExactEvaluations = 1),
        None, 1.0).decode(counters)
      assertEquals(result.status, DecodeStatus.Accepted)
      assertEqualsDouble(result.coordinates(0), 0.2, 1e-12)
      assertEqualsDouble(result.amplitudes(0), 1.2, 1e-12)
      assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
      assertEqualsDouble(result.augmentedHessian(0), 2.0, 1e-12)
      assertEqualsDouble(result.conditionalSd(0), 1.0, 1e-12)
      assertEquals(result.newtonSteps, 1)
      assertEquals(counters.newtonSteps, 1L)
      assertEquals(counters.candidateAttempts, 1L)
      assertEquals(counters.jets, 2L)
      assertEquals(counters.exactEvaluations, if maxJets == 2 then 1L else 0L)
      assertEquals(counters.terminalVerifications, if maxJets == 2 then 1L else 0L)

  test("equal rounded quartic energy refuses a nonstationary move and preserves halving"):
    val objective = new OffsetQuartic
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 2,
        maxCandidateAttempts = 2), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(objective.energyProbes(0), 0.10588235294117647, 1e-12)
    assertEqualsDouble(objective.energyProbes(1), 0.052941176470588235, 1e-12)
    assertEqualsDouble(result.coordinates(0), 0.0, 1e-12)
    assertEqualsDouble(result.amplitudes(0), 1.0, 1e-12)
    assertEqualsDouble(result.dataHessian(0), 6.8, 1e-12)
    assertEqualsDouble(result.augmentedHessian(0), 6.8, 1e-12)
    assertEqualsDouble(result.conditionalSd(0), math.sqrt(2.0 / 6.8), 1e-12)
    assertEquals(result.newtonSteps, 0)
    assertEquals(counters.candidateAttempts, 2L)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.exactEvaluations, 2L)
    assertEquals(counters.terminalVerifications, 1L)

  test("equal energy candidate requires a finite positive and matching full jet"):
    for behavior <- List("refused", "nan", "indefinite", "higher") do
      val objective = new OffsetQuadratic(candidateJet = behavior)
      val counters = new DecoderCounters
      val result = new ShapeDecoder(objective,
        DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 1,
          maxCandidateAttempts = 1), None, 1.0).decode(counters)
      assertEquals(result.status, DecodeStatus.BudgetExceeded, behavior)
      assertEqualsDouble(result.coordinates(0), 0.0, 1e-12)
      assertEqualsDouble(result.amplitudes(0), 1.0, 1e-12)
      assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
      assertEquals(counters.jets, 2L)
      assertEquals(counters.terminalVerifications, 1L)
      assertEquals(counters.exactEvaluations, 1L)
    for (objective, maxJets) <- List(
      (new OffsetQuadratic, 1),
      (new OffsetQuadratic(energyHigher = true), 2)
    ) do
      val counters = new DecoderCounters
      val result = new ShapeDecoder(objective,
        DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = maxJets, maxExactEvaluations = 1,
          maxCandidateAttempts = 1), None, 1.0).decode(counters)
      assertEquals(result.status, DecodeStatus.BudgetExceeded)
      assertEqualsDouble(result.coordinates(0), 0.0, 1e-12)
      assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
      assertEquals(counters.jets, 1L)
    val higherJet = new OffsetQuadratic(candidateJet = "higher")
    val higherCounters = new DecoderCounters
    val higher = new ShapeDecoder(higherJet,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 3, maxExactEvaluations = 0,
        maxCandidateAttempts = 1), None, 1.0).decode(higherCounters)
    assertEquals(higher.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(higher.coordinates(0), 0.0, 1e-12)
    assertEqualsDouble(higher.amplitudes(0), 1.0, 1e-12)
    assertEquals(higherCounters.jets, 2L)
    assertEquals(higherCounters.exactEvaluations, 0L)

  test("equal energy stationarity uses prior augmentation and retains identification gates"):
    val objective = new OffsetQuadratic
    val prior = ShapePrior(Vector(0.0), Vector(1.0))
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 1),
      Some(prior), 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.coordinates(0), 0.1, 1e-12)
    assertEqualsDouble(result.dataHessian(0), 2.0, 1e-12)
    assertEqualsDouble(result.augmentedHessian(0), 4.0, 1e-12)
    assertEqualsDouble(result.conditionalSd(0), 1.0, 1e-12)
    val weak = new ShapeDecoder(new OffsetQuadratic,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 1,
        weakSdLimit = Vector(0.5)), None, 1.0).decode(new DecoderCounters)
    assertEquals(weak.status, DecodeStatus.WeaklyIdentified)
    assertEqualsDouble(weak.coordinates(0), 0.2, 1e-12)

    val boundaryObjective = new ShapeObjective:
      val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
      def amplitudeCount: Int = 1
      def scoreNode(node: Int): Double = if node == 1 then 1e15 else 1e15 + 1.0
      private def fill(x: Double, out: ProfileJetBuffer): Unit =
        out.energy = 1e15
        out.gradient(0) = -2.0
        out.hessian(0) = 2.0
        out.amplitudes(0) = x + 1.0
        out.curvature = CurvatureStatus.PositiveDefinite
      def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
        fill(grid.point(node)(0), out)
        true
      def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
        fill(coordinates(0), out)
        true
      def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
        fill(coordinates(0), out)
        out.energy
    val boundary = new ShapeDecoder(boundaryObjective,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 2, maxExactEvaluations = 1),
      None, 1.0).decode(new DecoderCounters)
    assertEquals(boundary.status, DecodeStatus.Boundary)
    assertEqualsDouble(boundary.coordinates(0), 1.0, 1e-12)

  test("exhausted refinement and rejected candidate caps are explicit and counted"):
    val noEvaluationCounters = new DecoderCounters
    val noEvaluation = new ShapeDecoder(
      new Quartic,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 2, maxJets = 1, maxExactEvaluations = 0),
      None,
      1.0
    ).decode(noEvaluationCounters)
    assertEquals(noEvaluation.status, DecodeStatus.BudgetExceeded)
    assertEquals(noEvaluation.budgetExit, Some(DecodeBudgetExit.RemainingEvaluationQuota))
    assertEquals(noEvaluation.newtonSteps, 0)
    assertEquals(noEvaluationCounters.candidateAttempts, 0L)

    val rejectedCounters = new DecoderCounters
    val rejected = new ShapeDecoder(
      new RejectingLineSearch,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 1, maxExactEvaluations = 4, maxCandidateAttempts = 4),
      None,
      1.0
    ).decode(rejectedCounters)
    assertEquals(rejected.status, DecodeStatus.BudgetExceeded)
    assertEquals(rejected.budgetExit, Some(DecodeBudgetExit.CandidateAttemptCap))
    assertEqualsDouble(rejected.coordinates(0), 0.0, 1e-12)
    assertEqualsDouble(rejected.dataHessian(0), 2.0, 1e-12)
    assertEquals(rejectedCounters.candidateAttempts, 4L)
    assertEquals(rejectedCounters.exactEvaluations, 4L)

  test("budget exits reset per decode and observation delegates exactly once"):
    val observed = new ObservedObjective(new Quartic)
    val decoder = new ShapeDecoder(observed,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 1, maxExactEvaluations = 0), None, 1.0)
    val first = decoder.decode(new DecoderCounters)
    assertEquals(first.budgetExit, Some(DecodeBudgetExit.RemainingEvaluationQuota))
    val second = decoder.decode(new DecoderCounters)
    assertEquals(second.budgetExit, Some(DecodeBudgetExit.RemainingEvaluationQuota))
    assertEquals(observed.nodeJets, 2L)
    assertEquals(observed.jets, 0L)
    assertEquals(observed.exact, 0L)

  test("node ranking, ambiguity inputs and fallback use the data-plus-prior objective"):
    val objective = new Quartic
    val prior = ShapePrior(Vector(1.0), Vector(100.0))
    val withPriorDecoder = new ShapeDecoder(objective, DecodeBudget(coarseStride = 1, maxNewtonSteps = 0), Some(prior), 1.0)
    val withPrior = withPriorDecoder.decode(new DecoderCounters)
    val withoutPriorDecoder = new ShapeDecoder(objective, DecodeBudget(coarseStride = 1, maxNewtonSteps = 0), None, 1.0)
    val withoutPrior = withoutPriorDecoder.decode(new DecoderCounters)
    assertEquals(withPrior.node, 2)
    assertEquals(withoutPrior.node, 1)
    var node = 0
    while node < objective.grid.count do
      assertEqualsDouble(withPriorDecoder.lastNodeEnergies(node), objective.scoreNode(node), 1e-12)
      assertEqualsDouble(withoutPriorDecoder.lastAugmentedNodeEnergies(node), withoutPriorDecoder.lastNodeEnergies(node), 1e-12)
      node += 1

  test("fallback terminal verification reports curvature at its returned point"):
    val objective = new Quartic(nodeCurvature = Some(-1.0))
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, DecodeBudget(coarseStride = 1), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.CurvatureNotPositive)
    assertEqualsDouble(result.dataHessian(0), objective.curvature(result.coordinates(0)), 1e-12)
    assertEquals(counters.terminalVerifications, 1L)
    assertEquals(counters.jets, 2L)
    assertEquals(counters.exactEvaluations, 1L)

  test("nonfinite and refused node banks return typed empty results without stale worker state"):
    val infinite = new ShapeDecoder(new InfiniteObjective, DecodeBudget(coarseStride = 1), None, 1.0).decode(new DecoderCounters)
    assertEquals(infinite.status, DecodeStatus.NoAdmissibleNode)
    assertEquals(infinite.node, -1)
    assert(infinite.coordinates.forall(_.isNaN))
    assert(infinite.amplitudes.forall(_.isNaN))

    val refusingObjective = new RefusingObjective
    val refusingDecoder = new ShapeDecoder(refusingObjective, DecodeBudget(coarseStride = 1), None, 1.0)
    val refused = refusingDecoder.decode(new DecoderCounters)
    assertEquals(refused.status, DecodeStatus.NoAdmissibleNode)
    assertEquals(refused.node, 1)
    assert(refused.amplitudes.forall(_.isNaN))
    refusingObjective.refuse = false
    val accepted = refusingDecoder.decode(new DecoderCounters)
    assertEquals(accepted.status, DecodeStatus.Accepted)
    assertEquals(accepted.amplitudes, Vector(2.0))

    val family = GaussianFamily.Default
    val step = PositiveSeconds(0.2).fold(error => fail(error.message), identity)
    val basis = HrfKernelBasis
      .compile(KernelBasisSpec(family, step, Vector(26, 21), tolerance = 1e-3, maxRank = 40))
      .fold(error => fail(error.message), identity)
    val m = basis.rank
    val gram = Array.tabulate(m * m)(i => if i / m == i % m then 1.0 else 0.0)
    val objective = new GramConditionObjective(new GramConditionJets(gram, 1, m, 2), basis, NodeGrid(family.chart, Vector(3, 3)))
    val decoder = new ShapeDecoder(objective, DecodeBudget(), None, 1.0)
    objective.pointAt(Array.fill(m)(Double.NaN), Double.NaN)
    val invalid = decoder.decode(new DecoderCounters)
    assertEquals(invalid.status, DecodeStatus.NoAdmissibleNode)
    assertEquals(invalid.node, -1)
    assert(invalid.dataHessian.forall(_.isNaN))
    objective.pointAt(Array.fill(m)(0.0), 1.0)
    val valid = decoder.decode(new DecoderCounters)
    assert(valid.status != DecodeStatus.NoAdmissibleNode)
    assert(valid.coordinates.forall(value => !value.isNaN && !value.isInfinite))
    assert(valid.amplitudes.forall(value => !value.isNaN && !value.isInfinite))

  private final class CornerTrap(refuseCenter: Boolean = false) extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -3.0, 3.0)), Vector(2))
    val amplitudeCount = 1
    var continuousJets = 0
    def scoreNode(node: Int): Double = 1.0 - math.cos(grid.point(node)(0) - 0.2)
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      out.energy = 1.0 - math.cos(x - 0.2)
      out.gradient(0) = math.sin(x - 0.2)
      out.hessian(0) = math.cos(x - 0.2)
      out.amplitudes(0) = x
      out.curvature = CurvatureStatus.PositiveDefinite
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      continuousJets += 1
      fill(coordinates(0), out)
      !(refuseCenter && coordinates(0) == 0.0)
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      fill(coordinates(0), out)
      out.energy

  test("an opt-in center probe escapes corner curvature and remains charged and terminally verified"):
    val old = new ShapeDecoder(new CornerTrap, DecodeBudget(maxNewtonSteps = 6, maxJets = 8), None, 1.0)
      .decode(new DecoderCounters)
    assertEquals(old.status, DecodeStatus.CurvatureNotPositive)
    val objective = new CornerTrap
    val budget = DecodeBudget(maxNewtonSteps = 6, maxJets = 8, initialization = DecodeInitialization.ChartCenterProbe)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, budget, None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.coordinates.head, 0.2, 1e-9)
    assertEqualsDouble(result.amplitudes.head, result.coordinates.head, 1e-15)
    assertEqualsDouble(result.dataHessian.head, math.cos(result.coordinates.head - 0.2), 1e-15)
    assertEquals(counters.nodeScores, 2L)
    assertEquals(counters.jets, 1L + objective.continuousJets)
    assertEquals(counters.candidateAttempts, objective.continuousJets.toLong)
    assert(counters.jets <= budget.maxJets)
    assertEquals(counters.fallbacks, 0L)

  test("center initialization cannot spend an exhausted jet budget or turn a failed probe into acceptance"):
    val exhausted = new CornerTrap
    val counters = new DecoderCounters
    val result = new ShapeDecoder(exhausted,
      DecodeBudget(maxJets = 1, initialization = DecodeInitialization.ChartCenterProbe), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.CurvatureNotPositive)
    assertEquals(exhausted.continuousJets, 0)
    assertEquals(counters.jets, 1L)
    val refusing = new CornerTrap(refuseCenter = true)
    val refused = new ShapeDecoder(refusing,
      DecodeBudget(maxJets = 2, initialization = DecodeInitialization.ChartCenterProbe), None, 1.0).decode(new DecoderCounters)
    assertEquals(refused.status, DecodeStatus.CurvatureNotPositive)
    assertEqualsDouble(refused.coordinates.head, 3.0, 1e-15)
    assertEqualsDouble(refused.amplitudes.head, 3.0, 1e-15)

  test("center probe selection includes the prior and has distinct reproducible policy provenance"):
    val objective = new CornerTrap
    val prior = ShapePrior(Vector(3.0), Vector(20.0))
    val base = DecodeBudget(maxNewtonSteps = 0, maxJets = 2)
    val probed = base.copy(initialization = DecodeInitialization.ChartCenterProbe)
    val result = new ShapeDecoder(objective, probed, Some(prior), 1.0).decode(new DecoderCounters)
    assertEqualsDouble(result.coordinates.head, 3.0, 1e-15)
    assertEqualsDouble(result.amplitudes.head, 3.0, 1e-15)
    assertEqualsDouble(result.dataHessian.head, math.cos(2.8), 1e-15)
    val original = ConditionProfileProvenance.budgetCanonical(base)
    assert(original.startsWith("decode-budget/v2|"))
    assert(original.endsWith("|initialization=bank-node"))
    assertEquals(ConditionProfileProvenance.budgetCanonical(probed), original.stripSuffix("|initialization=bank-node") + "|initialization=chart-center-probe")

  test("bounded independent search escapes negative curvature and charges one voxel and every oracle call"):
    val objective = new CornerTrap
    val budget = DecodeBudget(maxNewtonSteps = 6, maxJets = 161, maxCandidateAttempts = 30,
      initialization = DecodeInitialization.BoundedMultistart)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, budget, None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.coordinates.head, 0.2, 1e-8)
    assertEqualsDouble(result.amplitudes.head, result.coordinates.head, 1e-15)
    assertEqualsDouble(result.dataHessian.head, math.cos(result.coordinates.head - 0.2), 1e-15)
    val receipt = result.search.get
    assertEquals(receipt.requestedStarts, 3)
    assertEquals(receipt.trajectories.length, 3)
    assertEquals(counters.voxels, 1L)
    assertEquals(counters.nodeScores, 2L)
    assertEquals(counters.jets, objective.continuousJets.toLong)
    assert(counters.jets > receipt.trajectories.map(_.result.work.evaluations.toLong).sum)
    assert(counters.jets <= budget.maxJets)
    assert(counters.newtonSteps <= budget.maxNewtonSteps)
    assert(counters.terminalVerifications >= 1)
    assert(ConditionProfileProvenance.budgetCanonical(budget).endsWith("|initialization=bounded-multistart/v3"))

  test("a jet budget too small for independent starts remains a coherent charged refusal"):
    val counters = new DecoderCounters
    val result = new ShapeDecoder(new CornerTrap,
      DecodeBudget(maxJets = 1, initialization = DecodeInitialization.BoundedMultistart), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assertEquals(result.budgetExit, Some(DecodeBudgetExit.SearchStartJetQuota))
    assertEquals(counters.jets, 1L)
    assertEquals(counters.voxels, 1L)
    assertEquals(result.search.get.trajectories.length, 0)
    assertEqualsDouble(result.amplitudes.head, result.coordinates.head, 1e-15)

  test("refused search oracles cannot fall through to a successful bank-only fit"):
    val base = new CornerTrap
    val objective = new ShapeObjective:
      val grid = base.grid
      val amplitudeCount = base.amplitudeCount
      def scoreNode(node: Int): Double = base.scoreNode(node)
      def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean = base.jetAtNode(node, out)
      def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean = false
      def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double = Double.PositiveInfinity
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective,
      DecodeBudget(maxJets = 41, initialization = DecodeInitialization.BoundedMultistart), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.NoAdmissibleNode)
    assertEquals(result.search.get.trajectories.length, 3)
    assert(result.search.get.trajectories.forall(_.result.point.isEmpty))
    assertEquals(counters.jets, 4L)

  test("bounded search ranks the prior-augmented objective but preserves data-curvature refusal"):
    val prior = ShapePrior(Vector(3.0), Vector(20.0))
    val result = new ShapeDecoder(new CornerTrap,
      DecodeBudget(maxNewtonSteps = 6, maxJets = 161, maxCandidateAttempts = 30,
        initialization = DecodeInitialization.BoundedMultistart), Some(prior), 1.0).decode(new DecoderCounters)
    assert(result.coordinates.head > 2.9)
    assertEquals(result.status, DecodeStatus.WeaklyIdentified)
    assert(result.dataHessian.head < 0.0)
    assert(result.augmentedHessian.head > 0.0)

  test("independent trajectories retain a better well rather than the center's negative curvature"):
    val objective = new ShapeObjective:
      val grid = NodeGrid(ShapeChart(("x", -2.0, 2.0)), Vector(2))
      val amplitudeCount = 1
      private def fill(x: Double, out: ProfileJetBuffer): Unit =
        out.energy = math.pow(x * x - 1.0, 2) + 0.2 * x
        out.gradient(0) = 4.0 * x * (x * x - 1.0) + 0.2
        out.hessian(0) = 12.0 * x * x - 4.0
        out.amplitudes(0) = x
      def scoreNode(node: Int): Double =
        val x = grid.point(node)(0)
        math.pow(x * x - 1.0, 2) + 0.2 * x
      def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
        fill(grid.point(node)(0), out)
        true
      def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
        fill(coordinates(0), out)
        true
      def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
        fill(coordinates(0), out)
        out.energy
    val result = new ShapeDecoder(objective,
      DecodeBudget(maxNewtonSteps = 6, maxJets = 161, maxCandidateAttempts = 30,
        initialization = DecodeInitialization.BoundedMultistart), None, 1.0).decode(new DecoderCounters)
    assertEquals(result.status, DecodeStatus.Accepted)
    assert(result.coordinates.head < -1.0)
    assert(result.energy < -0.2)
    assert(result.search.get.trajectories.exists(_.result.point.exists(_.coordinates.head > 0.5)))

  test("first-order search includes prior, charges failures and always requires a full terminal jet"):
    for behavior <- Vector("valid", "refused", "incomplete", "terminal-refused") do
      var gradients = 0L
      var fullJets = 0L
      val objective = new FirstOrderShapeObjective:
        val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(2))
        val amplitudeCount = 1
        def scoreNode(node: Int): Double = math.pow(grid.point(node)(0) - 0.2, 2)
        def gradientAt(x: Array[Double], out: ProfileGradientBuffer): Boolean =
          gradients += 1L
          out.energy = math.pow(x(0) - 0.2, 2)
          if behavior != "incomplete" then out.gradient(0) = 2.0 * (x(0) - 0.2)
          out.amplitudes(0) = x(0)
          behavior != "refused"
        def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean = jetAt(grid.point(node).coordinates.toArray, out)
        def jetAt(x: Array[Double], out: ProfileJetBuffer): Boolean =
          fullJets += 1L
          out.energy = math.pow(x(0) - 0.2, 2)
          out.gradient(0) = 2.0 * (x(0) - 0.2)
          out.hessian(0) = 2.0
          out.amplitudes(0) = x(0)
          out.curvature = CurvatureStatus.PositiveDefinite
          behavior != "terminal-refused"
        def energyAt(x: Array[Double], out: ProfileJetBuffer): Double =
          jetAt(x, out)
          out.energy
      val counters = new DecoderCounters
      val result = new ShapeDecoder(objective,
        DecodeBudget(maxNewtonSteps = 4, maxJets = 81, initialization = DecodeInitialization.BoundedMultistart),
        Some(ShapePrior(Vector(0.6), Vector(1.0))), 1.0).decode(counters)
      assertEquals(counters.firstOrderAttempts, gradients)
      assertEquals(counters.jets, gradients + fullJets)
      assert(counters.jets <= 81L)
      assert(fullJets >= 1L)
      assertEquals(result.search.get.trajectories.map(_.result.work.evaluations.toLong).sum, gradients)
      if behavior == "valid" then
        assertEquals(result.status, DecodeStatus.Accepted)
        assertEqualsDouble(result.coordinates.head, 0.4, 1e-9)
        assertEqualsDouble(result.dataHessian.head, 2.0, 0.0)
        assertEqualsDouble(result.augmentedHessian.head, 4.0, 0.0)
      else assert(result.status != DecodeStatus.Accepted)
