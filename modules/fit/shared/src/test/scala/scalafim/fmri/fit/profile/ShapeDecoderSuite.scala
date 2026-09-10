package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.ShapeChart

class ShapeDecoderSuite extends munit.FunSuite:

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
    val decoder = new ShapeDecoder(bowl, DecodeBudget(coarseStride = 2, maxNewtonSteps = 3, maxJets = 3, maxExactEvaluations = 6), None, 1.0)
    val counters = new DecoderCounters
    val result = decoder.decode(counters)
    // The true minimiser is perturbed from the target by the cosine term; verify stationarity instead.
    val buf = new ProfileJetBuffer(2, 1)
    bowl.jetAt(result.coordinates.toArray, buf)
    assert(math.abs(buf.gradient(0)) < 1e-6 && math.abs(buf.gradient(1)) < 1e-6, s"gradient ${buf.gradient.toVector} at ${result.coordinates}")
    assertEquals(result.status, DecodeStatus.Accepted)
    assert(counters.nodeScores <= 64 + 9, s"node scores ${counters.nodeScores}")
    assert(counters.jets <= 3)
    assert(counters.exactEvaluations <= 6)
    assert(result.conditionalSd.forall(_ > 0.0))

  test("an optimum outside the box lands on the boundary with a projected step"):
    val bowl = new Bowl(grid2, Array(5.0, 1.0), Array(2.0, 0.0, 0.0, 2.0))
    val decoder = new ShapeDecoder(bowl, DecodeBudget(coarseStride = 2, maxNewtonSteps = 3, maxJets = 3), None, 1.0)
    val result = decoder.decode(new DecoderCounters)
    assertEquals(result.status, DecodeStatus.Boundary)
    assertEqualsDouble(result.coordinates(0), 3.0, 1e-12)
    val buf = new ProfileJetBuffer(2, 1)
    bowl.jetAt(result.coordinates.toArray, buf)
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
    val withPrior = new ShapeDecoder(bowl, DecodeBudget(maxNewtonSteps = 3, maxJets = 3), Some(prior), 1.0).decode(new DecoderCounters)
    val without = new ShapeDecoder(bowl, DecodeBudget(maxNewtonSteps = 3, maxJets = 3), None, 1.0).decode(new DecoderCounters)
    assert(withPrior.coordinates(0) > without.coordinates(0), "prior pulls a towards 2")
    assertEqualsDouble(withPrior.augmentedHessian(0) - withPrior.dataHessian(0), 8.0, 1e-12)
    assertEquals(without.augmentedHessian, without.dataHessian)

  test("weak identification and ambiguity are separate statuses"):
    val flat = new Bowl(grid2, Array(0.7, 1.9), Array(1e-4, 0.0, 0.0, 1e-4))
    val weak = new ShapeDecoder(flat, DecodeBudget(weakSdLimit = Vector(0.5, 0.5)), None, 1.0).decode(new DecoderCounters)
    assertEquals(weak.status, DecodeStatus.WeaklyIdentified)
    val bowl = new Bowl(grid2, Array(0.7, 1.9), Array(3.0, 0.4, 0.4, 2.0))
    val ambiguous = new ShapeDecoder(bowl, DecodeBudget(ambiguityEnergy = 1e9), None, 1.0).decode(new DecoderCounters)
    assertEquals(ambiguous.status, DecodeStatus.AmbiguousCells)
    assert(ambiguous.ambiguityGap > 0.0)

  test("three-dimensional charts decode with a hierarchical scan"):
    val chart3 = ShapeChart(("a", -1.0, 1.0), ("b", -1.0, 1.0), ("c", -1.0, 1.0))
    val grid3 = NodeGrid(chart3, Vector(7, 7, 7))
    val bowl = new Bowl(grid3, Array(0.2, -0.3, 0.4), Array(2.0, 0.1, 0.0, 0.1, 3.0, 0.2, 0.0, 0.2, 2.5))
    val counters = new DecoderCounters
    val result = new ShapeDecoder(bowl, DecodeBudget(coarseStride = 2, maxNewtonSteps = 3, maxJets = 3), None, 1.0).decode(counters)
    val buf = new ProfileJetBuffer(3, 1)
    bowl.jetAt(result.coordinates.toArray, buf)
    assert(buf.gradient.forall(g => math.abs(g) < 1e-6), s"gradient ${buf.gradient.toVector}")
    assertEquals(result.status, DecodeStatus.Accepted)
    assert(counters.nodeScores <= 64 + 27, s"node scores ${counters.nodeScores}")
