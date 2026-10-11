package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.ShapeChart

class RoutedShapeDecoderSuite extends munit.FunSuite:
  private final class Quadratic(val referenceCandidates: Vector[Int], indefinite: Boolean = false) extends RoutedShapeObjective:
    val chart = ShapeChart(("x", -1.0, 1.0))
    val points = TrialReferencePoints(chart, Vector(Vector(-0.75), Vector(0.25), Vector(0.75)))
    var refusedNodes = Set.empty[Int]
    var scored = Vector.empty[Int]
    var jetNodes = Vector.empty[Int]
    def referenceCount: Int = points.count
    def referenceCoordinatesInto(node: Int, out: Array[Double]): Unit = points.coordinatesInto(node, out)
    def amplitudeCount: Int = 1
    private def energy(x: Double): Double = 5.0 + (if indefinite then -1.0 else 1.0) * (x - 0.25) * (x - 0.25)
    private def fill(x: Double, out: ProfileJetBuffer): Boolean =
      out.energy = energy(x)
      out.gradient(0) = (if indefinite then -2.0 else 2.0) * (x - 0.25)
      out.hessian(0) = if indefinite then -2.0 else 2.0
      out.amplitudes(0) = x + 2.0
      out.curvature = CurvatureStatus.PositiveDefinite
      true
    def scoreNode(node: Int): Double =
      scored = scored :+ node
      if refusedNodes(node) then Double.PositiveInfinity else energy(points.coordinates(node)(0))
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      jetNodes = jetNodes :+ node
      fill(points.coordinates(node)(0), out)
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean = fill(coordinates(0), out)
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      fill(coordinates(0), out)
      out.energy

  test("frozen routing scores only listed points and continuous terminal state agrees with analytic optimum"):
    val objective = new Quadratic(Vector(0, 2))
    val counters = new DecoderCounters
    val decoder = new ShapeDecoder(objective, DecodeBudget(maxNewtonSteps = 2, maxJets = 2), None, 1.0)
    val result = decoder.decode(counters)
    assertEquals(objective.scored, Vector(0, 2))
    assertEquals(objective.jetNodes, Vector(2))
    assertEquals(counters.nodeScores, 2L)
    assertEquals(counters.jets, 2L)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEqualsDouble(result.coordinates.head, 0.25, 1e-15)
    assertEqualsDouble(result.energy, 5.0, 1e-15)
    assertEqualsDouble(result.dataHessian.head, 2.0, 1e-15)
    assertEqualsDouble(result.amplitudes.head, 2.25, 1e-15)
    assert(decoder.lastNodeEnergies(1).isNaN)

  test("routing refusals never scan an unlisted admissible reference and workers recover on the next voxel"):
    val objective = new Quadratic(Vector(0, 2))
    objective.refusedNodes = Set(0, 2)
    val counters = new DecoderCounters
    val decoder = new ShapeDecoder(objective, DecodeBudget(), None, 1.0)
    assertEquals(decoder.decode(counters).status, DecodeStatus.NoAdmissibleNode)
    assertEquals(objective.scored, Vector(0, 2))
    assertEquals(counters.jets, 0L)
    objective.refusedNodes = Set.empty
    assertEquals(decoder.decode(counters).status, DecodeStatus.Accepted)
    assertEquals(counters.nodeScores, 4L)

  test("prior selection and ambiguity use only paid routed alternatives"):
    val objective = new Quadratic(Vector(0, 2))
    val counters = new DecoderCounters
    val decoder = new ShapeDecoder(objective, DecodeBudget(maxNewtonSteps = 0),
      Some(ShapePrior(Vector(-0.75), Vector(10.0))), 1.0)
    val result = decoder.decode(counters)
    assertEquals(result.node, 0)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(result.ambiguityGap, 21.75, 1e-14)
    assertEquals(counters.nodeScores, 2L)
    assertEquals(counters.candidateAttempts, 0L)

  test("explicit points do not invent grid fallback after indefinite curvature"):
    val objective = new Quadratic(Vector(0), indefinite = true)
    val counters = new DecoderCounters
    val result = new ShapeDecoder(objective, DecodeBudget(), None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.CurvatureNotPositive)
    assertEquals(counters.fallbacks, 0L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(objective.scored, Vector(0))

  test("invalid frozen routes and banks are rejected before scoring"):
    Vector(Vector.empty, Vector(0, 0), Vector(0, 1, 2), Vector(-1), Vector(3)).foreach: route =>
      val objective = new Quadratic(route)
      intercept[IllegalArgumentException](new ShapeDecoder(objective, DecodeBudget(), None, 1.0))
      assert(objective.scored.isEmpty)
    val chart = ShapeChart(("x", 0.0, 1.0))
    val points = TrialReferencePoints(chart, Vector.tabulate(9)(i => Vector(i / 8.0)))
    intercept[IllegalArgumentException](TrialReferenceDecodePolicy(points, Vector(0)))
