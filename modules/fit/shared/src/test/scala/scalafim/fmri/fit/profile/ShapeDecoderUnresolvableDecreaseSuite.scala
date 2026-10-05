package scalafim.fmri.fit.profile

import scalafim.fmri.hrf.family.ShapeChart

/** The unresolvable-decrease stationarity rule at its exact binary64 boundaries.
  *
  * Every fixture is `E = offset + (x - target)^2 / 2` on `[-1, 1]` with nodes at -1, 0 and 1, so the
  * node at 0 is selected, the gradient there is `-target`, the Hessian is 1, the full Newton correction
  * is exactly `target` and its predicted decrease is exactly `target^2 / 2`. Targets are dyadic, so
  * each case sits on, below or above half an ULP of the node energy without rounding ambiguity.
  */
class ShapeDecoderUnresolvableDecreaseSuite extends munit.FunSuite:

  private final class HalfQuadratic(offset: Double, target: Double) extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    def amplitudeCount: Int = 1
    def energy(x: Double): Double = offset + 0.5 * (x - target) * (x - target)
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      out.energy = energy(x)
      out.gradient(0) = x - target
      out.hessian(0) = 1.0
      out.amplitudes(0) = x + 1.0
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double = energy(grid.point(node)(0))
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      fill(coordinates(0), out)
      true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      fill(coordinates(0), out)
      out.energy

  private val budget = DecodeBudget(coarseStride = 1, maxNewtonSteps = 2, maxJets = 3, maxExactEvaluations = 0)

  private def decode(objective: ShapeObjective, b: DecodeBudget = budget): (ShapeDecodeResult, DecoderCounters) =
    val counters = new DecoderCounters
    (new ShapeDecoder(objective, b, None, 1.0).decode(counters), counters)

  private val pow2 = (k: Int) => java.lang.Double.longBitsToDouble((1023L + k) << 52)

  test("Math.ulp is the exact binary64 spacing on every platform"):
    assertEquals(math.ulp(1.0), pow2(-52))
    assertEquals(math.ulp(5.0), pow2(-50))
    assertEquals(math.ulp(-5.0), pow2(-50))
    assertEquals(math.ulp(java.lang.Math.nextDown(8.0)), pow2(-50))
    assertEquals(math.ulp(8.0), pow2(-49))
    assertEquals(math.ulp(81.22678109225922), pow2(-46))
    assertEquals(math.ulp(1e15), 0.125)
    assertEquals(math.ulp(pow2(50)), 0.25)
    assertEquals(math.ulp(0.0), java.lang.Double.MIN_VALUE)
    assertEquals(math.ulp(java.lang.Double.MIN_VALUE), java.lang.Double.MIN_VALUE)
    assertEquals(math.ulp(java.lang.Double.MIN_NORMAL), java.lang.Double.MIN_VALUE)
    assertEquals(math.ulp(Double.MaxValue), pow2(971))
    assertEquals(math.ulp(Double.PositiveInfinity), Double.PositiveInfinity)
    assert(math.ulp(Double.NaN).isNaN)

  test("a correction predicting less than half an ULP is stationary without a candidate"):
    val target = pow2(-26) // predicted decrease 2^-53 < ulp(5) / 2 = 2^-51; correction > 1e-9
    val objective = HalfQuadratic(5.0, target)
    assertEquals(objective.energy(0.0), 5.0)
    val (result, counters) = decode(objective)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEquals(result.stationarity, Some(DecodeStationarity.UnresolvableDecrease))
    assertEquals(result.coordinates, Vector(0.0))
    assertEquals(result.newtonSteps, 0)
    assertEquals(counters.jets, 1L)
    assertEquals(counters.candidateAttempts, 0L)
    assertEquals(counters.exactEvaluations, 0L)
    assertEqualsDouble(result.dataHessian(0), 1.0, 0.0)

  test("a predicted decrease of exactly half an ULP is stationary"):
    val target = pow2(-25) // predicted decrease 2^-51 = ulp(5) / 2
    val objective = HalfQuadratic(5.0, target)
    assertEquals(objective.energy(0.0), 5.0)
    val (result, counters) = decode(objective)
    assertEquals(result.stationarity, Some(DecodeStationarity.UnresolvableDecrease))
    assertEquals(result.coordinates, Vector(0.0))
    assertEquals(counters.candidateAttempts, 0L)

  test("a predicted decrease just above half an ULP keeps iterating"):
    val target = pow2(-25) * (1.0 + pow2(-20)) // predicted decrease 2^-51 (1 + 2^-19 + 2^-40)
    val objective = HalfQuadratic(5.0, target)
    assertEquals(objective.energy(0.0), 5.0 + pow2(-50)) // the decrease to 5.0 is one representable ULP
    val (result, counters) = decode(objective)
    assertEquals(result.status, DecodeStatus.Accepted)
    assertEquals(result.stationarity, Some(DecodeStationarity.StepTolerance))
    assertEquals(result.coordinates, Vector(target))
    assertEquals(result.newtonSteps, 1)
    assertEquals(counters.candidateAttempts, 1L)
    assertEquals(counters.jets, 2L)

  test("the sqrt(stationarityStepTolerance) limit, not the energy precision, separates a large unresolvable step"):
    // At 2^50 the half ULP is 0.125, and a 0.25 correction predicts only 0.03125: unrepresentable, yet far from
    // converged. Only the step limit decides whether the decoder may stop here.
    val offset = pow2(50)
    val target = 0.25
    assertEquals(HalfQuadratic(offset, target).energy(0.0), offset)

    val (default, defaultCounters) = decode(HalfQuadratic(offset, target)) // limit sqrt(1e-9) ~ 3.2e-5 < 0.25
    assertEquals(default.stationarity, Some(DecodeStationarity.StepTolerance))
    assertEquals(default.coordinates, Vector(target))
    assertEquals(default.newtonSteps, 1)
    assertEquals(defaultCounters.candidateAttempts, 1L)

    val justBelow = budget.copy(stationarityStepTolerance = 0.06) // limit ~0.2449 < 0.25
    val (below, belowCounters) = decode(HalfQuadratic(offset, target), justBelow)
    assertEquals(below.stationarity, Some(DecodeStationarity.StepTolerance))
    assertEquals(below.coordinates, Vector(target))
    assertEquals(belowCounters.candidateAttempts, 1L)

    val atLimit = budget.copy(stationarityStepTolerance = 0.0625) // limit exactly 0.25; 0.25 > tolerance
    val (stopped, stoppedCounters) = decode(HalfQuadratic(offset, target), atLimit)
    assertEquals(stopped.stationarity, Some(DecodeStationarity.UnresolvableDecrease))
    assertEquals(stopped.coordinates, Vector(0.0))
    assertEquals(stopped.newtonSteps, 0)
    assertEquals(stoppedCounters.candidateAttempts, 0L)

  test("a resolvable decrease is never stopped by the rule, whatever the step"):
    val (result, counters) = decode(HalfQuadratic(5.0, 0.25)) // predicted decrease 0.03125 >> ulp(5)
    assertEquals(result.stationarity, Some(DecodeStationarity.StepTolerance))
    assertEquals(result.coordinates, Vector(0.25))
    assertEquals(counters.candidateAttempts, 1L)
