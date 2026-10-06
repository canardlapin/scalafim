package scalafim.fmri.fit.profile

import scalafim.fmri.design.event.{Event, EventTerm}
import scalafim.fmri.design.hrf.{ExpandedConditionDesign, HrfKernelBasis, KernelBasisSpec}
import scalafim.fmri.hrf.{Hrf, HrfDescriptor, HrfKind, PositiveSeconds, Seconds}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ParametricHrfFamily, ShapeChart, ShapePoint, ShapeSummary}

class CompactPairedComparisonSuite extends munit.FunSuite:
  private def checked[E, A](value: Either[E, A]): A = value.fold(error => fail(error.toString), identity)

  private final class CallbackFamily extends ParametricHrfFamily:
    private val base = GaussianFamily.Default
    var callback: () => Unit = () => ()
    def name: String = base.name
    def kind: HrfKind = base.kind
    def chart: ShapeChart = base.chart
    def horizon: PositiveSeconds = base.horizon
    def libraryNormalization: NormalizationRule = base.libraryNormalization
    def supports(rule: NormalizationRule): Boolean = base.supports(rule)
    def evalInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit = base.evalInto(lags, point, out)
    def jetInto(lags: Array[Double], point: ShapePoint, out: Array[Double]): Unit =
      callback()
      base.jetInto(lags, point, out)
    def scaleJetInto(rule: NormalizationRule, point: ShapePoint, out: Array[Double]): Unit = base.scaleJetInto(rule, point, out)
    def summaries(point: ShapePoint): ShapeSummary = base.summaries(point)
    def descriptor(point: ShapePoint): HrfDescriptor = base.descriptor(point)
    def toHrf(point: ShapePoint): Hrf = base.toHrf(point)

  private final class Fixture:
    val family = new CallbackFamily
    val basis = checked(HrfKernelBasis.compile(KernelBasisSpec(family, checked(PositiveSeconds(0.2)),
      Vector(8, 7), tolerance = 0.01, maxRank = 16, heldOutPoints = 1)))
    assert(basis.rank >= 3, "shape comparison fixture needs an identifiable multi-component kernel space")
    val frame = SamplingFrame(blockLens = Seq(60), tr = Seq(1.0))
    val term = EventTerm(Vector(Event.factor(Vector("A", "A", "A"), "condition")),
      Vector(5.0, 24.0, 42.0).map(Seconds(_)), blockIds = Vector.fill(3)(0))
    val expanded = checked(ExpandedConditionDesign.lower(term, frame, basis, Seconds(0.2)))
    val target = checked(family.chart.point(5.3, math.log(1.55)))
    val admission = checked(ObservedFamilyCertification.admitForCompact(expanded, term, frame, Seconds(0.2),
      None, None, Vector(target), ObservedFamilyRequirements(0.5, 1e10, 1e-9)))
    val prep = checked(CompactConditionPreparation.prepare(expanded, admission, None, None, term, frame, Seconds(0.2)))
    val grid = NodeGrid(family.chart, Vector(3, 3))
    val previous = Array(5.0, math.log(1.4))
    val candidate = target.coordinates.toArray
    val coefficients = new Array[Double](basis.rank)
    basis.coefficientsInto(target, new Array[Double](basis.fineCount), coefficients)
    val assembler = new CompactConditionJets(prep.rHat, prep.rank, prep.conditions, basis.rank, family.dimension)
    assembler.assemble(new Array[Double](prep.rank), 0.0, coefficients, 1)
    val targetDesign = assembler.valueDesign
    val response = Array.tabulate(prep.rank)(row => 1.7 * targetDesign(row) + 0.002 * math.sin(row * 0.31))
    val energy = response.iterator.map(value => value * value).sum + 0.5
    def objective(offset: Double = 0.0): CompactConditionObjective =
      val objective = new CompactConditionObjective(prep, grid)
      objective.enablePairedComparison()
      objective.pointAt(response, energy + offset)
      objective
    def jet(objective: CompactConditionObjective, coordinates: Array[Double]): ProfileJetBuffer =
      val out = new ProfileJetBuffer(2, 1)
      assert(objective.jetAt(coordinates, out))
      out
    def captured(objective: CompactConditionObjective): ProfileJetBuffer =
      val out = jet(objective, previous)
      objective.captureAcceptedProfile(previous, out)
      out

  private def copyJet(value: ProfileJetBuffer): ProfileJetBuffer =
    val out = new ProfileJetBuffer(value.dimension, value.amplitudeCount)
    out.energy = value.energy
    out.curvature = value.curvature
    System.arraycopy(value.gradient, 0, out.gradient, 0, value.gradient.length)
    System.arraycopy(value.hessian, 0, out.hessian, 0, value.hessian.length)
    System.arraycopy(value.amplitudes, 0, out.amplitudes, 0, value.amplitudes.length)
    out

  test("pointAt owns caller response and explicit reset invalidates previous snapshots"):
    val f = new Fixture
    val objective = new CompactConditionObjective(f.prep, f.grid)
    objective.enablePairedComparison()
    val borrowed = f.response.clone()
    objective.pointAt(borrowed, f.energy)
    java.util.Arrays.fill(borrowed, Double.NaN)
    val original = f.captured(objective)
    val reference = f.jet(f.objective(), f.previous)
    assertEquals(original.toJet, reference.toJet)
    val next = f.jet(objective, f.candidate)
    assert(objective.pairedCandidateAvailable(f.candidate, next))
    objective.pointAt(f.response, f.energy)
    val reset = f.jet(objective, f.candidate)
    assert(!objective.pairedCandidateAvailable(f.candidate, reset))
    assertEquals(objective.comparePairedCandidate(f.candidate, reset), Left(CompactComparisonFailure.SnapshotMismatch))

  test("bank jets bind counted original node designs before any candidate callback"):
    val f = new Fixture
    val objective = f.objective()
    val coordinates = f.grid.point(4).coordinates.toArray
    val node = new ProfileJetBuffer(2, 1)
    assert(objective.jetAtNode(4, node))
    objective.captureAcceptedProfile(coordinates, node)
    val current = f.jet(objective, coordinates)
    assert(objective.pairedCandidateAvailable(coordinates, current))
    assert(objective.comparePairedCandidate(coordinates, current).isLeft)

  test("wrong and stale full jets cannot bind comparison evidence"):
    val f = new Fixture
    val objective = f.objective()
    val old = f.captured(objective)
    val current = f.jet(objective, f.candidate)
    assert(!objective.pairedCandidateAvailable(f.previous, old))
    assert(!objective.pairedCandidateAvailable(f.previous, current))
    val wrongEnergy = copyJet(current)
    wrongEnergy.energy += 1.0
    val wrongBeta = copyJet(current)
    wrongBeta.amplitudes(0) += 0.1
    val wrongGradient = copyJet(current)
    wrongGradient.gradient(0) += 1.0
    val wrongHessian = copyJet(current)
    wrongHessian.hessian(0) += 1.0
    val wrongCurvature = copyJet(current)
    wrongCurvature.curvature = CurvatureStatus.GramNotPositiveDefinite
    for wrong <- Vector(wrongEnergy, wrongBeta, wrongGradient, wrongHessian, wrongCurvature) do
      assert(!objective.pairedCandidateAvailable(f.candidate, wrong))
      assertEquals(objective.comparePairedCandidate(f.candidate, wrong), Left(CompactComparisonFailure.SnapshotMismatch))
    val energyOnly = new ProfileJetBuffer(2, 1)
    objective.energyAt(f.candidate, energyOnly)
    assert(!objective.pairedCandidateAvailable(f.candidate, current))

  test("rejected comparison preserves accepted predictors through workspace reuse"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    val identical = f.jet(objective, f.previous)
    assert(objective.comparePairedCandidate(f.previous, identical).isLeft)
    val wrong = f.jet(objective, Array(4.0, math.log(2.0)))
    objective.comparePairedCandidate(Array(4.0, math.log(2.0)), wrong)
    val target = f.jet(objective, f.candidate)
    val proof = checked(objective.comparePairedCandidate(f.candidate, target))
    assertEquals(proof.previousCoordinates, f.previous.toVector)
    assertEquals(proof.candidateCoordinates, f.candidate.toVector)
    assert(proof.comparison.certifiesProfileDecrease)

  test("real compact comparison is invariant under a common additive energy offset"):
    val f = new Fixture
    val results = Vector(0.0, 1e8, 1e16).map: offset =>
      val objective = f.objective(offset)
      f.captured(objective)
      checked(objective.comparePairedCandidate(f.candidate, f.jet(objective, f.candidate))).comparison
    results.tail.foreach: result =>
      assertEqualsDouble(result.modelDifference.lower, results.head.modelDifference.lower, 0.0)
      assertEqualsDouble(result.modelDifference.upper, results.head.modelDifference.upper, 0.0)
      assertEquals(result.certifiesProfileDecrease, results.head.certifiesProfileDecrease)

  test("paid actual compact comparison charges before success and binds the coherent terminal jet"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    val candidate = f.jet(objective, f.candidate)
    val before = candidate.toJet
    val counters = new DecoderCounters
    val result = PaidProfileComparison.attempt(objective, f.candidate, candidate, 0, 2, counters)
    assertEquals(result.exactUsed, 1)
    assertEquals(counters.exactEvaluations, 1L)
    assertEquals(counters.pairedComparisons, 1L)
    assertEquals(candidate.toJet, before)
    val proof = result.proof.getOrElse(fail("actual descending model needs a paid certificate"))
    assertEquals(proof.previousCoordinates, f.previous.toVector)
    assertEquals(proof.candidateCoordinates, f.candidate.toVector)
    assert(proof.comparison.certifiesProfileDecrease)

  test("paid unresolved comparison spends quota, preserves old state, and cannot exceed the cap"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    val identical = f.jet(objective, f.previous)
    val counters = new DecoderCounters
    val unresolved = PaidProfileComparison.attempt(objective, f.previous, identical, 0, 1, counters)
    assertEquals(unresolved.exactUsed, 1)
    assertEquals(unresolved.proof, None)
    assertEquals(counters.exactEvaluations, 1L)
    assertEquals(counters.pairedComparisons, 1L)
    val candidate = f.jet(objective, f.candidate)
    val capped = PaidProfileComparison.attempt(objective, f.candidate, candidate, unresolved.exactUsed, 1, counters)
    assertEquals(capped.exactUsed, 1)
    assertEquals(capped.proof, None)
    assertEquals(counters.exactEvaluations, 1L)
    val freshCounter = new DecoderCounters
    val proof = PaidProfileComparison.attempt(objective, f.candidate, candidate, 0, 1, freshCounter).proof
      .getOrElse(fail("refused transaction must retain the original accepted predictor"))
    assertEquals(proof.previousCoordinates, f.previous.toVector)

  test("wrong candidate bindings do not invoke a paid comparison"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    val wrong = f.jet(objective, f.candidate)
    wrong.energy += 1.0
    val counters = new DecoderCounters
    val result = PaidProfileComparison.attempt(objective, f.candidate, wrong, 0, 2, counters)
    assertEquals(result.exactUsed, 0)
    assertEquals(result.proof, None)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(counters.pairedComparisons, 0L)

  test("numerical refusal is charged before invocation and cap blocks invocation"):
    val counters = new DecoderCounters
    var calls = 0
    val refused = PaidProfileComparison.charge(0, 1, counters):
      calls += 1
      assertEquals(counters.exactEvaluations, 1L)
      assertEquals(counters.pairedComparisons, 1L)
      Left(CompactComparisonFailure.Numerical(gale.numeric.PairedResidualError.UnboundedArithmetic(
        gale.numeric.PairedResidualStage.ReturnedModels, 0, 0)))
    assertEquals(refused.exactUsed, 1)
    assertEquals(refused.proof, None)
    val capped = PaidProfileComparison.charge(1, 1, counters):
      calls += 1
      fail("comparison must not run after exact cap")
    assertEquals(capped.exactUsed, 1)
    assertEquals(capped.proof, None)
    assertEquals(calls, 1)
    assertEquals(counters.exactEvaluations, 1L)

  test("successful physical comparison sees its paid quota before the call"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    val candidate = f.jet(objective, f.candidate)
    val counters = new DecoderCounters
    val result = PaidProfileComparison.charge(0, 2, counters):
      assertEquals(counters.exactEvaluations, 1L)
      assertEquals(counters.pairedComparisons, 1L)
      objective.comparePairedCandidate(f.candidate, candidate)
    assert(result.proof.nonEmpty)
    assertEquals(result.exactUsed, 1)

  test("a family callback changing the response epoch cannot produce coherent comparison state"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    f.family.callback = () => objective.pointAt(f.response, f.energy)
    val out = new ProfileJetBuffer(2, 1)
    assert(!objective.jetAt(f.candidate, out))
    assert(!objective.pairedCandidateAvailable(f.candidate, out))
    f.family.callback = () => ()

  test("reentrant full and energy callbacks supersede and invalidate the outer stamp"):
    val f = new Fixture
    val objective = f.objective()
    f.captured(objective)
    var inner: Option[ProfileJetBuffer] = None
    f.family.callback = () =>
      f.family.callback = () => ()
      val nested = new ProfileJetBuffer(2, 1)
      assert(objective.jetAt(f.previous, nested))
      inner = Some(nested)
    val outer = new ProfileJetBuffer(2, 1)
    assert(!objective.jetAt(f.candidate, outer))
    assert(!objective.pairedCandidateAvailable(f.previous, inner.get))
    f.family.callback = () =>
      f.family.callback = () => ()
      val nested = new ProfileJetBuffer(2, 1)
      assert(objective.jetAt(f.candidate, nested))
      inner = Some(nested)
    val energy = new ProfileJetBuffer(2, 1)
    assertEquals(objective.energyAt(f.candidate, energy), Double.PositiveInfinity)
    assert(!objective.pairedCandidateAvailable(f.candidate, inner.get))

  test("storage receipt includes owned response, both snapshots and Gale scratch before allocation"):
    val f = new Fixture
    val budget = DecodeBudget(maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2)
    val runtime = new CompactConditionRuntime(f.prep, f.grid, budget, None, 1.0, NormalizationRule.Unnormalised)
    val k = f.prep.rank.toLong
    val c = f.prep.conditions.toLong
    val d = f.family.dimension.toLong
    assertEquals(runtime.comparisonWorkspaceReceipt.retainedDoubles, 2L * k * c + 4L * k + 4L * c + 3L * d + d * d)
    assertEquals(runtime.comparisonWorkspaceReceipt.estimatedBytes, runtime.comparisonWorkspaceReceipt.retainedDoubles * 8L)
    val prior = Some(ShapePrior(Vector(5.0, 0.2), Vector(1.0, 0.0, 0.0, 1.0)))
    val scalar = new CompactConditionRuntime(f.prep, f.grid, budget, prior, 1.0, NormalizationRule.Unnormalised)
    assert(!scalar.comparisonWorkspaceReceipt.enabled)
    assertEquals(scalar.comparisonWorkspaceReceipt.retainedDoubles, k)
    val noQuota = new CompactConditionRuntime(f.prep, f.grid, budget.copy(maxExactEvaluations = 0), None, 1.0,
      NormalizationRule.Unnormalised)
    assert(!noQuota.comparisonWorkspaceReceipt.enabled)

    val loose = new CompactConditionRuntime(f.prep, f.grid, budget.copy(stationarityStepTolerance = 1e-8),
      None, 1.0, NormalizationRule.Unnormalised)
    assert(!loose.comparisonWorkspaceReceipt.enabled)
    assertEquals(loose.comparisonWorkspaceReceipt.snapshotValues, 0L)
    assertEquals(loose.comparisonWorkspaceReceipt.comparisonScratchValues, 0L)
    val twoJets = new CompactConditionRuntime(f.prep, f.grid, budget.copy(maxJets = 2),
      None, 1.0, NormalizationRule.Unnormalised)
    assert(!twoJets.comparisonWorkspaceReceipt.enabled)
    intercept[IllegalArgumentException](runtime.comparisonWorkspaceReceipt.copy(snapshotValues = -1L))
    intercept[IllegalArgumentException](runtime.comparisonWorkspaceReceipt.copy(snapshotValues = Long.MaxValue))

  private final class UnsupportedRoundedBowl extends ShapeObjective:
    val grid = NodeGrid(ShapeChart(("x", -1.0, 1.0)), Vector(3))
    def amplitudeCount: Int = 1
    private def fill(x: Double, out: ProfileJetBuffer): Unit =
      out.energy = 1e16 + (x - 0.2) * (x - 0.2) + (if x == 0.0 then 0.0 else 64.0)
      out.gradient(0) = 2.0 * (x - 0.2)
      out.hessian(0) = 2.0
      out.amplitudes(0) = 1.0 + x
      out.curvature = CurvatureStatus.PositiveDefinite
    def scoreNode(node: Int): Double =
      val x = grid.point(node)(0)
      1e16 + (x - 0.2) * (x - 0.2)
    def jetAtNode(node: Int, out: ProfileJetBuffer): Boolean =
      fill(grid.point(node)(0), out)
      true
    def jetAt(coordinates: Array[Double], out: ProfileJetBuffer): Boolean =
      fill(coordinates(0), out)
      true
    def energyAt(coordinates: Array[Double], out: ProfileJetBuffer): Double =
      fill(coordinates(0), out)
      out.energy

  test("unsupported callbacks retain scalar refusal and spend no paired comparison quota"):
    val counters = new DecoderCounters
    val result = new ShapeDecoder(new UnsupportedRoundedBowl,
      DecodeBudget(coarseStride = 1, maxNewtonSteps = 1, maxJets = 3, maxExactEvaluations = 2, maxCandidateAttempts = 1),
      None, 1.0).decode(counters)
    assertEquals(result.status, DecodeStatus.BudgetExceeded)
    assertEqualsDouble(result.coordinates.head, 0.0, 0.0)
    assertEquals(counters.pairedComparisons, 0L)
    assertEquals(counters.pairedCertifiedMoves, 0L)
    assertEquals(result.pairedDecrease, None)

  test("Some prior keeps compact decoding on the scalar path"):
    val f = new Fixture
    val runtime = new CompactConditionRuntime(f.prep, f.grid,
      DecodeBudget(maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 2),
      Some(ShapePrior(Vector(5.0, 0.2), Vector(0.01, 0.0, 0.0, 0.01))), 1.0, NormalizationRule.Unnormalised)
    val counters = new DecoderCounters
    runtime.fit(Array.tabulate(60)(row => math.sin(row * 0.21)), 0, counters)
    assertEquals(counters.pairedComparisons, 0L)
    assertEquals(counters.pairedCertifiedMoves, 0L)

  test("zero exact quota disables compact comparisons before any candidate"):
    val f = new Fixture
    val runtime = new CompactConditionRuntime(f.prep, f.grid,
      DecodeBudget(maxNewtonSteps = 6, maxJets = 8, maxExactEvaluations = 0),
      None, 1.0, NormalizationRule.Unnormalised)
    val counters = new DecoderCounters
    val result = runtime.fit(Array.tabulate(60)(row => math.sin(row * 0.21)), 0, counters)
    assertEquals(counters.exactEvaluations, 0L)
    assertEquals(counters.pairedComparisons, 0L)
    assertEquals(result.decode.pairedDecrease, None)

  test("comparison provenance marker is restricted to eligible compact decoding"):
    val policy = ProfileDecodePolicy(Vector(3, 3), DecodeBudget(maxJets = 3), None, ExecutionBudget(1, 1))
    assert(ProfileFitIdentity.compactComparisonMarker("direct-condition-compact", policy).contains(CompactComparisonWorkspaceReceipt.PolicyId))
    assertEquals(ProfileFitIdentity.compactComparisonMarker("direct-condition-gram", policy), "")
    assertEquals(ProfileFitIdentity.compactComparisonMarker("trial-banded", policy), "")
    assertEquals(ProfileFitIdentity.compactComparisonMarker("direct-condition-compact", policy.copy(prior = Some(
      ShapePrior(Vector(5.0, 0.2), Vector(1.0, 0.0, 0.0, 1.0))))), "")
    assertEquals(ProfileFitIdentity.compactComparisonMarker("direct-condition-compact", policy.copy(
      budget = policy.budget.copy(stationarityStepTolerance = 1e-8))), "")
