package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import multivar.inference.{MonteCarloDraws, PermutationAction, RowCount}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class ComponentInferenceSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "component-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val planId = PlanId.derived(EstimandId("component-inference-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  private def exposure(identity: String) = EvidenceExposure.internal(ExposureReference(planId, identity, "fixture", ResultIdentity("component-result")))

  private final class Fixture(mapping: Vector[Int] = Vector.range(0, 8), unitCount: Int = 8, includeIntercept: Boolean = true):
    val rows = axis("confirmation", 8); val training = axis("training", 8)
    val units = axis("confirmation-units", unitCount); val trainUnits = axis("training-units", 8)
    val neural = axis("neural", 2); val target = axis("target", 2); val component = axis("component", 2)
    val u = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    val v = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    val e = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val f = Vector(1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0)
    val drift = Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val x = DMat.tabulate(8, 2)((i, j) => if j == 0 then u(i) else .5 * u(i) + math.sqrt(.75) * v(i))
    val yTrain = DMat.tabulate(8, 2)((i, j) => if j == 0 then 7.0 + 2.0 * x(i, 0) + 3.0 * x(i, 1) + 4.0 * drift(i)
      else -3.0 - x(i, 0) + 2.0 * x(i, 1) - 2.0 * drift(i))
    val y = DMat.tabulate(8, 2)((i, j) => yTrain(i, j) + (if j == 0 then .5 * e(i) else .25 * f(i)))
    val a = right(PatternFactors(neural, target, component, DMat.eye(2), DMat.eye(2), GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector(1.0, 1.0))); val metric = right(AxisValues(target, Vector(1.0, 2.0)))
    val artifact = right(PatternArtifact(a, right(TargetGeometry.continuous(target, unit, metric, Vector("all" -> unit))),
      CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "component-discovery", "frozen")), Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainUnits, Vector.range(0, 8))), artifact,
      right(FrozenProjection(neural, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)), "support", "none", "discovery-prep"))
    val snapshot = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, mapping)), "confirmation-prep"))
    val nuisanceMatrix = if includeIntercept then DMat.tabulate(8, 2)((i, j) => if j == 0 then 1.0 else drift(i)) else DMat.tabulate(8, 1)((i, _) => drift(i))
    val nuisance = right(ConfirmationNuisance(rows, nuisanceMatrix))
    val trainingNuisance = right(ConfirmationNuisance(training, nuisanceMatrix))
    val contract = C1Contract("fixed discovery", "paired association and incremental usefulness", "these independent units",
      Vector("association-0", "association-1", "incremental-0", "incremental-1"), "pending calibration", Vector("projections", "metric", "family"), Vector("association", "held-out loss"))
    val law = if mapping.distinct.size == 8 then ConfirmationErrorLaw.IndependentGaussian
      else
        val covariance = right(ResidualCovariance.fromFactors(rows, Vector.fill(8)(1.0), DMat.tabulate(8, unitCount)((i, j) => if mapping(i) == j then .25 else 0.0)))
        val bound = right(BoundConfirmationCovariance(rows, covariance, "known-independent-blocks", TemporalCovarianceStatus.KnownGaussian))
        ConfirmationErrorLaw.RepeatedSubjects(right(BoundRepeatedGaussian(snapshot.samples, bound)))
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, snapshot, exposure(snapshot.identity), contract, nuisance, law))
    val names = if includeIntercept then Vector("intercept", "drift") else Vector("drift")
    val plan = right(FrozenComponentConfirmation.freeze(design, contract.multiplicityFamily.take(2), contract.multiplicityFamily.drop(2), names, exposure(discovery.identity)))
    var trainingReads = 0; var confirmationReads = 0; var targetReads = 0
    private def operator(isTraining: Boolean) = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        if isTraining then trainingReads += 1 else confirmationReads += 1
        x.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = x.transposeApplyTo(input, output)
    val trainingBrain = right(Observations.fromOperator(training, neural, operator(true), value("training-brain"), source("training-brain")))
    val trainingTargets = right(MultiResponse.fromDense(training, target, yTrain, value("training-target"), source("training-target")))
    val brain = right(Observations.fromOperator(rows, neural, operator(false), value("confirmation-brain"), source("confirmation-brain")))
    val responses = right(MultiResponse.fromDense(rows, target, y, value("confirmation-target"), source("confirmation-target")))
    val poisonOperator = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        targetReads += 1
        throw IllegalStateException("preflight must not read")
    val poisonTargets = right(MultiResponse.fromOperator(rows, target, poisonOperator, value("poison"), source("poison")))
    def heads = ComponentConfirmation.fitHeads(plan, trainingBrain, trainingTargets, trainingNuisance)


  private def gamma(f: Fixture): DMat = DMat.tabulate(f.rows.size * f.target.size, f.rows.size * f.target.size): (i, j) =>
    if i / 2 != j / 2 then 0.0
    else if i % 2 == 0 && j % 2 == 0 then .25
    else if i % 2 == 1 && j % 2 == 1 then .0625
    else .06

  private def predictionReference(f: Fixture, heads: ComponentPredictionHeads, covariance: DMat,
      status: ComponentOutcomeCovarianceStatus = ComponentOutcomeCovarianceStatus.KnownConditionalGaussian,
      scope: ComponentOutcomeConditioning = ComponentOutcomeConditioning.DiscoveryHeadsAndConfirmationPredictors,
      brainIdentity: Option[EvidenceIdentity] = None, targetIdentity: Option[EvidenceIdentity] = None,
      account: Option[EvidenceExposure] = None, maximum: Long = 10000000L) =
    FrozenComponentPredictionReference.freeze(heads, f.rows.descriptor, f.target.descriptor,
      brainIdentity.getOrElse(f.brain.identity), targetIdentity.getOrElse(f.responses.identity), covariance, status, scope,
      "independent known response covariance; row-major targets", account.getOrElse(exposure(f.snapshot.identity)), maximum)

  private def associationReference(f: Fixture, draws: Int = 19, budget: ComponentInferenceBudget = ComponentInferenceBudget()) =
    FrozenComponentAssociationReference.freeze(f.plan, right(RankJointGaussian.declare(f.design, "independent spherical joint score law")),
      f.brain.identity, f.responses.identity, exposure(f.snapshot.identity), Seed.fromLong(73L), right(MonteCarloDraws(draws)), budget)

  test("independent R oracle: full target covariance and separately refitted component improvements"):
    val f = new Fixture
    val heads = right(f.heads)
    val reference = right(predictionReference(f, heads, gamma(f)))
    val result = right(ComponentInference.incremental(reference, f.brain, f.responses))
    assertEqualsDouble(result.arithmetic.meanImprovements(0), 4.5, 1e-10)
    assertEqualsDouble(result.arithmetic.meanImprovements(1), 12.75, 1e-10)
    val covariance = Vector(Vector(.28875, -.21), Vector(-.21, 1.75875))
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(result.knownMeanCovariance(i, j), covariance(i)(j), 1e-10)
    val expected = Vector((.53735463150511686, 3.4468042753241779, 5.5531957246758203),
      (1.32617872098748402, 10.1507374698011361, 15.3492625301988568))
    result.candidateCalculations.zip(expected).foreach: (actual, oracle) =>
      assertEqualsDouble(actual.standardError, oracle._1, 1e-10)
      assertEqualsDouble(actual.lower95, oracle._2, 1e-10)
      assertEqualsDouble(actual.upper95, oracle._3, 1e-10)
      assertEquals(actual.candidateDecision, ComponentGaussianPointDecision.AboveFixedFivePercentBoundary)
    assertEquals(f.trainingReads, 2)
    assertEquals(f.confirmationReads, 2)
    assert(result.pValues.isLeft && result.admittedC1.isLeft)
    assertEquals(result.release, ComponentInferenceRelease.PendingFrozenProtocol)

  test("independent R oracle: unequal units use actual per-row weights and keep cross-component covariance"):
    val f = new Fixture(Vector(0, 0, 0, 0, 0, 0, 1, 1), 2)
    val result = right(ComponentInference.incremental(right(predictionReference(f, right(f.heads), gamma(f))), f.brain, f.responses))
    assertEqualsDouble(result.arithmetic.meanImprovements(0), 3.3066243270259346, 1e-10)
    assertEqualsDouble(result.arithmetic.meanImprovements(1), 12.1726497308103738, 1e-10)
    val covariance = Vector(Vector(.162720146361994111, .043316150746190815),
      Vector(.043316150746190829, 2.345))
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(result.knownMeanCovariance(i, j), covariance(i)(j), 1e-10)
    assertEquals(result.arithmetic.independentUnits, f.units.descriptor)
    assertEquals(result.arithmetic.rowUnitOrdinals, Vector(0, 0, 0, 0, 0, 0, 1, 1))

  test("association preserves nuisance-adjusted correlations and common reconstructible provider actions"):
    val f = new Fixture
    val reference = right(associationReference(f))
    val result = right(ComponentInference.association(reference, f.brain, f.responses))
    assertEqualsDouble(result.arithmetic.correlations(0), 3.5 / math.sqrt(19.25), 1e-10)
    assertEqualsDouble(result.arithmetic.correlations(1), 1.5 / math.sqrt(3.0625), 1e-10)
    assertEquals(result.residualRows, 6)
    result.candidateCalculations.zip(result.arithmetic.correlations).foreach: (calculation, correlation) =>
      assertEqualsDouble(calculation.correlations.head, math.abs(correlation), 1e-10)
      assertEquals(calculation.receipts.head.consumed.value, 19)
      assert(calculation.receipts.head.pValue.value > 0.0)
    val ids = result.candidateCalculations.head.sampledCandidateIds
    assert(result.candidateCalculations.forall(_.sampledCandidateIds == ids))
    val action = PermutationAction.unrestricted(right(RowCount(6)))
    val transformations = ids.map(id => right(action.draw(reference.seed, id)).toIArray.toVector)
    assertEquals(transformations.distinct.size, 19)
    assert(transformations.forall(p => p != Vector.range(0, 6)))
    assertEquals(f.confirmationReads, 2)
    assert(result.admittedC1.isLeft && result.associationIntervals.isLeft)

  test("known within-unit dependence and off-diagonal target covariance match independent R"):
    val mapping = Vector(0, 0, 0, 0, 0, 0, 1, 1)
    val f = new Fixture(mapping, 2)
    val responseBlock = Vector(Vector(.25, .06), Vector(.06, .0625))
    val covariance = DMat.tabulate(16, 16): (i, j) =>
      val rowFactor = (if i / 2 == j / 2 then 1.0 else 0.0) +
        (if mapping(i / 2) == mapping(j / 2) then .0625 else 0.0)
      rowFactor * responseBlock(i % 2)(j % 2)
    val result = right(ComponentInference.incremental(right(predictionReference(f, right(f.heads), covariance)), f.brain, f.responses))
    val oracle = Vector(Vector(.168093085897722400, .06466763263361644),
      Vector(.064667632633616454, 2.58927083333333163))
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(result.knownMeanCovariance(i, j), oracle(i)(j), 1e-10)
    val intervals = Vector((.4099915680812502, 2.5030556196215827, 4.1101930344302868),
      (1.6091211369357286, 9.0188302556541977, 15.3264692059665428))
    result.candidateCalculations.zip(intervals).foreach: (actual, expected) =>
      assertEqualsDouble(actual.standardError, expected._1, 1e-10)
      assertEqualsDouble(actual.lower95, expected._2, 1e-10)
      assertEqualsDouble(actual.upper95, expected._3, 1e-10)

  test("complete finite association group uses inclusive extrema and an independent dot-product oracle"):
    val f = new Fixture
    val result = right(ComponentInference.association(right(associationReference(f, 719)), f.brain, f.responses))
    def permutations(values: Vector[Int]): Vector[Vector[Int]] =
      if values.isEmpty then Vector(Vector.empty)
      else values.flatMap(v => permutations(values.filterNot(_ == v)).map(v +: _))
    val group = permutations(Vector.range(0, 6))
    assertEquals(group.size, 720)
    var k = 0
    while k < 2 do
      val x = Vector.tabulate(6)(i => result.preparedScores(i, k))
      val y = Vector.tabulate(6)(i => result.preparedScores(i, 2 + k))
      val scale = math.sqrt(x.map(v => v * v).sum * y.map(v => v * v).sum)
      def magnitude(p: Vector[Int]): Double = math.abs(x.indices.map(i => x(i) * y(p(i))).sum / scale)
      val observed = magnitude(Vector.range(0, 6))
      // IEEE differences between a scalar dot oracle and QR/SVD can move a
      // mathematical tie by a few ULPs. Count strict/non-strict brackets;
      // identity is explicitly inclusive and the numerical p stays bracketed.
      val lower = group.count(p => magnitude(p) > observed + 1e-10) / 720.0
      val upper = group.count(p => magnitude(p) >= observed - 1e-10) / 720.0
      val receipt = result.candidateCalculations(k).receipts.head
      assert(receipt.pValue.value >= lower && receipt.pValue.value <= upper)
      assertEqualsDouble(receipt.pValue.value, (1.0 + receipt.exceedances) / 720.0, 1e-15)
      assertEquals(result.candidateCalculations(k).sampledCandidateIds.size, 719)
      k += 1

  test("one perfect member refuses the association family instead of silently omitting it"):
    val f = new Fixture
    val perfect = right(MultiResponse.fromDense(f.rows, f.target, f.x, value("perfect-inference"), source("perfect-inference")))
    val ref = right(FrozenComponentAssociationReference.freeze(f.plan, right(RankJointGaussian.declare(f.design, "joint Gaussian")),
      f.brain.identity, perfect.identity, exposure(f.snapshot.identity), Seed.fromLong(13L), right(MonteCarloDraws(19))))
    assert(ComponentInference.association(ref, f.brain, perfect).isLeft)

  test("complete2r candidate family retains bindings and cannot become corrected or released significance"):
    val f = new Fixture
    val association = right(ComponentInference.association(right(associationReference(f)), f.brain, f.responses))
    val prediction = right(ComponentInference.incremental(right(predictionReference(f, right(f.heads), gamma(f))), f.brain, f.responses))
    val family = right(ComponentInference.complete(f.plan, association, prediction))
    assertEquals(family.members, Vector("association-0", "association-1", "incremental-0", "incremental-1"))
    assert(family.correctedFamilyInference.isLeft && family.admittedC1.isLeft)
    val foreign = new Fixture
    val wrongPlan = right(FrozenComponentConfirmation.freeze(foreign.design, foreign.plan.associationMembers, foreign.plan.incrementalMembers,
      foreign.names, exposure(foreign.discovery.identity)))
    // An independently re-created but identical plan remains value-identical;
    // a changed exposure receipt changes the frozen source/plan identity.
    val changedPlan = right(FrozenComponentConfirmation.freeze(foreign.design, foreign.plan.associationMembers, foreign.plan.incrementalMembers,
      foreign.names, EvidenceExposure.internal(ExposureReference(planId, foreign.discovery.identity, "different-provenance", ResultIdentity("other")))))
    assertEquals(wrongPlan.identity, f.plan.identity)
    assert(ComponentInference.complete(changedPlan, association, prediction).isLeft)

  test("unknown, estimated, marginal and cross-unit covariance cannot quietly license Gaussian prediction"):
    val f = new Fixture
    val heads = right(f.heads)
    for status <- Vector(ComponentOutcomeCovarianceStatus.Unknown, ComponentOutcomeCovarianceStatus.Estimated) do
      assert(predictionReference(f, heads, gamma(f), status = status).isLeft)
    assert(predictionReference(f, heads, gamma(f), scope = ComponentOutcomeConditioning.MarginalOutcomes).isLeft)
    val crossed = DMat.tabulate(16, 16)((i, j) => if (i == 0 && j == 2) || (i == 2 && j == 0) then .001 else gamma(f)(i, j))
    assert(predictionReference(f, heads, crossed).isLeft)
    val nonsymmetric = DMat.tabulate(16, 16)((i, j) => if i == 0 && j == 1 then .061 else gamma(f)(i, j))
    assert(predictionReference(f, heads, nonsymmetric).isLeft)
    assert(predictionReference(f, heads, DMat.eye(8)).isLeft)
    assertEquals(f.confirmationReads, 0)

  test("frozen identity endpoints and covariance budgets refuse before confirmation operator reads"):
    val f = new Fixture
    val heads = right(f.heads)
    assert(predictionReference(f, heads, gamma(f), brainIdentity = Some(f.trainingBrain.identity)).isLeft)
    assert(predictionReference(f, heads, gamma(f), targetIdentity = Some(f.trainingTargets.identity)).isLeft)
    assert(predictionReference(f, heads, gamma(f), maximum = 0).isLeft)
    val wrongOrder = axis("wrong-target-order", 2)
    assert(FrozenComponentPredictionReference.freeze(heads, f.rows.descriptor, wrongOrder.descriptor,
      f.brain.identity, f.responses.identity, gamma(f), ComponentOutcomeCovarianceStatus.KnownConditionalGaussian,
      ComponentOutcomeConditioning.DiscoveryHeadsAndConfirmationPredictors, "known", exposure(f.snapshot.identity)).isLeft)
    assertEquals(f.confirmationReads, 0)

  test("unknown or already exposed holdout cannot freeze reference laws"):
    val f = new Fixture
    val heads = right(f.heads)
    val untouched = exposure(f.snapshot.identity)
    val request = ExposureRequest(ExposurePurpose.DerivedScoreView, ExposureActorRole.Analyst, ExposureScope.Holdout,
      ExposurePayload.DerivedScore, ExposureAssurance.Instrumented, 1)
    val exposed = untouched.append(ExposureEvent(request, ExposureOutcome.Succeeded, AdaptiveSelectionDependency.NoneObserved))
    val unknown = EvidenceExposure.external(untouched.reference)
    for account <- Vector(exposed, unknown) do
      assert(predictionReference(f, heads, gamma(f), account = Some(account)).isLeft)
      assert(FrozenComponentAssociationReference.freeze(f.plan, right(RankJointGaussian.declare(f.design, "joint Gaussian")),
        f.brain.identity, f.responses.identity, account, Seed.fromLong(9L), right(MonteCarloDraws(19))).isLeft)
    assertEquals(f.confirmationReads, 0)

  test("aggregate budget and changed evidence refuse before any source projection"):
    val f = new Fixture
    val heads = right(f.heads)
    val ref = right(predictionReference(f, heads, gamma(f)))
    // A nonzero limit can admit each old sub-workspace independently, yet
    // cannot admit their simultaneous arithmetic/covariance/inference sum.
    val combined = ComponentInference.incremental(ref, f.brain, f.responses, ComponentInferenceBudget(1000))
    assert(combined.left.toOption.exists(_.isInstanceOf[ComponentInferenceError.Budget]))
    assert(ComponentInference.incremental(ref, f.brain, f.responses, ComponentInferenceBudget(0)).isLeft)
    assert(ComponentInference.incremental(ref, f.brain, f.poisonTargets).isLeft)
    val refPoison = right(predictionReference(f, heads, gamma(f), targetIdentity = Some(f.poisonTargets.identity)))
    assert(ComponentInference.incremental(refPoison, f.brain, f.poisonTargets, ComponentInferenceBudget(0)).isLeft)
    assertEquals(f.confirmationReads, 0)
    assertEquals(f.targetReads, 0)
    val assoc = right(associationReference(f, budget = ComponentInferenceBudget(maximumOwnedCells = 0)))
    assert(ComponentInference.association(assoc, f.brain, f.responses).isLeft)
    val combinedAssociation = right(associationReference(f, budget = ComponentInferenceBudget(maximumOwnedCells = 3000)))
    assert(ComponentInference.association(combinedAssociation, f.brain, f.responses).left.toOption.exists(_.isInstanceOf[ComponentInferenceError.Budget]))
    assertEquals(f.confirmationReads, 0)

  test("dependent association remains unavailable despite a known response covariance"):
    val f = new Fixture(Vector(0, 0, 0, 0, 0, 0, 1, 1), 2)
    assert(RankJointGaussian.declare(f.design, "known response is not a joint score law").isLeft)
    assert(ComponentConfirmation.association(f.plan, f.brain, f.responses).isLeft)
