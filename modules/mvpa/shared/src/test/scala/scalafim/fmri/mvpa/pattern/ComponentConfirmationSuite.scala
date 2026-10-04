package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class ComponentConfirmationSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "component-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val planId = PlanId.derived(EstimandId("component-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
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

  test("correlated components require reduced-head refitting; analytic held-out metric losses"):
    val f = new Fixture
    val heads = right(f.heads)
    assertEquals(heads.interceptAdded, false)
    assertEquals(f.trainingReads, 2) // One projected matrix application, two component vectors.
    val result = right(ComponentConfirmation.incremental(heads, f.brain, f.responses))
    // Orthogonal Walsh fixture: Var(X1|X2)=Var(X2|X1)=3/4.
    // Target metric diag(1,2), slopes (2,-1) and (3,2).
    assertEqualsDouble(result.meanImprovements(0), .75 * (4.0 + 2.0), 1e-11)
    assertEqualsDouble(result.meanImprovements(1), .75 * (9.0 + 8.0), 1e-11)
    assertEqualsDouble(result.fullUnitLoss.sum / 8, .375, 1e-12)
    // Direct coefficient deletion gives 6 and 17, a different estimand.
    assert(math.abs(result.meanImprovements(0) - 6.0) > 1.0)
    assert(math.abs(result.meanImprovements(1) - 17.0) > 4.0)
    // Remaining slopes absorb the omitted correlated component.
    assertEqualsDouble(heads.reduced(0)(2, 0), 4.0, 1e-12)
    assertEqualsDouble(heads.reduced(1)(2, 0), 3.5, 1e-12)
    assertEquals(f.trainingReads, 2)
    assertEquals(f.confirmationReads, 2)
    assertEquals(result.rowUnitOrdinals, Vector.range(0, 8))
    assertEquals(result.targetMetric, Vector(1.0, 2.0))
    assertEquals(result.calibrationStatus, ComponentCalibrationStatus.PendingFrozenProtocol)
    assertEquals(result.members, f.plan.incrementalMembers)

  test("association is paired nuisance-adjusted correlation, not incremental usefulness"):
    val f = new Fixture
    val result = right(ComponentConfirmation.association(f.plan, f.brain, f.responses))
    // Residual Y1 = 3.5*u + 3*sqrt(.75)*v + .5*e; Y2=sqrt(3)*v+.25*f.
    assertEqualsDouble(result.correlations(0), 3.5 / math.sqrt(19.25), 1e-12)
    assertEqualsDouble(result.correlations(1), 1.5 / math.sqrt(3.0625), 1e-12)
    assertEquals(result.nuisanceRank, 2)
    assertEquals(result.members, f.plan.associationMembers)
    assertEquals(result.calibrationStatus, ComponentCalibrationStatus.PendingFrozenProtocol)
    assertEquals(f.confirmationReads, 2)

  test("intercept addition retains the training layout and all arithmetic"):
    val f = new Fixture(includeIntercept = false)
    val heads = right(f.heads)
    assertEquals(heads.interceptAdded, true)
    val result = right(ComponentConfirmation.incremental(heads, f.brain, f.responses))
    assertEqualsDouble(result.meanImprovements(0), 4.5, 1e-11)
    assertEqualsDouble(result.meanImprovements(1), 12.75, 1e-11)
    val association = right(ComponentConfirmation.association(f.plan, f.brain, f.responses))
    assertEqualsDouble(association.correlations(0), 3.5 / math.sqrt(19.25), 1e-12)

  test("unequal run sizes receive equal unit weights, not equal fold or row weights"):
    val f = new Fixture(Vector(0, 0, 0, 0, 0, 0, 1, 1), 2)
    val result = right(ComponentConfirmation.incremental(right(f.heads), f.brain, f.responses))
    // Independent analytic residual for omitted X1 is 2*(.75*u-.5*sqrt(.75)*v)
    // in target 1 and its negative half in target 2. independent unit-mean oracle values below.
    assertEqualsDouble(result.meanImprovements(0), 3.3066243270259355, 1e-11)
    assertEqualsDouble(result.meanImprovements(1), 12.172649730810374, 1e-11)
    assertEquals(result.independentUnits, f.units.descriptor)
    assertEquals(result.rowUnitOrdinals, Vector(0, 0, 0, 0, 0, 0, 1, 1))
    assert(ComponentConfirmation.association(f.plan, f.brain, f.responses).left.toOption.exists(_.isInstanceOf[ComponentConfirmationError.Unavailable]))

  test("budget, foreign rows, and incomplete units refuse before operator reads"):
    val f = new Fixture
    assert(ComponentConfirmation.association(f.plan, f.brain, f.poisonTargets, ComponentConfirmationBudget(0)).isLeft)
    assert(ComponentConfirmation.fitHeads(f.plan, f.trainingBrain, f.trainingTargets, f.trainingNuisance, ComponentConfirmationBudget(0)).isLeft)
    assertEquals(f.trainingReads, 0)
    assert(ComponentConfirmation.association(f.plan, f.trainingBrain, f.trainingTargets).isLeft)
    val heads = right(f.heads)
    assert(ComponentConfirmation.incremental(heads, f.brain, f.poisonTargets, ComponentConfirmationBudget(0)).isLeft)
    assertEquals(f.confirmationReads, 0)
    assertEquals(f.targetReads, 0)
    val incomplete = new Fixture(unitCount = 9)
    assert(ComponentConfirmation.incremental(right(incomplete.heads), incomplete.brain, incomplete.poisonTargets).isLeft)
    assertEquals(incomplete.confirmationReads, 0)
    assertEquals(incomplete.targetReads, 0)

  test("family and exposure are frozen and cannot silently gain another uncorrected view"):
    val f = new Fixture
    assert(FrozenComponentConfirmation.freeze(f.design, Vector("a", "b"), Vector("c", "d"), f.names, exposure(f.discovery.identity)).isLeft)
    assert(FrozenComponentConfirmation.freeze(f.design, f.plan.associationMembers, f.plan.incrementalMembers, f.names, exposure("foreign")).isLeft)
    assert(FrozenComponentConfirmation.freeze(f.design, f.plan.associationMembers, f.plan.incrementalMembers, Vector("drift", "drift"), exposure(f.discovery.identity)).isLeft)
    assert(FrozenComponentConfirmation.freeze(f.design, f.plan.associationMembers, f.plan.incrementalMembers, f.names,
      EvidenceExposure.external(ExposureReference(planId, f.discovery.identity, "fixture", ResultIdentity("unknown")))).isLeft)

  test("nonfinite targets refuse the complete family before brain reads"):
    val f = new Fixture
    val missing = right(MultiResponse.fromDense(f.rows, f.target, DMat.tabulate(8, 2)((i, j) => if i == 0 && j == 0 then Double.NaN else f.y(i, j)), value("missing"), source("missing")))
    assert(ComponentConfirmation.association(f.plan, f.brain, missing).isLeft)
    assert(ComponentConfirmation.incremental(right(f.heads), f.brain, missing).isLeft)
    assertEquals(f.confirmationReads, 0)

  test("aliased components fail without producing fallback or partially fitted heads"):
    val f = new Fixture
    val aliased = right(Observations.fromDense(f.training, f.neural, DMat.tabulate(8, 2)((i, _) => f.u(i)), value("aliased"), source("aliased")))
    assert(ComponentConfirmation.fitHeads(f.plan, aliased, f.trainingTargets, f.trainingNuisance).left.toOption.exists(_.isInstanceOf[ComponentConfirmationError.NonEstimable]))

  test("negative held-out usefulness is retained and confirmation never refits heads"):
    val f = new Fixture
    val heads = right(f.heads)
    // Confirmation follows the reduced model: fitting on these outcomes would
    // erase the held-out penalty and incorrectly report a training advantage.
    val shifted = DMat.tabulate(8, 2)((i, j) => if j == 0 then 7.0 + 4.0 * f.drift(i) + 4.0 * f.x(i, 1)
      else -3.0 - 2.0 * f.drift(i) + 1.5 * f.x(i, 1))
    val targets = right(MultiResponse.fromDense(f.rows, f.target, shifted, value("shifted"), source("shifted")))
    val result = right(ComponentConfirmation.incremental(heads, f.brain, targets))
    assertEqualsDouble(result.meanImprovements(0), -4.5, 1e-11)
    assertEqualsDouble((0 until 8).map(i => result.reducedUnitLoss(i, 0)).sum, 0.0, 1e-24)
    assertEquals(f.trainingReads, 2)
    assertEquals(result.headIdentity, heads.identity)

  test("nuisance-aliased scores are refused, while perfect nonconstant association remains descriptive"):
    val f = new Fixture
    val constant = right(MultiResponse.fromDense(f.rows, f.target, DMat.tabulate(8, 2)((_, _) => 7.0), value("constant"), source("constant")))
    assert(ComponentConfirmation.association(f.plan, f.brain, constant).left.toOption.exists(_.isInstanceOf[ComponentConfirmationError.NonEstimable]))
    val perfect = right(MultiResponse.fromDense(f.rows, f.target, f.x, value("perfect"), source("perfect")))
    val result = right(ComponentConfirmation.association(f.plan, f.brain, perfect))
    result.correlations.foreach(c => assertEqualsDouble(c, 1.0, 1e-12))
    assertEquals(result.calibrationStatus, ComponentCalibrationStatus.PendingFrozenProtocol)

  test("foreign training nuisance and missing training targets refuse before brain reads"):
    val f = new Fixture
    assert(ComponentConfirmation.fitHeads(f.plan, f.trainingBrain, f.trainingTargets, f.nuisance).isLeft)
    val missing = right(MultiResponse.fromDense(f.training, f.target, DMat.tabulate(8, 2)((i, j) => if i == 0 then Double.PositiveInfinity else f.yTrain(i, j)), value("missing-training"), source("missing-training")))
    assert(ComponentConfirmation.fitHeads(f.plan, f.trainingBrain, missing, f.trainingNuisance).isLeft)
    assertEquals(f.trainingReads, 0)
