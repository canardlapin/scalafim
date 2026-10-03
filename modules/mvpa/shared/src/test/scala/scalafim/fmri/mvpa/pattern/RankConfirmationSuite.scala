package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import multivar.inference.{Alpha, CanonicalRankSampling, CanonicalResidualMethod, MonteCarloDraws, PermutationAction}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}
import scalafim.scenarios.{ScenarioHarness, ScenarioResult, ScenarioTolerance}

class RankConfirmationSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "rank-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val plan = PlanId.derived(EstimandId("rank-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  private def exposure(identity: String) = EvidenceExposure.internal(ExposureReference(plan, identity, "fixture", ResultIdentity("rank-result")))
  private def walsh(row: Int, mask: Int): Double = if Integer.bitCount(row & mask) % 2 == 0 then 1.0 else -1.0

  private final class Fixture(rank: Int = 2, dependent: Boolean = false):
    val rows = axis("rank-confirm-rows", 16); val units = axis("rank-confirm-units", 16)
    val training = axis("rank-discovery-rows", 4); val trainUnits = axis("rank-discovery-units", 4)
    val neural = axis("rank-neural", 3); val target = axis("rank-target", 2); val component = axis("rank-component", 2)
    val factors = right(PatternFactors(neural, target, component, DMat.tabulate(3, 2)((i, j) => if i == j then 1.0 else 0.0), DMat.eye(2), GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val artifact = right(PatternArtifact(factors, right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit))),
      CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "rank-discovery", "frozen")), Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainUnits, Vector.range(0, 4))), artifact,
      right(FrozenProjection(neural, component, factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)), "support", "none", "discovery-prep"))
    val snapshot = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, Vector.range(0, 16))), "confirm-prep"))
    val nuisance = right(ConfirmationNuisance(rows, DMat.tabulate(16, 2)((i, j) => if j == 0 then 1.0 else walsh(i, 12))))
    val covariance = right(ResidualCovariance.fromFactors(rows, Vector.fill(16)(4.0), DMat.tabulate(16, 1)((_, _) => .2)))
    val law = if dependent then ConfirmationErrorLaw.DependentTime(right(BoundConfirmationCovariance(rows, covariance, "known-common-row-shape", TemporalCovarianceStatus.KnownGaussian))) else ConfirmationErrorLaw.IndependentGaussian
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, snapshot, exposure(snapshot.identity),
      C1Contract("fixed discovery", "remaining canonical roots zero", "these candidate spaces in held-out units", Vector("rank-1", "rank-2"), "joint Gaussian common row shape", Vector("candidate projections"), Vector("residual coordinates", "stepwise CCA")), nuisance, law))
    val brainProjection = right(FrozenProjection(neural, axis("brain-candidate", 3), DMat.eye(3), ProjectionKind.DeclaredLinearProjection))
    val targetProjection = right(FrozenProjection(target, axis("target-candidate", 2), DMat.eye(2), ProjectionKind.DeclaredLinearProjection))
    val candidates = right(RankFrozenSubspaces.freeze(discovery, brainProjection, targetProjection, exposure(discovery.identity), "discovery-only-candidates"))
    val joint = RankJointGaussian.declare(design, "synthetic joint Gaussian independent-row assumption")
    val x = DMat.tabulate(16, 3)((i, j) => walsh(i, Vector(1, 2, 4)(j)) + 2.0 * walsh(i, 12) + 3.0)
    val y = DMat.tabulate(16, 2): (i, j) =>
      val signal = if j == 0 && rank >= 1 then .8 * walsh(i, 1) + .6 * walsh(i, 8)
        else if j == 1 && rank >= 2 then .3 * walsh(i, 2) + math.sqrt(.91) * walsh(i, 3)
        else walsh(i, if j == 0 then 8 else 3)
      signal - 4.0 * walsh(i, 12) + 7.0
    var reads = 0
    val operator = new DoubleLinearOperator:
      val rows = 16; val cols = 3
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        x.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = x.transposeApplyTo(input, output)
    val observations = right(Observations.fromOperator(rows, neural, operator, value("rank-brain"), source("rank-brain")))
    val responses = right(MultiResponse.fromDense(rows, target, y, value("rank-target"), source("rank-target")))
    def run(budget: RankConfirmationBudget = RankConfirmationBudget()) = joint.flatMap(law => RankConfirmation.run(design, candidates, law, observations, responses, Seed.fromLong(39251L), right(MonteCarloDraws(39)), right(Alpha(.05)), budget))

  private def analyticScenario(rank: Int, dependent: Boolean): ScenarioResult =
    val f = new Fixture(rank, dependent)
    val result = right(f.run())
    val expected = Vector(if rank >= 1 then .8 else 0.0, if rank >= 2 then .3 else 0.0)
    ScenarioHarness.result(s"umvpa-rank-$rank-known-row-shape-$dependent", ScenarioHarness.values("canonical-correlations", result.candidateArithmetic.correlations, expected, ScenarioTolerance.absolute(1e-10)) ++ Vector(
      ScenarioHarness.fact("actual-residual-rows", result.residualRows == 14 && result.nuisanceRank == 2, s"rows=${result.residualRows},rank=${result.nuisanceRank}"),
      ScenarioHarness.fact("unequal-candidates", result.brainCandidateAxis.size == 3 && result.targetCandidateAxis.size == 2, "P=3,Q=2"),
      ScenarioHarness.fact("all-fixed-draws", result.candidateArithmetic.receipts.size == 2 && result.candidateArithmetic.receipts.forall(_.consumed.value == 39) && result.candidateArithmetic.completedCompactFits == 78L, s"families=${result.candidateArithmetic.receipts.size}, fits=${result.candidateArithmetic.completedCompactFits}"),
      ScenarioHarness.fact("calibration-pending", result.calibrationStatus == RankCalibrationStatus.PendingFrozenProtocol && result.admittedDetectableRank.isLeft, "unit fixtures do not qualify frozen calibration"),
      ScenarioHarness.fact("actual-rows-and-units", result.confirmationRows == f.rows.descriptor && result.independentUnits == f.units.descriptor && result.rowUnitOrdinals == Vector.range(0,16), "actual confirmation mapping"),
      ScenarioHarness.fact("actual-residual-basis", result.residualBasis.method == CanonicalResidualMethod.HuhJhun && result.residualBasis.matrix.rows == 16 && result.residualBasis.matrix.cols == 14 && result.nuisanceWorkingDesign.cols == 3 && result.residualBasisIdentity.length == 64, "actual Huh-Jhun basis and augmented nuisance design"),
      ScenarioHarness.fact("explicit-exchangeability", result.jointLaw.assumption == RankExchangeabilityAssumption.SphericalJointGaussianRows && result.candidateArithmetic.action.isInstanceOf[PermutationAction.Unrestricted] && result.candidateArithmetic.sampling.isInstanceOf[CanonicalRankSampling.DistinctNonIdentity], "declared spherical Gaussian residual coordinates and distinct non-identity unrestricted actions")
    ))

  test("rank-zero, rank-one and rank-two analytic workflows retain fixed candidates and pending calibration"):
    for rank <- 0 to 2 do
      val result = analyticScenario(rank, false)
      assert(result.ciPass, result.render)

  test("a known voxel covariance does not license row randomization of dependent confirmation"):
    val f = new Fixture(2, true)
    assert(f.run().left.toOption.exists(_.isInstanceOf[RankConfirmationError.Unavailable]))
    assertEquals(f.reads, 0)

  test("candidate freeze refuses foreign discovery and unknown or exposed holdout snapshots"):
    val f = new Fixture
    assert(RankFrozenSubspaces.freeze(f.discovery, f.brainProjection, f.targetProjection, exposure("foreign"), "receipt").isLeft)
    assert(RankFrozenSubspaces.freeze(f.discovery, f.brainProjection, f.targetProjection, EvidenceExposure.external(exposure(f.discovery.identity).reference), "receipt").isLeft)
    val request = ExposureRequest(ExposurePurpose.ModelSelection, ExposureActorRole.Analyst, ExposureScope.Holdout, ExposurePayload.Payload, ExposureAssurance.Declared, 1)
    val current = exposure(f.discovery.identity)
    val permit = right(ExposureControl.permit(current, request))
    val viewed = ExposureControl.read(current, permit, request)(Right(())) match
      case ExposureAttempt.Completed(_, next) => next
      case other => fail(other.toString)
    assert(RankFrozenSubspaces.freeze(f.discovery, f.brainProjection, f.targetProjection, viewed, "receipt").isLeft)
    assertEquals(f.reads, 0)

  test("budget and actual endpoint refusals happen before confirmation source application"):
    val f = new Fixture
    assert(f.run(RankConfirmationBudget(0, 100)).isLeft)
    assert(f.run(RankConfirmationBudget(1000000, 1)).isLeft)
    assert(f.run(RankConfirmationBudget(1000000, 1000, 38)).isLeft)
    val foreignRows = axis("foreign-rank-rows",16)
    val foreign = right(MultiResponse.fromDense(foreignRows, f.target, f.y, value("foreign-target"), source("foreign-target")))
    val foreignBrain = right(Observations.fromDense(foreignRows, f.neural, f.x, value("foreign-brain"), source("foreign-brain")))
    assert(RankConfirmation.run(f.design, f.candidates, right(f.joint), foreignBrain, foreign, Seed.fromLong(1L), right(MonteCarloDraws(39)), right(Alpha(.05))).isLeft)
    assert(RankJointGaussian.declare(f.design, "").isLeft)
    assertEquals(f.reads, 0)

  test("budget preflight cannot apply a poison target provider"):
    val f = new Fixture
    var targetReads = 0
    val poison = new DoubleLinearOperator:
      val rows = 16; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        targetReads += 1
        throw IllegalStateException("rank preflight applied targets")
    val responses = right(MultiResponse.fromOperator(f.rows, f.target, poison, value("poison"), source("poison")))
    assert(RankConfirmation.run(f.design, f.candidates, right(f.joint), f.observations, responses, Seed.fromLong(1L), right(MonteCarloDraws(39)), right(Alpha(.05)), RankConfirmationBudget(0, 1000)).isLeft)
    assertEquals(targetReads, 0)
    assertEquals(f.reads, 0)
