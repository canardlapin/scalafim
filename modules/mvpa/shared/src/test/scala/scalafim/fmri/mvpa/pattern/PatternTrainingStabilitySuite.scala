package scalafim.fmri.mvpa.analysis

import gale.linalg.DMat
import gale.optim.{FirstOrderConfig, FirstOrderTolerance}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.core.DigestAlgorithm
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}
import scalafim.scenarios.{ScenarioHarness, ScenarioResult, ScenarioTolerance}

class PatternTrainingStabilitySuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, n: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(n)(i => s"$name-$i"), "stability", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val plan = PlanId.derived(EstimandId("training-stability"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)

  private final class Fixture:
    val samples = axis("training", 8); val units = axis("training-units", 2)
    val neural = axis("brain", 3); val target = axis("target", 2); val component = axis("component", 2)
    val t1 = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    val t2 = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val x = DMat.tabulate(8, 3)((i, j) => if j == 0 then 2.0 * t1(i) else if j == 1 then 1.5 * t2(i) else .2 * t1(i) * t2(i))
    val y = DMat.tabulate(8, 2)((i, j) => if j == 0 then t1(i) else t2(i))
    val observations = right(Observations.fromDense(samples, neural, x, value("discovery-x"), source("discovery-x")))
    val targets = right(MultiResponse.fromDense(samples, target, y, value("discovery-y"), source("discovery-y")))
    val groups = right(Column.fromValues(samples, Vector("a", "a", "a", "a", "b", "b", "b", "b"), value("training-blocks")))
    val design = right(LeaveOneGroupOutDesign.bind(samples, groups, ScientificSeed.fromLong(71))(identity))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector.fill(3)(1.0), DMat.zeros(3, 1)))
    val graph = right(SupportGraph(neural.descriptor, Vector.empty, SupportTopology.Declared("independent fixture voxels"), "one"))
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val config = right(StructuredPatternConfig(right(SupportPenalty(0.0, 0.0)), maximumOuterIterations = 250,
      inner = right(FirstOrderConfig.from(100000, right(FirstOrderTolerance.from(1e-10, 1e-10)))),
      stationarityTolerance = 1e-7, objectiveTolerance = 1e-9, maximumWorkspaceCells = 10000000L, maximumOperatorColumns = 100000L))
    var refitCalls = 0
    val refitter = new PatternBlockRefit[neural.Id, target.Id]:
      def fit[K](rows: AxisRef[K])(brain: Observations[rows.Id, neural.Id], response: MultiResponse[rows.Id, target.Id], binding: TrainingBinding): Either[String, PatternArtifact] =
        refitCalls += 1
        StructuredPatternOptimizer.fit(rows, neural, target, component)(brain, response, covariance, graph, geometry,
          CenteringPolicy.CenteredBeforeFit("block-centered-x", "block-centered-y"), config, binding, Vector("training-block-refit"), PatternReplay.Repeatable("owned-training"))
          .left.map(_.toString).flatMap(result => result.artifact.toRight("unconverged refit"))
    val artifact = right(refitter.fit(samples)(observations, targets, right(TrainingBinding(samples.descriptor, "discovery", "baseline"))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(samples, units, Vector(0,0,0,0,1,1,1,1))), artifact,
      right(FrozenProjection(neural, component, artifact.factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, artifact.factors.targetByComponent, ProjectionKind.DeclaredLinearProjection)), "frozen-support", "fixed-order", "training-prep"))
    val exposure = EvidenceExposure.internal(ExposureReference(plan, discovery.identity, "training", ResultIdentity("stability-result")))

  private def stableRefitScenario(): ScenarioResult =
    val f = new Fixture
    val before = f.refitCalls
    val result = right(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, f.exposure, f.refitter))
    val measurements = result.units.flatMap: unit =>
      unit.outcome match
        case PatternStabilityOutcome.Measured(brain, target, ordered) =>
          ScenarioHarness.values(s"${unit.unitAddress}-brain-angles", brain.principalAngles, Vector.fill(2)(0.0), ScenarioTolerance.absolute(3e-8)) ++
          ScenarioHarness.values(s"${unit.unitAddress}-target-angles", target.principalAngles, Vector.fill(2)(0.0), ScenarioTolerance.absolute(3e-8)) ++
          ScenarioHarness.values(s"${unit.unitAddress}-brain-axes", brain.fixedAxisAbsoluteCosines.toVector.flatten, Vector.fill(2)(1.0), ScenarioTolerance.absolute(1e-6)) ++
          Vector(ScenarioHarness.fact(s"${unit.unitAddress}-ordered", ordered, "declared component correspondence"))
        case other => Vector(ScenarioHarness.fact(s"${unit.unitAddress}-measured", false, other.toString))
    ScenarioHarness.result("umvpa-training-block-refit-stability", measurements ++ Vector(
      ScenarioHarness.fact("all-measured", result.allMeasured, result.units.toString),
      ScenarioHarness.fact("actual-refits", f.refitCalls - before == 2, s"refits=${f.refitCalls - before}"),
      ScenarioHarness.fact("selected-rows", result.units.map(_.analysisOrdinals).toSet == Set(Vector(0,1,2,3), Vector(4,5,6,7)), result.units.map(_.analysisOrdinals).toString),
      ScenarioHarness.fact("unit-coverage", result.units.size == f.design.keys.size, s"units=${result.units.size}"),
      ScenarioHarness.fact("exposure-attempts", result.exposure.events.size == 2, s"events=${result.exposure.events.size}")
    ))

  test("actual leave-one-training-group-out structured fits retain selected rows and stable subspaces"):
    val result = stableRefitScenario()
    assert(result.ciPass, result.render)

  test("confirmation rows, unknown exposure and refit-count budget refuse before invoking the fitter"):
    val f = new Fixture
    val before = f.refitCalls
    assert(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, f.exposure, f.refitter, PatternStabilityBudget(maximumRefits = 1)).isLeft)
    assert(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, EvidenceExposure.external(f.exposure.reference), f.refitter).isLeft)
    val confirm = axis("confirmation", 8)
    val x = right(Observations.fromDense(confirm, f.neural, f.x, value("confirmation-x"), source("confirmation-x")))
    val y = right(MultiResponse.fromDense(confirm, f.target, f.y, value("confirmation-y"), source("confirmation-y")))
    val groups = right(Column.fromValues(confirm, Vector("a","a","a","a","b","b","b","b"), value("confirmation-groups")))
    val design = right(LeaveOneGroupOutDesign.bind(confirm, groups, ScientificSeed.fromLong(71))(identity))
    assert(PatternTrainingStability.run(f.discovery, design, x, y, f.exposure, f.refitter).isLeft)
    assertEquals(f.refitCalls, before)

  test("a failed refit is retained and subsequent groups still complete"):
    val f = new Fixture
    var calls = 0
    val refitter = new PatternBlockRefit[f.neural.Id, f.target.Id]:
      def fit[K](rows: AxisRef[K])(x: Observations[rows.Id, f.neural.Id], y: MultiResponse[rows.Id, f.target.Id], binding: TrainingBinding): Either[String, PatternArtifact] =
        calls += 1
        if calls == 1 then Left("deliberate training-block failure") else f.refitter.fit(rows)(x, y, binding)
    val result = right(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, f.exposure, refitter))
    assert(!result.allMeasured)
    assertEquals(calls, 2)
    assert(result.units.head.outcome.isInstanceOf[PatternStabilityOutcome.Failed])
    assert(result.units.last.outcome.isInstanceOf[PatternStabilityOutcome.Measured])
    assertEquals(result.exposure.events.size, 2)

  private def rotatedRefitScenario(): ScenarioResult =
    val f = new Fixture
    val c = math.sqrt(.5)
    val rotation = DMat.dense(2, 2, Vector(c, -c, c, c))
    val refitter = new PatternBlockRefit[f.neural.Id, f.target.Id]:
      def fit[K](rows: AxisRef[K])(x: Observations[rows.Id, f.neural.Id], y: MultiResponse[rows.Id, f.target.Id], binding: TrainingBinding): Either[String, PatternArtifact] =
        f.refitter.fit(rows)(x, y, binding).flatMap: artifact =>
          for
            factors <- PatternFactors(f.neural, f.target, f.component, artifact.factors.neuralByComponent * rotation,
              artifact.factors.targetByComponent * rotation, GaugeEvidence.PendingNumericalCheck).left.map(_.toString)
            changed <- PatternArtifact(factors, artifact.target, artifact.centering, artifact.degenerateTarget,
              artifact.residualCovariance, artifact.trainingBinding, artifact.trainingLineage, artifact.diagnostics).left.map(_.toString)
          yield changed
    val result = right(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, f.exposure, refitter))
    val measurements = result.units.flatMap: unit =>
      unit.outcome match
        case PatternStabilityOutcome.Measured(brain, target, _) =>
          ScenarioHarness.values(s"${unit.unitAddress}-brain-angles", brain.principalAngles, Vector.fill(2)(0.0), ScenarioTolerance.absolute(3e-8)) ++
          ScenarioHarness.values(s"${unit.unitAddress}-target-angles", target.principalAngles, Vector.fill(2)(0.0), ScenarioTolerance.absolute(3e-8)) ++
          ScenarioHarness.values(s"${unit.unitAddress}-target-axes", target.fixedAxisAbsoluteCosines.toVector.flatten, Vector.fill(2)(c), ScenarioTolerance.absolute(1e-6)) ++
          ScenarioHarness.values(s"${unit.unitAddress}-brain-axes", brain.fixedAxisAbsoluteCosines.toVector.flatten, Vector(.8, .6), ScenarioTolerance.absolute(1e-6))
        case other => Vector(ScenarioHarness.fact(s"${unit.unitAddress}-measured", false, other.toString))
    ScenarioHarness.result("umvpa-refit-subspace-versus-fixed-axes", measurements ++ Vector(
      ScenarioHarness.fact("all-measured", result.allMeasured, result.units.toString),
      ScenarioHarness.fact("unit-coverage", result.units.size == f.design.keys.size, s"units=${result.units.size}"),
      ScenarioHarness.fact("exposure-attempts", result.exposure.events.size == 2, s"events=${result.exposure.events.size}")
    ))

  test("an actual refit followed by within-plane gauge rotation distinguishes axes from a stable subspace"):
    val result = rotatedRefitScenario()
    assert(result.ciPass, result.render)

  test("returned discovery artifact cannot impersonate a selected-row refit"):
    val f = new Fixture
    val refitter = new PatternBlockRefit[f.neural.Id, f.target.Id]:
      def fit[K](rows: AxisRef[K])(x: Observations[rows.Id, f.neural.Id], y: MultiResponse[rows.Id, f.target.Id], binding: TrainingBinding): Either[String, PatternArtifact] =
        Right(f.artifact)
    val result = right(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, f.exposure, refitter))
    assertEquals(result.units.size, 2)
    assert(result.units.forall(_.outcome match
      case PatternStabilityOutcome.Failed(PatternStabilityError.AxisMismatch(_)) => true
      case _ => false))
    assertEquals(result.exposure.events.size, 2)

  test("actual one-component refits retain unequal dimensions without inventing axis correspondence"):
    val f = new Fixture
    val reduced = axis("rank-one-component", 1)
    val refitter = new PatternBlockRefit[f.neural.Id, f.target.Id]:
      def fit[K](rows: AxisRef[K])(x: Observations[rows.Id, f.neural.Id], y: MultiResponse[rows.Id, f.target.Id], binding: TrainingBinding): Either[String, PatternArtifact] =
        StructuredPatternOptimizer.fit(rows, f.neural, f.target, reduced)(x, y, f.covariance, f.graph, f.geometry,
          CenteringPolicy.CenteredBeforeFit("block-centered-x", "block-centered-y"), f.config, binding, Vector("rank-one-refit"), PatternReplay.Repeatable("owned-training"))
          .left.map(_.toString).flatMap(result => result.artifact.toRight("unconverged one-component refit"))
    val result = right(PatternTrainingStability.run(f.discovery, f.design, f.observations, f.targets, f.exposure, refitter))
    assert(result.allMeasured, result.units.toString)
    result.units.foreach: unit =>
      unit.outcome match
        case PatternStabilityOutcome.Measured(brain, target, ordered) =>
          assertEquals((brain.leftRank, brain.rightRank), (2, 1))
          assertEquals((target.leftRank, target.rightRank), (2, 1))
          assertEquals(brain.fixedAxisAbsoluteCosines, None)
          assertEquals(target.fixedAxisAbsoluteCosines, None)
          assert(!ordered)
          brain.principalAngles.foreach(a => assertEqualsDouble(a, 0.0, 3e-8))
          target.principalAngles.foreach(a => assertEqualsDouble(a, 0.0, 3e-8))
        case other => fail(other.toString)
