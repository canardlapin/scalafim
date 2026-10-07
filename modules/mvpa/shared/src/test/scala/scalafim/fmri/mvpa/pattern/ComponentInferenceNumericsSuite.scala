package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

/** Public-pipeline arithmetic controls; these do not qualify M4.09 release. */
class ComponentInferenceNumericsSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "component-numerics", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val planId = PlanId.derived(EstimandId("component-inference-numerics"), Vector(AxisSignature.unsafe("0" * 64)),
    AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question",
    Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  private def exposure(identity: String) =
    EvidenceExposure.internal(ExposureReference(planId, identity, "fixed numerical control", ResultIdentity("component-numerics-result")))

  private final class Fixture(confirmationValue: Double = 1e8, trainingSlope: Double = 1.0):
    val training = axis("numerics-discovery-rows", 8)
    val trainingUnits = axis("numerics-discovery-units", 8)
    val rows = axis("numerics-confirmation-rows", 8)
    val units = axis("numerics-confirmation-units", 8)
    val neural = axis("numerics-neural", 1)
    val target = axis("numerics-target", 1)
    val component = axis("numerics-component", 1)
    val trainX = DMat.tabulate(8, 1)((i, _) => if i % 2 == 0 then -1.0 else 1.0)
    val trainY = trainX * trainingSlope
    val factors = right(PatternFactors(neural, target, component,
      DMat.dense(1, 1, Vector(trainingSlope)), DMat.eye(1), GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector(1.0)))
    val artifact = right(PatternArtifact(factors,
      right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit))),
      CenteringPolicy.CenteredBeforeFit("training-x", "training-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted,
      right(TrainingBinding(training.descriptor, "numerics-discovery", "frozen")), Vector("discovery-only"),
      right(PatternFitDiagnostics(Vector(0.0), "numerical fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainingUnits, Vector.range(0, 8))), artifact,
      right(FrozenProjection(neural, component, DMat.eye(1), ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(1), ProjectionKind.DeclaredLinearProjection)),
      "fixed-support", "fixed-single-component", "training-preparation"))
    val confirmation = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, Vector.range(0, 8))), "confirmation-preparation"))
    val nuisance = right(ConfirmationNuisance(rows, DMat.tabulate(8, 1)((_, _) => 1.0)))
    val trainingNuisance = right(ConfirmationNuisance(training, DMat.tabulate(8, 1)((_, _) => 1.0)))
    val contract = C1Contract("discovery and heads fixed", "association and positive conditional loss improvement", "these eight independent rows",
      Vector("association-0", "incremental-0"), "known conditional Gaussian outcome law; pending protocol admission",
      Vector("projection", "metric", "heads"), Vector("confirmation arithmetic"))
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, confirmation,
      exposure(confirmation.identity), contract, nuisance, ConfirmationErrorLaw.IndependentGaussian))
    val plan = right(FrozenComponentConfirmation.freeze(design, Vector("association-0"), Vector("incremental-0"),
      Vector("intercept"), exposure(discovery.identity)))
    val trainingBrain = right(Observations.fromDense(training, neural, trainX, value("training-brain"), source("training-brain")))
    val trainingTargets = right(MultiResponse.fromDense(training, target, trainY, value("training-target"), source("training-target")))
    val brain = right(Observations.fromDense(rows, neural, DMat.tabulate(8, 1)((_, _) => 1.0),
      value("confirmation-brain"), source("confirmation-brain")))
    val targets = right(MultiResponse.fromDense(rows, target, DMat.tabulate(8, 1)((_, _) => confirmationValue),
      value("confirmation-target"), source("confirmation-target")))
    val heads = right(ComponentConfirmation.fitHeads(plan, trainingBrain, trainingTargets, trainingNuisance))
    val reference = right(FrozenComponentPredictionReference.freeze(heads, rows.descriptor, target.descriptor,
      brain.identity, targets.identity, DMat.eye(8), ComponentOutcomeCovarianceStatus.KnownConditionalGaussian,
      ComponentOutcomeConditioning.DiscoveryHeadsAndConfirmationPredictors, "independent unit-variance conditional outcome law",
      exposure(confirmation.identity)))
    def inference = ComponentInference.incremental(reference, brain, targets)

  test("large outcome offsets preserve the exact affine improvement and independently known Gaussian interval"):
    val f = new Fixture
    assertEqualsDouble(f.heads.full(0, 0), 0.0, 1e-14)
    assertEqualsDouble(f.heads.full(1, 0), 1.0, 1e-14)
    assertEqualsDouble(f.heads.reduced.head(0, 0), 0.0, 1e-14)
    val out = right(f.inference)
    val calculation = out.candidateCalculations.head
    // Exact arithmetic: 2*1*100000000 - 1 = 199999999. With eight
    // independent rows, the coefficient on each Y is 2/8, so variance is
    // 8*(2/8)^2=.5. Independent base R 4.x:
    // d<-199999999; s<-sqrt(4/8); c(d, s, d-qnorm(.975)*s, d+qnorm(.975)*s)
    assertEqualsDouble(out.arithmetic.meanImprovements.head, 199999999.0, 1e-6)
    for i <- 0 until 8 do assertEqualsDouble(out.arithmetic.unitImprovements(i, 0), 199999999.0, 1e-6)
    assertEqualsDouble(out.knownMeanCovariance(0, 0), .5, 1e-12)
    assertEqualsDouble(calculation.knownVariance, .5, 1e-12)
    assertEqualsDouble(calculation.standardError, .70710678118654757, 1e-12)
    assertEqualsDouble(calculation.lower95, 199999997.61409616, 1e-6)
    assertEqualsDouble(calculation.upper95, 200000000.38590384, 1e-6)
    assertEqualsDouble(calculation.z, 282842711.06040543, 1e-5)
    assertEquals(calculation.candidateDecision, ComponentGaussianPointDecision.AboveFixedFivePercentBoundary)
    assert(calculation.lower95 > 0.0)
    assertEquals(out.release, ComponentInferenceRelease.PendingFrozenProtocol)
    // The old subtract-two-large-losses route loses one whole unit here.
    val y = 1e8
    val roundedDifference = y * y - (y - 1.0) * (y - 1.0)
    assertEqualsDouble(roundedDifference, 200000000.0, 0.0)
    assert(math.abs(out.arithmetic.meanImprovements.head - roundedDifference) > .5)

  test("a large negative improvement retains positive variance and never rejects the one-sided positive null"):
    val out = right(new Fixture(confirmationValue = -1e8).inference)
    val calculation = out.candidateCalculations.head
    assertEqualsDouble(calculation.meanImprovement, -200000001.0, 1e-6)
    assertEqualsDouble(calculation.knownVariance, .5, 1e-12)
    assert(calculation.z < 0.0 && calculation.upper95 < 0.0)
    assertEquals(calculation.candidateDecision, ComponentGaussianPointDecision.NotAboveFixedFivePercentBoundary)

  test("identical fitted full and reduced predictions refuse zero known contrast variance through the public pipeline"):
    // A zero response in training gives exactly zero fitted heads, so their
    // prediction contrast is zero even though Gamma=I8 is positive definite.
    val f = new Fixture(trainingSlope = 0.0)
    assertEqualsDouble(f.heads.full(0, 0), 0.0, 0.0)
    assertEqualsDouble(f.heads.full(1, 0), 0.0, 0.0)
    assertEqualsDouble(f.heads.reduced.head(0, 0), 0.0, 0.0)
    val result = f.inference
    assert(result.left.toOption.exists(_.isInstanceOf[ComponentInferenceError.NonEstimable]), result.toString)
