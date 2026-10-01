package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.SpaceRole
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef

class PatternArtifactSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def error[A](value: Either[PatternArtifactError, A]): PatternArtifactError =
    value.fold(identity, _ => fail("expected a typed refusal"))
  private def invalidTarget[A](value: Either[PatternArtifactError, A]): Unit = error(value) match
    case PatternArtifactError.InvalidTarget(_) => ()
    case other => fail(s"expected InvalidTarget, got $other")
  private def axis(name: String, keys: Vector[String]) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "coordinate", "none", "one"))

  private final class Fixture:
    val samples = axis("samples", Vector("s1", "s2"))
    val neural = axis("neural", Vector("v1", "v2"))
    val target = axis("target", Vector("score"))
    val components = axis("components", Vector("r1"))
    val factors = right(PatternFactors(neural, target, components,
      DMat.dense(2, 1, Vector(1.0, 2.0)), DMat.dense(1, 1, Vector(3.0)),
      GaugeEvidence.VerifiedByGaleGramCholesky(1e-12)))
    val positive = right(AxisValues(target, Vector(1.0)))
    val geometry = right(TargetGeometry.continuous(target, positive, positive, Vector("all" -> positive)))
    val binding = right(TrainingBinding(samples.descriptor, "fixture", "declared-digest"))
    val diagnostics = right(PatternFitDiagnostics(Vector(4.0, 1.0), "declared-solver", Vector.empty))
    def artifact(
        centering: CenteringPolicy = CenteringPolicy.CenteredBeforeFit("neural:fixture", "target:fixture"),
        covariance: ResidualCovarianceCapability = ResidualCovarianceCapability.NotFitted,
        lineage: Vector[String] = Vector("training:fixture"),
        degeneracy: DegenerateTargetPolicy = DegenerateTargetPolicy.Refuse
    ): Either[PatternArtifactError, PatternArtifact] =
      PatternArtifact(factors, geometry, centering, degeneracy, covariance, binding, lineage, diagnostics)

  test("factorized categorical forward operation is callable through the artifact"):
    val conditions = axis("conditions", Vector("face", "house", "scramble"))
    val target = axis("contrasts", Vector("face-house", "mean-vs-scramble"))
    val neural = axis("voxels", Vector("v1", "v2", "v3"))
    val components = axis("components", Vector("c1", "c2"))
    val contrast = DMat.dense(3, 2, Vector(1.0, 1.0 / 3, -1.0, 1.0 / 3, 0.0, -2.0 / 3))
    val geometry = right(TargetGeometry.categorical(conditions, target, contrast,
      right(AxisValues(conditions, Vector(1.0 / 3, 1.0 / 3, 1.0 / 3)))))
    val factors = right(PatternFactors(neural, target, components,
      DMat.dense(3, 2, Vector(1.0, 2.0, 0.0, 1.0, 3.0, -1.0)),
      DMat.dense(2, 2, Vector(2.0, 0.0, 0.0, 3.0)), GaugeEvidence.VerifiedByGaleGramCholesky(1e-12)))
    val artifact = right(PatternArtifact(factors, geometry,
      CenteringPolicy.CenteredBeforeFit("neural:fixture", "target:fixture"),
      DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted,
      right(TrainingBinding(axis("samples", Vector("s1")).descriptor, "fixture", "declared-digest")),
      Vector("training:fixture"), right(PatternFitDiagnostics(Vector(1.0), "declared", Vector.empty))))
    val y = right(AxisValues(target, Vector(2.0, -1.0)))
    val factorMean = right(artifact.factors.forwardMean(y))
    val modelMean = right(artifact.forwardMean(y))
    // Independent hand contraction: C-transpose y=(4,-3), then A(4,-3).
    Vector(-2.0, -3.0, 15.0).zipWithIndex.foreach: (expected, row) =>
      assertEqualsDouble(factorMean.values(row), expected, 1e-12)
      assertEqualsDouble(modelMean.values(row), expected, 1e-12)
    assertEquals(modelMean.axis.descriptor, neural.descriptor)
    val foreign = axis("foreign-target", Vector("q1", "q2"))
    assertEquals(error(artifact.forwardMean(right(AxisValues(foreign, Vector(2.0, -1.0))))),
      PatternArtifactError.AxisMismatch("forward target", target.descriptor.stableKey, foreign.descriptor.stableKey))
    assertEquals(factors.coordinateGauge, CoordinateGauge.UnfixedBasis)
    assertEquals(artifact.interpretation, InterpretationStatus.ExperimentalFitOnly)

  test("explicit intercept changes the model mean and is neural-axis bound"):
    val f = new Fixture
    val y = right(AxisValues(f.target, Vector(4.0)))
    val centered = right(right(f.artifact()).forwardMean(y))
    assertEqualsDouble(centered.values(0), 12.0, 1e-12)
    assertEqualsDouble(centered.values(1), 24.0, 1e-12)
    val intercept = right(AxisValues(f.neural, Vector(7.0, -5.0)))
    val mean = right(right(f.artifact(CenteringPolicy.ExplicitIntercept(intercept, "target:fixture"))).forwardMean(y))
    assertEqualsDouble(mean.values(0), 19.0, 1e-12)
    assertEqualsDouble(mean.values(1), 19.0, 1e-12)
    val foreign = axis("foreign-neural", Vector("v1", "v2"))
    assertEquals(error(f.artifact(CenteringPolicy.ExplicitIntercept(right(AxisValues(foreign, Vector(7.0, -5.0))), "target:fixture"))),
      PatternArtifactError.AxisMismatch("intercept", f.neural.descriptor.stableKey, foreign.descriptor.stableKey))
    assertEquals(error(f.artifact(CenteringPolicy.CenteredBeforeFit("neural", " "))), PatternArtifactError.InvalidPolicy("centering receipts"))
    assertEquals(error(f.artifact(CenteringPolicy.ExplicitIntercept(intercept, " "))), PatternArtifactError.InvalidPolicy("target centering receipt"))

  test("numerical rank admission is scale-relative and refuses singular or near-collinear factors"):
    val neural = axis("neural", Vector("v1", "v2"))
    val target = axis("target", Vector("q1", "q2"))
    val components = axis("components", Vector("r1", "r2"))
    val evidence = GaugeEvidence.VerifiedByGaleGramCholesky(1e-12)
    for scale <- Vector(1e-150, 1.0, 1e150) do
      val a = DMat.dense(2, 2, Vector(scale, scale, scale, 2.0 * scale))
      assertEquals(right(PatternFactors(neural, target, components, a, DMat.eye(2), evidence)).gauge, evidence)
      val near = DMat.dense(2, 2, Vector(scale, scale, scale, (1.0 + 1e-7) * scale))
      assertEquals(error(PatternFactors(neural, target, components, near, DMat.eye(2), evidence)), PatternArtifactError.InvalidPolicy("Gale numerical rank admission"))
    val singular = DMat.dense(2, 2, Vector(1.0, 2.0, 2.0, 4.0))
    assertEquals(error(PatternFactors(neural, target, components, singular, DMat.eye(2), evidence)), PatternArtifactError.InvalidPolicy("Gale numerical rank admission"))
    assertEquals(error(PatternFactors(neural, target, components, DMat.eye(2), singular, evidence)), PatternArtifactError.InvalidPolicy("Gale numerical rank admission"))
    assertEquals(error(PatternFactors(neural, target, components, DMat.eye(2), DMat.eye(2), GaugeEvidence.VerifiedByGaleGramCholesky(0.0))), PatternArtifactError.InvalidPolicy("relative rank tolerance"))
    // Reciprocal component rescaling leaves A C-transpose unchanged.
    val rescaled = right(PatternFactors(neural, target, components,
      DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 1e-7)),
      DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 1e7)), evidence))
    val rescaledMean = right(rescaled.forwardMean(right(AxisValues(target, Vector(4.0, -3.0)))))
    assertEqualsDouble(rescaledMean.values(0), 4.0, 1e-12)
    assertEqualsDouble(rescaledMean.values(1), -3.0, 1e-12)
    // Unverified factors remain explicitly experimental rather than acquiring rank evidence.
    assertEquals(right(PatternFactors(neural, target, components, singular, singular, GaugeEvidence.PendingNumericalCheck)).gauge, GaugeEvidence.PendingNumericalCheck)

  test("factor rank ceiling and finite storage are independent constructor refusals"):
    val f = new Fixture
    val two = axis("two-components", Vector("r1", "r2"))
    assertEquals(error(PatternFactors(f.neural, f.target, two, DMat.eye(2), DMat.dense(1, 2, Vector(1.0, 2.0)), GaugeEvidence.PendingNumericalCheck)), PatternArtifactError.InvalidRank(2, 1))
    assertEquals(error(PatternFactors(f.neural, f.target, f.components, DMat.dense(2, 1, Vector(Double.NaN, 1.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.PendingNumericalCheck)), PatternArtifactError.NonFinite("A/C"))
    assertEquals(error(PatternFactors(f.neural, f.target, f.components, DMat.dense(2, 1, Vector(1.0, 1.0)), DMat.dense(1, 1, Vector(Double.PositiveInfinity)), GaugeEvidence.PendingNumericalCheck)), PatternArtifactError.NonFinite("A/C"))
    assertEquals(error(PatternFactors(f.neural, f.target, f.components,
      DMat.dense(2, 1, Vector(1.0, 1.0)), DMat.dense(1, 1, Vector(1.0)),
      GaugeEvidence.DeclaredSolverDiagnostic(" ", 1))), PatternArtifactError.InvalidPolicy("declared rank diagnostic"))
    assertEquals(error(PatternFactors(f.neural, f.target, f.components,
      DMat.dense(2, 1, Vector(1.0, 1.0)), DMat.dense(1, 1, Vector(1.0)),
      GaugeEvidence.DeclaredSolverDiagnostic("solver", 2))), PatternArtifactError.InvalidPolicy("declared rank diagnostic"))

  test("categorical prior, centering, coding rank and axis refusals each have one invalid input"):
    val conditions = axis("conditions", Vector("a", "b"))
    val target = axis("target", Vector("q1"))
    val valid = DMat.dense(2, 1, Vector(1.0, -1.0))
    val prior = right(AxisValues(conditions, Vector(0.5, 0.5)))
    right(TargetGeometry.categorical(conditions, target, valid, prior))
    invalidTarget(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1.0, 1.0)), prior))
    invalidTarget(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1e-150, 1e-150)), prior))
    right(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1e150, -1e150)), prior))
    invalidTarget(TargetGeometry.categorical(conditions, target, valid, right(AxisValues(conditions, Vector(0.4, 0.4)))))
    invalidTarget(TargetGeometry.categorical(conditions, target, valid, right(AxisValues(conditions, Vector(1.0, 0.0)))))
    val foreign = axis("foreign", Vector("a", "b"))
    assertEquals(error(TargetGeometry.categorical(conditions, target, valid, right(AxisValues(foreign, Vector(0.5, 0.5))))),
      PatternArtifactError.AxisMismatch("categorical prior", conditions.descriptor.stableKey, foreign.descriptor.stableKey))
    val three = axis("three-conditions", Vector("a", "b", "c"))
    val two = axis("two-targets", Vector("q1", "q2"))
    invalidTarget(TargetGeometry.categorical(three, two, DMat.dense(3, 2, Vector(1.0, 1.0, -1.0, -1.0, 0.0, 0.0)), right(AxisValues(three, Vector(1.0 / 3, 1.0 / 3, 1.0 / 3)))))

  test("K-class coding cannot expose more than K-1 mean-discrimination coordinates"):
    val three = axis("three-conditions", Vector("a", "b", "c"))
    // By the zero-sum subspace dimension, full rank and centering cannot both
    // hold for K columns. This tests the joint impossible-coding refusal.
    val tooMany = axis("three-targets", Vector("q1", "q2", "q3"))
    invalidTarget(TargetGeometry.categorical(three, tooMany, DMat.eye(3), right(AxisValues(three, Vector(1.0 / 3, 1.0 / 3, 1.0 / 3)))))

  test("continuous geometry checks target axes, positive weights and uniquely named blocks"):
    val f = new Fixture
    val foreign = axis("foreign", Vector("score"))
    assertEquals(error(TargetGeometry.continuous(f.target, right(AxisValues(foreign, Vector(1.0))), f.positive, Vector("all" -> f.positive))),
      PatternArtifactError.AxisMismatch("continuous prior", f.target.descriptor.stableKey, foreign.descriptor.stableKey))
    invalidTarget(TargetGeometry.continuous(f.target, f.positive, right(AxisValues(f.target, Vector(0.0))), Vector("all" -> f.positive)))
    invalidTarget(TargetGeometry.continuous(f.target, f.positive, f.positive, Vector.empty))
    invalidTarget(TargetGeometry.continuous(f.target, f.positive, f.positive, Vector("all" -> f.positive, "all" -> f.positive)))
    invalidTarget(TargetGeometry.continuous(f.target, f.positive, f.positive, Vector(" " -> f.positive)))
    invalidTarget(TargetGeometry.continuous(f.target, f.positive, f.positive, Vector("all" -> right(AxisValues(foreign, Vector(1.0))))))

  test("covariance, lineage and degenerate-policy declarations cannot bypass validation"):
    val f = new Fixture
    assertEquals(error(f.artifact(covariance = ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor, 3))), PatternArtifactError.InvalidPolicy("residual covariance"))
    assertEquals(error(f.artifact(covariance = ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor, -1))), PatternArtifactError.InvalidPolicy("residual covariance"))
    assertEquals(error(f.artifact(covariance = ResidualCovarianceCapability.ProviderBacked(f.neural.descriptor, " "))), PatternArtifactError.InvalidPolicy("residual covariance provider"))
    assertEquals(error(f.artifact(covariance = ResidualCovarianceCapability.ProviderBacked(f.target.descriptor, "backend"))), PatternArtifactError.InvalidPolicy("residual covariance provider"))
    assertEquals(error(f.artifact(lineage = Vector.empty)), PatternArtifactError.InvalidLineage("training lineage must be non-empty and named"))
    assertEquals(error(f.artifact(degeneracy = DegenerateTargetPolicy.RecordExperimental(" "))), PatternArtifactError.InvalidPolicy("degenerate target"))
    val experimental = right(f.artifact(degeneracy = DegenerateTargetPolicy.RecordExperimental("single observed outcome")))
    assertEquals(experimental.degenerateTarget, DegenerateTargetPolicy.RecordExperimental("single observed outcome"))
    assertEquals(experimental.interpretation, InterpretationStatus.ExperimentalFitOnly)
