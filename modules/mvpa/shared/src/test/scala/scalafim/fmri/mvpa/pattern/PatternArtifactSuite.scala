package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.SpaceRole
import munit.FunSuite
import scalafim.fmri.mvpa.AxisRef

class PatternArtifactSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, keys: Vector[String]) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "coordinate", "none", "one"))

  test("categorical factor artifact computes the factorized forward mean"):
    val conditions = axis("conditions", Vector("face", "house", "scramble"))
    val target = axis("contrasts", Vector("face-house", "mean-vs-scramble"))
    val neural = axis("voxels", Vector("v1", "v2", "v3"))
    val components = axis("components", Vector("c1", "c2"))
    val priors = right(AxisValues(conditions, Vector(1.0 / 3, 1.0 / 3, 1.0 / 3)))
    val contrast = DMat.dense(3, 2, Vector(1.0, 1.0 / 3, -1.0, 1.0 / 3, 0.0, -2.0 / 3))
    val geometry = right(TargetGeometry.categorical(conditions, target, contrast, priors))
    val a = DMat.dense(3, 2, Vector(1.0, 2.0, 0.0, 1.0, 3.0, -1.0))
    val c = DMat.dense(2, 2, Vector(2.0, 0.0, 0.0, 3.0))
    val factors = right(PatternFactors(neural, target, components, a, c, GaugeEvidence.VerifiedByGaleGramCholesky))
    right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("fixture"), DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted, right(TrainingBinding(neural.descriptor, "fixture", "fixture-digest")), Vector("training:fixture"), right(PatternFitDiagnostics(Vector(4.0, 1.0), "gale-pivoted-qr", Vector.empty))))
    val actual = right(factors.forwardMean(right(AxisValues(target, Vector(2.0, -1.0)))))
    Vector(-2.0, -3.0, 15.0).zipWithIndex.foreach: (expected, row) =>
      assertEqualsDouble(actual.values(row), expected, 1e-12)
    val foreign = axis("foreign-target", Vector("q1", "q2"))
    assert(factors.forwardMean(right(AxisValues(foreign, Vector(2.0, -1.0)))).isLeft)
    assertEqualsDouble(contrast(0, 0) + contrast(1, 0) + contrast(2, 0), 0.0, 1e-12)
    assertEqualsDouble(contrast(0, 1) + contrast(1, 1) + contrast(2, 1), 0.0, 1e-12)

  test("rejects foreign axes, impossible ranks, nonfinite factors, and invalid covariance"):
    val conditions = axis("conditions", Vector("a", "b"))
    val foreign = axis("foreign", Vector("a", "b"))
    val target = axis("target", Vector("q1"))
    val neural = axis("neural", Vector("n1", "n2"))
    val components = axis("components", Vector("r1"))
    val priors = right(AxisValues(conditions, Vector(0.5, 0.5)))
    assert(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1.0, -1.0)), right(AxisValues(foreign, Vector(0.5, 0.5)))).isLeft)
    assert(TargetGeometry.categorical(conditions, axis("too-many", Vector("q1", "q2")), DMat.dense(2, 2, Vector(1.0, 1.0, -1.0, -1.0)), priors).isLeft)
    assert(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.DeclaredSolverDiagnostic("", 1)).isLeft)
    assert(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(Double.NaN, 0.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.PendingNumericalCheck).isLeft)
    assert(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.dense(1, 1, Vector(Double.PositiveInfinity)), GaugeEvidence.PendingNumericalCheck).isLeft)
    val factors = right(PatternFactors(neural, target, components, DMat.dense(2, 1, Vector(1.0, 0.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.PendingNumericalCheck))
    val geometry = right(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(1.0, -1.0)), priors))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.ExplicitIntercept(neural.descriptor), DegenerateTargetPolicy.RecordExperimental("single observed target level"), ResidualCovarianceCapability.NotFitted, right(TrainingBinding(neural.descriptor, "fixture", "fixture-digest")), Vector("training:degenerate-fixture"), right(PatternFitDiagnostics(Vector.empty, "declared", Vector.empty))))
    assertEquals(artifact.interpretation, InterpretationStatus.ExperimentalFitOnly)
    assert(PatternArtifact(factors, geometry, CenteringPolicy.ExplicitIntercept(neural.descriptor), DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.ProviderBacked(target.descriptor, " "), right(TrainingBinding(neural.descriptor, "fixture", "fixture-digest")), Vector("training:fixture"), right(PatternFitDiagnostics(Vector.empty, "declared", Vector.empty))).isLeft)
