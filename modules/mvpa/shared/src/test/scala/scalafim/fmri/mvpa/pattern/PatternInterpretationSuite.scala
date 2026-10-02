package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import resample4s.core.{IndexSpace, Injection}
import scalafim.fmri.mvpa.{AxisRef, Column, ReindexingLeg}

class PatternInterpretationSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, size: Int) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(size)(i => s"$name-$i"), "interpretation", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def close(actual: Double, expected: Double) = assertEqualsDouble(actual, expected, 1e-10)

  private final class Fixture:
    val population = axis("population", 10)
    def split(indices: Int*) = right(ReindexingLeg.bind(population,
      right(Injection.from(IArray.from(indices), right(IndexSpace.of(10))))))
    val training = split(0, 1)
    val held = split(2, 3, 4, 5, 6, 7, 8, 9)
    val neural = axis("neural", 2)
    val target = axis("target", 1)
    val component = axis("component", 1)
    val a = DMat.dense(2, 1, Vector(1.0, 0.0))
    val c = DMat.dense(1, 1, Vector(1.0))
    val unit = right(AxisValues(target, Vector(1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("task" -> unit)))
    val factors = right(PatternFactors(neural, target, component, a, c, GaugeEvidence.PendingNumericalCheck))
    // Exactly Psi = [[2,1],[1,1]], inverse [[1,-1],[-1,2]].
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(.5, 1.0 / 3.0),
      DMat.dense(2, 1, Vector(math.sqrt(1.5), math.sqrt(2.0 / 3.0)))))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("x", "y"),
      DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, 1),
      right(TrainingBinding(training.child.descriptor, "training-only", "training-fingerprint")), Vector("training-only"),
      right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val prior = right(TargetPriorCovariance(target, DMat.eye(1), value("target-prior"), "unit target variance"))
    val heads = right(PatternPrediction.fromArtifact(neural, target, component, artifact, covariance, Some(prior)))
    val t = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    val n = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    val e = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    def observations(model: Boolean, identity: String = "held-x") =
      val rows = Vector.tabulate(8): i =>
        val x1 = if model then t(i) + n(i) + e(i) else t(i) + 2.0 * n(i)
        right(AxisValues(neural, Vector(x1, n(i))))
      right(Column.fromValues(held.child, rows, value(identity)))

  test("calibrated filters separate forward loadings from suppressor weights and recover the named score"):
    val f = new Fixture
    val interpretation = right(PatternInterpretation.calibrated(f.heads))
    close(interpretation.forwardLoadings(0, 0), 1.0)
    close(interpretation.forwardLoadings(1, 0), 0.0)
    close(interpretation.neuralByComponent(0, 0), 1.0)
    close(interpretation.neuralByComponent(1, 0), -1.0)
    // x1=s+n, x2=n: decoder suppresses nuisance while the task map has no x2 loading.
    val x = right(AxisValues(f.neural, Vector(4.0, 3.0)))
    close(right(interpretation.scores(x)).values.values.head, 1.0)
    close((interpretation.neuralByComponent.t * interpretation.forwardLoadings)(0, 0), 1.0)
    assertEquals(interpretation.score, InterpretationScore.CalibratedComponents)

  test("model-covariance held-out Walsh samples give the independent Haufe identity"):
    val f = new Fixture
    val diagnostic = right(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector.empty, Vector.empty)(f.observations(true)))
    // Var(t)=Var(n)=Var(e)=1, z=t+e: Cov(x,z)=(2,0), Var(z)=2.
    // The common sample denominator 7 scales both equally, hence Haufe=(1,0)=A.
    close(diagnostic.neuralByScore(0, 0), 1.0)
    close(diagnostic.neuralByScore(1, 0), 0.0)
    assertEquals(diagnostic.rows, 8)
    assertEquals(diagnostic.covarianceDenominator, 7)
    assertEquals(diagnostic.scope.assessment, f.held.identity)

  test("independent empirical maps may be dense and posterior scores have different scaling"):
    val f = new Fixture
    val observations = f.observations(false)
    val calibrated = right(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector.empty, Vector.empty)(observations))
    // z=t+n, Cov(x,z)=(3,1), Var(z)=2 => (1.5,.5), not the sparse A.
    close(calibrated.neuralByScore(0, 0), 1.5)
    close(calibrated.neuralByScore(1, 0), .5)
    val posterior = right(PatternInterpretation.empiricalTargets(f.heads, f.training, f.held, Vector.empty, Vector.empty)(observations))
    // Unit prior and G=1 imply posterior target=.5 z, doubling its covariance pattern.
    close(posterior.neuralByScore(0, 0), 3.0)
    close(posterior.neuralByScore(1, 0), 1.0)
    assertEquals(posterior.score, InterpretationScore.PosteriorTargets)
    assertEquals(posterior.scoreAxis.descriptor, f.target.descriptor)
    assertNotEquals(calibrated.identity, posterior.identity)
    val revision = right(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector.empty, Vector.empty)(f.observations(false, "other-evidence")))
    assertNotEquals(calibrated.identity, revision.identity)

  test("diagnostic rows used in training selection or tuning refuse and singular empirical scores are explicit"):
    val f = new Fixture
    val x = f.observations(false)
    val reused = f.split(0, 1, 2, 3, 4, 5, 6, 7)
    val reusedX = right(Column.fromValues(reused.child, x.values, x.valueIdentity))
    assertEquals(PatternInterpretation.empiricalCalibrated(f.heads, f.training, reused, Vector.empty, Vector.empty)(reusedX),
      Left(PatternInterpretationError.AssessmentReuse("training", Vector(0, 1))))
    assertEquals(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector(f.split(0, 2)), Vector.empty)(x),
      Left(PatternInterpretationError.AssessmentReuse("selection", Vector(2))))
    assertEquals(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector.empty, Vector(f.split(1, 3)))(x),
      Left(PatternInterpretationError.AssessmentReuse("tuning", Vector(3))))
    val zero = right(Column.fromValues(f.held.child, Vector.fill(8)(right(AxisValues(f.neural, Vector(0.0, 0.0)))), value("zero")))
    assert(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector.empty, Vector.empty)(zero).left.toOption.exists:
      case PatternInterpretationError.Prediction(PatternPredictionError.Factorization("empirical score covariance", _)) => true
      case _ => false)
    assert(PatternInterpretation.empiricalCalibrated(f.heads, f.training, f.held, Vector.empty, Vector.empty)(x, PatternPredictionPolicy(1e-12, 0)).isLeft)

  test("rank-deficient or ill-conditioned G is refused without a silent inverse or jitter"):
    val neural = axis("conditioning-neural", 2)
    val target = axis("conditioning-target", 2)
    val components = axis("conditioning-components", 2)
    val samples = axis("conditioning-samples", 4)
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(1.0, 1.0), DMat.zeros(2, 1)))
    def heads(second: Double, tolerance: Double = 0.0) =
      val factors = right(PatternFactors(neural, target, components, DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, second)),
        DMat.eye(2), GaugeEvidence.PendingNumericalCheck))
      val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse,
        ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, 1), right(TrainingBinding(samples.descriptor, "training", "fingerprint")),
        Vector("training"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
      right(PatternPrediction.fromArtifact(neural, target, components, artifact, covariance, policy = PatternPredictionPolicy(tolerance, 100000)))
    assert(PatternInterpretation.calibrated(heads(0.0)).isLeft)
    assert(PatternInterpretation.calibrated(heads(1e-8)).isLeft)
    val admitted = right(PatternInterpretation.calibrated(heads(1e-8), PatternPredictionPolicy(0.0, 100000)))
    close((admitted.neuralByComponent.t * admitted.forwardLoadings)(1, 1), 1.0)
    assertEquals(admitted.relativePivotTolerance, 0.0)
    val strictHeads = heads(1e-8, 1e-12)
    val x = right(AxisValues(neural, Vector(0.0, 1e-8)))
    assert(strictHeads.calibratedScores(x).isLeft)
    val relaxed = right(PatternInterpretation.calibrated(strictHeads, PatternPredictionPolicy(0.0, 100000)))
    close(right(relaxed.scores(x)).values.values(1), 1.0)

  test("posterior target diagnostics retain a full-rank coordinate mix rather than the task-loading columns"):
    val f = new Fixture
    val target = axis("mixed-target", 2)
    val components = axis("mixed-components", 2)
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val geometry = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val factors = right(PatternFactors(f.neural, target, components, DMat.eye(2),
      DMat.dense(2, 2, Vector(.6, -.8, .8, .6)), GaugeEvidence.PendingNumericalCheck))
    val covariance = right(ResidualCovariance.fromFactors(f.neural, Vector(1.0, 1.0), DMat.zeros(2, 1)))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(f.neural.descriptor, 1),
      right(TrainingBinding(f.training.child.descriptor, "training", "fingerprint")), Vector("training"),
      right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val prior = right(TargetPriorCovariance(target, DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 3.0)), value("mixed-prior"), "target variances 1 and 3"))
    val prediction = right(PatternPrediction.fromArtifact(f.neural, target, components, artifact, covariance, Some(prior)))
    val x = right(Column.fromValues(f.held.child, Vector.tabulate(8)(i => right(AxisValues(f.neural, Vector(f.t(i), f.n(i))))), value("mixed-held-x")))
    val diagnostic = right(PatternInterpretation.empiricalTargets(prediction, f.training, f.held, Vector.empty, Vector.empty)(x))
    // yHat = diag(1/2,3/4) C x, so the full-rank Haufe transform is
    // C^T diag(2,4/3) = [[6/5,16/15],[-8/5,4/5]].
    val expected = Vector(6.0 / 5.0, 16.0 / 15.0, -8.0 / 5.0, 4.0 / 5.0)
    for i <- 0 until 2; j <- 0 until 2 do close(diagnostic.neuralByScore(i, j), expected(2 * i + j))
    assertEquals(diagnostic.scoreAxis.descriptor, target.descriptor)
