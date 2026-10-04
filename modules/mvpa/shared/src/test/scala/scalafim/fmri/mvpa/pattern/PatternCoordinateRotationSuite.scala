package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.AxisRef

class PatternCoordinateRotationSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, size: Int) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(size)(i => s"$name-$i"), "rotation", "one", "raw"))
  private def close(actual: DMat, expected: DMat, tolerance: Double = 1e-8): Unit =
    assertEquals(actual.rows, expected.rows); assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do assertEqualsDouble(actual(row, col), expected(row, col), tolerance)

  private final class Fixture(categorical: Boolean = false):
    val neural = axis("rotation-neural", 2)
    val target = axis("rotation-target", 2)
    val components = axis("rotation-components", 2)
    val samples = axis("rotation-samples", 4)
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val conditions = axis("class", 3)
    val geometry = if categorical then right(TargetGeometry.categorical(conditions, target,
      DMat.dense(3, 2, Vector(1.0, 0.0, 0.0, 1.0, -1.0, -1.0)), right(AxisValues(conditions, Vector(.2, .3, .5)))))
      else right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val factors = right(PatternFactors(neural, target, components,
      DMat.dense(2, 2, Vector(1.0, .4, .1, 1.2)), DMat.dense(2, 2, Vector(1.1, .2, .3, .9)), GaugeEvidence.PendingNumericalCheck))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.ExplicitIntercept(right(AxisValues(neural, Vector(.3, -.4))), "fixed discovery intercept"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, 1), right(TrainingBinding(samples.descriptor, "discovery", "training-fingerprint")),
      Vector("discovery"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(1.0, 1.0), DMat.zeros(2, 1)))
    val prior = right(TargetPriorCovariance(target, DMat.eye(2), ValueIdentity.source(ValueId.unsafe("rotation-prior")), "fixed unit Gaussian target covariance"))
    val prediction = right(PatternPrediction.fromArtifact(neural, target, components, artifact, covariance, if categorical then None else Some(prior)))

  test("rotation retains the original prediction and reconstructs its effective forward map") {
    val f = new Fixture
    val rotation = right(PatternCoordinateRotation.varimax(f.prediction, ScoreCovarianceBasis.ExplicitlyWhitenedFactorIdentity))
    val original = f.prediction.factors.neuralByComponent * f.prediction.factors.targetByComponent.t

    assert(rotation.prediction eq f.prediction)
    assert(rotation.artifact eq f.artifact)
    close(right(rotation.materializeForward(4)), original)
    assert(rotation.materializeForward(3).isLeft)
    close(rotation.middle, rotation.brainInverse * rotation.targetInverse.t)
  }

  test("the admitted supplied shear has distinct display axes and the required inverse covariance") {
    val f = new Fixture
    val shear = DMat.dense(2, 2, Vector(1.0, .5, 0.0, 1.0))
    val target = DMat.dense(2, 2, Vector(1.0, 0.0, -.25, 1.0))
    val rotation = right(PatternCoordinateRotation.supplied(f.prediction, shear, target, ScoreCovarianceBasis.ExplicitlyWhitenedFactorIdentity))
    val covariance = rotation.rotatedScoreCovariance

    close(covariance, DMat.dense(2, 2, Vector(1.25, -.5, -.5, 1.0)))
    assertEqualsDouble(rotation.factorCorrelation(0, 1), -1.0 / math.sqrt(5.0), 1e-12)
    assertNotEquals(rotation.brainAxes.axis.descriptor, rotation.targetAxes.axis.descriptor)
    assert(rotation.brainAxes.axis.descriptor != f.components.descriptor)
    close(right(rotation.materializeForward(4)), f.prediction.factors.neuralByComponent * f.prediction.factors.targetByComponent.t)
  }

  test("near-singular condition and owned-work budgets refuse before display admission") {
    val f = new Fixture
    assert(PatternCoordinateRotation.varimax(f.prediction, ScoreCovarianceBasis.ExplicitlyWhitenedFactorIdentity,
      PatternCoordinateRotationPolicy(maximumOwnedCells = 1)).isLeft)
    assert(PatternCoordinateRotation.varimax(f.prediction, ScoreCovarianceBasis.ActualFactorCoordinates(DMat.eye(1))).isLeft)
    assert(PatternCoordinateRotation.supplied(f.prediction, DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 1e-12)), DMat.eye(2),
      ScoreCovarianceBasis.ExplicitlyWhitenedFactorIdentity, PatternCoordinateRotationPolicy(maximumCondition = 1e6)).isLeft)
    assert(PatternCoordinateRotation.supplied(f.prediction, DMat.eye(2), DMat.eye(2),
      ScoreCovarianceBasis.ActualFactorCoordinates(DMat.dense(2, 2, Vector(1.0, 2.0, 2.0, 1.0)))).isLeft)
  }

  test("supplied sign-permutation and scaled shears preserve intercept-aware Gaussian and categorical heads"):
    val brain = DMat.dense(2, 2, Vector(0.0, -2.0, .5, .25))
    val target = DMat.dense(2, 2, Vector(1.5, .2, -.3, .8))
    for categorical <- Vector(false, true) do
      val f = new Fixture(categorical)
      val covariance = DMat.dense(2, 2, Vector(2.0, .4, .4, 1.0))
      val rotated = right(PatternCoordinateRotation.supplied(f.prediction, brain, target, ScoreCovarianceBasis.ActualFactorCoordinates(covariance)))
      assertEquals(rotated.method, PatternRotationMethod.Supplied)
      close(rotated.rotatedScoreCovariance, DMat.dense(2, 2, Vector(4.525, -.65, -.65, .5)), 1e-12)
      close(rotated.rawFilters, DMat.dense(2, 2, Vector(.2, -1.9, .6, .1)), 1e-12)
      close(right(rotated.materializeForward(4)), DMat.dense(2, 2, Vector(1.18, .66, .35, 1.11)), 1e-12)
      val y = right(AxisValues(f.target, Vector(.7, -.2)))
      val encoded = right(rotated.encode(y)).values
      encoded.zip(Vector(.3 + 1.18 * .7 - .66 * .2, -.4 + .35 * .7 - 1.11 * .2)).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
      val x = right(AxisValues(f.neural, Vector(.7, -.2)))
      if categorical then
        val actual = right(rotated.classify(x))
        val original = right(f.prediction.classify(x))
        assertEquals(actual.keys, original.keys)
        actual.probabilities.zip(original.probabilities).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
        val means = Vector(Vector(1.18, .35), Vector(.66, 1.11), Vector(-1.84, -1.46))
        val logScores = means.zip(Vector(.2,.3,.5)).map: (mean, prior) =>
          math.log(prior) + mean(0) * .4 + mean(1) * .2 - .5 * (mean(0) * mean(0) + mean(1) * mean(1))
        val sum = logScores.map(s => math.exp(s - logScores.max)).sum
        actual.probabilities.zip(logScores).foreach((a, e) => assertEqualsDouble(a, math.exp(e - logScores.max) / sum, 1e-12))
      else
        val actual = right(rotated.decode(x)).values.values
        val h00 = 1.0 + 1.18 * 1.18 + .66 * .66
        val h01 = 1.18 * .35 + .66 * 1.11
        val h11 = 1.0 + .35 * .35 + 1.11 * 1.11
        val determinant = h00 * h11 - h01 * h01
        val px = (h11 * .4 - h01 * .2) / determinant
        val py = (-h01 * .4 + h00 * .2) / determinant
        actual.zip(Vector(1.18 * px + .35 * py, .66 * px + 1.11 * py)).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
      val raw = right(f.prediction.rawScores(x)).values.values
      val rawMatrix = DMat.dense(2, 1, raw)
      val transformedRaw = right(rotated.rawScores(rawMatrix, 2))
      close(transformedRaw, DMat.dense(2, 1, Vector(.5 * raw(1), -2.0 * raw(0) + .25 * raw(1))), 1e-12)
      val calibrated = right(f.prediction.calibratedScores(x)).values.values
      val transformed = right(rotated.calibratedScores(DMat.dense(2, 1, calibrated), 2))
      // Independent inverse of [[0,-2],[.5,.25]], determinant one.
      close(transformed, DMat.dense(2, 1, Vector(.25 * calibrated(0) + 2.0 * calibrated(1), -.5 * calibrated(0))), 1e-12)
      assert(rotated.rawScores(rawMatrix, 1).isLeft)
      assert(rotated.calibratedScores(DMat.dense(2, 1, Vector(Double.NaN, 1.0))).isLeft)
