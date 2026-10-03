package scalafim.fmri.mvpa.dataset.predictive

import alder.data.FixedCoverage
import alder.kernel.*
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*

class AlderPatternPredictionSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, keys: Vector[String]) =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "fixture", "none", "one"))
  private given FitContext = FitContext.root(Seed(17L), PlanFingerprint("pattern-head"), SchemaFingerprint("fixture-v1"), NumericMode.Deterministic)
  private final class Fixture:
    val samples = axis("samples", Vector("s1", "s2"))
    val rootMapping = right(NativeAxisMapping.fromAxis(samples, Vector(7L, 9L), DataFingerprint.external("root")))
    val rows = right(AlderPredictiveAdmission.materialized(samples.descriptor, DMat.eye(2), DMat.dense(2, 1, Vector(1.0, 0.0)), Vector("s1", "s2"), rootMapping, right(MaterializationBudget(8L))))
    val split = right(rows.fixedHoldout(Vector(7L), Vector(9L), FixedCoverage.Exhaustive))
    val training = axis("training", Vector("s1"))
    val mapping = right(NativeAxisMapping.fromAxis(training, Vector(7L), split.train.fingerprint))
    val neural = axis("neural", Vector("v1", "v2"))
    val target = axis("target", Vector("score"))
    val component = axis("component", Vector("c1"))
    val conditions = axis("conditions", Vector("second", "first"))
    val factors = right(PatternFactors(neural, target, component, DMat.dense(2, 1, Vector(1.0, 2.0)), DMat.dense(1, 1, Vector(1.0)), GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector(1.0)))
    val continuous = right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit)))
    val categorical = right(TargetGeometry.categorical(conditions, target, DMat.dense(2, 1, Vector(-1.0, 1.0)), right(AxisValues(conditions, Vector(.25, .75)))))
    val covariance = right(ResidualCovariance.fromFactors(neural, Vector(1.0, 1.0), DMat.dense(2, 1, Vector(0.0, 0.0))))
    val prior = right(TargetPriorCovariance(target, DMat.eye(1), ValueIdentity.source(ValueId.unsafe("prior")), "target coordinates"))
    def artifact(geometry: TargetGeometry, digest: String = split.train.fingerprint.digest) =
      right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse,
        ResidualCovarianceCapability.DiagonalPlusLowRank(neural.descriptor, 1), right(TrainingBinding(training.descriptor, "fixture", digest)),
        Vector("fixture"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val gaussian = right(PatternPrediction.fromArtifact(neural, target, component, artifact(continuous), covariance, Some(prior)))
    val classes = right(PatternPrediction.fromArtifact(neural, target, component, artifact(categorical), covariance))

  test("real Alder encoding and decoding pipes use inputs and retain the training receipt"):
    val f = new Fixture
    val decoding = right(AlderPatternPrediction.decoding(f.split.train, f.gaussian, f.mapping, f.split.train.fingerprint).value)
    val encoding = right(AlderPatternPrediction.encoding(f.split.train, f.gaussian, f.mapping, f.split.train.fingerprint).value)
    assertEquals(decoding.audit.data, encoding.audit.data)
    assertEquals(decoding.audit.data.digest, f.split.train.fingerprint.digest)
    assertEquals(decoding.audit.data.policy, f.split.train.fingerprint.policy)
    assertNotEquals(decoding.audit.data.digest, f.split.test.fingerprint.digest)
    val first = right(decoding.artifact.run(Array(1.0, 2.0)))
    val second = right(decoding.artifact.run(Array(0.0, 0.0)))
    assertEqualsDouble(first.values.values.head, 5.0 / 6.0, 1e-12)
    assertEqualsDouble(second.values.values.head, 0.0, 1e-12)
    assertEquals(first.priorIdentity, f.prior.valueIdentity)
    assertEquals(right(encoding.artifact.run(Array(3.0))).values, Vector(3.0, 6.0))
    assert(decoding.artifact.run(Array(1.0)).isLeft)

  test("categorical Alder pipe preserves class order and direct likelihood probabilities"):
    val f = new Fixture
    val trained = right(AlderPatternPrediction.classification(f.split.train, f.classes, f.mapping, f.split.train.fingerprint).value)
    val result = right(trained.artifact.run(Array(1.0, 0.0)))
    assertEquals(result.keys, Vector("second", "first"))
    val scores = Vector(math.log(.25) - 3.5, math.log(.75) - 1.5)
    result.logScores.zip(scores).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-12))
    assertEqualsDouble(result.probabilities.sum, 1.0, 1e-12)
    assert(result.probabilities(1) > result.probabilities(0))

  test("head admission and exact row provenance fail before producing a trained pipe"):
    val f = new Fixture
    val unsupportedClass = AlderPatternPrediction.classification(f.split.train, f.gaussian, f.mapping, f.split.train.fingerprint).value
    assert(unsupportedClass.left.toOption.exists(_.cause match
      case AlderPatternPredictionError.Prediction(PatternPredictionError.UnsupportedHead(_)) => true
      case _ => false))
    val noPrior = right(PatternPrediction.fromArtifact(f.neural, f.target, f.component, f.artifact(f.continuous), f.covariance))
    assert(AlderPatternPrediction.decoding(f.split.train, noPrior, f.mapping, f.split.train.fingerprint).value.isLeft)
    val wrongExpected = AlderPatternPrediction.encoding(f.split.train, f.gaussian, f.mapping, DataFingerprint.external("foreign")).value
    assertEquals(wrongExpected.left.toOption.map(_.cause), Some(AlderPatternPredictionError.ExpectedTrainingReceiptMismatch))
    val wrongMapping = right(NativeAxisMapping.fromAxis(f.training, Vector(9L), f.split.train.fingerprint))
    val wrongOrder = AlderPatternPrediction.encoding(f.split.train, f.gaussian, wrongMapping, f.split.train.fingerprint).value
    assertEquals(wrongOrder.left.toOption.map(_.cause), Some(AlderPatternPredictionError.TrainingRowOrderMismatch))
    val wrongBound = right(PatternPrediction.fromArtifact(f.neural, f.target, f.component, f.artifact(f.continuous, "foreign"), f.covariance, Some(f.prior)))
    val badBinding = AlderPatternPrediction.encoding(f.split.train, wrongBound, f.mapping, f.split.train.fingerprint).value
    assertEquals(badBinding.left.toOption.map(_.cause), Some(AlderPatternPredictionError.ArtifactTrainingBindingMismatch))
