package scalafim.fmri.mvpa.dataset.predictive

import alder.data.FixedCoverage
import alder.kernel.*
import gale.linalg.DMat
import multivar.core.SpaceRole
import munit.FunSuite
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*

class AlderPatternArtifactSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  test("Alder learner preserves the labelled target factors and training receipt"):
    val samples = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector("s1", "s2"), "trial", "none", "one"))
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector(7L, 9L), DataFingerprint.external("pattern-artifact-fixture")))
    val rows = right(AlderPredictiveAdmission.materialized(samples.descriptor, DMat.eye(2), DMat.dense(2, 1, Vector(1.0, 0.0)), Vector("s1", "s2"), mapping, right(MaterializationBudget(8L))))
    val split = right(rows.fixedHoldout(Vector(7L), Vector(9L), FixedCoverage.Exhaustive))
    val target = right(AxisRef.fromStableKeys("target", SpaceRole.Observed, Vector("score"), "feature", "none", "one"))
    val neural = right(AxisRef.fromStableKeys("neural", SpaceRole.Observed, Vector("v1", "v2"), "voxel", "none", "one"))
    val component = right(AxisRef.fromStableKeys("component", SpaceRole.Observed, Vector("c1"), "component", "none", "one"))
    val factors = right(PatternFactors(neural, target, component, DMat.dense(2, 1, Vector(1.0, 2.0)), DMat.dense(1, 1, Vector(3.0)), GaugeEvidence.PendingNumericalCheck))
    val prior = right(AxisValues(target, Vector(1.0)))
    val geometry = right(TargetGeometry.continuous(target, prior, prior, Vector("all" -> prior)))
    val binding = right(TrainingBinding(samples.descriptor, "fixture", split.train.fingerprint.digest))
    val artifact = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("neural:fixture", "target:fixture"), DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted, binding, Vector("training:fixture"), right(PatternFitDiagnostics(Vector(1.0), "declared", Vector.empty))))
    val preserved = right(AlderPatternArtifact.attach(split.train, artifact, split.train.fingerprint)(using FitContext.root(Seed(1L), PlanFingerprint("pattern"), SchemaFingerprint("scalafim.pattern.v1"), NumericMode.Deterministic)).value)
    assertEqualsDouble(preserved.artifact.factors.targetByComponent(0, 0), 3.0, 1e-12)
    assertEquals(preserved.artifact.interpretation, InterpretationStatus.ExperimentalFitOnly)
    assertEquals(preserved.trainingReceipt, preserved.trained.audit.data)
    val wrongExpected = AlderPatternArtifact.attach(split.train, artifact, DataFingerprint.external("foreign-training"))(using FitContext.root(Seed(1L), PlanFingerprint("foreign"), SchemaFingerprint("scalafim.pattern.v1"), NumericMode.Deterministic)).value
    assertEquals(wrongExpected.left.toOption.map(_.cause), Some(AlderPatternArtifactError.ExpectedTrainingReceiptMismatch))
    val wrongBinding = right(TrainingBinding(samples.descriptor, "fixture", "different-digest"))
    val wronglyBound = right(PatternArtifact(factors, geometry, CenteringPolicy.CenteredBeforeFit("neural:fixture", "target:fixture"), DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted, wrongBinding, Vector("training:fixture"), right(PatternFitDiagnostics(Vector(1.0), "declared", Vector.empty))))
    val wrongArtifact = AlderPatternArtifact.attach(split.train, wronglyBound, split.train.fingerprint)(using FitContext.root(Seed(2L), PlanFingerprint("wrong-binding"), SchemaFingerprint("scalafim.pattern.v1"), NumericMode.Deterministic)).value
    assertEquals(wrongArtifact.left.toOption.map(_.cause), Some(AlderPatternArtifactError.ArtifactTrainingBindingMismatch))
