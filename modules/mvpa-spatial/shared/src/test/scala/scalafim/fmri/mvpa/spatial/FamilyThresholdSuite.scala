package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.fmri.mvpa.spatial.*
import scalafim.fmri.threshold.{Alpha, MaxNull, MaxNullDistribution, NullReference, ThresholdAlternative, ThresholdCutoff}
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class FamilyThresholdSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed,
    Vector.tabulate(count)(i => s"$name-$i"), "family-threshold-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val plan = PlanId.derived(EstimandId("family-threshold-suite"), Vector(AxisSignature.unsafe("0" * 64)),
    AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question",
    Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)

  private final class Fixture:
    val rows = axis("confirmation", 5); val units = axis("confirm-units", 5)
    val training = axis("discovery", 4); val trainUnits = axis("discover-units", 4)
    val neural = axis("neural", 2); val target = axis("target", 1); val component = axis("component", 1)
    val factors = right(PatternFactors(neural, target, component, DMat.dense(2, 1, Vector(1.0, 0.0)),
      DMat.eye(1), GaugeEvidence.PendingNumericalCheck))
    val one = right(AxisValues(target, Vector(1.0)))
    val artifact = right(PatternArtifact(factors, right(TargetGeometry.continuous(target, one, one, Vector("all" -> one))),
      CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "discovery", "frozen")),
      Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainUnits, Vector(0, 1, 2, 3))), artifact,
      right(FrozenProjection(neural, component, factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(1), ProjectionKind.DeclaredLinearProjection)), "support", "none", "discovery-prep"))
    val snapshot = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, Vector.range(0, 5))), "confirm-prep"))
    val exposure = EvidenceExposure.internal(ExposureReference(plan, snapshot.identity, "fixture", ResultIdentity("family-threshold-result")))
    val members = Vector("voxel-0-omnibus", "voxel-1-omnibus")
    val nuisance = right(ConfirmationNuisance(rows, DMat.tabulate(5, 1)((_, _) => 1.0)))
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, snapshot, exposure,
      C1Contract("fixed discovery and confirmation targets", "all target coefficients zero per voxel", "held-out forward omnibus",
        members, "separable joint Gaussian known row shape", Vector("target projection"), Vector("voxel regressions")),
      nuisance, ConfirmationErrorLaw.IndependentGaussian))
    val brain = right(Observations.fromDense(rows, neural,
      DMat.dense(5, 2, Vector(2.0, 1.0, -1.0, 3.0, 3.0, -2.0, 0.0, 2.0, 4.0, 0.0)), value("brain"), source("brain")))
    val targets = right(MultiResponse.fromDense(rows, target, DMat.dense(5, 1, Vector(1.0, -2.0, 0.0, 2.0, -1.0)), value("target"), source("target")))
    def reference(seed: Long = 73L, alpha: Double = 0.05) = right(FrozenVoxelOmnibusReference.freeze(design,
      members, brain.identity, targets.identity, exposure, "conditional separable joint Gaussian", Seed.fromLong(seed), 23, PatternReplay.SinglePass, alpha))
    def completed(ref: FrozenVoxelOmnibusReference) =
      val prepared = right(VoxelFamilyRandomization.prepare(ref, brain, targets))
      right(right(prepared.run()).completed)

  test("sealed complete fields use inclusive plus-one maxima and the same repaired cutoff decisions"):
    val f = new Fixture; val reference = f.reference(); val complete = f.completed(reference)
    val result = right(FamilyThreshold.omnibus(reference, complete))
    complete.observed.zipWithIndex.foreach: (score, index) =>
      val expected = (1.0 + complete.maxima.count(_ >= score)) / 24.0
      assertEqualsDouble(result.adjustedP(index).value, expected, 0.0)
      assertEquals(result.rejected(index), expected <= reference.alpha)
      assertEquals(result.cutoff.rejects(score).toOption.get, expected <= reference.alpha)
    assert(result.admittedC1.isLeft)
    assert(result.fdr.isLeft)
    assert(FamilyThreshold.mixedComponentFamilyUnavailable.isLeft)

  test("foreign null, seed or alpha reference cannot reuse a completed family"):
    val f = new Fixture; val reference = f.reference(); val complete = f.completed(reference)
    assert(FamilyThreshold.omnibus(f.reference(seed = 74L), complete).isLeft)
    assert(FamilyThreshold.omnibus(f.reference(alpha = 0.1), complete).isLeft)
    val foreign = new Fixture
    val changedSource = right(Observations.fromDense(foreign.rows, foreign.neural,
      DMat.tabulate(5, 2)((i, j) => (i + j).toDouble), value("changed-source"), source("changed-source")))
    val changed = right(FrozenVoxelOmnibusReference.freeze(foreign.design, foreign.members, changedSource.identity,
      foreign.targets.identity, foreign.exposure, "conditional separable joint Gaussian", Seed.fromLong(73L), 23, PatternReplay.SinglePass))
    assert(FamilyThreshold.omnibus(changed, complete).isLeft)

  test("the repaired threshold boundary rejects strictly above tied maximum null at B19 alpha .05"):
    // Independent deterministic scalar threshold control. The family adapter
    // delegates to this API rather than approximating its boundary.
    val maxima = Array.tabulate(19)(i => i.toDouble + 1.0)
    val distribution = right(MaxNullDistribution.fromOrientedMaxima(maxima, ThresholdAlternative.Greater, NullReference.MonteCarlo))
    val cutoff = right(MaxNull.cutoff(distribution, right(Alpha(0.05))))
    cutoff match
      case ThresholdCutoff.Exclusive(value) => assertEqualsDouble(value, 19.0, 0.0)
      case other => fail(s"expected exclusive upper-tail boundary, got $other")
    assertEqualsDouble(right(MaxNull.pValues(Array(19.0), distribution)).head.value, 0.1, 0.0)
    assertEqualsDouble(right(MaxNull.pValues(Array(Math.nextUp(19.0)), distribution)).head.value, 0.05, 0.0)
    assertEquals(right(cutoff.rejects(19.0)), false)
    assertEquals(right(cutoff.rejects(Math.nextUp(19.0))), true)

  test("fixed alpha below attainable MC probability yields explicit no rejections"):
    val f = new Fixture; val reference = f.reference(alpha = Math.nextDown(1.0 / 24.0))
    val result = right(FamilyThreshold.omnibus(reference, f.completed(reference)))
    assertEquals(result.cutoff, ThresholdCutoff.NoRejections)
    assertEquals(result.rejected, Vector(false, false))
