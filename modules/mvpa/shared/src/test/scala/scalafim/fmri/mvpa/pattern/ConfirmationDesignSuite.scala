package scalafim.fmri.mvpa.analysis

import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import munit.FunSuite
import scalafim.fmri.mvpa.{AxisRef, AxisSignature}
import scalafim.fmri.mvpa.pattern.*

class ConfirmationDesignSuite extends FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), value => value)
  private def axis(name: String, keys: Vector[String]) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, keys, "fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private val plan = PlanId.derived(EstimandId("confirmation-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  private val contract = C1Contract("fixed realized discovery representation", "zero conditional association", "confirmation rows only; no population claim", Vector("all component-voxel tests"), "Gaussian conditional reference", Vector("projection", "rank", "spatial support", "rotation", "preprocessing"), Vector("nuisance fit", "sufficient statistics"))

  private def artifact(training: AxisRef[?], scale: Double = 1.0,
      gauge: GaugeEvidence = GaugeEvidence.PendingNumericalCheck,
      lineage: Vector[String] = Vector("fit"), notes: Vector[String] = Vector.empty) =
    val n = axis("neural", Vector("n0", "n1")); val q = axis("target", Vector("q0")); val r = axis("component", Vector("r0"))
    val unit = right(AxisValues(q, Vector(1.0)))
    val factors = right(PatternFactors(n, q, r, DMat.dense(2, 1, Vector(scale, 0.0)), DMat.eye(1), gauge))
    right(PatternArtifact(factors, right(TargetGeometry.continuous(q, unit, unit, Vector("all" -> unit))), CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.DiagonalPlusLowRank(n.descriptor, 0), right(TrainingBinding(training.descriptor, "fit", "digest")), lineage, right(PatternFitDiagnostics(Vector(0.0), "fixture", notes))))

  private def freeze[S, U](samples: ConfirmationUnits[S, U], fitted: PatternArtifact): DiscoverySnapshot[S, U] =
    val brain = right(FrozenProjection(fitted.factors.neuralAxis, fitted.factors.componentAxis, fitted.factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection))
    val target = right(FrozenProjection(fitted.factors.targetAxis, fitted.factors.componentAxis, fitted.factors.targetByComponent, ProjectionKind.DeclaredLinearProjection))
    right(DiscoverySnapshot(samples, fitted, brain, target, "support-v1", "rotation-v1", "discover-prep"))

  private def snapshot(rows: Vector[String], units: Vector[String], mapping: Vector[Int], receipt: String) =
    val rowAxis = axis(s"rows-$receipt", rows); val unitAxis = axis(s"units-$receipt", units)
    right(ConfirmationSnapshot(right(ConfirmationUnits(rowAxis, unitAxis, mapping)), receipt))
  private def discovery(unitKeys: Vector[String] = Vector("d0", "d1"), scale: Double = 1.0) =
    val rows = axis("discovery-rows", Vector("dr0", "dr1")); val units = axis("discovery-units", unitKeys)
    freeze(right(ConfirmationUnits(rows, units, Vector(0, 1))), artifact(rows, scale))
  private def exposure(identity: String) = EvidenceExposure.internal(ExposureReference(plan, identity, "provenance", ResultIdentity("confirmation-result")))
  private def nuisance[S, U](snapshot: ConfirmationSnapshot[S, U], values: Vector[Double]) =
    right(ConfirmationNuisance(snapshot.samples.rows, DMat.dense(snapshot.samples.rows.size, 1, values)))

  test("admits a concrete untouched C1 design with disjoint actual units"):
    val confirmation = snapshot(Vector("cr0", "cr1", "cr2", "cr3"), Vector("c0", "c1", "c2", "c3"), Vector(0, 1, 2, 3), "confirm-prep")
    val admitted = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), confirmation, exposure(confirmation.identity), contract, nuisance(confirmation, Vector(1.0, 1.0, 1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian))
    assert(admitted.identity.nonEmpty)

  test("refuses leaked, foreign, unknown, and overlapping confirmation evidence"):
    val confirmation = snapshot(Vector("cr0", "cr1"), Vector("d0", "c1"), Vector(0, 1), "leak-prep")
    val z = nuisance(confirmation, Vector(1.0, 1.0))
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), confirmation, exposure(confirmation.identity), contract, z, ConfirmationErrorLaw.IndependentGaussian).isLeft)
    val disjoint = snapshot(Vector("fr0", "fr1"), Vector("f0", "f1"), Vector(0, 1), "foreign-prep")
    val dz = nuisance(disjoint, Vector(1.0, 1.0))
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), disjoint, exposure("other-snapshot"), contract, dz, ConfirmationErrorLaw.IndependentGaussian).isLeft)
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), disjoint, EvidenceExposure.external(ExposureReference(plan, disjoint.identity, "p", ResultIdentity("external"))), contract, dz, ConfirmationErrorLaw.IndependentGaussian).isLeft)

  test("identity changes with frozen artifact values and nuisance design, while unavailable claims are typed"):
    val confirmation = snapshot(Vector("ir0", "ir1"), Vector("i0", "i1"), Vector(0, 1), "identity-prep")
    val first = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(scale = 1.0), confirmation, exposure(confirmation.identity), contract, nuisance(confirmation, Vector(1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian))
    val changedArtifact = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(scale = 2.0), confirmation, exposure(confirmation.identity), contract, nuisance(confirmation, Vector(1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian))
    val changedNuisance = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(scale = 1.0), confirmation, exposure(confirmation.identity), contract, nuisance(confirmation, Vector(1.0, -1.0)), ConfirmationErrorLaw.IndependentGaussian))
    val changedFamily = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(scale = 1.0), confirmation, exposure(confirmation.identity), contract.copy(multiplicityFamily = Vector("all", "component-voxel tests")), nuisance(confirmation, Vector(1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian))
    assertNotEquals(first.identity, changedArtifact.identity); assertNotEquals(first.identity, changedNuisance.identity); assertNotEquals(first.identity, changedFamily.identity)
    assert(ConfirmationDesign.admit(ConfirmationClaim.CompleteSelectionAwareC2, discovery(), confirmation, exposure(confirmation.identity), contract, nuisance(confirmation, Vector(1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian).left.toOption.exists:
      case ConfirmationDesignError.UnavailableClaim("C2", _) => true
      case _ => false)

  test("actual recorded confirmation reads and failed attempts block untouched admission"):
    val confirmation = snapshot(Vector("ex0", "ex1"), Vector("ex-unit0", "ex-unit1"), Vector(0, 1), "exposure")
    val z = nuisance(confirmation, Vector(1.0, 1.0))
    for payload <- Vector(ExposurePayload.Payload, ExposurePayload.DerivedScore) do
      val initial = exposure(confirmation.identity)
      val purpose = if payload == ExposurePayload.Payload then ExposurePurpose.PayloadRead else ExposurePurpose.DerivedScoreView
      val request = ExposureRequest(purpose, ExposureActorRole.Analyst, ExposureScope.Holdout, payload, ExposureAssurance.Instrumented, 1L)
      val permit = right(ExposureControl.permit(initial, request))
      val attempted = ExposureControl.read(initial, permit, request)(Left("recorded failed attempt")) match
        case ExposureAttempt.Failed(_, record) => record
        case other => fail(s"expected recorded failure, got $other")
      assertEquals(attempted.events.size, 1)
      assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), confirmation, attempted, contract, z, ConfirmationErrorLaw.IndependentGaussian).isLeft)

  test("same nominal rows cannot be made independent by relabeling their units"):
    val d = discovery()
    val c = right(ConfirmationSnapshot(right(ConfirmationUnits(d.samples.rows,
      axis("renamed-units", Vector("new0", "new1")), Vector(0, 1))), "renamed-prep"))
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, d, c, exposure(c.identity), contract, nuisance(c, Vector(1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian).isLeft)

  test("independent nominal datasets may retain identical local row ordinal keys"):
    val rowKeys = Vector("dataset-row:0", "dataset-row:1")
    val dRows = axis("dataset-A", rowKeys)
    val d = freeze(right(ConfirmationUnits(dRows, axis("A-subjects", Vector("A:0", "A:1")), Vector(0, 1))), artifact(dRows))
    val c = snapshot(rowKeys, Vector("B:0", "B:1"), Vector(0, 1), "dataset-B")
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, d, c, exposure(c.identity), contract, nuisance(c, Vector(1.0, 1.0)), ConfirmationErrorLaw.IndependentGaussian).isRight)

  test("foreign projection, nuisance, and discovery training axes refuse despite equal shapes"):
    val d = discovery()
    val f = d.artifact
    val foreign = axis("foreign-neural", Vector("f0", "f1"))
    val foreignBrain = right(FrozenProjection(foreign, f.factors.componentAxis, f.factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection))
    assert(DiscoverySnapshot(d.samples, f, foreignBrain, d.targetProjection, "support", "rotation", "prep").isLeft)
    val foreignRows = right(ConfirmationUnits(axis("foreign-training", Vector("dr0", "dr1")), d.samples.units, Vector(0, 1)))
    assert(DiscoverySnapshot(foreignRows, f, d.brainProjection, d.targetProjection, "support", "rotation", "prep").isLeft)
    val c = snapshot(Vector("nx0", "nx1"), Vector("nu0", "nu1"), Vector(0, 1), "nuisance")
    val wrongNuisance = right(ConfirmationNuisance(axis("foreign-nuisance", Vector("nx0", "nx1")), DMat.dense(2, 1, Vector(1.0, 1.0))))
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, d, c, exposure(c.identity), contract, wrongNuisance, ConfirmationErrorLaw.IndependentGaussian).isLeft)

  test("frozen identity binds rank evidence, training lineage, and fit diagnostics"):
    val d = discovery()
    val changedGauge = freeze(d.samples, artifact(d.samples.rows, gauge = GaugeEvidence.DeclaredSolverDiagnostic("known-rank", 1)))
    val changedLineage = freeze(d.samples, artifact(d.samples.rows, lineage = Vector("fit", "training-selection")))
    val changedNotes = freeze(d.samples, artifact(d.samples.rows, notes = Vector("fit receipt")))
    assertNotEquals(d.identity, changedGauge.identity)
    assertNotEquals(d.identity, changedLineage.identity)
    assertNotEquals(d.identity, changedNotes.identity)

  test("known repeated Gaussian blocks are actual numeric capabilities and estimated covariance refuses"):
    val c = snapshot(Vector("rep0", "rep1", "rep2", "rep3"), Vector("subject-c0", "subject-c1"), Vector(0, 0, 1, 1), "repeated")
    val u = DMat.dense(4, 2, Vector(.5, 0.0, .4, 0.0, 0.0, .6, 0.0, .7))
    val covariance = right(ResidualCovariance.fromFactors(c.samples.rows, Vector.fill(4)(1.0), u))
    def bound(status: TemporalCovarianceStatus) = right(BoundConfirmationCovariance(c.samples.rows, covariance, "known-blocks", status))
    val repeated = right(BoundRepeatedGaussian(c.samples, bound(TemporalCovarianceStatus.KnownGaussian)))
    val rebuilt = right(BoundRepeatedGaussian(c.samples, bound(TemporalCovarianceStatus.KnownGaussian)))
    assertEquals(repeated.identity, rebuilt.identity)
    val z = nuisance(c, Vector.fill(4)(1.0))
    val first = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.RepeatedSubjects(repeated)))
    val second = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.RepeatedSubjects(rebuilt)))
    assertEquals(first.identity, second.identity)
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.IndependentGaussian).isLeft)
    assert(BoundRepeatedGaussian(c.samples, bound(TemporalCovarianceStatus.EstimatedUncalibrated)).left.toOption.exists(_.isInstanceOf[ConfirmationDesignError.UnavailableClaim]))
    val zeroCov = right(ResidualCovariance.fromFactors(c.samples.rows, Vector.fill(4)(1.0), DMat.zeros(4, 1)))
    val remapped = right(ConfirmationUnits(c.samples.rows, c.samples.units, Vector(0, 1, 0, 1)))
    val other = right(BoundRepeatedGaussian(remapped, right(BoundConfirmationCovariance(c.samples.rows, zeroCov, "remapped", TemporalCovarianceStatus.KnownGaussian))))
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.RepeatedSubjects(other)).isLeft)

  test("large finite residual scales cannot hide cross-subject dependence through overflow"):
    val c = snapshot(Vector("large0", "large1", "large2", "large3"), Vector("large-subject0", "large-subject1"), Vector(0, 0, 1, 1), "large")
    // Cross-subject covariance 1e200 and correlation .5; d_i*d_j overflows.
    val covariance = right(ResidualCovariance.fromFactors(c.samples.rows, Vector.fill(4)(1e200), DMat.dense(4, 1, Vector.fill(4)(1e100))))
    val bound = right(BoundConfirmationCovariance(c.samples.rows, covariance, "large-known", TemporalCovarianceStatus.KnownGaussian))
    assert(BoundRepeatedGaussian(c.samples, bound).isLeft)
    assert(BoundRepeatedGaussian(c.samples, bound, relativeTolerance = 1.0).isLeft)
    assert(BoundRepeatedGaussian(c.samples, bound, maximumWorkspaceCells = 0L).isLeft)

  test("dependent-time Gaussian covariance is bound numerically and estimated references remain unavailable"):
    val c = snapshot(Vector("time0", "time1", "time2", "time3"), Vector("independent-run"), Vector(0, 0, 0, 0), "temporal")
    val covariance = right(ResidualCovariance.fromFactors(c.samples.rows, Vector.fill(4)(1.0), DMat.dense(4, 1, Vector(.2, .3, .4, .5))))
    def bound(status: TemporalCovarianceStatus) = right(BoundConfirmationCovariance(c.samples.rows, covariance, "known-temporal-model", status))
    val z = nuisance(c, Vector.fill(4)(1.0))
    val first = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.DependentTime(bound(TemporalCovarianceStatus.KnownGaussian))))
    val second = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.DependentTime(bound(TemporalCovarianceStatus.KnownGaussian))))
    assertEquals(first.identity, second.identity)
    assert(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery(), c, exposure(c.identity), contract, z, ConfirmationErrorLaw.DependentTime(bound(TemporalCovarianceStatus.EstimatedUncalibrated))).isLeft)
    assert(ConfirmationNuisance(c.samples.rows, DMat.eye(4)).isLeft)
    assert(ConfirmationNuisance(c.samples.rows, DMat.dense(4, 1, Vector.fill(4)(1.0)), maximumWorkspaceCells = 0L).isLeft)
