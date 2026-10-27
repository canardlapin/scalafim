package scalafim.fmri.mvpa.group

import mvpa4s.testkit.MvpaFixtures
import mvpa4s.analysis.*
import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import neuroim4s.spaces.SubjectId
import scalafim.estimates.{DfRole, PoolingScope, ScientificFact}
import scalafim.fmri.group.*
import mvpa4s.*
import scalafim.fmri.mvpa.group.*
import mvpa4s.measurement.*
import mvpa4s.pattern.*
import locus4s.provenance.{Provenance, ProvenanceId, SourceId}

trait SubjectGroupScenarioFixture extends munit.FunSuite:
  protected def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  protected def axis(name: String, size: Int, units: String = "one") =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(size)(i => s"$name-$i"), "subject-group-fixture", units, "raw"))
  protected def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  protected def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  protected def close(left: DMat, expected: DMat, tolerance: Double = 1e-10): Unit =
    assertEquals((left.rows, left.cols), (expected.rows, expected.cols))
    for i <- 0 until left.rows; j <- 0 until left.cols do assertEqualsDouble(left(i, j), expected(i, j), tolerance, s"$i,$j")
  protected val planId = MvpaFixtures.unsafePlanId(EstimandId("subject-group-suite"), Vector(MvpaFixtures.unsafeSignature("0" * 64)),
    MvpaFixtures.unsafeSignature("1" * 64), MvpaFixtures.unsafeSignature("2" * 64), "evidence", "design", "frame", "question",
    Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  protected def exposure(identity: String, result: String = "discovery") =
    EvidenceExposure.internal(ExposureReference(planId, identity, "fixture provenance", ResultIdentity(result)))
  protected val units = right(SubjectCoordinateValueUnits("BOLD-percent/score", "BOLD-percent/task", "explicit numeric units"))
  protected val facts = GroupFitProvenance(ScientificFact.Known("unpenalized loading coefficients"),
    ScientificFact.Known("known Gaussian row shape"), ScientificFact.Known("identified intercept and nuisance"),
    ScientificFact.Known("one declared confirmation block"))

  protected final class Context(cg: DMat = DMat.eye(2)):
    val neural = axis("common-features", 2, "mm")
    val task = axis("common-task", 2)
    val components = axis("common-components", 2)
    def discovery(name: String, cs: DMat): DiscoverySnapshot[?, ?] =
      val rows = axis(s"$name-discovery-rows", 4); val independent = axis(s"$name-discovery-units", 4)
      val factors = right(PatternFactors(neural, task, components, DMat.eye(2), cs, GaugeEvidence.PendingNumericalCheck))
      val unit = right(AxisValues(task, Vector(1.0, 1.0)))
      val artifact = right(PatternArtifact(factors, right(TargetGeometry.continuous(task, unit, unit, Vector("all" -> unit))),
        CenteringPolicy.CenteredBeforeFit("x", "y"), DegenerateTargetPolicy.Refuse, ResidualCovarianceCapability.NotFitted,
        right(TrainingBinding(rows.descriptor, s"$name-fit", "fixed")), Vector("discovery-only"),
        right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
      right(DiscoverySnapshot(right(ConfirmationUnits(rows, independent, Vector.range(0, 4))), artifact,
        right(FrozenProjection(neural, components, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)),
        right(FrozenProjection(task, components, cs, ProjectionKind.DeclaredLinearProjection)), "support", "rotation", "preparation"))
    def stability(d: DiscoverySnapshot[?, ?]) = right(SubjectAxisStability.freeze(d, exposure(d.identity), SubjectStabilityKind.StableAxes, "discovery axis diagnostic"))
    val global = discovery("global", cg)
    val shared = right(SharedTaskCoordinates.freeze(global, exposure(global.identity), neural, stability(global), units))
    val domain = right(SubjectGroupDomain.samples(neural, GroupGeometryEvidence.Unknown("abstract identified measurements; no voxel registration claim")))

    final class Subject(name: String, cs: DMat = DMat.eye(2)):
      val key = right(SubjectCoordinateKey(name))
      val rows = axis(s"$name-confirmation-rows", 8); val independent = axis(s"$name-confirmation-units", 8)
      val local = discovery(name, cs)
      val confirmation = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, independent, Vector.range(0, 8))), "confirmation"))
      val drift = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
      val nuisance = right(ConfirmationNuisance(rows, DMat.tabulate(8, 2)((i, j) => if j == 0 then 1.0 else drift(i))))
      val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, local, confirmation,
        exposure(confirmation.identity), C1Contract("fixed discovery", "forward loading", "these subject rows", Vector("component-0", "component-1"),
          "estimated residual covariance", Vector("projection"), Vector("OLS")), nuisance, ConfirmationErrorLaw.IndependentGaussian))
      val subjects = right(Column.fromValues(rows, Vector.fill(8)(key), value(s"$name-subject-labels")))
      val t1 = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
      val t2 = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
      val e1 = Vector(1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0)
      val e2 = Vector(1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0)
      val x = DMat.tabulate(8, 2)((i, j) => if j == 0 then 7 + 4 * drift(i) + 2 * t1(i) - t2(i) + .5 * e1(i)
        else -3 - 2 * drift(i) - t1(i) + 3 * t2(i) + .25 * (.5 * e1(i) + math.sqrt(.75) * e2(i)))
      val brain = right(Observations.fromDense(rows, neural, x, value(s"$name-brain"), source(s"$name-brain")))
      val targets = right(MultiResponse.fromDense(rows, task, DMat.tabulate(8, 2)((i, j) => if j == 0 then t1(i) else t2(i)),
        value(s"$name-target"), source(s"$name-target")))
      val leg = right(MeasurementLeg.identity(neural, MeasurementId.unsafe(s"$name-identity-map")))
      val alignment = right(SubjectSpatialAlignment.freeze(local, shared, leg, exposure(local.identity, leg.descriptor.semanticId)))
      val loading = right(VoxelLoadingConfirmation.fit(design, brain, targets, PatternReplay.SinglePass, LoadingConfirmationBudget(batchVoxels = 2)))
      def coordinates(mean: DMat, covariance: DMat, df: SubjectCoordinateDf = SubjectCoordinateDf.Known(6, "declared df"),
          origin: SubjectCovarianceOrigin = SubjectCovarianceOrigin.Known("known covariance fixture"),
          dfRole: SubjectCoordinateDfRole = SubjectCoordinateDfRole.Residual) =
        val input = right(SubjectCoefficientEstimate.bind(subjects, neural, design, mean, covariance, brain.identity, targets.identity,
          value(s"$name-effects"), value(s"$name-covariance"), df, "declared joint covariance source", stability(local), units, origin, dfRole))
        right(SubjectCoordinates.transport(input, shared, alignment, SubjectComparisonKind.SharedComponentCoefficients))
      val jointLaw = right(LoadingSeparableGaussian.declare(loading, LoadingJointGaussianModel.HomogeneousSeparable,
        "known row shape with homogeneous joint Gaussian feature covariance"))
      def loadingSubject(residual: LoadingResidualCovariance) = SubjectGroupSubject.fromLoading(subjects, neural, design, loading, residual,
        shared, alignment, SubjectComparisonKind.SharedComponentCoefficients, stability(local), units, facts, PoolingScope.JointRuns)

    def declared(coordinates: SubjectCoordinates, origin: GroupVarianceOrigin) =
      SubjectGroupSubject.declared(coordinates, SubjectGroupUncertainty(coordinates.identity, coordinates.source.covarianceSource,
        coordinates.source.uncertaintyReceipt, origin, facts, PoolingScope.JointRuns))

  protected def joint(component: DMat): DMat =
    val feature = DMat.dense(2, 2, Vector(1.0, .25, .25, 9.0))
    DMat.tabulate(4, 4)((i, j) => feature(i / 2, j / 2) * component(i % 2, j % 2))

