package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.DMat
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.dataset.SubjectId
import scalafim.estimates.{DfRole, PoolingScope, ScientificFact}
import scalafim.fmri.group.*
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.group.*
import scalafim.fmri.mvpa.measurement.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class SubjectGroupBridgeSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, size: Int, units: String = "one") =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(size)(i => s"$name-$i"), "subject-group-fixture", units, "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private def close(left: DMat, expected: DMat, tolerance: Double = 1e-10): Unit =
    assertEquals((left.rows, left.cols), (expected.rows, expected.cols))
    for i <- 0 until left.rows; j <- 0 until left.cols do assertEqualsDouble(left(i, j), expected(i, j), tolerance, s"$i,$j")
  private val planId = PlanId.derived(EstimandId("subject-group-suite"), Vector(AxisSignature.unsafe("0" * 64)),
    AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question",
    Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  private def exposure(identity: String, result: String = "discovery") =
    EvidenceExposure.internal(ExposureReference(planId, identity, "fixture provenance", ResultIdentity(result)))
  private val units = right(SubjectCoordinateValueUnits("BOLD-percent/score", "BOLD-percent/task", "explicit numeric units"))
  private val facts = GroupFitProvenance(ScientificFact.Known("unpenalized loading coefficients"),
    ScientificFact.Known("known Gaussian row shape"), ScientificFact.Known("identified intercept and nuisance"),
    ScientificFact.Known("one declared confirmation block"))

  private final class Context(cg: DMat = DMat.eye(2)):
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

  private def joint(component: DMat): DMat =
    val feature = DMat.dense(2, 2, Vector(1.0, .25, .25, 9.0))
    DMat.tabulate(4, 4)((i, j) => feature(i / 2, j / 2) * component(i % 2, j % 2))

  test("independent small-group known-variance fixture delegates to adopted group and retains full covariance"):
    val context = new Context
    val subjects = Vector("A", "B", "C").map(name => new context.Subject(name))
    val covariances = Vector(DMat.dense(2, 2, Vector(1.0, .4, .4, 4.0)), DMat.dense(2, 2, Vector(4.0, -.3, -.3, 1.0)), DMat.dense(2, 2, Vector(1.0, .2, .2, 1.0))).map(joint)
    val inputs = subjects.indices.toVector.map: i =>
      val mean = DMat.dense(2, 2, Vector(1.0 + 2 * i, 2.0 + 2 * i, 10.0 + 20 * i, 20.0 + 20 * i))
      right(context.declared(subjects(i).coordinates(mean, covariances(i), origin = SubjectCovarianceOrigin.Known("explicit independent Gaussian coefficient covariance")), GroupVarianceOrigin.Known("explicit independent Gaussian coefficient covariance")))
    val input = right(SubjectGroupBridge.eager(context.shared, context.domain, subjects.map(_.key), inputs))
    val model = right(input.marginalModel(GroupDesign.intercept(3), input.data.subjects, SubjectGroupCalculation.KnownVarianceGaussianFixedEffects))
    val result = right(model.fit())
    // Independent arithmetic: component weights are (1,1/4,1) and
    // (1/4,1,1). Means=(3,14/3), variances=(4/9,4/9).
    val fits = result.native.contrasts.map(name => result.native.fit(name).get)
    assertEqualsDouble(fits(0).coefficients(0, 0), 3.0, 1e-12)
    assertEqualsDouble(fits(1).coefficients(0, 0), 14.0 / 3, 1e-12)
    assertEqualsDouble(fits(0).standardErrors(0, 0), 2.0 / 3, 1e-12)
    assertEqualsDouble(fits(1).standardErrors(0, 0), 2.0 / 3, 1e-12)
    assertEqualsDouble(fits(0).coefficients(0, 1), 30.0, 1e-11)
    assertEqualsDouble(fits(1).coefficients(0, 1), 140.0 / 3, 1e-11)
    assertEqualsDouble(fits(0).standardErrors(0, 1), 2.0, 1e-12)
    covariances.indices.foreach(i => close(input.fullCovariances(i), covariances(i)))
    assert(result.model.input eq input)
    assert(input.jointInference.isLeft && input.durableEstimateExport.isLeft)
    assertEquals(input.coefficientUnits, "BOLD-percent/score")
    assertEquals(input.domain.axis.units, "mm")

  test("qualified loading bridge adopts full component Gram and explicit cross-feature covariance as Estimated"):
    val context = new Context(DMat.dense(2, 2, Vector(1.0, .5, 0.0, 1.0)))
    val subject = new context.Subject("loading", DMat.dense(2, 2, Vector(1.0, 1.0, 0.0, 1.0)))
    val residualMatrix = DMat.dense(2, 2, Vector(.5, .125, .125, .125))
    val residual = right(LoadingResidualCovariance.bind(subject.loading, subject.jointLaw, residualMatrix, value("actual-residual-cross-products"), "Walsh residual cross-product covariance, df=4"))
    val input = right(subject.loadingSubject(residual))
    // Independent Walsh/R OLS: residual covariance [[.5,.125],[.125,.125]],
    // target inverse Gram in common shear coordinates [[1.25,-.5],[-.5,1]]/8.
    close(input.coordinates.estimates, DMat.dense(2, 2, Vector(2.5, -1.0, -2.5, 3.0)))
    close(input.coordinates.covariance, DMat.dense(4, 4, Vector(
      .078125, -.03125, .01953125, -.0078125,
      -.03125, .0625, -.0078125, .015625,
      .01953125, -.0078125, .01953125, -.0078125,
      -.0078125, .015625, -.0078125, .015625)))
    assertEquals(input.coordinates.degreesOfFreedom, SubjectCoordinateDf.Known(4.0, "VoxelLoadingConfirmation residual df n - nuisance/task rank"))
    input.uncertainty.origin match
      case GroupVarianceOrigin.Estimated(df) =>
        assertEquals(df.role, DfRole.Residual); assertEquals(df.values, GroupDfValues.Scalar(4.0)); assert(!df.approximate)
      case other => fail(s"loading uncertainty was promoted to $other")
    assertEquals(input.coordinates.covarianceOrigin,
      SubjectCovarianceOrigin.Estimated("VoxelLoadingConfirmation estimated residual feature covariance"))
    assertEquals(input.coordinates.degreesOfFreedomRole, SubjectCoordinateDfRole.Residual)
    // Extracting the public coordinates cannot discard the established origin,
    // even with the exact source and receipt or a renamed method string.
    for method <- Vector("known now", "VoxelLoadingConfirmation estimated residual feature covariance") do
      assert(context.declared(input.coordinates, GroupVarianceOrigin.Known(method)).isLeft)
    input.uncertainty.origin match
      case GroupVarianceOrigin.Estimated(df) =>
        assert(SubjectGroupSubject.declared(input.coordinates,
          input.uncertainty.copy(origin = GroupVarianceOrigin.Estimated(df.copy(role = DfRole.Effective)))).isLeft)
      case other => fail(other.toString)

  test("eager and materialized conversions preserve every covariance cell, df role and source"):
    val context = new Context
    val subjects = Vector("estimated", "approximate").map(name => new context.Subject(name))
    val dfs = Vector(SubjectCoordinateDf.Estimated(5.5, "estimated effective df"), SubjectCoordinateDf.Approximate(4.5, "explicit approximation"))
    val inputs = subjects.indices.toVector.map: i =>
      val coordinates = subjects(i).coordinates(DMat.eye(2), joint(DMat.dense(2, 2, Vector(2.0, .5, .5, 1.0))), dfs(i), if i == 0 then SubjectCovarianceOrigin.Estimated("estimated covariance") else SubjectCovarianceOrigin.Approximate("approximate covariance"), SubjectCoordinateDfRole.Effective)
      val df = GroupDegreesOfFreedom(DfRole.Effective, GroupDfValues.Scalar(if i == 0 then 5.5 else 4.5),
        if i == 0 then "estimated effective df" else "explicit approximation", i == 1)
      right(context.declared(coordinates, GroupVarianceOrigin.Estimated(df)))
    val original = right(SubjectGroupBridge.eager(context.shared, context.domain, subjects.map(_.key), inputs))
    val snapshot = right(original.materialize())
    val restored = right(SubjectGroupBridge.fromMaterialized(snapshot))
    assertEquals(restored.identity, original.identity)
    assertEquals(restored.originalDegreesOfFreedom, dfs)
    assertEquals(restored.originalDfRoles, Vector.fill(2)(SubjectCoordinateDfRole.Effective))
    assertEquals(restored.rows.map(_.source.coordinates.covarianceOrigin), inputs.map(_.coordinates.covarianceOrigin))
    assertEquals(restored.data.uncertainty.get.sources, original.data.uncertainty.get.sources)
    inputs.indices.foreach(i => close(restored.fullCovariances(i), inputs(i).coordinates.covariance))
    assert(restored.marginalModel(GroupDesign.intercept(2), restored.data.subjects, SubjectGroupCalculation.KnownVarianceGaussianFixedEffects).isLeft)
    val approximate = right(restored.marginalModel(GroupDesign.intercept(2), restored.data.subjects,
      SubjectGroupCalculation.ApproximateMixedEffects(TauEstimator.PauleMandel, MetaInference.ModifiedKnappHartung, "explicit numerical approximation, no calibration claim")))
    assert(right(approximate.fit()).model.input eq restored)

  test("mismatched source declarations and unsupported uncertainty refuse instead of becoming known Gaussian"):
    val context = new Context; val subject = new context.Subject("subject")
    val coordinates = subject.coordinates(DMat.eye(2), DMat.eye(4))
    val base = SubjectGroupUncertainty(coordinates.identity, coordinates.source.covarianceSource, coordinates.source.uncertaintyReceipt,
      GroupVarianceOrigin.Known("known covariance fixture"), facts, PoolingScope.JointRuns)
    assert(SubjectGroupSubject.declared(coordinates, base.copy(coordinateIdentity = "foreign-source")).isLeft)
    assert(SubjectGroupSubject.declared(coordinates, base.copy(covarianceSource = value("foreign-covariance"))).isLeft)
    assert(SubjectGroupSubject.declared(coordinates, base.copy(uncertaintyReceipt = "foreign-receipt")).isLeft)
    assert(SubjectGroupSubject.declared(coordinates, base.copy(origin = GroupVarianceOrigin.Unknown("not admitted"))).isLeft)
    val wrongDf = GroupDegreesOfFreedom(DfRole.Residual, GroupDfValues.Scalar(7), "declared df", false)
    assert(SubjectGroupSubject.declared(coordinates, base.copy(origin = GroupVarianceOrigin.Estimated(wrongDf))).isLeft)
    for role <- Vector(SubjectCoordinateDfRole.Reference, SubjectCoordinateDfRole.Unspecified) do
      val estimated = subject.coordinates(DMat.eye(2), DMat.eye(4), origin = SubjectCovarianceOrigin.Estimated("estimated"), dfRole = role)
      val df = GroupDegreesOfFreedom(DfRole.Residual, GroupDfValues.Scalar(6), "declared df", false)
      assert(context.declared(estimated, GroupVarianceOrigin.Estimated(df)).isLeft)

  test("reordered subjects, task/space endpoints and tampered materialized covariance refuse"):
    val context = new Context; val a = new context.Subject("A"); val b = new context.Subject("B")
    def known(s: context.Subject) = right(context.declared(s.coordinates(DMat.eye(2), DMat.eye(4)), GroupVarianceOrigin.Known("known covariance fixture")))
    val input = right(SubjectGroupBridge.eager(context.shared, context.domain, Vector(a.key, b.key), Vector(known(a), known(b))))
    assert(SubjectGroupBridge.eager(context.shared, context.domain, Vector(b.key, a.key), Vector(known(a), known(b))).isLeft)
    assert(input.marginalModel(GroupDesign.intercept(2), input.data.subjects.reverse, SubjectGroupCalculation.KnownVarianceGaussianFixedEffects).isLeft)
    val record = context.neural.toRecord
    val reordered = right(AxisRef.fromStableKeys(record.namespace, record.role, record.stableKeys.reverse, record.basis, record.units, record.scale, record.lineage))
    val wrongDomain = right(SubjectGroupDomain.samples(reordered, GroupGeometryEvidence.Unknown("wrong spatial order")))
    assert(SubjectGroupBridge.eager(context.shared, wrongDomain, Vector(a.key, b.key), Vector(known(a), known(b))).isLeft)
    val snapshot = right(input.materialize())
    val tampered = snapshot.copy(rows = snapshot.rows.updated(0, snapshot.rows.head.copy(covariance = DMat.eye(4) * 2.0)))
    assert(SubjectGroupBridge.fromMaterialized(tampered).isLeft)
    assert(SubjectGroupBridge.fromMaterialized(snapshot, maximumOwnedCells = 1).isLeft)

  test("residual covariance must name actual loading values, dimensions and diagonal variances"):
    val context = new Context; val subject = new context.Subject("source"); val other = new context.Subject("other")
    val sigma = DMat.dense(2, 2, Vector(.5, .125, .125, .125))
    assert(LoadingResidualCovariance.bind(subject.loading, subject.jointLaw, DMat.eye(2), value("bad"), "wrong diagonal").isLeft)
    assert(LoadingResidualCovariance.bind(subject.loading, subject.jointLaw, DMat.eye(1), value("bad"), "wrong shape").isLeft)
    assert(LoadingResidualCovariance.bind(subject.loading, subject.jointLaw, sigma, value("budget"), "source", maximumOwnedCells = 1).isLeft)
    val residual = right(LoadingResidualCovariance.bind(subject.loading, subject.jointLaw, sigma, value("actual"), "actual estimated feature covariance"))
    assert(other.loadingSubject(residual).left.toOption.exists(_.isInstanceOf[SubjectGroupError.Binding]))
    assert(LoadingResidualCovariance.bind(subject.loading, other.jointLaw, sigma, value("actual"), "foreign joint model")
      .left.toOption.exists(_.isInstanceOf[SubjectGroupError.Binding]))

  test("marginal Gaussian row laws cannot stand in for a homogeneous separable joint model"):
    val context = new Context; val subject = new context.Subject("joint-law")
    for model <- Vector(LoadingJointGaussianModel.GeneralJoint, LoadingJointGaussianModel.Unknown) do
      assert(LoadingSeparableGaussian.declare(subject.loading, model, "nonblank marginal Gaussian row receipt")
        .left.toOption.exists(_.isInstanceOf[SubjectGroupError.Unavailable]))
    assert(LoadingSeparableGaussian.declare(subject.loading, LoadingJointGaussianModel.HomogeneousSeparable, " ").isLeft)
    val missing = compileErrors("""scalafim.fmri.mvpa.group.LoadingResidualCovariance.bind(null, gale.linalg.DMat.eye(2), multivar.core.ValueIdentity.source(multivar.core.ValueId.unsafe("source")), "only marginal row law")""")
    assert(missing.contains("LoadingSeparableGaussian"), missing)
    // Independent analytic counterexample: rows are independent Gaussian and
    // both feature marginal row covariances are I4. Each row's feature pair has
    // covariance c=(.25,-.25,-.25,.25), so joint covariance is SPD (eigenvalues
    // .75/1.25) but feature covariance is not homogeneous across rows.
    val t = Vector(-2.0, -1.0, 1.0, 2.0)
    val c = Vector(.25, -.25, -.25, .25)
    val joint = DMat.tabulate(8, 8)((i, j) => if i == j then 1.0 else if i / 2 == j / 2 then c(i / 2) else 0.0)
    assert(joint.cholesky.isRight)
    val trueSlopeCrossCovariance = t.indices.map(i => t(i) * t(i) * c(i)).sum / 100.0
    val expectedPooledResidualCrossCovariance = t.indices.map(i => (1.0 - .25 - t(i) * t(i) / 10.0) * c(i)).sum / 2.0
    assertEqualsDouble(trueSlopeCrossCovariance, .015, 1e-15)
    assertEqualsDouble(expectedPooledResidualCrossCovariance, -.075, 1e-15)
    assertEqualsDouble(expectedPooledResidualCrossCovariance * .1, -.0075, 1e-15)
    // The unsupported GeneralJoint source is refused rather than producing the
    // wrong-sign Sigma_feature⊗G surrogate for this model.
    assert(LoadingSeparableGaussian.declare(subject.loading, LoadingJointGaussianModel.GeneralJoint,
      "independent rows with heterogeneous feature covariance; true cross covariance .015, pooled surrogate -.0075").isLeft)

  test("relabelled duplicate brain evidence refuses before covariance access but independent brains may share row schemas and targets"):
    val context = new Context; val a = new context.Subject("A")
    val bKey = right(SubjectCoordinateKey("B"))
    val labels = right(Column.fromValues(a.rows, Vector.fill(8)(bKey), value("B-row-subject-labels")))
    val original = right(context.declared(a.coordinates(DMat.eye(2), DMat.eye(4)), GroupVarianceOrigin.Known("known covariance fixture")))
    def second(brain: EvidenceIdentity): SubjectGroupSubject =
      val input = right(SubjectCoefficientEstimate.bind(labels, context.neural, a.design, DMat.eye(2), DMat.eye(4), brain, a.targets.identity,
        value("B-effects"), value("B-covariance"), SubjectCoordinateDf.Known(6, "declared df"), "B-uncertainty-receipt",
        context.stability(a.local), units, SubjectCovarianceOrigin.Known("known covariance fixture"), SubjectCoordinateDfRole.Residual))
      val coordinates = right(SubjectCoordinates.transport(input, context.shared, a.alignment, SubjectComparisonKind.SharedComponentCoefficients))
      right(context.declared(coordinates, GroupVarianceOrigin.Known("known covariance fixture")))
    val duplicated = second(a.brain.identity)
    val poison = DMat.dense(1, 1, Vector(Double.NaN))
    val snapshot = MaterializedSubjectGroupInput(context.shared, context.domain, Vector(a.key, bKey),
      Vector(MaterializedSubjectGroupRow(original, original.coordinates.estimates, original.coordinates.covariance),
        MaterializedSubjectGroupRow(duplicated, poison, poison)))
    val refusal = SubjectGroupBridge.fromMaterialized(snapshot, maximumOwnedCells = 0)
    assert(refusal.left.toOption.exists {
      case SubjectGroupError.Binding(detail) => detail.contains("same actual brain evidence")
      case _ => false
    }, refusal.toString)
    val independentBrain = right(Observations.fromDense(a.rows, context.neural, a.x,
      value("independent-B-brain-values"), source("independent-B-brain")))
    val independent = second(independentBrain.identity)
    assert(SubjectGroupBridge.eager(context.shared, context.domain, Vector(a.key, bKey), Vector(original, independent)).isRight)
