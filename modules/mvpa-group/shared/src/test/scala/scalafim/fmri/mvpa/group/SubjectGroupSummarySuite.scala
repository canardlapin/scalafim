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

class SubjectGroupSummarySuite extends munit.FunSuite:
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
      def coordinates(mean: DMat, covariance: DMat, df: SubjectCoordinateDf = SubjectCoordinateDf.Known(6, "declared df"),
          origin: SubjectCovarianceOrigin = SubjectCovarianceOrigin.Known("known covariance fixture"),
          dfRole: SubjectCoordinateDfRole = SubjectCoordinateDfRole.Residual,
          kind: SubjectComparisonKind = SubjectComparisonKind.SharedComponentCoefficients) =
        val input = right(SubjectCoefficientEstimate.bind(subjects, neural, design, mean, covariance, brain.identity, targets.identity,
          value(s"$name-effects"), value(s"$name-covariance"), df, "declared joint covariance source", stability(local), units, origin, dfRole))
        right(SubjectCoordinates.transport(input, shared, alignment, kind))
    def declared(coordinates: SubjectCoordinates, origin: GroupVarianceOrigin) =
      SubjectGroupSubject.declared(coordinates, SubjectGroupUncertainty(coordinates.identity, coordinates.source.covarianceSource,
        coordinates.source.uncertaintyReceipt, origin, facts, PoolingScope.JointRuns))

  private def joint(component: DMat): DMat =
    val feature = DMat.dense(2, 2, Vector(1.0, .25, .25, 9.0))
    DMat.tabulate(4, 4)((i, j) => feature(i / 2, j / 2) * component(i % 2, j % 2))

  private val contract = SubjectPopulationContract("declared study population", "independent recruited subjects",
    "fixed discovery task coordinates and measurement map", "subject intercept and nuisance", "all task/measurement coordinates")
  private val mixed = SubjectGroupCalculation.ApproximateMixedEffects(TauEstimator.PauleMandel,
    MetaInference.ModifiedKnappHartung, "explicit plug-in estimated-variance arithmetic; qualification remains separate")

  private def cohort(estimated: Boolean = false, count: Int = 3, flat: Boolean = false,
      kind: SubjectComparisonKind = SubjectComparisonKind.SharedComponentCoefficients, overflow: Boolean = false,
      offset: Double = 1.0, variances: Vector[Vector[Double]] = Vector(Vector(1.0, 4.0), Vector(4.0, 1.0), Vector(1.0, 1.0))
  ): SubjectGroupInput =
    val context = new Context
    val subjects = Vector.tabulate(count)(i => new context.Subject(s"subject-$i"))
    val inputs = subjects.indices.toVector.map: i =>
      val mean = DMat.tabulate(2, 2)((j, k) => (if flat then 2.0 else offset + 2 * i + k) *
        (if j == 0 then 1.0 else if overflow then 1e160 else 10.0))
      val covariance = joint(DMat.dense(2, 2, Vector(variances(i)(0), .2, .2, variances(i)(1))))
      val origin = if estimated then SubjectCovarianceOrigin.Estimated("estimated fixture") else SubjectCovarianceOrigin.Known("known covariance fixture")
      val native = if estimated then GroupVarianceOrigin.Estimated(GroupDegreesOfFreedom(DfRole.Residual,
        GroupDfValues.Scalar(6), "declared df", false)) else GroupVarianceOrigin.Known("known covariance fixture")
      right(context.declared(subjects(i).coordinates(mean, covariance, origin = origin, kind = kind), native))
    right(SubjectGroupBridge.eager(context.shared, context.domain, subjects.map(_.key), inputs))

  test("known-variance common-effect summary matches independent R and retains every subject covariance"):
    val input = cohort()
    val result = right(SubjectGroupSummary.fit(input, contract, SubjectGroupCalculation.KnownVarianceGaussianFixedEffects))
    close(result.mean, DMat.dense(2, 2, Vector(3.0, 14.0 / 3, 30.0, 140.0 / 3)))
    close(result.standardErrors, DMat.dense(2, 2, Vector(2.0 / 3, 2.0 / 3, 2.0, 2.0)))
    close(result.empiricalMean, DMat.dense(2, 2, Vector(3.0, 4.0, 30.0, 40.0)))
    close(result.empiricalVariance, DMat.dense(2, 2, Vector(4.0, 4.0, 400.0, 400.0)))
    close(result.heterogeneity.cochranQ, DMat.dense(2, 2, Vector(8.0, 4.0, 800.0 / 9, 400.0 / 9)))
    assert(!result.heterogeneity.tauEstimated)
    assertEquals(result.scope, SubjectMeanScope.CommonEffectKnownGaussian)
    assertEquals(result.subjects, input.subjectKeys)
    input.rows.indices.foreach(i => close(result.subjectExpressions(i).covariance, input.rows(i).covariance))
    assert(result.input eq input)
    assertEquals(result.fit.native.subjects.size, 3)
    assert(result.prevalence.isLeft && result.input.jointInference.isLeft)

  test("Paule-Mandel heterogeneity stays distinct from observed variability with estimated uncertainty"):
    val result = right(SubjectGroupSummary.fit(cohort(estimated = true), contract, mixed))
    // Independent R uniroot solves sum((y-mu)^2/(v+tau²))=n-1.
    assertEqualsDouble(result.heterogeneity.tauSquared(0, 0), 3.0, 1e-9)
    assertEqualsDouble(result.mean(0, 0), 3.0, 1e-12)
    assertEqualsDouble(result.standardErrors(0, 0), math.sqrt(14.0 / 9), 1e-9)
    assertEqualsDouble(result.heterogeneity.iSquared(0, 0), .75, 1e-12)
    assertEqualsDouble(result.empiricalVariance(0, 0), 4.0, 1e-12)
    close(result.mean, DMat.dense(2, 2, Vector(3, 4.42264973081036, 30, 40.4448230282125)), 1e-8)
    close(result.standardErrors, DMat.dense(2, 2, Vector(1.24721912892465, 1.05030171486715, 11.6706532263821, 11.4783124257333)), 1e-8)
    val expectedTau = DMat.dense(2, 2, Vector(3, 1.732050807569, 391, 377.655309153624))
    // Native PM stops at |Q-df| <= 1e-9*df. Tau has variance units:
    // compare relatively, and check the defining Q equation independently.
    for j <- 0 until 2; k <- 0 until 2 do
      val tau = result.heterogeneity.tauSquared(j, k)
      assertEqualsDouble(tau, expectedTau(j, k), 1e-9 * math.max(1.0, expectedTau(j, k)))
      val residualQ = result.input.rows.map: row =>
        val delta = row.estimates(j, k) - result.mean(j, k)
        delta * delta / (row.covariance(2 * j + k, 2 * j + k) + tau)
      .sum
      assertEqualsDouble(residualQ, 2.0, 2e-9)
    close(result.pointwisePValues, DMat.dense(2, 2, Vector(.137956343300964, .0520351053449188, .123843244747733, .0719579834343744)), 1e-9)
    assertEquals(result.scope, SubjectMeanScope.ApproximatePopulationMean)
    assert(result.heterogeneity.tauEstimated)
    assertEquals(result.fit.native.fits.values.head.statistic, GroupStatistic.unsafeStudentT(2))
    assertEquals(result.input.originalDfRoles, Vector.fill(3)(SubjectCoordinateDfRole.Residual))
    assert(result.prevalence.isLeft)

  test("homogeneous effects reach the zero heterogeneity boundary without losing subject variation provenance"):
    val result = right(SubjectGroupSummary.fit(cohort(flat = true), contract, mixed))
    for j <- 0 until 2; k <- 0 until 2 do
      assertEqualsDouble(result.heterogeneity.tauSquared(j, k), 0.0, 1e-12)
      assertEqualsDouble(result.heterogeneity.iSquared(j, k), 0.0, 1e-12)
      assertEqualsDouble(result.empiricalVariance(j, k), 0.0, 1e-12)
    assertEquals(result.subjectExpressions.size, 3)

  test("estimated covariance cannot be promoted to common-effect known-Gaussian inference"):
    assert(SubjectGroupSummary.fit(cohort(estimated = true), contract,
      SubjectGroupCalculation.KnownVarianceGaussianFixedEffects).left.toOption.exists(_.isInstanceOf[SubjectGroupError.Unavailable]))

  test("too few subjects and summary allocation limits are explicit refusals"):
    assert(SubjectGroupSummary.fit(cohort(count = 1), contract, mixed).isLeft)
    assert(SubjectGroupSummary.fit(cohort(), contract, mixed, maximumSummaryCells = 31)
      .left.toOption.exists(_.isInstanceOf[SubjectGroupError.Budget]))
    assert(SubjectGroupSummary.fit(cohort(), contract, mixed, maximumSummaryCells = 32).isRight)

  test("summary identity binds population and numerical policy and does not change the estimand silently"):
    val input = cohort()
    val base = right(SubjectGroupSummary.fit(input, contract, mixed))
    val repeat = right(SubjectGroupSummary.fit(input, contract, mixed))
    val changed = right(SubjectGroupSummary.fit(input, contract.copy(population = "different population"), mixed))
    val fixed = right(SubjectGroupSummary.fit(input, contract, SubjectGroupCalculation.KnownVarianceGaussianFixedEffects))
    assertEquals(base.identity, repeat.identity)
    assertNotEquals(base.identity, changed.identity)
    assertNotEquals(base.identity, fixed.identity)
    assert(base.fittingScope.contains("separate subject fits"))

  test("task-linked operator summaries retain their task axis and physical value units"):
    val input = cohort(kind = SubjectComparisonKind.TaskLinkedForwardOperator)
    val result = right(SubjectGroupSummary.fit(input, contract, mixed))
    assertEquals(result.input.kind, SubjectComparisonKind.TaskLinkedForwardOperator)
    assertEquals(result.input.taskAxis, input.shared.taskAxis)
    assertEquals(result.input.coefficientUnits, "BOLD-percent/task")
    assertEqualsDouble(result.mean(0, 0), 3.0, 1e-12)

  test("a partially failed native map cannot become a complete group summary"):
    val input = cohort(overflow = true)
    val native = right(right(input.marginalModel(GroupDesign.intercept(3), input.data.subjects, mixed)).fit())
    assert(native.native.fits.values.forall(_.failures.map(_.sample) == Vector(1)))
    assert(SubjectGroupSummary.fit(input, contract, mixed).left.toOption.exists(_.isInstanceOf[SubjectGroupError.Numerical]))

  test("equal known variances match the unmerged branch's analytic oracle and metafor"):
    // docs/verification/umvpa-group-salvage-20261010/equal-variance/group-oracle.tsv
    // (closed-form PM root var(y) - v) and metafor-crosscheck.tsv p-values.
    // Measurement one has y = (-1, 1, 3) + k with v = 1; measurement two has
    // ten times the effects and nine times the variances.
    val input = cohort(offset = -1.0, variances = Vector.fill(3)(Vector(1.0, 1.0)))
    val fixed = right(SubjectGroupSummary.fit(input, contract, SubjectGroupCalculation.KnownVarianceGaussianFixedEffects))
    val pm = right(SubjectGroupSummary.fit(input, contract, mixed))
    // metafor rma(method = "FE") and rma(method = "PM", test = "adhoc") p-values.
    val fixedP = DMat.dense(2, 2, Vector(.0832645166635504, .000532005505139251, 7.76403653793065e-9, 7.64375838563106e-31))
    val pmP = Vector(.477767032132907, .225403330758517)
    for k <- 0 until 2 do
      assertEqualsDouble(fixed.mean(0, k), 1.0 + k, 1e-12)
      assertEqualsDouble(fixed.standardErrors(0, k), .577350269189626, 1e-12)
      assertEqualsDouble(fixed.heterogeneity.cochranQ(0, k), 8.0, 1e-12)
      assertEqualsDouble(fixed.heterogeneity.iSquared(0, k), .75, 1e-12)
      assertEqualsDouble(fixed.heterogeneity.tauSquared(0, k), 0.0, 0.0)
      assertEqualsDouble(fixed.empiricalVariance(0, k), 4.0, 1e-12)
      assertEqualsDouble(fixed.pointwisePValues(0, k), fixedP(0, k), 1e-12 * fixedP(0, k))
      assertEqualsDouble(pm.mean(0, k), 1.0 + k, 1e-12)
      assertEqualsDouble(pm.heterogeneity.tauSquared(0, k), 3.0, 1e-9)
      assertEqualsDouble(pm.standardErrors(0, k), 1.15470053837925, 1e-9)
      assertEqualsDouble(pm.pointwisePValues(0, k), pmP(k), 1e-9)
      // Equal variances: Q-based and tau-based I² coincide (metafor .75).
      assertEqualsDouble(pm.heterogeneity.iSquared(0, k), .75, 1e-12)
      assertEqualsDouble(fixed.mean(1, k), 10.0 * (1 + k), 1e-11)
      assertEqualsDouble(fixed.standardErrors(1, k), math.sqrt(3.0), 1e-12)
      assertEqualsDouble(fixed.heterogeneity.cochranQ(1, k), 800.0 / 9, 1e-10)
      assertEqualsDouble(fixed.heterogeneity.iSquared(1, k), .9775, 1e-12)
      assertEqualsDouble(fixed.pointwisePValues(1, k), fixedP(1, k), 1e-9 * fixedP(1, k))
      assertEqualsDouble(pm.heterogeneity.tauSquared(1, k), 391.0, 1e-9 * 391.0)
      assertEqualsDouble(pm.standardErrors(1, k), 11.5470053837925, 1e-8)
      assertEqualsDouble(pm.pointwisePValues(1, k), pmP(k), 1e-9)
    assertEquals(pm.fit.native.fits.values.head.statistic, GroupStatistic.unsafeStudentT(2))

  test("I² is the Higgins-Thompson Q statistic, not metafor's tau-based PM I²"):
    // Unequal variances (1, 4, 1): metafor rma(method = "PM") reports
    // I² = tau²/(tau² + s²) = 3/4.5, while the summary keeps (Q - df)/Q = .75
    // under every calculation. See metafor-crosscheck.tsv.
    val pm = right(SubjectGroupSummary.fit(cohort(estimated = true), contract, mixed))
    assertEqualsDouble(pm.heterogeneity.iSquared(0, 0), .75, 1e-12)
    assert(math.abs(pm.heterogeneity.iSquared(0, 0) - 2.0 / 3) > .08)
