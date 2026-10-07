package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.fmri.mvpa.measurement.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class SubjectCoordinatesSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def axis(name: String, count: Int, units: String = "one") =
    right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "subject-fixture", units, "raw"))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private def close(actual: DMat, expected: DMat, tolerance: Double = 1e-10): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    for i <- 0 until actual.rows; j <- 0 until actual.cols do assertEqualsDouble(actual(i, j), expected(i, j), tolerance, s"$i,$j")
  private val plan = PlanId.derived(EstimandId("subject-coordinates-suite"), Vector(AxisSignature.unsafe("0" * 64)),
    AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question",
    Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)

  private def declaredStability(discovery: DiscoverySnapshot[?, ?], kind: SubjectStabilityKind = SubjectStabilityKind.StableAxes) =
    right(SubjectAxisStability.freeze(discovery,
      EvidenceExposure.internal(ExposureReference(plan, discovery.identity, "discovery diagnostic", ResultIdentity("axis-stability"))),
      kind, "independent discovery stability diagnostic"))

  private def exposedHoldout(exposure: EvidenceExposure): EvidenceExposure =
    val request = ExposureRequest(ExposurePurpose.RoiSelection, ExposureActorRole.Analyst, ExposureScope.Holdout,
      ExposurePayload.DerivedScore, ExposureAssurance.Declared, 1L)
    ExposureControl.read(exposure, right(ExposureControl.permit(exposure, request)), request)(Right(())) match
      case ExposureAttempt.Completed(_, updated) => updated
      case other => fail(other.toString)

  // Independent integer oracle supplied by the acceptance reviewer. Sigma=A A^T for
  // A=[[2,0,0,0],[1,2,0,0],[0,1,3,0],[1,0,1,2]]. For row-major vec(B),
  // T=M⊗R^-1 with R=[[1,1],[0,1]], M=[[1,2],[-1,1]]. Expectations below
  // are direct exact-integer matrix arithmetic, not this adapter's block helper.
  private val physicalUnits = right(SubjectCoordinateValueUnits("BOLD-percent/latent-score",
    "BOLD-percent/stimulus-amplitude", "source value-unit and original target-value convention"))
  private val originalMean = DMat.dense(2, 2, Vector(2.0, 3.0, 5.0, 7.0))
  private val originalCovariance = DMat.dense(4, 4, Vector(
    4.0, 2.0, 0.0, 2.0,
    2.0, 5.0, 2.0, 1.0,
    0.0, 2.0, 10.0, 3.0,
    2.0, 1.0, 3.0, 6.0))
  private val spatial = DMat.dense(2, 2, Vector(1.0, 2.0, -1.0, 1.0))
  private val expectedMean = DMat.dense(2, 2, Vector(-5.0, 17.0, -1.0, 4.0))
  private val expectedCovariance = DMat.dense(4, 4, Vector(
    33.0, -11.0, 18.0, -4.0,
    -11.0, 33.0, -4.0, 6.0,
    18.0, -4.0, 21.0, -8.0,
    -4.0, 6.0, -8.0, 9.0))

  private final class Fixture(q: Int = 2, localMatrix: Option[DMat] = None, sharedMatrix: Option[DMat] = None,
      commonCount: Int = 2, spatialMatrix: DMat = spatial, sharedStability: SubjectStabilityKind = SubjectStabilityKind.StableAxes):
    val rows = axis("subject-confirmation-rows", 8)
    val independent = axis("subject-confirmation-units", 8)
    val neural = axis("subject-native-voxels", 2, "mm")
    val common = axis("common-anatomical-measurements", commonCount, "mm")
    val task = axis("ordered-task-conditions", q, "task")
    val components = axis("subject-components", 2)
    val cs = localMatrix.getOrElse(DMat.tabulate(q, 2)((i, j) => if i == j then 1.0 else 0.0))
    val cg = sharedMatrix.getOrElse(DMat.tabulate(q, 2)((i, j) => if i == j then 1.0 else if i == 0 && j == 1 then 1.0 else 0.0))

    def discovery(prefix: String, projection: DMat, input: AxisRef[String] = task, units: Option[AxisRef[String]] = None): DiscoverySnapshot[?, ?] =
      val training = axis(s"$prefix-discovery-rows", 4)
      val trainingUnits = units.getOrElse(axis(s"$prefix-discovery-units", 4))
      val factors = right(PatternFactors(neural, input, components, DMat.eye(2), projection, GaugeEvidence.PendingNumericalCheck))
      val unit = right(AxisValues(input, Vector.fill(input.size)(1.0)))
      val artifact = right(PatternArtifact(factors, right(TargetGeometry.continuous(input, unit, unit, Vector("all" -> unit))),
        CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
        ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, s"$prefix-training", "frozen")),
        Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
      right(DiscoverySnapshot(right(ConfirmationUnits(training, trainingUnits, Vector.range(0, 4))), artifact,
        right(FrozenProjection(neural, components, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)),
        right(FrozenProjection(input, components, projection, ProjectionKind.DeclaredLinearProjection)),
        "spatial-support", "discovery-selection", "discovery-preparation"))

    val localDiscovery = discovery("local", cs)
    val globalDiscovery = discovery("global", cg)
    def exposure(d: DiscoverySnapshot[?, ?]) = EvidenceExposure.internal(ExposureReference(plan, d.identity, "fixture", ResultIdentity("shared-task-fit")))
    val shared = right(SharedTaskCoordinates.freeze(globalDiscovery, exposure(globalDiscovery), common, declaredStability(globalDiscovery, sharedStability), physicalUnits))
    val confirmation = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, independent, Vector.range(0, 8))), "confirmation-preparation"))
    val nuisance = right(ConfirmationNuisance(rows, DMat.tabulate(8, 1)((_, _) => 1.0)))
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, localDiscovery, confirmation,
      EvidenceExposure.internal(ExposureReference(plan, confirmation.identity, "fixture", ResultIdentity("subject-fit"))),
      C1Contract("fixed discovery", "forward coefficients", "declared confirmation units", Vector("task-coefficients"), "explicit covariance source",
        Vector("task coordinates"), Vector("coefficient estimation")), nuisance, ConfirmationErrorLaw.IndependentGaussian))
    val subjects = right(Column.fromValues(rows, Vector.fill(8)(right(SubjectCoordinateKey("subject-A"))), value("row-subject-labels")))
    var brainReads = 0
    private val brainData = DMat.zeros(8, 2)
    private val brainOperator = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        brainReads += 1
        brainData.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
        brainReads += 1
        brainData.transposeApplyTo(input, output)
    val brain = right(Observations.fromOperator(rows, neural, brainOperator, value("brain-values"), source("brain")))
    val target = right(MultiResponse.fromDense(rows, task, DMat.zeros(8, q), value("target-values"), source("target")))
    val leg = right(MeasurementLeg.basisMap(neural, common, MeasurementId.unsafe("native-to-common"), spatialMatrix,
      MetricCapability.DeclaredMetricRequired("explicit linear anatomical measurement, not an isometry"),
      MaterializationCost.LinearOperatorApplications(1, "fixture map")))
    def spatialExposure(map: MeasurementDescriptor) = EvidenceExposure.internal(ExposureReference(plan, localDiscovery.identity, "discovery spatial selection", ResultIdentity(map.semanticId)))
    def selected(sharedCoordinates: SharedTaskCoordinates = shared) =
      right(SubjectSpatialAlignment.freeze(localDiscovery, sharedCoordinates, leg, spatialExposure(leg.descriptor)))
    def estimate(mean: DMat = originalMean, covariance: DMat = originalCovariance,
        df: SubjectCoordinateDf = SubjectCoordinateDf.Known(6.0, "Gaussian residual df"),
        stability: SubjectStabilityKind = SubjectStabilityKind.StableAxes) =
      SubjectCoefficientEstimate.bind(subjects, neural, design, mean, covariance, brain.identity, target.identity,
        value("coefficients"), value("joint-covariance"), df, "joint component and feature covariance provider", declaredStability(localDiscovery, stability), physicalUnits)
    def transport(estimate: SubjectCoefficientEstimate, kind: SubjectComparisonKind = SubjectComparisonKind.SharedComponentCoefficients,
        policy: SubjectCoordinatePolicy = SubjectCoordinatePolicy()) = SubjectCoordinates.transport(estimate, shared, selected(), kind, policy)

  test("oblique subject coefficients and full correlated covariance match the exact independent oracle"):
    val f = new Fixture
    val input = right(f.estimate())
    val result = right(f.transport(input))
    close(result.estimates, expectedMean)
    close(result.covariance, expectedCovariance)
    close(result.taskCoefficientTransform, DMat.dense(2, 2, Vector(1.0, 0.0, -1.0, 1.0)))
    assertEquals(result.kind, SubjectComparisonKind.SharedComponentCoefficients)
    assertEquals(result.subject.value, "subject-A")
    assertEquals(result.featureAxis, f.common.descriptor)
    assertEquals(result.taskAxis, f.shared.componentAxis)
    assertEquals(result.brainEvidence, f.brain.identity)
    assertEquals(result.targetEvidence, f.target.identity)
    assertEquals(result.source.subjectColumn, f.subjects.identity)
    assertEquals(result.source.covarianceSource, value("joint-covariance"))
    assertEquals(result.degreesOfFreedom, SubjectCoordinateDf.Known(6.0, "Gaussian residual df"))
    assertEquals(result.coefficientUnits, "BOLD-percent/latent-score")
    assertEquals(result.covarianceUnits, "(BOLD-percent/latent-score)^2")
    assertEquals(result.featureAxis.units, "mm") // coordinate geometry must not become physical beta units
    assertEquals(result.covarianceOrdering, "feature-major/task-coordinate-minor")
    assert(result.relativeProjectionResidual.exists(_ <= 1e-10))

  test("dropping cross-feature or cross-component covariance changes the transported uncertainty"):
    val f = new Fixture
    val diagonal = DMat.tabulate(4, 4)((i, j) => if i == j then originalCovariance(i, j) else 0.0)
    val blocks = DMat.tabulate(4, 4)((i, j) => if i / 2 == j / 2 then originalCovariance(i, j) else 0.0)
    for covariance <- Vector(diagonal, blocks) do
      val out = right(f.transport(right(f.estimate(covariance = covariance))))
      assert(math.abs(out.covariance(0, 0) - expectedCovariance(0, 0)) > 1.0)

  test("signed rotation and component reindexing preserve the explicit coefficient orientation"):
    val f = new Fixture(sharedMatrix = Some(DMat.dense(2, 2, Vector(0.0, -1.0, 1.0, 0.0))))
    val out = right(f.transport(right(f.estimate())))
    close(out.estimates, DMat.dense(2, 2, Vector(17.0, -12.0, 4.0, -3.0)))
    // Independent permutation/sign change applied to the spatial-only covariance.
    close(out.covariance, DMat.dense(4, 4, Vector(
      33.0, -22.0, 6.0, -2.0,
      -22.0, 44.0, -2.0, 16.0,
      6.0, -2.0, 9.0, -1.0,
      -2.0, 16.0, -1.0, 14.0)))

  test("stable subspaces return a named task operator with singular covariance and no forced component labels"):
    val f = new Fixture(q = 3)
    val input = right(f.estimate(stability = SubjectStabilityKind.StableSubspace))
    assert(f.transport(input).left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.Unavailable]))
    val out = right(f.transport(input, SubjectComparisonKind.TaskLinkedForwardOperator))
    assertEquals(out.kind, SubjectComparisonKind.TaskLinkedForwardOperator)
    assertEquals(out.taskAxis, f.task.descriptor)
    assertEquals(out.coordinateCondition, None)
    close(out.estimates, DMat.dense(2, 3, Vector(12.0, 17.0, 0.0, 3.0, 4.0, 0.0)))
    assertEqualsDouble(out.covariance(2, 2), 0.0, 0.0)
    assertEqualsDouble(out.covariance(5, 5), 0.0, 0.0)
    assertEquals(out.coefficientUnits, "BOLD-percent/stimulus-amplitude")

  test("task operator and its covariance are invariant to an oblique discovery gauge"):
    val base = new Fixture
    val sheared = new Fixture(localMatrix = Some(DMat.dense(2, 2, Vector(1.0, 1.0, 0.0, 1.0))))
    // Exact row-wise change Bs→Bs R^-T, Sigma→(I⊗R^-1) Sigma (I⊗R^-1)^T.
    val changedMean = DMat.dense(2, 2, Vector(-1.0, 3.0, -2.0, 7.0))
    val changedCovariance = DMat.dense(4, 4, Vector(
      5.0, -3.0, -3.0, 1.0,
      -3.0, 5.0, 1.0, 1.0,
      -3.0, 1.0, 10.0, -3.0,
      1.0, 1.0, -3.0, 6.0))
    val a = right(base.transport(right(base.estimate()), SubjectComparisonKind.TaskLinkedForwardOperator))
    val b = right(sheared.transport(right(sheared.estimate(changedMean, changedCovariance)), SubjectComparisonKind.TaskLinkedForwardOperator))
    close(b.estimates, a.estimates)
    close(b.covariance, a.covariance)

  test("unequal anatomical dimensions preserve every covariance cell without requiring positive definiteness"):
    val f = new Fixture(commonCount = 3, spatialMatrix = DMat.dense(3, 2, Vector(1.0, 0.0, 0.0, 1.0, .5, .5)))
    val out = right(f.transport(right(f.estimate())))
    close(out.estimates, DMat.dense(3, 2, Vector(-1.0, 3.0, -2.0, 7.0, -1.5, 5.0)))
    close(out.covariance, DMat.dense(6, 6, Vector(
      5.0, -3.0, -3.0, 1.0, 1.0, -1.0,
      -3.0, 5.0, 1.0, 1.0, -1.0, 3.0,
      -3.0, 1.0, 10.0, -3.0, 3.5, -1.0,
      1.0, 1.0, -3.0, 6.0, -1.0, 3.5,
      1.0, -1.0, 3.5, -1.0, 2.25, -1.0,
      -1.0, 3.0, -1.0, 3.5, -1.0, 3.25)))

  test("shared unstable axes cannot license component matches but retain task-operator comparison"):
    val f = new Fixture(sharedStability = SubjectStabilityKind.StableSubspace)
    val estimate = right(f.estimate())
    assert(f.transport(estimate).left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.Unavailable]))
    val operator = right(f.transport(estimate, SubjectComparisonKind.TaskLinkedForwardOperator))
    assertEquals(operator.taskAxis, f.task.descriptor)
    close(operator.estimates, DMat.dense(2, 2, Vector(12.0, 17.0, 3.0, 4.0)))

  test("all df classifications and source identities survive arithmetic transport without group admission"):
    val f = new Fixture
    for df <- Vector(SubjectCoordinateDf.Known(6, "known"), SubjectCoordinateDf.Estimated(5.5, "Satterthwaite"),
        SubjectCoordinateDf.Approximate(5, "approximation"), SubjectCoordinateDf.Unknown("not supplied"), SubjectCoordinateDf.NotApplicable("operator arithmetic")) do
      val input = right(f.estimate(df = df))
      val out = right(f.transport(input))
      assertEquals(out.degreesOfFreedom, df)
      assert(out.source eq input)

  test("subject differences remain visible under the same admitted coordinates"):
    val f = new Fixture
    val changed = DMat.dense(2, 2, Vector(3.0, 3.0, 5.0, 7.0))
    val out = right(f.transport(right(f.estimate(mean = changed))))
    assertEqualsDouble(out.estimates(0, 0), -4.0, 1e-10)
    assertEqualsDouble(out.estimates(1, 0), -2.0, 1e-10)

  test("dimension-only, reordered task and wrong destination bindings refuse"):
    val f = new Fixture
    val input = right(f.estimate())
    val foreign = axis("foreign-anatomical-measurements", 2, "mm")
    val wrong = right(MeasurementLeg.basisMap(f.neural, foreign, MeasurementId.unsafe("wrong-map"), spatial,
      MetricCapability.DeclaredMetricRequired("not an isometry"), MaterializationCost.None))
    assert(SubjectSpatialAlignment.freeze(f.localDiscovery, f.shared, wrong, f.spatialExposure(wrong.descriptor)).isLeft)
    val record = f.task.toRecord
    val reordered = right(AxisRef.fromStableKeys(record.namespace, record.role, record.stableKeys.reverse, record.basis, record.units, record.scale, record.lineage))
    val discovery = f.discovery("wrong-order", f.cg, reordered)
    val shared = right(SharedTaskCoordinates.freeze(discovery, f.exposure(discovery), f.common, declaredStability(discovery), physicalUnits))
    assert(SubjectCoordinates.transport(input, shared, f.selected(shared), SubjectComparisonKind.SharedComponentCoefficients).isLeft)

  test("confirmation-derived or overlapping shared coordinate selection refuses"):
    val f = new Fixture
    val external = EvidenceExposure.external(ExposureReference(plan, f.globalDiscovery.identity, "external", ResultIdentity("external-fit")))
    assert(SharedTaskCoordinates.freeze(f.globalDiscovery, external, f.common, declaredStability(f.globalDiscovery), physicalUnits).isLeft)
    assert(SharedTaskCoordinates.freeze(f.globalDiscovery, f.exposure(f.localDiscovery), f.common, declaredStability(f.globalDiscovery), physicalUnits).isLeft)
    val overlapDiscovery = f.discovery("overlapping", f.cg, units = Some(f.independent))
    val overlap = right(SharedTaskCoordinates.freeze(overlapDiscovery, f.exposure(overlapDiscovery), f.common, declaredStability(overlapDiscovery), physicalUnits))
    assert(SubjectCoordinates.transport(right(f.estimate()), overlap, f.selected(overlap), SubjectComparisonKind.SharedComponentCoefficients)
      .left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.Independence]))

  test("spatial alignment and stability cannot be selected from confirmation or borrowed discovery accounts"):
    val f = new Fixture
    val account = f.spatialExposure(f.leg.descriptor)
    assert(SubjectSpatialAlignment.freeze(f.localDiscovery, f.shared, f.leg, exposedHoldout(account))
      .left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.Independence]))
    val wrongAccount = EvidenceExposure.internal(ExposureReference(plan, f.globalDiscovery.identity, "wrong discovery", ResultIdentity(f.leg.descriptor.semanticId)))
    assert(SubjectSpatialAlignment.freeze(f.localDiscovery, f.shared, f.leg, wrongAccount).isLeft)
    assert(SubjectSpatialAlignment.freeze(f.localDiscovery, f.shared, f.leg, f.exposure(f.localDiscovery)).isLeft)
    assert(SubjectAxisStability.freeze(f.localDiscovery, exposedHoldout(f.exposure(f.localDiscovery)), SubjectStabilityKind.StableAxes, "selected using confirmation").isLeft)
    assert(SubjectCoefficientEstimate.bind(f.subjects, f.neural, f.design, originalMean, originalCovariance, f.brain.identity, f.target.identity,
      value("coefficients"), value("covariance"), SubjectCoordinateDf.Known(6, "df"), "joint", declaredStability(f.globalDiscovery), physicalUnits).isLeft)

  test("physical value units must match independently of anatomical coordinate units"):
    val f = new Fixture
    val different = right(SubjectCoordinateValueUnits("microtesla/latent-score", physicalUnits.taskOperator, "different declared physical units"))
    val shared = right(SharedTaskCoordinates.freeze(f.globalDiscovery, f.exposure(f.globalDiscovery), f.common,
      declaredStability(f.globalDiscovery), different))
    assert(SubjectCoordinates.transport(right(f.estimate()), shared, f.selected(shared), SubjectComparisonKind.SharedComponentCoefficients)
      .left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.AxisMismatch]))

  test("covariance units are mechanically squared and cannot be replaced by unrelated physical labels"):
    val units = right(SubjectCoordinateValueUnits("BOLD-percent/score", "BOLD-percent/stimulus", "value-unit contract"))
    assertEquals(units.coefficientCovariance, "(BOLD-percent/score)^2")
    assertEquals(units.taskOperatorCovariance, "(BOLD-percent/stimulus)^2")
    assert(SubjectCoordinateValueUnits("", "task", "receipt").isLeft)
    assert(compileErrors("""scalafim.fmri.mvpa.pattern.SubjectCoordinateValueUnits("BOLD-percent/score", "mm", "BOLD-percent/stimulus", "mm", "receipt")""").nonEmpty)

  test("subject rows cannot silently relabel a multisubject confirmation fit"):
    val f = new Fixture
    val mixed = right(Column.fromValues(f.rows, Vector.tabulate(8)(i => right(SubjectCoordinateKey(if i < 4 then "A" else "B"))), value("mixed-subjects")))
    assert(SubjectCoefficientEstimate.bind(mixed, f.neural, f.design, originalMean, originalCovariance, f.brain.identity, f.target.identity,
      value("coefficients"), value("covariance"), SubjectCoordinateDf.Known(6, "df"), "joint", declaredStability(f.localDiscovery), physicalUnits).isLeft)
    val foreignRows = axis("foreign-confirmation-rows", 8)
    val labels = right(Column.fromValues(foreignRows, Vector.fill(8)(right(SubjectCoordinateKey("A"))), value("foreign-labels")))
    assert(SubjectCoefficientEstimate.bind(labels, f.neural, f.design, originalMean, originalCovariance, f.brain.identity, f.target.identity,
      value("coefficients"), value("covariance"), SubjectCoordinateDf.Known(6, "df"), "joint", declaredStability(f.localDiscovery), physicalUnits).isLeft)

  test("ill-conditioned, different task subspaces, invalid covariance and budgets fail closed"):
    val bad = new Fixture(sharedMatrix = Some(DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 1e-12))))
    assert(bad.transport(right(bad.estimate())).left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.Condition]))
    val mismatch = new Fixture(q = 3, sharedMatrix = Some(DMat.dense(3, 2, Vector(1.0, 0.0, 0.0, 1.0, 0.0, .5))))
    assert(mismatch.transport(right(mismatch.estimate())).isLeft)
    val f = new Fixture
    assert(f.estimate(covariance = DMat.eye(2)).isLeft) // marginal-only covariance cannot masquerade as full joint
    assert(f.estimate(covariance = DMat.dense(4, 4, Vector.tabulate(16)(i => if i == 0 then -1.0 else if i / 4 == i % 4 then 1.0 else 0.0))).isLeft)
    assert(f.estimate(mean = DMat.dense(2, 2, Vector(Double.NaN, 0.0, 0.0, 0.0))).isLeft)
    assert(f.transport(right(f.estimate()), policy = SubjectCoordinatePolicy(maximumOwnedCells = 1)).isLeft)
    assert(f.transport(right(f.estimate()), policy = SubjectCoordinatePolicy(maximumScalarProducts = 1)).isLeft)
    assert(f.transport(right(f.estimate(stability = SubjectStabilityKind.UnstableSolution)), SubjectComparisonKind.TaskLinkedForwardOperator).isLeft)

  test("budget and metadata refusals never apply the original confirmation evidence source"):
    val f = new Fixture
    val input = right(f.estimate())
    assertEquals(f.brainReads, 0)
    assert(f.transport(input, policy = SubjectCoordinatePolicy(maximumOwnedCells = 0)).isLeft)
    assert(f.transport(input, policy = SubjectCoordinatePolicy(maximumScalarProducts = 0)).isLeft)
    val wrongUnits = right(SubjectCoordinateValueUnits(physicalUnits.coefficients, "other-value/target", "different original target-value denominator"))
    val shared = right(SharedTaskCoordinates.freeze(f.globalDiscovery, f.exposure(f.globalDiscovery), f.common,
      declaredStability(f.globalDiscovery), wrongUnits))
    assert(SubjectCoordinates.transport(input, shared, f.selected(shared), SubjectComparisonKind.TaskLinkedForwardOperator,
      SubjectCoordinatePolicy(maximumOwnedCells = 0)).left.toOption.exists(_.isInstanceOf[SubjectCoordinateError.AxisMismatch]))
    assertEquals(f.brainReads, 0)
    right(f.brain.patterns(DMat.eye(2))) // positive control: the spy observes actual applications
    assert(f.brainReads > 0)
