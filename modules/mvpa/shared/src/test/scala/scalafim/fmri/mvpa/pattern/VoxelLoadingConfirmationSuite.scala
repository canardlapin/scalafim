package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class VoxelLoadingConfirmationSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "loading-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val plan = PlanId.derived(EstimandId("loading-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)

  private final class Fixture(alias: Boolean = false, dependent: Boolean = false):
    val rows = axis("confirm-rows", 8); val units = axis("confirm-units", 8)
    val training = axis("discovery-rows", 4); val trainUnits = axis("discover-units", 4)
    val neural = axis("neural", 2); val target = axis("target", 2); val component = axis("component", 2)
    val a = right(PatternFactors(neural, target, component, DMat.eye(2), DMat.eye(2), GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector(1.0, 1.0)))
    val artifact = right(PatternArtifact(a, right(TargetGeometry.continuous(target, unit, unit, Vector("all" -> unit))),
      CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "loading-discovery", "frozen")), Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainUnits, Vector(0, 1, 2, 3))), artifact,
      right(FrozenProjection(neural, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)), "support", "none", "discovery-prep"))
    val snapshot = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, Vector.range(0, 8))), "confirm-prep"))
    val t1 = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    val t2 = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    val nuisanceValues = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val e1 = Vector(1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0)
    val e2 = Vector(1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0)
    val nuisance = right(ConfirmationNuisance(rows, DMat.tabulate(8, 2)((i, j) => if j == 0 then 1.0 else nuisanceValues(i))))
    val covariance = right(ResidualCovariance.fromFactors(rows, Vector.tabulate(8)(i => 1.0 + .1 * i), DMat.dense(8, 1, Vector(.2, -.1, .3, -.2, .1, -.3, .2, .4))))
    val law = if dependent then ConfirmationErrorLaw.DependentTime(right(BoundConfirmationCovariance(rows, covariance, "known-row-shape", TemporalCovarianceStatus.KnownGaussian))) else ConfirmationErrorLaw.IndependentGaussian
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, snapshot,
      EvidenceExposure.internal(ExposureReference(plan, snapshot.identity, "fixture", ResultIdentity("loading-result"))),
      C1Contract("fixed discovery", "target-derived loading zero", "these held-out units", Vector("two-components-by-two-voxels"), "Gaussian row shape with voxel scale", Vector("target projection"), Vector("nuisance", "unpenalized regression")), nuisance, law))
    val y = DMat.tabulate(8, 2)((i, j) => if j == 0 then t1(i) else if alias then nuisanceValues(i) else t2(i))
    val x = DMat.tabulate(8, 2): (i, j) =>
      if j == 0 then 7.0 + 4.0 * nuisanceValues(i) + 2.0 * t1(i) - t2(i) + .5 * e1(i)
      else -3.0 - 2.0 * nuisanceValues(i) - t1(i) + 3.0 * t2(i) + .25 * e2(i)
    var reads = 0
    val operator = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        x.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = x.transposeApplyTo(input, output)
    val observations = right(Observations.fromOperator(rows, neural, operator, value("brain"), source("brain")))
    val responses = right(MultiResponse.fromDense(rows, target, y, value("target"), source("target")))
    def fit(batch: Int = 1, maximum: Long = 1000000L, replay: PatternReplay = PatternReplay.Repeatable("fixture")) =
      VoxelLoadingConfirmation.fit(design, observations, responses, replay, LoadingConfirmationBudget(batch, maximum))

  test("target-derived batched loadings match an independent orthogonal nuisance t/F oracle"):
    val f = new Fixture
    val result = right(f.fit())
    assertEquals(result.calibrationStatus, LoadingCalibrationStatus.PendingFrozenProtocol)
    assertEquals(result.residualDegreesOfFreedom, 4)
    assertEquals(result.componentDegreesOfFreedom, 2)
    assertEquals(result.batchReads, 2)
    assertEquals(f.reads, 2)
    val beta = Vector(Vector(2.0, -1.0), Vector(-1.0, 3.0))
    val se = Vector(.25, .125) // RSS=[2,.5], df=4, each target sum squares=8.
    for i <- 0 until 2; j <- 0 until 2 do
      assertEqualsDouble(result.estimates(i, j), beta(i)(j), 1e-12)
      assertEqualsDouble(result.standardErrors(i, j), se(i), 1e-12)
      assertEqualsDouble(result.tStatistics(i, j), beta(i)(j) / se(i), 1e-11)
    assertEqualsDouble(result.omnibusF(0), 40.0, 1e-10)
    assertEqualsDouble(result.omnibusF(1), 320.0, 1e-9)
    // R qt(.975,4), independently generated; no automatic coverage claim.
    val critical = 2.7764451051977987
    val intervals = right(result.intervalsAt(critical, 8))
    assertEqualsDouble(intervals.lower(0, 0), 1.3058887237005503, 1e-12)
    assertEqualsDouble(intervals.upper(1, 1), 3.347055638149725, 1e-12)
    assert(result.intervalsAt(critical, 7).isLeft)
    assert(result.intervalsAt(Double.PositiveInfinity, 8).isLeft)
    val together = right(f.fit(batch = 2, replay = PatternReplay.SinglePass))
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(together.estimates(i, j), result.estimates(i, j), 1e-12)

  test("aliased target dimensions and missing values refuse before any brain reads"):
    val f = new Fixture(alias = true)
    assert(f.fit().left.toOption.exists(_.isInstanceOf[LoadingConfirmationError.NonEstimable]))
    assertEquals(f.reads, 0)
    val good = new Fixture
    val missing = right(MultiResponse.fromDense(good.rows, good.target, DMat.dense(8, 2, Vector.fill(16)(Double.NaN)), value("missing"), source("missing")))
    assert(VoxelLoadingConfirmation.fit(good.design, good.observations, missing, PatternReplay.Repeatable("fixture")).isLeft)
    assertEquals(good.reads, 0)

  test("foreign actual rows, target endpoint, budgets and replay refuse before brain reads"):
    val f = new Fixture
    assert(f.fit(maximum = 0).isLeft)
    assert(f.fit(replay = PatternReplay.SinglePass).isLeft)
    assert(f.fit(replay = PatternReplay.Repeatable("")).isLeft)
    val foreignRows = axis("foreign-confirmation", 8)
    val foreign = right(MultiResponse.fromDense(foreignRows, f.target, f.y, value("foreign"), source("foreign")))
    val foreignBrain = right(Observations.fromDense(foreignRows, f.neural, f.x, value("foreign-brain"), source("foreign-brain")))
    assert(VoxelLoadingConfirmation.fit(f.design, foreignBrain, foreign, PatternReplay.SinglePass).isLeft)
    assertEquals(f.reads, 0)

  test("structured row whitening is an isometry against independent diagonal-plus-rank-one precision"):
    val f = new Fixture(dependent = true)
    val v = DMat.tabulate(8, 3)((i, j) => (i - j).toDouble / 4.0)
    val w = right(f.covariance.whiten(v))
    val d = f.covariance.diagonalValues; val u = f.covariance.loadingsMatrix
    val denominator = 1.0 + (0 until 8).map(i => u(i, 0) * u(i, 0) / d(i)).sum
    for a <- 0 until 3; b <- 0 until 3 do
      val independent = (0 until 8).map(i => v(i, a) * v(i, b) / d(i)).sum -
        (0 until 8).map(i => v(i, a) * u(i, 0) / d(i)).sum * (0 until 8).map(i => v(i, b) * u(i, 0) / d(i)).sum / denominator
      assertEqualsDouble((w.t * w)(a, b), independent, 1e-12)

  test("known dependent row shape retains exact units and matches independent R GLS fixture"):
    val f = new Fixture(dependent = true)
    val result = right(f.fit())
    // Independent R solve(crossprod(D, solve(V,D))) and residual quadratic.
    val beta = Vector(2.0409555566340161, -0.90468934939161416, -0.98928414474021209, 3.0076631374039833)
    val se = Vector(.24337185381938867, .24566751830688088, .12347726684378776, .12464199633923817)
    for i <- 0 until 2; j <- 0 until 2 do
      assertEqualsDouble(result.estimates(i, j), beta(2 * i + j), 1e-10)
      assertEqualsDouble(result.standardErrors(i, j), se(2 * i + j), 1e-10)
    assertEqualsDouble(result.omnibusF(0), 41.810054454945032, 1e-9)
    assertEqualsDouble(result.omnibusF(1), 322.39283937304106, 1e-8)
    assertEquals(result.confirmationRows, f.rows.descriptor)
    assertEquals(result.independentUnits, f.units.descriptor)
    assertEquals(result.rowUnitOrdinals, Vector.range(0, 8))
    assertEquals(result.errorLaw, f.law)

  test("a frozen nonorthogonal target projection changes component loading coordinates and preserves the omnibus hypothesis"):
    val f = new Fixture
    val changedProjection = right(FrozenProjection(f.target, f.component, DMat.dense(2, 2, Vector(2.0, .5, 0.0, 1.0)), ProjectionKind.DeclaredLinearProjection))
    val changedDiscovery = right(DiscoverySnapshot(f.discovery.samples, f.artifact, f.discovery.brainProjection, changedProjection, "support", "none", "discovery-prep"))
    val changed = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, changedDiscovery, f.snapshot,
      EvidenceExposure.internal(ExposureReference(plan, f.snapshot.identity, "fixture", ResultIdentity("rotated-loading"))),
      f.design.contract, f.nuisance, f.law))
    val result = right(VoxelLoadingConfirmation.fit(changed, f.observations, f.responses, PatternReplay.SinglePass))
    val expected = Vector(1.25, -1.0, -1.25, 3.0)
    for i <- 0 until 2; j <- 0 until 2 do assertEqualsDouble(result.estimates(i, j), expected(2 * i + j), 1e-12)
    assertEqualsDouble(result.standardErrors(0, 0), .25 * math.sqrt(.3125), 1e-12)
    assertEqualsDouble(result.omnibusF(0), 40.0, 1e-10)
    assertEqualsDouble(result.omnibusF(1), 320.0, 1e-9)
    assertNotEquals(result.designIdentity, f.design.identity)

  test("preflight budget refuses a poison target application and a foreign target endpoint"):
    val f = new Fixture
    var targetReads = 0
    val poison = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        targetReads += 1
        throw IllegalStateException("budget preflight read targets")
    val response = right(MultiResponse.fromOperator(f.rows, f.target, poison, value("poison-target"), source("poison-target")))
    assert(VoxelLoadingConfirmation.fit(f.design, f.observations, response, PatternReplay.SinglePass, LoadingConfirmationBudget(2, 0)).isLeft)
    assertEquals(targetReads, 0)
    val wrong = right(MultiResponse.fromDense(f.rows, axis("wrong-target", 2), f.y, value("wrong-target"), source("wrong-target")))
    assert(VoxelLoadingConfirmation.fit(f.design, f.observations, wrong, PatternReplay.SinglePass).isLeft)
    assertEquals(f.reads, 0)
