package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import multivar.inference.CanonicalResidualBasis
import resample4s.kernel.Seed
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.execution.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class VoxelFamilyRandomizationSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed,
    Vector.tabulate(count)(i => s"$name-$i"), "voxel-family-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val plan = PlanId.derived(EstimandId("voxel-family-suite"), Vector(AxisSignature.unsafe("0" * 64)),
    AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question",
    Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)

  private final class Fixture(tiny: Boolean = false, dependent: Boolean = false, failedVoxel: Boolean = false,
      nonnullSecond: Boolean = true, allFailed: Boolean = false):
    val n = if tiny then 5 else 8; val r = if tiny then 1 else 2
    val rows = axis("confirm", n); val units = axis("confirm-units", n)
    val training = axis("discovery", 4); val trainUnits = axis("discover-units", 4)
    val neural = axis("neural", 2); val target = axis("target", r); val component = axis("component", r)
    val factors = right(PatternFactors(neural, target, component, DMat.tabulate(2, r)((i, j) => if i == j then 1.0 else 0.0),
      DMat.eye(r), GaugeEvidence.PendingNumericalCheck))
    val one = right(AxisValues(target, Vector.fill(r)(1.0)))
    val artifact = right(PatternArtifact(factors, right(TargetGeometry.continuous(target, one, one, Vector("all" -> one))),
      CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "discovery", "frozen")),
      Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainUnits, Vector(0, 1, 2, 3))), artifact,
      right(FrozenProjection(neural, component, factors.neuralByComponent, ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(r), ProjectionKind.DeclaredLinearProjection)), "support", "none", "discovery-prep"))
    val snapshot = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, Vector.range(0, n))), "confirm-prep"))
    val t1 = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    val t2 = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    val nuisanceValues = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val e1 = Vector(1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0)
    val e2 = Vector(1.0, -1.0, 1.0, -1.0, -1.0, 1.0, -1.0, 1.0)
    val nuisance = right(ConfirmationNuisance(rows, if tiny then DMat.tabulate(n, 1)((_, _) => 1.0)
      else DMat.tabulate(n, 2)((i, j) => if j == 0 then 1.0 else nuisanceValues(i))))
    val covariance = right(ResidualCovariance.fromFactors(rows, Vector.tabulate(n)(i => 1.0 + .1 * i),
      DMat.tabulate(n, 1)((i, _) => .1 * (i % 3 - 1))))
    val law = if dependent then ConfirmationErrorLaw.DependentTime(right(BoundConfirmationCovariance(rows, covariance,
      "known common row shape", TemporalCovarianceStatus.KnownGaussian))) else ConfirmationErrorLaw.IndependentGaussian
    val exposure = EvidenceExposure.internal(ExposureReference(plan, snapshot.identity, "fixture", ResultIdentity("voxel-family-result")))
    val members = Vector("voxel-0-omnibus", "voxel-1-omnibus")
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, snapshot, exposure,
      C1Contract("fixed discovery and confirmation targets", "all target coefficients zero per voxel", "held-out forward omnibus",
        members, "separable joint Gaussian known row shape", Vector("target projection"), Vector("voxel regressions")), nuisance, law))
    val y = if tiny then DMat.dense(5, 1, Vector(1.0, -2.0, 0.0, 2.0, -1.0))
      else DMat.tabulate(n, r)((i, j) => if j == 0 then t1(i) else t2(i))
    val x = if tiny then DMat.dense(5, 2, Vector(2.0, 1.0, -1.0, 3.0, 3.0, -2.0, 0.0, 2.0, 4.0, 0.0))
      else DMat.tabulate(n, 2): (i, j) =>
        if allFailed || (failedVoxel && j == 1) then Double.NaN
        else if j == 0 then 7.0 + 4.0 * nuisanceValues(i) + 2.0 * t1(i) - t2(i) + .5 * e1(i)
        else -3.0 - 2.0 * nuisanceValues(i) + (if nonnullSecond then -t1(i) + 3.0 * t2(i) else 0.0) + .25 * e2(i)
    var reads = 0
    val operator = new DoubleLinearOperator:
      val rows = n; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        reads += 1
        var i = 0
        while i < n do
          var sum = 0.0; var j = 0
          while j < 2 do
            if math.abs(input(j)) > 0.0 then sum += x(i, j) * input(j)
            j += 1
          output(i) = sum; i += 1
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = x.transposeApplyTo(input, output)
    val observations = right(Observations.fromOperator(rows, neural, operator, value("brain"), source("brain")))
    var targetReads = 0
    val targetOperator = new DoubleLinearOperator:
      val rows = n; val cols = r
      def applyTo(input: DVec, output: MutableDVec): Unit =
        targetReads += 1; y.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = y.transposeApplyTo(input, output)
    val responses = right(MultiResponse.fromOperator(rows, target, targetOperator, value("target"), source("target")))
    def reference(draws: Int = 19, budget: VoxelFamilyBudget = VoxelFamilyBudget(),
        account: Option[EvidenceExposure] = None, names: Option[Vector[String]] = None,
        replay: PatternReplay = PatternReplay.SinglePass) =
      FrozenVoxelOmnibusReference.freeze(design, names.getOrElse(members), observations.identity, responses.identity,
        account.getOrElse(exposure), "E|T,Z ~ MN(0,known-row-shape,arbitrary-voxel-shape)", Seed.fromLong(73L), draws, replay, budget = budget)
    def prepare(draws: Int = 19, budget: VoxelFamilyBudget = VoxelFamilyBudget(), replay: PatternReplay = PatternReplay.SinglePass) =
      VoxelFamilyRandomization.prepare(right(reference(draws, budget, replay = replay)), observations, responses)

  test("common Gaussian residual actions retain the independent GLS observed omnibus estimates"):
    for dependent <- Vector(false, true) do
      val f = new Fixture(dependent = dependent)
      val prepared = right(f.prepare())
      val direct = right(VoxelLoadingConfirmation.fit(f.design, f.observations, f.responses, PatternReplay.SinglePass,
        LoadingConfirmationBudget(batchVoxels = 2)))
      assertEquals(prepared.residualDegreesOfFreedom, direct.residualDegreesOfFreedom)
      prepared.localEstimates.zipWithIndex.foreach: (estimate, i) =>
        val actual = right(estimate)
        assertEqualsDouble(actual.omnibusF, direct.omnibusF(i), 1e-8)
        assertEqualsDouble(actual.residualScale, direct.residualScales(i), 1e-11)
        for j <- 0 until f.r do assertEqualsDouble(actual.coefficients(j), direct.estimates(i, j), 1e-10)
      val complete = right(right(prepared.run()).completed)
      assertEquals(complete.replicates.size, 19)
      assertEquals(complete.replicates.map(_.action.permutationDigest).distinct.size, 19)
      assert(complete.replicates.forall(value => value.fieldMembers == 2 && value.completeMembers == 2 &&
        value.failedMembers == 0 && value.firstFailure.isEmpty))
      assert(complete.admittedC1.isLeft)
      assertEquals(prepared.transformIndices, 19L * prepared.residualRows)

  test("tiny exhausted MC group maxima agree with independent scalar regression enumeration"):
    val f = new Fixture(tiny = true)
    val prepared = right(f.prepare(draws = 23)) // 4! minus identity, plus-one restores exact denominator.
    val complete = right(right(prepared.run()).completed)
    // Reproduce the declared working geometry, including the augmented
    // intercept. Equivalent nuisance spans can select different orthogonal
    // bases and therefore different finite permutation orbits.
    val working = DMat.tabulate(5, f.nuisance.matrix.cols + 1)((i, j) => if j == 0 then 1.0 else f.nuisance.matrix(i, j - 1))
    val basis = right(CanonicalResidualBasis.from(working))
    val gram = basis.matrix.t * basis.matrix; val orthogonality = basis.matrix.t * working
    for i <- 0 until 4; j <- 0 until 4 do assertEqualsDouble(gram(i, j), if i == j then 1.0 else 0.0, 1e-12)
    for i <- 0 until 4; j <- 0 until working.cols do assertEqualsDouble(orthogonality(i, j), 0.0, 1e-12)
    val x = right(basis.project(f.x)); val t = right(basis.project(f.y))
    val tt = (0 until 4).map(i => t(i, 0) * t(i, 0)).sum
    val maxima = Vector(0, 1, 2, 3).permutations.toVector.filterNot(_ == Vector(0, 1, 2, 3)).map: order =>
      val statistics = Vector.tabulate(2): voxel =>
        val slope = (0 until 4).map(i => t(i, 0) * x(order(i), voxel)).sum / tt
        val rss = (0 until 4).map(i => math.pow(x(order(i), voxel) - slope * t(i, 0), 2)).sum
        slope * slope * tt / (rss / 3.0)
      statistics.max
    val actual = complete.maxima.sorted; val expected = maxima.sorted
    actual.zip(expected).foreach((a, b) => assertEqualsDouble(a, b, 1e-9))
    assertEquals(prepared.actions.map(_.candidate).distinct.size, 23)
    assertEquals(prepared.candidateDraws, 23 + prepared.identityDraws + prepared.duplicateDraws)

  test("ordering and identical retries preserve actions, digests, denominator and bitwise maxima"):
    val prepared = right(new Fixture().prepare())
    val ordered = right(right(prepared.run()).completed)
    val reversed = right(right(prepared.run(Some(Vector.range(0, 19).reverse ++ Vector(2, 7)))).completed)
    assertEquals(reversed.identity, ordered.identity)
    assertEquals(reversed.replicates, ordered.replicates)
    assertEquals(reversed.maxima.map(java.lang.Double.doubleToRawLongBits), ordered.maxima.map(java.lang.Double.doubleToRawLongBits))

  test("omitted, cancelled and refused commits cannot report completed fixed B"):
    val prepared = right(new Fixture().prepare())
    assert(right(prepared.run(Some(Vector.range(0, 18)))).completed.isLeft)
    assert(right(prepared.run(cancellationRequested = () => true)).completed.isLeft)
    assert(prepared.run(Some(Vector(19))).isLeft)
    val refuse = new BeforeCommit:
      def check(address: WorkAddress): Either[UnitError, Unit] = Left(UnitError.BeforeCommit(s"refused ${address.replicate.value}"))
    assert(right(prepared.run(beforeCommit = refuse)).completed.isLeft)

  test("one nonfinite voxel preserves valid local coefficients and blocks the complete family"):
    val f = new Fixture(failedVoxel = true)
    val prepared = right(f.prepare(budget = VoxelFamilyBudget(batchVoxels = 1), replay = PatternReplay.Repeatable("selector-aware source capture")))
    val first = right(prepared.localEstimates.head)
    assertEqualsDouble(first.coefficients.head, 2.0, 1e-11)
    assertEqualsDouble(first.omnibusF, 40.0, 1e-9)
    assert(prepared.localEstimates(1).isLeft)
    val run = right(prepared.run())
    assert(run.completed.isLeft)
    run.execution match
      case FamilyState.Complete(value) =>
        assertEquals(value.reduction.size, 19)
        assert(value.reduction.forall(_.completeMembers == 1))
      case other => fail(s"expected operational completion with statistical failure, got $other")

  test("all failed members retain every observed outcome and only one bounded failure summary per replicate"):
    val prepared = right(new Fixture(allFailed = true).prepare())
    assertEquals(prepared.localEstimates.size, 2)
    assert(prepared.localEstimates.forall(_.isLeft))
    assertEquals(prepared.plannedFailureSummaryEntries, 19)
    val run = right(prepared.run())
    assert(run.completed.isLeft)
    run.execution match
      case FamilyState.Complete(value) =>
        assertEquals(value.reduction.size, 19)
        value.reduction.foreach: receipt =>
          assertEquals(receipt.fieldMembers, 2)
          assertEquals(receipt.completeMembers, 0)
          assertEquals(receipt.failedMembers, 2)
          assertEquals(receipt.firstFailure.map(_._1), Some(0))
          assertEquals(receipt.maximum, None)
          assert(receipt.fieldDigest.nonEmpty)
      case other => fail(s"expected bounded complete operational summaries, got $other")

  test("omnibus null-column fields ignore an alternative in a different voxel"):
    val alternative = new Fixture(nonnullSecond = true); val changed = new Fixture(nonnullSecond = false)
    val a = right(alternative.prepare()); val b = right(changed.prepare())
    val firstA = right(a.localEstimates.head); val firstB = right(b.localEstimates.head)
    assertEqualsDouble(firstA.omnibusF, firstB.omnibusF, 0.0)
    assertEquals(a.actions, b.actions)
    // This deterministic column-locality law is not an empirical strong-FWER qualification.
    assert(right(right(a.run()).completed).admittedC1.isLeft)

  test("complete family, source endpoints, law receipt and pre-exposure binding refuse before reads"):
    val f = new Fixture
    assert(f.reference(names = Some(f.members.reverse)).isLeft)
    assert(f.reference(names = Some(Vector(f.members.head))).isLeft)
    val request = ExposureRequest(ExposurePurpose.DerivedScoreView, ExposureActorRole.Analyst, ExposureScope.Holdout,
      ExposurePayload.DerivedScore, ExposureAssurance.Instrumented, 1)
    val exposed = f.exposure.append(ExposureEvent(request, ExposureOutcome.Succeeded, AdaptiveSelectionDependency.NoneObserved))
    assert(f.reference(account = Some(exposed)).isLeft)
    assert(f.reference(account = Some(EvidenceExposure.external(f.exposure.reference))).isLeft)
    assert(FrozenVoxelOmnibusReference.freeze(f.design, f.members, f.responses.identity, f.observations.identity,
      f.exposure, "joint law", Seed.fromLong(73L), 19, PatternReplay.SinglePass).isLeft)
    assert(FrozenVoxelOmnibusReference.freeze(f.design, f.members, f.observations.identity, f.responses.identity,
      f.exposure, "", Seed.fromLong(73L), 19, PatternReplay.SinglePass).isLeft)
    assert(FrozenVoxelOmnibusReference.freeze(f.design, f.members, f.observations.identity, f.responses.identity,
      f.exposure, "joint law", Seed.fromLong(73L), 19, PatternReplay.SinglePass, mode = VoxelFamilyReferenceMode.ExactGroupIncludingIdentity).isLeft)
    assertEquals(f.reads, 0)

  test("actual action seed binds the adopted work address and separates different frozen plans"):
    val f = new Fixture
    val first = right(f.reference())
    val changed = right(FrozenVoxelOmnibusReference.freeze(f.design, f.members, f.observations.identity, f.responses.identity,
      f.exposure, "a distinct frozen joint-law receipt", Seed.fromLong(73L), 19, PatternReplay.SinglePass))
    assertNotEquals(first.identity, changed.identity)
    assertNotEquals(first.actionSeed.value, changed.actionSeed.value)
    val address = WorkAddress(first.executionPlan, right(SplitCoordinate(0)), right(ReplicateCoordinate(0)),
      right(StageCoordinate(0)), scalafim.fmri.mvpa.measurement.MeasurementId.unsafe(s"voxel-family-${first.identity}"),
      right(MeasurementCoordinate(0)))
    assertEquals(first.actionSeed.value, WorkAddress.seed(first.seed, address).value)
    val prepared = right(VoxelFamilyRandomization.prepare(first, f.observations, f.responses))
    assert(prepared.actions.forall(_.actionSeed == first.actionSeed.value))
    assert(first.actionSeedLineage.contains(first.identity))

  test("aggregate storage, transform indices and fixed candidate caps refuse without brain reads"):
    val f = new Fixture(tiny = true)
    assert(f.prepare(budget = VoxelFamilyBudget(maximumOwnedCells = 0)).isLeft)
    assert(f.prepare(budget = VoxelFamilyBudget(maximumTransformIndices = 0)).isLeft)
    val capped = right(f.reference(draws = 23, budget = VoxelFamilyBudget(maximumCandidates = 23)))
    assert(VoxelFamilyRandomization.prepare(capped, f.observations, f.responses).isLeft)
    assertEquals(f.reads, 0)
    val good = right(f.reference())
    val foreign = right(Observations.fromDense(f.rows, f.neural, f.x, value("foreign-brain"), source("foreign-brain")))
    assert(VoxelFamilyRandomization.prepare(good, foreign, f.responses).isLeft)
    assertEquals(f.reads, 0)

  test("multiple capture batches and blank replay declarations refuse before either source read"):
    val f = new Fixture
    assert(f.reference(budget = VoxelFamilyBudget(batchVoxels = 1), replay = PatternReplay.SinglePass).isLeft)
    assert(f.reference(replay = PatternReplay.Repeatable(" ")).isLeft)
    assertEquals(f.reads, 0)
    assertEquals(f.targetReads, 0)

  test("full-width single capture and admitted repeatable batches retain the same observed and exhaustive null calculations"):
    val f = new Fixture(tiny = true)
    val single = right(f.prepare(draws = 23, budget = VoxelFamilyBudget(batchVoxels = 2), replay = PatternReplay.SinglePass))
    val repeated = right(f.prepare(draws = 23, budget = VoxelFamilyBudget(batchVoxels = 1),
      replay = PatternReplay.Repeatable("immutable selector-aware capture of these source identities")))
    single.localEstimates.zip(repeated.localEstimates).foreach: (left, other) =>
      val a = right(left); val b = right(other)
      assertEqualsDouble(a.omnibusF, b.omnibusF, 1e-12)
      assertEqualsDouble(a.coefficients.head, b.coefficients.head, 1e-12)
      assertEqualsDouble(a.residualScale, b.residualScale, 1e-12)
    val brainReads = f.reads; val targetReads = f.targetReads
    val a = right(right(single.run()).completed); val b = right(right(repeated.run()).completed)
    a.maxima.sorted.zip(b.maxima.sorted).foreach((left, other) => assertEqualsDouble(left, other, 1e-9))
    assertEquals(f.reads, brainReads)
    assertEquals(f.targetReads, targetReads)
    assertNotEquals(single.reference.identity, repeated.reference.identity)
