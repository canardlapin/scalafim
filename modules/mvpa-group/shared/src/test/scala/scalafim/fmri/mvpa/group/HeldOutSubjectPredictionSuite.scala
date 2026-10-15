package scalafim.fmri.mvpa.analysis

import gale.backend.Backend.given
import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}
import multivar.core.{SpaceRole, ValueId, ValueIdentity}
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import scalafim.fmri.mvpa.group.*
import scalafim.response.{Provenance, ProvenanceId, SourceId}

class HeldOutSubjectPredictionSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, count: Int) = right(AxisRef.fromStableKeys(name, SpaceRole.Observed, Vector.tabulate(count)(i => s"$name-$i"), "component-fixture", "one", "raw"))
  private def value(name: String) = ValueIdentity.source(ValueId.unsafe(name))
  private def source(name: String) =
    val id = SourceId.unsafe(name)
    right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe(s"$name-root"), id)))
  private val planId = PlanId.derived(EstimandId("component-suite"), Vector(AxisSignature.unsafe("0" * 64)), AxisSignature.unsafe("1" * 64), AxisSignature.unsafe("2" * 64), "evidence", "design", "frame", "question", Vector.empty, Vector.empty, "reduction", Vector.empty, Set.empty)
  private def exposure(identity: String) = EvidenceExposure.internal(ExposureReference(planId, identity, "fixture", ResultIdentity("component-result")))

  private final class Fixture(mapping: Vector[Int] = Vector.range(0, 8), unitCount: Int = 8, includeIntercept: Boolean = true, trainingName: String = "training", assessmentName: String = "confirmation"):
    val rows = axis(assessmentName, 8); val training = axis(trainingName, 8)
    val units = axis(s"$assessmentName-units", unitCount); val trainUnits = axis(s"$trainingName-units", 8)
    val neural = axis("neural", 2); val target = axis("target", 2); val component = axis("component", 2)
    val u = Vector(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, -1.0)
    val v = Vector(1.0, 1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0)
    val e = Vector(1.0, -1.0, 1.0, -1.0, 1.0, -1.0, 1.0, -1.0)
    val f = Vector(1.0, 1.0, -1.0, -1.0, -1.0, -1.0, 1.0, 1.0)
    val drift = Vector(1.0, -1.0, -1.0, 1.0, 1.0, -1.0, -1.0, 1.0)
    val x = DMat.tabulate(8, 2)((i, j) => if j == 0 then u(i) else .5 * u(i) + math.sqrt(.75) * v(i))
    val yTrain = DMat.tabulate(8, 2)((i, j) => if j == 0 then 7.0 + 2.0 * x(i, 0) + 3.0 * x(i, 1) + 4.0 * drift(i)
      else -3.0 - x(i, 0) + 2.0 * x(i, 1) - 2.0 * drift(i))
    val y = DMat.tabulate(8, 2)((i, j) => yTrain(i, j) + (if j == 0 then .5 * e(i) else .25 * f(i)))
    val a = right(PatternFactors(neural, target, component, DMat.eye(2), DMat.eye(2), GaugeEvidence.PendingNumericalCheck))
    val unit = right(AxisValues(target, Vector(1.0, 1.0))); val metric = right(AxisValues(target, Vector(1.0, 2.0)))
    val artifact = right(PatternArtifact(a, right(TargetGeometry.continuous(target, unit, metric, Vector("all" -> unit))),
      CenteringPolicy.CenteredBeforeFit("discovery-x", "discovery-y"), DegenerateTargetPolicy.Refuse,
      ResidualCovarianceCapability.NotFitted, right(TrainingBinding(training.descriptor, "component-discovery", "frozen")), Vector("discovery-only"), right(PatternFitDiagnostics(Vector(0.0), "fixture", Vector.empty))))
    val discovery = right(DiscoverySnapshot(right(ConfirmationUnits(training, trainUnits, Vector.range(0, 8))), artifact,
      right(FrozenProjection(neural, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)),
      right(FrozenProjection(target, component, DMat.eye(2), ProjectionKind.DeclaredLinearProjection)), "support", "none", "discovery-prep"))
    val snapshot = right(ConfirmationSnapshot(right(ConfirmationUnits(rows, units, mapping)), "confirmation-prep"))
    val nuisanceMatrix = if includeIntercept then DMat.tabulate(8, 2)((i, j) => if j == 0 then 1.0 else drift(i)) else DMat.tabulate(8, 1)((i, _) => drift(i))
    val nuisance = right(ConfirmationNuisance(rows, nuisanceMatrix))
    val trainingNuisance = right(ConfirmationNuisance(training, nuisanceMatrix))
    val contract = C1Contract("fixed discovery", "paired association and incremental usefulness", "these independent units",
      Vector("association-0", "association-1", "incremental-0", "incremental-1"), "pending calibration", Vector("projections", "metric", "family"), Vector("association", "held-out loss"))
    val law = if mapping.distinct.size == 8 then ConfirmationErrorLaw.IndependentGaussian
      else
        val covariance = right(ResidualCovariance.fromFactors(rows, Vector.fill(8)(1.0), DMat.tabulate(8, unitCount)((i, j) => if mapping(i) == j then .25 else 0.0)))
        val bound = right(BoundConfirmationCovariance(rows, covariance, "known-independent-blocks", TemporalCovarianceStatus.KnownGaussian))
        ConfirmationErrorLaw.RepeatedSubjects(right(BoundRepeatedGaussian(snapshot.samples, bound)))
    val design = right(ConfirmationDesign.admit(ConfirmationClaim.FixedDiscoveryC1, discovery, snapshot, exposure(snapshot.identity), contract, nuisance, law))
    val names = if includeIntercept then Vector("intercept", "drift") else Vector("drift")
    val plan = right(FrozenComponentConfirmation.freeze(design, contract.multiplicityFamily.take(2), contract.multiplicityFamily.drop(2), names, exposure(discovery.identity)))
    var trainingReads = 0; var confirmationReads = 0; var targetReads = 0
    private def operator(isTraining: Boolean) = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        if isTraining then trainingReads += 1 else confirmationReads += 1
        x.applyTo(input, output)
      override def transposeApplyTo(input: DVec, output: MutableDVec): Unit = x.transposeApplyTo(input, output)
    val trainingBrain = right(Observations.fromOperator(training, neural, operator(true), value("training-brain"), source("training-brain")))
    val trainingTargets = right(MultiResponse.fromDense(training, target, yTrain, value("training-target"), source("training-target")))
    val brain = right(Observations.fromOperator(rows, neural, operator(false), value("confirmation-brain"), source("confirmation-brain")))
    val responses = right(MultiResponse.fromDense(rows, target, y, value("confirmation-target"), source("confirmation-target")))
    val poisonOperator = new DoubleLinearOperator:
      val rows = 8; val cols = 2
      def applyTo(input: DVec, output: MutableDVec): Unit =
        targetReads += 1
        throw IllegalStateException("preflight must not read")
    val poisonTargets = right(MultiResponse.fromOperator(rows, target, poisonOperator, value("poison"), source("poison")))
    def heads = ComponentConfirmation.fitHeads(plan, trainingBrain, trainingTargets, trainingNuisance)

  private def key(name: String) = right(SubjectCoordinateKey(name))
  private val a = key("A"); private val b = key("B"); private val learner = key("learning-subject")
  private val contract = SubjectPredictionContract("new study participants", "independent external cohort",
    "squared target-metric loss conditional on the frozen learning cohort")
  private def labels(rows: AxisRef[?], values: Vector[SubjectCoordinateKey], id: String = "subject-labels") =
    right(Column.fromValues(rows, values, value(id)))
  private def shared(f: Fixture): SharedTaskCoordinates =
    val stability = right(SubjectAxisStability.freeze(f.discovery, exposure(f.discovery.identity), SubjectStabilityKind.StableAxes, "fixed diagnostic"))
    right(SharedTaskCoordinates.freeze(f.discovery, exposure(f.discovery.identity), f.neural, stability,
      right(SubjectCoordinateValueUnits("signal/score", "signal/target", "fixture units"))))
  private def headAccount(heads: ComponentPredictionHeads) = EvidenceExposure.internal(
    ExposureReference(planId, heads.plan.design.discovery.identity, "actual head training", ResultIdentity(heads.identity)))
  private def freeze(f: Fixture, heads: ComponentPredictionHeads, subjectRows: Vector[SubjectCoordinateKey] = Vector.fill(6)(a) ++ Vector.fill(2)(b)) =
    HeldOutSubjectPrediction.freeze(shared(f), heads, labels(f.training, Vector.fill(8)(learner)),
      labels(f.training, Vector.fill(8)(learner)), labels(f.rows, subjectRows), SubjectHeadTraining.SharedTraining,
      contract, headAccount(heads), exposure(f.snapshot.identity))

  test("actual frozen-head predictions use subjects as units despite unequal row counts"):
    val f = new Fixture
    val heads = right(f.heads)
    val plan = right(freeze(f, heads))
    val result = right(plan.evaluate(f.brain, f.responses, exposure(f.snapshot.identity)).result)
    val summary = right(SubjectPredictiveSummary.combine(Vector(a, b), Vector(result)))
    assertEquals(result.rowCounts, Vector(6, 2))
    assertEqualsDouble(summary.meanFullLoss, .375, 1e-12)
    // Independent R OLS with refitted reduced heads, then subject means.
    assertEqualsDouble(summary.meanImprovements(0), 3.3066243270259355, 1e-11)
    assertEqualsDouble(summary.meanImprovements(1), 12.172649730810374, 1e-11)
    assertEqualsDouble(result.improvements(0, 0), 5.69337567297406, 1e-11)
    assertEqualsDouble(result.improvements(1, 0), .919872981077807, 1e-11)
    assertEqualsDouble(summary.empiricalImprovementVariance(0), 11.3931639747704, 1e-10)
    assertEqualsDouble(summary.empiricalImprovementVariance(1), 2.66666666666667, 1e-10)
    assert(math.abs(summary.meanImprovements(0) - result.native.meanImprovements(0)) > 1)
    assertEquals(f.trainingReads, 2)
    assertEquals(f.confirmationReads, 2)
    assert(summary.prevalence.isLeft && summary.populationInference.isLeft)
    assertEquals(result.native.headIdentity, heads.identity)
    assertEquals(result.native.targetEvidenceIdentity, f.responses.identity)

  test("the subject reducer is invariant to grouping rows into unequal independent blocks within each subject"):
    val rows = new Fixture
    val blocks = new Fixture(Vector(0, 0, 0, 0, 1, 1, 2, 2), 3)
    val left = right(right(freeze(rows, right(rows.heads))).evaluate(rows.brain, rows.responses, exposure(rows.snapshot.identity)).result)
    val rightResult = right(right(freeze(blocks, right(blocks.heads))).evaluate(blocks.brain, blocks.responses, exposure(blocks.snapshot.identity)).result)
    for s <- 0 until 2; k <- 0 until 2 do
      assertEqualsDouble(rightResult.improvements(s, k), left.improvements(s, k), 1e-11)

  test("a unit spanning subjects is rejected before assessment sources are read"):
    val f = new Fixture(Vector(0, 0, 0, 0, 0, 0, 0, 1), 2)
    assert(freeze(f, right(f.heads)).left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Binding]))
    assertEquals(f.confirmationReads, 0)

  test("learning-subject contamination cannot be relabelled held out"):
    val f = new Fixture
    assert(freeze(f, right(f.heads), Vector.fill(8)(learner)).left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Leakage]))
    assertEquals(f.confirmationReads, 0)

  test("foreign row-bound columns and a borrowed head exposure account refuse"):
    val f = new Fixture; val heads = right(f.heads)
    val training = labels(f.training, Vector.fill(8)(learner)); val assessment = labels(f.rows, Vector.fill(8)(a))
    assert(HeldOutSubjectPrediction.freeze(shared(f), heads, assessment, training, assessment,
      SubjectHeadTraining.SharedTraining, contract, headAccount(heads), exposure(f.snapshot.identity)).isLeft)
    assert(HeldOutSubjectPrediction.freeze(shared(f), heads, training, training, assessment,
      SubjectHeadTraining.SharedTraining, contract, exposure(f.discovery.identity), exposure(f.snapshot.identity)).isLeft)

  test("unknown or subsequently viewed assessment exposure and numeric budgets refuse without reads"):
    val f = new Fixture; val heads = right(f.heads); val plan = right(freeze(f, heads))
    val account = exposure(f.snapshot.identity)
    val request = ExposureRequest(ExposurePurpose.DerivedScoreView, ExposureActorRole.Analyst, ExposureScope.Holdout,
      ExposurePayload.DerivedScore, ExposureAssurance.Declared, 1)
    val changed = ExposureControl.read(account, right(ExposureControl.permit(account, request)), request)(Right(())) match
      case ExposureAttempt.Completed(_, exposure) => exposure
      case other => fail(other.toString)
    val external = EvidenceExposure.external(account.reference)
    Vector(plan.evaluate(f.brain, f.poisonTargets, changed) -> changed,
      plan.evaluate(f.brain, f.poisonTargets, external) -> external,
      plan.evaluate(f.brain, f.poisonTargets, account, maximumSummaryCells = 0) -> account,
      plan.evaluate(f.brain, f.poisonTargets, account, ComponentConfirmationBudget(0)) -> account).foreach: (evaluation, supplied) =>
      assert(evaluation.result.isLeft)
      assert(!evaluation.readAttempted)
      assert(evaluation.exposure eq supplied, "pre-read refusals return the supplied ledger unchanged")
    assert(plan.evaluate(f.brain, f.poisonTargets, account, ComponentConfirmationBudget(0)).result.left.toOption
      .exists(_.isInstanceOf[SubjectPredictionError.Component]))
    assertEquals(f.confirmationReads, 0); assertEquals(f.targetReads, 0)

  test("evaluation records the held-out read, and the returned ledger refuses reuse of those subjects"):
    val f = new Fixture; val heads = right(f.heads); val plan = right(freeze(f, heads))
    val account = exposure(f.snapshot.identity)
    val evaluation = plan.evaluate(f.brain, f.responses, account)
    assert(evaluation.result.isRight && evaluation.readAttempted)
    assertEquals(evaluation.exposure.events.map(_.request.purpose), Vector(ExposurePurpose.PayloadRead, ExposurePurpose.DerivedScoreView))
    assert(evaluation.exposure.events.forall(_.request.scope == ExposureScope.Holdout))
    assert(evaluation.exposure.hasPayloadAccess(ExposureScope.Holdout) && evaluation.exposure.hasObservedScores(ExposureScope.Holdout))
    assert(ExposureControl.untouchedConfirmation(evaluation.exposure, ExposureScope.Holdout).isLeft)
    assert(account.events.isEmpty, "the supplied snapshot is an immutable value")
    val reads = f.confirmationReads
    val again = plan.evaluate(f.brain, f.responses, evaluation.exposure)
    assert(again.result.left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Leakage]) && !again.readAttempted)
    assertEquals(f.confirmationReads, reads)
    // A replacement plan cannot present the viewed assessment as untouched.
    assert(HeldOutSubjectPrediction.freeze(shared(f), heads, labels(f.training, Vector.fill(8)(learner)),
      labels(f.training, Vector.fill(8)(learner)), labels(f.rows, Vector.fill(6)(a) ++ Vector.fill(2)(b)),
      SubjectHeadTraining.SharedTraining, contract, headAccount(heads), evaluation.exposure)
      .left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Leakage]))

  test("a failed assessment read still records the attempted exposure"):
    val f = new Fixture; val plan = right(freeze(f, right(f.heads)))
    val evaluation = plan.evaluate(f.brain, f.poisonTargets, exposure(f.snapshot.identity))
    assert(evaluation.result.isLeft && evaluation.readAttempted)
    assertEquals(f.targetReads, 1)
    assert(evaluation.exposure.hasPayloadAccess(ExposureScope.Holdout))
    assert(!evaluation.exposure.hasObservedScores(ExposureScope.Holdout))
    assert(ExposureControl.untouchedConfirmation(evaluation.exposure, ExposureScope.Holdout).isLeft)

  test("single-subject head adaptation uses separate rows while shared coordinates stay external"):
    val global = new Fixture(trainingName = "global-training", assessmentName = "unused")
    val sharedCoordinates = shared(global)
    val learning = labels(global.training, Vector.fill(8)(learner))
    def adapted(subject: SubjectCoordinateKey): HeldOutSubjectResult =
      val f = new Fixture(trainingName = s"${subject.value}-adaptation", assessmentName = s"${subject.value}-assessment")
      val heads = right(f.heads)
      val plan = right(HeldOutSubjectPrediction.freeze(sharedCoordinates, heads, learning,
        labels(f.training, Vector.fill(8)(subject)), labels(f.rows, Vector.fill(8)(subject)),
        SubjectHeadTraining.SubjectAdaptation(subject, "OLS head on eight independent calibration rows"), contract,
        headAccount(heads), exposure(f.snapshot.identity)))
      val result = right(plan.evaluate(f.brain, f.responses, exposure(f.snapshot.identity)).result)
      assertEquals(f.trainingReads, 2)
      result
    val first = adapted(a); val second = adapted(b)
    val result = right(SubjectPredictiveSummary.combine(Vector(a, b), Vector(first, second)))
    assertEqualsDouble(result.meanFullLoss, .375, 1e-12)
    assertEqualsDouble(result.meanImprovements(0), 4.5, 1e-11)
    assertEqualsDouble(result.meanImprovements(1), 12.75, 1e-11)
    assertEquals(global.trainingReads, 0)
    assertNotEquals(first.plan.heads.identity, second.plan.heads.identity)

  test("adaptation cannot use another subject or replace the shared representation"):
    val global = new Fixture(trainingName = "global-training", assessmentName = "unused")
    val f = new Fixture; val heads = right(f.heads)
    val result = HeldOutSubjectPrediction.freeze(shared(global), heads, labels(global.training, Vector.fill(8)(learner)),
      labels(f.training, Vector.fill(8)(b)), labels(f.rows, Vector.fill(8)(a)),
      SubjectHeadTraining.SubjectAdaptation(a, "separate calibration"), contract, headAccount(heads), exposure(f.snapshot.identity))
    assert(result.left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Leakage]))
    val foreignFeatures = axis("foreign-common-features", 2)
    val base = shared(global)
    val foreign = right(SharedTaskCoordinates.freeze(global.discovery, exposure(global.discovery.identity), foreignFeatures, base.stability, base.valueUnits))
    assert(HeldOutSubjectPrediction.freeze(foreign, heads, labels(global.training, Vector.fill(8)(learner)),
      labels(f.training, Vector.fill(8)(a)), labels(f.rows, Vector.fill(8)(a)),
      SubjectHeadTraining.SubjectAdaptation(a, "separate calibration"), contract, headAccount(heads), exposure(f.snapshot.identity)).isLeft)
    // Even the exact same learning rows cannot be renamed as a new subject's calibration data.
    assert(HeldOutSubjectPrediction.freeze(shared(f), heads, labels(f.training, Vector.fill(8)(learner)),
      labels(f.training, Vector.fill(8)(a)), labels(f.rows, Vector.fill(8)(a)),
      SubjectHeadTraining.SubjectAdaptation(a, "separate calibration"), contract, headAccount(heads), exposure(f.snapshot.identity)).isLeft)

  test("complete cohort coverage rejects missing, repeated and reordered subjects"):
    val f = new Fixture
    val result = right(right(freeze(f, right(f.heads))).evaluate(f.brain, f.responses, exposure(f.snapshot.identity)).result)
    assert(SubjectPredictiveSummary.combine(Vector(a), Vector(result)).isLeft)
    assert(SubjectPredictiveSummary.combine(Vector(b, a), Vector(result)).isLeft)
    assert(SubjectPredictiveSummary.combine(Vector(a, b, a, b), Vector(result, result)).isLeft)
    assert(SubjectPredictiveSummary.combine(Vector(a, b), Vector.empty).isLeft)
    assert(SubjectPredictiveSummary.combine(Vector(a, b), Vector(result), maximumSummaryCells = 0)
      .left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Budget]))

  test("duplicated assessment evidence cannot create two additional subjects"):
    val f = new Fixture; val heads = right(f.heads)
    val first = right(right(freeze(f, heads)).evaluate(f.brain, f.responses, exposure(f.snapshot.identity)).result)
    val c = key("C"); val d = key("D")
    val second = right(right(freeze(f, heads, Vector.fill(6)(c) ++ Vector.fill(2)(d)))
      .evaluate(f.brain, f.responses, exposure(f.snapshot.identity)).result)
    assert(SubjectPredictiveSummary.combine(Vector(a, b, c, d), Vector(first, second))
      .left.toOption.exists(_.isInstanceOf[SubjectPredictionError.Binding]))

  test("negative subject usefulness is retained without clipping or assessment refitting"):
    val f = new Fixture; val heads = right(f.heads)
    val reducedTruth = DMat.tabulate(8, 2)((i, j) => if j == 0 then 7 + 4 * f.drift(i) + 4 * f.x(i, 1)
      else -3 - 2 * f.drift(i) + 1.5 * f.x(i, 1))
    val targets = right(MultiResponse.fromDense(f.rows, f.target, reducedTruth, value("reduced-truth"), source("reduced-truth")))
    val result = right(right(freeze(f, heads)).evaluate(f.brain, targets, exposure(f.snapshot.identity)).result)
    val summary = right(SubjectPredictiveSummary.combine(Vector(a, b), Vector(result)))
    assert(summary.meanImprovements.head < 0)
    assertEquals(f.trainingReads, 2)
    assert(summary.prevalence.isLeft)
