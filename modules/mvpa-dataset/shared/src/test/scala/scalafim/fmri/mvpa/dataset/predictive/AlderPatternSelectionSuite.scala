package scalafim.fmri.mvpa.dataset.predictive

import alder.kernel.DataFingerprint
import gale.linalg.DMat
import gale.optim.{FirstOrderConfig, FirstOrderTolerance}
import multivar.core.SpaceRole
import resample4s.core.{Coverage, DigestAlgorithm, Labels}
import resample4s.designs.FixedPartitions
import scalafim.fmri.mvpa.*
import scalafim.fmri.mvpa.pattern.*
import multivar.core.{ValueId, ValueIdentity}
import scalafim.response.{SourceId, ProvenanceId, Provenance}

class AlderPatternSelectionSuite extends munit.FunSuite:
  given DigestAlgorithm = DigestAlgorithm.fnv1a64
  private def right[A](value: Either[?, A]): A = value.fold(error => fail(error.toString), identity)
  private def axis(name: String, n: Int, role: SpaceRole = SpaceRole.Observed): AxisRef[String] =
    right(AxisRef.fromStableKeys(name, role, Vector.tabulate(n)(i => s"$name-$i"), "fixture", "one", "raw", Vector("pattern-selection-fixture")))
  private val strict = right(FirstOrderConfig.from(100000, right(FirstOrderTolerance.from(1e-10, 1e-10))))
  private def candidate(rank: Int = 1, sparsity: Double = 0.0, relativeFloor: Double = 1e-3,
      supportTv: Double = 0.0, signedSmoothness: Double = 0.0): PatternCandidate =
    val fit = right(StructuredPatternConfig(right(SupportPenalty(sparsity, supportTv, signedSmoothness)), maximumOuterIterations = 250, inner = strict,
      stationarityTolerance = 1e-7, objectiveTolerance = 1e-9))
    PatternCandidate(rank, TwoStagePatternFitPolicy(fit, right(ResidualCovarianceFitPolicy(1,
      relativeFloor = relativeFloor, maximumIterations = 20000, tolerance = 1e-13, diagonalMomentTolerance = 1e-10)), 10000000L))
  private val budget = PatternSelectionBudget(100L, 10000000L, 10000000L)
  private val inner = new PatternInnerValidation:
    def build(samples: AxisRef[String]): Either[EvidenceError, ValidationDesign[samples.Id, String, Coverage.ExactOnce]] =
      val assignment = IArray.from(Vector.tabulate(samples.size)(i => i / 8))
      for
        labels <- Labels.retained(assignment).left.map(error => EvidenceError.InvalidSource(error.toString))
        fixed <- FixedPartitions.once(labels).left.map(error => EvidenceError.InvalidSource(error.toString))
        bound <- ValidationDesign.bind(samples, fixed, ScientificSeed.fromLong(71L))
      yield bound

  private final class Fixture(x: DMat, y: DMat, edges: Vector[SupportEdge] = Vector.empty):
    val samples = axis("samples", 32, SpaceRole.Samples)
    val neural = axis("neural", x.cols)
    val target = axis("target", y.cols)
    val keys = samples.toRecord.stableKeys
    val mapping = right(NativeAxisMapping.fromAxis(samples, Vector.tabulate(32)(i => 1000L - 17L * i), DataFingerprint.external("pattern-fixture-v1")))
    val rows = right(AlderPredictiveAdmission.materialized(samples.descriptor, x, y, keys, mapping, right(MaterializationBudget(10000L))))
    val design = right(ValidationDesign.bind(samples, right(FixedPartitions.once(right(Labels.retained(IArray.from(Vector.tabulate(32)(i => i / 16)))))), ScientificSeed.fromLong(37L)))
    val graph = right(SupportGraph(neural.descriptor, edges, SupportTopology.Declared("fixture-graph"), "one"))
    def run(selection: PatternSelection, task: PatternSelectionTask = PatternSelectionTask.Regression(target), admitted: PatternSelectionBudget = budget) =
      AlderPatternSelection.crossValidate(rows, design, neural, graph, task, selection, admitted)

  private def walsh(row: Int, mask: Int): Double = if (Integer.bitCount((row % 8) & mask) % 2) == 0 then 1.0 else -1.0
  private val loading = Vector(2.0, 1.0, -1.0)
  private val intercept = Vector(10.0, 20.0, 30.0)
  private val regressionX = DMat.tabulate(32, 3)((row, col) => intercept(col) + loading(col) * walsh(row, 4) + walsh(row, Vector(2, 1, 3)(col)))
  private def regressionY(offset: Double = 0.0): DMat = DMat.tabulate(32, 1)((row, _) => 7.0 + walsh(row, 4) + (if row < 16 then offset else 0.0))

  test("fixed fit reports keyed held-out Gaussian loss and rich actual training artifacts"):
    val f = new Fixture(regressionX, regressionY())
    val result = right(f.run(PatternSelection.Fixed(candidate())))
    assertEquals(result.rows.map(_.stableKey), f.keys)
    assertEqualsDouble(result.assessment.taskLoss, 1.0 / 7.0, 2e-6)
    assertEquals(result.work.plannedLearnerCalls, 2L)
    assertEquals(result.work.actualLearnerCalls, 2L)
    assertEquals(result.work.successfulLearnerCalls, 2L)
    assertEquals(result.work.plannedStructuredFits, 4L)
    assertEquals(result.work.plannedCovarianceFitCalls, 2L)
    assert(result.fits.forall(_.model.fitted.covarianceFit.receipt.converged))
    result.fits.foreach: fit =>
      assertEquals(fit.audit.data.digest, fit.model.prediction.artifact.trainingBinding.fingerprintDigest)
      assertEquals(fit.trainingStableKeys.size, 16)
      fit.model.neuralMean.zip(intercept).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-12))
      assertEqualsDouble(fit.model.targetMean.head, 7.0, 1e-12)
      val predicted = right(fit.model.run(Array(12.0, 21.0, 29.0)))
      predicted match
        case PatternHeldOutPrediction.Continuous(values) => assertEqualsDouble(values.head, 7.0 + 6.0 / 7.0, 2e-6)
        case other => fail(other.toString)

  test("native inner search selects held-out task loss and outer targets cannot tune the first fold"):
    val good = candidate()
    val misspecified = candidate(sparsity = 100.0)
    val selection = PatternSelection.Nested(Vector(good, misspecified), inner)
    val f = new Fixture(regressionX, regressionY())
    val result = right(f.run(selection))
    val changed = right(new Fixture(regressionX, regressionY(13.0)).run(selection))
    assertEquals(result.work.plannedLearnerCalls, 10L)
    assertEquals(result.work.actualLearnerCalls, 10L)
    assertEquals(result.work.successfulLearnerCalls, 10L)
    assertEquals(result.work.plannedStructuredFits, 20L)
    assert(result.fits.forall(_.selected.policy.structured.penalty.sparsity == 0.0))
    val original = result.fits.head.model
    val perturbed = changed.fits.head.model
    assertEquals(original.neuralMean, perturbed.neuralMean)
    assertEquals(original.targetMean, perturbed.targetMean)
    assertEquals(result.fits.head.selected.policy.structured.penalty.sparsity, changed.fits.head.selected.policy.structured.penalty.sparsity)
    assertEquals(original.prediction.covariance.diagonalValues, perturbed.prediction.covariance.diagonalValues)
    for row <- 0 until 3 do
      assertEqualsDouble(original.prediction.factors.neuralByComponent(row, 0), perturbed.prediction.factors.neuralByComponent(row, 0), 1e-9)
    assertEquals(right(original.run(Array(12.0, 21.0, 29.0))), right(perturbed.run(Array(12.0, 21.0, 29.0))))
    assertEquals(original.trainingContentIdentity, perturbed.trainingContentIdentity)
    assertEquals(original.servingIdentity, perturbed.servingIdentity)
    assertNotEquals(result.assessment.taskLoss, changed.assessment.taskLoss)
    result.fits.foreach: fit =>
      val study = fit.innerSearch.get
      assertEquals(study.trials.size, 2)
      assert(study.trials.forall(_.objective.isRight), "a bad decoder remains a numerically valid fit")
      val goodLoss = study.trials.head.objective.toOption.get
      val badLoss = study.trials(1).objective.toOption.get
      assert(goodLoss < badLoss, "valid numerical fits can have different predictive performance")
      assert(study.evidence.flatMap(_.folds).forall(_.analysis != fit.audit.data))

  test("unbalanced categorical codebook restores the correct serving intercept and training priors"):
    val x = DMat.tabulate(32, 3): (row, col) =>
      val local = row % 8
      val h = if local < 6 then -1.0 else 1.0
      val noise = if local / 2 == col then (if local % 2 == 0 then 2.0 else -2.0) else 0.0
      intercept(col) + loading(col) * h + noise
    val y = DMat.tabulate(32, 1)((row, _) => if row % 8 < 6 then 0.0 else 1.0)
    val f = new Fixture(x, y)
    val conditions = right(AxisRef.fromStableKeys("conditions", SpaceRole.Observed, Vector("negative", "positive"), "fixture", "one", "raw", Vector("fixed-codebook")))
    val task = PatternSelectionTask.Classification(conditions, f.target, DMat.dense(2, 1, Vector(-1.0, 1.0)), Vector(0.0, 1.0))
    val result = right(f.run(PatternSelection.Fixed(candidate()), task))
    assertEqualsDouble(result.assessment.classificationAccuracy.get, 1.0, 1e-12)
    result.fits.foreach: fit =>
      assertEqualsDouble(fit.model.targetMean.head, -0.5, 1e-12)
      fit.model.prediction.effectiveCentering match
        case CenteringPolicy.ExplicitIntercept(offset, _) => offset.values.zip(intercept).foreach((actual, expected) => assertEqualsDouble(actual, expected, 2e-6))
        case other => fail(other.toString)
      val atIntercept = right(fit.model.run(intercept.toArray))
      atIntercept match
        case PatternHeldOutPrediction.Categorical(value) =>
          assertEquals(value.keys, Vector("negative", "positive"))
          assertEqualsDouble(value.probabilities.head, 0.75, 2e-6)
          assertEqualsDouble(value.probabilities(1), 0.25, 2e-6)
        case other => fail(other.toString)

  test("two-response fixed fit reports the independent posterior MSE one quarter"):
    val a = Vector(Vector(2.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0), Vector(-1.0, 0.0))
    val x = DMat.tabulate(32, 4)((row, col) => col + 10.0 + a(col)(0) * walsh(row, 4) + a(col)(1) * walsh(row, 2) + walsh(row, Vector(1, 3, 5, 6)(col)))
    val y = DMat.tabulate(32, 2)((row, col) => (if col == 0 then 7.0 else -3.0) + walsh(row, if col == 0 then 4 else 2))
    val f = new Fixture(x, y)
    val result = right(f.run(PatternSelection.Fixed(candidate(rank = 2))))
    assertEqualsDouble(result.assessment.taskLoss, 0.25, 5e-6)
    assertEquals(result.assessment.regression.get.outputs, 2)
    assertEquals(result.rows.size, 32)
    assert(result.fits.forall(_.model.prediction.factors.componentAxis.size == 2))

  test("same declared source and ID fingerprint cannot alias shifted training target means"):
    val first = new Fixture(regressionX, regressionY())
    val second = new Fixture(regressionX, DMat.tabulate(32, 1)((row, _) => regressionY()(row, 0) + 13.0))
    val original = right(first.run(PatternSelection.Fixed(candidate()))).fits.head
    val shifted = right(second.run(PatternSelection.Fixed(candidate()))).fits.head
    assertEquals(original.audit.data.digest, shifted.audit.data.digest)
    assertEquals(original.audit.data.policy, shifted.audit.data.policy)
    assertNotEquals(original.model.trainingContentIdentity, shifted.model.trainingContentIdentity)
    assertNotEquals(original.model.servingIdentity, shifted.model.servingIdentity)
    assertNotEquals(original.model.prediction.numericalIdentity, shifted.model.prediction.numericalIdentity)
    val a = right(original.model.run(Array(12.0, 21.0, 29.0)))
    val b = right(shifted.model.run(Array(12.0, 21.0, 29.0)))
    (a, b) match
      case (PatternHeldOutPrediction.Continuous(before), PatternHeldOutPrediction.Continuous(after)) =>
        assertEqualsDouble(after.head - before.head, 13.0, 2e-6)
      case other => fail(other.toString)

  test("native inner loss selects predictive rank over a numerically valid rank-one misspecification"):
    val a = Vector(Vector(2.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0), Vector(-1.0, 0.0))
    val x = DMat.tabulate(32, 4)((row, col) => col + 10.0 + a(col)(0) * walsh(row, 4) + a(col)(1) * walsh(row, 2) + walsh(row, Vector(1, 3, 5, 6)(col)))
    val y = DMat.tabulate(32, 2)((row, col) => (if col == 0 then 7.0 else -3.0) + walsh(row, if col == 0 then 4 else 2))
    val f = new Fixture(x, y)
    val result = right(f.run(PatternSelection.Nested(Vector(candidate(rank = 1), candidate(rank = 2)), inner)))
    assert(result.fits.forall(_.selected.rank == 2))
    result.fits.foreach: fit =>
      val study = fit.innerSearch.get
      assert(study.trials.forall(_.objective.isRight), "both ranks must be numerically valid")
      assert(study.trials.head.objective.toOption.get > study.trials(1).objective.toOption.get)
    assertEqualsDouble(result.assessment.taskLoss, 0.25, 5e-6)

  test("native inner loss selects residual covariance regularization on correlated training noise"):
    val x = DMat.tabulate(32, 3)((row, col) => intercept(col) + loading(col) * walsh(row, 4) +
      2.0 * walsh(row, 1) + walsh(row, Vector(2, 3, 5)(col)))
    val f = new Fixture(x, regressionY())
    val result = right(f.run(PatternSelection.Nested(Vector(candidate(relativeFloor = 0.9), candidate()), inner)))
    assert(result.fits.forall(_.selected.policy.covariance.relativeFloor == 1e-3))
    result.fits.foreach: fit =>
      val study = fit.innerSearch.get
      assert(study.trials.forall(_.objective.isRight), "both noise policies must be numerically valid")
      assert(study.trials.head.objective.toOption.get > study.trials(1).objective.toOption.get)
    // Psi = I + 4 1 1^T, A = (2,1,-1): A^T Psi^-1 A = 62/13.
    assertEqualsDouble(result.assessment.taskLoss, 13.0 / 75.0, 5e-6)

  test("native materialized feature and regression target axes cannot be relabeled"):
    val f = new Fixture(regressionX, regressionY())
    val id = SourceId.unsafe("pattern-native-axis")
    val source = right(EvidenceSource(id, Provenance.source(ProvenanceId.unsafe("pattern-native-axis-root"), id)))
    val x = right(Observations.fromDense(f.samples, f.neural, regressionX, ValueIdentity.source(ValueId.unsafe("native-pattern-x")), source))
    val y = right(MultiResponse.fromDense(f.samples, f.target, regressionY(), ValueIdentity.source(ValueId.unsafe("native-pattern-y")), source))
    val native = right(AlderPredictiveAdmission.nativeTables(x, y, f.keys, DataFingerprint.external("native-pattern-metadata"),
      f.mapping, right(NativeReadPolicy(3, right(MaterializationBudget(10000L))))))
    val foreignNeural = axis("foreign-neural", 3)
    val foreignSupport = right(SupportGraph(foreignNeural.descriptor, Vector.empty, SupportTopology.Declared("foreign"), "one"))
    val neuralRefusal = AlderPatternSelection.crossValidate(native, f.design, foreignNeural, foreignSupport,
      PatternSelectionTask.Regression(f.target), PatternSelection.Fixed(candidate()), budget)
    assertEquals(neuralRefusal.left.toOption, Some(AlderPatternSelectionError.Admission("native neural evidence axis does not match the declared fit axis")))
    val foreignTarget = axis("foreign-target", 1)
    val targetRefusal = AlderPatternSelection.crossValidate(native, f.design, f.neural, f.graph,
      PatternSelectionTask.Regression(foreignTarget), PatternSelection.Fixed(candidate()), budget)
    assertEquals(targetRefusal.left.toOption, Some(AlderPatternSelectionError.Admission("native regression target evidence axis does not match the declared fit axis")))
    assert(right(AlderPatternSelection.crossValidate(native, f.design, f.neural, f.graph,
      PatternSelectionTask.Regression(f.target), PatternSelection.Fixed(candidate()), budget)).fits.nonEmpty)

  test("support and signed penalties are selected by held-out loss on a declared spatial graph"):
    val f = new Fixture(regressionX, regressionY(), Vector(SupportEdge(0, 2, 1.0)))
    val result = right(f.run(PatternSelection.Nested(Vector(candidate(sparsity = 0.1, supportTv = 1.0),
      candidate(signedSmoothness = 1.0), candidate()), inner)))
    assert(result.fits.forall(fit => fit.selected.policy.structured.penalty.supportTv == 0.0 &&
      fit.selected.policy.structured.penalty.signedSmoothness == 0.0), result.fits.map(_.innerSearch.get.trials.map(_.objective)).toString)
    result.fits.foreach: fit =>
      val trials = fit.innerSearch.get.trials
      assert(trials.forall(_.objective.isRight), trials.map(_.objective).toString)
      assert(trials(2).objective.toOption.get < trials.head.objective.toOption.get)
      assert(trials(2).objective.toOption.get < trials(1).objective.toOption.get)
    assertEquals(result.work.actualLearnerCalls, 14L)

  test("missing classes unknown held-out codes and singular targets are explicit refusals"):
    val conditions = right(AxisRef.fromStableKeys("failure-conditions", SpaceRole.Observed, Vector("zero", "one"), "fixture", "one", "raw", Vector("fixed-codebook")))
    val missing = new Fixture(regressionX, DMat.zeros(32, 1))
    val task = PatternSelectionTask.Classification(conditions, missing.target, DMat.dense(2, 1, Vector(-1.0, 1.0)), Vector(0.0, 1.0))
    assertEquals(missing.run(PatternSelection.Fixed(candidate()), task).left.toOption, Some(AlderPatternSelectionError.MissingClass("one")))
    val singular = new Fixture(regressionX, DMat.tabulate(32, 1)((_, _) => 7.0))
    assert(singular.run(PatternSelection.Fixed(candidate())).isLeft, "constant targets must refuse a singular Gaussian prior")
    val unknown = new Fixture(regressionX, DMat.tabulate(32, 1)((row, _) => if row < 16 then 9.0 else if row % 8 < 4 then 0.0 else 1.0))
    assert(unknown.run(PatternSelection.Fixed(candidate()), task).isLeft, "an undeclared held-out class cannot produce accuracy")

  test("full nested cost and materialization are admitted before any candidate fit"):
    val f = new Fixture(regressionX, regressionY())
    assert(f.run(PatternSelection.Nested(Vector(candidate(), candidate(sparsity = 100.0)), inner),
      admitted = budget.copy(maximumLearnerCalls = 9L)).isLeft)
    assert(f.run(PatternSelection.Fixed(candidate()), admitted = budget.copy(maximumTrainingCells = 0L)).isLeft)
    assert(f.run(PatternSelection.Fixed(candidate()), admitted = budget.copy(maximumRetainedCells = 0L)).isLeft)
    assert(f.run(PatternSelection.Nested(Vector.empty, inner)).isLeft)
