package scalafim.fmri.mvpa.fit

import scalafim.multivar.family.canonical.ResidualRegularization

import scalafim.dataset.RunId
import scalafim.fmri.fit.*
import scalafim.fmri.model.FitConfig
import scalafim.fmri.mvpa.*
import gale.linalg.{DMat, Matrix}

class ManovaMvpaSuite extends munit.FunSuite:

  test("one-shot MANOVA matches the independent dense base-R fold fixture"):
    val payload = success(ManovaMvpa.run(dataset(ordinaryContrast), plan, ResidualRegularization.Unregularized))

    payload.folds.zip(ManovaReferenceFixtures.folds).foreach: (actual, expected) =>
      actual.heldOutRoots.values.zip(expected.roots).foreach: (root, reference) =>
        assertClose(root.value, reference, 1e-7, 1e-9)
      assertClose(actual.heldOutStatistics.royLargestRoot, expected.roy, 1e-7, 1e-9)
      assertClose(actual.heldOutStatistics.wilksLambda, expected.wilks, 1e-9, 1e-9)
      assertClose(actual.heldOutStatistics.pillaiTrace, expected.pillai, 1e-9, 1e-9)
      assertClose(actual.heldOutStatistics.hotellingLawleyTrace, expected.hotelling, 1e-7, 1e-9)
      assertEquals(actual.receipt.execution, ManovaMomentExecution.RunwiseSufficientStatistics)
      assertEquals(actual.trainingFit.programFit.program.objective.label, "maximize-trace")

    val mean = payload.meanStatistics
    assertClose(mean.royLargestRoot, ManovaReferenceFixtures.mean(0), 1e-7, 1e-9)
    assertClose(mean.wilksLambda, ManovaReferenceFixtures.mean(1), 1e-9, 1e-9)
    assertClose(mean.pillaiTrace, ManovaReferenceFixtures.mean(2), 1e-9, 1e-9)
    assertClose(mean.hotellingLawleyTrace, ManovaReferenceFixtures.mean(3), 1e-7, 1e-9)

  test("invertible contrast-basis changes preserve every held-out estimand"):
    val ordinary = success(ManovaMvpa.run(dataset(ordinaryContrast), plan, ResidualRegularization.Unregularized))
    val changed = success(ManovaMvpa.run(dataset(changedContrast), plan, ResidualRegularization.Unregularized))

    ordinary.folds.zip(changed.folds).foreach: (left, right) =>
      left.heldOutRoots.values.zip(right.heldOutRoots.values).foreach: (a, b) =>
        assertClose(a.value, b.value, 1e-7, 1e-9)
      assertClose(left.heldOutStatistics.royLargestRoot, right.heldOutStatistics.royLargestRoot, 1e-7, 1e-9)
      assertClose(left.heldOutStatistics.wilksLambda, right.heldOutStatistics.wilksLambda, 1e-9, 1e-9)
      assertClose(left.heldOutStatistics.pillaiTrace, right.heldOutStatistics.pillaiTrace, 1e-9, 1e-9)
      assertClose(left.heldOutStatistics.hotellingLawleyTrace, right.heldOutStatistics.hotellingLawleyTrace, 1e-7, 1e-9)

  test("held-out perturbations cannot alter the frozen training spectrum"):
    val baseline = success(ManovaMvpa.run(dataset(ordinaryContrast), plan, ResidualRegularization.Unregularized))
    val changedResponses = ManovaReferenceFixtures.responses.updated(0, perturb(ManovaReferenceFixtures.responses.head))
    val perturbed = success(
      ManovaMvpa.run(dataset(ordinaryContrast, changedResponses), plan, ResidualRegularization.Unregularized)
    )
    val before = baseline.folds.head
    val after = perturbed.folds.head

    before.trainingFit.roots.values.zip(after.trainingFit.roots.values).foreach: (left, right) =>
      assertEqualsDouble(left.value, right.value, 0.0)
    assertEqualsDouble(before.trainingFit.programFit.objectiveValue, after.trainingFit.programFit.objectiveValue, 0.0)
    assertNotEquals(before.heldOutStatistics.hotellingLawleyTrace, after.heldOutStatistics.hotellingLawleyTrace)

  test("MANOVA feature traversal uses the ordinary summary and streaming stop contract"):
    val data = dataset(ordinaryContrast)
    val featureSets = FeatureSetPlan
      .regional(
        "manova-regions",
        Vector(
          FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2, 3, 4)),
          FeatureSet.unsafe(RoiId(2), Vector(0, 1, 2))
        )
      )
      .toOption
      .get
    val result = ManovaMvpa.run(data, featureSets, ResidualRegularization.Unregularized)
    var visited = 0
    ManovaMvpa.foreach(data, featureSets, ResidualRegularization.Unregularized): _ =>
      visited += 1
      MvpaStreamControl.Stop

    assertEquals(result.summary.analysisName, ManovaMvpa.AnalysisName)
    assertEquals(result.summary.outcomes.length, 2)
    assertEquals(result.summary.successes.length, 2)
    assertEquals(result.summary.successes.head.metrics("RoyLargestRoot").isDefined, true)
    assertEquals(visited, 1)

  private val ordinaryContrast =
    FContrast("a-and-b", Vector(Map("a" -> 1.0), Map("b" -> 1.0)))

  private val changedContrast =
    FContrast(
      "changed-basis",
      Vector(
        Map("a" -> 1.0, "b" -> 1.0),
        Map("a" -> 2.0, "b" -> -1.0)
      )
    )

  private val plan = FeatureSetPlan
    .regional("manova-fixture", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2, 3, 4))))
    .toOption
    .get

  private def dataset(
      contrast: FContrast,
      responses: Vector[DMat] = ManovaReferenceFixtures.responses
  ): ManovaDataset =
    val geometry = ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareManova(
        DesignMatrix.unsafe(ManovaReferenceFixtures.design),
        Vector("intercept", "a", "b", "drift"),
        contrast,
        SelectedTimepointIndices.unsafe((0 until 10).toVector),
        Vector(RunPartition(0, (0 until 10).toVector, (0 until 10).toVector)),
        TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get
    val schedule = ManovaGeometrySchedule.stable(geometry).toOption.get
    val runs = responses.zipWithIndex.map: (response, index) =>
      ManovaRunInput.make(RunId(s"run-${index + 1}"), ResponseBlock.unsafe(response), schedule).toOption.get
    ManovaDataset.make(runs).toOption.get

  private def success(result: ManovaMvpaResult): ManovaFeatureSetPayload =
    result.outcomes.head match
      case ManovaFeatureSetOutcome.Success(payload) => payload
      case other                                     => fail(s"unexpected MANOVA outcome: $other")

  private def perturb(value: DMat): DMat =
    val out = Matrix.newBuilder(value.rows, value.cols)
    var row = 0
    while row < value.rows do
      var col = 0
      while col < value.cols do
        val delta = if col == 0 then 0.35 * (row % 3 - 1).toDouble else 0.0
        out(row, col) = value(row, col) + delta
        col += 1
      row += 1
    out.result()

  private def assertClose(actual: Double, expected: Double, absTol: Double, relTol: Double): Unit =
    val tolerance = math.max(absTol, relTol * math.max(math.abs(actual), math.abs(expected)))
    assertEqualsDouble(actual, expected, tolerance)
