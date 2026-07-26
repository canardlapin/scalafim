package scalafim.fmri.mvpa.fit

import scalafim.multivar.contract.RequestedOptimizationClaim
import scalafim.multivar.family.canonical.{CanonicalEffectReferenceFixtures as R, ResidualRegularization, TraceRidgeFraction}
import scalafim.multivar.optimization.FeasibleSetKind

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.RunId
import scalafim.fmri.fit.{
  DesignMatrix,
  ResponseBlock,
  ResponsePreparationPlan,
  RunPartition,
  SelectedTimepointIndices,
  TContrast,
  TemporalNuisanceRank
}
import scalafim.fmri.model.FitConfig
import scalafim.fmri.mvpa.{FeatureSet, FeatureSetPlan, MvpaStreamControl, RoiId}

class ConstrainedCanonicalMvpaSuite extends munit.FunSuite:
  test("complete nonnegative one-shot MVPA agrees with the independent base-R active-face oracle"):
    val payload = success(analyze(R.runs.map(_.response)))

    payload.folds.zip(ConstrainedCanonicalReferenceFixtures.folds).foreach: (actual, expected) =>
      assertEqualsDouble(actual.trainingFit.root.value, expected.trainingRoot, 1e-7)
      assertEqualsDouble(actual.trainingFit.regularization.ridgeAmount, expected.ridge, 1e-10)
      assertEqualsDouble(actual.heldOutRoot, expected.heldOutRoot, 1e-7)
      var coordinate = 0
      while coordinate < actual.trainingFit.direction.length do
        assertEqualsDouble(actual.trainingFit.direction(coordinate), expected.direction(coordinate), 1e-7)
        assert(actual.trainingFit.direction(coordinate) >= -1e-12)
        coordinate += 1
      assertEquals(
        actual.trainingFit.programFit.program.constraints.map(_.feasibleSet),
        Vector(FeasibleSetKind.NonnegativeOrthant)
      )
      assertEquals(
        actual.trainingFit.programFit.program.resultSemantics.requestedClaim,
        RequestedOptimizationClaim.Stationary
      )
      assertEquals(actual.receipt.execution, CanonicalMomentExecution.RunwiseSufficientStatistics)

    assertEqualsDouble(payload.meanHeldOutRoot, ConstrainedCanonicalReferenceFixtures.meanHeldOutRoot, 1e-7)
    assertEqualsDouble(payload.rootToCorrelation, ConstrainedCanonicalReferenceFixtures.rootToCorrelationOfMean, 1e-9)

  test("held-out response perturbation cannot change its frozen training fit"):
    val baseline = success(analyze(R.runs.map(_.response)))
    val perturbedResponses = R.runs.map(_.response).updated(0, perturb(R.runs.head.response))
    val perturbed = success(analyze(perturbedResponses))
    val before = baseline.folds.head
    val after = perturbed.folds.head

    assertEqualsDouble(before.trainingFit.root.value, after.trainingFit.root.value, 0.0)
    var coordinate = 0
    while coordinate < before.trainingFit.direction.length do
      assertEqualsDouble(before.trainingFit.direction(coordinate), after.trainingFit.direction(coordinate), 0.0)
      coordinate += 1
    assertNotEquals(before.heldOutRoot, after.heldOutRoot)

  test("feature permutations preserve the estimand while general rotations need not"):
    val baseline = success(analyze(R.runs.map(_.response)))
    val permutation = Vector(2, 0, 1)
    val permuted = success(analyze(R.runs.map(run => permuteColumns(run.response, permutation))))
    val rotation = fromRows(
      Vector(
        Vector(0.8, -0.6, 0.0),
        Vector(0.6, 0.8, 0.0),
        Vector(0.0, 0.0, 1.0)
      )
    )
    val rotated = success(analyze(R.runs.map(run => run.response * rotation)))

    baseline.folds.zip(permuted.folds).foreach: (left, right) =>
      assertEqualsDouble(left.trainingFit.root.value, right.trainingFit.root.value, 1e-7)
      assertEqualsDouble(left.heldOutRoot, right.heldOutRoot, 1e-7)
    assert(Math.abs(baseline.meanHeldOutRoot - rotated.meanHeldOutRoot) > 1e-3)

  test("ordinary MVPA summary and streaming stop contracts remain available"):
    val dataset = canonicalDataset(R.runs.map(_.response))
    val plan = FeatureSetPlan
      .regional(
        "nonnegative-regions",
        Vector(
          FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2)),
          FeatureSet.unsafe(RoiId(2), Vector(0, 2))
        )
      )
      .toOption
      .get
    val result = NonnegativeCanonicalMvpa.run(dataset, plan, ridge)
    var visited = 0
    NonnegativeCanonicalMvpa.foreach(dataset, plan, ridge): _ =>
      visited += 1
      MvpaStreamControl.Stop

    assertEquals(result.summary.analysisName, NonnegativeCanonicalMvpa.AnalysisName)
    assertEquals(result.model.selection, ConstrainedCanonicalSelection.FixedBeforeFolds)
    assertEquals(result.summary.outcomes.length, 2)
    assertEquals(result.summary.successes.length, 2)
    assert(result.summary.successes.head.metrics("MeanNonnegativeCanonicalRoot").isDefined)
    assertEquals(visited, 1)

  private val ridge: ResidualRegularization =
    ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(R.ridgeFraction))

  private def analyze(responses: Vector[DMat]): NonnegativeCanonicalMvpaResult =
    val dataset = canonicalDataset(responses)
    val plan = FeatureSetPlan
      .regional("nonnegative-canonical", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2))))
      .toOption
      .get
    NonnegativeCanonicalMvpa.run(dataset, plan, ridge)

  private def canonicalDataset(responses: Vector[DMat]): CanonicalEffectDataset =
    val geometry = ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = DesignMatrix.unsafe(R.design),
        columnNames = Vector("intercept", "task", "drift"),
        contrast = TContrast("task", Map("task" -> 1.0)),
        selectedTimepoints = SelectedTimepointIndices.unsafe((0 until R.design.rows).toVector),
        partitions = Vector(RunPartition(0, (0 until R.design.rows).toVector, (0 until R.design.rows).toVector)),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get
    val schedule = CanonicalGeometrySchedule.stable(geometry).toOption.get
    val runs = responses.zipWithIndex.map: (response, index) =>
      CanonicalRunInput.make(RunId(s"run-$index"), ResponseBlock.unsafe(response), schedule).toOption.get
    CanonicalEffectDataset.make(runs).toOption.get

  private def success(result: NonnegativeCanonicalMvpaResult): NonnegativeCanonicalFeatureSetPayload =
    result.outcomes.head match
      case NonnegativeCanonicalFeatureSetOutcome.Success(payload) => payload
      case NonnegativeCanonicalFeatureSetOutcome.Failure(_, error) => fail(error.message)

  private def perturb(value: DMat): DMat =
    Matrix.tabulate(value.rows, value.cols): (row, column) =>
      value(row, column) + (if column == 0 then 0.4 * ((row % 3) - 1).toDouble else 0.0)

  private def permuteColumns(value: DMat, permutation: Vector[Int]): DMat =
    Matrix.tabulate(value.rows, value.cols)((row, column) => value(row, permutation(column)))

  private def fromRows(rows: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))
