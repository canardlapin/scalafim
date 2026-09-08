package scalafim.fmri.mvpa

import multivar.contract.RequestedOptimizationClaim
import multivar.family.canonical.{ResidualRegularization, TraceRidgeFraction}
import multivar.optimization.FeasibleSetKind

import gale.linalg.{DMat, Matrix}
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

class ConstrainedCanonicalMvpaSuite extends munit.FunSuite:
  private val R = CanonicalEffectReferenceFixtures
  test("complete nonnegative relation analysis agrees with the independent base-R active-face oracle"):
    val payload = success(analyze(R.runs.map(_.response)))

    payload.folds
      .zip(ConstrainedCanonicalReferenceFixtures.folds)
      .foreach: (actual, expected) =>
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
        assertEquals(actual.receipt.training.length, 2)

    assertEqualsDouble(payload.meanHeldOutRoot, ConstrainedCanonicalReferenceFixtures.meanHeldOutRoot, 1e-7)
    assertEqualsDouble(
      payload.canonicalCorrelation,
      ConstrainedCanonicalReferenceFixtures.rootToCorrelationOfMean,
      1e-9
    )

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

    baseline.folds
      .zip(permuted.folds)
      .foreach: (left, right) =>
        assertEqualsDouble(left.trainingFit.root.value, right.trainingFit.root.value, 1e-7)
        assertEqualsDouble(left.heldOutRoot, right.heldOutRoot, 1e-7)
    assert(Math.abs(baseline.meanHeldOutRoot - rotated.meanHeldOutRoot) > 1e-3)

  test("constrained canonical uses the ordinary typed result and execution receipt"):
    val dataset = canonicalDataset(R.runs.map(_.response))
    val result = CanonicalAnalysisTestSupport.constrained(
      dataset,
      Vector(0, 1, 2),
      ridge,
      revision = "constrained-result-shell"
    )

    assertEquals(result.counts.succeeded, 1)
    assertEquals(result.values.length, 1)
    assertEquals(result.receipt.work.succeeded, 1)
    assertEquals(success(result).folds.length, 3)

  private val ridge: ResidualRegularization =
    ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(R.ridgeFraction))

  private def analyze(responses: Vector[DMat]): CanonicalAnalysisTestSupport.ConstrainedResult =
    val dataset = canonicalDataset(responses)
    CanonicalAnalysisTestSupport.constrained(
      dataset,
      Vector(0, 1, 2),
      ridge,
      revision = s"constrained-${responses.length}-${responses.head.rows}"
    )

  private def canonicalDataset(
      responses: Vector[DMat]
  ): CanonicalEffectDataset[? <: multivar.core.SemanticSpace, FeatureId] =
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
    val neural = AxisRef
      .create(
        AxisId.unsafe("constrained-canonical-neural"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(responses.head.cols)(index => FeatureId.unsafe(s"feature-$index")),
        CoordinateBasis.unsafe("declared-feature-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("constrained-canonical-suite", "v1")
      )
      .toOption
      .get
    val runs = responses.zipWithIndex.map: (response, index) =>
      CanonicalRunInput(
        PartitionId.unsafe(s"run-$index"),
        ResponseBlock.unsafe(response),
        schedule,
        neural
      ).toOption.get
    CanonicalEffectDataset(runs).toOption.get

  private def success(
      result: CanonicalAnalysisTestSupport.ConstrainedResult
  ): ConstrainedCanonicalEstimate =
    CanonicalAnalysisTestSupport.success(result)

  private def perturb(value: DMat): DMat =
    Matrix.tabulate(value.rows, value.cols): (row, column) =>
      value(row, column) + (if column == 0 then 0.4 * ((row % 3) - 1).toDouble else 0.0)

  private def permuteColumns(value: DMat, permutation: Vector[Int]): DMat =
    Matrix.tabulate(value.rows, value.cols)((row, column) => value(row, permutation(column)))

  private def fromRows(rows: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, column) => rows(row)(column))
