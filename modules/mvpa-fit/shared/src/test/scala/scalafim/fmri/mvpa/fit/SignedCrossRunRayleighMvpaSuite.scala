package scalafim.fmri.mvpa.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.RunId
import scalafim.fmri.fit.{
  DesignMatrix,
  PreparedContrastGeometry,
  ResponseBlock,
  ResponsePreparationPlan,
  RunPartition,
  SelectedTimepointIndices,
  TContrast,
  TemporalNuisanceRank
}
import scalafim.fmri.model.FitConfig
import scalafim.fmri.mvpa.{FeatureSet, FeatureSetPlan, MvpaStreamControl, RoiId}
import scalafim.multivar.{CanonicalEffectSolution, ResidualRegularization, TraceRidgeFraction}

class SignedCrossRunRayleighMvpaSuite extends munit.FunSuite:
  import SignedCrossRunRayleighReferenceFixtures as R

  private val regularization = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
  private val plan = FeatureSetPlan
    .regional("signed-cross-run-reference", Vector(FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2, 3))))
    .toOption
    .get

  test("folds match the independent dense base-R cross-run fixture"):
    val result = SignedCrossRunRayleighMvpa.run(dataset(R.responses), plan, regularization)

    assertEquals(result.summary.analysisName, SignedCrossRunRayleighMvpa.AnalysisName)
    assertEquals(result.summary.failures, Vector.empty)
    val payload = result.successes.head
    assertEquals(payload.folds.length, 3)
    payload.folds.zipWithIndex.foreach: (fold, index) =>
      assertEqualsDouble(fold.numerator, R.numerators(index), 1e-8)
      assertEqualsDouble(fold.denominator, R.denominators(index), 1e-10)
      assertEqualsDouble(fold.statistic.value, R.statistics(index), 1e-8)
      assertEqualsDouble(fold.trainingFit.regularization.ridgeAmount, R.ridgeAmounts(index), 1e-11)
      fold.trainingFit.solution match
        case CanonicalEffectSolution.Simple(direction, _) =>
          (0 until direction.length).foreach: feature =>
            assertEqualsDouble(direction(feature), R.directions(index)(feature), 1e-7)
        case other => fail(s"expected a simple direction, got $other")
      assertEquals(fold.receipt.estimator, SignedCrossRunEstimator.FrozenTrainingDirection)
      assertEquals(fold.receipt.orientation, SignedCrossRunOrientation.AgreementSign)
      assertEquals(fold.receipt.exchangeability, SignedCrossRunExchangeability.RunWiseContrastSignFlip)
      assertEquals(fold.receipt.canonical.trainingRuns.length, 2)
      assertEquals(fold.receipt.canonical.temporalPreparation.length, 3)
    assertEqualsDouble(payload.meanStatistic.value, R.meanStatistic, 1e-8)
    assertEqualsDouble(result.summary.successes.head.metrics("SignedCrossRunRayleigh").get, R.meanStatistic, 1e-8)

  test("a held-out run sign flip reverses its signed fold without changing the frozen training fit"):
    val ordinary = onlyPayload(dataset(R.responses))
    val flippedResponses = R.responses.updated(0, scaleRows(R.responses.head, -1.0))
    val flipped = onlyPayload(dataset(flippedResponses))
    val ordinaryFold = foldFor(ordinary, RunId("run-0"))
    val flippedFold = foldFor(flipped, RunId("run-0"))

    assertEqualsDouble(flippedFold.statistic.value, -ordinaryFold.statistic.value, 1e-9)
    assertEqualsDouble(flippedFold.numerator, -ordinaryFold.numerator, 1e-9)
    assertEqualsDouble(flippedFold.denominator, ordinaryFold.denominator, 1e-12)
    assertEquals(
      flippedFold.trainingFit.provenance.regularizedResidual,
      ordinaryFold.trainingFit.provenance.regularizedResidual
    )
    assertMatrixClose(
      flippedFold.trainingFit.functionalFrame.weights.toDense.toOption.get,
      ordinaryFold.trainingFit.functionalFrame.weights.toDense.toOption.get,
      0.0
    )

  test("global response sign, common scale, contrast scale, and run order obey their invariance laws"):
    val reference = onlyPayload(dataset(R.responses))
    val globalSign = onlyPayload(dataset(R.responses.map(scaleRows(_, -1.0))))
    val commonScale = onlyPayload(dataset(R.responses.map(scaleRows(_, 7.5))))
    val contrastScale = onlyPayload(dataset(R.responses, contrastWeight = -4.0))
    val permutation = Vector(2, 0, 1)
    val reordered = onlyPayload(dataset(permutation.map(R.responses), runIds = permutation.map(index => RunId(s"run-$index"))))

    assertEqualsDouble(globalSign.meanStatistic.value, reference.meanStatistic.value, 1e-9)
    assertEqualsDouble(commonScale.meanStatistic.value, reference.meanStatistic.value, 1e-8)
    assertEqualsDouble(contrastScale.meanStatistic.value, reference.meanStatistic.value, 1e-8)
    reference.folds.foreach: fold =>
      val id = fold.receipt.canonical.heldOutRun
      assertEqualsDouble(foldFor(globalSign, id).statistic.value, fold.statistic.value, 1e-9)
      assertEqualsDouble(foldFor(commonScale, id).statistic.value, fold.statistic.value, 1e-8)
      assertEqualsDouble(foldFor(contrastScale, id).statistic.value, fold.statistic.value, 1e-8)
      assertEqualsDouble(foldFor(reordered, id).statistic.value, fold.statistic.value, 1e-8)

  test("ordinary feature-set streaming can stop without a signed-specific execution engine"):
    val multiPlan = FeatureSetPlan
      .regional(
        "signed-streaming",
        Vector(
          FeatureSet.unsafe(RoiId(10), Vector(0, 1)),
          FeatureSet.unsafe(RoiId(11), Vector(1, 2)),
          FeatureSet.unsafe(RoiId(12), Vector(2, 3))
        )
      )
      .toOption
      .get
    var visited = 0

    SignedCrossRunRayleighMvpa.foreach(dataset(R.responses), multiPlan, regularization): _ =>
      visited += 1
      MvpaStreamControl.Stop

    assertEquals(visited, 1)

  test("non-finite signed values and degenerate training residuals are represented as typed failures"):
    SignedCrossRunRayleigh(Double.NaN).left.toOption match
      case Some(OneShotMvpaError.InvalidSignedCrossRunValue(value)) => assert(value.isNaN)
      case other => fail(s"expected a typed non-finite value, got $other")
    val exactResponses = Vector.tabulate(3): run =>
      R.design.map: row =>
        Vector(
          row(0) + (run + 1.0) * row(1),
          2.0 * row(0) - row(1),
          row(2),
          row(0) + row(1) + row(2)
        )
    val result = SignedCrossRunRayleighMvpa.run(dataset(exactResponses), plan, regularization)

    assertEquals(result.successes, Vector.empty)
    result.outcomes.head match
      case SignedCrossRunFeatureSetOutcome.Failure(_, error: OneShotMvpaError.CanonicalEffectFailure) =>
        assert(error.message.nonEmpty)
      case other => fail(s"expected a typed canonical degeneracy, got $other")

  private def onlyPayload(data: CanonicalEffectDataset): SignedCrossRunFeatureSetPayload =
    SignedCrossRunRayleighMvpa.run(data, plan, regularization).successes.head

  private def foldFor(payload: SignedCrossRunFeatureSetPayload, runId: RunId): SignedCrossRunFoldResult =
    payload.folds.find(_.receipt.canonical.heldOutRun == runId).getOrElse(fail(s"missing fold for ${runId.value}"))

  private def dataset(
      responses: Vector[Vector[Vector[Double]]],
      contrastWeight: Double = 1.0,
      runIds: Vector[RunId] = Vector(RunId("run-0"), RunId("run-1"), RunId("run-2"))
  ): CanonicalEffectDataset =
    val schedule = CanonicalGeometrySchedule.stable(geometry(contrastWeight)).toOption.get
    val runs = responses.zip(runIds).map: (rows, runId) =>
      CanonicalRunInput.make(runId, ResponseBlock.unsafe(fromRows(rows)), schedule).toOption.get
    CanonicalEffectDataset.make(runs).toOption.get

  private def geometry(contrastWeight: Double): PreparedContrastGeometry =
    val design = DesignMatrix.unsafe(fromRows(R.design))
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = design,
        columnNames = Vector("intercept", "task", "drift"),
        contrast = TContrast("task", Map("task" -> contrastWeight)),
        selectedTimepoints = SelectedTimepointIndices.unsafe(R.design.indices.toVector),
        partitions = Vector(RunPartition(0, R.design.indices.toVector, R.design.indices.toVector)),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get

  private def scaleRows(rows: Vector[Vector[Double]], scale: Double): Vector[Vector[Double]] =
    rows.map(_.map(_ * scale))

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
