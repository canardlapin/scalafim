package scalafim.fmri.mvpa

import multivar.family.canonical.{CanonicalEffectSolution, ResidualRegularization, TraceRidgeFraction}

import gale.linalg.{DMat, Matrix}
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

class SignedCrossRunRayleighMvpaSuite extends munit.FunSuite:
  import SignedCrossRunRayleighReferenceFixtures as R

  private val regularization = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
  test("folds match the independent dense base-R cross-run fixture"):
    val result = CanonicalAnalysisTestSupport.signed(
      dataset(R.responses),
      Vector(0, 1, 2, 3),
      regularization,
      revision = "signed-base-r"
    )

    assertEquals(result.counts.failed, 0)
    val payload = CanonicalAnalysisTestSupport.success(result)
    assertEquals(payload.folds.length, 3)
    payload.folds.zipWithIndex.foreach: (fold, index) =>
      assertEqualsDouble(fold.numerator, R.numerators(index), 1e-8)
      assertEqualsDouble(fold.denominator, R.denominators(index), 1e-10)
      assertEqualsDouble(fold.statistic.toDouble, R.statistics(index), 1e-8)
      assertEqualsDouble(fold.trainingFit.regularization.ridgeAmount, R.ridgeAmounts(index), 1e-11)
      fold.trainingFit.solution match
        case CanonicalEffectSolution.Simple(direction, _) =>
          (0 until direction.length).foreach: feature =>
            assertEqualsDouble(direction(feature), R.directions(index)(feature), 1e-7)
        case other => fail(s"expected a simple direction, got $other")
      assertEquals(fold.receipt.training.length, 2)
      assertEquals(fold.receipt.relationFits.length, 3)
    assertEqualsDouble(payload.meanStatistic.toDouble, R.meanStatistic, 1e-8)

  test("a held-out run sign flip reverses its signed fold without changing the frozen training fit"):
    val ordinary = onlyPayload(dataset(R.responses))
    val flippedResponses = R.responses.updated(0, scaleRows(R.responses.head, -1.0))
    val flipped = onlyPayload(dataset(flippedResponses))
    val ordinaryFold = foldFor(ordinary, PartitionId.unsafe("run-0"))
    val flippedFold = foldFor(flipped, PartitionId.unsafe("run-0"))

    assertEqualsDouble(flippedFold.statistic.toDouble, -ordinaryFold.statistic.toDouble, 1e-9)
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
    val reordered = onlyPayload(
      dataset(
        permutation.map(R.responses),
        partitions = permutation.map(index => PartitionId.unsafe(s"run-$index"))
      )
    )

    assertEqualsDouble(globalSign.meanStatistic.toDouble, reference.meanStatistic.toDouble, 1e-9)
    assertEqualsDouble(commonScale.meanStatistic.toDouble, reference.meanStatistic.toDouble, 1e-8)
    assertEqualsDouble(contrastScale.meanStatistic.toDouble, reference.meanStatistic.toDouble, 1e-8)
    reference.folds.foreach: fold =>
      val id = fold.receipt.heldOut
      assertEqualsDouble(foldFor(globalSign, id).statistic.toDouble, fold.statistic.toDouble, 1e-9)
      assertEqualsDouble(foldFor(commonScale, id).statistic.toDouble, fold.statistic.toDouble, 1e-8)
      assertEqualsDouble(foldFor(contrastScale, id).statistic.toDouble, fold.statistic.toDouble, 1e-8)
      assertEqualsDouble(foldFor(reordered, id).statistic.toDouble, fold.statistic.toDouble, 1e-8)

  test("signed cross-run Rayleigh uses the ordinary typed measurement result and receipt"):
    val result = CanonicalAnalysisTestSupport.signed(
      dataset(R.responses),
      Vector(0, 1, 2, 3),
      regularization,
      revision = "signed-result-shell"
    )

    assertEquals(result.counts.succeeded, 1)
    assertEquals(result.values.length, 1)
    assertEquals(result.receipt.work.succeeded, 1)
    assertEquals(CanonicalAnalysisTestSupport.success(result).computation.folds.length, 3)

  test("non-finite signed values and degenerate training residuals are represented as typed failures"):
    scalafim.fmri.mvpa.SignedCrossRunRayleigh(Double.NaN).left.toOption match
      case Some(CanonicalTaskFailure.InvalidResult(detail)) => assert(detail.contains("finite"))
      case other                                            => fail(s"expected a typed non-finite value, got $other")
    val exactResponses = Vector.tabulate(3): run =>
      R.design.map: row =>
        Vector(
          row(0) + (run + 1.0) * row(1),
          2.0 * row(0) - row(1),
          row(2),
          row(0) + row(1) + row(2)
        )
    val result = CanonicalAnalysisTestSupport.signed(
      dataset(exactResponses),
      Vector(0, 1, 2, 3),
      regularization,
      revision = "signed-degenerate"
    )

    assertEquals(result.counts.succeeded, 0)
    result.values.head.outcome match
      case MeasurementOutcome.Failed(error: CanonicalTaskFailure, _) =>
        assert(error.message.nonEmpty)
      case other => fail(s"expected a typed canonical degeneracy, got $other")

  private def onlyPayload[N <: multivar.core.SemanticSpace](
      data: CanonicalEffectDataset[N, FeatureId]
  ): SignedCrossRunRayleighEstimate =
    CanonicalAnalysisTestSupport.success(
      CanonicalAnalysisTestSupport.signed(data, Vector(0, 1, 2, 3), regularization)
    )

  private def foldFor(payload: SignedCrossRunRayleighEstimate, partition: PartitionId): SignedCrossRunFoldEstimate =
    payload.folds.find(_.receipt.heldOut == partition).getOrElse(fail(s"missing fold for ${partition.value}"))

  private def dataset(
      responses: Vector[Vector[Vector[Double]]],
      contrastWeight: Double = 1.0,
      partitions: Vector[PartitionId] = Vector(
        PartitionId.unsafe("run-0"),
        PartitionId.unsafe("run-1"),
        PartitionId.unsafe("run-2")
      )
  ): CanonicalEffectDataset[? <: multivar.core.SemanticSpace, FeatureId] =
    val schedule = CanonicalGeometrySchedule.stable(geometry(contrastWeight)).toOption.get
    val neural = AxisRef
      .create(
        AxisId.unsafe("signed-cross-run-neural"),
        AxisPurpose.NeuralFeatures,
        Vector.tabulate(responses.head.head.length)(index => FeatureId.unsafe(s"feature-$index")),
        CoordinateBasis.unsafe("declared-feature-order"),
        None,
        AxisScale.nominal,
        CoordinateProvenance.unsafe("signed-cross-run-suite", "v1")
      )
      .toOption
      .get
    val runs = responses
      .zip(partitions)
      .map: (rows, partition) =>
        CanonicalRunInput(
          partition,
          ResponseBlock.unsafe(fromRows(rows)),
          schedule,
          neural
        ).toOption.get
    CanonicalEffectDataset(runs).toOption.get

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
