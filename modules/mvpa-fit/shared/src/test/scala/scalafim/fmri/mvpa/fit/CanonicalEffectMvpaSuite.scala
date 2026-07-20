package scalafim.fmri.mvpa.fit

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.Matrix
import scalafim.dataset.RunId
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegments, WhiteningMethod, WhiteningPlan}
import scalafim.fmri.fit.{
  DesignMatrix,
  PreparedContrastGeometry,
  ResponseBlock,
  ResponsePreparationPlan,
  RunPartition,
  SelectedTimepointIndices,
  TContrast,
  TemporalNuisanceRank,
  TemporalPreparationScope,
  TrainingRunScope
}
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig}
import scalafim.fmri.mvpa.{FeatureIndex, FeatureSet, FeatureSetPlan, MvpaStreamControl, RoiId}
import scalafim.multivar.{ResidualRegularization, TraceRidgeFraction}

class CanonicalEffectMvpaSuite extends munit.FunSuite:

  test("runwise streaming moments agree with batch products and explicit dense projectors"):
    val geometry = iidGeometry()
    val response = responseBlock(runResponses.head)
    val positions = Array(3, 0, 2)
    val streamed = CanonicalMoments.accumulate(response, geometry, positions).toOption.get
    val selected = selectColumns(response.value, positions)
    val design = geometry.preparedDesign.value
    val cross = selected.t * design
    val effectScore = cross * geometry.effectBasis
    val expectedEffect = effectScore * effectScore.t
    val expectedResidual = subtract(selected.t * selected, cross * geometry.inverseXtX * cross.t)
    val xProjector = design * geometry.inverseXtX * design.t
    val effectProjector = design * geometry.effectBasis * geometry.effectBasis.t * design.t
    val explicitEffect = selected.t * effectProjector * selected
    val explicitResidual = selected.t * subtract(DMat.eye(design.rows), xProjector) * selected

    assertMatrixClose(streamed.total, selected.t * selected, 1e-11)
    assertMatrixClose(streamed.responseDesign, cross, 1e-11)
    assertMatrixClose(streamed.effect, expectedEffect, 1e-11)
    assertMatrixClose(streamed.residual, expectedResidual, 1e-11)
    assertMatrixClose(streamed.effect, explicitEffect, 1e-11)
    assertMatrixClose(streamed.residual, explicitResidual, 1e-11)

  test("regional and searchlight plans execute through ordinary MVPA summaries and typed canonical payloads"):
    val dataset = canonicalDataset(runResponses)
    val ridge = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
    val regional = FeatureSetPlan
      .regional(
        "canonical-regions",
        Vector(
          FeatureSet.unsafe(RoiId(10), Vector(0, 1)),
          FeatureSet.unsafe(RoiId(11), Vector(0, 1, 2, 3))
        )
      )
      .toOption
      .get
    val searchlight = FeatureSetPlan
      .searchlight(
        "canonical-searchlights",
        Vector(
          FeatureSet.unsafe(RoiId(1), Vector(0, 1, 2), center = Some(1)),
          FeatureSet.unsafe(RoiId(2), Vector(1, 2, 3), center = Some(2))
        )
      )
      .toOption
      .get

    Vector(regional, searchlight).foreach: plan =>
      val result = CanonicalEffectMvpa.run(dataset, plan, ridge)
      assertEquals(result.summary.analysisName, CanonicalEffectMvpa.AnalysisName)
      assertEquals(result.summary.featureSetPlan, Some(plan))
      assertEquals(result.summary.failures, Vector.empty)
      assertEquals(result.successes.length, plan.size)
      result.successes.foreach: payload =>
        assertEquals(payload.folds.length, 3)
        assert(payload.meanHeldOutRoot >= 0.0)
        assert(payload.rootToCorrelation >= 0.0 && payload.rootToCorrelation <= 1.0)
        assert(payload.folds.forall(_.receipt.execution == CanonicalMomentExecution.RunwiseSufficientStatistics))
        assert(payload.folds.forall(_.receipt.temporalPreparation.length == 3))
      result.summary.successes.foreach: outcome =>
        assert(outcome.metrics("MeanCanonicalRoot").exists(_ >= 0.0))
        assert(outcome.metrics("CanonicalCorrelation").exists(value => value >= 0.0 && value <= 1.0))

  test("held-out response perturbations change the held-out score but cannot alter its frozen training frame"):
    val plan = FeatureSetPlan
      .regional("leakage-sentinel", Vector(FeatureSet.unsafe(RoiId(20), Vector(0, 1, 2, 3))))
      .toOption
      .get
    val regularization = ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
    val ordinary = CanonicalEffectMvpa.run(canonicalDataset(runResponses), plan, regularization).successes.head
    val perturbedRows = runResponses.updated(0, addHeldOutTaskSignal(runResponses.head, 25.0))
    val perturbed = CanonicalEffectMvpa.run(canonicalDataset(perturbedRows), plan, regularization).successes.head
    val ordinaryFold = foldFor(ordinary, "run-0")
    val perturbedFold = foldFor(perturbed, "run-0")
    val ordinaryFrame = ordinaryFold.trainingFit.functionalFrame.weights.toDense.toOption.get
    val perturbedFrame = perturbedFold.trainingFit.functionalFrame.weights.toDense.toOption.get

    assertMatrixClose(ordinaryFrame, perturbedFrame, 0.0)
    assertEquals(
      ordinaryFold.trainingFit.provenance.regularizedResidual,
      perturbedFold.trainingFit.provenance.regularizedResidual
    )
    assertNotEquals(ordinaryFold.heldOutRoot, perturbedFold.heldOutRoot)

  test("feature-set traversal honors the existing streaming stop control"):
    val dataset = canonicalDataset(runResponses)
    val plan = FeatureSetPlan
      .regional(
        "stop-after-one",
        Vector(
          FeatureSet.unsafe(RoiId(30), Vector(0, 1)),
          FeatureSet.unsafe(RoiId(31), Vector(1, 2)),
          FeatureSet.unsafe(RoiId(32), Vector(2, 3))
        )
      )
      .toOption
      .get
    var visited = 0

    CanonicalEffectMvpa.foreach(dataset, plan, ResidualRegularization.Unregularized): _ =>
      visited += 1
      MvpaStreamControl.Stop

    assertEquals(visited, 1)

  test("dataset and geometry schedules reject incomplete or mismatched run structure"):
    val geometry = iidGeometry()
    val schedule = CanonicalGeometrySchedule.stable(geometry).toOption.get
    val response = responseBlock(runResponses.head)
    val first = CanonicalRunInput.make(RunId("run-0"), response, schedule).toOption.get
    val duplicate = CanonicalEffectDataset.make(Vector(first, first))
    val wrongAxis = CanonicalRunInput
      .make(
        RunId("run-1"),
        response,
        schedule,
        Vector(FeatureIndex(0), FeatureIndex(1), FeatureIndex(2), FeatureIndex(99))
      )
      .toOption
      .get
    val mismatched = CanonicalEffectDataset.make(Vector(first, wrongAxis))

    assertEquals(
      CanonicalEffectDataset.make(Vector(first)).left.toOption,
      Some(OneShotMvpaError.InsufficientRunsForCrossValidation(1))
    )
    duplicate.left.toOption match
      case Some(OneShotMvpaError.DuplicateRuns(ids)) => assertEquals(ids, Vector(RunId("run-0")))
      case other => fail(s"expected duplicate-run failure, got $other")
    assert(mismatched.left.toOption.exists(_.isInstanceOf[OneShotMvpaError.FeatureAxisMismatch]))

  test("response-learned temporal geometry must resolve the exact training-run scope"):
    val geometries = trainingFoldGeometries()
    val complete = CanonicalGeometrySchedule.trainingFolds(geometries).toOption.get
    val incomplete = CanonicalGeometrySchedule.trainingFolds(geometries.take(1)).toOption.get
    val completeRuns = runResponses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput.make(RunId(s"run-$index"), responseBlock(rows), complete).toOption.get
    val incompleteRuns = runResponses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput.make(RunId(s"run-$index"), responseBlock(rows), incomplete).toOption.get

    val dataset = CanonicalEffectDataset.make(completeRuns).toOption.get
    val result = CanonicalEffectMvpa.run(
      dataset,
      FeatureSetPlan.regional("fold-scoped", Vector(FeatureSet.unsafe(RoiId(35), Vector(0, 1)))).toOption.get,
      ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
    )

    assertEquals(result.summary.failures, Vector.empty)
    result.successes.head.folds.zipWithIndex.foreach: (fold, heldOut) =>
      val expected = (0 until 3).filter(_ != heldOut).toVector
      assert(
        fold.receipt.temporalPreparation.forall: (_, receipt) =>
          receipt.scope match
            case TemporalPreparationScope.TrainingFold(scope) => scope.runs.map(_.value) == expected
            case _ => false
      )
    assert(CanonicalEffectDataset.make(incompleteRuns).left.toOption.exists(_.isInstanceOf[OneShotMvpaError.MissingFoldGeometry]))

  test("the canonical analysis surface requires no trialwise beta or TrialReadout artifact"):
    val payload = CanonicalEffectMvpa
      .run(
        canonicalDataset(runResponses),
        FeatureSetPlan.regional("no-trial-beta", Vector(FeatureSet.unsafe(RoiId(40), Vector(0, 1)))).toOption.get,
        ResidualRegularization.TraceScaled(TraceRidgeFraction.unsafe(0.05))
      )
      .successes
      .head

    assert(payload.folds.forall(_.receipt.execution == CanonicalMomentExecution.RunwiseSufficientStatistics))
    assert(payload.folds.forall(_.trainingFit.programFit.program.objective.label == "generalized-rayleigh"))

  private def canonicalDataset(responses: Vector[Vector[Vector[Double]]]): CanonicalEffectDataset =
    val geometry = iidGeometry()
    val schedule = CanonicalGeometrySchedule.stable(geometry).toOption.get
    val runs = responses.zipWithIndex.map: (rows, index) =>
      CanonicalRunInput
        .make(RunId(s"run-$index"), responseBlock(rows), schedule)
        .toOption
        .get
    CanonicalEffectDataset.make(runs).toOption.get

  private def iidGeometry(): PreparedContrastGeometry =
    val design = DesignMatrix.unsafe(fromRows(designRows))
    ResponsePreparationPlan
      .fromConfig(FitConfig())
      .prepareContrast(
        design = design,
        columnNames = Vector("intercept", "task", "drift"),
        contrast = TContrast("task", Map("task" -> 1.0)),
        selectedTimepoints = SelectedTimepointIndices.unsafe(designRows.indices.toVector),
        partitions = Vector(RunPartition(0, designRows.indices.toVector, designRows.indices.toVector)),
        nuisanceRank = TemporalNuisanceRank.unsafe(2)
      )
      .toOption
      .get

  private def trainingFoldGeometries(): Vector[PreparedContrastGeometry] =
    val design = DesignMatrix.unsafe(fromRows(designRows))
    val partitions = Vector(RunPartition(0, designRows.indices.toVector, designRows.indices.toVector))
    val options = ArOptions(structure = ArStructure.Ar(1), global = true)
    val segments = TimeSegments.continuous(design.timepoints)
    val whitening = WhiteningPlan.global(
      ArmaCoefficients(Vector(0.2)),
      segments,
      exactFirstAr1 = true,
      method = WhiteningMethod.Estimated
    )
    Vector(Vector(1, 2), Vector(0, 2), Vector(0, 1)).map: training =>
      val scope = TrainingRunScope.fromInts(training).toOption.get
      ResponsePreparationPlan
        .fromConfig(FitConfig(autocorrelation = options))
        .prepareContrast(
          design = design,
          columnNames = Vector("intercept", "task", "drift"),
          contrast = TContrast("task", Map("task" -> 1.0)),
          selectedTimepoints = SelectedTimepointIndices.unsafe(designRows.indices.toVector),
          partitions = partitions,
          nuisanceRank = TemporalNuisanceRank.unsafe(2),
          scope = TemporalPreparationScope.TrainingFold(scope),
          whitening = scalafim.fmri.fit.CanonicalTemporalWhitening.Shared(whitening)
        )
        .toOption
        .get

  private def foldFor(payload: CanonicalFeatureSetPayload, heldOut: String): CanonicalFoldResult =
    payload.folds.find(_.receipt.heldOutRun == RunId(heldOut)).getOrElse(fail(s"missing fold for $heldOut"))

  private def addHeldOutTaskSignal(rows: Vector[Vector[Double]], amount: Double): Vector[Vector[Double]] =
    rows.zip(designRows).map: (response, design) =>
      response.updated(0, response.head + amount * design(1))

  private val designRows = Vector(
    Vector(1.0, -1.0, -1.0),
    Vector(1.0, -1.0, -0.7),
    Vector(1.0, 1.0, -0.4),
    Vector(1.0, 1.0, -0.1),
    Vector(1.0, -1.0, 0.1),
    Vector(1.0, -1.0, 0.4),
    Vector(1.0, 1.0, 0.7),
    Vector(1.0, 1.0, 1.0)
  )

  private val baseResponse = Vector(
    Vector(-0.9, 0.1, -0.15, -0.2),
    Vector(-1.14, 0.5, -0.09, -0.5),
    Vector(0.91, -0.85, 0.21, 0.5),
    Vector(0.67, -0.55, 0.47, 0.2),
    Vector(-1.02, 0.75, -0.47, -0.1),
    Vector(-0.56, 0.65, -0.56, -0.4),
    Vector(0.99, -0.2, 0.49, 0.6),
    Vector(1.05, -0.4, 0.1, 0.3)
  )

  private val runResponses: Vector[Vector[Vector[Double]]] =
    Vector.tabulate(3): run =>
      baseResponse.zipWithIndex.map: (row, time) =>
        row.zipWithIndex.map: (value, feature) =>
          value + 0.03 * run.toDouble * ((time + feature) % 3 - 1).toDouble

  private def responseBlock(rows: Vector[Vector[Double]]): ResponseBlock =
    ResponseBlock.unsafe(fromRows(rows))

  private def selectColumns(matrix: DMat, positions: Array[Int]): DMat =
    val out = Matrix.newBuilder(matrix.rows, positions.length)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < positions.length do
        out(row, col) = matrix(row, positions(col))
        col += 1
      row += 1
    out.result()

  private def subtract(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) - right(row, col)
        col += 1
      row += 1
    out.result()

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
