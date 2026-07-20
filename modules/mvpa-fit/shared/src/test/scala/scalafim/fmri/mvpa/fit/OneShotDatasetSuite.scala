package scalafim.fmri.mvpa.fit

import scalafim.dataset.RunId
import scalafim.fmri.fit.{LeastSquaresSeparate, LssTrialDesign, ResponseBlock}
import scalafim.fmri.mvpa.*
import gale.linalg.{DMat, Matrix}

class OneShotDatasetSuite extends munit.FunSuite:

  test("run-local readouts compose and stack to the explicit trial-pattern oracle"):
    val dataset = oneShotDataset()
    val expected = expectedPatterns()
    val composed = dataset.patterns.materialize.toOption.get
    val explicit = dataset.explicitPatterns.toOption.get

    assertEquals(dataset.samples, 6)
    assertEquals(dataset.features, 4)
    assertEquals(dataset.patterns.provenance.origin, PatternOperatorOrigin.Composed)
    assertEquals(dataset.patterns.provenance.stackedParts, 3)
    assert(dataset.runs.forall(_.fitScope == ReadoutFitScope.DesignOnly))
    assertMatrixClose(composed.value, expected)
    assertMatrixClose(explicit.value, expected)
    assertEquals(dataset.trialRows.map(_.sampleIndex.value), Vector(0, 1, 2, 3, 4, 5))
    assertEquals(dataset.trialRows.map(_.runId.value), Vector("run-1", "run-1", "run-2", "run-2", "run-3", "run-3"))
    assertEquals(dataset.trialRows.map(_.runOrdinal.value), Vector(0, 0, 1, 1, 2, 2))
    assertEquals(dataset.trialRows.map(_.trialWithinRun.value), Vector(0, 1, 0, 1, 0, 1))
    assertEquals(dataset.trialRows.map(_.trialId.value), Vector("a_1", "b_1", "a_2", "b_2", "a_3", "b_3"))

  test("stacked one-shot operator satisfies adjoint and fold-restriction laws"):
    val dataset = oneShotDataset()
    val weights = fromRows(
      Vector(
        Vector(0.5, -1.0),
        Vector(1.0, 0.25),
        Vector(-0.5, 2.0),
        Vector(0.75, -0.25)
      )
    )
    val sampleProbe = fromRows(
      Vector(
        Vector(1.0, -0.5),
        Vector(0.25, 1.0),
        Vector(-1.5, 0.75),
        Vector(0.5, -1.0),
        Vector(2.0, 0.25),
        Vector(-0.75, 1.5)
      )
    )
    val forward = dataset.patterns.applyTo(weights).toOption.get
    val adjoint = dataset.patterns.transposeApplyTo(sampleProbe).toOption.get

    assertEqualsDouble(frobeniusDot(forward, sampleProbe), frobeniusDot(weights, adjoint), 1e-10)

    val folds = dataset.leaveOneRunOut.toOption.get
    assertEquals(folds.folds.length, 3)
    assertEquals(folds.folds.map(_.test.map(_.value)), Vector(Vector(0, 1), Vector(2, 3), Vector(4, 5)))

    val firstFold = folds.folds.head
    val training = dataset.patternsFor(firstFold, FoldPartition.Training).toOption.get.materialize.toOption.get
    val test = dataset.patternsFor(firstFold, FoldPartition.Test).toOption.get.materialize.toOption.get
    val expected = PatternMatrix(
      expectedPatterns(),
      Vector.tabulate(6)(SampleIndex.apply),
      Vector.tabulate(4)(FeatureIndex.apply)
    )

    assertMatrixClose(training.value, expected.selectRows(firstFold.train).toOption.get.value)
    assertMatrixClose(test.value, expected.selectRows(firstFold.test).toOption.get.value)

  test("one-shot pattern source pushes ROI selection into each run composition"):
    val dataset = oneShotDataset()
    val featureSet = FeatureSet.unsafe(RoiId(9), Vector(3, 0))
    val pushedDown = dataset.patternSource.selectFeatures(featureSet).toOption.get
    val genericRestriction = dataset.patterns.selectFeatures(featureSet).toOption.get
    val explicit = dataset.explicitPatterns.toOption.get.selectFeatures(featureSet).toOption.get

    assertEquals(pushedDown.featureIndices.map(_.value), Vector(3, 0))
    assertEquals(pushedDown.provenance.origin, PatternOperatorOrigin.Composed)
    assertEquals(pushedDown.provenance.featureSelections, dataset.runs.length)
    assertMatrixClose(pushedDown.materialize.toOption.get.value, explicit.value)
    assertMatrixClose(pushedDown.materialize.toOption.get.value, genericRestriction.materialize.toOption.get.value)

  test("one-shot engine returns existing ridge-LDA outcomes across feature sets"):
    val dataset = oneShotDataset()
    val folds = dataset.leaveOneRunOut.toOption.get
    val response = Response.categorical(Vector("a", "b", "a", "b", "a", "b")).toOption.get
    val featureSets = Vector(
      FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
      FeatureSet.unsafe(RoiId(2), Vector(0, 1, 2, 3))
    )
    val plan = FeatureSetPlan.regional("one-shot-regions", featureSets).toOption.get
    val denseAnalysis = CrossValidatedClassifierAnalysis(RidgeLdaClassifier(gamma = 0.2), storePredictions = true)
    val operatorAnalysis = RoiAnalysis.materializing(denseAnalysis)

    val actual = OneShotMvpaEngine.run(dataset, plan, response, operatorAnalysis).toOption.get
    val expected = MvpaEngine
      .run(dataset.explicitPatterns.toOption.get, plan, response, denseAnalysis, Some(folds))
      .toOption
      .get

    assertEquals(actual.analysisName, expected.analysisName)
    assertEquals(actual.failures, Vector.empty)
    assertEquals(actual.successes.map(_.roiId.value), expected.successes.map(_.roiId.value))
    actual.successes.zip(expected.successes).foreach: (observed, reference) =>
      assertEqualsDouble(observed.metrics("Accuracy").get, reference.metrics("Accuracy").get, 1e-12)
      (observed.payload, reference.payload) match
        case (Some(RoiPayload.Classification(left)), Some(RoiPayload.Classification(right))) =>
          assertMatrixClose(left.probabilities, right.probabilities)
          assertEquals(left.predicted.map(_.value), right.predicted.map(_.value))
        case other => fail(s"unexpected ridge payloads: $other")

  test("one-shot engine trains operator-native ridge through composed readouts"):
    val dataset = oneShotDataset()
    val folds = dataset.leaveOneRunOut.toOption.get
    val response = Response.categorical(Vector("a", "b", "a", "b", "a", "b")).toOption.get
    val featureSets = Vector(
      FeatureSet.unsafe(RoiId(11), Vector(0, 1)),
      FeatureSet.unsafe(RoiId(12), Vector(3, 0, 2))
    )
    val plan = FeatureSetPlan.regional("native-one-shot-ridge", featureSets).toOption.get
    val config = OperatorRidgeConfig.unsafe(penalty = 0.5, tolerance = 1e-12)
    val analysis = CrossValidatedOperatorRidgeAnalysis(config, storePredictions = true)

    val actual = OneShotMvpaEngine.run(dataset, plan, response, analysis).toOption.get
    val explicitOperator = PatternOperator.fromMatrix(dataset.explicitPatterns.toOption.get).toOption.get
    val expected = MvpaEngine
      .runSource(
        PatternSource.fromOperator(explicitOperator),
        plan,
        response,
        analysis,
        Some(folds)
      )
      .toOption
      .get

    assertEquals(actual.analysisName, "cv_operator_ridge")
    assertEquals(actual.failures, Vector.empty)
    assertEquals(actual.successes.map(_.roiId.value), Vector(11, 12))
    actual.successes.zip(expected.successes).foreach: (observed, reference) =>
      assertEqualsDouble(
        observed.metrics("TargetMse").get,
        reference.metrics("TargetMse").get,
        1e-9
      )
      assertEqualsDouble(
        observed.metrics("TargetArgmaxAccuracy").get,
        reference.metrics("TargetArgmaxAccuracy").get,
        1e-12
      )
      (observed.payload, reference.payload) match
        case (Some(RoiPayload.OperatorRidge(left)), Some(RoiPayload.OperatorRidge(right))) =>
          assertMatrixClose(left.prediction.get.scores, right.prediction.get.scores, absTol = 1e-8, relTol = 1e-8)
          assertEquals(left.prediction.get.predicted, right.prediction.get.predicted)
          assertEquals(left.receipt.executionMode, OperatorRidgeExecutionMode.OperatorProducts)
          assertEquals(left.receipt.targetKind, ClassMembershipKind.HardLabels)
          assert(left.receipt.forwardApplications > 0)
          assert(left.receipt.transposeApplications > 0)
        case other => fail(s"unexpected operator-ridge payloads: $other")

  test("one-shot constructors reject incompatible run and feature structure"):
    val runs = runBlocks()
    val duplicate = OneShotDataset.make(Vector(runs.head, runs.head))
    val mismatchedRun = makeRun(
      "run-x",
      "x",
      runResponses.head,
      featureIndices = Vector(FeatureIndex(0), FeatureIndex(1), FeatureIndex(2), FeatureIndex(99))
    )
    val mismatchedAxis = OneShotDataset.make(Vector(runs.head, mismatchedRun))
    val shortResponse = ResponseBlock.unsafe(fromRows(runResponses.head.take(3)))
    val timepointMismatch = RunTrialReadout.make(
      RunId("short"),
      shortResponse,
      runs.head.readout
    )

    assertEquals(OneShotDataset.make(Vector.empty).left.toOption, Some(OneShotMvpaError.EmptyRuns))
    duplicate.left.toOption match
      case Some(OneShotMvpaError.DuplicateRuns(ids)) => assertEquals(ids.map(_.value), Vector("run-1"))
      case other                                     => fail(s"unexpected duplicate-run result: $other")
    assertEquals(
      mismatchedAxis.left.toOption,
      Some(
        OneShotMvpaError.FeatureAxisMismatch(
          runId = RunId("run-x"),
          expected = Vector.tabulate(4)(FeatureIndex.apply),
          actual = Vector(FeatureIndex(0), FeatureIndex(1), FeatureIndex(2), FeatureIndex(99))
        )
      )
    )
    assertEquals(
      timepointMismatch.left.toOption,
      Some(OneShotMvpaError.TimepointMismatch(RunId("short"), readoutTimepoints = 4, responseTimepoints = 3))
    )
    assertEquals(
      OneShotDataset.make(Vector(runs.head)).toOption.get.leaveOneRunOut.left.toOption,
      Some(OneShotMvpaError.InsufficientRunsForCrossValidation(1))
    )

  private def oneShotDataset(): OneShotDataset =
    OneShotDataset.make(runBlocks()).toOption.get

  private def runBlocks(): Vector[RunTrialReadout] =
    runResponses.zipWithIndex.map: (rows, index) =>
      makeRun(s"run-${index + 1}", (index + 1).toString, rows)

  private def makeRun(
      runName: String,
      trialSuffix: String,
      responseRows: Vector[Vector[Double]],
      featureIndices: Vector[FeatureIndex] = Vector.empty
  ): RunTrialReadout =
    val trialDesign = fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 1.0),
        Vector(0.0, 1.0)
      )
    )
    val readout = LeastSquaresSeparate
      .unsafePrepare(LssTrialDesign.unsafe(trialDesign, Vector(s"a_$trialSuffix", s"b_$trialSuffix")))
      .trialReadout
      .toOption
      .get
    val response = ResponseBlock.unsafe(fromRows(responseRows))
    val run =
      if featureIndices.isEmpty then
        RunTrialReadout.make(
          runId = RunId(runName),
          timeSeries = response,
          readout = readout
        )
      else
        RunTrialReadout.make(
          runId = RunId(runName),
          timeSeries = response,
          readout = readout,
          featureIndices = featureIndices
        )
    run.toOption.get

  private val runResponses: Vector[Vector[Vector[Double]]] =
    Vector(
      Vector(
        Vector(2.0, 2.0, 0.2, -0.1),
        Vector(2.2, 1.8, 0.0, 0.1),
        Vector(-2.0, -2.0, 0.1, 0.0),
        Vector(-2.2, -1.8, -0.1, 0.2)
      ),
      Vector(
        Vector(1.8, 2.2, 0.2, 0.0),
        Vector(2.0, 2.0, 0.0, -0.2),
        Vector(-1.8, -2.2, -0.2, 0.0),
        Vector(-2.0, -2.0, 0.0, 0.2)
      ),
      Vector(
        Vector(2.1, 1.9, 0.1, 0.1),
        Vector(1.9, 2.1, -0.1, -0.1),
        Vector(-2.1, -1.9, 0.1, -0.1),
        Vector(-1.9, -2.1, -0.1, 0.1)
      )
    )

  private def expectedPatterns(): DMat =
    fromRows(
      Vector(
        Vector(2.1, 1.9, 0.1, 0.0),
        Vector(-2.1, -1.9, 0.0, 0.1),
        Vector(1.9, 2.1, 0.1, -0.1),
        Vector(-1.9, -2.1, -0.1, 0.1),
        Vector(2.0, 2.0, 0.0, 0.0),
        Vector(-2.0, -2.0, 0.0, 0.0)
      )
    )

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    require(rows.nonEmpty, "test matrix rows must be non-empty")
    val cols = rows.head.length
    require(rows.forall(_.length == cols), "test matrix rows must have equal length")
    Matrix.tabulate(rows.length, cols)((row, col) => rows(row)(col))

  private def frobeniusDot(left: DMat, right: DMat): Double =
    require(left.rows == right.rows && left.cols == right.cols, "dot-product shape mismatch")
    var total = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        total += left(row, col) * right(row, col)
        col += 1
      row += 1
    total

  private def assertMatrixClose(actual: DMat, expected: DMat, absTol: Double = 1e-10, relTol: Double = 1e-10): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        val difference = math.abs(actual(row, col) - expected(row, col))
        val scale = math.max(math.abs(actual(row, col)), math.abs(expected(row, col)))
        assert(difference <= absTol + relTol * scale, s"matrix mismatch at ($row, $col)")
        col += 1
      row += 1
