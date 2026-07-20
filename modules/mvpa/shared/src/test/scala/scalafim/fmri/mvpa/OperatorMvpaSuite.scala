package scalafim.fmri.mvpa

import scala.compiletime.testing.typeCheckErrors

import gale.linalg.DMat

class OperatorMvpaSuite extends munit.FunSuite:

  private val patterns = PatternMatrix.fromRows(
    Vector(
      Vector(2.0, 2.0, 0.0),
      Vector(-2.0, -2.0, 0.0),
      Vector(2.2, 1.8, 0.1),
      Vector(-2.1, -1.9, -0.1),
      Vector(1.9, 2.1, 0.0),
      Vector(-1.8, -2.2, 0.1),
      Vector(2.1, 2.0, -0.1),
      Vector(-2.0, -2.1, 0.0)
    )
  )
  private val response =
    Response.categorical(Vector("a", "b", "a", "b", "a", "b", "a", "b")).toOption.get
  private val folds =
    FoldPlan.leaveOneBlockOut(Vector(1, 1, 2, 2, 3, 3, 4, 4)).toOption.get
  private val featureSets = Vector(
    FeatureSet.unsafe(RoiId(1), Vector(0, 1)),
    FeatureSet.unsafe(RoiId(2), Vector(0, 1, 2))
  )
  private val operator = PatternOperator.fromMatrix(patterns).toOption.get
  private val operatorSource = PatternSource.fromOperator(operator)

  test("pattern source and ROI analysis representations must agree at compile time") {
    val errors = typeCheckErrors("""
      import scalafim.fmri.mvpa.*
      val source: OperatorPatternSource = ???
      val analysis: DenseRoiAnalysis = ???
      val featureSet: FeatureSet = ???
      val response: Response = ???
      MvpaTask.evaluate(source, featureSet, response, analysis)
    """)

    assert(errors.nonEmpty)
  }

  test("canonical MVPA engine accepts operator analyses without a parallel hierarchy") {
    val denseAnalysis =
      CrossValidatedClassifierAnalysis(RidgeLdaClassifier(gamma = 0.25), storePredictions = true)
    val operatorAnalysis = RoiAnalysis.materializing(denseAnalysis)

    val expected =
      MvpaEngine.run(patterns, featureSets, response, denseAnalysis, Some(folds)).toOption.get
    val actual =
      MvpaEngine
        .runSource(operatorSource, featureSets, response, operatorAnalysis, Some(folds))
        .toOption
        .get

    assertEquals(actual.analysisName, expected.analysisName)
    assertEquals(actual.failures, Vector.empty)
    assertEquals(actual.successes.map(_.roiId.value), expected.successes.map(_.roiId.value))
    actual.successes.zip(expected.successes).foreach { case (observed, reference) =>
      assertEqualsDouble(observed.metrics("Accuracy").get, reference.metrics("Accuracy").get, 1e-12)
      (observed.payload, reference.payload) match
        case (Some(RoiPayload.Classification(left)), Some(RoiPayload.Classification(right))) =>
          assertMatrixClose(left.probabilities, right.probabilities)
          assertEquals(left.predicted.map(_.value), right.predicted.map(_.value))
        case other => fail(s"unexpected ridge payloads: $other")
    }
  }

  test("operator analyses use the ordinary task and streaming boundaries") {
    val analysis = new OperatorRoiAnalysis:
      override val name: String = "operator_score_sum"
      override val minFeatures: Int = 1

      override def evaluate(
          roi: PatternOperator,
          context: RoiContext
      ): Either[MvpaError, RoiAnalysisResult] =
        val weights = GaleTestMatrix.fromRows(Vector.fill(roi.features)(Vector(1.0)))
        roi.applyTo(weights).map: scores =>
          var total = 0.0
          var row = 0
          while row < scores.rows do
            total += scores(row, 0)
            row += 1
          RoiAnalysisResult(MetricVector("ScoreSum" -> total), None)

    val taskOutcome =
      MvpaTask.evaluate(operatorSource, featureSets.head, response, analysis)
    val streamed =
      MvpaStream
        .outcomes(operatorSource, featureSets, response, analysis, None)
        .toOption
        .get
        .toVector

    taskOutcome match
      case RoiOutcome.Success(_, _, metrics, _) =>
        var expected = 0.0
        var row = 0
        while row < patterns.samples do
          expected += patterns.value(row, 0) + patterns.value(row, 1)
          row += 1
        assertEqualsDouble(metrics("ScoreSum").get, expected, 1e-12)
      case failure => fail(s"unexpected operator-analysis result: $failure")
    assertEquals(streamed.head, taskOutcome)
    assertEquals(streamed.map(_.id.value), Vector(1, 2))
  }

  test("operator execution preserves canonical fold and feature failure contracts") {
    val folded =
      RoiAnalysis.materializing(CrossValidatedClassifierAnalysis(RidgeLdaClassifier()))
    val missingFold =
      MvpaTask.evaluate(operatorSource, featureSets.head, response, folded)
    val missingFeature = MvpaTask.evaluate(
      operatorSource,
      FeatureSet.unsafe(RoiId(7), Vector(99)),
      response,
      RoiAnalysis.materializing(DenseAnalysisForTest),
      Some(folds)
    )
    val shortResponse = Response.categorical(Vector("a", "b")).toOption.get
    val invalidEngine = MvpaEngine.runSource(
      operatorSource,
      featureSets,
      shortResponse,
      RoiAnalysis.materializing(DenseAnalysisForTest),
      None
    )

    assertEquals(
      missingFold,
      RoiOutcome.Failure(
        featureSets.head.id,
        featureSets.head.featureIndices,
        MvpaError.InvalidClassifierInput("cross-validated classification requires a fold plan")
      )
    )
    missingFeature match
      case RoiOutcome.Failure(id, _, MvpaError.MissingFeature(errorId, feature)) =>
        assertEquals(id, RoiId(7))
        assertEquals(errorId, RoiId(7))
        assertEquals(feature.value, 99)
      case other => fail(s"unexpected missing-feature result: $other")
    assertEquals(invalidEngine.left.toOption, Some(MvpaError.ResponseLengthMismatch(8, 2)))
  }

  private object DenseAnalysisForTest extends DenseRoiAnalysis:
    override val name: String = "test_analysis"
    override val minFeatures: Int = 1

    override def evaluate(
        roi: PatternMatrix,
        context: RoiContext
    ): Either[MvpaError, RoiAnalysisResult] =
      Right(RoiAnalysisResult(MetricVector("unused" -> 0.0), None))

  private def assertMatrixClose(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), 1e-12)
        col += 1
      row += 1
