package scalafim.fmri.mvpa.fit

import scalafim.multivar.family.canonical.{LdaObjective, TraceRidgeFraction, TrialNuisanceDesign, WithinScatterPolicy}

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.mvpa.*

class SoftLdaSuite extends munit.FunSuite:

  test("pulled-back soft LDA agrees for operator and explicit pattern sources"):
    val patterns = patternMatrix()
    val operator = PatternOperator
      .fromOperator(
        SampleAxis(patterns.samples).toOption.get,
        patterns.featureIndices,
        patterns.value,
        PatternOperatorProvenance.composed
      )
      .toOption
      .get
    val targets = hardTargets()
    val config = defaultConfig()

    val pulledBack = SoftLda.crossValidate(operator, targets, folds(), config).toOption.get
    val explicit = SoftLda
      .crossValidate(PatternOperator.fromMatrix(patterns).toOption.get, targets, folds(), config)
      .toOption
      .get

    assertMatrixClose(pulledBack.prediction.probabilities, explicit.prediction.probabilities)
    assertEquals(pulledBack.prediction.predicted, explicit.prediction.predicted)
    assertEquals(pulledBack.receipt.executionMode, SoftLdaExecutionMode.PulledBackOperatorProducts)
    assert(pulledBack.receipt.folds.forall(_.patternProvenance.origin == PatternOperatorOrigin.Composed))
    assert(pulledBack.receipt.folds.forall(_.composedPatternInput))
    assertEquals(pulledBack.receipt.folds.length, 4)
    assert(pulledBack.receipt.folds.forall(_.fit.programFit.program.objective.label == "generalized-rayleigh"))
    assert(pulledBack.targetArgmaxAccuracy >= 0.9)

  test("simplex memberships remain soft through every training fold"):
    val operator = PatternOperator.fromMatrix(patternMatrix()).toOption.get
    val hard = hardTargets()
    val softenedValues = Matrix.tabulate(hard.samples, hard.classCount): (row, col) =>
      val trueClass = row % hard.classCount
      val secondary = (trueClass + 1 + (row / hard.classCount) % 2) % hard.classCount
      if col == trueClass then 0.65
      else if col == secondary then 0.30
      else 0.05
    val soft = ClassMembership.simplex(hard.classes, softenedValues).toOption.get

    val hardFit = SoftLda.crossValidate(operator, hard, folds(), defaultConfig()).toOption.get
    val softFit = SoftLda.crossValidate(operator, soft, folds(), defaultConfig()).toOption.get

    assertEquals(softFit.receipt.targetKind, ClassMembershipKind.Simplex)
    assertEquals(hardFit.receipt.targetKind, ClassMembershipKind.HardLabels)
    assert(matrixDistance(softFit.prediction.probabilities, hardFit.prediction.probabilities) > 1e-5)
    assert(softFit.targetMse >= 0.0)

  test("trial nuisance is selected within folds and remains distinct in the receipt"):
    val operator = PatternOperator.fromMatrix(patternMatrix()).toOption.get
    val nuisance = TrialNuisanceDesign.from(
      Matrix.tabulate(12, 1)((row, _) => (row / 3).toDouble - 1.5)
    ).toOption.get
    val adjustedConfig = defaultConfig().copy(
      objective = LdaObjective.TraceRatio,
      trialNuisance = Some(nuisance)
    )

    val adjusted = SoftLda.crossValidate(operator, hardTargets(), folds(), adjustedConfig).toOption.get

    assertEquals(adjusted.receipt.objective, LdaObjective.TraceRatio)
    assert(adjusted.receipt.folds.forall(_.trialNuisanceColumns == 1))
    assert(adjusted.receipt.folds.forall(_.trainingSamples.length == 9))
    assert(adjusted.receipt.folds.forall(_.testSamples.length == 3))
    adjusted.receipt.folds.foreach: receipt =>
      assertEquals(receipt.trainingSamples.toSet.intersect(receipt.testSamples.toSet), Set.empty)
      assertEquals(receipt.composedPatternInput, false)
      assertEquals(receipt.fit.programFit.program.objective.label, "trace-ratio")

  test("analysis adapter integrates soft LDA with the ordinary MVPA engine"):
    val patterns = patternMatrix()
    val plan = FeatureSetPlan
      .regional("soft-lda", Vector(FeatureSet.unsafe(RoiId(71), Vector(0, 1, 2))))
      .toOption
      .get
    val response = Response.Categorical(hardTargets().argmaxLabels)
    val analysis = CrossValidatedSoftLdaAnalysis(defaultConfig(), storePredictions = true)
    val result = MvpaEngine
      .runSource(
        PatternSource.fromOperator(PatternOperator.fromMatrix(patterns).toOption.get),
        plan,
        response,
        analysis,
        Some(folds())
      )
      .toOption
      .get

    assertEquals(result.analysisName, "cv_soft_lda")
    assertEquals(result.failures, Vector.empty)
    assertEquals(result.successes.length, 1)
    assert(result.successes.head.metrics("TargetArgmaxAccuracy").get >= 0.9)
    assert(result.successes.head.payload.exists(_.isInstanceOf[RoiPayload.Classification]))

  private def defaultConfig(): SoftLdaConfig =
    SoftLdaConfig(
      WithinScatterPolicy.FixedTraceScaledRidge(TraceRidgeFraction.unsafe(0.05))
    )

  private def hardTargets(): ClassMembership =
    ClassMembership
      .hard(Vector("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c").map(ClassLabel.apply))
      .toOption
      .get

  private def folds(): FoldPlan =
    FoldPlan.leaveOneBlockOut(Vector(0, 0, 0, 1, 1, 1, 2, 2, 2, 3, 3, 3)).toOption.get

  private def patternMatrix(): PatternMatrix =
    val values = fromRows(
      Seq(
        Seq(2.2, 0.1, 0.2), Seq(0.0, 2.0, -0.2), Seq(-2.0, -1.8, 0.1),
        Seq(2.0, -0.1, 0.0), Seq(0.2, 2.2, 0.1), Seq(-2.2, -2.0, -0.1),
        Seq(2.3, 0.2, -0.1), Seq(-0.1, 1.9, 0.2), Seq(-1.9, -2.1, 0.0),
        Seq(1.9, 0.0, 0.1), Seq(0.1, 2.1, -0.1), Seq(-2.1, -1.9, 0.2)
      )
    )
    PatternMatrix(
      values,
      Vector.tabulate(values.rows)(SampleIndex.apply),
      Vector.tabulate(values.cols)(FeatureIndex.apply)
    )

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def matrixDistance(left: DMat, right: DMat): Double =
    var squared = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        val difference = left(row, col) - right(row, col)
        squared += difference * difference
        col += 1
      row += 1
    Math.sqrt(squared)

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double = 1e-9): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1
