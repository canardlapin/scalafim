package scalafim.fmri.mvpa

import gale.linalg.{CholeskyOptions, DMat, LinearOperator, Matrix}

class OperatorRidgeSuite extends munit.FunSuite:

  private val labels = Vector("a", "b", "c", "a", "b", "c", "a", "b", "c", "a", "b", "c")
    .map(ClassLabel.apply)
  private val patterns = fromRows(
    Vector(
      Vector(2.0, 0.8, 0.2, 1.0),
      Vector(-1.8, -0.9, 0.4, -0.5),
      Vector(0.1, 2.1, -1.0, 0.3),
      Vector(2.2, 1.1, 0.0, 0.8),
      Vector(-2.1, -1.2, 0.1, -0.7),
      Vector(-0.2, 1.8, -1.2, 0.5),
      Vector(1.9, 0.9, 0.3, 1.2),
      Vector(-1.9, -1.0, 0.2, -0.4),
      Vector(0.2, 2.2, -0.8, 0.4),
      Vector(2.1, 1.0, 0.1, 0.9),
      Vector(-2.2, -0.8, 0.3, -0.6),
      Vector(0.0, 1.9, -1.1, 0.2)
    )
  )
  private val patternMatrix =
    PatternMatrix(
      patterns,
      Vector.tabulate(patterns.rows)(SampleIndex.apply),
      Vector.tabulate(patterns.cols)(FeatureIndex.apply)
    )
  private val operator = PatternOperator.fromMatrix(patternMatrix).toOption.get
  private val targets = ClassMembership.hard(labels).toOption.get
  private val folds = FoldPlan.leaveOneBlockOut(Vector.tabulate(12)(_ / 3)).toOption.get
  private val config = OperatorRidgeConfig.unsafe(penalty = 0.7, tolerance = 1e-12, maxIterations = 2000)

  test("membership and solver smart constructors reject invalid states") {
    val hard = ClassMembership.hard(Vector(ClassLabel("a"), ClassLabel("b"), ClassLabel("a"))).toOption.get
    val soft = ClassMembership
      .simplex(
        Vector(ClassLabel("a"), ClassLabel("b")),
        fromRows(Vector(Vector(0.8, 0.2), Vector(0.3, 0.7)))
      )
      .toOption
      .get

    assertEquals(hard.kind, ClassMembershipKind.HardLabels)
    assertEquals(hard.classes.map(_.value), Vector("a", "b"))
    assertMatrixClose(hard.values, fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 0.0))))
    assertEquals(soft.kind, ClassMembershipKind.Simplex)
    assertEquals(Response.Probabilistic(soft).length, 2)
    assertEquals(Response.Probabilistic(soft).subset(Vector(1)).length, 1)

    assertEquals(
      ClassMembership
        .simplex(Vector(ClassLabel("a"), ClassLabel("b")), fromRows(Vector(Vector(0.8, 0.3))))
        .left
        .toOption,
      Some(MvpaError.InvalidClassMembership("membership row 0 sums to 1.1 instead of 1"))
    )
    assertEquals(
      OperatorRidgeConfig(penalty = 0.0).left.toOption,
      Some(OperatorRidgeError.InvalidPenalty(0.0))
    )
    OperatorRidgeConfig(tolerance = Double.NaN).left.toOption match
      case Some(OperatorRidgeError.InvalidTolerance(value)) => assert(value.isNaN)
      case other                                            => fail(s"unexpected tolerance result: $other")
    assertEquals(
      OperatorRidgeConfig(maxIterations = 0).left.toOption,
      Some(OperatorRidgeError.InvalidIterationLimit(0))
    )
  }

  test("analytic centered ridge fit has an unpenalized intercept and operator receipt") {
    val x = PatternMatrix.fromRows(
      Vector(Vector(-1.0, 5.0), Vector(1.0, 5.0), Vector(-1.0, 5.0), Vector(1.0, 5.0))
    )
    val xOperator = PatternOperator.fromMatrix(x).toOption.get
    val y = ClassMembership
      .hard(Vector(ClassLabel("a"), ClassLabel("b"), ClassLabel("a"), ClassLabel("b")))
      .toOption
      .get
    val model = OperatorRidge
      .fit(xOperator, y, OperatorRidgeConfig.unsafe(penalty = 1.0, tolerance = 1e-13))
      .toOption
      .get
    val prediction = model.predict(xOperator).toOption.get

    assertEqualsDouble(model.coefficients(0, 0), -0.4, 1e-10)
    assertEqualsDouble(model.coefficients(0, 1), 0.4, 1e-10)
    assertEqualsDouble(model.coefficients(1, 0), 0.0, 1e-10)
    assertEqualsDouble(model.coefficients(1, 1), 0.0, 1e-10)
    assertEqualsDouble(model.intercepts(0), 0.5, 1e-10)
    assertEqualsDouble(model.intercepts(1), 0.5, 1e-10)
    assertMatrixClose(
      prediction.scores,
      fromRows(Vector(Vector(0.9, 0.1), Vector(0.1, 0.9), Vector(0.9, 0.1), Vector(0.1, 0.9))),
      absTol = 1e-9,
      relTol = 1e-9
    )
    assertEquals(prediction.predicted.map(_.value), Vector("a", "b", "a", "b"))
    assertEqualsDouble(model.receipt.penalty.value, 1.0, 0.0)
    assertEqualsDouble(model.receipt.tolerance.value, 1e-13, 0.0)
    assertEquals(model.receipt.maxIterations.value, 2000)
    assertEquals(model.receipt.patternProvenance.origin, PatternOperatorOrigin.Dense)
    assertEquals(
      model.receipt.forwardApplications,
      model.receipt.classFits.map(_.iterations).sum
    )
    assertEquals(
      model.receipt.transposeApplications,
      1 + model.classes.length + model.receipt.classFits.map(_.iterations).sum
    )
  }

  test("operator LSQR coefficients and scores match an independent dense normal-equation oracle") {
    folds.folds.foreach: fold =>
      val trainPositions = fold.train.map(_.value)
      val testPositions = fold.test.map(_.value)
      val trainOperator = operator.selectRows(fold.train).toOption.get
      val testOperator = operator.selectRows(fold.test).toOption.get
      val trainTargets = targets.selectPositions(trainPositions)
      val targetBlock = ClassMembership.simplex(targets.classes, trainTargets).toOption.get
      val fitted = OperatorRidge.fit(trainOperator, targetBlock, config).toOption.get
      val predicted = fitted.predict(testOperator).toOption.get
      val oracle = denseRidge(
        selectRows(patterns, trainPositions),
        trainTargets,
        config.penalty.value
      )
      val expectedScores = denseScores(selectRows(patterns, testPositions), oracle._1, oracle._2)

      assertMatrixClose(fitted.coefficients, oracle._1, absTol = 1e-7, relTol = 1e-7)
      assertVectorClose(fitted.intercepts, oracle._2, absTol = 1e-7, relTol = 1e-7)
      assertMatrixClose(predicted.scores, expectedScores, absTol = 1e-7, relTol = 1e-7)
  }

  test("feature translation and permutation preserve fitted class scores") {
    val baseline = OperatorRidge.fit(operator, targets, config).toOption.get.predict(operator).toOption.get
    val translatedRows = Vector.tabulate(patterns.rows): row =>
      Vector.tabulate(patterns.cols): col =>
        patterns(row, col) + Vector(10.0, -3.0, 7.5, 2.0)(col)
    val translatedMatrix = PatternMatrix.fromRows(translatedRows)
    val translated = PatternOperator.fromMatrix(translatedMatrix).toOption.get
    val translatedScores = OperatorRidge.fit(translated, targets, config).toOption.get.predict(translated).toOption.get
    val permutation = Vector(2, 0, 3, 1)
    val permutedRows = Vector.tabulate(patterns.rows): row =>
      permutation.map(col => patterns(row, col))
    val permutedMatrix = PatternMatrix.fromRows(permutedRows)
    val permuted = PatternOperator.fromMatrix(permutedMatrix).toOption.get
    val permutedScores = OperatorRidge.fit(permuted, targets, config).toOption.get.predict(permuted).toOption.get

    assertMatrixClose(translatedScores.scores, baseline.scores, absTol = 1e-7, relTol = 1e-7)
    assertMatrixClose(permutedScores.scores, baseline.scores, absTol = 1e-7, relTol = 1e-7)
  }

  test("soft simplex targets obey the affine target law under cross-validation") {
    val hardResult = OperatorRidge.crossValidate(operator, targets, folds, config).toOption.get
    val softenedValues = Matrix.tabulate(targets.samples, targets.classCount): (sample, klass) =>
      0.7 * targets.values(sample, klass) + 0.1
    val softened = ClassMembership.simplex(targets.classes, softenedValues).toOption.get
    val softResult = OperatorRidge.crossValidate(operator, softened, folds, config).toOption.get

    assertEquals(softResult.receipt.targetKind, ClassMembershipKind.Simplex)
    var row = 0
    while row < hardResult.prediction.scores.rows do
      var klass = 0
      while klass < hardResult.prediction.scores.cols do
        assertEqualsDouble(
          softResult.prediction.scores(row, klass),
          0.7 * hardResult.prediction.scores(row, klass) + 0.1,
          1e-7
        )
        klass += 1
      row += 1
    assertEquals(softResult.prediction.predicted, hardResult.prediction.predicted)

    val plan = FeatureSetPlan
      .regional("soft-ridge", Vector(FeatureSet.unsafe(RoiId(90), Vector(0, 1, 2, 3))))
      .toOption
      .get
    val engineResult = MvpaEngine
      .runSource(
        PatternSource.fromOperator(operator),
        plan,
        Response.Probabilistic(softened),
        CrossValidatedOperatorRidgeAnalysis(config),
        Some(folds)
      )
      .toOption
      .get
    engineResult.successes.head.payload match
      case Some(RoiPayload.OperatorRidge(payload)) =>
        assertEquals(payload.receipt.targetKind, ClassMembershipKind.Simplex)
        assertEquals(payload.prediction, None)
      case other => fail(s"unexpected soft-target payload: $other")
  }

  test("held-out target perturbations cannot change fitted fold scores") {
    val heldOutFold = FoldPlan.unsafe(
      Vector(Fold.unsafe("held-out", 3 until patterns.rows, 0 until 3)),
      samples = patterns.rows
    )
    val perturbedValues = Matrix.tabulate(targets.samples, targets.classCount): (sample, klass) =>
      if sample < 3 then targets.values(sample, (klass + 1) % targets.classCount)
      else targets.values(sample, klass)
    val perturbedTargets = ClassMembership.simplex(targets.classes, perturbedValues).toOption.get
    val baseline = OperatorRidge.crossValidate(operator, targets, heldOutFold, config).toOption.get
    val perturbed = OperatorRidge.crossValidate(operator, perturbedTargets, heldOutFold, config).toOption.get

    assertMatrixClose(perturbed.prediction.scores, baseline.prediction.scores, absTol = 1e-10, relTol = 1e-10)
    assertEquals(perturbed.prediction.predicted, baseline.prediction.predicted)
    assert(perturbed.targetMse > baseline.targetMse)
    assert(perturbed.targetArgmaxAccuracy < baseline.targetArgmaxAccuracy)
  }

  test("canonical operator analysis returns scores and execution receipts without a dense adapter") {
    val plan = FeatureSetPlan
      .regional("operator-ridge", Vector(FeatureSet.unsafe(RoiId(91), Vector(0, 1, 2, 3))))
      .toOption
      .get
    val response = Response.Categorical(labels)
    val result = MvpaEngine
      .runSource(
        PatternSource.fromOperator(operator),
        plan,
        response,
        CrossValidatedOperatorRidgeAnalysis(config, storePredictions = true),
        Some(folds)
      )
      .toOption
      .get
    val success = result.successes.head

    assertEquals(result.analysisName, "cv_operator_ridge")
    assertEquals(result.failures, Vector.empty)
    assert(success.metrics("TargetMse").exists(_.isFinite))
    assertEqualsDouble(success.metrics("TargetArgmaxAccuracy").get, 1.0, 1e-12)
    success.payload match
      case Some(RoiPayload.OperatorRidge(payload)) =>
        assert(payload.prediction.nonEmpty)
        assertEquals(payload.receipt.executionMode, OperatorRidgeExecutionMode.OperatorProducts)
        assertEquals(payload.receipt.folds.map(_.foldId), Vector("0", "1", "2", "3"))
        assert(payload.receipt.forwardApplications > 0)
        assert(payload.receipt.transposeApplications > 0)
      case other => fail(s"unexpected operator-ridge payload: $other")
  }

  test("missing fold classes, non-convergence, poisoned operators, and feature mismatch are typed failures") {
    val missingFold = FoldPlan
      .unsafe(Vector(Fold.unsafe("missing-b", Seq(0, 3, 6, 9), Seq(1, 2))), samples = patterns.rows)
    val missingClass = OperatorRidge.crossValidate(operator, targets, missingFold, config)
    assertEquals(
      missingClass.left.toOption,
      Some(OperatorRidgeError.MissingTrainingClass("missing-b", ClassLabel("b")))
    )

    val oneStep = OperatorRidge.fit(
      operator,
      targets,
      OperatorRidgeConfig.unsafe(penalty = 1e-4, tolerance = 1e-30, maxIterations = 1)
    )
    oneStep.left.toOption match
      case Some(OperatorRidgeError.DidNotConverge(_, _, iterations, residual)) =>
        assertEquals(iterations, 1)
        assert(residual.isFinite)
      case other => fail(s"unexpected one-step solver result: $other")

    val poisonedLinear = LinearOperator.fromFunctions(patterns.rows, patterns.cols)(
      (_, output) =>
        var index = 0
        while index < output.length do
          output(index) = Double.NaN
          index += 1,
      (_, output) =>
        var index = 0
        while index < output.length do
          output(index) = Double.NaN
          index += 1
    )
    val poisoned = PatternOperator
      .fromOperator(
        SampleAxis(patterns.rows).toOption.get,
        Vector.tabulate(patterns.cols)(FeatureIndex.apply),
        poisonedLinear,
        PatternOperatorProvenance.composed
      )
      .toOption
      .get
    assert(OperatorRidge.fit(poisoned, targets, config).left.toOption.exists {
      case OperatorRidgeError.NumericalFailure(_, _) => true
      case _                                         => false
    })

    val model = OperatorRidge.fit(operator, targets, config).toOption.get
    val wrongFeatureMatrix =
      PatternMatrix.fromRows(Vector.fill(patterns.rows)(Vector(1.0)))
    val wrongFeatures =
      PatternOperator.fromMatrix(wrongFeatureMatrix).toOption.get
    model.predict(wrongFeatures).left.toOption match
      case Some(OperatorRidgeError.FeatureAxisMismatch(expected, actual)) =>
        assertEquals(expected.map(_.value), Vector(0, 1, 2, 3))
        assertEquals(actual.map(_.value), Vector(0))
      case other => fail(s"unexpected feature-axis result: $other")
  }

  private def denseRidge(
      x: DMat,
      y: DMat,
      penalty: Double
  ): (DMat, Vector[Double]) =
    val xMeans = columnMeans(x)
    val yMeans = columnMeans(y)
    val centeredX = center(x, xMeans)
    val centeredY = center(y, yMeans)
    val normal = Matrix.newBuilder(x.cols, x.cols)
    var left = 0
    while left < x.cols do
      var right = 0
      while right < x.cols do
        var total = 0.0
        var sample = 0
        while sample < x.rows do
          total += centeredX(sample, left) * centeredX(sample, right)
          sample += 1
        normal(left, right) = total + (if left == right then penalty else 0.0)
        right += 1
      left += 1
    val rhs = centeredX.t * centeredY
    val weights = normal.result().cholesky(CholeskyOptions(1e-12)).toOption.get.solve(rhs).toOption.get
    val intercepts = Vector.tabulate(y.cols): klass =>
      var contribution = 0.0
      var feature = 0
      while feature < x.cols do
        contribution += xMeans(feature) * weights(feature, klass)
        feature += 1
      yMeans(klass) - contribution
    (weights, intercepts)

  private def denseScores(x: DMat, weights: DMat, intercepts: Vector[Double]): DMat =
    val raw = x * weights
    Matrix.tabulate(raw.rows, raw.cols): (row, col) =>
      raw(row, col) + intercepts(col)

  private def selectRows(matrix: DMat, positions: IndexedSeq[Int]): DMat =
    Matrix.tabulate(positions.length, matrix.cols): (row, col) =>
      matrix(positions(row), col)

  private def columnMeans(matrix: DMat): Vector[Double] =
    Vector.tabulate(matrix.cols): col =>
      var total = 0.0
      var row = 0
      while row < matrix.rows do
        total += matrix(row, col)
        row += 1
      total / matrix.rows

  private def center(matrix: DMat, means: Vector[Double]): DMat =
    Matrix.tabulate(matrix.rows, matrix.cols): (row, col) =>
      matrix(row, col) - means(col)

  private def fromRows(rows: Seq[Seq[Double]]): DMat =
    require(rows.nonEmpty, "test matrix rows must be non-empty")
    val cols = rows.head.length
    require(rows.forall(_.length == cols), "test matrix rows must have equal length")
    Matrix.tabulate(rows.length, cols)((row, col) => rows(row)(col))

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      absTol: Double,
      relTol: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    var index = 0
    while index < actual.length do
      val difference = math.abs(actual(index) - expected(index))
      val scale = math.max(math.abs(actual(index)), math.abs(expected(index)))
      assert(difference <= absTol + relTol * scale, s"vector mismatch at $index")
      index += 1

  private def assertMatrixClose(
      actual: DMat,
      expected: DMat,
      absTol: Double = 1e-10,
      relTol: Double = 1e-10
  ): Unit =
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
