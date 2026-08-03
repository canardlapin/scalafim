package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}

class TrialReadoutSuite extends munit.FunSuite:

  test("LSS trial readout matches the independent fMRILSS fixture and owns coefficient identity") {
    val (trials, fixed, response) = lssFixture()
    val prepared = LeastSquaresSeparate.unsafePrepare(
      LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3")),
      LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
    )
    val readout = prepared.trialReadout.toOption.get
    val coefficients = readout.forward(ResponseBlock.unsafe(response)).toOption.get
    val expected = GaleTestMatrix.fromRows(
      Vector(
        Vector(-2.00714771389244, 0.18904127763313),
        Vector(1.8141791857713, 4.7225165628497),
        Vector(0.717973602484473, 3.83721532091097)
      )
    )

    assertEquals(readout.timepoints, 8)
    assertEquals(readout.trials, 3)
    assertEquals(readout.trialNames, Vector("trial_1", "trial_2", "trial_3"))
    assertEquals(readout.axis.estimability, Vector.fill(3)(TrialEstimability.Estimable))
    assertEquals(readout.receipt, TrialReadoutReceipt(TrialReadoutMethod.LeastSquaresSeparate, fixedRank = 2))
    assertMatrixClose(coefficients.value, expected)

    val fit = prepared.fit(ResponseBlock.unsafe(response)).toOption.get
    assertMatrixClose(fit.coefficients.value, coefficients.value, absTol = 1e-12, relTol = 1e-12)
  }

  test("readout folds fixed nuisance projection into both forward and adjoint maps") {
    val (trials, fixed, response) = lssFixture()
    val prepared = LeastSquaresSeparate.unsafePrepare(
      LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3")),
      LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
    )
    val readout = prepared.trialReadout.toOption.get
    val nuisanceContaminated = addFixedSignal(response, fixed)
    val cleanCoefficients = readout.forward(ResponseBlock.unsafe(response)).toOption.get
    val contaminatedCoefficients = readout.forward(ResponseBlock.unsafe(nuisanceContaminated)).toOption.get

    assertMatrixClose(contaminatedCoefficients.value, cleanCoefficients.value, absTol = 1e-10, relTol = 1e-10)

    val trialScores = CoefficientBlock(
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.4, -0.8),
          Vector(1.2, 0.3),
          Vector(-0.7, 1.5)
        )
      )
    )
    val timeScores = readout.adjoint(trialScores).toOption.get

    var fixedColumn = 0
    while fixedColumn < fixed.cols do
      var scoreColumn = 0
      while scoreColumn < timeScores.voxels do
        var innerProduct = 0.0
        var timepoint = 0
        while timepoint < fixed.rows do
          innerProduct += fixed(timepoint, fixedColumn) * timeScores.value(timepoint, scoreColumn)
          timepoint += 1
        assertEqualsDouble(innerProduct, 0.0, 1e-10)
        scoreColumn += 1
      fixedColumn += 1
  }

  test("readout satisfies the Frobenius adjoint law") {
    val (trials, fixed, response) = lssFixture()
    val readout = LeastSquaresSeparate
      .unsafePrepare(
        LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3")),
        LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
      )
      .trialReadout
      .toOption
      .get
    val trialProbe = CoefficientBlock(
      GaleTestMatrix.fromRows(
        Vector(
          Vector(0.5, -1.0),
          Vector(1.5, 0.25),
          Vector(-0.75, 2.0)
        )
      )
    )

    val forward = readout.forward(ResponseBlock.unsafe(response)).toOption.get.value
    val adjoint = readout.adjoint(trialProbe).toOption.get.value

    assertEqualsDouble(frobeniusDot(forward, trialProbe.value), frobeniusDot(response, adjoint), 1e-10)
  }

  test("readout commutes with downstream feature mixing") {
    val (trials, fixed, response) = lssFixture()
    val readout = LeastSquaresSeparate
      .unsafePrepare(
        LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3")),
        LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
      )
      .trialReadout
      .toOption
      .get
    val weights = GaleTestMatrix.fromRows(
      Vector(
        Vector(0.5, -1.0, 0.25),
        Vector(1.5, 0.75, -0.4)
      )
    )

    val fused = readout.forward(ResponseBlock.unsafe(multiply(response, weights))).toOption.get.value
    val explicit = multiply(readout.forward(ResponseBlock.unsafe(response)).toOption.get.value, weights)

    assertMatrixClose(fused, explicit, absTol = 1e-10, relTol = 1e-10)
  }

  test("one prepared readout is cached and applies a response batch columnwise") {
    val (trials, fixed, response) = lssFixture()
    val prepared = LeastSquaresSeparate.unsafePrepare(
      LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3")),
      LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
    )
    val first = prepared.trialReadout.toOption.get
    val second = prepared.trialReadout.toOption.get
    val batched = first.forward(ResponseBlock.unsafe(response)).toOption.get.value

    assert(first eq second)
    var column = 0
    while column < response.cols do
      val singleResponse = Matrix.tabulate(response.rows, 1)((row, _) => response(row, column))
      val single = first.forward(ResponseBlock.unsafe(singleResponse)).toOption.get.value
      var trial = 0
      while trial < first.trials do
        assertEqualsDouble(batched(trial, column), single(trial, 0), 1e-12)
        trial += 1
      column += 1
  }

  test("zero trial regressors produce a zero readout row and zero adjoint contribution") {
    val trials = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      )
    )
    val readout = LeastSquaresSeparate
      .unsafePrepare(LssTrialDesign.unsafe(trials, Vector("active", "zero")))
      .trialReadout
      .toOption
      .get

    assertEquals(readout.axis.estimability, Vector(TrialEstimability.Estimable, TrialEstimability.ZeroRegressor))

    val zeroOnlyScore = CoefficientBlock(GaleTestMatrix.fromRows(Vector(Vector(0.0), Vector(7.0))))
    val timeScore = readout.adjoint(zeroOnlyScore).toOption.get.value
    var row = 0
    while row < timeScore.rows do
      assertEqualsDouble(timeScore(row, 0), 0.0, 1e-12)
      row += 1
  }

  test("trial readout and trial coefficient axes reject malformed inputs with typed errors") {
    val trials = GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(0.0, 1.0)))

    assertEquals(
      LssTrialDesign.fromMatrix(trials, Vector("trial", " trial ")).left.toOption,
      Some(FitError.UnsupportedLssDesign("trial names must be unique"))
    )
    assertEquals(
      LssTrialDesign.fromMatrix(trials, Vector("trial", "  ")).left.toOption,
      Some(FitError.UnsupportedLssDesign("trial names must be non-empty"))
    )
    assertEquals(
      TrialCoefficientAxis
        .fromNames(Vector("trial"), Vector.empty)
        .left
        .toOption,
      Some(FitError.InvalidFitAxis("trial coefficient axis", "estimability length 0 does not match id length 1"))
    )

    val readout = LeastSquaresSeparate
      .unsafePrepare(LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2")))
      .trialReadout
      .toOption
      .get
    val shortResponse = ResponseBlock.unsafe(GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))))
    val wrongTrialRows = CoefficientBlock(GaleTestMatrix.fromRows(Vector(Vector(1.0))))
    val nonFiniteScores = CoefficientBlock(GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(Double.NaN))))

    assertEquals(readout.forward(shortResponse).left.toOption, Some(FitError.RowMismatch(2, 3)))
    assertEquals(
      readout.adjoint(wrongTrialRows).left.toOption,
      Some(FitError.InvalidFitAxis("trial score block", "rows 1 do not match readout trials 2"))
    )
    assertEquals(readout.adjoint(nonFiniteScores).left.toOption, Some(FitError.NonFiniteInput("trial score block")))
  }

  private def lssFixture(): (DMat, DMat, DMat) =
    val trials = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0),
        Vector(0.8, 0.2, 0.0),
        Vector(0.1, 1.0, 0.0),
        Vector(0.0, 0.6, 0.3),
        Vector(0.0, 0.2, 1.0),
        Vector(0.4, 0.0, 0.6),
        Vector(0.0, 0.0, 0.8),
        Vector(0.2, 0.5, 0.1)
      )
    )
    val fixed = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, -3.5),
        Vector(1.0, -2.5),
        Vector(1.0, -1.5),
        Vector(1.0, -0.5),
        Vector(1.0, 0.5),
        Vector(1.0, 1.5),
        Vector(1.0, 2.5),
        Vector(1.0, 3.5)
      )
    )
    val response = GaleTestMatrix.fromRows(
      Vector(
        Vector(5.2, -1.0),
        Vector(4.7, 0.5),
        Vector(6.4, 1.8),
        Vector(7.3, 1.0),
        Vector(8.1, 2.4),
        Vector(6.9, 0.7),
        Vector(8.8, 2.1),
        Vector(7.9, 1.5)
      )
    )
    (trials, fixed, response)

  private def addFixedSignal(response: DMat, fixed: DMat): DMat =
    Matrix.tabulate(response.rows, response.cols): (row, col) =>
      val interceptWeight = if col == 0 then 4.0 else -2.5
      val trendWeight = if col == 0 then -0.7 else 1.1
      response(row, col) + fixed(row, 0) * interceptWeight + fixed(row, 1) * trendWeight

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

  private def multiply(left: DMat, right: DMat): DMat =
    require(left.cols == right.rows, "matrix-product shape mismatch")
    Matrix.tabulate(left.rows, right.cols): (row, col) =>
      var total = 0.0
      var inner = 0
      while inner < left.cols do
        total += left(row, inner) * right(inner, col)
        inner += 1
      total

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
        assert(
          difference <= absTol + relTol * scale,
          s"matrix mismatch at ($row, $col): actual=${actual(row, col)}, expected=${expected(row, col)}"
        )
        col += 1
      row += 1
