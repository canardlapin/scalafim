package scalafim.fmri.fit

import gale.linalg.DMat

class LssSuite extends munit.FunSuite:

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  test("LeastSquaresSeparate matches independent per-trial least-squares oracle with fixed regressors") {
    val trials = scalafim.fmri.fit.GaleTestMatrix.fromRows(
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
    val fixed = scalafim.fmri.fit.GaleTestMatrix.fromRows(
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
    val response = scalafim.fmri.fit.GaleTestMatrix.fromRows(
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

    val fit = LeastSquaresSeparate.unsafeFit(
      trials = LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3")),
      response = ResponseBlock.unsafe(response),
      fixed = LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
    )
    val expected = explicitLss(trials, response, fixed)
    val fmrilssExpected = scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(-2.00714771389244, 0.18904127763313),
        Vector(1.8141791857713, 4.7225165628497),
        Vector(0.717973602484473, 3.83721532091097)
      )
    )

    assertEquals(fit.trialNames, Vector("trial_1", "trial_2", "trial_3"))
    assertEquals(fit.diagnostics.fixedRank, 2)
    assertEquals(fit.diagnostics.zeroTrialRegressors, Vector.empty)
    assertEquals(fit.diagnostics.degenerateOtherRegressors, Vector.empty)
    assertEquals(fit.diagnostics.nonEstimableTrials, Vector.empty)
    assertMatrixClose(fit.coefficients.value, expected, tol = 1e-10)
    assertMatrixClose(fit.coefficients.value, fmrilssExpected, tol = 1e-10)
  }

  test("LeastSquaresSeparate handles a single trial as residualized simple regression") {
    val trials = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(1.0), Vector(0.0), Vector(0.5)))
    val fixed = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(1.0), Vector(1.0), Vector(1.0), Vector(1.0)))
    val response = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(4.0), Vector(5.0), Vector(2.0), Vector(3.0)))

    val fit = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(trials, Vector("only_trial")),
      ResponseBlock.unsafe(response),
      LssFixedDesign.unsafe(fixed, Vector("intercept"))
    )
    val expected = explicitLss(trials, response, fixed)

    assertEquals(fit.trials, 1)
    assertEquals(fit.coefficient("only_trial", 0), Some(fit.coefficients(0, 0)))
    assertMatrixClose(fit.coefficients.value, expected, tol = 1e-10)
  }

  test("LeastSquaresSeparate prepared design reuses fixed projection and trial workspace") {
    val trials = scalafim.fmri.fit.GaleTestMatrix.fromRows(
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
    val fixed = scalafim.fmri.fit.GaleTestMatrix.fromRows(
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
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
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
    )
    val trialDesign = LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3"))
    val fixedDesign = LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))

    val prepared = LeastSquaresSeparate.unsafePrepare(trialDesign, fixedDesign)
    val preparedFit = prepared.fit(response).toOption.get
    val directFit = LeastSquaresSeparate.unsafeFit(trialDesign, response, fixedDesign)

    assertEquals(prepared.fixedRank, 2)
    assertEquals(prepared.trialNames, Vector("trial_1", "trial_2", "trial_3"))
    assertEquals(prepared.workspaces.map(_.status).distinct, Vector(LssTrialStatus.Active))
    assert(prepared.workspaces.forall(_.denominator > 0.0))
    assertMatrixClose(preparedFit.coefficients.value, directFit.coefficients.value, tol = 1e-12)
  }

  test("LeastSquaresSeparate reports zero trial and degenerate other regressors") {
    val trials = scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 0.0),
        Vector(1.0, 0.0),
        Vector(0.0, 0.0),
        Vector(0.0, 0.0)
      )
    )
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(2.0), Vector(3.0), Vector(4.0), Vector(5.0))))

    val fit = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(trials, Vector("active", "zero")),
      response
    )

    assertEquals(fit.diagnostics.zeroTrialRegressors, Vector("zero"))
    assertEquals(fit.diagnostics.degenerateOtherRegressors, Vector("active"))
    assertEquals(fit.diagnostics.nonEstimableTrials, Vector.empty)
    assertEqualsDouble(fit.coefficients(1, 0), 0.0, 1e-12)
  }

  test("LeastSquaresSeparate returns typed metadata errors from safe constructors") {
    val trialDesign = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(0.0)))
    val fixedDesign = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0, 0.0), Vector(1.0, 1.0)))

    assertEquals(
      LssTrialDesign.fromMatrix(trialDesign, Vector("a", "b")).left.toOption,
      Some(FitError.UnsupportedLssDesign("trial names length 2 must match trial-design columns 1"))
    )
    assertEquals(
      LssFixedDesign.fromMatrix(fixedDesign, Vector("intercept")).left.toOption,
      Some(FitError.UnsupportedLssDesign("fixed-design column names length 1 must match columns 2"))
    )
  }

  test("LeastSquaresSeparate reports trials absorbed by fixed regressors without inventing betas") {
    val trials = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(1.0), Vector(1.0), Vector(1.0)))
    val fixed = scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(1.0), Vector(1.0), Vector(1.0)))
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(2.0), Vector(3.0), Vector(4.0), Vector(5.0))))

    val fit = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(trials, Vector("absorbed")),
      response,
      LssFixedDesign.unsafe(fixed, Vector("intercept"))
    )

    assertEquals(fit.diagnostics.fixedRank, 1)
    assertEquals(fit.diagnostics.zeroTrialRegressors, Vector("absorbed"))
    assertEquals(fit.diagnostics.nonEstimableTrials, Vector.empty)
    assertEqualsDouble(fit.coefficients(0, 0), 0.0, 1e-12)
  }

  test("LeastSquaresSeparate rejects collinear trial and other-trial regressors") {
    val trials = scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 1.0),
        Vector(0.0, 0.0),
        Vector(1.0, 1.0),
        Vector(0.0, 0.0)
      )
    )
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0))))

    val result = LeastSquaresSeparate.fit(
      LssTrialDesign.unsafe(trials, Vector("trial_a", "trial_b")),
      response
    )

    assertEquals(result.left.toOption, Some(FitError.NonEstimableLssTrials(Vector("trial_a", "trial_b"))))
  }

  test("LeastSquaresSeparate rejects non-finite matrix inputs") {
    val trials = LssTrialDesign.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(Double.NaN))))
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0))))

    val result = LeastSquaresSeparate.fit(trials, response)

    assertEquals(result.left.toOption, Some(FitError.NonFiniteInput("LSS trial design")))
  }

  test("LeastSquaresSeparate validates row alignment") {
    val trials = LssTrialDesign.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(0.0))))
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0))))

    val result = LeastSquaresSeparate.fit(trials, response)

    assertEquals(result.left.toOption, Some(FitError.RowMismatch(2, 3)))
  }

  private def explicitLss(trials: DMat, response: DMat, fixed: DMat, eps: Double = 1e-12): DMat =
    require(trials.rows == response.rows && fixed.rows == response.rows, "row mismatch")
    val out = new Array[Double](trials.cols * response.cols)
    val total = new Array[Double](trials.rows)

    var row = 0
    while row < trials.rows do
      var trial = 0
      while trial < trials.cols do
        total(row) += trials(row, trial)
        trial += 1
      row += 1

    var trial = 0
    while trial < trials.cols do
      var otherNorm2 = 0.0
      row = 0
      while row < trials.rows do
        val other = total(row) - trials(row, trial)
        otherNorm2 += other * other
        row += 1

      val includeOther = trials.cols > 1 && otherNorm2 > eps
      val cols = fixed.cols + 1 + (if includeOther then 1 else 0)
      val design = new Array[Double](trials.rows * cols)

      row = 0
      while row < trials.rows do
        var col = 0
        while col < fixed.cols do
          design(row * cols + col) = fixed(row, col)
          col += 1
        design(row * cols + fixed.cols) = trials(row, trial)
        if includeOther then design(row * cols + fixed.cols + 1) = total(row) - trials(row, trial)
        row += 1

      val coefficients = LeastSquaresOracle.coefficients(
        scalafim.fmri.fit.GaleTestMatrix.fromArray(trials.rows, cols, design),
        response
      )
      var voxel = 0
      while voxel < response.cols do
        out(trial * response.cols + voxel) = coefficients(fixed.cols, voxel)
        voxel += 1

      trial += 1

    scalafim.fmri.fit.GaleTestMatrix.fromArray(trials.cols, response.cols, out)
