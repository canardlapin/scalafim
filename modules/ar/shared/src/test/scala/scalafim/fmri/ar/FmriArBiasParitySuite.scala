package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.fixtures.FmriArBiasRFixture
import scalafim.fmri.ar.fixtures.FmriArBiasRFixture.*

/** Parity of the design-aware residual-bias correction with fmriAR (`acvf_bias_matrix()`,
  * `fit_noise(design =)`, `noise_acvf(design =)`). Inputs are fmriAR's own deterministic designs and OLS
  * residuals; nothing here draws random numbers.
  */
class FmriArBiasParitySuite extends munit.FunSuite:

  /** Tolerance for values that are products of the same arithmetic: fixtures carry 13 significant digits. */
  private val Tol = 1e-10

  private def toMatrix(rows: Vector[Vector[Double]]): DMat =
    Matrix.tabulate(rows.length, rows.head.length)((row, col) => rows(row)(col))

  private def valueOrFail[A](result: Either[ArError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def layoutFor(runLengths: Vector[Int], censorOneBased: Vector[Int]): NoiseEstimationLayout =
    valueOrFail(
      NoiseEstimationLayout.excludingRows(
        TimeSegments.fromRunLengths(runLengths),
        runLengths.sum,
        censorOneBased.map(_ - 1).toSet
      )
    )

  private def assertClose(actual: Seq[Double], expected: Seq[Double], tol: Double, label: String): Double =
    assertEquals(actual.length, expected.length, label)
    var worst = 0.0
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      val diff = math.abs(a - e)
      worst = math.max(worst, diff)
      assert(diff <= tol, clues(label, i, a, e, diff))
    }
    worst

  private def rowMajor(matrix: DMat): Vector[Double] =
    Vector.tabulate(matrix.rows * matrix.cols)(i => matrix(i / matrix.cols, i % matrix.cols))

  private def optionsFor(fit: FitResult): ArFitOptions =
    val order = if fit.order == "auto" then ArOrder.Auto(fit.pMax) else ArOrder.Fixed(fit.order.toInt)
    val pooling = if fit.pooling == "global" then NoisePooling.Global else NoisePooling.Run
    ArFitOptions(order = order, pooling = pooling, exactFirstAr1 = false)

  biasCases.foreach { c =>
    test(s"acvf_bias_matrix parity: ${c.name}") {
      val layout = layoutFor(c.runLengths, c.censorOneBased)
      val result = valueOrFail(AcvfBias.matrices(toMatrix(c.design), layout, c.requestedLag))
      assertEquals(result.lag, c.lag)
      assertEquals(result.budgetCapped, c.capped)
      assertEquals(result.byRun.length, c.matrices.length)
      result.byRun.zip(c.matrices).zipWithIndex.foreach { case ((actual, expected), run) =>
        assertEquals(actual.rows, c.lag + 1)
        assertClose(rowMajor(actual), expected.flatten, Tol, s"${c.name} run $run")
      }
    }

    test(s"acvf_bias_matrix reciprocal condition agrees with LAPACK: ${c.name}") {
      val layout = layoutFor(c.runLengths, c.censorOneBased)
      val result = valueOrFail(AcvfBias.matrices(toMatrix(c.design), layout, c.requestedLag))
      // Both are 1-norm condition estimators (Hager/Higham) and may differ slightly; the gate at 1e-6 only needs
      // them to agree on the order of magnitude.
      result.byRun.zip(c.reciprocalCondition).foreach { case (matrix, expected) =>
        val actual = AcvfBias.reciprocalCondition(matrix)
        if expected < AcvfBias.ReciprocalConditionFloor then
          // Far below the floor the two estimators need only agree on which side of the gate they fall.
          assert(actual < AcvfBias.ReciprocalConditionFloor, clues(actual, expected))
        else
          val ratio = actual / expected
          assert(ratio > 0.5 && ratio < 2.0, clues(ratio, expected))
      }
    }
  }

  test("budget above the residual degrees of freedom is capped to max(1, df - 1), as in R") {
    val c = biasCases.find(_.capped).getOrElse(fail("no capped fixture"))
    val layout = layoutFor(c.runLengths, c.censorOneBased)
    val result = valueOrFail(AcvfBias.matrices(toMatrix(c.design), layout, c.requestedLag))
    assertEquals(result.residualDf, 6)
    assertEquals(result.requestedLag, 8)
    assertEquals(result.lag, 5)
  }

  fitCases.filterNot(_.name == "rcond_rejected").foreach { c =>
    c.fits.foreach { fit =>
      test(s"fit_noise(design) parity: ${c.name} p=${fit.order} pooling=${fit.pooling}") {
        val layout = layoutFor(c.runLengths, c.censorOneBased)
        val policy =
          EstimationPolicy.DesignCorrected(toMatrix(c.design), CorrectionBudget.Fixed(c.correctionMaxLag))
        val residuals = toMatrix(c.residuals)
        val fitted = valueOrFail(NoiseFit.estimate(residuals, layout, optionsFor(fit), policy))

        val rejected = fitted.corrections.zipWithIndex.collect { case (RunCorrection.IllConditioned(_), run) => run }
        assertEquals(rejected, fit.rejectedRuns, clues(fitted.corrections))
        assert(
          fitted.corrections.forall(c => !c.isInstanceOf[RunCorrection.Uncorrected.type]),
          clues(fitted.corrections)
        )
        assertEquals(fitted.plan.coefficients.length, fit.phi.length)
        fitted.plan.coefficients.zip(fit.phi).zipWithIndex.foreach { case ((coefficients, expected), i) =>
          assertClose(coefficients.phi, expected, Tol, s"phi[$i]")
        }
        fitted.acvf.zip(fit.gamma).zipWithIndex.foreach { case ((gamma, expected), i) =>
          assertClose(gamma, expected, Tol, s"gamma[$i]")
        }
        fitted.innovationVariance.zip(fit.sigma2).zipWithIndex.foreach { case ((sigma2, expected), i) =>
          // NaN is R's NA: the innovation variance is undefined (no usable autocovariance).
          if expected.isNaN then assertEquals(sigma2, None, s"sigma2[$i]")
          else assertClose(Vector(sigma2.getOrElse(Double.NaN)), Vector(expected), Tol, s"sigma2[$i]")
        }

        // The design-corrected plan is what fitNoise(policy) returns; the raw default must differ from it.
        val plan = valueOrFail(ArEstimation.fitNoise(residuals, layout, optionsFor(fit), policy))
        assertEquals(plan.coefficients, fitted.plan.coefficients)
        val raw = valueOrFail(ArEstimation.fitNoise(residuals, layout, optionsFor(fit)))
        assertNotEquals(raw.coefficients, plan.coefficients)
      }
    }
  }

  test("rcond gate: an ill-conditioned bias matrix leaves the run uncorrected, matching R") {
    val c = fitCases.find(_.name == "rcond_rejected").getOrElse(fail("no rejection fixture"))
    val fit = c.fits.head
    assertEquals(fit.rejectedRuns, Vector(0), "R must have rejected this budget")
    assert(FmriArBiasRFixture.rejectedReciprocalCondition < AcvfBias.ReciprocalConditionFloor)

    val layout = layoutFor(c.runLengths, c.censorOneBased)
    val residuals = toMatrix(c.residuals)
    val policy = EstimationPolicy.DesignCorrected(toMatrix(c.design), CorrectionBudget.Fixed(c.correctionMaxLag))
    val fitted = valueOrFail(NoiseFit.estimate(residuals, layout, optionsFor(fit), policy))

    fitted.corrections match
      case Vector(RunCorrection.IllConditioned(rcond)) =>
        assert(rcond < AcvfBias.ReciprocalConditionFloor, clues(rcond))
      case other => fail(s"expected one IllConditioned run, got $other")
    assertEquals(fitted.biasMatrices.map(_.lag), Some(57))

    assertClose(fitted.plan.coefficients.head.phi, fit.phi.head, Tol, "phi")
    assertClose(fitted.plan.coefficients.head.phi, FmriArBiasRFixture.rejectedRawPhi, Tol, "phi vs R raw")
    val raw = valueOrFail(ArEstimation.fitNoise(residuals, layout, optionsFor(fit)))
    assertClose(raw.coefficients.head.phi, fitted.plan.coefficients.head.phi, 1e-12, "phi vs Scala raw")
    assertClose(fitted.acvf.head, fit.gamma.head, Tol, "gamma")
  }

  test("only the short run is IllConditioned for runs of 70 and 14 rows (status and phi match R)") {
    val c = fitCases.find(_.name == "runs_70_and_14_auto").getOrElse(fail("missing fixture"))
    val fit = c.fits.head
    assertEquals(fit.rejectedRuns, Vector(1))
    val layout = layoutFor(c.runLengths, c.censorOneBased)
    val policy = EstimationPolicy.DesignCorrected(toMatrix(c.design), CorrectionBudget.Fixed(c.correctionMaxLag))
    val fitted = valueOrFail(NoiseFit.estimate(toMatrix(c.residuals), layout, optionsFor(fit), policy))
    fitted.corrections match
      case Vector(RunCorrection.Applied(_), RunCorrection.IllConditioned(_)) => ()
      case other                                                              => fail(s"unexpected statuses $other")
    fitted.plan.coefficients.zip(fit.phi).zipWithIndex.foreach { case ((coefficients, expected), i) =>
      assertClose(coefficients.phi, expected, Tol, s"phi[$i]")
    }
  }

  test("a fully censored run keeps the identity bias matrix and an empty fit, as in R") {
    val c = fitCases.find(_.name == "run_two_fully_censored").getOrElse(fail("missing fixture"))
    c.fits.foreach { fit =>
      assert(fit.phi.forall(_.length <= 1))
      if fit.pooling == "run" then
        assertEquals(fit.phi(1), Vector.empty[Double])
        assertEquals(fit.gamma(1), Vector.empty[Double])
    }
  }

  adaptiveCases.foreach { c =>
    test(s"adaptive lag budget matches fmrireg .ar_correction_lag_budget: ${c.name}") {
      val layout = layoutFor(c.runLengths, c.censorOneBased)
      val lag = valueOrFail(
        AcvfBias.resolveLag(CorrectionBudget.Adaptive(c.ceiling), toMatrix(c.design), layout, c.order)
      )
      assertEquals(lag, c.budget)
    }
  }

  acvfCases.zipWithIndex.foreach { case (expected, index) =>
    test(s"noise_acvf(design) parity #$index: ${expected.fit} ${expected.pooling} maxLag=${expected.maxLag}") {
      val c = fitCases.find(_.name == expected.fit).getOrElse(fail("missing fit case"))
      val layout = layoutFor(c.runLengths, c.censorOneBased)
      val pooling = if expected.pooling == "global" then NoisePooling.Global else NoisePooling.Run
      val policy = EstimationPolicy.DesignCorrected(toMatrix(c.design), CorrectionBudget.Fixed(c.correctionMaxLag))
      val result = valueOrFail(NoiseAcvf.estimate(toMatrix(c.residuals), layout, expected.maxLag, pooling, policy))

      assertEquals(result.corrected, expected.corrected)
      assertEquals(result.units.length, expected.acvf.length)
      result.units.zip(expected.acvf).zip(expected.pairs).zip(expected.segments).foreach {
        case (((unit, gamma), pairs), segments) =>
          assertClose(unit.acvf, gamma, Tol, "acvf")
          // The pair count here is summed over residual columns; fmriAR reports it per column.
          assertClose(unit.pairs.map(_.toDouble / c.residuals.head.length), pairs, 1e-12, "pairs")
          assertEquals(unit.segmentCount, segments)
      }
    }
  }
