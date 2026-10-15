package scalafim.fmri.laws

import gale.linalg.Matrix
import scalafim.fmri.fit.FitPlanExecutor
import scalafim.fmri.model.{FitEngine, FitPlan}
import scalafim.scenarios.{ScenarioObservation, ScenarioResult}

class CorrectedGlsSimulatorSuite extends munit.FunSuite:
  private val study = CorrectedGlsQualification

  test("corrected GLS simulator validates Gaussian innovations and stationary AR covariance"):
    val observations = Vector.newBuilder[ScenarioObservation]
    def fact(name: String, ok: Boolean, detail: String): Unit =
      observations += ScenarioObservation.Fact(name, ok, detail)
    val rng = new GlsStudyRng(study.seed(0, 0, 20000))
    val draws = Vector.fill(32768)(rng.gaussian())
    val mean = study.mean(draws)
    val variance = study.variance(draws)
    fact("Gaussian-mean", math.abs(mean) <= 0.02, s"mean=$mean")
    fact("Gaussian-variance", math.abs(variance - 1.0) <= 0.03, s"variance=$variance")
    Vector(study.cells.head, study.cells(3)).zipWithIndex.foreach { (cell, index) =>
      val phi1 = cell.phi(0)
      val phi2 = cell.phi.lift(1).getOrElse(0.0)
      val gamma0 = 1.0 / (1.0 - phi1 * phi1 * (1.0 + phi2) / (1.0 - phi2) - phi2 * phi2)
      val gamma1 = phi1 * gamma0 / (1.0 - phi2)
      val expected = Vector(gamma0, gamma1, phi1 * gamma1 + phi2 * gamma0)
      val sums = Array.fill(3)(0.0)
      val counts = Array.fill(3)(0L)
      var boundary = 0.0
      var replicate = 0
      while replicate < 128 do
        val errors = study.noise(cell, study.seed(0, index, replicate))
        var column = 0
        while column < errors.cols do
          var start = 0
          cell.runLengths.foreach { length =>
            var lag = 0
            while lag < 3 do
              var row = start + lag
              while row < start + length do
                sums(lag) += errors(row, column) * errors(row - lag, column)
                counts(lag) += 1L
                row += 1
              lag += 1
            start += length
          }
          boundary += errors(95, column) * errors(96, column)
          column += 1
        replicate += 1
      expected.indices.foreach { lag =>
        val actual = sums(lag) / counts(lag)
        fact(
          s"AR${cell.phi.length}-gamma-$lag",
          math.abs(actual - expected(lag)) <= 0.06 * gamma0,
          s"actual=$actual expected=${expected(lag)} bound=${0.06 * gamma0}"
        )
      }
      val covarianceAcrossRuns = boundary / (128.0 * 4.0)
      fact(
        s"AR${cell.phi.length}-independent-runs",
        math.abs(covarianceAcrossRuns) <= 0.25 * gamma0,
        s"boundaryCovariance=$covarianceAcrossRuns"
      )
    }
    val result = ScenarioResult("corrected-gls.simulator.covariance", observations.result())
    println(result.render)
    assert(result.ciPass, result.render)

  test("corrected GLS simulator recovers noiseless planted task and nuisance coefficients"):
    val observations = Vector.newBuilder[ScenarioObservation]
    study.cells.foreach { cell =>
      val model = study.model(cell, Matrix.zeros(cell.rows, 4), s"${cell.id}-noiseless")
      val fitted = FitPlanExecutor.fitDense(FitPlan(model, engine = FitEngine.OrdinaryLeastSquares))
      val observation = fitted match
        case Left(error)  => ScenarioObservation.Fact(cell.id, false, error.message)
        case Right(value) =>
          var gap = 0.0
          var col = 0
          while col < cell.design.cols do
            var voxel = 0
            while voxel < 4 do
              gap = math.max(gap, math.abs(value.coefficients(col, voxel) - cell.coefficient(col, voxel)))
              voxel += 1
            col += 1
          ScenarioObservation.Fact(
            cell.id,
            gap <= 1e-10 && value.olsDiagnostics.exists(_.fullRank),
            s"max coefficient gap=$gap"
          )
      observations += observation
    }
    val result = ScenarioResult("corrected-gls.simulator.noiseless", observations.result())
    println(result.render)
    assert(result.ciPass, result.render)
