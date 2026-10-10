package scalafim.fmri.laws

import gale.linalg.Matrix
import scalafim.fmri.fit.FitPlanExecutor
import scalafim.fmri.model.{FitEngine, FitPlan}
import scalafim.scenarios.{ScenarioObservation, ScenarioResult}

class GlsFactorSimulatorSuite extends munit.FunSuite:
  test("factorial and longer-run models recover the planted noiseless coefficients"):
    val observations = (GlsFactorStudy.diagnosticCells ++ GlsFactorStudy.confirmationCells).map { cell =>
      val model = CorrectedGlsQualification.model(cell, Matrix.zeros(cell.rows, 4), s"${cell.id}-factor-noiseless")
      FitPlanExecutor.fitDense(FitPlan(model, engine = FitEngine.OrdinaryLeastSquares)) match
        case Left(error) => ScenarioObservation.Fact(cell.id, false, error.message)
        case Right(fit)  =>
          var gap = 0.0
          var column = 0
          while column < cell.design.cols do
            var voxel = 0
            while voxel < 4 do
              gap = math.max(gap, math.abs(fit.coefficients(column, voxel) - cell.coefficient(column, voxel)))
              voxel += 1
            column += 1
          ScenarioObservation.Fact(cell.id, gap <= 1e-10 && fit.olsDiagnostics.exists(_.fullRank), s"gap=$gap")
    }
    val result = ScenarioResult("gls-factors.simulator.noiseless", observations)
    println(result.render)
    assert(result.ciPass, result.render)
