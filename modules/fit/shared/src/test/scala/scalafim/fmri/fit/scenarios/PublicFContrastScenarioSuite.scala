package scalafim.fmri.fit.scenarios

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{Intercept, NuisanceCheck}
import scalafim.fmri.fit.{
  DesignMatrix,
  FContrast,
  FitError,
  FitPlanExecutor,
  MatrixAdapters,
  Ols,
  ResponseBlock,
  ResidualDegreesOfFreedom,
  TContrast
}
import scalafim.fmri.fit.GaleTestMatrix
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, NuisanceRegressors}
import gale.linalg.DVec

class PublicFContrastScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)
  private val NilearnTol = ScenarioTolerance.mixed(1e-8, 1e-8)
  private val NilearnProfile = ScenarioComparisonTolerance.bounded(
    maxAbsoluteL2Error = 1e-7,
    maxRelativeL2Error = 1e-8,
    minimumSignedCorrelation = 0.99999999,
    maxNormRatioDeviation = 1e-8
  )

  test("public fit path preserves design semantics for T and F contrasts") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = PublicFContrastNilearnFixture
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("scenario-public-f-contrast"),
          GaleTestMatrix.fromRows(fixture.responseRows),
          SampleSpaces(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(fixture.task.length), tr = Seq(1.0)),
        events = DatasetEvents(
          fixture.task.indices.toVector.map { i =>
            Map("onset" -> i.toString, "task" -> fixture.task(i).toString)
          }
        )
      )

    val nuisance = Mat.fromRows(fixture.motion.map(value => Vector(value)))
    val plan =
      FmriModelBuilder.buildPlan(
        dataset,
        ModelBuildSpec(
          formula = "onset ~ covariate(task)",
          baselineIntercept = Intercept.Global,
          nuisance = Some(
            NuisanceRegressors(
              matrices = Vector(nuisance),
              names = Some(Vector(Vector("motion_x"))),
              check = NuisanceCheck.Drop
            )
          )
        )
      )

    val publicResult = value(FitPlanExecutor.fitDense(plan))
    val generatedDesign = value(MatrixAdapters.designMatrix(plan.model, fixture.task.indices.toVector)).value
    val oracleDesign = fixture.design
    val oracle = Ols.unsafeFit(DesignMatrix.unsafe(oracleDesign), ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(fixture.responseRows)))
    val t = value(TContrast("task", Map("task" -> 1.0)).evaluate(publicResult))
    val fTask = value(FContrast("task", Vector(Map("task" -> 1.0))).evaluate(publicResult))
    val fTaskAndMotion =
      value(
        FContrast(
          "task_and_motion",
          Vector(
            Map("task" -> 1.0),
            Map("motion_x" -> 1.0)
          )
        ).evaluate(publicResult)
      )

    val observations =
      Vector(
        ScenarioHarness.fact(
          "scenario id",
          fixture.scenarioId == "fit.public-f-contrast.v1",
          s"actual=${fixture.scenarioId} expected=fit.public-f-contrast.v1"
        ),
        ScenarioHarness.fact(
          "column names",
          publicResult.columnNames == Vector("task", "base_constant", "motion_x"),
          s"actual=${publicResult.columnNames.mkString(",")} expected=task,base_constant,motion_x"
        ),
        ScenarioHarness.fact(
          "voxel indices",
          publicResult.voxelIndices == Vector(0, 1),
          s"actual=${publicResult.voxelIndices.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "residual degrees of freedom",
          publicResult.residualDegreesOfFreedom == oracle.residualDegreesOfFreedom,
          s"actual=${publicResult.residualDegreesOfFreedom} expected=${oracle.residualDegreesOfFreedom}"
        ),
        ScenarioHarness.fact(
          "nilearn residual degrees of freedom",
          publicResult.residualDegreesOfFreedom == ResidualDegreesOfFreedom.unsafe(fixture.residualDegreesOfFreedom),
          s"actual=${publicResult.residualDegreesOfFreedom} expected=${fixture.residualDegreesOfFreedom}"
        )
      ) ++
        ScenarioHarness.matrix("generated design", generatedDesign, oracleDesign, Tol) ++
        ScenarioHarness.matrix("coefficients", publicResult.coefficients.value, oracle.coefficients.value, Tol) ++
        ScenarioHarness.matrix("nilearn coefficients", publicResult.coefficients.value, fixture.coefficients, NilearnTol) ++
        ScenarioHarness.matrixMetrics("nilearn coefficients", publicResult.coefficients.value, fixture.coefficients, NilearnProfile) ++
        ScenarioHarness.matrix("standard errors", publicResult.standardErrors.value, oracle.standardErrors.value, Tol) ++
        ScenarioHarness.vector("nilearn residual variance", publicResult.residualVariance, fixture.residualVariance, NilearnTol) ++
        ScenarioHarness.vector("residual variance", publicResult.residualVariance, oracle.residualVariance, Tol) ++
        ScenarioHarness.vector("task t estimate", t.estimates, oracle.coefficients.value.row(0), Tol) ++
        ScenarioHarness.vector("nilearn task t estimate", t.estimates, fixture.taskTEstimates, NilearnTol) ++
        ScenarioHarness.vector("task t standard error", t.standardErrors, oracle.standardErrors.value.row(0), Tol) ++
        ScenarioHarness.vector("nilearn task t standard error", t.standardErrors, fixture.taskTStandardErrors, NilearnTol) ++
        ScenarioHarness.vector("nilearn task t statistic", t.statistics, fixture.taskTStatistics, NilearnTol) ++
        ScenarioHarness.vectorMetrics("nilearn task t statistic", t.statistics, fixture.taskTStatistics, NilearnProfile) ++
        Vector(ScenarioHarness.finite("task t statistic finite", t.statistics.toVector)) ++
        ScenarioHarness.matrix("task F estimates", fTask.estimates, scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(oracle.coefficients.value.row(0).toVector)), Tol) ++
        ScenarioHarness.vector(
          "single-df F equals t squared",
          fTask.statistics,
          DVec.fromSeq(t.statistics.toVector.map(value => value * value)),
          Tol
        ) ++
        ScenarioHarness.vector("nilearn task F statistic", fTask.statistics, fixture.taskFStatistics, NilearnTol) ++
        ScenarioHarness.vector(
          "nilearn task and motion F statistic",
          fTaskAndMotion.statistics,
          fixture.taskAndMotionFStatistics,
          NilearnTol
        ) ++
        ScenarioHarness.vectorMetrics(
          "nilearn task and motion F statistic",
          fTaskAndMotion.statistics,
          fixture.taskAndMotionFStatistics,
          NilearnProfile
        ) ++
        ScenarioHarness.values(
          "nilearn one-df log-p identity",
          fixture.taskTPValues.toSeq.toVector.map(math.log),
          fixture.taskFPValues.toSeq.toVector.map(math.log),
          ScenarioTolerance.absolute(1e-6)
        ) ++
        Vector(
          ScenarioHarness.fact(
            "task F numerator df",
            fTask.numeratorDegreesOfFreedom == 1,
            s"actual=${fTask.numeratorDegreesOfFreedom} expected=1"
          ),
          ScenarioHarness.fact(
            "task and motion F numerator df",
            fTaskAndMotion.numeratorDegreesOfFreedom == 2,
            s"actual=${fTaskAndMotion.numeratorDegreesOfFreedom} expected=2"
          ),
          ScenarioHarness.fact(
            "task and motion F residual df",
            fTaskAndMotion.residualDegreesOfFreedom == oracle.residualDegreesOfFreedom,
            s"actual=${fTaskAndMotion.residualDegreesOfFreedom} expected=${oracle.residualDegreesOfFreedom}"
          ),
          ScenarioHarness.finite("task and motion F statistic finite", fTaskAndMotion.statistics.toVector)
        )

    ScenarioHarness.result("fit.public-f-contrast.v1", observations)

  private def value[A](either: Either[FitError, A]): A =
    either.fold(error => fail(error.message), identity)
