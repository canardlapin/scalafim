package scalafim.fmri.fit.scenarios

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{Intercept, NuisanceCheck}
import scalafim.fmri.fit.{
  DenseFmriFitResult,
  DesignMatrix,
  FContrast,
  FitError,
  FitPlanExecutor,
  MatrixAdapters,
  Ols,
  ResponseBlock,
  TContrast
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec, NuisanceRegressors}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

class PublicFContrastScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)

  test("public fit path preserves design semantics for T and F contrasts") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = publicFitFixture
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("scenario-public-f-contrast"),
          DMat.fromRows(fixture.responseRows),
          NeuroSpace(Vector(2, 1, 1))
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

    val publicResult = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val generatedDesign = value(MatrixAdapters.designMatrix(plan.model, fixture.task.indices.toVector)).value
    val oracleDesign = fixture.oracleDesign
    val oracle = Ols.unsafeFit(DesignMatrix.unsafe(oracleDesign), ResponseBlock.unsafe(DoubleMatrix.fromRows(fixture.responseRows)))
    val t = value(TContrast("task", Map("task" -> 1.0)).evaluate(publicResult))
    val fTask = value(FContrast("task", Vector(Map("task" -> 1.0))).evaluate(publicResult))
    val fTaskAndMotion =
      value(
        FContrast(
          "task_and_motion",
          Vector(
            Map("task" -> 1.0),
            Map("nuis#01_1" -> 1.0)
          )
        ).evaluate(publicResult)
      )

    val observations =
      Vector(
        ScenarioHarness.fact(
          "column names",
          publicResult.columnNames == Vector("task", "base_constant", "nuis#01_1"),
          s"actual=${publicResult.columnNames.mkString(",")}"
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
        )
      ) ++
        ScenarioHarness.matrix("generated design", generatedDesign, oracleDesign, Tol) ++
        ScenarioHarness.matrix("coefficients", publicResult.coefficients.value, oracle.coefficients.value, Tol) ++
        ScenarioHarness.matrix("standard errors", publicResult.standardErrors.value, oracle.standardErrors.value, Tol) ++
        ScenarioHarness.vector("residual variance", publicResult.residualVariance, oracle.residualVariance, Tol) ++
        ScenarioHarness.vector("task t estimate", t.estimates, oracle.coefficients.value.row(0), Tol) ++
        ScenarioHarness.vector("task t standard error", t.standardErrors, oracle.standardErrors.value.row(0), Tol) ++
        Vector(ScenarioHarness.finite("task t statistic finite", t.statistics.toVector)) ++
        ScenarioHarness.matrix("task F estimates", fTask.estimates, DoubleMatrix.fromRows(Vector(oracle.coefficients.value.row(0).toVector)), Tol) ++
        ScenarioHarness.vector(
          "single-df F equals t squared",
          fTask.statistics,
          DoubleVector.fromSeq(t.statistics.toVector.map(value => value * value)),
          Tol
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

  private final case class PublicFitFixture(
      task: Vector[Double],
      motion: Vector[Double],
      responseRows: Vector[Vector[Double]]
  ):
    def oracleDesign: DoubleMatrix =
      DoubleMatrix.fromRows(
        task.indices.toVector.map { i =>
          Vector(task(i), 1.0, motion(i))
        }
      )

  private def publicFitFixture: PublicFitFixture =
    val task = Vector(-1.5, -1.0, -0.25, 0.75, 1.25, -0.5, 0.5, 1.75)
    val motion = Vector(0.2, -0.4, 0.7, -0.6, 0.1, 0.9, -0.8, 0.3)
    val noise0 = Vector(0.05, -0.02, 0.03, -0.04, 0.01, -0.03, 0.02, -0.02)
    val noise1 = Vector(-0.01, 0.04, -0.02, 0.03, -0.05, 0.02, -0.03, 0.01)
    val rows =
      task.indices.toVector.map { i =>
        val x = task(i)
        val m = motion(i)
        Vector(
          1.5 + 2.0 * x - 0.4 * m + noise0(i),
          -2.0 - 1.25 * x + 0.85 * m + noise1(i)
        )
      }

    PublicFitFixture(task, motion, rows)
