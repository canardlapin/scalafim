package scalafim.fmri.fit.scenarios

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.{
  DenseFmriFitResult,
  DesignMatrix,
  FContrast,
  FContrastResult,
  FitError,
  FitPlanExecutor,
  MatrixAdapters,
  Ols,
  OlsFit,
  ResponseBlock,
  TContrast,
  TContrastResult
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.DoubleMatrix

class SemanticContrastReorderedColumnsScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)

  test("name-keyed contrasts survive reordered public design columns") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = semanticFixture
    val dataset = fixture.dataset
    val canonical =
      fitSurface(
        label = "canonical",
        dataset = dataset,
        formula = "onset ~ covariate(task) + covariate(drift)"
      )
    val reordered =
      fitSurface(
        label = "reordered",
        dataset = dataset,
        formula = "onset ~ covariate(drift) + covariate(task)"
      )
    val canonicalOracle = oracle(fixture.canonicalDesign, fixture.responseRows)
    val reorderedOracle = oracle(fixture.reorderedDesign, fixture.responseRows)
    val expectedFEstimates =
      DoubleMatrix.fromRows(
        Vector(
          canonicalOracle.coefficients.value.row(0).toVector,
          canonicalOracle.coefficients.value.row(1).toVector
        )
      )

    val observations =
      Vector(
        ScenarioHarness.fact(
          "canonical column names",
          canonical.result.columnNames == Vector("task", "drift", "base_constant"),
          s"actual=${canonical.result.columnNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "reordered column names",
          reordered.result.columnNames == Vector("drift", "task", "base_constant"),
          s"actual=${reordered.result.columnNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "canonical residual df",
          canonical.result.residualDegreesOfFreedom == canonicalOracle.residualDegreesOfFreedom,
          s"actual=${canonical.result.residualDegreesOfFreedom} expected=${canonicalOracle.residualDegreesOfFreedom}"
        ),
        ScenarioHarness.fact(
          "reordered residual df",
          reordered.result.residualDegreesOfFreedom == reorderedOracle.residualDegreesOfFreedom,
          s"actual=${reordered.result.residualDegreesOfFreedom} expected=${reorderedOracle.residualDegreesOfFreedom}"
        )
      ) ++
        ScenarioHarness.matrix("canonical generated design", canonical.design, fixture.canonicalDesign, Tol) ++
        ScenarioHarness.matrix("reordered generated design", reordered.design, fixture.reorderedDesign, Tol) ++
        namedCoefficientChecks("canonical", canonical.result, canonicalOracle, Vector("task", "drift", "base_constant")) ++
        namedCoefficientChecks("reordered", reordered.result, reorderedOracle, Vector("drift", "task", "base_constant")) ++
        sameNamedCoefficientChecks(canonical.result, reordered.result, Vector("task", "drift", "base_constant")) ++
        ScenarioHarness.vector("task t estimate invariant", canonical.task.estimates, reordered.task.estimates, Tol) ++
        ScenarioHarness.vector("task t statistic invariant", canonical.task.statistics, reordered.task.statistics, Tol) ++
        ScenarioHarness.vector("task-minus-drift estimate invariant", canonical.taskMinusDrift.estimates, reordered.taskMinusDrift.estimates, Tol) ++
        ScenarioHarness.vector("task-minus-drift statistic invariant", canonical.taskMinusDrift.statistics, reordered.taskMinusDrift.statistics, Tol) ++
        ScenarioHarness.matrix("task and drift F estimates canonical", canonical.taskAndDrift.estimates, expectedFEstimates, Tol) ++
        ScenarioHarness.matrix("task and drift F estimates reordered", reordered.taskAndDrift.estimates, expectedFEstimates, Tol) ++
        ScenarioHarness.vector("task and drift F statistic invariant", canonical.taskAndDrift.statistics, reordered.taskAndDrift.statistics, Tol) ++
        Vector(
          ScenarioHarness.finite("canonical task t finite", canonical.task.statistics.toVector),
          ScenarioHarness.finite("reordered task t finite", reordered.task.statistics.toVector),
          ScenarioHarness.finite("canonical F finite", canonical.taskAndDrift.statistics.toVector),
          ScenarioHarness.finite("reordered F finite", reordered.taskAndDrift.statistics.toVector)
        )

    ScenarioHarness.result("fit.semantic-contrast-reordered-columns.v1", observations)

  private final case class FitSurface(
      label: String,
      result: DenseFmriFitResult,
      design: DoubleMatrix,
      task: TContrastResult,
      taskMinusDrift: TContrastResult,
      taskAndDrift: FContrastResult
  )

  private def fitSurface(label: String, dataset: FmriDataset, formula: String): FitSurface =
    val plan =
      FmriModelBuilder.buildPlan(
        dataset,
        ModelBuildSpec(
          formula = formula,
          baselineIntercept = Intercept.Global
        )
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val design = value(MatrixAdapters.designMatrix(plan.model, result.timepoints)).value
    val task = value(TContrast("task", Map("task" -> 1.0)).evaluate(result))
    val taskMinusDrift =
      value(TContrast("task_minus_drift", Map("task" -> 1.0, "drift" -> -1.0)).evaluate(result))
    val taskAndDrift =
      value(
        FContrast(
          "task_and_drift",
          Vector(
            Map("task" -> 1.0),
            Map("drift" -> 1.0)
          )
        ).evaluate(result)
      )

    FitSurface(label, result, design, task, taskMinusDrift, taskAndDrift)

  private def namedCoefficientChecks(
      label: String,
      result: DenseFmriFitResult,
      oracle: OlsFit,
      names: Vector[String]
  ): Vector[ScenarioObservation] =
    names.zipWithIndex.flatMap { case (name, row) =>
      result.voxelIndices.zipWithIndex.map { case (voxel, col) =>
        ScenarioHarness.scalar(
          s"$label coefficient $name voxel $voxel",
          result.coefficient(name, voxel).get,
          oracle.coefficients.value(row, col),
          Tol
        )
      }
    }

  private def sameNamedCoefficientChecks(
      canonical: DenseFmriFitResult,
      reordered: DenseFmriFitResult,
      names: Vector[String]
  ): Vector[ScenarioObservation] =
    names.flatMap { name =>
      canonical.voxelIndices.map { voxel =>
        ScenarioHarness.scalar(
          s"semantic coefficient $name voxel $voxel",
          canonical.coefficient(name, voxel).get,
          reordered.coefficient(name, voxel).get,
          Tol
        )
      }
    }

  private def oracle(design: DoubleMatrix, responseRows: Vector[Vector[Double]]): OlsFit =
    Ols.unsafeFit(
      DesignMatrix.unsafe(design),
      ResponseBlock.unsafe(DoubleMatrix.fromRows(responseRows))
    )

  private def value[A](either: Either[FitError, A]): A =
    either.fold(error => fail(error.message), identity)

  private final case class SemanticFixture(
      task: Vector[Double],
      drift: Vector[Double],
      responseRows: Vector[Vector[Double]]
  ):
    def dataset: FmriDataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("scenario-semantic-contrast-reordered-columns"),
          DMat.fromRows(responseRows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(task.length), tr = Seq(1.0)),
        events = DatasetEvents(
          task.indices.toVector.map { i =>
            Map(
              "onset" -> i.toString,
              "task" -> task(i).toString,
              "drift" -> drift(i).toString
            )
          }
        )
      )

    def canonicalDesign: DoubleMatrix =
      design(Vector("task", "drift", "base_constant"))

    def reorderedDesign: DoubleMatrix =
      design(Vector("drift", "task", "base_constant"))

    private def design(names: Vector[String]): DoubleMatrix =
      DoubleMatrix.fromRows(
        task.indices.toVector.map { i =>
          names.map {
            case "task"          => task(i)
            case "drift"         => drift(i)
            case "base_constant" => 1.0
            case other           => throw new IllegalArgumentException(s"unknown design column $other")
          }
        }
      )

  private def semanticFixture: SemanticFixture =
    val task = Vector(-1.5, -1.0, -0.2, 0.4, 1.2, -0.8, 0.2, 0.9, 1.5, -1.2)
    val drift = Vector(-0.4, -0.1, 0.3, 0.8, -0.7, 1.0, -1.1, 0.5, -0.9, 0.6)
    val noise0 = Vector(0.04, -0.03, 0.02, -0.05, 0.01, -0.02, 0.03, -0.01, 0.02, -0.01)
    val noise1 = Vector(-0.02, 0.03, -0.01, 0.04, -0.03, 0.02, -0.04, 0.01, -0.02, 0.02)
    val rows =
      task.indices.toVector.map { i =>
        val x = task(i)
        val d = drift(i)
        Vector(
          2.5 + 1.75 * x - 0.65 * d + noise0(i),
          -1.0 - 0.8 * x + 1.2 * d + noise1(i)
        )
      }

    SemanticFixture(task, drift, rows)
