package scalafim.fmri.fit.scenarios

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{
  DataSelection,
  DatasetEvents,
  DatasetId,
  FmriDataset,
  InMemoryDatasetBackend,
  RunId,
  TimepointSelection
}
import scalafim.fmri.fit.GaleTestMatrix
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.fit.{
  DenseFmriFitResult,
  DesignMatrix,
  FitError,
  FitPlanExecutor,
  MatrixAdapters,
  Ols,
  RankPreviewDifference,
  ResidualDegreesOfFreedom,
  ResponseBlock,
  RunPartition,
  TContrast
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec}
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}

class CensoredMultirunConcatScenarioSuite extends munit.FunSuite:
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)

  test("censored multi-run concatenated fit row-deletes design and response consistently") {
    val result = runScenario()

    if !result.ciPass then fail(result.render)
  }

  private def runScenario(): ScenarioResult =
    val fixture = censoredFixture
    val plan =
      FmriModelBuilder.buildPlan(
        fixture.dataset,
        ModelBuildSpec(
          formula = "onset ~ covariate(task)",
          baselineIntercept = Intercept.Runwise
        )
      )
    val publicResult =
      FitPlanExecutor.unsafeFit(plan, fixture.selection).asInstanceOf[DenseFmriFitResult]
    val selectedSeries = plan.model.dataset.series(fixture.selection)
    val selectedDesign = value(MatrixAdapters.designMatrix(plan.model, fixture.keepTimepoints)).value
    val selectedResponse = value(MatrixAdapters.responseBlock(selectedSeries)).value
    val oracle = Ols.unsafeFit(DesignMatrix.unsafe(selectedDesign), ResponseBlock.unsafe(selectedResponse))
    val fullResult = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val task = value(TContrast("task", Map("task" -> 1.0)).evaluate(publicResult))
    val rankComparison = value(publicResult.rankPreviewComparison)
    val expectedPartitions = RunPartition.fromSamplingFrame(fixture.samplingFrame, fixture.keepTimepoints)
    val datasetPartitions = fixture.dataset.runPartitions(fixture.selection)

    val observations =
      Vector(
        ScenarioHarness.fact(
          "column names",
          publicResult.columnNames == Vector("task", "base_constant1_block_1", "base_constant1_block_2"),
          s"actual=${publicResult.columnNames.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "selected timepoints",
          publicResult.timepoints == fixture.keepTimepoints,
          s"actual=${publicResult.timepoints.mkString(",")} expected=${fixture.keepTimepoints.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "selected response rows",
          selectedSeries.timepoints == fixture.keepTimepoints,
          s"actual=${selectedSeries.timepoints.mkString(",")} expected=${fixture.keepTimepoints.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "residual degrees of freedom",
          publicResult.residualDegreesOfFreedom == ResidualDegreesOfFreedom.unsafe(fixture.keepTimepoints.length - publicResult.columnNames.length),
          s"actual=${publicResult.residualDegreesOfFreedom} expected=${fixture.keepTimepoints.length - publicResult.columnNames.length}"
        ),
        ScenarioHarness.fact(
          "fit-time rank evidence records aggressive row selection",
          rankComparison.differences.contains(RankPreviewDifference.SelectedRows(12, 6)) &&
            rankComparison.fitted.numericalRank == publicResult.columnNames.length,
          s"differences=${rankComparison.differences}; rank=${rankComparison.fitted.numericalRank}"
        ),
        ScenarioHarness.fact(
          "fit run partitions",
          expectedPartitions.map(_.timepoints) == Vector(Vector(0, 2, 4), Vector(6, 9, 11)),
          s"actual=${expectedPartitions.map(_.timepoints).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "fit run partition rows",
          expectedPartitions.map(_.rowIndices) == Vector(Vector(0, 1, 2), Vector(3, 4, 5)),
          s"actual=${expectedPartitions.map(_.rowIndices).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "dataset run ids",
          datasetPartitions.map(_.run.value) == Vector("run-1", "run-2"),
          s"actual=${datasetPartitions.map(_.run.value).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "dataset local run timepoints",
          datasetPartitions.map(_.localTimepoints) == Vector(Vector(0, 2, 4), Vector(0, 3, 5)),
          s"actual=${datasetPartitions.map(_.localTimepoints).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "full-data outliers affect the uncensored task coefficient",
          math.abs(fullResult.coefficient("task", 0).get - fixture.beta(0, 0)) > 0.1,
          s"uncensored=${fullResult.coefficient("task", 0).get} censoredExpected=${fixture.beta(0, 0)}"
        )
      ) ++
        ScenarioHarness.matrix("selected design", selectedDesign, fixture.selectedDesign, Tol) ++
        ScenarioHarness.matrix("selected response", selectedResponse, scalafim.fmri.fit.GaleTestMatrix.fromRows(fixture.selectedResponseRows), Tol) ++
        ScenarioHarness.matrix("coefficients", publicResult.coefficients.value, oracle.coefficients.value, Tol) ++
        ScenarioHarness.matrix("known beta after censoring", publicResult.coefficients.value, fixture.beta, Tol) ++
        ScenarioHarness.matrix("standard errors", publicResult.standardErrors.value, oracle.standardErrors.value, Tol) ++
        ScenarioHarness.vector("residual variance", publicResult.residualVariance, oracle.residualVariance, Tol) ++
        ScenarioHarness.vector("task estimate", task.estimates, fixture.beta.row(0), Tol) ++
        Vector(ScenarioHarness.finite("task statistic finite", task.statistics.toVector))

    ScenarioHarness.result("fit.censored-multirun-concat.v1", observations)

  private def value[A](either: Either[FitError, A]): A =
    either.fold(error => fail(error.message), identity)

  private final case class CensoredFixture(
      samplingFrame: SamplingFrame,
      task: Vector[Double],
      responseRows: Vector[Vector[Double]],
      keepTimepoints: Vector[Int],
      beta: DMat
  ):
    def selection: DataSelection =
      DataSelection(time = TimepointSelection.indices(keepTimepoints*))

    def dataset: FmriDataset =
      FmriDataset
        .open(
          backend = InMemoryDatasetBackend(
            DatasetId("scenario-censored-multirun-concat"),
            GaleTestMatrix.fromRows(responseRows),
            SampleSpaces(Vector(2, 1, 1))
          ),
          samplingFrame = samplingFrame,
          events = DatasetEvents(
            task.indices.toVector.map { timepoint =>
              val run = if timepoint < samplingFrame.blockLens.head then "run-1" else "run-2"
              Map(
                "onset" -> timepoint.toString,
                "run" -> run,
                "task" -> task(timepoint).toString
              )
            }
          ),
          runIds = Vector(RunId("run-1"), RunId("run-2"))
        )
        .fold(error => fail(error.message), identity)

    def selectedDesign: DMat =
      scalafim.fmri.fit.GaleTestMatrix.fromRows(keepTimepoints.map(designRow))

    def selectedResponseRows: Vector[Vector[Double]] =
      keepTimepoints.map(responseRows)

    private def designRow(timepoint: Int): Vector[Double] =
      Vector(
        task(timepoint),
        if timepoint < samplingFrame.blockLens.head then 1.0 else 0.0,
        if timepoint >= samplingFrame.blockLens.head then 1.0 else 0.0
      )

  private def censoredFixture: CensoredFixture =
    val samplingFrame = SamplingFrame(blockLens = Seq(6, 6), tr = Seq(1.0, 1.0))
    val task =
      Vector(
        -2.0, -1.0, 0.0, 1.0, 2.0, 3.0,
        -2.0, -1.0, 3.0, 0.0, 1.0, 2.0
      )
    val keep = Vector(0, 2, 4, 6, 9, 11)
    val beta =
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(0.65, -0.40),
          Vector(1.20, -0.80),
          Vector(-0.35, 0.55)
        )
      )
    val selectedResiduals =
      Vector(
        Vector(0.06, -0.04),
        Vector(-0.12, 0.08),
        Vector(0.06, -0.04),
        Vector(0.05, 0.03),
        Vector(-0.10, -0.06),
        Vector(0.05, 0.03)
      )
    val residualByTimepoint = keep.zip(selectedResiduals).toMap
    val rows =
      task.indices.toVector.map { timepoint =>
        if !keep.contains(timepoint) then
          if timepoint == 5 then Vector(40.0, -35.0)
          else Vector(-30.0, 25.0)
        else
          val run1 = if timepoint < samplingFrame.blockLens.head then 1.0 else 0.0
          val run2 = if timepoint >= samplingFrame.blockLens.head then 1.0 else 0.0
          val residual = residualByTimepoint(timepoint)
          Vector(
            beta(0, 0) * task(timepoint) + beta(1, 0) * run1 + beta(2, 0) * run2 + residual(0),
            beta(0, 1) * task(timepoint) + beta(1, 1) * run1 + beta(2, 1) * run2 + residual(1)
          )
      }

    CensoredFixture(samplingFrame, task, rows, keep, beta)
