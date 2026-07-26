package scalafim.fmri.fit.scenarios

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.{
  ChunkedFitExecutor,
  FitChunkPlan,
  FitChunkingStrategy,
  FitError,
  FitParallelism,
  FitPlanExecutor,
  FmriFitResult,
  RunwiseFitBlockResult,
  RunwiseFmriFitResult
}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FmriModel}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import gale.linalg.{DMat, DVec}

import scala.concurrent.ExecutionContext.Implicits.global

class ChunkedRunwiseExecutionScenarioSuite extends munit.FunSuite:
  private val ScenarioId = "fit.chunked-runwise-execution.v1"
  private val Tol = ScenarioTolerance.mixed(1e-10, 1e-10)

  test("chunked runwise regression preserves chunk order, run partitions, and coefficients") {
    val fixture = ChunkedRunwiseFixture()

    FitPlanExecutor
      .fitChunkedFuture(fixture.plan, fixture.selection, fixture.chunking, FitParallelism.unsafe(2))
      .map { futureEither =>
        val result = runScenario(fixture, futureEither)
        if !result.ciPass then fail(result.render)
      }
  }

  private def runScenario(
      fixture: ChunkedRunwiseFixture,
      futureEither: Either[FitError, FmriFitResult]
  ): ScenarioResult =
    val chunkPlan = value(FitChunkPlan.fromSelection(fixture.plan, fixture.selection, fixture.chunking))
    val blockResults = value(ChunkedFitExecutor.fitChunks(fixture.plan, chunkPlan))
    val runwiseBlocks = blockResults.collect { case block: RunwiseFitBlockResult => block }
    val sequential =
      value(FitPlanExecutor.fitChunked(fixture.plan, fixture.selection, fixture.chunking))
        .asInstanceOf[RunwiseFmriFitResult]
    val future =
      value(futureEither)
        .asInstanceOf[RunwiseFmriFitResult]
    val unchunked =
      FitPlanExecutor
        .unsafeFit(fixture.plan, fixture.selection)
        .asInstanceOf[RunwiseFmriFitResult]

    val observations =
      Vector(
        ScenarioHarness.fact(
          "chunk count",
          chunkPlan.length == 3,
          s"actual=${chunkPlan.length} expected=3"
        ),
        ScenarioHarness.fact(
          "chunk ordinals",
          chunkPlan.indexed.map(_.ordinal.value).toVector == Vector(0, 1, 2),
          s"actual=${chunkPlan.indexed.map(_.ordinal.value).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "chunk voxel order",
          chunkPlan.indexed.map(_.voxelIndices).toVector == Vector(Vector(3, 1), Vector(4, 0), Vector(2)),
          s"actual=${chunkPlan.indexed.map(_.voxelIndices.mkString("[", ",", "]")).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "block result type",
          runwiseBlocks.length == blockResults.length,
          s"runwise=${runwiseBlocks.length} total=${blockResults.length}"
        ),
        ScenarioHarness.fact(
          "block run counts",
          runwiseBlocks.forall(_.runs.length == 2),
          s"actual=${runwiseBlocks.map(_.runs.length).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "selected voxel order",
          sequential.voxelIndices == fixture.selectedVoxels,
          s"actual=${sequential.voxelIndices.mkString(",")} expected=${fixture.selectedVoxels.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "selected timepoints",
          sequential.timepoints == fixture.selectedTimepoints,
          s"actual=${sequential.timepoints.mkString(",")} expected=${fixture.selectedTimepoints.mkString(",")}"
        ),
        ScenarioHarness.fact(
          "run indices",
          sequential.runs.map(_.runIndex) == Vector(0, 1),
          s"actual=${sequential.runs.map(_.runIndex).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "run row partitions",
          sequential.runs.map(_.rowIndices) == Vector(Vector(0, 1, 2, 3, 4), Vector(5, 6, 7, 8, 9)),
          s"actual=${sequential.runs.map(_.rowIndices.mkString("[", ",", "]")).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "run timepoint partitions",
          sequential.runs.map(_.timepoints) == Vector(Vector(0, 1, 2, 4, 5), Vector(6, 7, 8, 10, 11)),
          s"actual=${sequential.runs.map(_.timepoints.mkString("[", ",", "]")).mkString(",")}"
        ),
        ScenarioHarness.fact(
          "engine",
          sequential.engine == FitEngine.RunwiseLeastSquares,
          s"actual=${sequential.engine} expected=${FitEngine.RunwiseLeastSquares}"
        ),
        ScenarioHarness.fact(
          "column names",
          sequential.columnNames == Vector("task", "base_constant"),
          s"actual=${sequential.columnNames.mkString(",")}"
        )
      ) ++
        runwiseResultObservations("chunked vs unchunked", sequential, unchunked) ++
        runwiseResultObservations("future vs sequential chunked", future, sequential) ++
        analyticRunObservations("analytic run 0", sequential.runs(0), fixture.expectedCoefficients(run = 0)) ++
        analyticRunObservations("analytic run 1", sequential.runs(1), fixture.expectedCoefficients(run = 1)) ++
        finiteRunObservations("chunked", sequential) ++
        finiteRunObservations("future", future)

    ScenarioHarness.result(ScenarioId, observations)

  private def runwiseResultObservations(
      name: String,
      actual: RunwiseFmriFitResult,
      expected: RunwiseFmriFitResult
  ): Vector[ScenarioObservation] =
    Vector(
      ScenarioHarness.fact(s"$name.engine", actual.engine == expected.engine, s"actual=${actual.engine} expected=${expected.engine}"),
      ScenarioHarness.fact(s"$name.columns", actual.columnNames == expected.columnNames, s"actual=${actual.columnNames.mkString(",")}"),
      ScenarioHarness.fact(s"$name.voxels", actual.voxelIndices == expected.voxelIndices, s"actual=${actual.voxelIndices.mkString(",")}"),
      ScenarioHarness.fact(s"$name.timepoints", actual.timepoints == expected.timepoints, s"actual=${actual.timepoints.mkString(",")}"),
      ScenarioHarness.fact(s"$name.run-count", actual.runs.length == expected.runs.length, s"actual=${actual.runs.length} expected=${expected.runs.length}")
    ) ++ actual.runs.zip(expected.runs).zipWithIndex.flatMap { case ((left, right), run) =>
      Vector(
        ScenarioHarness.fact(s"$name.run[$run].index", left.runIndex == right.runIndex, s"actual=${left.runIndex} expected=${right.runIndex}"),
        ScenarioHarness.fact(s"$name.run[$run].rows", left.rowIndices == right.rowIndices, s"actual=${left.rowIndices.mkString(",")}"),
        ScenarioHarness.fact(s"$name.run[$run].timepoints", left.timepoints == right.timepoints, s"actual=${left.timepoints.mkString(",")}"),
        ScenarioHarness.fact(
          s"$name.run[$run].df",
          left.residualDegreesOfFreedom == right.residualDegreesOfFreedom,
          s"actual=${left.residualDegreesOfFreedom} expected=${right.residualDegreesOfFreedom}"
        ),
        ScenarioHarness.fact(
          s"$name.run[$run].diagnostics",
          left.olsDiagnostics == right.olsDiagnostics,
          s"actual=${left.olsDiagnostics} expected=${right.olsDiagnostics}"
        )
      ) ++
        ScenarioHarness.matrix(s"$name.run[$run].coefficients", left.coefficients.value, right.coefficients.value, Tol) ++
        ScenarioHarness.matrix(s"$name.run[$run].standard-errors", left.standardErrors.value, right.standardErrors.value, Tol) ++
        ScenarioHarness.matrix(s"$name.run[$run].covariance", left.normalizedCovariance, right.normalizedCovariance, Tol) ++
        ScenarioHarness.vector(s"$name.run[$run].residual-variance", left.residualVariance, right.residualVariance, Tol)
    }.toVector

  private def analyticRunObservations(
      name: String,
      actual: scalafim.fmri.fit.RunwiseFmriRunResult,
      expectedCoefficients: DMat
  ): Vector[ScenarioObservation] =
    ScenarioHarness.matrix(s"$name.coefficients", actual.coefficients.value, expectedCoefficients, Tol) ++
      Vector(
        ScenarioHarness.fact(
          s"$name.df",
          actual.residualDegreesOfFreedom.value == 3,
          s"actual=${actual.residualDegreesOfFreedom.value} expected=3"
        )
      )

  private def finiteRunObservations(name: String, result: RunwiseFmriFitResult): Vector[ScenarioObservation] =
    result.runs.zipWithIndex.flatMap { case (run, index) =>
      Vector(
        ScenarioHarness.finite(s"$name.run[$index].coefficients finite", run.coefficients.value.copyData.toVector),
        ScenarioHarness.finite(s"$name.run[$index].standard-errors finite", run.standardErrors.value.copyData.toVector),
        ScenarioHarness.finite(s"$name.run[$index].residual-variance finite", run.residualVariance.toVector)
      )
    }.toVector

  private def value[A](either: Either[FitError, A]): A =
    either.fold(error => fail(error.message), identity)

  private final case class ChunkedRunwiseFixture():
    val selectedTimepoints: Vector[Int] =
      Vector(0, 1, 2, 4, 5, 6, 7, 8, 10, 11)

    val selectedVoxels: Vector[Int] =
      Vector(3, 1, 4, 0, 2)

    val chunking: FitChunkingStrategy =
      FitChunkingStrategy.unsafeByVoxelCount(2)

    private val samplingFrame =
      SamplingFrame(blockLens = Seq(6, 6), tr = Seq(1.0, 1.0))

    private val taskByLocalTime =
      Vector(-2.0, -1.0, 0.0, 1.0, 2.0, 3.0)

    private val slopes =
      Vector(
        Vector(0.40, -0.80, 1.20, 0.10, -1.50),
        Vector(-0.30, 0.60, 1.50, -0.20, 0.80)
      )

    private val intercepts =
      Vector(
        Vector(1.00, -2.00, 0.50, 3.00, -1.00),
        Vector(2.00, 1.00, -1.00, 0.75, 4.00)
      )

    private val residualPattern =
      Map(0 -> 0.030, 1 -> -0.060, 2 -> 0.025, 4 -> 0.015, 5 -> -0.010)

    private val residualScale =
      Vector(
        Vector(1.0, -0.5, 0.25, 2.0, -1.5),
        Vector(-1.25, 0.75, -0.5, 1.5, 0.5)
      )

    def selection: DataSelection =
      DataSelection(
        time = IndexSelection.indices(selectedTimepoints*),
        voxels = IndexSelection.indices(selectedVoxels*)
      )

    def plan: FitPlan =
      FitPlan(model, engine = FitEngine.RunwiseLeastSquares)

    def expectedCoefficients(run: Int): DMat =
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          selectedVoxels.map(voxel => slopes(run)(voxel)),
          selectedVoxels.map(voxel => intercepts(run)(voxel))
        )
      )

    private def model: FmriModel =
      val eventModel =
        EventModel(
          terms = Vector.empty,
          samplingFrame = samplingFrame,
          designMatrix = Mat.fromRows(taskByTimepoint.map(value => Vector(value))),
          columnNames = Vector("task"),
          termSpans = Vector(0 -> 1),
          colIndices = Map("task" -> Vector(0))
        )
      val baseline =
        BaselineModel.build(
          samplingFrame = samplingFrame,
          basis = BaselineBasis.Constant,
          intercept = Intercept.Global
        )
      val dataset =
        FmriDataset.unsafe(
          backend = InMemoryDatasetBackend(
            DatasetId("scenario-chunked-runwise-execution"),
            ImageDMat.fromRows(responseRows),
            NeuroSpace(Vector(5, 1, 1))
          ),
          samplingFrame = samplingFrame
        )
      FmriModel(eventModel, baseline, dataset)

    private def taskByTimepoint: Vector[Double] =
      taskByLocalTime ++ taskByLocalTime

    private def responseRows: Vector[Vector[Double]] =
      (0 until samplingFrame.blockLens.sum).toVector.map { timepoint =>
        val run = if timepoint < samplingFrame.blockLens.head then 0 else 1
        val local = timepoint - samplingFrame.blockLens.take(run).sum
        if !residualPattern.contains(local) then
          Vector.tabulate(5)(voxel => if run == 0 then 20.0 + voxel.toDouble else -15.0 - voxel.toDouble)
        else
          Vector.tabulate(5) { voxel =>
            intercepts(run)(voxel) +
              slopes(run)(voxel) * taskByLocalTime(local) +
              residualPattern(local) * residualScale(run)(voxel)
          }
      }
