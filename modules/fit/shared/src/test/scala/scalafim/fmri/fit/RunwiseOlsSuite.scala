package scalafim.fmri.fit

import scalafim.image.SampleSpaces

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FmriModel}

class RunwiseOlsSuite extends munit.FunSuite:

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4, 4), tr = Seq(1.0, 1.0))

  private def model: FmriModel =
    val x = Vector(0.0, 1.0, 2.0, 3.0, 0.0, 1.0, 2.0, 3.0)
    val y = Vector(1.0, 3.0, 5.0, 7.0, 10.0, 9.0, 8.0, 7.0)
    val data = GaleTestMatrix.fromRows(y.map(v => Vector(v)))
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("runwise-demo"), data, SampleSpaces(Vector(1, 1, 1))),
        samplingFrame = samplingFrame
      )
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = samplingFrame,
        designMatrix = Mat.fromRows(x.map(v => Vector(v))),
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
    FmriModel(eventModel, baseline, dataset)

  test("RunwiseLeastSquares fits separate coefficients per sampling-frame block") {
    val result =
      FitPlanExecutor
        .unsafeFit(FitPlan(model, engine = FitEngine.RunwiseLeastSquares))
        .asInstanceOf[RunwiseFmriFitResult]

    assertEquals(result.engine, FitEngine.RunwiseLeastSquares)
    assertEquals(result.runs.map(_.runIndex), Vector(0, 1))
    assertEquals(result.runs.map(_.timepoints), Vector(Vector(0, 1, 2, 3), Vector(4, 5, 6, 7)))

    val run0 = result.run(0).get
    val run1 = result.run(1).get
    assertEquals(run0.olsDiagnostics.solveMethod, OlsSolveMethod.QrRankRevealing)
    assertEquals(run0.olsDiagnostics.rank, 2)
    assertEquals(run1.olsDiagnostics.solveMethod, OlsSolveMethod.QrRankRevealing)
    assertEquals(run1.olsDiagnostics.rank, 2)
    assertEqualsDouble(run0.coefficient("task", 0, result.columnNames, result.voxelIndices).get, 2.0, 1e-10)
    assertEqualsDouble(run0.coefficient("base_constant", 0, result.columnNames, result.voxelIndices).get, 1.0, 1e-10)
    assertEqualsDouble(run1.coefficient("task", 0, result.columnNames, result.voxelIndices).get, -1.0, 1e-10)
    assertEqualsDouble(run1.coefficient("base_constant", 0, result.columnNames, result.voxelIndices).get, 10.0, 1e-10)
  }

  test("RunwiseOls reports singular run designs with run identity") {
    val design = DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(1.0, 1.0),
          Vector(1.0, 0.0),
          Vector(1.0, 1.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(3.0), Vector(4.0)))
    )
    val partitions = Vector(
      RunPartition(runIndex = 0, rowIndices = Vector(0, 1), timepoints = Vector(0, 1)),
      RunPartition(runIndex = 1, rowIndices = Vector(2, 3), timepoints = Vector(2, 3))
    )

    val result = RunwiseOls.fit(design, response, partitions)
    result match
      case Left(FitError.RunwiseFitFailed(0, FitError.RankDeficientDesign(report))) =>
        assert(report.deficient)
      case Left(FitError.RunwiseFitFailed(0, FitError.SingularDesign(_))) => assert(true)
      case other => fail(s"unexpected result: $other")
  }
