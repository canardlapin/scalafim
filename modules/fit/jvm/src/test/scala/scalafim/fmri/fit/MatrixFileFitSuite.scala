package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset}
import scalafim.dataset.io.MatrixFileDatasetBackend
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{FmriModelBuilder, ModelBuildSpec}
import scalafim.image.NeuroSpace

import java.nio.charset.StandardCharsets
import java.nio.file.Files

class MatrixFileFitSuite extends munit.FunSuite:

  private def withMatrixFile[A](contents: String)(f: java.nio.file.Path => A): A =
    val path = Files.createTempFile("scalafim-fit-matrix-", ".csv")
    Files.writeString(path, contents, StandardCharsets.UTF_8)
    try f(path)
    finally Files.deleteIfExists(path)

  test("matrix-file dataset backend runs through builder-created OLS plan") {
    withMatrixFile("1,2\n3,1\n5,0\n7,-1\n") { path =>
      val events = DatasetEvents(
        Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
      )
      val dataset = FmriDataset(
        backend = MatrixFileDatasetBackend(
          id = DatasetId("matrix-fit-demo"),
          path = path,
          space = NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0)),
        events = events
      )
      val plan = FmriModelBuilder.buildPlan(
        dataset,
        ModelBuildSpec("onset ~ covariate(task)", baselineIntercept = Intercept.Global)
      )
      val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

      assertEquals(result.voxelIndices, Vector(0, 1))
      assertEquals(result.timepoints, Vector(0, 1, 2, 3))
      assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
      assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
      assertEqualsDouble(result.coefficient("base_constant", 0).get, 1.0, 1e-10)
      assertEqualsDouble(result.coefficient("base_constant", 1).get, 2.0, 1e-10)
    }
  }
