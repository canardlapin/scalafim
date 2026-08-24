package scalafim.fmri.fit

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.NuisanceCheck
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FmriModelBuilder, ModelBuildSpec, NuisanceRegressors}
import scalafim.image.{DMat as ImageDMat, SomeSampleSpace}

class FitPlanBuilderSuite extends munit.FunSuite:

  private def dataset: FmriDataset =
    val rows = Vector.tabulate(4) { i =>
      val x = i.toDouble
      Vector(1.0 + 2.0 * x, 2.0 - x)
    }
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )

    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("builder-ols-demo"),
        ImageDMat.fromRows(rows),
        SampleSpaces(Vector(2, 1, 1))
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0)),
      events = events
    )

  test("builder-created FitPlan runs OLS and recovers known coefficients") {
    val plan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec("onset ~ covariate(task)", baselineIntercept = Intercept.Global)
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEquals(plan.engine, FitEngine.OrdinaryLeastSquares)
    assertEquals(result.columnNames, Vector("task", "base_constant"))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 1).get, 2.0, 1e-10)
  }

  test("builder-created FitPlan recovers task effects after nuisance adjustment") {
    val task = Vector(0.0, 1.0, 0.0, 1.0, 0.0, 1.0)
    val motion = Vector(0.0, 0.0, 1.0, 1.0, 2.0, 2.0)
    val rows = task.zip(motion).map { case (x, m) =>
      Vector(3.0 + 2.0 * x + 5.0 * m)
    }
    val events = DatasetEvents(
      task.indices.toVector.map(i => Map("onset" -> i.toString, "task" -> task(i).toString))
    )
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("builder-nuisance-demo"),
        ImageDMat.fromRows(rows),
        SampleSpaces(Vector(1, 1, 1))
      ),
      samplingFrame = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0)),
      events = events
    )
    val nuisance = Mat.fromRows(
      motion.map(m => Vector(m, m, 1.0))
    )
    val plan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        "onset ~ covariate(task)",
        baselineIntercept = Intercept.Global,
        nuisance = Some(
          NuisanceRegressors(
            matrices = Vector(nuisance),
            names = Some(Vector(Vector("motion_y", "motion_y_dup", "constant_conf"))),
            check = NuisanceCheck.Drop
          )
        )
      )
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEquals(plan.model.baselineModel.nuisanceReport.map(_.retainedByBlock).get, Vector(Vector("motion_y")))
    assertEquals(plan.model.baselineModel.nuisanceReport.map(_.droppedByBlock).get, Vector(Vector("motion_y_dup", "constant_conf")))
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 3.0, 1e-10)
    assertEqualsDouble(result.coefficient("nuis#01_1", 0).get, 5.0, 1e-10)
  }
