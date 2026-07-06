package scalafim.fmri.model

import scalafim.dataset.{DatasetEvents, DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{Intercept, NuisanceCheck}
import scalafim.fmri.design.data.Column
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.image.{DMat, NeuroSpace}

class ModelBuilderSuite extends munit.FunSuite:

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset(events: DatasetEvents): FmriDataset =
    val data = DMat.fromRows(
      Vector(
        Vector(1.0),
        Vector(3.0),
        Vector(5.0),
        Vector(7.0)
      )
    )
    FmriDataset(
      backend = InMemoryDatasetBackend(DatasetId("builder-demo"), data, NeuroSpace(Vector(1, 1, 1))),
      samplingFrame = samplingFrame,
      events = events
    )

  test("eventsTable infers typed columns from dataset event rows") {
    val table = FmriModelBuilder.eventsTable(
      DatasetEvents(
        Vector(
          Map("onset" -> "0.0", "run" -> "1", "condition" -> "face", "keep" -> "true"),
          Map("onset" -> "1.5", "run" -> "1", "condition" -> "house", "keep" -> "false")
        )
      )
    )

    assertEquals(table.nrows, 2)
    assertEquals(table.column("run"), Column.Ints(Vector(1, 1)))
    assertEquals(table.doubles("onset"), Vector(0.0, 1.5))
    assertEquals(table.strings("condition"), Vector("face", "house"))
    assertEquals(table.bools("keep"), Vector(true, false))
  }

  test("buildModel constructs an inspectable model from dataset-resident events") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )

    val model = FmriModelBuilder.buildModel(
      dataset(events),
      ModelBuildSpec("onset ~ covariate(task)", baselineIntercept = Intercept.Global)
    )

    assertEquals(model.nTimepoints, 4)
    assertEquals(model.columnNames, Vector("task", "base_constant"))
    assertEquals(model.eventModel.designMatrix.rows, 4)
    assertEquals(model.eventModel.designMatrix.cols, 1)
    assertEquals(model.eventModel.designMatrix.col(0).data.toVector, Vector(0.0, 1.0, 2.0, 3.0))
  }

  test("eventsTable rejects ragged event rows before model construction") {
    val err = intercept[IllegalArgumentException] {
      FmriModelBuilder.eventsTable(
        DatasetEvents(
          Vector(
            Map("onset" -> "0.0", "task" -> "1.0"),
            Map("onset" -> "1.0")
          )
        )
      )
    }

    assert(err.getMessage.contains("dataset event row 1"))
  }

  test("buildModel rejects missing block and duration columns explicitly") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )

    intercept[IllegalArgumentException] {
      FmriModelBuilder.buildModel(
        dataset(events),
        ModelBuildSpec("onset ~ covariate(task)", blockColumn = Some("run"))
      )
    }

    intercept[IllegalArgumentException] {
      FmriModelBuilder.buildModel(
        dataset(events),
        ModelBuildSpec("onset ~ covariate(task)", durationColumn = Some("duration"))
      )
    }
  }

  test("buildModel attaches nuisance regressors and exposes diagnostics") {
    val events = DatasetEvents(
      Vector.tabulate(4)(i => Map("onset" -> i.toString, "task" -> i.toString))
    )
    val nuisance = Mat.fromRows(
      Vector(
        Vector(0.0, 0.0, 1.0),
        Vector(1.0, 1.0, 1.0),
        Vector(2.0, 2.0, 1.0),
        Vector(3.0, 3.0, 1.0)
      )
    )

    val model = FmriModelBuilder.buildModel(
      dataset(events),
      ModelBuildSpec(
        "onset ~ covariate(task)",
        baselineIntercept = Intercept.Global,
        nuisance = Some(
          NuisanceRegressors(
            matrices = Vector(nuisance),
            names = Some(Vector(Vector("motion_x", "motion_x_dup", "constant_conf"))),
            check = NuisanceCheck.Drop
          )
        )
      )
    )

    val report = model.baselineModel.nuisanceReport.get
    assertEquals(report.retainedByBlock, Vector(Vector("motion_x")))
    assertEquals(report.droppedByBlock, Vector(Vector("motion_x_dup", "constant_conf")))
    assertEquals(model.baselineModel.termKeys, Vector("drift", "nuisance"))
    assertEquals(model.baselineModel.colIndices("nuisance").length, 1)
  }
