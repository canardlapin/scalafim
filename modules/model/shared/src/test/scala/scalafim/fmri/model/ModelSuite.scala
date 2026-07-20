package scalafim.fmri.model

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.image.{DMat, NeuroSpace}
import gale.linalg.Matrix

class ModelSuite extends munit.FunSuite:

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(3), tr = Seq(1.0))

  private def dataset: FmriDataset =
    val data = DMat.fromRows(
      Vector(
        Vector(1.0, 2.0, 3.0, 4.0),
        Vector(5.0, 6.0, 7.0, 8.0),
        Vector(9.0, 10.0, 11.0, 12.0)
      )
    )
    FmriDataset(
      backend = InMemoryDatasetBackend(DatasetId("demo"), data, NeuroSpace(Vector(2, 2, 1))),
      samplingFrame = samplingFrame
    )

  private def eventModel: EventModel =
    EventModel(
      terms = Vector.empty,
      samplingFrame = samplingFrame,
      designMatrix = Mat.fromRows(Vector(Vector(1.0), Vector(0.0), Vector(1.0))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )

  private def baselineModel: BaselineModel =
    BaselineModel.build(
      samplingFrame = samplingFrame,
      basis = BaselineBasis.Constant,
      intercept = Intercept.Global
    )

  test("FmriModel validates row alignment and combines design matrices") {
    val model = FmriModel(eventModel, baselineModel, dataset)
    assertEquals(model.nTimepoints, 3)
    assertEquals(model.nPredictors, 2)
    assertEquals(model.designMatrix.rows, 3)
    assertEquals(model.designMatrix.cols, 2)
    assertEquals(model.columnNames, Vector("task", "base_constant"))
    assertEquals(model.designBlock.columnNames, Vector("task", "base_constant"))
  }

  test("FitConfig validates algebraic options") {
    intercept[IllegalArgumentException] {
      ArOptions(structure = ArStructure.Ar(0))
    }
    intercept[IllegalArgumentException] {
      ArOptions(structure = ArStructure.Ar(2), rho = Some(0.2))
    }
    intercept[IllegalArgumentException] {
      ArOptions(structure = ArStructure.Ar(2), phi = Some(Vector(0.2)))
    }
    intercept[IllegalArgumentException] {
      ArOptions(structure = ArStructure.Iid, phi = Some(Vector(0.2)))
    }
    intercept[IllegalArgumentException] {
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(Vector.empty))
    }
    intercept[IllegalArgumentException] {
      FitConfig(lss = LssConfig(eps = 0.0))
    }

    val cfg = FitConfig(
      robust = RobustOptions(psi = RobustPsi.Huber()),
      autocorrelation = ArOptions(structure = ArStructure.Ar(2), phi = Some(Vector(0.2, -0.1))),
      lss = LssConfig(trialTerm = Some("trial"))
    )
    assertEquals(cfg.robust.maxIterations, 2)
    assertEquals(cfg.autocorrelation.phi, Some(Vector(0.2, -0.1)))
    assertEquals(cfg.lss.trialTerm, Some("trial"))
  }

  test("FitPlan summarizes model and fitting policy") {
    val cfg = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1)))
    val plan = FitPlan(FmriModel(eventModel, baselineModel, dataset), FitEngine.GeneralizedLeastSquares, cfg)
    assertEquals(plan.summary.timepoints, 3)
    assertEquals(plan.summary.predictors, 2)
    assertEquals(plan.summary.voxels, 4)
    assert(plan.summary.autocorrelated)
  }

  test("FitPlan can describe a LeastSquaresSeparate engine") {
    val plan = FitPlan(FmriModel(eventModel, baselineModel, dataset), FitEngine.LeastSquaresSeparate)
    assertEquals(plan.summary.engine, FitEngine.LeastSquaresSeparate)
    assertEquals(plan.summary.predictors, 2)
  }

  test("FitPlan validates strategy-dependent dimensions at construction") {
    val model = FmriModel(eventModel, baselineModel, dataset)

    val shortWeights = FitControls(
      volumeWeighting = ModelVolumeWeighting.Fixed(TimepointWeights.unsafe(Vector(1.0, 1.0)))
    )
    assert(FitPlan.make(model, FitStrategy.OrdinaryLeastSquares(shortWeights)).isLeft)

    val nuisance = FitControls(
      nuisanceProjection = ModelNuisanceProjection.MatrixProjection(
        NuisanceMatrix.unsafe(Matrix.dense(2, 1)(1.0, 2.0)),
        Regularization.Auto
      )
    )
    assert(FitPlan.make(model, FitStrategy.OrdinaryLeastSquares(nuisance)).isLeft)

    val ar = AutocorrelationConfig.unsafe(order = 1, censoredTimepoints = Vector(3))
    assert(FitPlan.make(model, FitStrategy.GeneralizedLeastSquares(ar)).isLeft)
  }

  test("FitStrategy rejects illegal legacy engine/config pairings") {
    val arConfig = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1)))
    val ols = FitStrategy.fromLegacy(FitEngine.OrdinaryLeastSquares, arConfig)
    assert(ols.isLeft)

    val robustConfig = FitConfig(robust = RobustOptions(psi = RobustPsi.Huber()))
    val robust = FitStrategy.fromLegacy(FitEngine.RobustLeastSquares, robustConfig)
    assertEquals(robust.map(_.engine), Right(FitEngine.RobustLeastSquares))
  }

  test("LSS strategy validates selected trial term against model terms") {
    val model = FmriModel(eventModel, baselineModel, dataset)
    val lss = LssStrategyConfig.unsafe(trialTerm = Some("trial"))
    val plan = FitPlan.make(model, FitStrategy.LeastSquaresSeparate(lss))
    assert(plan.isLeft)
  }
