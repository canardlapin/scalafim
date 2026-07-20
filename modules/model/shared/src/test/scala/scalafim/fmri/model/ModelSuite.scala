package scalafim.fmri.model

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.DoubleMatrix

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
        NuisanceMatrix.unsafe(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0)))),
        Regularization.Auto
      )
    )
    assert(FitPlan.make(model, FitStrategy.OrdinaryLeastSquares(nuisance)).isLeft)

    val ar = AutocorrelationConfig.unsafe(order = 1, censoredTimepoints = Vector(3))
    assert(FitPlan.make(model, FitStrategy.GeneralizedLeastSquares(ar)).isLeft)
  }

  test("Low-rank strategies validate component requests before execution") {
    val model = FmriModel(eventModel, baselineModel, dataset)

    val latent = FitPlan(
      model,
      FitStrategy.LatentSketch(
        LatentSketchConfig.unsafe(components = LowRankComponentSpec.unsafeFixed(model.dataset.shape.spatialSize))
      )
    )
    assertEquals(latent.summary.engine, FitEngine.LatentSketch)
    assertEquals(latent.summary.autocorrelated, false)

    val implicitCompressedLatent =
      FitPlan.make(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(components = LowRankComponentSpec.unsafeFixed(2))
        )
      )
    assert(implicitCompressedLatent.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "latent sketch method" && detail.contains("contiguous voxel averaging")
      case _ =>
        false
    })

    val explicitCompressedLatent =
      FitPlan(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(2),
            method = LatentSketchMethod.ContiguousVoxelAveraging
          )
        )
    )
    assertEquals(explicitCompressedLatent.summary.engine, FitEngine.LatentSketch)

    val principalComponents =
      FitPlan(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(2),
            method = LatentSketchMethod.PrincipalComponents
          )
        )
      )
    assertEquals(principalComponents.summary.engine, FitEngine.LatentSketch)

    val implicitFullPca =
      FitPlan.make(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(method = LatentSketchMethod.PrincipalComponents)
        )
      )
    assert(implicitFullPca.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "latent sketch method" && detail.contains("explicit fixed component count")
      case _ =>
        false
    })

    val tooManyPca =
      FitPlan.make(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(model.nTimepoints + 1),
            method = LatentSketchMethod.PrincipalComponents
          )
        )
      )
    assert(tooManyPca.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "latent sketch components" && detail.contains("maximum")
      case _ =>
        false
    })

    val tooManyLatent =
      FitPlan.make(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(components = LowRankComponentSpec.unsafeFixed(model.dataset.shape.spatialSize + 1))
        )
      )
    assert(tooManyLatent.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "latent sketch components" && detail.contains("maximum")
      case _ =>
        false
    })

    val reduced = FitPlan(
      model,
      FitStrategy.ReducedRankGls(
        ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeFixed(model.eventModel.columnNames.length))
      )
    )
    assertEquals(reduced.summary.engine, FitEngine.ReducedRankGls)
    assertEquals(reduced.summary.autocorrelated, true)

    val bootstrapConfig =
      ReducedRankBootstrapConfig.unsafe(
        replicates = 32,
        blockSize = 2,
        seed = 7
      )
    val bootstrapReduced = FitPlan(
      model,
      FitStrategy.ReducedRankGls(
        ReducedRankGlsConfig.unsafe(
          components = ReducedRankComponentSpec.unsafeFixed(model.eventModel.columnNames.length),
          inference = ReducedRankInferencePolicy.Bootstrap(bootstrapConfig)
        )
      )
    )
    assertEquals(bootstrapReduced.summary.engine, FitEngine.ReducedRankGls)

    assert(ReducedRankBootstrapConfig(replicates = 1).isLeft)
    assert(ReducedRankBootstrapConfig(blockSize = 0).isLeft)
    assert(ReducedRankBootstrapConfig(seed = -1).isLeft)

    val oversizedBlock = FitPlan.make(
      model,
      FitStrategy.ReducedRankGls(
        ReducedRankGlsConfig.unsafe(
          inference = ReducedRankInferencePolicy.Bootstrap(
            ReducedRankBootstrapConfig.unsafe(blockSize = model.nTimepoints + 1)
          )
        )
      )
    )
    assert(oversizedBlock.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "reduced-rank bootstrap block size" && detail.contains("timepoints")
      case _ =>
        false
    })

    val energyReduced = FitPlan(
      model,
      FitStrategy.ReducedRankGls(
        ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeEnergyRetained(0.9))
      )
    )
    assertEquals(energyReduced.summary.engine, FitEngine.ReducedRankGls)

    val rssReduced = FitPlan(
      model,
      FitStrategy.ReducedRankGls(
        ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeResidualSumsOfSquaresBudget(0.0))
      )
    )
    assertEquals(rssReduced.summary.engine, FitEngine.ReducedRankGls)

    val tooManyReduced =
      FitPlan.make(
        model,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(components = ReducedRankComponentSpec.unsafeFixed(model.eventModel.columnNames.length + 1))
        )
      )
    assert(tooManyReduced.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "reduced-rank GLS components" && detail.contains("maximum")
      case _ =>
        false
    })

    intercept[IllegalArgumentException] {
      ReducedRankComponentSpec.unsafeEnergyRetained(0.0)
    }
    intercept[IllegalArgumentException] {
      ReducedRankComponentSpec.unsafeEnergyRetained(1.1)
    }
    intercept[IllegalArgumentException] {
      ReducedRankComponentSpec.unsafeResidualSumsOfSquaresBudget(-1.0)
    }
  }

  test("AutocorrelationConfig rejects impossible GLS strategy states") {
    val bothPooling = AutocorrelationConfig(global = true, voxelwise = true)
    assert(bothPooling.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "AR pooling" && detail.contains("cannot both")
      case _ =>
        false
    })

    val fixedVoxelwise =
      AutocorrelationConfig(voxelwise = true, coefficients = ArCoefficientSpec.Rho(0.2))
    assert(fixedVoxelwise.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "AR voxelwise estimation" && detail.contains("estimated coefficients")
      case _ =>
        false
    })

    val reestimateFixed =
      AutocorrelationConfig(iterations = 2, coefficients = ArCoefficientSpec.Rho(0.2))
    assert(reestimateFixed.left.toOption.exists {
      case ModelError.InvalidParameter(name, detail) =>
        name == "AR iterations" && detail.contains("fixed coefficients")
      case _ =>
        false
    })

    val iterative = AutocorrelationConfig(iterations = 2).toOption.get
    assertEquals(iterative.iterations, 2)
    assertEquals(iterative.coefficients, ArCoefficientSpec.Estimate)
  }

  test("FitStrategy rejects illegal legacy engine/config pairings") {
    val arConfig = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1)))
    val ols = FitStrategy.fromLegacy(FitEngine.OrdinaryLeastSquares, arConfig)
    assert(ols.isLeft)

    val robustConfig = FitConfig(robust = RobustOptions(psi = RobustPsi.Huber()))
    val robust = FitStrategy.fromLegacy(FitEngine.RobustLeastSquares, robustConfig)
    assertEquals(robust.map(_.engine), Right(FitEngine.RobustLeastSquares))
  }

  test("RobustLeastSquares represents AR re-estimation as a typed policy") {
    val model = FmriModel(eventModel, baselineModel, dataset)
    val robust = RobustConfig.huber(maxIterations = 3).toOption.get
    val ar = AutocorrelationConfig.unsafe(order = 1)
    val strategy =
      FitStrategy.RobustLeastSquares(
        robust = robust,
        autocorrelation = RobustAutocorrelation.Reestimate(ar)
      )
    val plan = FitPlan(model, strategy)

    assertEquals(plan.summary.robust, true)
    assertEquals(plan.summary.autocorrelated, true)
    assertEquals(plan.config.robust.reestimateAutocorrelation, true)
    assertEquals(plan.config.autocorrelation.structure, ArStructure.Ar(1))

    val legacy =
      FitStrategy.fromLegacy(
        FitEngine.RobustLeastSquares,
        FitConfig(
          robust = RobustOptions(psi = RobustPsi.Huber(), reestimateAutocorrelation = true),
          autocorrelation = ArOptions(structure = ArStructure.Ar(1))
        )
      )
    assertEquals(legacy.map(_.config.robust.reestimateAutocorrelation), Right(true))
    assertEquals(legacy.map(_.config.autocorrelation.structure), Right(ArStructure.Ar(1)))

    val missingAr =
      FitStrategy.fromLegacy(
        FitEngine.RobustLeastSquares,
        FitConfig(robust = RobustOptions(psi = RobustPsi.Huber(), reestimateAutocorrelation = true))
      )
    assert(missingAr.isLeft)

    val voxelwiseAr =
      FitStrategy.fromLegacy(
        FitEngine.RobustLeastSquares,
        FitConfig(
          robust = RobustOptions(psi = RobustPsi.Huber(), reestimateAutocorrelation = true),
          autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true)
        )
      )
    assert(voxelwiseAr.isLeft)
  }

  test("LSS strategy validates selected trial term against model terms") {
    val model = FmriModel(eventModel, baselineModel, dataset)
    val lss = LssStrategyConfig.unsafe(trialTerm = Some("trial"))
    val plan = FitPlan.make(model, FitStrategy.LeastSquaresSeparate(lss))
    assert(plan.isLeft)
  }
