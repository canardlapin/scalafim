package scalafim.fmri.fit

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.contrast.ContrastSpec
import scalafim.fmri.design.data.{Column, DataTable}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.design.formula.EventModelBuilder
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{FitEngine, FitPlan, FitSummary, FmriModel}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

class InferenceSuite extends munit.FunSuite:

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def noisyModel: FmriModel =
    val data = DMat.fromRows(Vector(Vector(1.0), Vector(2.0), Vector(2.0), Vector(4.0)))
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(DatasetId("inference-demo"), data, NeuroSpace(Vector(1, 1, 1))),
        samplingFrame = samplingFrame
      )
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = samplingFrame,
        designMatrix = Mat.fromRows(Vector(Vector(0.0), Vector(1.0), Vector(2.0), Vector(3.0))),
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

  private def attachedContrastModel: FmriModel =
    val sf = SamplingFrame(blockLens = Seq(24), tr = Seq(1.0))
    val events = DataTable.fromColumns(
      "onset" -> Column.Doubles(Vector(1.0, 4.0, 7.0, 10.0, 13.0, 16.0)),
      "cond" -> Column.Strings(Vector("A", "B", "A", "B", "A", "B"))
    )
    val cset =
      ContrastSpec.ContrastSet(
        ContrastSpec.Pair(
          name = "A_vs_B",
          A = cell => cell("cond") == "A",
          B = cell => cell("cond") == "B"
        )
      )
    val eventModel = EventModelBuilder.build(
      formula = "onset ~ hrf(cond, id = task, contrasts = taskContrasts)",
      data = events,
      samplingFrame = sf,
      blockIds = Vector.fill(6)(0),
      contrastSets = Map("taskContrasts" -> cset)
    )
    val baseline =
      BaselineModel.build(
        samplingFrame = sf,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    val design = eventModel.designMatrix ++ baseline.designMatrix
    val columnNames = eventModel.columnNames ++ baseline.columnNames
    val beta = columnNames.map { name =>
      if name.contains("cond.A") then 1.0
      else if name.contains("cond.B") then -0.5
      else 0.25
    }
    val rows = Vector.tabulate(design.rows) { r =>
      var fitted = 0.0
      var c = 0
      while c < design.cols do
        fitted += design(r, c) * beta(c)
        c += 1
      val noise =
        r % 3 match
          case 0 => 0.1
          case 1 => -0.05
          case _ => 0.0
      Vector(fitted + noise)
    }
    val dataset =
      FmriDataset(
        backend = InMemoryDatasetBackend(DatasetId("attached-contrast-demo"), DMat.fromRows(rows), NeuroSpace(Vector(1, 1, 1))),
        samplingFrame = sf
      )
    FmriModel(eventModel, baseline, dataset)

  private def zeroResidualVarianceResult: DenseFmriFitResult =
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(1.0, 1.0),
          Vector(1.0, 2.0),
          Vector(1.0, 3.0)
        )
      )
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(3.0), Vector(5.0), Vector(7.0)))
    )
    val fit = Ols.unsafeFit(design, response)
    val residualVariance = DoubleVector.unsafe(Array(0.0))
    val covariance = CoefficientCovariance.unsafeShared(fit.normalizedCovariance)
    DenseFmriFitResult(
      coefficients = fit.coefficients,
      inference = CoefficientInference.unsafeFromExisting(
        CoefficientInferenceScope.All,
        fit.standardErrors,
        covariance,
        residualVariance,
        fit.residualDegreesOfFreedom
      ),
      residualVariance = residualVariance,
      residualDegreesOfFreedom = fit.residualDegreesOfFreedom,
      columnNames = Vector("base_constant", "task"),
      voxelIndices = Vector(0),
      timepoints = Vector(0, 1, 2, 3),
      engine = FitEngine.OrdinaryLeastSquares,
      summary = FitSummary(
        engine = FitEngine.OrdinaryLeastSquares,
        timepoints = 4,
        predictors = 2,
        voxels = 1,
        robust = false,
        autocorrelated = false
      )
    )

  test("Dense OLS results expose standard errors and diagnostics") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]

    assertEqualsDouble(result.coefficient("task", 0).get, 0.9, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 0.9, 1e-10)
    assertEqualsDouble(result.residualVariance(0), 0.35, 1e-10)
    assertEqualsDouble(result.standardErrors(0, 0), math.sqrt(0.07), 1e-10)
    assertEqualsDouble(result.standardErrors(1, 0), math.sqrt(0.245), 1e-10)
    assertEquals(result.diagnostics.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
  }

  test("T contrasts align weights by column name") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]
    val contrast = TContrast("task", Map("task" -> 1.0))
    val evaluated = contrast.evaluate(result).toOption.get

    assertEquals(evaluated.voxelIndices, Vector(0))
    assertEquals(evaluated.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(evaluated.estimates(0), 0.9, 1e-10)
    assertEqualsDouble(evaluated.standardErrors(0), math.sqrt(0.07), 1e-10)
    assertEqualsDouble(evaluated.statistics(0), 0.9 / math.sqrt(0.07), 1e-10)
  }

  test("T contrasts can evaluate an explicitly inference-ready dense fit") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]
    val ready = result.inferenceReady.toOption.get
    val evaluated = TContrast("task", Map("task" -> 1.0)).evaluate(ready).toOption.get

    assertEquals(evaluated.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEquals(evaluated.selectedVoxels.toVector, Vector(0))
  }

  test("T contrasts reject unknown columns") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]
    val evaluated = TContrast("bad", Map("missing" -> 1.0)).evaluate(result)
    assertEquals(evaluated.left.toOption, Some(FitError.UnknownContrastColumn("missing")))
  }

  test("F contrasts evaluate single-df statistics, df, and voxel alignment") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]
    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(result).toOption.get
    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get

    assertEquals(f.voxelIndices, Vector(0))
    assertEquals(f.numeratorDegreesOfFreedom, 1)
    assertEquals(f.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(f.estimates(0, 0), 0.9, 1e-10)
    assertEqualsDouble(f.statistics(0), t.statistics(0) * t.statistics(0), 1e-10)
  }

  test("F contrasts evaluate multi-df tests") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]
    val f = FContrast(
      "task_and_baseline",
      Vector(
        Map("task" -> 1.0),
        Map("base_constant" -> 1.0)
      )
    ).evaluate(result).toOption.get

    assertEquals(f.numeratorDegreesOfFreedom, 2)
    assertEquals(f.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(f.estimates(0, 0), 0.9, 1e-10)
    assertEqualsDouble(f.estimates(1, 0), 0.9, 1e-10)
    assertEqualsDouble(f.statistics(0), 24.3 / 2.0 / 0.35, 1e-10)
  }

  test("contrasts use voxelwise coefficient covariance when supplied") {
    val firstCovariance =
      DoubleMatrix.fromRows(
        Vector(
          Vector(0.25, 0.0),
          Vector(0.0, 1.0)
        )
      )
    val secondCovariance =
      DoubleMatrix.fromRows(
        Vector(
          Vector(1.0, 0.0),
          Vector(0.0, 1.0)
        )
      )
    val covariance = CoefficientCovariance.unsafeVoxelwise(Vector(firstCovariance, secondCovariance))
    val residualVariance = DoubleVector.fromSeq(Vector(1.0, 1.0))
    val residualDegreesOfFreedom = ResidualDegreesOfFreedom.unsafe(8)
    val result =
      DenseFmriFitResult(
        coefficients = CoefficientBlock(
          DoubleMatrix.fromRows(
            Vector(
              Vector(2.0, 2.0),
              Vector(0.0, 0.0)
            )
          )
        ),
        inference = CoefficientInference.unsafeFromExisting(
          CoefficientInferenceScope.All,
          StandardErrorBlock(
            DoubleMatrix.fromRows(
              Vector(
                Vector(0.5, 1.0),
                Vector(1.0, 1.0)
              )
            )
          ),
          covariance,
          residualVariance,
          residualDegreesOfFreedom
        ),
        residualVariance = residualVariance,
        residualDegreesOfFreedom = residualDegreesOfFreedom,
        columnNames = Vector("task", "base_constant"),
        voxelIndices = Vector(10, 20),
        timepoints = Vector.range(0, 10),
        engine = FitEngine.GeneralizedLeastSquares,
        summary = FitSummary(
          engine = FitEngine.GeneralizedLeastSquares,
          timepoints = 10,
          predictors = 2,
          voxels = 2,
          robust = false,
          autocorrelated = true
        )
      )

    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(result).toOption.get

    assertEqualsDouble(t.standardErrors(0), 0.5, 1e-12)
    assertEqualsDouble(t.standardErrors(1), 1.0, 1e-12)
    assertEqualsDouble(t.statistics(0), 4.0, 1e-12)
    assertEqualsDouble(t.statistics(1), 2.0, 1e-12)
    assertEqualsDouble(f.statistics(0), 16.0, 1e-12)
    assertEqualsDouble(f.statistics(1), 4.0, 1e-12)
  }

  test("contrasts reject zero residual variance instead of emitting non-finite statistics") {
    val result = zeroResidualVarianceResult

    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result)
    assert(t.left.toOption.exists {
      case FitError.NonEstimableContrast("task", detail) => detail.contains("contrast variance")
      case _                                             => false
    })

    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(result)
    assert(f.left.toOption.exists {
      case FitError.NonEstimableContrast("task", detail) => detail.contains("residual variance")
      case _                                             => false
    })
  }

  test("F contrasts reject unknown and non-estimable columns explicitly") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(noisyModel)).asInstanceOf[DenseFmriFitResult]

    val unknown = FContrast("bad", Vector(Map("missing" -> 1.0))).evaluate(result)
    assertEquals(unknown.left.toOption, Some(FitError.UnknownContrastColumn("missing")))

    val duplicate = FContrast(
      "duplicate",
      Vector(
        Map("task" -> 1.0),
        Map("task" -> 1.0)
      )
    ).evaluate(result)

    assert(duplicate.left.toOption.exists {
      case FitError.NonEstimableContrast("duplicate", _) => true
      case _                                             => false
    })
  }

  test("DesignContrasts evaluates attached t contrasts and generated F contrasts") {
    val model = attachedContrastModel
    val result = FitPlanExecutor.unsafeFit(FitPlan(model)).asInstanceOf[DenseFmriFitResult]

    val attached = DesignContrasts.attachedTContrasts(model)
    assertEquals(attached.map(_.name), Vector("task#A_vs_B"))
    val t = attached.head.evaluate(result).toOption.get
    assertEquals(t.voxelIndices, Vector(0))
    assertEquals(t.residualDegreesOfFreedom, result.residualDegreesOfFreedom)
    assert(t.statistics(0).isFinite)

    val generated = DesignContrasts.generatedFContrasts(model)
    assertEquals(generated.map(_.name), Vector("task#cond"))
    val f = generated.head.evaluate(result).toOption.get
    assertEquals(f.voxelIndices, Vector(0))
    assertEquals(f.numeratorDegreesOfFreedom, 1)
    assertEquals(f.residualDegreesOfFreedom, result.residualDegreesOfFreedom)
    assert(f.statistics(0).isFinite)
  }
