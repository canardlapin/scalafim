package scalafim.fmri.fit

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{
  AcquisitionContext,
  DataSelection,
  DatasetEvents,
  DatasetId,
  DatasetKey,
  DatasetResponseSchema,
  DatasetRunQuery,
  FmriDataset,
  IndexSelection,
  InMemoryDatasetBackend,
  OpenedDataset,
  ResponseKey
}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{
  ConvolvedTerm,
  EventModel,
  EventTermColumnRole
}
import scalafim.fmri.fit.fixtures.ReducedRankGlsFmriregFixtures
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  ArCoefficientSpec,
  AutocorrelationConfig,
  FitConfig,
  FitEngine,
  FitPlan,
  FitStrategy,
  FmriModel,
  FmriModelBuilder,
  LatentSketchConfig,
  LatentSketchMethod,
  LowRankComponentSpec,
  LssStrategyConfig,
  ModelBuildSpec,
  ReducedRankBootstrapConfig,
  ReducedRankComponentSpec,
  ReducedRankGlsConfig,
  ReducedRankInferencePolicy
}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import scalafim.response.{
  InMemoryResponseSource,
  ResponseSchemaId,
  SourceId,
  UnitId
}
import gale.linalg.{DMat, DVec}

import scala.concurrent.ExecutionContext.Implicits.{global as executionContext}

class FitPlanExecutorSuite extends munit.FunSuite:

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset: FmriDataset =
    val data = ImageDMat.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 1.0),
        Vector(5.0, 0.0),
        Vector(7.0, -1.0)
      )
    )
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("ols-demo"), data, NeuroSpace(Vector(2, 1, 1))),
      samplingFrame = samplingFrame
    )

  private def model: FmriModel =
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

  private def pcaModel: FmriModel =
    val signal = Vector(1.0, 3.0, 5.0, 7.0)
    val data = ImageDMat.fromRows(signal.map(value => Vector(value, -2.0 * value, 0.5 * value)))
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
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("pca-sketch-demo"), data, NeuroSpace(Vector(3, 1, 1))),
        samplingFrame = samplingFrame
      )
    FmriModel(eventModel, baseline, dataset)

  private def partitionedReducedRankModel: FmriModel =
    val frame = SamplingFrame(blockLens = Seq(6), tr = Seq(1.0))
    val eventRows =
      ReducedRankGlsFmriregFixtures.partitionedDesign.toRows.map(row => row.take(2))
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = frame,
        designMatrix = Mat.fromRows(eventRows),
        columnNames = Vector("task_a", "task_b"),
        termSpans = Vector(0 -> 2),
        colIndices = Map("task" -> Vector(0, 1))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = frame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("rrr-partitioned-demo"),
          ImageDMat.fromRows(ReducedRankGlsFmriregFixtures.partitionedResponse.toRows),
          NeuroSpace(Vector(3, 1, 1))
        ),
        samplingFrame = frame
      )
    FmriModel(eventModel, baseline, dataset)

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  private def assertVectorClose(actual: DVec, expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  private def assertTaskStandardErrors(
      result: DenseFmriFitResult,
      expected: DMat,
      tol: Double
  ): Unit =
    assertEquals(expected.rows, 2)
    assertEquals(expected.cols, result.voxels)
    var task = 0
    while task < expected.rows do
      var voxel = 0
      while voxel < expected.cols do
        assertEqualsDouble(result.standardErrors(task, voxel), expected(task, voxel), tol)
        voxel += 1
      task += 1
    var voxel = 0
    while voxel < result.voxels do
      assertEqualsDouble(result.standardErrors(2, voxel), 0.0, 0.0)
      voxel += 1

  private def embedTaskCovariance(task: DMat): DMat =
    val out = Array.ofDim[Double](9)
    var row = 0
    while row < 2 do
      var col = 0
      while col < 2 do
        out(row * 3 + col) = task(row, col)
        col += 1
      row += 1
    scalafim.fmri.fit.GaleTestMatrix.fromArray(3, 3, out)

  private def selectTaskCovariance(full: DMat): DMat =
    scalafim.fmri.fit.GaleTestMatrix.fromRows(
      Vector(
        Vector(full(0, 0), full(0, 1)),
        Vector(full(1, 0), full(1, 1))
      )
    )

  private def assertCoefficientCovarianceClose(
      actual: CoefficientCovariance,
      expected: CoefficientCovariance,
      tol: Double
  ): Unit =
    assertEquals(actual.scope, expected.scope)
    assertEquals(actual.matrixCount, expected.matrixCount)
    var i = 0
    while i < actual.matrixCount do
      assertMatrixClose(actual.matrices(i), expected.matrices(i), tol)
      i += 1

  test("FitPlanExecutor runs end-to-end OLS against an in-memory dataset") {
    val result = FitPlanExecutor.unsafeFit(FitPlan(model)).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.engine, FitEngine.OrdinaryLeastSquares)
    assertEquals(result.columnNames, Vector("task", "base_constant"))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.timepoints, Vector(0, 1, 2, 3))
    assertEquals(result.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 1).get, 2.0, 1e-10)
    assertEqualsDouble(result.residualVariance(0), 0.0, 1e-10)
    assertEqualsDouble(result.residualVariance(1), 0.0, 1e-10)
  }

  test("OpenedDatasetFitExecutor confines effects to the attached response read") {
    val plan = FitPlan(model)
    val attachedDataset = plan.model.dataset
    val schemaId = ResponseSchemaId.unsafe("fit-opened-schema")
    val schema =
      DatasetResponseSchema
        .fromDataset(attachedDataset, schemaId, UnitId.unsafe("unit"))
        .fold(error => fail(error.message), identity)
    val source =
      InMemoryResponseSource
        .copyFromRowMajor[IO](
          SourceId.unsafe("fit-opened-source"),
          schema,
          Array[Double](
            1.0, 2.0,
            3.0, 1.0,
            5.0, 0.0,
            7.0, -1.0
          )
        )
        .fold(error => fail(error.message), identity)
    val acquisition =
      AcquisitionContext
        .volume(
          attachedDataset,
          DatasetKey.unsafe("sub-01"),
          ResponseKey.unsafe("bold"),
          schemaId,
          UnitId.unsafe("unit")
        )
        .fold(error => fail(error.message), identity)
    val opened =
      OpenedDataset
        .attach(attachedDataset, source, acquisition)
        .toEither
        .fold(
          issues =>
            fail(issues.toNonEmptyList.toList.map(_.message).mkString("; ")),
          identity
        )

    OpenedDatasetFitExecutor
      .fit(opened, plan, DatasetRunQuery.All)
      .value
      .unsafeToFuture()
      .map: evaluated =>
        val result =
          evaluated
            .fold(error => fail(error.message), identity)
            .asInstanceOf[DenseFmriFitResult]
        assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
        assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
  }

  test("FitPlanExecutor preserves voxel selections in the result surface") {
    val result = FitPlanExecutor.unsafeFit(
      FitPlan(model),
      DataSelection(voxels = IndexSelection.indices(1))
    ).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.voxelIndices, Vector(1))
    assertEquals(result.voxels, 1)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
    assertEquals(result.coefficient("task", 0), None)
  }

  test("FitPlanExecutor rejects unsupported engine/configuration paths") {
    val result = FitPlan.makeLegacy(model, engine = FitEngine.GeneralizedLeastSquares)
    assert(result.left.toOption.exists {
      error => error.message.contains("AR(p)") && error.message.contains("not iid")
    })
  }

  test("FitInterpreters exposes executable built-ins") {
    val builtIns = Vector(
      FitEngine.OrdinaryLeastSquares,
      FitEngine.GeneralizedLeastSquares,
      FitEngine.RunwiseLeastSquares,
      FitEngine.RobustLeastSquares,
      FitEngine.LeastSquaresSeparate,
      FitEngine.LatentSketch,
      FitEngine.ReducedRankGls
    )

    builtIns.foreach { engine =>
      val interpreter = FitInterpreters.forEngine(engine).toOption.get
      assertEquals(interpreter.engine, engine)
    }
  }

  test("LatentSketch full-rank identity fallback matches OLS and fixed sketches execute explicitly") {
    val expected = FitPlanExecutor.unsafeFit(FitPlan(model)).asInstanceOf[DenseFmriFitResult]
    val actual =
      FitPlanExecutor
        .unsafeFit(FitPlan(model, FitStrategy.LatentSketch()))
        .asInstanceOf[DenseFmriFitResult]

    assertEquals(actual.engine, FitEngine.LatentSketch)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-10)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-10)

    val compressed =
      FitPlan(
        model,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(1),
            method = LatentSketchMethod.ContiguousVoxelAveraging
          )
        )
      )

    val sketched = FitPlanExecutor.unsafeFit(compressed).asInstanceOf[DenseFmriFitResult]
    assertEquals(sketched.engine, FitEngine.LatentSketch)
    assertEquals(sketched.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
    assertEquals(sketched.coefficientCovariance.matrixCount, 2)
    assertEqualsDouble(sketched.coefficient("task", 0).get, 0.5, 1e-10)
    assertEqualsDouble(sketched.coefficient("task", 1).get, 0.5, 1e-10)
    assertEqualsDouble(sketched.coefficient("base_constant", 0).get, 1.5, 1e-10)
    assertEqualsDouble(sketched.coefficient("base_constant", 1).get, 1.5, 1e-10)
    assertEqualsDouble(sketched.residualVariance(0), 11.75, 1e-10)
    assertEqualsDouble(sketched.residualVariance(1), 11.75, 1e-10)

    val t = TContrast("task", Map("task" -> 1.0)).evaluate(sketched).toOption.get
    assert(t.statistics.toVector.forall(_.isFinite))
  }

  test("LatentSketch principal components recovers rank-one response coefficients") {
    val plan = FitPlan(pcaModel)
    val expected = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val pcaPlan =
      FitPlan(
        pcaModel,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(1),
            method = LatentSketchMethod.PrincipalComponents
          )
        )
      )

    val actual = FitPlanExecutor.unsafeFit(pcaPlan).asInstanceOf[DenseFmriFitResult]

    assertEquals(actual.engine, FitEngine.LatentSketch)
    assertEquals(actual.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
    assertEquals(actual.coefficientCovariance.matrixCount, 3)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-8)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-8)
    var voxel = 0
    while voxel < actual.voxels do
      assertEqualsDouble(actual.residualVariance(voxel), expected.residualVariance(voxel), 1e-8)
      voxel += 1
  }

  test("ReducedRankGls full-rank fallback matches GLS and full task ranks execute through fallback") {
    val ar = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.25))
    val expected =
      FitPlanExecutor
        .unsafeFit(FitPlan(model, FitEngine.GeneralizedLeastSquares, FitConfig(autocorrelation = ar)))
        .asInstanceOf[DenseFmriFitResult]
    val actual =
      FitPlanExecutor
        .unsafeFit(FitPlan(model, FitEngine.ReducedRankGls, FitConfig(autocorrelation = ar)))
        .asInstanceOf[DenseFmriFitResult]

    assertEquals(actual.engine, FitEngine.ReducedRankGls)
    assertEquals(actual.autocorrelation, expected.autocorrelation)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-10)
    assertEquals(actual.inferenceScope.allowedIndices(actual.predictors), Vector(0))
    assertEquals(actual.inference.method, CoefficientInferenceMethod.ReducedRankConditional)
    assert(TContrast("baseline", Map("base_constant" -> 1.0)).evaluate(actual).isLeft)

    val fixedFullTask =
      FitPlan(
        model,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(
            components = ReducedRankComponentSpec.unsafeFixed(1),
            autocorrelation = AutocorrelationConfig.unsafe(
              order = 1,
              coefficients = ArCoefficientSpec.Rho(0.25)
            )
          )
        )
      )

    val reduced = FitPlanExecutor.unsafeFit(fixedFullTask).asInstanceOf[DenseFmriFitResult]

    assertEquals(reduced.engine, FitEngine.ReducedRankGls)
    assertEquals(reduced.autocorrelation.map(_.sharedNormalizedCovariance), Some(true))
    assertMatrixClose(reduced.coefficients.value, expected.coefficients.value, 1e-10)
    assertEquals(reduced.inferenceScope.allowedIndices(reduced.predictors), Vector(0))
    assertEquals(reduced.inference.method, CoefficientInferenceMethod.ReducedRankConditional)
    assert(TContrast("baseline", Map("base_constant" -> 1.0)).evaluate(reduced).isLeft)
    assert(reduced.residualVariance.toVector.forall(_.isFinite))
    assert(reduced.standardErrors.value.copyData.forall(_.isFinite))
  }

  test("ReducedRankGls rank-one coefficients match fmrireg QR-SVD fixture") {
    val design = DesignMatrix.unsafe(ReducedRankGlsFmriregFixtures.design)
    val response = ResponseBlock.unsafe(ReducedRankGlsFmriregFixtures.response)
    val rows = (0 until response.timepoints).toVector
    val partitions = Vector(RunPartition(0, rowIndices = rows, timepoints = rows))
    val config =
      ReducedRankGlsConfig.unsafe(
        components = ReducedRankComponentSpec.unsafeFixed(1),
        autocorrelation = AutocorrelationConfig.unsafe(
          order = 1,
          iterations = 0,
          coefficients = ArCoefficientSpec.Rho(0.0)
        )
      )
    val prepared =
      ReducedRankGlsPrepared
        .prepare(design, response, partitions, config, selectedVoxelIndices = Vector(0, 1))
        .fold(error => fail(error.message), identity)
    val result =
      prepared
        .fitBlock(FitBlockInput(design, response, voxelIndices = Vector(0, 1), timepoints = rows, partitions = partitions))
        .fold(error => fail(error.message), identity)

    assertEquals(result.engine, FitEngine.ReducedRankGls)
    assertMatrixClose(result.coefficients.value, ReducedRankGlsFmriregFixtures.rankOneCoefficients, 1e-10)
    ReducedRankGlsFmriregFixtures.rankOneResidualVariance.zipWithIndex.foreach { case (expected, voxel) =>
      assertEqualsDouble(result.residualVariance(voxel), expected, 1e-10)
    }
  }

  test("ReducedRankGls reduces event targets after residualizing nuisance predictors") {
    val plan =
      FitPlan(
        partitionedReducedRankModel,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(
            components = ReducedRankComponentSpec.unsafeFixed(1),
            autocorrelation = AutocorrelationConfig.unsafe(
              order = 1,
              iterations = 0,
              coefficients = ArCoefficientSpec.Rho(0.0)
            )
          )
        )
      )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.columnNames, Vector("task_a", "task_b", "base_constant"))
    assertEquals(
      result.inferenceScope,
      CoefficientInferenceScope.unsafeOnly(Vector(0, 1), "reduced-rank GLS target/event coefficient")
    )
    assertEquals(result.coefficientCovariance.scope, CoefficientCovarianceScope.Shared)
    assertMatrixClose(result.coefficients.value, ReducedRankGlsFmriregFixtures.partitionedRankOneCoefficients, 1e-10)
    ReducedRankGlsFmriregFixtures.partitionedRankOneResidualVariance.zipWithIndex.foreach { case (expected, voxel) =>
      assertEqualsDouble(result.residualVariance(voxel), expected, 1e-10)
    }

    assertEquals(result.inference.method, CoefficientInferenceMethod.ReducedRankConditional)
    assertEquals(result.coefficientCovariance.scope, CoefficientCovarianceScope.Shared)
    assertMatrixClose(
      result.coefficientCovariance.canonicalMatrix,
      embedTaskCovariance(ReducedRankGlsFmriregFixtures.partitionedTaskNormalizedCovariance),
      1e-12
    )
    assertVectorClose(result.inference.varianceScale, ReducedRankGlsFmriregFixtures.partitionedConditionalVariance, 1e-12)
    assertTaskStandardErrors(
      result,
      ReducedRankGlsFmriregFixtures.partitionedConditionalStandardErrors,
      1e-12
    )

    val taskContrast = TContrast("task_a", Map("task_a" -> 1.0)).evaluate(result).toOption.get
    assertVectorClose(taskContrast.statistics, ReducedRankGlsFmriregFixtures.partitionedTaskATStatistics, 1e-9)

    val taskF = FContrast(
      "task",
      Vector(Map("task_a" -> 1.0), Map("task_b" -> 1.0))
    ).evaluate(result).toOption.get
    assertVectorClose(taskF.statistics, ReducedRankGlsFmriregFixtures.partitionedTaskFStatistics, 1e-8)

    val baselineContrast = TContrast("baseline", Map("base_constant" -> 1.0)).evaluate(result)
    assert(baselineContrast.left.toOption.exists {
      case FitError.NonEstimableContrast(name, detail) =>
        name == "baseline" && detail.contains("base_constant") && detail.contains("target/event")
      case _ =>
        false
    })

    val mixedF = FContrast("task_plus_baseline", Vector(Map("task_a" -> 1.0), Map("base_constant" -> 1.0))).evaluate(result)
    assert(mixedF.left.toOption.exists {
      case FitError.NonEstimableContrast(name, detail) =>
        name == "task_plus_baseline" && detail.contains("base_constant") && detail.contains("target/event")
      case _ =>
        false
    })
  }

  test("ReducedRankGls bootstrap inference is deterministic and matches the R reference fixture") {
    val bootstrap =
      ReducedRankBootstrapConfig.unsafe(
        replicates = ReducedRankGlsFmriregFixtures.bootstrapReplicates,
        blockSize = ReducedRankGlsFmriregFixtures.bootstrapBlockSize,
        seed = ReducedRankGlsFmriregFixtures.bootstrapSeed
      )
    val plan =
      FitPlan(
        partitionedReducedRankModel,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(
            components = ReducedRankComponentSpec.unsafeFixed(1),
            autocorrelation = AutocorrelationConfig.unsafe(
              order = 1,
              iterations = 0,
              coefficients = ArCoefficientSpec.Rho(0.0)
            ),
            inference = ReducedRankInferencePolicy.Bootstrap(bootstrap)
          )
        )
      )

    val first = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val second = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEquals(
      first.inference.method,
      CoefficientInferenceMethod.ReducedRankBootstrap(
        ReducedRankGlsFmriregFixtures.bootstrapReplicates,
        ReducedRankGlsFmriregFixtures.bootstrapBlockSize,
        ReducedRankGlsFmriregFixtures.bootstrapSeed
      )
    )
    assertMatrixClose(first.standardErrors.value, second.standardErrors.value, 0.0)
    assertCoefficientCovarianceClose(first.coefficientCovariance, second.coefficientCovariance, 0.0)
    assertTaskStandardErrors(first, ReducedRankGlsFmriregFixtures.partitionedBootstrapStandardErrors, 1e-10)

    ReducedRankGlsFmriregFixtures.partitionedBootstrapCovariance.zipWithIndex.foreach { case (expected, voxel) =>
      val actual = selectTaskCovariance(first.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel))
      assertMatrixClose(actual, expected, 1e-10)
    }

    val task = TContrast("task_a", Map("task_a" -> 1.0)).evaluate(first).toOption.get
    assertVectorClose(task.statistics, ReducedRankGlsFmriregFixtures.partitionedBootstrapTaskATStatistics, 1e-8)
  }

  test("ReducedRankGls adaptive rank policies match fixed rank when the task spectrum selects one component") {
    def fitWith(components: ReducedRankComponentSpec): DenseFmriFitResult =
      val plan =
        FitPlan(
          partitionedReducedRankModel,
          FitStrategy.ReducedRankGls(
            ReducedRankGlsConfig.unsafe(
              components = components,
              autocorrelation = AutocorrelationConfig.unsafe(
                order = 1,
                iterations = 0,
                coefficients = ArCoefficientSpec.Rho(0.0)
              )
            )
          )
        )
      FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    val fixed = fitWith(ReducedRankComponentSpec.unsafeFixed(1))
    val energy = fitWith(ReducedRankComponentSpec.unsafeEnergyRetained(0.98))
    val rssBudget = fitWith(ReducedRankComponentSpec.unsafeResidualSumsOfSquaresBudget(0.5))

    assertEquals(energy.engine, FitEngine.ReducedRankGls)
    assertEquals(rssBudget.engine, FitEngine.ReducedRankGls)
    assertMatrixClose(energy.coefficients.value, fixed.coefficients.value, 1e-10)
    assertMatrixClose(rssBudget.coefficients.value, fixed.coefficients.value, 1e-10)
    assertMatrixClose(energy.standardErrors.value, fixed.standardErrors.value, 1e-10)
    assertMatrixClose(rssBudget.standardErrors.value, fixed.standardErrors.value, 1e-10)
    energy.residualVariance.toVector.zip(fixed.residualVariance.toVector).foreach { case (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-10)
    }
    rssBudget.residualVariance.toVector.zip(fixed.residualVariance.toVector).foreach { case (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-10)
    }
  }

  test("FitPlanExecutor runs LeastSquaresSeparate from a builder-created trialwise model") {
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(math.sin(x / 5.0) + x / 20.0, math.cos(x / 7.0) - x / 30.0)
    }
    val events = DatasetEvents(
      Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString))
    )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-trialwise-demo"),
          ImageDMat.fromRows(rows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = events
      )
    val plan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        strategy = FitStrategy.LeastSquaresSeparate()
      )
    )

    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val fixedEventCols =
      plan.model.eventModel.columnNames.indices.filterNot((trialCols ++ aggregateCols).toSet.contains).toVector
    val trialDesign =
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      )
    val fixedEvent =
      MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols)
    val fixedBaseline =
      MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
    val fixed =
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(fixedEvent, fixedBaseline),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    val expected = LeastSquaresSeparate.unsafeFit(
      trialDesign,
      MatrixAdapters.responseBlock(series).toOption.get,
      fixed
    )

    assertEquals(result.engine, FitEngine.LeastSquaresSeparate)
    assertEquals(result.trialNames, trialCols.map(plan.model.eventModel.columnNames))
    assert(aggregateCols.map(plan.model.eventModel.columnNames).forall(name => !result.trialNames.contains(name)))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.lssDiagnostics.fixedRank, expected.diagnostics.fixedRank)
    assertMatrixClose(result.coefficients.value, expected.coefficients.value, tol = 1e-10)
  }

  test("FitPlanExecutor selects LSS trial columns by metadata rather than span position") {
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(math.sin(x / 5.0) + x / 20.0, math.cos(x / 7.0) - x / 30.0)
    }
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-reordered-trialwise-demo"),
          ImageDMat.fromRows(rows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString)))
      )
    val basePlan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        strategy = FitStrategy.LeastSquaresSeparate()
      )
    )
    val plan = moveTrialwiseMeanFirst(basePlan, termKey = "trial")
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val fixedEventCols =
      plan.model.eventModel.columnNames.indices.filterNot((trialCols ++ aggregateCols).toSet.contains).toVector

    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val expected = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      ),
      MatrixAdapters.responseBlock(series).toOption.get,
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(
          MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols),
          MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
        ),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    )

    assertEquals(result.trialNames, trialCols.map(plan.model.eventModel.columnNames))
    assert(aggregateCols.map(plan.model.eventModel.columnNames).forall(name => !result.trialNames.contains(name)))
    assertMatrixClose(result.coefficients.value, expected.coefficients.value, tol = 1e-10)
  }

  test("FitPlanExecutor selects LSS aggregate columns by metadata rather than column name") {
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(math.sin(x / 5.0) + x / 20.0, math.cos(x / 7.0) - x / 30.0)
    }
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-renamed-aggregate-demo"),
          ImageDMat.fromRows(rows),
          NeuroSpace(Vector(2, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString)))
      )
    val basePlan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        strategy = FitStrategy.LeastSquaresSeparate()
      )
    )
    val plan = renameTrialwiseAggregate(basePlan, termKey = "trial", aggregateName = "trial_shared_signal")
    val series = plan.model.dataset.series()
    val trialCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.Trial)
    val aggregateCols = eventColumnsByRole(plan, "trial", EventTermColumnRole.TrialAggregate)
    val fixedEventCols =
      plan.model.eventModel.columnNames.indices.filterNot((trialCols ++ aggregateCols).toSet.contains).toVector

    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[LssFmriFitResult]
    val expected = LeastSquaresSeparate.unsafeFit(
      LssTrialDesign.unsafe(
        MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, trialCols),
        trialCols.map(plan.model.eventModel.columnNames)
      ),
      MatrixAdapters.responseBlock(series).toOption.get,
      LssFixedDesign.unsafe(
        MatrixAdapters.bindColumns(
          MatrixAdapters.fromHrfMatrixRowsCols(plan.model.eventModel.designMatrix, series.timepoints, fixedEventCols),
          MatrixAdapters.fromHrfMatrixRows(plan.model.baselineModel.designMatrix, series.timepoints)
        ),
        fixedEventCols.map(plan.model.eventModel.columnNames) ++ plan.model.baselineModel.columnNames
      )
    )

    assertEquals(aggregateCols.map(plan.model.eventModel.columnNames), Vector("trial_shared_signal"))
    assert(!plan.model.eventModel.columnNames.exists(_ == "trial_mean"))
    assertEquals(result.trialNames, trialCols.map(plan.model.eventModel.columnNames))
    assert(!result.trialNames.contains("trial_shared_signal"))
    assertMatrixClose(result.coefficients.value, expected.coefficients.value, tol = 1e-10)
  }

  test("FitPlanExecutor requires explicit LSS trial term when a model has multiple trialwise terms") {
    val nTime = 24
    val nTrials = 3
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("lss-ambiguous-demo"),
          ImageDMat.fromRows(Vector.tabulate(nTime)(i => Vector(i.toDouble))),
          NeuroSpace(Vector(1, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = DatasetEvents(Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 5).toString)))
      )
    val ambiguousPlan = FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(label = \"trial_a\") + trialwise(label = \"trial_b\")",
        baselineIntercept = Intercept.Global,
        strategy = FitStrategy.LeastSquaresSeparate()
      )
    )

    val ambiguous = FitPlanExecutor.fit(ambiguousPlan)
    assertEquals(ambiguous.left.toOption, Some(FitError.AmbiguousLssTrialTerms(Vector("trial_a", "trial_b"))))

    val selectedPlan =
      ambiguousPlan.copy(strategy = FitStrategy.LeastSquaresSeparate(LssStrategyConfig.unsafe(trialTerm = Some("trial_a"))))
    val selected = FitPlanExecutor.unsafeFit(selectedPlan).asInstanceOf[LssFmriFitResult]
    assertEquals(selected.trialNames.length, nTrials)
    assert(selected.trialNames.forall(_.startsWith("trial_a_")))
  }

  private def moveTrialwiseMeanFirst(plan: FitPlan, termKey: String): FitPlan =
    val eventModel = plan.model.eventModel
    val termIndex = eventModel.terms.indexWhere(_._1 == termKey)
    require(termIndex >= 0, s"missing term $termKey")
    val (start, endExcl) = eventModel.termSpans(termIndex)
    val (_, term) = eventModel.terms(termIndex)
    val convolved = term.asInstanceOf[ConvolvedTerm]
    val roles = convolved.resolvedColumnRoles
    val aggregateLocal = roles.indexOf(EventTermColumnRole.TrialAggregate)
    require(aggregateLocal >= 0, s"term $termKey has no aggregate column")
    val localOrder = (aggregateLocal +: convolved.columnNames.indices.filterNot(_ == aggregateLocal).toVector).toVector
    val reorderedTerm =
      convolved.copy(
        data = reorderColumns(convolved.data, localOrder),
        columnNames = localOrder.map(convolved.columnNames),
        columnRoles = localOrder.map(roles)
      )

    val globalOrder = eventModel.columnNames.indices.map { col =>
      if col >= start && col < endExcl then start + localOrder(col - start) else col
    }.toVector
    val model =
      plan.model.copy(
        eventModel = eventModel.copy(
          terms = eventModel.terms.updated(termIndex, termKey -> reorderedTerm),
          designMatrix = reorderColumns(eventModel.designMatrix, globalOrder),
          columnNames = globalOrder.map(eventModel.columnNames)
        )
    )
    plan.copy(model = model)

  private def renameTrialwiseAggregate(plan: FitPlan, termKey: String, aggregateName: String): FitPlan =
    val eventModel = plan.model.eventModel
    val termIndex = eventModel.terms.indexWhere(_._1 == termKey)
    require(termIndex >= 0, s"missing term $termKey")
    val (start, endExcl) = eventModel.termSpans(termIndex)
    val (_, term) = eventModel.terms(termIndex)
    val convolved = term.asInstanceOf[ConvolvedTerm]
    val roles = convolved.resolvedColumnRoles
    val aggregateLocal = roles.indexOf(EventTermColumnRole.TrialAggregate)
    require(aggregateLocal >= 0, s"term $termKey has no aggregate column")

    val renamedLocalNames = convolved.columnNames.updated(aggregateLocal, aggregateName)
    val renamedGlobalNames = eventModel.columnNames.zipWithIndex.map { case (name, col) =>
      if col >= start && col < endExcl then renamedLocalNames(col - start) else name
    }

    plan.copy(
      model = plan.model.copy(
        eventModel = eventModel.copy(
          terms = eventModel.terms.updated(termIndex, termKey -> convolved.copy(columnNames = renamedLocalNames)),
          columnNames = renamedGlobalNames
        )
      )
    )

  private def eventColumnsByRole(plan: FitPlan, termKey: String, role: EventTermColumnRole): Vector[Int] =
    val eventModel = plan.model.eventModel
    val termIndex = eventModel.terms.indexWhere(_._1 == termKey)
    require(termIndex >= 0, s"missing term $termKey")
    val (start, endExcl) = eventModel.termSpans(termIndex)
    val (_, term) = eventModel.terms(termIndex)
    val roles = term.resolvedColumnRoles
    require(roles.length == endExcl - start, s"term $termKey role metadata length mismatch")
    roles.zipWithIndex.collect { case (r, localCol) if r == role => start + localCol }

  private def reorderColumns(matrix: Mat, columnOrder: IndexedSeq[Int]): Mat =
    require(columnOrder.length == matrix.cols, "column order length must match matrix cols")
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        val sourceCol = columnOrder(col)
        require(sourceCol >= 0 && sourceCol < matrix.cols, s"column $sourceCol out of bounds")
        out(row * matrix.cols + col) = matrix.data(row * matrix.cols + sourceCol)
        col += 1
      row += 1
    Mat.unsafe(matrix.rows, matrix.cols, out)
