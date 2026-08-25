package scalafim.fmri.fit

import scalafim.image.SampleSpaces

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
  DvarsWeightEstimator,
  FitConfig,
  FitEngine,
  FitPlan,
  FitStrategy,
  FixedWeightAlignment,
  FmriModel,
  FmriModelBuilder,
  LatentSketchConfig,
  LatentSketchMethod,
  LowRankComponentSpec,
  LssStrategyConfig,
  MissingDataPolicy,
  ModelBuildSpec,
  NuisanceProjection,
  ReducedRankBootstrapConfig,
  ReducedRankComponentSpec,
  ReducedRankGlsConfig,
  ReducedRankInferencePolicy,
  Regularization,
  RobustConfig,
  VolumeWeighting
}
import scalafim.response.{
  InMemoryResponseSource,
  ResponseSchemaId,
  SourceId,
  UnitId
}
import gale.linalg.{DMat, DVec}

class FitPlanExecutorSuite extends munit.FunSuite:

  private given scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  private def samplingFrame: SamplingFrame =
    SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))

  private def dataset: FmriDataset =
    val data = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0),
        Vector(3.0, 1.0),
        Vector(5.0, 0.0),
        Vector(7.0, -1.0)
      )
    )
    FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("ols-demo"), data, SampleSpaces(Vector(2, 1, 1))),
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
    val data = GaleTestMatrix.fromRows(signal.map(value => Vector(value, -2.0 * value, 0.5 * value)))
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
        backend = InMemoryDatasetBackend(DatasetId("pca-sketch-demo"), data, SampleSpaces(Vector(3, 1, 1))),
        samplingFrame = samplingFrame
      )
    FmriModel(eventModel, baseline, dataset)

  private def missingDataModel: FmriModel =
    val data = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, -1.0),
        Vector(2.0, Double.NaN, 0.0),
        Vector(2.0, 0.0, 3.0),
        Vector(4.0, -1.0, 2.0)
      )
    )
    val source = model
    val missingDataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("missing-data-demo"), data, SampleSpaces(Vector(3, 1, 1))),
        samplingFrame = samplingFrame
      )
    source.copy(dataset = missingDataset)

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
          GaleTestMatrix.fromRows(ReducedRankGlsFmriregFixtures.partitionedResponse.toRows),
          SampleSpaces(Vector(3, 1, 1))
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
    val result = FitPlanExecutor.fitDense(FitPlan(model)).toOption.getOrElse(fail("OLS should produce a dense result"))

    assertEquals(result.engine, FitEngine.OrdinaryLeastSquares)
    assertEquals(result.columnNames, Vector("task", "base_constant"))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.timepoints, Vector(0, 1, 2, 3))
    assertEquals(result.coefficientAxis, FitPlan(model).coefficientAxis)
    assertEquals(result.coefficientAxis.map(_.columnIds), Some(Vector(
      model.designBlock.columnIds(0),
      model.designBlock.columnIds(1)
    )))
    assertEquals(result.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    val rankComparison = result.rankPreviewComparison.toOption.getOrElse(fail("full OLS fit should compare with its design preview"))
    assert(rankComparison.exact)
    assertEquals(rankComparison.preview.pivotOrder, rankComparison.fitted.pivotOrder.map(_.id))
    assertEquals(rankComparison.preview.numericalRank, result.predictors)
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 1.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 1).get, 2.0, 1e-10)
    assertEqualsDouble(result.residualVariance(0), 0.0, 1e-10)
    assertEqualsDouble(result.residualVariance(1), 0.0, 1e-10)
  }

  test("missing-data propagation excludes whole non-finite voxels without weakening ResponseBlock") {
    val strictPlan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.Error)
    )
    val propagatePlan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.Propagate)
    )
    val excludePlan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.ExcludeVoxel)
    )

    assertEquals(
      FitPlanExecutor.fitDense(strictPlan).left.toOption,
      Some(FitError.NonFiniteInput("response block"))
    )

    val expected = FitPlanExecutor
      .fitDense(strictPlan, DataSelection(voxels = IndexSelection.indices(0, 2)))
      .fold(error => fail(error.message), identity)
    val actual = FitPlanExecutor.fitDense(propagatePlan).fold(error => fail(error.message), identity)
    val explicitlyExcluded = FitPlanExecutor.fitDense(excludePlan).fold(error => fail(error.message), identity)

    assertEquals(actual.voxelIndices, Vector(0, 2))
    assertEquals(actual.fitExclusions, Vector(VoxelInferenceExclusion(1, VoxelFitStatus.NonFinite)))
    assertEquals(actual.voxelStatus(1), Some(VoxelFitStatus.NonFinite))
    assertEquals(actual.coefficient("task", 1), None)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-12)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-12)
    assertMatrixClose(explicitlyExcluded.coefficients.value, actual.coefficients.value, 1e-12)
    assertEquals(explicitlyExcluded.fitExclusions, actual.fitExclusions)
    assert(actual.coefficients.value.toRows.flatten.forall(_.isFinite))
    assert(actual.standardErrors.value.toRows.flatten.forall(_.isFinite))
    assert(actual.preparationProvenance.exists(_.applied.exists(
      _.step == ResponsePreparationStep.MissingData(MissingDataPolicy.Propagate)
    )))

    val contrast = TContrast("task", Map("task" -> 1.0)).evaluate(actual).fold(error => fail(error.message), identity)
    assertEquals(contrast.voxelIndices, Vector(0, 2))
    assertEquals(contrast.excludedVoxels, actual.fitExclusions)
    assert(contrast.statistics.toVector.forall(_.isFinite))

    val statuses = AnalysisProvenance.fromResult(actual).voxelStatuses.getOrElse(fail("missing voxel statuses"))
    assert(statuses.contains(VoxelFitStatusRecord(1, VoxelFitStatus.NonFinite)))
  }

  test("missing-data propagation reports an all-excluded selection explicitly") {
    val plan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.Propagate)
    )
    val selection = DataSelection(voxels = IndexSelection.indices(1))

    FitPlanExecutor.fit(plan, selection) match
      case Left(FitError.AllVoxelsExcluded(exclusions)) =>
        assertEquals(exclusions, Vector(VoxelInferenceExclusion(1, VoxelFitStatus.NonFinite)))
      case other =>
        fail(s"expected typed all-voxel exclusion, got $other")
  }

  test("voxel-specific row omission preserves pattern-specific OLS geometry and inference") {
    val plan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )

    val result = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity)
    val patterned = result match
      case value: PatternedFmriFitResult => value
      case other => fail(s"expected patterned missing-response result, got $other")

    assertEquals(patterned.voxelIndices, Vector(0, 1, 2))
    assertEquals(patterned.timepoints, Vector(0, 1, 2, 3))
    assertEquals(patterned.patternResults.length, 2)
    val complete = patterned.patternForVoxel(0).getOrElse(fail("complete pattern"))
    val incomplete = patterned.patternForVoxel(1).getOrElse(fail("incomplete pattern"))
    assertEquals(complete.sourceTimepoints, Vector(0, 1, 2, 3))
    assertEquals(complete.sourceVoxels, Vector(0, 2))
    assertEquals(incomplete.sourceTimepoints, Vector(0, 2, 3))
    assertEquals(incomplete.sourceVoxels, Vector(1))

    val completeResult = patterned.resultForVoxel(0).get.asInstanceOf[DenseFmriFitResult]
    val incompleteResult = patterned.resultForVoxel(1).get.asInstanceOf[DenseFmriFitResult]
    assertEquals(completeResult.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(2))
    assertEquals(incompleteResult.residualDegreesOfFreedom, ResidualDegreesOfFreedom.unsafe(1))

    val explicit = FitPlanExecutor
      .fitDense(
        FitPlan(missingDataModel, FitEngine.OrdinaryLeastSquares, FitConfig()),
        DataSelection(
          time = IndexSelection.indices(0, 2, 3),
          voxels = IndexSelection.indices(1)
        )
      )
      .fold(error => fail(error.message), identity)
    assertMatrixClose(incompleteResult.coefficients.value, explicit.coefficients.value, 1e-12)
    assertMatrixClose(incompleteResult.standardErrors.value, explicit.standardErrors.value, 1e-12)

    val contrast = TContrast("task", Map("task" -> 1.0))
      .evaluate(patterned)
      .fold(error => fail(error.message), identity)
    assertEquals(
      contrast.residualDegreesOfFreedomByVoxel,
      Map(
        0 -> ResidualDegreesOfFreedom.unsafe(2),
        1 -> ResidualDegreesOfFreedom.unsafe(1),
        2 -> ResidualDegreesOfFreedom.unsafe(2)
      )
    )
    assertEquals(contrast.voxelIndices, Vector(0, 1, 2))
    assert(contrast.patternResults.flatMap(_._2.statistics.toVector).forall(_.isFinite))

    val fContrast = FContrast("task-f", Vector(Map("task" -> 1.0)))
      .evaluate(patterned)
      .fold(error => fail(error.message), identity)
    assertEquals(fContrast.residualDegreesOfFreedomByVoxel, contrast.residualDegreesOfFreedomByVoxel)
    assertEquals(fContrast.voxelIndices, Vector(0, 1, 2))
    assert(fContrast.patternResults.flatMap(_._2.statistics.toVector).forall(_.isFinite))
  }

  test("fitDense rejects unlike voxel observation patterns explicitly") {
    val plan = FitPlan(
      missingDataModel,
      FitEngine.OrdinaryLeastSquares,
      FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    FitPlanExecutor.fitDense(plan) match
      case Left(FitError.UnsupportedMissingDataPolicy(detail)) =>
        assert(detail.contains("do not share one dense inference geometry"))
      case other => fail(s"expected patterned dense-result rejection, got $other")
  }

  test("row omission preserves an explicitly reordered OLS time selection") {
    val maskedPlan = FitPlan(
      missingDataModel,
      FitEngine.OrdinaryLeastSquares,
      FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val strictPlan = FitPlan(missingDataModel, FitEngine.OrdinaryLeastSquares, FitConfig())
    val selection = DataSelection(time = IndexSelection.indices(3, 0, 2, 1))
    val patterned = FitPlanExecutor.fit(maskedPlan, selection)
      .fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]
    val incompletePattern = patterned.patternForVoxel(1).getOrElse(fail("incomplete pattern"))
    val actual = patterned.resultForVoxel(1).get.asInstanceOf[DenseFmriFitResult]
    val expected = FitPlanExecutor.fitDense(
      strictPlan,
      DataSelection(
        time = IndexSelection.indices(3, 0, 2),
        voxels = IndexSelection.indices(1)
      )
    ).fold(error => fail(error.message), identity)

    assertEquals(incompletePattern.rows, Vector(0, 1, 2))
    assertEquals(incompletePattern.sourceTimepoints, Vector(3, 0, 2))
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-12)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-12)
  }

  test("row omission rejects response-derived weights and explicit nuisance matrices at their policy boundary") {
    val dvarsPlan = FitPlan(
      missingDataModel,
      FitEngine.OrdinaryLeastSquares,
      FitConfig(
        missingData = MissingDataPolicy.OmitRowsPerVoxel,
        volumeWeighting = VolumeWeighting.Estimated(DvarsWeightEstimator())
      )
    )
    val nuisancePlan = FitPlan(
      missingDataModel,
      FitEngine.OrdinaryLeastSquares,
      FitConfig(
        missingData = MissingDataPolicy.OmitRowsPerVoxel,
        nuisanceProjection = NuisanceProjection.MatrixProjection(
          DMat.tabulate(4, 1)((row, _) => row.toDouble),
          Regularization.Fixed(0.0)
        )
      )
    )

    assert(FitPlanExecutor.fit(dvarsPlan).left.toOption.exists {
      case FitError.UnsupportedVolumeWeighting(detail) => detail.contains("observation patterns")
      case _ => false
    })
    assert(FitPlanExecutor.fit(nuisancePlan).left.toOption.exists {
      case FitError.UnsupportedMissingDataPolicy(detail) => detail.contains("nuisance-projection matrix")
      case _ => false
    })
  }

  test("fixed AR row omission matches explicit selection and records the reset gap") {
    val ar = ArOptions(
      structure = ArStructure.Ar(1),
      iterations = 1,
      exactFirst = true,
      rho = Some(0.25)
    )
    val maskedPlan = FitPlan(
      missingDataModel,
      FitEngine.GeneralizedLeastSquares,
      FitConfig(
        autocorrelation = ar,
        missingData = MissingDataPolicy.OmitRowsPerVoxel
      )
    )
    val strictPlan = FitPlan(
      missingDataModel,
      FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ar)
    )
    val patterned = FitPlanExecutor.fit(maskedPlan).fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]
    val actual = patterned.resultForVoxel(1).get.asInstanceOf[DenseFmriFitResult]
    val expected = FitPlanExecutor.fitDense(
      strictPlan,
      DataSelection(
        time = IndexSelection.indices(0, 2, 3),
        voxels = IndexSelection.indices(1)
      )
    ).fold(error => fail(error.message), identity)

    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-12)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-12)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    val whitening = actual.autocorrelation.toVector.flatMap(_.whitening.segments)
    assertEquals(whitening.map(segment => segment.startTimepoint -> segment.endTimepointExclusive), Vector(0 -> 1, 2 -> 4))
  }

  test("row omission subsets selected-row weights by original observation position") {
    val weightedDataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("masked-selected-row-weights"),
        GaleTestMatrix.fromRows(
          Vector(
            Vector(1.0, 2.0, -1.0),
            Vector(2.0, Double.NaN, 0.0),
            Vector(2.0, 0.4, 3.0),
            Vector(4.0, -1.0, 2.0)
          )
        ),
        SampleSpaces(Vector(3, 1, 1))
      ),
      samplingFrame = samplingFrame
    )
    val weightedModel = missingDataModel.copy(dataset = weightedDataset)
    val outerWeights = Vector(1.0, 4.0, 9.0, 16.0)
    val maskedPlan = FitPlan(
      weightedModel,
      FitEngine.OrdinaryLeastSquares,
      FitConfig(
        missingData = MissingDataPolicy.OmitRowsPerVoxel,
        volumeWeighting = VolumeWeighting.Fixed(outerWeights, FixedWeightAlignment.SelectedRows)
      )
    )
    val strictPlan = FitPlan(
      weightedModel,
      FitEngine.OrdinaryLeastSquares,
      FitConfig(
        volumeWeighting = VolumeWeighting.Fixed(
          Vector(outerWeights(0), outerWeights(2), outerWeights(3)),
          FixedWeightAlignment.SelectedRows
        )
      )
    )
    val patterned = FitPlanExecutor.fit(maskedPlan).fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]
    val actual = patterned.resultForVoxel(1).get.asInstanceOf[DenseFmriFitResult]
    val expected = FitPlanExecutor.fitDense(
      strictPlan,
      DataSelection(
        time = IndexSelection.indices(0, 2, 3),
        voxels = IndexSelection.indices(1)
      )
    ).fold(error => fail(error.message), identity)

    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-12)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-12)
  }

  test("row omission reports an all-missing voxel without contaminating finite voxels") {
    val data = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, Double.NaN, -1.0),
        Vector(2.0, Double.PositiveInfinity, 0.0),
        Vector(2.0, Double.NegativeInfinity, 3.0),
        Vector(4.0, Double.NaN, 2.0)
      )
    )
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("all-missing-voxel"), data, SampleSpaces(Vector(3, 1, 1))),
      samplingFrame = samplingFrame
    )
    val plan = FitPlan(
      model.copy(dataset = dataset),
      FitEngine.OrdinaryLeastSquares,
      FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val result = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity)
      .asInstanceOf[DenseFmriFitResult]

    assertEquals(result.voxelIndices, Vector(0, 2))
    assertEquals(
      result.fitExclusions,
      Vector(VoxelInferenceExclusion(1, VoxelFitStatus.NoObservedResponses))
    )
    assertEquals(result.voxelStatus(1), Some(VoxelFitStatus.NoObservedResponses))
    assert(result.coefficients.value.toRows.flatten.forall(_.isFinite))
  }

  test("row omission gives insufficient residual degrees of freedom a typed voxel status") {
    val data = GaleTestMatrix.fromRows(
      Vector(
        Vector(1.0, 2.0, -1.0),
        Vector(2.0, Double.NaN, 0.0),
        Vector(2.0, Double.NaN, 3.0),
        Vector(4.0, 5.0, 2.0)
      )
    )
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("zero-df-observed-pattern"), data, SampleSpaces(Vector(3, 1, 1))),
      samplingFrame = samplingFrame
    )
    val plan = FitPlan(
      model.copy(dataset = dataset),
      FitEngine.OrdinaryLeastSquares,
      FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val result = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity)
      .asInstanceOf[DenseFmriFitResult]

    assertEquals(result.voxelIndices, Vector(0, 2))
    assertEquals(
      result.fitExclusions,
      Vector(VoxelInferenceExclusion(1, VoxelFitStatus.InsufficientResidualDegreesOfFreedom))
    )
  }

  test("row-omission exclusions preserve requested voxel order across failure classes") {
    val data = GaleTestMatrix.fromRows(
      Vector(
        Vector(Double.NaN, 2.0, 1.0),
        Vector(Double.PositiveInfinity, Double.NaN, 2.0),
        Vector(Double.NegativeInfinity, Double.NaN, 2.0),
        Vector(Double.NaN, Double.NaN, 4.0)
      )
    )
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("ordered-masked-response-exclusions"),
        data,
        SampleSpaces(Vector(3, 1, 1))
      ),
      samplingFrame = samplingFrame
    )
    val plan = FitPlan(
      model.copy(dataset = dataset),
      FitEngine.OrdinaryLeastSquares,
      FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val result = FitPlanExecutor
      .fit(plan, DataSelection(voxels = IndexSelection.indices(2, 1, 0)))
      .fold(error => fail(error.message), identity)
      .asInstanceOf[DenseFmriFitResult]

    assertEquals(result.voxelIndices, Vector(2))
    assertEquals(
      result.fitExclusions,
      Vector(
        VoxelInferenceExclusion(1, VoxelFitStatus.RankDeficientObservedDesign),
        VoxelInferenceExclusion(0, VoxelFitStatus.NoObservedResponses)
      )
    )
  }

  test("estimated AR row omission matches a separately estimated explicit-row GLS fit") {
    val frame = SamplingFrame(blockLens = Seq(16), tr = Seq(1.0))
    val task = Vector.tabulate(16)(row => math.sin((row + 1).toDouble * 0.41) + row.toDouble * 0.025)
    val states = Array.fill(3)(0.0)
    val finiteRows = Vector.tabulate(16) { row =>
      Vector.tabulate(3) { voxel =>
        val innovation = math.sin((row + 1).toDouble * 1.73 + voxel.toDouble * 0.91) * 0.18
        states(voxel) = 0.42 * states(voxel) + innovation
        0.7 + (0.35 + voxel.toDouble * 0.2) * task(row) + states(voxel)
      }
    }
    val rows = finiteRows.updated(5, finiteRows(5).updated(1, Double.NaN))
    val eventModel = EventModel(
      terms = Vector.empty,
      samplingFrame = frame,
      designMatrix = Mat.fromRows(task.map(value => Vector(value))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(
      samplingFrame = frame,
      basis = BaselineBasis.Constant,
      intercept = Intercept.Global
    )
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("estimated-ar-masked-response"),
        GaleTestMatrix.fromRows(rows),
        SampleSpaces(Vector(3, 1, 1))
      ),
      samplingFrame = frame
    )
    val ar = ArOptions(
      structure = ArStructure.Ar(1),
      iterations = 1,
      global = true,
      exactFirst = false
    )
    val maskedPlan = FitPlan(
      FmriModel(eventModel, baseline, dataset),
      FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ar, missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val strictPlan = FitPlan(
      maskedPlan.model,
      FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ar)
    )
    val retained = (0 until 16).filterNot(_ == 5).toVector
    val patterned = FitPlanExecutor.fit(maskedPlan).fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]
    val actual = patterned.resultForVoxel(1).get.asInstanceOf[DenseFmriFitResult]
    val expected = FitPlanExecutor.fitDense(
      strictPlan,
      DataSelection(
        time = IndexSelection.indices(retained*),
        voxels = IndexSelection.indices(1)
      )
    ).fold(error => fail(error.message), identity)

    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-12)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-12)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(
      actual.autocorrelation.toVector.flatMap(_.whitening.segments)
        .map(segment => segment.startTimepoint -> segment.endTimepointExclusive),
      Vector(0 -> 5, 6 -> 16)
    )
    assertEqualsDouble(
      actual.autocorrelation.get.runs.head.rho,
      expected.autocorrelation.get.runs.head.rho,
      1e-12
    )
  }

  test("fitDense rejects engines with a different result shape") {
    FitPlanExecutor.fitDense(FitPlan(model, FitEngine.RunwiseLeastSquares)) match
      case Left(FitError.NonDenseFitResult(FitEngine.RunwiseLeastSquares)) => ()
      case other => fail(s"expected a typed non-dense-result failure, got $other")
  }

  test("public fit failures name aliased structural columns") {
    val duplicateEvent = EventModel(
      terms = Vector.empty,
      samplingFrame = samplingFrame,
      designMatrix = Mat.fromRows(Vector.fill(4)(Vector(1.0))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(
      samplingFrame = samplingFrame,
      basis = BaselineBasis.Constant,
      intercept = Intercept.Global
    )
    val duplicateModel = FmriModel(duplicateEvent, baseline, dataset)
    val plan = FitPlan(duplicateModel)

    FitPlanExecutor.fit(plan) match
      case Left(FitError.StructuralRankDeficientDesign(report)) =>
        val axisIds = plan.coefficientAxis.toVector.flatMap(_.columnIds)
        assertEquals(report.numericalRank, 1)
        assertEquals(report.predictorCount, 2)
        assertEquals(report.pivotOrder.map(_.id).toSet, axisIds.toSet)
        assertEquals(report.aliasedColumnIds.length, 1)
        assert(axisIds.contains(report.aliasedColumnIds.head))
      case other =>
        fail(s"expected structural rank-deficiency evidence, got $other")

    val nearEvent = EventModel(
      terms = Vector.empty,
      samplingFrame = samplingFrame,
      designMatrix = Mat.fromRows(
        Vector(
          Vector(1.0, 1.0),
          Vector(2.0, 2.0),
          Vector(3.0, 3.0),
          Vector(4.0, 4.0 + 1.0e-15)
        )
      ),
      columnNames = Vector("task", "near_task"),
      termSpans = Vector(0 -> 2),
      colIndices = Map("task" -> Vector(0), "near_task" -> Vector(1))
    )
    val noBaseline = BaselineModel.build(
      samplingFrame = samplingFrame,
      basis = BaselineBasis.Constant,
      intercept = Intercept.None
    )
    val nearPlan = FitPlan(FmriModel(nearEvent, noBaseline, dataset))
    FitPlanExecutor.fit(nearPlan) match
      case Left(FitError.StructuralRankDeficientDesign(report)) =>
        assertEquals(report.numericalRank, 2)
        assertEquals(report.predictorCount, 3)
        assertEquals(report.aliasedColumnIds.length, 1)
        assert(nearPlan.coefficientAxis.exists(axis => axis.columnIds.contains(report.aliasedColumnIds.head)))
      case other =>
        fail(s"expected near-singular structural rank-deficiency evidence, got $other")
  }

  test("GLS retains structural rank evidence and explains preview differences") {
    val ar = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.0))
    val result = FitPlanExecutor
      .unsafeFit(FitPlan(model, FitEngine.GeneralizedLeastSquares, FitConfig(autocorrelation = ar)))
      .asInstanceOf[DenseFmriFitResult]
    val report = result.structuralRankReport.toOption.getOrElse(fail("GLS should retain a structural rank report"))
    val comparison = result.rankPreviewComparison.toOption.getOrElse(fail("GLS should compare against the design preview"))

    assertEquals(report.pivotOrder.map(_.id).toSet, result.coefficientAxis.toVector.flatMap(_.columnIds).toSet)
    assertEquals(report.numericalRank, result.predictors)
    assert(comparison.differences.contains(RankPreviewDifference.GeneralizedLeastSquares))
  }

  test("every supported dense engine preserves the same structural coefficient axis") {
    val ar = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.0))
    val robust = RobustConfig.huber(maxIterations = 2).toOption.get
    val plans = Vector(
      FitPlan(model),
      FitPlan(model, FitEngine.GeneralizedLeastSquares, FitConfig(autocorrelation = ar)),
      FitPlan(model, FitEngine.RunwiseLeastSquares),
      FitPlan(model, FitStrategy.RobustLeastSquares(robust)),
      FitPlan(model, FitStrategy.LatentSketch()),
      FitPlan(model, FitStrategy.ReducedRankGls())
    )

    plans.foreach { plan =>
      val result = FitPlanExecutor.unsafeFit(plan)
      assertEquals(result.coefficientAxis, plan.coefficientAxis, plan.engine.toString)
      assertEquals(result.coefficientAxis.map(_.columnIds), plan.coefficientAxis.map(_.columnIds), plan.engine.toString)
    }
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
          GaleTestMatrix.fromRows(rows),
          SampleSpaces(Vector(2, 1, 1))
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
    assertEquals(result.coefficientAxis.map(_.columnIds), Some(trialCols.map(plan.model.designBlock.columnIds)))
    assertEquals(result.coefficientAxis.map(_.designFingerprint), plan.coefficientAxis.map(_.designFingerprint))
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
          GaleTestMatrix.fromRows(rows),
          SampleSpaces(Vector(2, 1, 1))
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
          GaleTestMatrix.fromRows(rows),
          SampleSpaces(Vector(2, 1, 1))
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
          GaleTestMatrix.fromRows(Vector.tabulate(nTime)(i => Vector(i.toDouble))),
          SampleSpaces(Vector(1, 1, 1))
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
