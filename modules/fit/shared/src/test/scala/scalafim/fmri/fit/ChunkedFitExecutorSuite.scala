package scalafim.fmri.fit

import scalafim.image.SampleSpaces

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  ArCoefficientSpec,
  AutocorrelationConfig,
  DvarsWeightEstimator,
  DvarsWeightFunction,
  FitConfig,
  FitEngine,
  FitPlan,
  FitStrategy,
  FmriModel,
  FmriModelBuilder,
  LatentSketchConfig,
  LatentSketchMethod,
  LowRankComponentSpec,
  MissingDataPolicy,
  ModelBuildSpec,
  ReducedRankBootstrapConfig,
  ReducedRankComponentSpec,
  ReducedRankGlsConfig,
  ReducedRankInferencePolicy,
  VolumeWeighting
}
import scalafim.image.SomeSampleSpace
import gale.linalg.{DMat, DVec}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ChunkedFitExecutorSuite extends munit.FunSuite:

  private val chunking = FitChunkingStrategy.unsafeByVoxelCount(2)
  private val singleVoxelChunking = FitChunkingStrategy.unsafeByVoxelCount(1)

  test("ChunkProgram rejects non-contiguous generic work ordinals") {
    val program = ChunkProgram.make(
      Vector(
        DummyWork(ChunkOrdinal.unsafe(0)),
        DummyWork(ChunkOrdinal.unsafe(2))
      )
    )

    assert(program.left.toOption.exists {
      case FitError.IncompatibleFitBlocks(detail) => detail.contains("ordinal 2")
      case _                                      => false
    })
  }

  test("SequentialChunkProgramInterpreter wraps failures with chunk identity") {
    val program = ChunkProgram.unsafe(
      Vector(
        DummyWork(ChunkOrdinal.unsafe(0)),
        DummyWork(ChunkOrdinal.unsafe(1))
      )
    )

    val result =
      SequentialChunkProgramInterpreter.execute(program) { work =>
        if work.ordinal.value == 1 then Left(FitError.EmptyResponse)
        else Right(work.ordinal.value)
      }

    assertEquals(result.left.toOption, Some(FitError.ChunkFailed(1, FitError.EmptyResponse)))
  }

  test("FutureChunkProgramInterpreter preserves ordinal order with bounded execution") {
    val program = ChunkProgram.unsafe(
      Vector(
        DummyWork(ChunkOrdinal.unsafe(0)),
        DummyWork(ChunkOrdinal.unsafe(1)),
        DummyWork(ChunkOrdinal.unsafe(2))
      )
    )

    FutureChunkProgramInterpreter
      .execute(program, FitParallelism.unsafe(2)) { work =>
        Future.successful(Right(work.ordinal.value))
      }
      .map { result =>
        val completed = result.toOption.get
        assertEquals(completed.map(_.ordinal.value), Vector(0, 1, 2))
        assertEquals(completed.map(_.result), Vector(0, 1, 2))
      }
  }

  test("FitChunkPlan exposes repeatable ordered voxel chunks") {
    val plan = FitPlan(olsModel)
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))

    val chunkPlan = FitChunkPlan.fromSelection(plan, selection, chunking).toOption.get

    assertEquals(chunkPlan.length, 2)
    assertEquals(chunkPlan.indexed.map(_.ordinal.value).toVector, Vector(0, 1))
    assertEquals(chunkPlan.indexed.map(_.voxelIndices).toVector, Vector(Vector(2, 0), Vector(1)))
    assertEquals(chunkPlan.iterator.map(_.voxelIndices).toVector, Vector(Vector(2, 0), Vector(1)))
    assertEquals(chunkPlan.iterator.map(_.voxelIndices).toVector, Vector(Vector(2, 0), Vector(1)))
  }

  test("FitChunkProgram exposes a repeatable scheduler-neutral work stream") {
    val plan = FitPlan(olsModel)
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))

    val program = FitChunkProgram.fromSelection(plan, selection, chunking).toOption.get

    assertEquals(program.chunkCount, 2)
    assertEquals(program.engine, FitEngine.OrdinaryLeastSquares)
    assertEquals(program.indexed.map(_.ordinal.value).toVector, Vector(0, 1))
    assertEquals(program.indexed.map(_.chunk.voxelIndices).toVector, Vector(Vector(2, 0), Vector(1)))
    assertEquals(program.iterator.map(_.chunk.voxelIndices).toVector, Vector(Vector(2, 0), Vector(1)))
    assertEquals(program.iterator.map(_.chunk.voxelIndices).toVector, Vector(Vector(2, 0), Vector(1)))
  }

  test("ChunkedFitExecutor matches unchunked OLS while preserving selected voxel order") {
    val plan = FitPlan(olsModel)
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]
    val actual =
      FitPlanExecutor
        .fitChunked(plan, selection, chunking)
        .toOption
        .get
        .asInstanceOf[DenseFmriFitResult]

    assertDenseClose(actual, expected)
  }

  test("single-voxel chunks preserve propagated non-finite exclusions") {
    val plan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.Propagate)
    )
    val expected = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity).asInstanceOf[DenseFmriFitResult]
    val actual = FitPlanExecutor
      .fitChunked(plan, chunking = singleVoxelChunking)
      .fold(error => fail(error.message), identity)
      .asInstanceOf[DenseFmriFitResult]

    assertDenseClose(actual, expected)
    assertEquals(actual.fitExclusions, Vector(VoxelInferenceExclusion(1, VoxelFitStatus.NonFinite)))
    assertEquals(
      TContrast("task", Map("task" -> 1.0)).evaluate(actual).toOption.map(_.excludedVoxels),
      Some(actual.fitExclusions)
    )
  }

  test("chunked voxel-specific row omission matches whole patterned OLS") {
    val plan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val expected = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]
    val actual = FitPlanExecutor
      .fitChunked(plan, chunking = singleVoxelChunking)
      .fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]

    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.patternResults.map(_.pattern), expected.patternResults.map(_.pattern))
    actual.voxelIndices.foreach { voxelIndex =>
      val left = actual.resultForVoxel(voxelIndex).get.asInstanceOf[DenseFmriFitResult]
      val right = expected.resultForVoxel(voxelIndex).get.asInstanceOf[DenseFmriFitResult]
      assertMatrixClose(left.coefficients.value, right.coefficients.value, 1e-12)
      assertMatrixClose(left.standardErrors.value, right.standardErrors.value, 1e-12)
      assertEquals(left.residualDegreesOfFreedom, right.residualDegreesOfFreedom)
    }
  }

  test("future single-voxel chunks preserve propagated non-finite exclusions") {
    val plan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.Propagate)
    )
    val expected = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, chunking = singleVoxelChunking, parallelism = FitParallelism.unsafe(2))
      .map { evaluated =>
        val actual = evaluated.fold(error => fail(error.message), identity).asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.fitExclusions, expected.fitExclusions)
    }
  }

  test("future chunking preserves voxel-specific observation patterns") {
    val plan = FitPlan(
      missingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val expected = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity)
      .asInstanceOf[PatternedFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, chunking = singleVoxelChunking, parallelism = FitParallelism.unsafe(2))
      .map { evaluated =>
        val actual = evaluated.fold(error => fail(error.message), identity)
          .asInstanceOf[PatternedFmriFitResult]
        assertEquals(actual.voxelIndices, expected.voxelIndices)
        assertEquals(actual.patternResults.map(_.pattern), expected.patternResults.map(_.pattern))
        actual.voxelIndices.foreach { voxelIndex =>
          val left = actual.resultForVoxel(voxelIndex).get.asInstanceOf[DenseFmriFitResult]
          val right = expected.resultForVoxel(voxelIndex).get.asInstanceOf[DenseFmriFitResult]
          assertMatrixClose(left.coefficients.value, right.coefficients.value, 1e-12)
          assertMatrixClose(left.standardErrors.value, right.standardErrors.value, 1e-12)
          assertEquals(left.residualDegreesOfFreedom, right.residualDegreesOfFreedom)
        }
      }
  }

  test("all-excluded and partially-excluded chunks preserve source exclusion order") {
    val plan = FitPlan(
      interleavedMissingDataModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(missingData = MissingDataPolicy.Propagate)
    )
    val expected = FitPlanExecutor.fit(plan).fold(error => fail(error.message), identity).asInstanceOf[DenseFmriFitResult]
    val actual = FitPlanExecutor
      .fitChunked(plan, chunking = chunking)
      .fold(error => fail(error.message), identity)
      .asInstanceOf[DenseFmriFitResult]

    assertDenseClose(actual, expected)
    assertEquals(
      actual.fitExclusions,
      Vector(
        VoxelInferenceExclusion(0, VoxelFitStatus.NonFinite),
        VoxelInferenceExclusion(1, VoxelFitStatus.NonFinite),
        VoxelInferenceExclusion(3, VoxelFitStatus.NonFinite)
      )
    )
  }

  test("ChunkedFitExecutor preserves fixed volume weighting applied to prepared responses") {
    val weights = Vector.tabulate(8)(row => 1.0 + row.toDouble / 3.0)
    val plan = FitPlan(
      olsModel,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(volumeWeighting = VolumeWeighting.Fixed(weights))
    )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.fit(plan, selection).toOption.get.asInstanceOf[DenseFmriFitResult]
    val actual =
      FitPlanExecutor
        .fitChunked(plan, selection, chunking)
        .toOption
        .get
        .asInstanceOf[DenseFmriFitResult]

    assertDenseClose(actual, expected)
    assertEquals(actual.preparationProvenance, expected.preparationProvenance)
    assert(actual.preparationProvenance.exists(_.applied.exists(_.step match
      case ResponsePreparationStep.VolumeWeights(VolumeWeighting.Fixed(_, _)) => true
      case _ => false
    )))
  }

  test("ChunkedFitExecutor estimates DVARS once from the whole selected response") {
    val rows = Vector.tabulate(8) { row =>
      val x = row.toDouble
      Vector(
        1.0 + 2.0 * x,
        3.0 - x,
        -2.0 + 0.25 * x + (if row == 4 then 30.0 else 0.0)
      )
    }
    val model = modelFromRows(
      id = "chunked-estimated-volume-weights",
      sampling = SamplingFrame(blockLens = Seq(8), tr = Seq(1.0)),
      task = Vector.tabulate(8)(_.toDouble),
      rows = rows
    )
    val estimator = DvarsWeightEstimator(DvarsWeightFunction.InverseSquared)
    val plan = FitPlan(
      model,
      engine = FitEngine.OrdinaryLeastSquares,
      config = FitConfig(volumeWeighting = VolumeWeighting.Estimated(estimator))
    )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.fit(plan, selection).toOption.get.asInstanceOf[DenseFmriFitResult]
    val actual = FitPlanExecutor
      .fitChunked(plan, selection, singleVoxelChunking)
      .toOption
      .get
      .asInstanceOf[DenseFmriFitResult]

    assertDenseClose(actual, expected)
    assertEquals(actual.preparationProvenance, expected.preparationProvenance)
    assert(actual.preparationProvenance.flatMap(_.volumeWeighting).exists(_.source == VolumeWeightingSource.ResponseDvars(estimator)))
  }

  test("FutureChunkedFitExecutor matches sequential chunked OLS with bounded parallelism") {
    val plan = FitPlan(olsModel)
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected =
      ChunkedFitExecutor
        .fit(plan, selection, chunking)
        .toOption
        .get
        .asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, chunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.fold(error => fail(s"future chunked OLS failed: ${error.message}"), identity).asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
      }
  }

  test("FutureChunkedFitExecutor matches unchunked full-rank LatentSketch") {
    val plan = FitPlan(olsModel, FitStrategy.LatentSketch())
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.fold(error => fail(s"future full-rank LatentSketch failed: ${error.message}"), identity).asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.engine, FitEngine.LatentSketch)
      }
  }

  test("FutureChunkedFitExecutor reuses one compressed LatentSketch across chunks") {
    val plan =
      FitPlan(
        olsModel,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(2),
            method = LatentSketchMethod.ContiguousVoxelAveraging
          )
        )
      )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.fold(error => fail(s"future voxelwise-AR chunk reuse failed: ${error.message}"), identity).asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.engine, FitEngine.LatentSketch)
        assertEquals(actual.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
        assertEquals(actual.coefficientCovariance.matrixCount, 3)
      }
  }

  test("FutureChunkedFitExecutor reuses one principal-component LatentSketch across chunks") {
    val plan =
      FitPlan(
        olsModel,
        FitStrategy.LatentSketch(
          LatentSketchConfig.unsafe(
            components = LowRankComponentSpec.unsafeFixed(2),
            method = LatentSketchMethod.PrincipalComponents
          )
        )
      )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.fold(error => fail(s"future principal-component LatentSketch failed: ${error.message}"), identity).asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.engine, FitEngine.LatentSketch)
        assertEquals(actual.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
        assertEquals(actual.coefficientCovariance.matrixCount, 3)
      }
  }

  test("ChunkedFitExecutor matches unchunked LSS") {
    val plan = lssPlan
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[LssFmriFitResult]
    val actual =
      ChunkedFitExecutor
        .fit(plan, selection, chunking)
        .toOption
        .get
        .asInstanceOf[LssFmriFitResult]

    assertLssClose(actual, expected)
  }

  test("ChunkedFitExecutor matches unchunked fixed-AR GLS") {
    val plan = glsPlan(
      ArOptions(
        structure = ArStructure.Ar(1),
        rho = Some(0.35)
      )
    )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]
    val actual =
      ChunkedFitExecutor
        .fit(plan, selection, chunking)
        .toOption
        .get
        .asInstanceOf[DenseFmriFitResult]

    assertDenseClose(actual, expected)
  }

  test("FutureChunkedFitExecutor prepares estimated-AR GLS once across bounded parallel chunks") {
    val plan = glsPlan(ArOptions(structure = ArStructure.Ar(1)))
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.toOption.get.asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.autocorrelation.map(_.runs.map(_.method)), Some(Vector("estimated")))
        assertEquals(actual.autocorrelation, expected.autocorrelation)
      }
  }

  test("FutureChunkedFitExecutor matches unchunked full-rank ReducedRankGls") {
    val plan =
      FitPlan(
        glsModel,
        engine = FitEngine.ReducedRankGls,
        config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.35)))
      )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.toOption.get.asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.engine, FitEngine.ReducedRankGls)
      }
  }

  test("FutureChunkedFitExecutor reuses one compressed ReducedRankGls basis across chunks") {
    val plan =
      FitPlan(
        reducedRankGlsModel,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(
            components = ReducedRankComponentSpec.unsafeFixed(1),
            autocorrelation = AutocorrelationConfig.unsafe(
              order = 1,
              coefficients = ArCoefficientSpec.Rho(0.35)
            )
          )
        )
      )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.toOption.get.asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.engine, FitEngine.ReducedRankGls)
        assertEquals(actual.coefficientCovariance.scope, CoefficientCovarianceScope.Shared)
        assertEquals(actual.coefficientCovariance.matrixCount, 1)
        assertEquals(actual.inference.method, CoefficientInferenceMethod.ReducedRankConditional)
        assertEquals(actual.inferenceScope.allowedIndices(actual.predictors), Vector(0, 1))
      }
  }

  test("FutureChunkedFitExecutor reuses deterministic ReducedRankGls bootstrap inference") {
    val plan =
      FitPlan(
        reducedRankGlsModel,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(
            components = ReducedRankComponentSpec.unsafeFixed(1),
            autocorrelation = AutocorrelationConfig.unsafe(
              order = 1,
              coefficients = ArCoefficientSpec.Rho(0.35)
            ),
            inference = ReducedRankInferencePolicy.Bootstrap(
              ReducedRankBootstrapConfig.unsafe(replicates = 16, blockSize = 2, seed = 11)
            )
          )
        )
      )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.toOption.get.asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(
          actual.inference.method,
          CoefficientInferenceMethod.ReducedRankBootstrap(replicates = 16, blockSize = 2, seed = 11)
        )
      }
  }

  test("full-rank voxelwise-AR ReducedRankGls fallback remains event-only across chunks") {
    val plan =
      FitPlan(
        glsModel,
        engine = FitEngine.ReducedRankGls,
        config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true))
      )
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    assertEquals(expected.inference.method, CoefficientInferenceMethod.ReducedRankFullRankVoxelwiseFallback)
    assertEquals(expected.inferenceScope.allowedIndices(expected.predictors), Vector(0))
    assert(TContrast("baseline", Map("base_constant" -> 1.0)).evaluate(expected).isLeft)

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.fold(error => fail(s"future voxelwise-AR reduced-rank fallback failed: ${error.message}"), identity).asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
        assertEquals(actual.inferenceScope.allowedIndices(actual.predictors), Vector(0))
      }
  }

  test("compressed voxelwise-AR ReducedRankGls remains an explicit design boundary") {
    val plan =
      FitPlan(
        reducedRankGlsModel,
        FitStrategy.ReducedRankGls(
          ReducedRankGlsConfig.unsafe(
            components = ReducedRankComponentSpec.unsafeFixed(1),
            autocorrelation = AutocorrelationConfig.unsafe(order = 1, voxelwise = true)
          )
        )
      )

    assert(FitPlanExecutor.fit(plan).left.toOption.exists {
      case FitError.UnsupportedEngine(detail) =>
        detail.contains("voxelwise-AR reduced-rank geometry") && detail.contains("tracked separately")
      case _ =>
        false
    })
  }

  test("FutureChunkedFitExecutor reuses full-selection voxelwise AR plans across chunks") {
    val plan = glsPlan(ArOptions(structure = ArStructure.Ar(1), voxelwise = true))
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, selection, singleVoxelChunking, FitParallelism.unsafe(2))
      .map { result =>
        val actual = result.fold(error => fail(s"future voxelwise-AR chunk reuse failed: ${error.message}"), identity).asInstanceOf[DenseFmriFitResult]
        val diagnostics = actual.autocorrelation.get
        val voxelRhos = diagnostics.runs.head.voxelwiseCoefficients.map(_.head)

        assertDenseClose(actual, expected)
        assertEquals(diagnostics.sharedNormalizedCovariance, false)
        assertEquals(diagnostics.runs.map(_.method), Vector("voxelwise-estimated"))
        assertEquals(voxelRhos.length, 3)
        assertEquals(voxelRhos, expected.autocorrelation.get.runs.head.voxelwiseCoefficients.map(_.head))
        assertEquals(actual.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
        assertEquals(actual.inferenceReady.isRight, true)
        val t = TContrast("task", Map("task" -> 1.0)).evaluate(actual).toOption.get
        val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(actual).toOption.get
        assert(t.statistics.toVector.forall(_.isFinite))
        assert(f.statistics.toVector.forall(_.isFinite))
      }
  }

  test("chunk interpreters agree from the same prepared GLS chunk program") {
    val plan = glsPlan(ArOptions(structure = ArStructure.Ar(1)))
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val program = FitChunkProgram.fromSelection(plan, selection, singleVoxelChunking).toOption.get
    val sequential = SequentialFitChunkInterpreter.execute(program).toOption.get

    FutureFitChunkInterpreter(FitParallelism.unsafe(2))
      .execute(program)
      .map { result =>
        val parallel = result.toOption.get
        val sequentialFit =
          ChunkedFitExecutor
            .mergeCompletedChunks(plan, sequential)
            .toOption
            .get
            .asInstanceOf[DenseFmriFitResult]
        val parallelFit =
          ChunkedFitExecutor
            .mergeCompletedChunks(plan, parallel)
            .toOption
            .get
            .asInstanceOf[DenseFmriFitResult]

        assertEquals(parallel.map(_.ordinal.value), sequential.map(_.ordinal.value))
        assertDenseClose(parallelFit, sequentialFit)
      }
  }

  test("ChunkedFitExecutor matches unchunked runwise OLS") {
    val plan = FitPlan(runwiseModel, engine = FitEngine.RunwiseLeastSquares)
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val expected = FitPlanExecutor.unsafeFit(plan, selection).asInstanceOf[RunwiseFmriFitResult]
    val actual =
      ChunkedFitExecutor
        .fit(plan, selection, chunking)
        .toOption
        .get
        .asInstanceOf[RunwiseFmriFitResult]

    assertRunwiseClose(actual, expected)
  }

  test("FitChunkPlan rejects non-contiguous ordinals") {
    val chunks = Vector(
      FitChunkSpec.unsafe(ChunkOrdinal.unsafe(0), Vector(0, 1), Vector(0)),
      FitChunkSpec.unsafe(ChunkOrdinal.unsafe(2), Vector(0, 1), Vector(1))
    )

    assert(FitChunkPlan.make(chunks).left.toOption.exists {
      case FitError.IncompatibleFitBlocks(detail) => detail.contains("ordinal 2")
      case _                                      => false
    })
  }

  test("FitChunkPlan rejects mixed timepoint chunks") {
    val chunks = Vector(
      FitChunkSpec.unsafe(ChunkOrdinal.unsafe(0), Vector(0, 1), Vector(0)),
      FitChunkSpec.unsafe(ChunkOrdinal.unsafe(1), Vector(0, 2), Vector(1))
    )

    assert(FitChunkPlan.make(chunks).left.toOption.exists {
      case FitError.IncompatibleFitBlocks(detail) => detail.contains("same selected timepoints")
      case _                                      => false
    })
  }

  private def olsModel: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(8), tr = Seq(1.0))
    val rows =
      Vector.tabulate(8) { i =>
        val x = i.toDouble
        Vector(
          1.0 + 2.0 * x,
          3.0 - x,
          -2.0 + 0.25 * x
        )
      }
    modelFromRows(
      id = "chunked-ols-demo",
      sampling = sampling,
      task = Vector.tabulate(8)(_.toDouble),
      rows = rows
    )

  private def runwiseModel: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(4, 4), tr = Seq(1.0, 1.0))
    val task = Vector.tabulate(8)(i => (i % 4).toDouble)
    val rows =
      Vector.tabulate(8) { i =>
        val x = (i % 4).toDouble
        if i < 4 then
          Vector(
            1.0 + 2.0 * x,
            3.0 - x,
            -2.0 + 0.25 * x
          )
        else
          Vector(
            10.0 - x,
            -2.0 + 1.5 * x,
            4.0 + 0.75 * x
          )
      }
    modelFromRows(
      id = "chunked-runwise-demo",
      sampling = sampling,
      task = task,
      rows = rows
    )

  private def glsPlan(options: ArOptions): FitPlan =
    FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = options)
    )

  private def glsModel: FmriModel =
    val nTime = 90
    val sampling = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0))
    val task = Vector.tabulate(nTime)(i => ((i % 9) - 4).toDouble)
    val residuals = Vector(
      ar1Residual(phi = 0.72, n = nTime, offset = 0),
      ar1Residual(phi = -0.48, n = nTime, offset = 500),
      ar1Residual(phi = 0.18, n = nTime, offset = 1000)
    )
    val betas = Vector(
      1.4 -> 2.0,
      -0.8 -> 1.0,
      0.35 -> -1.5
    )
    val rows =
      Vector.tabulate(nTime) { row =>
        betas.indices.toVector.map { voxel =>
          val (taskBeta, intercept) = betas(voxel)
          taskBeta * task(row) + intercept + residuals(voxel)(row)
        }
      }
    modelFromRows(
      id = "chunked-gls-demo",
      sampling = sampling,
      task = task,
      rows = rows
    )

  private def reducedRankGlsModel: FmriModel =
    val nTime = 90
    val sampling = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0))
    val taskA = Vector.tabulate(nTime)(i => ((i % 9) - 4).toDouble)
    val taskB = Vector.tabulate(nTime)(i => math.sin(i.toDouble / 6.0) + (if i % 5 == 0 then 0.5 else -0.25))
    val residuals = Vector(
      ar1Residual(phi = 0.35, n = nTime, offset = 1500),
      ar1Residual(phi = -0.2, n = nTime, offset = 2000),
      ar1Residual(phi = 0.1, n = nTime, offset = 2500)
    )
    val loadings = Vector(1.0, -0.55, 0.3)
    val rows =
      Vector.tabulate(nTime) { row =>
        loadings.indices.toVector.map { voxel =>
          val sharedTask = 1.2 * taskA(row) - 0.45 * taskB(row)
          loadings(voxel) * sharedTask + (voxel.toDouble - 1.0) + residuals(voxel)(row)
        }
      }
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = sampling,
        designMatrix = Mat.fromRows(taskA.indices.map(i => Vector(taskA(i), taskB(i))).toVector),
        columnNames = Vector("task_a", "task_b"),
        termSpans = Vector(0 -> 2),
        colIndices = Map("task" -> Vector(0, 1))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = sampling,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("chunked-rrr-gls-demo"), GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(3, 1, 1))),
        samplingFrame = sampling
      )
    FmriModel(eventModel, baseline, dataset)

  private def missingDataModel: FmriModel =
    modelFromRows(
      id = "chunked-missing-data-demo",
      sampling = SamplingFrame(blockLens = Seq(8), tr = Seq(1.0)),
      task = Vector.tabulate(8)(_.toDouble),
      rows = Vector.tabulate(8) { row =>
        val x = row.toDouble
        Vector(
          1.0 + 0.75 * x + (if row % 3 == 0 then 0.2 else -0.1),
          if row == 3 then Double.NaN else 2.0 - x,
          -1.0 + 0.25 * x + (if row % 2 == 0 then 0.15 else -0.05)
        )
      }
    )

  private def interleavedMissingDataModel: FmriModel =
    modelFromRows(
      id = "chunked-interleaved-missing-data-demo",
      sampling = SamplingFrame(blockLens = Seq(8), tr = Seq(1.0)),
      task = Vector.tabulate(8)(_.toDouble),
      rows = Vector.tabulate(8) { row =>
        val x = row.toDouble
        Vector(
          if row == 1 then Double.NaN else 1.0 + x,
          if row == 3 then Double.PositiveInfinity else 2.0 - x,
          -1.0 + 0.4 * x + (if row % 2 == 0 then 0.2 else -0.1),
          if row == 5 then Double.NegativeInfinity else 0.5 * x
        )
      }
    )

  private def ar1Residual(phi: Double, n: Int, offset: Int): Vector[Double] =
    val out = Array.ofDim[Double](n)
    var i = 0
    while i < n do
      val raw = math.sin((i + offset + 1).toDouble * 12.9898 + 78.233) * 43758.5453
      val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
      out(i) = innovation + (if i == 0 then 0.0 else phi * out(i - 1))
      i += 1
    out.toVector

  private def modelFromRows(
      id: String,
      sampling: SamplingFrame,
      task: Vector[Double],
      rows: Vector[Vector[Double]]
  ): FmriModel =
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = sampling,
        designMatrix = Mat.fromRows(task.map(value => Vector(value))),
        columnNames = Vector("task"),
        termSpans = Vector(0 -> 1),
        colIndices = Map("task" -> Vector(0))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = sampling,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId(id),
          GaleTestMatrix.fromRows(rows),
          SampleSpaces(Vector(rows.head.length, 1, 1))
        ),
        samplingFrame = sampling
      )
    FmriModel(eventModel, baseline, dataset)

  private def lssPlan: FitPlan =
    val nTime = 40
    val nTrials = 5
    val rows = Vector.tabulate(nTime) { i =>
      val x = i.toDouble
      Vector(
        math.sin(x / 5.0) + x / 20.0,
        math.cos(x / 7.0) - x / 30.0,
        0.25 + x / 50.0
      )
    }
    val events = DatasetEvents(
      Vector.tabulate(nTrials)(i => Map("onset" -> (2 + i * 6).toString))
    )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(
          DatasetId("chunked-lss-demo"),
          GaleTestMatrix.fromRows(rows),
          SampleSpaces(Vector(3, 1, 1))
        ),
        samplingFrame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0)),
        events = events
      )
    FmriModelBuilder.buildPlan(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ trialwise(basis = \"spmg1\", add_sum = TRUE, label = \"trial\")",
        baselineIntercept = Intercept.Global,
        strategy = FitStrategy.LeastSquaresSeparate()
      )
    )

  private def assertDenseClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.columnNames, expected.columnNames)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(actual.inferenceScope, expected.inferenceScope)
    assertEquals(actual.inference.method, expected.inference.method)
    assertEquals(actual.fitExclusions, expected.fitExclusions)
    assertEquals(actual.olsDiagnostics, expected.olsDiagnostics)
    assertEquals(actual.autocorrelation, expected.autocorrelation)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, tol = 1e-10)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, tol = 1e-10)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance, tol = 1e-10)
    assertCoefficientCovarianceClose(actual.coefficientCovariance, expected.coefficientCovariance, tol = 1e-10)
    assertVectorClose(actual.inference.varianceScale, expected.inference.varianceScale, tol = 1e-10)
    assertVectorClose(actual.residualVariance, expected.residualVariance, tol = 1e-10)

  private def assertLssClose(actual: LssFmriFitResult, expected: LssFmriFitResult): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.columnNames, expected.columnNames)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.lssDiagnostics, expected.lssDiagnostics)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, tol = 1e-10)

  private def assertRunwiseClose(actual: RunwiseFmriFitResult, expected: RunwiseFmriFitResult): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.columnNames, expected.columnNames)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.runs.map(_.runIndex), expected.runs.map(_.runIndex))
    var run = 0
    while run < actual.runs.length do
      val left = actual.runs(run)
      val right = expected.runs(run)
      assertEquals(left.rowIndices, right.rowIndices)
      assertEquals(left.timepoints, right.timepoints)
      assertEquals(left.residualDegreesOfFreedom, right.residualDegreesOfFreedom)
      assertEquals(left.olsDiagnostics, right.olsDiagnostics)
      assertMatrixClose(left.coefficients.value, right.coefficients.value, tol = 1e-10)
      assertMatrixClose(left.standardErrors.value, right.standardErrors.value, tol = 1e-10)
      assertMatrixClose(left.normalizedCovariance, right.normalizedCovariance, tol = 1e-10)
      assertVectorClose(left.residualVariance, right.residualVariance, tol = 1e-10)
      run += 1

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

  private def assertCoefficientCovarianceClose(actual: CoefficientCovariance, expected: CoefficientCovariance, tol: Double): Unit =
    assertEquals(actual.scope, expected.scope)
    assertEquals(actual.matrixCount, expected.matrixCount)
    var i = 0
    while i < actual.matrixCount do
      assertMatrixClose(actual.matrices(i), expected.matrices(i), tol)
      i += 1

  private def assertVectorClose(actual: DVec, expected: DVec, tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  private final case class DummyWork(ordinal: ChunkOrdinal) extends ChunkWork
