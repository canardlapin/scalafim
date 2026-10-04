package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import gale.linalg.DMat
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, DvarsWeightEstimator, DvarsWeightFunction, DvarsWeightScope, FitConfig, FitEngine, FitPlan, FmriModel, MissingDataPolicy, VolumeWeighting}
import scalafim.image.SampleSpaces
import scala.concurrent.ExecutionContext.Implicits.global

class FitPreparationSuite extends munit.FunSuite:
  private val times = (0 until 28).filterNot(Set(3, 16)).toVector
  private val selection = DataSelection(IndexSelection.Indices(times), IndexSelection.indices(4, 1, 3, 0, 2))

  private def plans: Vector[(String, FitPlan)] = Vector(
    "OLS" -> FitPlan(model()),
    "fixed weights" -> FitPlan(model(), engine = FitEngine.OrdinaryLeastSquares, config = FitConfig(volumeWeighting = VolumeWeighting.Fixed(
      Vector.tabulate(28)(row => if row == 7 then 0.0 else 0.4 + row / 20.0)))),
    "fixed AR" -> FitPlan(model(), FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.35)))),
    "voxelwise AR" -> FitPlan(model(), FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true, iterations = 2)))
  )

  for (name, plan) <- plans do
    test(s"$name has no eager preparation read and preserves inference in uneven chunks") {
      val reader = new BoundedReader(plan, 2)
      val chunkPlan = FitChunkPlan.fromSelection(plan, selection, FitChunkingStrategy.unsafeByVoxelCount(2)).toOption.get
      val program = FitChunkProgram.fromChunkPlan(reader, plan, chunkPlan).fold(error => fail(error.message), identity)
      assertEquals(reader.requests, Vector.empty)
      val blocks = ChunkedFitExecutor.fitChunks(program).fold(error => fail(error.message), identity)
      val actual = dense(ChunkedFitExecutor.mergeChunks(plan, blocks))
      val expected = dense(FitPlanExecutor.fit(plan, selection))
      assertClose(actual, expected)
      assertEquals(reader.requests, Vector(Vector(4, 1), Vector(3, 0), Vector(2)))
    }

    test(s"$name remains bounded with parallel chunks") {
      val expected = dense(FitPlanExecutor.fit(plan, selection))
      FutureChunkedFitExecutor.fit(new BoundedReader(plan, 1), plan, selection,
        FitChunkingStrategy.unsafeByVoxelCount(1), FitParallelism.unsafe(2)).map { result =>
        assertClose(dense(result), expected)
      }
    }

  for scope <- Vector(DvarsWeightScope.WithinRun, DvarsWeightScope.AcrossSelection) do
    for exclude <- Vector(false, true) do
      test(s"DVARS $scope exclusions=$exclude reduces globally with bounded reads") {
        val estimator = DvarsWeightEstimator(DvarsWeightFunction.InverseSquared, scope)
        val plan = FitPlan(model(exclude), engine = FitEngine.OrdinaryLeastSquares, config = FitConfig(
          volumeWeighting = VolumeWeighting.Estimated(estimator),
          missingData = if exclude then MissingDataPolicy.ExcludeVoxel else MissingDataPolicy.Error))
        val expected = dense(FitPlanExecutor.fit(plan, selection))
        val reader = new BoundedReader(plan, 1)
        val requirements = FitPreparation.describe(plan)
        assertEquals(requirements.topology, FitPreparationTopology.GlobalReduction)
        assertEquals(requirements.reductions, Vector(FitPreparationReduction.Dvars))
        val actual = dense(ChunkedFitExecutor.fit(reader, plan, selection, FitChunkingStrategy.unsafeByVoxelCount(1)))
        assertClose(actual, expected)
        assertEquals(reader.requests.length, 10) // reduction pass, then execution pass
        val actualWeights = actual.preparationProvenance.flatMap(_.volumeWeighting).get
        val expectedWeights = expected.preparationProvenance.flatMap(_.volumeWeighting).get
        assertEquals(actualWeights, expectedWeights)
        // A spike in just one voxel must affect the shared temporal weights.
        assert(actualWeights.weights.max - actualWeights.weights.min > 0.1)
      }

  test("parallel DVARS uses one completed reduction and preserves its receipt") {
    val plan = FitPlan(model(), engine = FitEngine.OrdinaryLeastSquares, config = FitConfig(volumeWeighting = VolumeWeighting.Estimated(
      DvarsWeightEstimator(DvarsWeightFunction.InverseSquared))))
    val expected = dense(FitPlanExecutor.fit(plan, selection))
    FutureChunkedFitExecutor.fit(new BoundedReader(plan, 2), plan, selection,
      FitChunkingStrategy.unsafeByVoxelCount(2), FitParallelism.unsafe(2)).map { result =>
      assertClose(dense(result), expected)
    }
  }

  test("streamed DVARS matches an analytic temporal-weight fixture") {
    // Temporal RMS differences are sqrt(3), sqrt(3), 3*sqrt(3).
    // Median completion/normalization gives (1,1,1,3), hence mean-one
    // inverse-square weights (5/4,5/4,5/4,1/4), independent of voxel chunking.
    val sampling = SamplingFrame(blockLens = Seq(4), tr = Seq(1.0))
    val event = EventModel(Vector.empty, sampling, Mat.fromRows(Vector.tabulate(4)(i => Vector(i.toDouble))),
      Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
    val baseline = BaselineModel.build(sampling, intercept = Intercept.Global)
    val rows = Vector(Vector(0.0, 0.0, 0.0), Vector(1.0, 2.0, 2.0),
      Vector(2.0, 4.0, 4.0), Vector(5.0, 10.0, 10.0))
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("analytic-dvars"),
      GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(3, 1, 1))), samplingFrame = sampling)
    val plan = FitPlan(FmriModel(event, baseline, dataset), engine = FitEngine.OrdinaryLeastSquares, config = FitConfig(
      volumeWeighting = VolumeWeighting.Estimated(DvarsWeightEstimator())))
    val underlying = SynchronousFmriDataset.readerFor(dataset).toOption.get
    val reader = new DatasetSeriesReader:
      def dataset: FmriDataset = underlying.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
          if resolved.voxels.length > 1 then Left(DatasetError.StorageFailure("unbounded reduction read"))
          else underlying.seriesEither(selection)
        }
    val chunks = FitChunkPlan.fromSelection(plan, chunking = FitChunkingStrategy.unsafeByVoxelCount(1)).toOption.get
    val prepared = FitPreparation.dvars(reader, plan, chunks).fold(error => fail(error.message), identity)
    val receipt = prepared.responsePreparation.volumeWeighting.get.receipt
    receipt.weights.zip(Vector(1.25, 1.25, 1.25, 0.25)).foreach { (actual, expected) =>
      assertEqualsDouble(actual, expected, 1e-14)
    }
  }

  private def dense(result: Either[FitError, FmriFitResult]): DenseFmriFitResult =
    result.fold(error => fail(error.message), _.asInstanceOf[DenseFmriFitResult])

  private def assertClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(actual.inference.scope, expected.inference.scope)
    assertEquals(actual.coefficientCovariance.scope, expected.coefficientCovariance.scope)
    assertEquals(actual.fitExclusions, expected.fitExclusions)
    assertEquals(actual.preparationProvenance, expected.preparationProvenance)
    assertEquals(actual.autocorrelation, expected.autocorrelation)
    assertEquals(actual.robustDiagnostics, expected.robustDiagnostics)
    matrixClose(actual.coefficients.value, expected.coefficients.value)
    matrixClose(actual.standardErrors.value, expected.standardErrors.value)
    actual.voxelIndices.indices.foreach { position =>
      matrixClose(actual.coefficientCovariance.matrixForVoxelPosition(position).toOption.get,
        expected.coefficientCovariance.matrixForVoxelPosition(position).toOption.get)
      assertEqualsDouble(actual.residualVariance(position), expected.residualVariance(position), 1e-10)
    }
    val contrast = TContrast("task", Map("task" -> 1.0))
    val left = contrast.evaluate(actual).toOption.get
    val right = contrast.evaluate(expected).toOption.get
    left.statistics.toVector.zip(right.statistics.toVector).foreach { (a, e) => assertEqualsDouble(a, e, 1e-9) }

  private def matrixClose(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do
      assertEqualsDouble(actual(row, col), expected(row, col), 1e-10)

  private class BoundedReader(plan: FitPlan, limit: Int) extends DatasetSeriesReader:
    private val underlying = SynchronousFmriDataset.readerFor(plan.model.dataset).toOption.get
    var requests = Vector.empty[Vector[Int]]
    def dataset: FmriDataset = underlying.dataset
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
        if resolved.voxels.length > limit then Left(DatasetError.StorageFailure("unbounded spatial read"))
        else if resolved.timepoints != times then Left(DatasetError.StorageFailure("split or reordered time series"))
        else
          synchronized { requests = requests :+ resolved.voxels }
          underlying.seriesEither(selection)
      }

  private def model(exclude: Boolean = false): FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(12, 16), tr = Seq(1.0, 1.0))
    val task = Vector.tabulate(28)(row => math.sin(row * 0.43) + row / 20.0)
    val rows = Vector.tabulate(28) { row => Vector.tabulate(5) { voxel =>
      if exclude && voxel == 1 && row == 8 then Double.NaN
      else 1.0 + voxel + (voxel + 1) * task(row) + math.cos(row * 1.17 + voxel) * 0.3 +
        (if voxel == 4 && row == 9 then 18.0 else 0.0)
    }}
    val event = EventModel(Vector.empty, sampling, Mat.fromRows(task.map(x => Vector(x))),
      Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
    val baseline = BaselineModel.build(sampling, BaselineBasis.Constant, intercept = Intercept.Global)
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("bounded-preparation"),
      GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(5, 1, 1))), samplingFrame = sampling)
    FmriModel(event, baseline, dataset)
