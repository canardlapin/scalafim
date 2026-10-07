package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, DvarsWeightEstimator, FitConfig, FitEngine, FitPlan, FmriModel, MissingDataPolicy, RobustOptions, RobustPsi, ScaleScope, VolumeWeighting}
import scalafim.image.SampleSpaces
import scala.concurrent.ExecutionContext.Implicits.global
import scalafim.fmri.fit.GaleTestSyntax.*

class BoundedRobustPreparationSuite extends munit.FunSuite:
  private val times = (0 until 24).filterNot(Set(3, 17)).toVector
  private val voxels = Vector(6, 1, 4, 0, 5, 2, 3)
  private def selection(indices: Vector[Int]) = DataSelection(IndexSelection.Indices(times), IndexSelection.Indices(indices))

  for psi <- Vector(RobustPsi.Huber(1.25), RobustPsi.Bisquare(4.685)); scope <- Vector(ScaleScope.Global, ScaleScope.Run, ScaleScope.Voxel) do
    test(s"bounded $psi $scope preserves population convergence and diagnostics") {
      val plan = makePlan(RobustOptions(psi, maxIterations = 3, scaleScope = scope))
      for population <- Vector(voxels, voxels.dropRight(1)) do
        val expected = dense(FitPlanExecutor.fit(plan, selection(population)))
        for width <- Vector(1, 3) do
          val reader = new BoundedReader(plan, width)
          val actual = dense(ChunkedFitExecutor.fit(reader, plan, selection(population), FitChunkingStrategy.unsafeByVoxelCount(width)))
          assertClose(actual, expected)
          assert(reader.requests.forall(_.length <= width))
          if psi != RobustPsi.Disabled || scope != ScaleScope.Voxel then
            assertEquals(FitPreparation.describe(plan).topology, FitPreparationTopology.GlobalReduction)
          assert(FitPreparation.describe(plan).supportsBoundedExecution(plan.engine))
    }

  for weighting <- Vector(
    VolumeWeighting.Fixed(Vector.tabulate(24)(row => if Set(7, 13).contains(row) then 0.0 else 0.5 + row / 20.0)),
    VolumeWeighting.Estimated(DvarsWeightEstimator())
  ) do
    test(s"public robust volume weighting remains a typed plan refusal: $weighting") {
      val model = makePlan(RobustOptions(RobustPsi.Huber())).model
      val result = FitPlan.makeLegacy(model, FitEngine.RobustLeastSquares,
        FitConfig(robust = RobustOptions(RobustPsi.Huber()), volumeWeighting = weighting))
      assert(result.left.toOption.exists(_.message.contains("only for ordinary least squares")))
    }

  test("disabled psi remains a typed robust strategy refusal") {
    val result = FitPlan.makeLegacy(makePlan(RobustOptions(RobustPsi.Huber())).model,
      FitEngine.RobustLeastSquares, FitConfig(robust = RobustOptions()))
    assert(result.left.toOption.exists(_.message.contains("requires a robust psi")))
  }

  test("robust AR reductions preserve iterations, gaps, censoring and run pooling") {
    val plan = makePlan(RobustOptions(RobustPsi.Huber(), 3, ScaleScope.Voxel, reestimateAutocorrelation = true),
      ar = ArOptions(ArStructure.Ar(1), iterations = 2, censoredTimepoints = Vector(8, 18)))
    val expected = dense(FitPlanExecutor.fit(plan, selection(voxels)))
    val actual = dense(ChunkedFitExecutor.fit(new BoundedReader(plan, 2), plan, selection(voxels), FitChunkingStrategy.unsafeByVoxelCount(2)))
    assertClose(actual, expected)
    val left = actual.autocorrelation.get
    val right = expected.autocorrelation.get
    assert(left.runs.forall(_.method == "robust-estimated"))
    assertEquals(left.iterations, right.iterations)
    left.runs.zip(right.runs).foreach { (a, e) =>
      a.coefficients.zip(e.coefficients).foreach((x, y) => assertEqualsDouble(x, y, 1e-10))
    }
  }

  test("fully excluded chunks do not contribute to robust global scales") {
    val plan = makePlan(RobustOptions(RobustPsi.Bisquare(), 3, ScaleScope.Global), exclude = true)
    assertClose(dense(ChunkedFitExecutor.fit(new BoundedReader(plan, 1), plan, selection(voxels), FitChunkingStrategy.unsafeByVoxelCount(1))),
      dense(FitPlanExecutor.fit(plan, selection(voxels))))
  }

  test("parallel final blocks use one completed robust reduction") {
    val plan = makePlan(RobustOptions(RobustPsi.Huber(), 3, ScaleScope.Run))
    val expected = dense(FitPlanExecutor.fit(plan, selection(voxels)))
    FutureChunkedFitExecutor.fit(new BoundedReader(plan, 2), plan, selection(voxels),
      FitChunkingStrategy.unsafeByVoxelCount(2), FitParallelism.unsafe(2)).map(result => assertClose(dense(result), expected))
  }

  test("a finite response mutation after preparation is refused") {
    val plan = makePlan(RobustOptions(RobustPsi.Huber(), 2, ScaleScope.Voxel))
    val reader = new BoundedReader(plan, 2)
    val chunks = FitChunkPlan.fromSelection(plan, selection(voxels), FitChunkingStrategy.unsafeByVoxelCount(2)).toOption.get
    val context = BoundedRobustPreparation.prepare(reader, plan, chunks).fold(error => fail(error.message), identity)
    reader.mutate = true
    val result = ChunkedFitExecutor.fitChunk(reader, chunks.indexed.head, context)
    assert(result.left.toOption.exists(_.isInstanceOf[FitError.PreparationReplayMismatch]))
  }

  test("a finite response mutation between reduction passes is refused") {
    val plan = makePlan(RobustOptions(RobustPsi.Huber(), 2, ScaleScope.Voxel))
    val reader = new BoundedReader(plan, 2)
    reader.mutateAtRead = 5 // the initial four-block scan must be immutable on replay
    val result = ChunkedFitExecutor.fit(reader, plan, selection(voxels), FitChunkingStrategy.unsafeByVoxelCount(2))
    def mismatch(error: FitError): Boolean = error match
      case FitError.PreparationReplayMismatch(_) => true
      case FitError.ChunkFailed(_, cause) => mismatch(cause)
      case _ => false
    assert(result.left.toOption.exists(mismatch))
  }

  for deficient <- Vector(false, true) do
    test(s"robust rank refusal retains structural column identities: initialDeficiency=$deficient") {
      val options = RobustOptions(if deficient then RobustPsi.Huber() else RobustPsi.Bisquare(1e-12), 2, ScaleScope.Voxel)
      val plan = makePlan(options, deficient = deficient)
      def rank(error: FitError): Option[StructuralRankReport] = error match
        case FitError.StructuralRankDeficientDesign(report) => Some(report)
        case FitError.ChunkFailed(_, cause) => rank(cause)
        case _ => None
      val expected = FitPlanExecutor.fit(plan, selection(voxels)).left.toOption.flatMap(rank).get
      val actual = ChunkedFitExecutor.fit(new BoundedReader(plan, 2), plan, selection(voxels),
        FitChunkingStrategy.unsafeByVoxelCount(2)).left.toOption.flatMap(rank).get
      assertEquals(actual.numericalRank, expected.numericalRank)
      assertEquals(actual.aliasedColumnIds, expected.aliasedColumnIds)
      assertEquals(actual.pivotOrder.map(_.id).toSet, plan.coefficientAxis.get.columnIds.toSet)
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
    matrixClose(actual.coefficients.value, expected.coefficients.value)
    matrixClose(actual.standardErrors.value, expected.standardErrors.value)
    actual.voxelIndices.indices.foreach { position =>
      matrixClose(actual.coefficientCovariance.matrixForVoxelPosition(position).toOption.get,
        expected.coefficientCovariance.matrixForVoxelPosition(position).toOption.get)
      assertEqualsDouble(actual.residualVariance(position), expected.residualVariance(position), 1e-9)
    }
    val a = actual.robustDiagnostics.get
    val e = expected.robustDiagnostics.get
    assertEquals(a.psi, e.psi)
    assertEquals(a.scaleScope, e.scaleScope)
    assertEquals(a.iterations, e.iterations)
    assertEquals(a.converged, e.converged)
    assertEquals(a.scaleEstimates.labels, e.scaleEstimates.labels)
    assertEqualsDouble(a.maxCoefficientDelta, e.maxCoefficientDelta, 1e-9)
    matrixClose(a.weights, e.weights)
    matrixClose(a.scaleEstimates.values, e.scaleEstimates.values)
    val contrast = TContrast("task", Map("task" -> 1.0))
    val left = contrast.evaluate(actual).toOption.get
    val right = contrast.evaluate(expected).toOption.get
    left.statistics.toVector.zip(right.statistics.toVector).foreach((x, y) => assertEqualsDouble(x, y, 1e-8))

  private def matrixClose(actual: DMat, expected: DMat): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    for row <- 0 until actual.rows; col <- 0 until actual.cols do
      assertEqualsDouble(actual(row, col), expected(row, col), 1e-9)

  private class BoundedReader(plan: FitPlan, limit: Int) extends DatasetSeriesReader:
    private val underlying = SynchronousFmriDataset.readerFor(plan.model.dataset).toOption.get
    var requests = Vector.empty[Vector[Int]]
    var mutate = false
    var mutateAtRead = Int.MaxValue
    def dataset: FmriDataset = underlying.dataset
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
        if resolved.voxels.length > limit then Left(DatasetError.StorageFailure("unbounded spatial read"))
        else if resolved.timepoints != times then Left(DatasetError.StorageFailure("split or reordered time series"))
        else
          synchronized { requests = requests :+ resolved.voxels }
          underlying.seriesEither(selection).flatMap { series =>
            if !mutate && requests.length < mutateAtRead then Right(series)
            else FmriSeries.fromIntIndices(Matrix.tabulate(series.data.rows, series.data.cols)((r, c) => series.data(r, c) + 0.01),
              series.voxelIndices, series.timepoints, series.shape)
          }
      }

  private def makePlan(robust: RobustOptions, weighting: VolumeWeighting = VolumeWeighting.Disabled,
      ar: ArOptions = ArOptions(), exclude: Boolean = false, deficient: Boolean = false): FitPlan =
    val sampling = SamplingFrame(blockLens = Seq(11, 13), tr = Seq(1.0, 1.0))
    val task = Vector.tabulate(24)(row => if deficient then 1.0 else math.sin(row * 0.43) + row / 20.0)
    val rows = Vector.tabulate(24)(row => Vector.tabulate(7)(voxel =>
      if exclude && voxel == 1 && row == 8 then Double.NaN
      else 1.0 + voxel + (voxel + 1) * task(row) + math.cos(row * 1.17 + voxel) * 0.3 +
        (if voxel >= 3 && row == 9 then 6.0 + voxel else 0.0)))
    val event = EventModel(Vector.empty, sampling, Mat.fromRows(task.map(x => Vector(x))),
      Vector("task"), Vector(0 -> 1), Map("task" -> Vector(0)))
    val baseline = BaselineModel.build(sampling, intercept = Intercept.Global)
    val dataset = FmriDataset.unsafe(InMemoryDatasetBackend(DatasetId("bounded-robust"),
      GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(7, 1, 1))), samplingFrame = sampling)
    FitPlan(FmriModel(event, baseline, dataset), FitEngine.RobustLeastSquares, FitConfig(robust = robust,
      autocorrelation = ar, volumeWeighting = weighting,
      missingData = if exclude then MissingDataPolicy.ExcludeVoxel else MissingDataPolicy.Error))
