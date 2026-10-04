package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetError, DatasetEvents, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.fmri.ar.NoisePooling
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArCoefficientSpec, ArOptions, ArStructure, AutocorrelationConfig, FitConfig, FitEngine, FitPlan, FitStrategy, FmriModel, FmriModelBuilder, MissingDataPolicy, ModelBuildSpec}
import scalafim.image.SampleSpaces
import gale.linalg.Matrix

import scala.concurrent.ExecutionContext.Implicits.global

class PooledGlsPreparationSuite extends munit.FunSuite:

  private val selectedTimes = (0 until 27).filterNot(Set(4, 13, 21).contains).toVector
  private val selection = DataSelection(
    time = IndexSelection.indices(selectedTimes*),
    voxels = IndexSelection.indices(3, 0, 2, 1)
  )
  private val blockSize = 2

  test("pooled GLS replays bounded blocks for shared run and global AR estimates") {
    Vector(NoisePooling.Run, NoisePooling.Global).foreach { pooling =>
      Vector(1, 2).foreach { order =>
        Vector(1, 3).foreach { iterations =>
          val plan = pooledPlan(pooling, order, iterations)
          val expected = dense(plan)
          val reader = recordingReader(plan.model.dataset)
          val actual = chunked(FitPlanExecutor.fitChunked(reader, plan, selection, chunking))
          assertDenseClose(actual, expected)
          assertEquals(reader.reads, (iterations + 1) * 2)
          assert(reader.selections.forall(_.time == selection.time))
          assert(reader.selections.forall(_.resolveEither(plan.model.dataset.shape,
            plan.model.dataset.voxelDomain).toOption.get.voxels.length <= blockSize))
          assertEquals(actual.autocorrelation.map(_.whitening.segments), expected.autocorrelation.map(_.whitening.segments))
          assertEquals(actual.autocorrelation.map(_.whitening.censorGaps), expected.autocorrelation.map(_.whitening.censorGaps))
          assertArClose(actual, expected)
          val contrast = TContrast("task", Map("task" -> 1.0))
          assertVectorClose(
            contrast.evaluate(actual).toOption.get.statistics.toVector,
            contrast.evaluate(expected).toOption.get.statistics.toVector
          )
        }
      }
    }
  }

  test("parallel pooled GLS uses the same prepared shared whitening") {
    val plan = pooledPlan(NoisePooling.Global, order = 2, iterations = 3)
    val expected = dense(plan)
    val reader = recordingReader(plan.model.dataset)
    FitPlanExecutor.fitChunkedFuture(reader, plan, selection, chunking, FitParallelism.unsafe(2)).map { result =>
      val actual = chunked(result)
      assertDenseClose(actual, expected)
      assertArClose(actual, expected)
    }
  }

  test("pooled GLS descriptors retain canonical selected axes") {
    val plan = pooledPlan(NoisePooling.Global, order = 1, iterations = 1)
    val reference = FitWorkReference("pooled-gls", "plan-v1", "source-v1")
    val compiled = FitWorkDescriptor.compile(reference, plan, ChunkSize.unsafe(blockSize), selection).toOption.get
    val descriptor = FitWorkDescriptor.decode(compiled.encode).toOption.get
    val reader = recordingReader(plan.model.dataset)
    val resolver = new FitWorkResolver:
      def resolve(request: FitWorkReference): Either[FitError, ResolvedFitWork] =
        if request == reference then Right(ResolvedFitWork(reference, plan, reader))
        else Left(FitError.InvalidFitAxis("fit work descriptor", "unexpected immutable reference"))

    assertEquals(descriptor.selection, selection)
    assertDenseClose(
      chunked(FitWorkExecutor.fit(descriptor, resolver)),
      dense(plan)
    )
  }

  test("runwise GLS preserves projected run-local coefficients through bounded preparation") {
    val strategy = FitStrategy.RunwiseGeneralizedLeastSquares(
      AutocorrelationConfig.unsafe(iterations = 2, coefficients = ArCoefficientSpec.Estimate)
    )
    val plan = FitPlan(projectedModel, strategy)
    val expected = runwise(FitPlanExecutor.fit(plan, selection))
    val actual = runwise(FitPlanExecutor.fitChunked(recordingReader(plan.model.dataset), plan, selection, chunking))

    assertEquals(actual.runs.map(_.projection.map(_.sourceColumnIndices)), expected.runs.map(_.projection.map(_.sourceColumnIndices)))
    assert(actual.runs.forall(_.projection.nonEmpty))
    assert(actual.runs.forall(_.projection.exists(_.sourceColumnIndices.nonEmpty)))
    actual.runs.zip(expected.runs).foreach { (left, right) =>
      assertMatrixClose(left.coefficients.value, right.coefficients.value)
      assertMatrixClose(left.standardErrors.value, right.standardErrors.value)
      assertMatrixClose(left.normalizedCovariance, right.normalizedCovariance)
      assertEquals(left.autocorrelation.map(_.whitening), right.autocorrelation.map(_.whitening))
      left.autocorrelation.get.runs.zip(right.autocorrelation.get.runs).foreach { (a, b) =>
        assertEquals(a.runIndex, b.runIndex)
        assertVectorClose(a.phi, b.phi)
      }
      assertEquals(left.residualDegreesOfFreedom, right.residualDegreesOfFreedom)
    }
  }

  test("excluded whole and partial blocks preserve the pooled population and exclusions") {
    val original = SynchronousFmriDataset.readerFor(model.dataset).toOption.get
      .seriesEither(DataSelection.All).toOption.get
    // Selected order is 3,0,2,1: the first block is entirely excluded.
    val responses = Matrix.tabulate(27, 4) { (row, voxel) =>
      if row == 6 && Set(3, 0, 2).contains(voxel) then Double.NaN
      else original.data(row, voxel)
    }
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("pooled-exclusions"), responses, SampleSpaces(Vector(4, 1, 1))),
      samplingFrame = model.dataset.samplingFrame
    )
    val plan = FitPlan(model.copy(dataset = dataset), engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), iterations = 2),
        missingData = MissingDataPolicy.Propagate))
    val expected = dense(plan)
    val actual = chunked(FitPlanExecutor.fitChunked(recordingReader(dataset), plan, selection, chunking))
    assertEquals(actual.voxelIndices, Vector(1))
    assertEquals(actual.fitExclusions, expected.fitExclusions)
    assertDenseClose(actual, expected)
    assertArClose(actual, expected)
  }

  test("all excluded responses take precedence over a rank-deficient pooled GLS design") {
    val plan = FitPlan(
      allExcludedRankDeficientModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(
        autocorrelation = ArOptions(structure = ArStructure.Ar(1)),
        missingData = MissingDataPolicy.Propagate
      )
    )
    val expected = FitPlanExecutor.fit(plan)
    val actual = FitPlanExecutor.fitChunked(
      SynchronousFmriDataset.readerFor(allExcludedRankDeficientModel.dataset).toOption.get,
      plan,
      DataSelection.All,
      chunking
    )
    Vector(expected, actual).foreach { result =>
      assert(result.left.toOption.exists {
        case FitError.AllVoxelsExcluded(exclusions) => exclusions.map(_.voxelIndex) == Vector(0, 1)
        case _ => false
      })
    }
  }

  test("a fully censored run is IID for shared GLS and invalid for runwise GLS") {
    val selected = selectedTimes.toSet
    val censored = (12 until 27).filter(selected.contains).toVector
    val shared = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(
        structure = ArStructure.Ar(1),
        censoredTimepoints = censored,
        global = false
      ))
    )
    val sharedDense = chunked(FitPlanExecutor.fit(shared, selection))
    val sharedResult = chunked(FitPlanExecutor.fitChunked(recordingReader(model.dataset), shared, selection, chunking))
    assertDenseClose(sharedResult, sharedDense)
    assertEquals(sharedResult.autocorrelation.map(_.runs.map(_.runIndex)), Some(Vector(0, 1)))
    assertEquals(sharedResult.autocorrelation.map(_.runs.last.coefficients), Some(Vector.empty))

    val runwise = FitPlan(model, FitStrategy.RunwiseGeneralizedLeastSquares(
      AutocorrelationConfig.unsafe(censoredTimepoints = censored, coefficients = ArCoefficientSpec.Estimate)
    ))
    assert(FitPlanExecutor.fitChunked(recordingReader(model.dataset), runwise, selection, chunking).left.toOption.exists {
      case FitError.RunwiseFitFailed(1, FitError.UnsupportedAutocorrelation(detail)) =>
        detail.contains("no retained rows")
      case _ => false
    })
  }

  test("pooled GLS refuses a reader that changes finite membership during replay") {
    val plan = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(
        autocorrelation = ArOptions(structure = ArStructure.Ar(1), iterations = 2),
        missingData = MissingDataPolicy.Propagate
      )
    )
    assert(FitPlanExecutor.fitChunked(membershipChangingReader(plan.model.dataset), plan, selection, chunking).left.toOption.exists {
      case FitError.InvalidFitAxis("pooled AR replay", detail) => detail.contains("membership changed")
      case _ => false
    })
  }

  private def pooledPlan(pooling: NoisePooling, order: Int, iterations: Int): FitPlan =
    FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(
        structure = ArStructure.Ar(order),
        global = pooling == NoisePooling.Global,
        iterations = iterations,
        censoredTimepoints = Vector(3, 17)
      ))
    )

  private def chunking = FitChunkingStrategy.unsafeByVoxelCount(blockSize)

  private def dense(plan: FitPlan): DenseFmriFitResult =
    FitPlanExecutor.fit(plan, selection).toOption.get.asInstanceOf[DenseFmriFitResult]

  private def chunked(result: Either[FitError, FmriFitResult]): DenseFmriFitResult =
    result.fold(error => fail(error.message), {
      case dense: DenseFmriFitResult => dense
      case other => fail(s"expected dense GLS result, got $other")
    })

  private def runwise(result: Either[FitError, FmriFitResult]): RunwiseFmriFitResult =
    result.fold(error => fail(error.message), {
      case value: RunwiseFmriFitResult => value
      case other => fail(s"expected runwise GLS result, got $other")
    })

  private final class RecordingReader(
      val dataset: FmriDataset,
      underlying: DatasetSeriesReader,
      selectedTimes: Vector[Int]
  ) extends DatasetSeriesReader:
    var reads = 0
    var selections = Vector.empty[DataSelection]
    def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
      selection.resolveEither(dataset.shape, dataset.voxelDomain).flatMap { resolved =>
        if resolved.timepoints != selectedTimes then
          Left(DatasetError.StorageFailure("pooled GLS changed selected timepoints"))
        else if resolved.voxels.length > blockSize then
          Left(DatasetError.StorageFailure("pooled GLS exceeded configured block size"))
        else
          reads += 1
          selections :+= selection
          underlying.seriesEither(selection)
      }

  private def recordingReader(dataset: FmriDataset): RecordingReader =
    val underlying = SynchronousFmriDataset.readerFor(dataset).toOption.get
    new RecordingReader(dataset, underlying, selectedTimes)

  private def membershipChangingReader(dataset: FmriDataset): DatasetSeriesReader =
    val underlying = SynchronousFmriDataset.readerFor(dataset).toOption.get
    var reads = 0
    new DatasetSeriesReader:
      def dataset: FmriDataset = underlying.dataset
      def seriesEither(request: DataSelection): Either[DatasetError, FmriSeries] =
        underlying.seriesEither(request).flatMap { series =>
          reads += 1
          if reads == 3 then
            val data = Array.ofDim[Double](series.nTimepoints * series.nVoxels)
            var row = 0
            while row < series.nTimepoints do
              var column = 0
              while column < series.nVoxels do
                data(row * series.nVoxels + column) =
                  if column == 0 then Double.NaN else series.data(row, column)
                column += 1
              row += 1
            FmriSeries.make(
              Matrix.dense(series.nTimepoints, series.nVoxels, data.toIndexedSeq),
              series.voxelIndexValues,
              series.timepointIndices,
              series.shape,
              series.metadata
            )
          else Right(series)
        }

  private def assertDenseClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(actual.inferenceScope, expected.inferenceScope)
    assertEquals(actual.preparationProvenance, expected.preparationProvenance)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance)
    assertMatrixClose(actual.coefficientCovariance.unsafeMatrixForVoxelPosition(0), expected.coefficientCovariance.unsafeMatrixForVoxelPosition(0))

  private def assertArClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    val left = actual.autocorrelation.getOrElse(fail("missing actual AR diagnostics")).runs
    val right = expected.autocorrelation.getOrElse(fail("missing expected AR diagnostics")).runs
    assertEquals(left.map(_.runIndex), right.map(_.runIndex))
    left.zip(right).foreach { (a, b) => assertVectorClose(a.phi, b.phi) }

  private def assertMatrixClose(left: gale.linalg.DMat, right: gale.linalg.DMat): Unit =
    assertEquals(left.rows, right.rows)
    assertEquals(left.cols, right.cols)
    var row = 0
    while row < left.rows do
      var column = 0
      while column < left.cols do
        assertEqualsDouble(left(row, column), right(row, column), 1e-8)
        column += 1
      row += 1

  private def assertVectorClose(left: Vector[Double], right: Vector[Double]): Unit =
    assertEquals(left.length, right.length)
    left.zip(right).foreach { (a, b) => assertEqualsDouble(a, b, 1e-8) }

  private lazy val model: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(12, 15), tr = Seq(1.0, 1.0))
    val task = Vector.tabulate(27) { row =>
      val local = if row < 12 then row else row - 12
      math.sin((local + 1).toDouble * 0.51)
    }
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = sampling,
      designMatrix = Mat.fromRows(task.map(value => Vector(value))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(samplingFrame = sampling, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    val rows = Vector.tabulate(27) { row =>
      val x = task(row)
      Vector.tabulate(4) { voxel =>
        val amplitude = (voxel + 1).toDouble * (if row < 12 then 0.8 else -0.55)
        val noise = 0.12 * math.sin((row + 1).toDouble * (voxel + 2).toDouble * 0.37)
        val spike = if row == 3 || row == 17 then 40.0 * (voxel + 1) else 0.0
        amplitude * x + 0.3 * voxel + noise + spike
      }
    }
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("pooled-gls-preparation"), GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(4, 1, 1))),
      samplingFrame = sampling
    )
    FmriModel(event, baseline, dataset)

  private lazy val projectedModel: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(12, 15), tr = Seq(1.0, 1.0))
    val events = DatasetEvents(Vector(
      Map("onset" -> "2.0", "condition" -> "a", "run" -> "run-1"),
      Map("onset" -> "7.0", "condition" -> "a", "run" -> "run-1"),
      Map("onset" -> "2.0", "condition" -> "a", "run" -> "run-2"),
      Map("onset" -> "8.0", "condition" -> "a", "run" -> "run-2")
    ))
    val rows = Vector.tabulate(27) { row =>
      Vector.tabulate(4) { voxel =>
        0.2 * (voxel + 1) + 0.1 * math.sin((row + 1).toDouble * (voxel + 2))
      }
    }
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(DatasetId("pooled-gls-projected"), GaleTestMatrix.fromRows(rows), SampleSpaces(Vector(4, 1, 1))),
      samplingFrame = sampling,
      events = events
    )
    FmriModelBuilder.buildModel(
      dataset,
      ModelBuildSpec(
        formula = "onset ~ hrf(condition)",
        blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise,
        defaultHrf = Hrfs.fir(nBasis = 2, span = 4.s),
        precision = 0.25.s,
        strategy = FitStrategy.RunwiseGeneralizedLeastSquares(
          AutocorrelationConfig.unsafe(coefficients = ArCoefficientSpec.Estimate)
        )
      )
    )

  private lazy val allExcludedRankDeficientModel: FmriModel =
    val sampling = SamplingFrame(blockLens = Seq(8), tr = Seq(1.0))
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = sampling,
      designMatrix = Mat.fromRows(Vector.fill(8)(Vector(1.0))),
      columnNames = Vector("constant-task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("constant-task" -> Vector(0))
    )
    val baseline = BaselineModel.build(samplingFrame = sampling, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    val dataset = FmriDataset.unsafe(
      backend = InMemoryDatasetBackend(
        DatasetId("pooled-gls-all-excluded"),
        GaleTestMatrix.fromRows(Vector.fill(8)(Vector(Double.NaN, Double.NaN))),
        SampleSpaces(Vector(2, 1, 1))
      ),
      samplingFrame = sampling
    )
    FmriModel(event, baseline, dataset)
