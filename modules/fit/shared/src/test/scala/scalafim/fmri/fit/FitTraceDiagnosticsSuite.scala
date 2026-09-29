package scalafim.fmri.fit

import scalafim.image.SampleSpaces
import scalafim.dataset.{DataSelection, DatasetError, DatasetId, DatasetSeriesReader, FmriDataset, FmriSeries, IndexSelection, InMemoryDatasetBackend, SynchronousFmriDataset}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FmriModel, VolumeWeighting}

class FitTraceDiagnosticsSuite extends munit.FunSuite:
  private val x = Vector(0.0, 1.0, 2.0, 3.0, 0.5, 1.5, 2.5, 3.5)
  private val noiseA = Vector(0.4, -0.6, 0.2, -0.3, 0.8, -0.2, 0.3, -0.5)
  private val noiseB = Vector(-0.1, 0.5, -0.4, 0.7, -0.3, 0.4, -0.2, 0.2)

  private def model: FmriModel =
    val y = x.indices.toVector.map { row =>
      Vector(1.7 * x(row) + 2.0 + noiseA(row), -0.8 * x(row) + 1.0 + noiseB(row))
    }
    modelFrom(x, y, Vector(4, 4))

  private def modelFrom(regressor: Vector[Double], y: Vector[Vector[Double]], runs: Vector[Int]): FmriModel =
    val frame = SamplingFrame(blockLens = runs, tr = Vector.fill(runs.length)(1.0))
    val dataset = FmriDataset.unsafe(
      InMemoryDatasetBackend(DatasetId("trace-fixture"), GaleTestMatrix.fromRows(y), SampleSpaces(Vector(2, 1, 1))),
      frame
    )
    val event = EventModel(
      terms = Vector.empty,
      samplingFrame = frame,
      designMatrix = Mat.fromRows(regressor.map(v => Vector(v))),
      columnNames = Vector("task"),
      termSpans = Vector(0 -> 1),
      colIndices = Map("task" -> Vector(0))
    )
    val baseline = BaselineModel.build(samplingFrame = frame, basis = BaselineBasis.Constant, intercept = Intercept.Global)
    FmriModel(event, baseline, dataset)

  private def capture(plan: FitPlan): DenseFmriFitResult =
    val reader = SynchronousFmriDataset.readerFor(plan.model.dataset).fold(e => fail(e.message), identity)
    FitPlanExecutor.fitWithTraceCapture(reader, plan, DataSelection.All, 8, 2)
      .fold(e => fail(e.toString), identity)

  private def largeModel: FmriModel =
    val rows = 80
    val x = Vector.tabulate(rows)(row => math.sin(row * 0.19) + (row % 7).toDouble * 0.1)
    def arNoise(rho: Double, phase: Double): Vector[Double] =
      val out = Array.ofDim[Double](rows)
      var row = 0
      while row < rows do
        val innovation = math.sin((row + 1) * phase) * 0.35 + math.cos((row + 3) * phase * 1.7) * 0.2
        out(row) = innovation + (if row % 40 == 0 then 0.0 else rho * out(row - 1))
        row += 1
      out.toVector
    val a = arNoise(0.7, 0.73)
    val b = arNoise(-0.35, 1.17)
    val y = x.indices.toVector.map(row => Vector(1.1 * x(row) + 0.5 + a(row),
      -1.3 * x(row) + 2.0 + b(row)))
    modelFrom(x, y, Vector(40, 40))

  test("bounded OLS trace agrees with analytic projection and remains orthogonal"):
    val captured = capture(FitPlan(model, FitEngine.OrdinaryLeastSquares))
    val result = captured.trace(FitTraceRequest(captured.traceIdentity.get, 0, (0 until 8).toVector,
      FitTraceSpace.Original, 2)).fold(e => fail(e.toString), identity)
    val y = x.indices.map(row => 1.7 * x(row) + 2.0 + noiseA(row)).toVector
    val xMean = x.sum / x.length
    val yMean = y.sum / y.length
    val slope = x.zip(y).map { (a, b) => (a - xMean) * (b - yMean) }.sum /
      x.map(v => (v - xMean) * (v - xMean)).sum
    val intercept = yMean - slope * xMean
    result.rows.foreach { row =>
      assertEqualsDouble(result.observed(row), y(row), 1e-12)
      assertEqualsDouble(result.fitted(row), slope * x(row) + intercept, 1e-10)
      assertEqualsDouble(result.observed(row), result.fitted(row) + result.residual(row), 1e-12)
    }
    assertEqualsDouble(result.residual.sum, 0.0, 1e-10)
    assertEqualsDouble(result.residual.zip(x).map(_ * _).sum, 0.0, 1e-10)
    assertEquals(result.runIndices, Vector(0, 0, 0, 0, 1, 1, 1, 1))
    assertEquals(result.acf(1).pairs, 6)

  test("fixed AR GLS transformed trace uses independent run-reset whitening"):
    val rho = 0.35
    val plan = FitPlan(model, FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(rho))))
    val captured = capture(plan)
    val original = captured.trace(FitTraceRequest(captured.traceIdentity.get, 0, (0 until 8).toVector,
      FitTraceSpace.Original, 2)).fold(e => fail(e.toString), identity)
    val transformed = captured.trace(FitTraceRequest(captured.traceIdentity.get, 0, (0 until 8).toVector,
      FitTraceSpace.Transformed, 2)).fold(e => fail(e.toString), identity)
    val firstScale = math.sqrt(1.0 - rho * rho)
    x.indices.foreach { row =>
      val expected = if row == 0 || row == 4 then original.observed(row) * firstScale
        else original.observed(row) - rho * original.observed(row - 1)
      assertEqualsDouble(transformed.observed(row), expected, 1e-12)
      assertEqualsDouble(transformed.observed(row), transformed.fitted(row) + transformed.residual(row), 1e-10)
    }
    val transformedX = x.indices.map { row =>
      if row == 0 || row == 4 then x(row) * firstScale else x(row) - rho * x(row - 1)
    }
    val transformedIntercept = x.indices.map { row => if row == 0 || row == 4 then firstScale else 1.0 - rho }
    val a = transformedX.map(v => v * v).sum
    val b = transformedX.zip(transformedIntercept).map(_ * _).sum
    val c = transformedIntercept.map(v => v * v).sum
    val d = transformedX.zip(transformed.observed).map(_ * _).sum
    val e = transformedIntercept.zip(transformed.observed).map(_ * _).sum
    val determinant = a * c - b * b
    val oracleSlope = (d * c - b * e) / determinant
    val oracleIntercept = (a * e - b * d) / determinant
    assertEqualsDouble(captured.coefficients(0, 0), oracleSlope, 1e-10)
    assertEqualsDouble(captured.coefficients(1, 0), oracleIntercept, 1e-10)
    assertEqualsDouble(transformed.residual.zip(transformedX).map(_ * _).sum, 0.0, 1e-10)
    assertEqualsDouble(transformed.residual.zip(transformedIntercept).map(_ * _).sum, 0.0, 1e-10)
    assertEquals(transformed.acf(1).pairs, 6)

  test("raw original space refuses weighted preparation while model-prepared space is explicit"):
    val weights = Vector(1.0, 0.7, 0.9, 1.2, 1.0, 0.8, 1.1, 0.6)
    val plan = FitPlan(model, FitEngine.OrdinaryLeastSquares,
      FitConfig(volumeWeighting = VolumeWeighting.Fixed(weights)))
    val fit = capture(plan)
    val base = FitTraceRequest(fit.traceIdentity.get, 0, (0 until 8).toVector,
      FitTraceSpace.Original, 1)
    assert(fit.trace(base).left.toOption.exists(_.isInstanceOf[FitTraceFailure.Unavailable]))
    val prepared = fit.trace(base.copy(space = FitTraceSpace.ModelPrepared))
      .fold(e => fail(e.toString), identity)
    prepared.rows.foreach(row => assertEqualsDouble(prepared.observed(row), prepared.fitted(row) + prepared.residual(row), 1e-10))

  test("source identity, row ordering, budgets and cancellation refuse without reading"):
    val plan = FitPlan(model, FitEngine.OrdinaryLeastSquares)
    val source = SynchronousFmriDataset.readerFor(plan.model.dataset).fold(e => fail(e.message), identity)
    var reads = 0
    val reader = new DatasetSeriesReader:
      val dataset: FmriDataset = plan.model.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        reads += 1
        source.seriesEither(selection)
    assert(FitTraceDiagnostics.fitAndCapture(reader, plan, DataSelection.All, 7, 2).isLeft)
    assert(FitTraceDiagnostics.fitAndCapture(reader, plan, DataSelection.All, 8, 2, () => true).isLeft)
    assertEquals(reads, 0)
    var checks = 0
    assert(FitTraceDiagnostics.fitAndCapture(reader, plan, DataSelection.All, 8, 2, () =>
      checks += 1
      checks == 2).isLeft)
    assertEquals(reads, 1)
    val broken = new DatasetSeriesReader:
      val dataset: FmriDataset = plan.model.dataset
      def seriesEither(selection: DataSelection): Either[DatasetError, FmriSeries] =
        Left(DatasetError.StorageFailure("synthetic read failure"))
    assert(FitTraceDiagnostics.fitAndCapture(broken, plan, DataSelection.All, 8, 2)
      .left.toOption.exists(_.isInstanceOf[FitTraceFailure.Fit]))
    val otherModel = model
    val foreign = SynchronousFmriDataset.readerFor(otherModel.dataset).fold(e => fail(e.message), identity)
    assert(FitTraceDiagnostics.fitAndCapture(foreign, plan, DataSelection.All, 8, 2)
      .left.toOption.exists(_.isInstanceOf[FitTraceFailure.Refused]))
    val captured = capture(plan)
    val ordinary = FitPlanExecutor.fitDense(reader, plan).fold(e => fail(e.message), identity)
    assert(ordinary.trace(FitTraceRequest(captured.traceIdentity.get, 0, Vector(0), FitTraceSpace.Original, 0))
      .left.toOption.exists(_.isInstanceOf[FitTraceFailure.Unavailable]))
    val foreignIdentity = captured.traceIdentity.get.copy(datasetId = DatasetId("foreign"))
    assert(captured.trace(FitTraceRequest(foreignIdentity, 0, Vector(0), FitTraceSpace.Original, 0)).isLeft)
    val wrongRuns = captured.traceIdentity.get.copy(runIndices = Vector.fill(8)(0))
    assert(captured.trace(FitTraceRequest(wrongRuns, 0, Vector(0), FitTraceSpace.Original, 0)).isLeft)
    assert(captured.trace(FitTraceRequest(captured.traceIdentity.get, 0, Vector(2, 1), FitTraceSpace.Original, 0)).isLeft)
    assert(captured.trace(FitTraceRequest(captured.traceIdentity.get, 50, Vector(0), FitTraceSpace.Original, 0)).isLeft)
    val changed = captured.copy(columnNames = captured.columnNames.map(_ + "-changed"))
    assert(changed.trace(FitTraceRequest(captured.traceIdentity.get, 0, Vector(0), FitTraceSpace.Original, 0))
      .left.toOption.exists(_.isInstanceOf[FitTraceFailure.Refused]))
    assert(FitTraceDiagnostics.historicalEstimateUnavailable.isInstanceOf[FitTraceFailure.Unavailable])

  test("estimated shared and voxelwise AR use their actual fitted operators across censor gaps"):
    val model = largeModel
    val selected = (0 until 80).filterNot(i => i == 20 || i == 21).toVector
    val data = DataSelection(time = IndexSelection.Indices(selected))
    val reader = SynchronousFmriDataset.readerFor(model.dataset).fold(e => fail(e.message), identity)
    def fitted(voxelwise: Boolean): DenseFmriFitResult =
      val plan = FitPlan(model, FitEngine.GeneralizedLeastSquares,
        FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = voxelwise)))
      FitPlanExecutor.fitWithTraceCapture(reader, plan, data, 80, 2)
        .fold(e => fail(e.toString), identity)
    for fit <- Vector(fitted(false), fitted(true)) do
      val ar = fit.autocorrelation.get
      assertEquals(ar.whitening.segments.length, 3)
      for voxel <- Vector(0, 1) do
        val trace = fit.trace(FitTraceRequest(fit.traceIdentity.get, voxel,
          (0 until selected.length).toVector, FitTraceSpace.Transformed, 2))
          .fold(e => fail(e.toString), identity)
        trace.rows.foreach(row => assertEqualsDouble(trace.observed(row), trace.fitted(row) + trace.residual(row), 1e-9))
        assertEquals(trace.acf(1).pairs, 75)
        assertEquals(trace.timepoints, selected)
      if ar.sharedNormalizedCovariance then assert(ar.runs.forall(_.voxelwiseCoefficients.isEmpty))
      else
        assertEquals(ar.runs.head.voxelwiseCoefficients.length, 2)
        assert(math.abs(ar.runs.head.voxelwiseCoefficients(0).head - ar.runs.head.voxelwiseCoefficients(1).head) > 1e-4)

  test("captured estimated AR coefficients and operator agree with different fit chunk sizes"):
    val model = largeModel
    val plan = FitPlan(model, FitEngine.GeneralizedLeastSquares,
      FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1))))
    val reader = SynchronousFmriDataset.readerFor(model.dataset).fold(e => fail(e.message), identity)
    val captured = FitPlanExecutor.fitWithTraceCapture(reader, plan, DataSelection.All, 80, 2)
      .fold(e => fail(e.toString), identity)
    for size <- Vector(1, 2) do
      val chunked = FitPlanExecutor.fitChunked(reader, plan, DataSelection.All,
        FitChunkingStrategy.unsafeByVoxelCount(size))
        .fold(e => fail(e.message), identity).asInstanceOf[DenseFmriFitResult]
      assertEquals(chunked.autocorrelation, captured.autocorrelation)
      for predictor <- 0 until captured.predictors; voxel <- 0 until captured.voxels do
        assertEqualsDouble(chunked.coefficients(predictor, voxel), captured.coefficients(predictor, voxel), 1e-10)
