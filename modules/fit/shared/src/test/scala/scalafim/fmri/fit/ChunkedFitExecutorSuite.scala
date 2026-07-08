package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.{ConvolvedTerm, EventModel, EventTermColumnRole}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FitStrategy, FmriModel, FmriModelBuilder, ModelBuildSpec}
import scalafim.image.{DMat, NeuroSpace}
import scalafim.linalg.{DoubleMatrix, DoubleVector}

import scala.concurrent.ExecutionContext.Implicits.global

class ChunkedFitExecutorSuite extends munit.FunSuite:

  private val chunking = FitChunkingStrategy.unsafeByVoxelCount(2)
  private val singleVoxelChunking = FitChunkingStrategy.unsafeByVoxelCount(1)

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
        val actual = result.toOption.get.asInstanceOf[DenseFmriFitResult]
        assertDenseClose(actual, expected)
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
      FmriDataset(
        backend = InMemoryDatasetBackend(DatasetId(id), DMat.fromRows(rows), NeuroSpace(Vector(3, 1, 1))),
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
      FmriDataset(
        backend = InMemoryDatasetBackend(
          DatasetId("chunked-lss-demo"),
          DMat.fromRows(rows),
          NeuroSpace(Vector(3, 1, 1))
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
    assertEquals(actual.olsDiagnostics, expected.olsDiagnostics)
    assertEquals(actual.autocorrelation, expected.autocorrelation)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, tol = 1e-10)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, tol = 1e-10)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance, tol = 1e-10)
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

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tol)
        col += 1
      row += 1

  private def assertVectorClose(actual: DoubleVector, expected: DoubleVector, tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1
