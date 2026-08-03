package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DatasetId, FmriDataset, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{
  ArOptions,
  ArStructure,
  AutocorrelationConfig,
  FitEngine,
  FitPlan,
  FitStrategy,
  FmriModel,
  RobustAutocorrelation,
  RobustConfig,
  RobustOptions,
  RobustPsi,
  ScaleScope
}
import scalafim.fmri.fit.fixtures.FmriregRobustFixtures
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import gale.linalg.{DMat, DVec}

import scala.concurrent.ExecutionContext.Implicits.global

class RobustSuite extends munit.FunSuite:

  private val nTime = 12
  private val rows = (0 until nTime).toVector
  private val targetSlope = 2.0
  private val targetIntercept = 1.0

  test("disabled robust options are equivalent to OLS") {
    val design = designMatrix()
    val response = responseBlock(outlier = false)
    val ols = Ols.unsafeFit(design, response)
    val robust = Robust
      .fit(design, response, partitions, RobustOptions())
      .toOption
      .get

    assertEquals(robust.diagnostics.psi, RobustPsi.Disabled)
    assertEquals(robust.diagnostics.iterations, 0)
    assertEquals(robust.diagnostics.sharedNormalizedCovariance, true)
    assertMatrixClose(robust.coefficients.value, ols.coefficients.value, 1e-12)
    assertMatrixClose(robust.standardErrors.value, ols.standardErrors.value, 1e-12)
  }

  test("Huber robust fit downweights an outlying observation") {
    val design = designMatrix()
    val response = responseBlock(outlier = true)
    val ols = Ols.unsafeFit(design, response)
    val robust = Robust
      .fit(
        design,
        response,
        partitions,
        RobustOptions(psi = RobustPsi.Huber(), maxIterations = 8, scaleScope = ScaleScope.Voxel)
      )
      .toOption
      .get

    val olsSlopeError = math.abs(ols.coefficients(0, 0) - targetSlope)
    val robustSlopeError = math.abs(robust.coefficients(0, 0) - targetSlope)

    assertEquals(robust.diagnostics.scaleScope, ScaleScope.Voxel)
    assertEquals(robust.diagnostics.weights.rows, nTime)
    assertEquals(robust.diagnostics.weights.cols, 1)
    assertEquals(robust.diagnostics.weightTopology, RobustWeightTopology.RowWise)
    assert(robust.diagnostics.weights(nTime - 1, 0) < 0.25, clues(robust.diagnostics.weights(nTime - 1, 0)))
    assert(robustSlopeError < olsSlopeError, clues(robust.coefficients(0, 0), ols.coefficients(0, 0)))
    assert(robust.standardErrors.value.copyData.forall(_.isFinite))
  }

  test("Bisquare robust fit can reject a severe outlier") {
    val robust = Robust
      .fit(
        designMatrix(),
        responseBlock(outlier = true),
        partitions,
        RobustOptions(psi = RobustPsi.Bisquare(), maxIterations = 8, scaleScope = ScaleScope.Run)
      )
      .toOption
      .get

    assertEquals(robust.diagnostics.scaleEstimates.labels, Vector("run_0"))
    assertEquals(robust.diagnostics.scaleEstimates.values.rows, 1)
    assertEquals(robust.diagnostics.scaleEstimates.values.cols, 1)
    assertEqualsDouble(robust.diagnostics.weights(nTime - 1, 0), 0.0, 1e-12)
    assert(math.abs(robust.coefficients(0, 0) - targetSlope) < 0.5, clues(robust.coefficients(0, 0)))
  }

  test("RobustLeastSquares executes through FitPlanExecutor") {
    val robust = RobustConfig.huber(maxIterations = 8, scaleScope = ScaleScope.Voxel).toOption.get
    val result = FitPlanExecutor
      .unsafeFit(FitPlan(model(outlier = true), FitStrategy.RobustLeastSquares(robust)))
      .asInstanceOf[DenseFmriFitResult]

    assertEquals(result.engine, FitEngine.RobustLeastSquares)
    assertEquals(result.robustDiagnostics.map(_.psi), Some(RobustPsi.Huber()))
    assert(result.robustDiagnostics.exists(_.weights(nTime - 1, 0) < 0.25))
    assert(result.inferenceReady.isRight)
  }

  test("chunked robust fit uses full-selection prepared row weights") {
    val robust = RobustConfig.huber(maxIterations = 8, scaleScope = ScaleScope.Voxel).toOption.get
    val plan = FitPlan(twoVoxelModel, FitStrategy.RobustLeastSquares(robust))

    val full = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val chunked = FitPlanExecutor
      .fitChunked(plan, chunking = FitChunkingStrategy.unsafeByVoxelCount(1))
      .toOption
      .get
      .asInstanceOf[DenseFmriFitResult]

    assertMatrixClose(chunked.coefficients.value, full.coefficients.value, 1e-10)
    assertMatrixClose(chunked.standardErrors.value, full.standardErrors.value, 1e-10)
    assertEquals(chunked.robustDiagnostics.map(_.weightTopology), Some(RobustWeightTopology.RowWise))
    assertEquals(chunked.robustDiagnostics.map(_.weights.cols), Some(1))
    assertEquals(chunked.robustDiagnostics.map(_.scaleEstimates.values.cols), Some(2))
  }

  test("fmrireg row-wise Huber fixture matches run-scope robust IRLS semantics") {
    val robust = Robust
      .fit(
        DesignMatrix.unsafe(FmriregRobustFixtures.design),
        ResponseBlock.unsafe(FmriregRobustFixtures.volumeSpikeResponse),
        Vector(RunPartition(0, rowIndices = (0 until 8).toVector, timepoints = (0 until 8).toVector)),
        RobustOptions(psi = RobustPsi.Huber(), maxIterations = 4, scaleScope = ScaleScope.Run)
      )
      .toOption
      .get

    assertEquals(robust.diagnostics.weightTopology, RobustWeightTopology.RowWise)
    assertMatrixClose(robust.coefficients.value, FmriregRobustFixtures.huberRunCoefficients, 1e-6)
    assertVectorClose(
      (0 until robust.diagnostics.weights.rows).map(row => robust.diagnostics.weights(row, 0)).toVector,
      FmriregRobustFixtures.huberRunWeights,
      1e-6
    )
    assertVectorClose(
      (0 until robust.diagnostics.scaleEstimates.values.cols).map(col => robust.diagnostics.scaleEstimates.values(0, col)).toVector,
      FmriregRobustFixtures.huberRunScaleComponents,
      1e-6
    )
  }

  test("fmrireg voxel scale scope keeps per-voxel scale components with row-wise weights") {
    val robust = Robust
      .fit(
        DesignMatrix.unsafe(FmriregRobustFixtures.design),
        ResponseBlock.unsafe(FmriregRobustFixtures.volumeSpikeResponse),
        Vector(RunPartition(0, rowIndices = (0 until 8).toVector, timepoints = (0 until 8).toVector)),
        RobustOptions(psi = RobustPsi.Huber(), maxIterations = 4, scaleScope = ScaleScope.Voxel)
      )
      .toOption
      .get

    assertEquals(robust.diagnostics.weightTopology, RobustWeightTopology.RowWise)
    assertEquals(robust.diagnostics.weights.cols, 1)
    assertVectorClose(
      (0 until robust.diagnostics.scaleEstimates.values.cols).map(col => robust.diagnostics.scaleEstimates.values(0, col)).toVector,
      FmriregRobustFixtures.huberVoxelScaleComponents,
      1e-6
    )
  }

  test("robust-triggered AR re-estimation exposes shared AR diagnostics") {
    val robust = Robust
      .fit(
        robustArDesign,
        robustArResponse,
        robustArPartitions,
        RobustOptions(
          psi = RobustPsi.Huber(),
          maxIterations = 8,
          scaleScope = ScaleScope.Run,
          reestimateAutocorrelation = true
        ),
        Some(ArOptions(structure = ArStructure.Ar(1)))
      )
      .toOption
      .get

    val ar = robust.autocorrelation.get

    assertEquals(ar.sharedNormalizedCovariance, true)
    assertEquals(ar.runs.map(_.method), Vector("robust-estimated"))
    assertEquals(ar.runs.map(_.coefficients.length), Vector(1))
    assert(ar.runs.head.rho.isFinite)
    assert(robust.diagnostics.weights(spikeRow, 0) < 0.8, clues(robust.diagnostics.weights(spikeRow, 0)))
    assert(robust.normalizedCovariance.copyData.forall(_.isFinite))
    assert(robust.standardErrors.value.copyData.forall(_.isFinite))

    val missingPolicy =
      Robust.fit(
        robustArDesign,
        robustArResponse,
        robustArPartitions,
        RobustOptions(psi = RobustPsi.Huber(), reestimateAutocorrelation = true)
      )

    assert(missingPolicy.left.toOption.exists {
      case FitError.UnsupportedRobust(message) => message.contains("AR autocorrelation policy")
      case _                                   => false
    })
  }

  test("future chunked robust AR reuses full-selection whitening and row weights") {
    val robust = RobustConfig.huber(maxIterations = 8, scaleScope = ScaleScope.Run).toOption.get
    val ar = AutocorrelationConfig.unsafe(order = 1)
    val plan =
      FitPlan(
        robustArModel,
        FitStrategy.RobustLeastSquares(
          robust = robust,
          autocorrelation = RobustAutocorrelation.Reestimate(ar)
        )
      )
    val full = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    FutureChunkedFitExecutor
      .fit(plan, chunking = FitChunkingStrategy.unsafeByVoxelCount(1), parallelism = FitParallelism.unsafe(2))
      .map { result =>
        val chunked = result.toOption.get.asInstanceOf[DenseFmriFitResult]

        assertDenseClose(chunked, full)
        assertEquals(chunked.autocorrelation.map(_.runs.map(_.method)), Some(Vector("robust-estimated")))
        assertEquals(chunked.robustDiagnostics.map(_.weightTopology), Some(RobustWeightTopology.RowWise))
        assertEquals(chunked.robustDiagnostics.map(_.weights.cols), Some(1))
        assertEquals(chunked.robustDiagnostics.map(_.scaleEstimates.values.cols), Some(2))
        assert(chunked.inferenceReady.isRight)
      }
  }

  private def designMatrix(): DesignMatrix =
    DesignMatrix.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(rows.map(i => Vector(i.toDouble, 1.0)))
    )

  private def responseBlock(outlier: Boolean): ResponseBlock =
    ResponseBlock.unsafe(
      scalafim.fmri.fit.GaleTestMatrix.fromRows(
        rows.map { i =>
          val clean = targetSlope * i.toDouble + targetIntercept
          val value = if outlier && i == nTime - 1 then clean + 80.0 else clean
          Vector(value)
        }
      )
    )

  private def partitions: Vector[RunPartition] =
    Vector(RunPartition(0, rowIndices = rows, timepoints = rows))

  private def model(outlier: Boolean): FmriModel =
    val frame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0))
    val data =
      ImageDMat.fromRows(
        rows.map { i =>
          val clean = targetSlope * i.toDouble + targetIntercept
          val value = if outlier && i == nTime - 1 then clean + 80.0 else clean
          Vector(value)
        }
      )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("robust-demo"), data, NeuroSpace(Vector(1, 1, 1))),
        samplingFrame = frame
      )
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = frame,
        designMatrix = Mat.fromRows(rows.map(i => Vector(i.toDouble))),
        columnNames = Vector("task"),
        termSpans = Vector(0 -> 1),
        colIndices = Map("task" -> Vector(0))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = frame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    FmriModel(eventModel, baseline, dataset)

  private def twoVoxelModel: FmriModel =
    val frame = SamplingFrame(blockLens = Seq(nTime), tr = Seq(1.0))
    val data =
      ImageDMat.fromRows(
        rows.map { i =>
          val clean = targetSlope * i.toDouble + targetIntercept
          val outlying = if i == nTime - 1 then clean + 80.0 else clean
          Vector(outlying, -1.5 * i.toDouble + 5.0)
        }
      )
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("robust-two-voxel-demo"), data, NeuroSpace(Vector(2, 1, 1))),
        samplingFrame = frame
      )
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = frame,
        designMatrix = Mat.fromRows(rows.map(i => Vector(i.toDouble))),
        columnNames = Vector("task"),
        termSpans = Vector(0 -> 1),
        colIndices = Map("task" -> Vector(0))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = frame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    FmriModel(eventModel, baseline, dataset)

  private val arTime = 80
  private val arRows = (0 until arTime).toVector
  private val spikeRow = arTime - 6

  private def robustArTask: Vector[Double] =
    arRows.map(i => math.sin(i.toDouble / 5.0) + ((i % 7).toDouble - 3.0) / 10.0)

  private def robustArDesign: DesignMatrix =
    val task = robustArTask
    DesignMatrix.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(task.map(value => Vector(value, 1.0))))

  private def robustArResponse: ResponseBlock =
    ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(robustArResponseRows))

  private def robustArPartitions: Vector[RunPartition] =
    Vector(RunPartition(0, rowIndices = arRows, timepoints = arRows))

  private def robustArModel: FmriModel =
    val frame = SamplingFrame(blockLens = Seq(arTime), tr = Seq(1.0))
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("robust-ar-demo"), ImageDMat.fromRows(robustArResponseRows), NeuroSpace(Vector(2, 1, 1))),
        samplingFrame = frame
      )
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = frame,
        designMatrix = Mat.fromRows(robustArTask.map(value => Vector(value))),
        columnNames = Vector("task"),
        termSpans = Vector(0 -> 1),
        colIndices = Map("task" -> Vector(0))
      )
    val baseline =
      BaselineModel.build(
        samplingFrame = frame,
        basis = BaselineBasis.Constant,
        intercept = Intercept.Global
      )
    FmriModel(eventModel, baseline, dataset)

  private def robustArResponseRows: Vector[Vector[Double]] =
    val task = robustArTask
    val residualA = ar1Residual(phi = 0.62, n = arTime, offset = 37)
    val residualB = ar1Residual(phi = 0.44, n = arTime, offset = 911)
    arRows.map { row =>
      val spike = if row == spikeRow then 18.0 else 0.0
      Vector(
        1.2 * task(row) + 0.5 + residualA(row) + spike,
        -0.7 * task(row) + 1.8 + residualB(row) + spike * 0.7
      )
    }

  private def ar1Residual(phi: Double, n: Int, offset: Int): Vector[Double] =
    val out = Array.ofDim[Double](n)
    var i = 0
    while i < n do
      val raw = math.sin((i + offset + 1).toDouble * 12.9898 + 78.233) * 43758.5453
      val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
      out(i) = innovation + (if i == 0 then 0.0 else phi * out(i - 1))
      i += 1
    out.toVector

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

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1

  private def assertDenseClose(actual: DenseFmriFitResult, expected: DenseFmriFitResult): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.columnNames, expected.columnNames)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertEquals(actual.olsDiagnostics, expected.olsDiagnostics)
    assertEquals(actual.autocorrelation, expected.autocorrelation)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, 1e-10)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, 1e-10)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance, 1e-10)
    assertVectorClose(actual.residualVariance, expected.residualVariance, 1e-10)

  private def assertVectorClose(actual: DVec, expected: DVec, tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    var i = 0
    while i < actual.length do
      assertEqualsDouble(actual(i), expected(i), tol)
      i += 1
