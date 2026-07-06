package scalafim.fmri.fit

import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine}
import scalafim.linalg.DoubleMatrix

class FitBlockSuite extends munit.FunSuite:

  private val timepoints = (0 until 8).toVector

  test("dense OLS block results merge to the same result as a full voxel block") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(timepoints.map(i => Vector(1.0, i.toDouble)))
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(
        timepoints.map { i =>
          val x = i.toDouble
          Vector(
            1.0 + 2.0 * x,
            2.0 - x,
            -1.0 + 0.5 * x
          )
        }
      )
    )

    val full = denseKernel(
      FitBlockInput(design, response, voxelIndices = Vector(0, 1, 2), timepoints = timepoints),
      FitEngine.OrdinaryLeastSquares
    )
    val left = denseKernel(
      FitBlockInput(design, selectResponse(response, Vector(0, 1)), voxelIndices = Vector(0, 1), timepoints = timepoints),
      FitEngine.OrdinaryLeastSquares
    )
    val right = denseKernel(
      FitBlockInput(design, selectResponse(response, Vector(2)), voxelIndices = Vector(2), timepoints = timepoints),
      FitEngine.OrdinaryLeastSquares
    )

    val merged = DenseFitBlockResult.merge(Vector(left, right)).toOption.get

    assertDenseClose(merged, full, tol = 1e-12)
  }

  test("fixed AR(1) GLS block results merge with identical diagnostics") {
    val design = DesignMatrix.unsafe(
      DoubleMatrix.fromRows(
        timepoints.map { i =>
          val x = (i % 4).toDouble - 1.5
          Vector(1.0, x, if i >= 4 then 1.0 else 0.0)
        }
      )
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(
        timepoints.map { i =>
          val x = (i % 4).toDouble - 1.5
          val run = if i >= 4 then 1.0 else 0.0
          Vector(
            2.0 + 1.25 * x + 0.5 * run + deterministicResidual(i, 0),
            -1.0 - 0.75 * x + 0.2 * run + deterministicResidual(i, 1),
            0.5 + 0.4 * x - 0.3 * run + deterministicResidual(i, 2)
          )
        }
      )
    )
    val partitions = Vector(
      RunPartition(0, rowIndices = Vector(0, 1, 2, 3), timepoints = Vector(0, 1, 2, 3)),
      RunPartition(1, rowIndices = Vector(4, 5, 6, 7), timepoints = Vector(4, 5, 6, 7))
    )
    val config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.25)))

    val full = denseKernel(
      FitBlockInput(design, response, voxelIndices = Vector(0, 1, 2), timepoints = timepoints, partitions = partitions),
      FitEngine.GeneralizedLeastSquares,
      config
    )
    val left = denseKernel(
      FitBlockInput(design, selectResponse(response, Vector(0, 1)), voxelIndices = Vector(0, 1), timepoints = timepoints, partitions = partitions),
      FitEngine.GeneralizedLeastSquares,
      config
    )
    val right = denseKernel(
      FitBlockInput(design, selectResponse(response, Vector(2)), voxelIndices = Vector(2), timepoints = timepoints, partitions = partitions),
      FitEngine.GeneralizedLeastSquares,
      config
    )

    val merged = DenseFitBlockResult.merge(Vector(left, right)).toOption.get

    assertEquals(merged.autocorrelation, full.autocorrelation)
    assertDenseClose(merged, full, tol = 1e-10)
  }

  test("LSS block results merge to the same result as a full voxel block") {
    val trials = DoubleMatrix.fromRows(
      timepoints.map { i =>
        val x = i.toDouble
        Vector(
          if i <= 2 then 1.0 - x * 0.1 else 0.0,
          if i >= 2 && i <= 5 then 0.2 + x * 0.05 else 0.0,
          if i >= 5 then 0.4 + x * 0.03 else 0.0
        )
      }
    )
    val fixed = DoubleMatrix.fromRows(
      timepoints.map(i => Vector(1.0, i.toDouble - 3.5))
    )
    val response = ResponseBlock.unsafe(
      DoubleMatrix.fromRows(
        timepoints.map { i =>
          val x = i.toDouble
          Vector(
            1.0 + math.sin(x / 3.0),
            -0.5 + math.cos(x / 4.0),
            0.25 + x / 10.0
          )
        }
      )
    )
    val trialDesign = LssTrialDesign.unsafe(trials, Vector("trial_1", "trial_2", "trial_3"))
    val prepared =
      LeastSquaresSeparate.unsafePrepare(
        trialDesign,
        LssFixedDesign.unsafe(fixed, Vector("intercept", "trend"))
      )
    val design = DesignMatrix.unsafe(trials)

    val full = lssKernel(
      FitBlockInput(
        design,
        response,
        voxelIndices = Vector(0, 1, 2),
        timepoints = timepoints,
        lssDesign = Some(LssBlockDesign(prepared))
      )
    )
    val left = lssKernel(
      FitBlockInput(
        design,
        selectResponse(response, Vector(0, 1)),
        voxelIndices = Vector(0, 1),
        timepoints = timepoints,
        lssDesign = Some(LssBlockDesign(prepared))
      )
    )
    val right = lssKernel(
      FitBlockInput(
        design,
        selectResponse(response, Vector(2)),
        voxelIndices = Vector(2),
        timepoints = timepoints,
        lssDesign = Some(LssBlockDesign(prepared))
      )
    )

    val merged = LssFitBlockResult.merge(Vector(left, right)).toOption.get

    assertLssClose(merged, full, tol = 1e-12)
  }

  test("dense block merge rejects incompatible execution context") {
    val design = DesignMatrix.unsafe(DoubleMatrix.fromRows(timepoints.map(i => Vector(1.0, i.toDouble))))
    val response = ResponseBlock.unsafe(DoubleMatrix.fromRows(timepoints.map(i => Vector(i.toDouble))))
    val block = denseKernel(
      FitBlockInput(design, response, voxelIndices = Vector(0), timepoints = timepoints),
      FitEngine.OrdinaryLeastSquares
    )
    val incompatible = block.copy(timepoints = timepoints.dropRight(1))

    assert(DenseFitBlockResult.merge(Vector(block, incompatible)).left.toOption.exists {
      case FitError.IncompatibleFitBlocks(detail) => detail.contains("timepoints")
      case _                                      => false
    })
  }

  test("LSS block merge rejects incompatible trial names") {
    val coefficients = CoefficientBlock(DoubleMatrix.fromRows(Vector(Vector(1.0), Vector(2.0))))
    val diagnostics = LssDiagnostics(fixedRank = 0, zeroTrialRegressors = Vector.empty, degenerateOtherRegressors = Vector.empty)
    val block = LssFitBlockResult(
      coefficients = coefficients,
      trialNames = Vector("a", "b"),
      diagnostics = diagnostics,
      voxelIndices = Vector(0),
      timepoints = timepoints
    )
    val incompatible = block.copy(trialNames = Vector("a", "c"))

    assert(LssFitBlockResult.merge(Vector(block, incompatible)).left.toOption.exists {
      case FitError.IncompatibleFitBlocks(detail) => detail.contains("trial names")
      case _                                      => false
    })
  }

  private def denseKernel(
      input: FitBlockInput,
      engine: FitEngine,
      config: FitConfig = FitConfig()
  ): DenseFitBlockResult =
    FitKernel.fit(input, engine, config) match
      case Right(dense: DenseFitBlockResult) => dense
      case Right(other) => fail(s"expected dense block result, got ${other.engine}")
      case Left(error)  => fail(error.message)

  private def lssKernel(input: FitBlockInput): LssFitBlockResult =
    FitKernel.fit(input, FitEngine.LeastSquaresSeparate) match
      case Right(lss: LssFitBlockResult) => lss
      case Right(other) => fail(s"expected LSS block result, got ${other.engine}")
      case Left(error)  => fail(error.message)

  private def selectResponse(response: ResponseBlock, cols: Vector[Int]): ResponseBlock =
    val out = new Array[Double](response.timepoints * cols.length)
    var row = 0
    while row < response.timepoints do
      var outCol = 0
      while outCol < cols.length do
        out(row * cols.length + outCol) = response.value(row, cols(outCol))
        outCol += 1
      row += 1
    ResponseBlock.unsafe(DoubleMatrix.unsafe(response.timepoints, cols.length, out))

  private def assertDenseClose(actual: DenseFitBlockResult, expected: DenseFitBlockResult, tol: Double): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.residualDegreesOfFreedom, expected.residualDegreesOfFreedom)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, tol)
    assertMatrixClose(actual.standardErrors.value, expected.standardErrors.value, tol)
    assertMatrixClose(actual.normalizedCovariance, expected.normalizedCovariance, tol)
    assertVectorClose(actual.residualVariance.toVector, expected.residualVariance.toVector, tol)

  private def assertLssClose(actual: LssFitBlockResult, expected: LssFitBlockResult, tol: Double): Unit =
    assertEquals(actual.engine, expected.engine)
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.timepoints, expected.timepoints)
    assertEquals(actual.trialNames, expected.trialNames)
    assertEquals(actual.diagnostics, expected.diagnostics)
    assertMatrixClose(actual.coefficients.value, expected.coefficients.value, tol)

  private def assertMatrixClose(actual: DoubleMatrix, expected: DoubleMatrix, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    actual.copyData.zip(expected.copyData).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def deterministicResidual(row: Int, voxel: Int): Double =
    val raw = math.sin((row + 1).toDouble * 12.9898 + (voxel + 1).toDouble * 78.233) * 43758.5453
    ((raw - math.floor(raw)) * 2.0 - 1.0) * 0.05
