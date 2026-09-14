package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*
import scalafim.dataset.{DataSelection, DatasetEvents, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.StructuralColumnOrigin
import scalafim.fmri.design.baseline.Intercept
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.model.{
  ArCoefficientSpec,
  AutocorrelationConfig,
  CoefficientScope,
  FitControls,
  FitEngine,
  FitPlan,
  FitStrategy,
  FmriModel,
  FmriModelBuilder,
  MissingDataPolicy,
  ModelBuildSpec
}
import scalafim.image.SampleSpaces
import gale.linalg.DMat

class RunwiseGlsSuite extends munit.FunSuite:
  private val Rho = 0.35

  test("runwise GLS is an explicit run-specific estimand and refuses across-run AR pooling") {
    val fixture = new StructuralFixture(Vector(28, 31), voxels = 2)
    val runwise = FitStrategy.RunwiseGeneralizedLeastSquares(fixedAr(Rho))
    val shared = FitStrategy.GeneralizedLeastSquares(fixedAr(Rho))
    val runwisePlan = FitPlan(fixture.model, runwise)
    val sharedPlan = FitPlan(fixture.model, shared)

    assertEquals(runwisePlan.engine, FitEngine.GeneralizedLeastSquares)
    assertEquals(runwisePlan.coefficientScope, CoefficientScope.RunSpecific)
    assertEquals(runwisePlan.summary.autocorrelated, true)
    assertEquals(sharedPlan.coefficientScope, CoefficientScope.SharedAcrossRuns)

    val pooledNoise = FitStrategy.RunwiseGeneralizedLeastSquares(
      AutocorrelationConfig.unsafe(global = true)
    )
    assert(FitPlan.make(fixture.model, pooledNoise).left.toOption.exists { error =>
      error.message.contains("global=false") && error.message.contains("shared-coefficient")
    })

    val unlikeRows = FitStrategy.RunwiseGeneralizedLeastSquares(
      fixedAr(Rho),
      FitControls(missingData = MissingDataPolicy.OmitRowsPerVoxel)
    )
    val unsupported = FitPlanExecutor.fit(FitPlan(fixture.model, unlikeRows))
    assert(unsupported.left.toOption.exists {
      case FitError.UnsupportedMissingDataPolicy(detail) =>
        detail.contains("runwise GLS") && detail.contains("run-local AR receipts")
      case _ => false
    })
  }

  test("fixed AR(1) runwise GLS matches an independent segmented GLS reference") {
    val x = Vector(-1.0, 0.5, 1.5, -0.25, 2.0, 0.75, -1.5, 1.0, 0.2, 1.7, -0.8, 0.9, 2.2, -1.1)
    val designRows = x.map(value => Vector(value, 1.0))
    val responseRows = x.indices.toVector.map { row =>
      val noise0 = 0.18 * math.sin((row + 1).toDouble * 0.71)
      val noise1 = 0.14 * math.cos((row + 2).toDouble * 0.43)
      if row < 7 then Vector(1.8 * x(row) + 0.7 + noise0, -0.9 * x(row) + 1.2 + noise1)
      else Vector(-1.1 * x(row) + 2.4 + noise0, 1.4 * x(row) - 0.3 + noise1)
    }
    val design = DesignMatrix.unsafe(GaleTestMatrix.fromRows(designRows))
    val response = ResponseBlock.unsafe(GaleTestMatrix.fromRows(responseRows))
    val partitions = Vector(
      RunPartition(0, (0 until 7).toVector, Vector(0, 1, 3, 4, 5, 7, 8)),
      RunPartition(1, (7 until 14).toVector, Vector(10, 11, 12, 14, 15, 16, 18))
    )

    val actual = RunwiseGls.fit(
      design,
      response,
      partitions,
      fixedAr(Rho).toLegacy
    ).fold(error => fail(error.message), identity)

    actual.runs.zip(partitions).foreach { case (run, partition) =>
      val localDesign = partition.rowIndices.map(designRows)
      val localResponse = partition.rowIndices.map(responseRows)
      val expected = independentAr1Gls(localDesign, localResponse, partition.timepoints, Rho)
      assertMatrixClose(run.fit.coefficients.value, expected.coefficients, 1e-10)
      assertMatrixClose(run.fit.normalizedCovariance, expected.normalizedCovariance, 1e-10)
      assertVectorClose(run.fit.residualVariance.toVector, expected.residualVariance, 1e-10)
      assertEquals(run.fit.residualDegreesOfFreedom.value, partition.rowIndices.length - 2)
      assertEquals(run.fit.diagnostics.runs.map(_.runIndex), Vector(partition.runIndex))
      assert(run.fit.diagnostics.whitening.censorGaps.nonEmpty)
      assert(math.abs(run.fit.normalizedCovariance(0, 1)) > 1e-6)
    }
    assert(actual.runs.head.fit.coefficients(0, 0) > 1.5)
    assert(actual.runs.last.fit.coefficients(0, 0) < -0.8)
  }

  test("structural FIR axes, selected gaps and fixed AR covariance survive public and chunked execution") {
    val fixture = new StructuralFixture(Vector(38, 43), voxels = 3)
    val plan = FitPlan(fixture.model, FitStrategy.RunwiseGeneralizedLeastSquares(fixedAr(Rho)))
    val selection = DataSelection(
      time = IndexSelection.indices(fixture.selectedTimepoints*),
      voxels = IndexSelection.indices(2, 0, 1)
    )
    val direct = runwise(FitPlanExecutor.fit(plan, selection))
    val chunked = runwise(FitPlanExecutor.fitChunked(
      plan,
      selection,
      FitChunkingStrategy.unsafeByVoxelCount(1)
    ))

    assertEquals(direct.engine, FitEngine.GeneralizedLeastSquares)
    assertEquals(direct.summary.coefficientScope, CoefficientScope.RunSpecific)
    assertEquals(direct.timepoints, fixture.selectedTimepoints)
    assertEquals(direct.voxelIndices, Vector(2, 0, 1))
    assertEquals(direct.runs.map(_.timepoints), fixture.selectedByRun)
    assertEquals(direct.runs.map(_.sourceColumns), chunked.runs.map(_.sourceColumns))
    assertEquals(direct.runs.map(_.projection.map(_.sourceColumnIndices)), chunked.runs.map(_.projection.map(_.sourceColumnIndices)))
    direct.runs.foreach { run =>
      val firRoles = run.coefficientAxis.toVector.flatMap(_.columns.flatMap { column =>
        column.origin match
          case StructuralColumnOrigin.Event(_, _, _, _, Some(reference), _, _) => reference.role
          case _ => None
      })
      assertEquals(firRoles, Vector(BasisRole.FirBin(1, 0.s, 2.s), BasisRole.FirBin(2, 2.s, 4.s)))
      assert(run.autocorrelation.exists(_.whitening.censorGaps.nonEmpty))
    }
    assertRunwiseClose(direct, chunked, 1e-10)
    assert(direct.runs.head.coefficients(0, 0) > 0.8)
    assert(direct.runs.last.coefficients(0, 0) < -0.2)
  }

  test("estimated voxelwise AR is chunk invariant and fixed effects consume every covariance matrix") {
    val fixture = new StructuralFixture(Vector(72, 79), voxels = 3)
    val strategy = FitStrategy.RunwiseGeneralizedLeastSquares(
      AutocorrelationConfig.unsafe(voxelwise = true)
    )
    val plan = FitPlan(fixture.model, strategy)
    val selection = DataSelection(voxels = IndexSelection.indices(2, 0, 1))
    val direct = runwise(FitPlanExecutor.fit(plan, selection))
    val chunked = runwise(FitPlanExecutor.fitChunked(
      plan,
      selection,
      FitChunkingStrategy.unsafeByVoxelCount(1)
    ))

    assertRunwiseClose(direct, chunked, 1e-9)
    direct.runs.foreach { run =>
      assertEquals(run.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
      assertEquals(run.coefficientCovariance.matrixCount, 3)
      assert(run.autocorrelation.exists(!_.sharedNormalizedCovariance))
      assertEquals(run.autocorrelation.toVector.flatMap(_.runs).flatMap(_.voxelwiseCoefficients).length, 3)
    }
    val adapted = direct.runs.head.denseResult(
      direct.columnNames,
      direct.voxelIndices,
      direct.summary
    ).fold(error => fail(error.message), identity)
    assertEquals(adapted.engine, FitEngine.GeneralizedLeastSquares)
    assertEquals(adapted.summary.coefficientScope, CoefficientScope.RunSpecific)
    assertEquals(adapted.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
    assertEquals(adapted.coefficientCovariance.matrixCount, 3)
    assert(adapted.autocorrelation.nonEmpty)
    val contrast = TContrast.column(adapted.columnNames.head).evaluate(adapted)
      .fold(error => fail(error.message), identity)
    assertEquals(contrast.fitProvenance.map(_.engine), Some(FitEngine.GeneralizedLeastSquares))
    assert(contrast.fitProvenance.exists(_.summary.coefficientScope == CoefficientScope.RunSpecific))
    assert(contrast.fitProvenance.exists(_.autocorrelation.nonEmpty))

    val fixed = FixedEffects.combine(direct).fold(error => fail(error.message), identity)
    val sourceColumns = fixed.sufficientStatistics.effectiveSourceColumnIndices
    assertEquals(sourceColumns.length, 2)
    assertEquals(fixed.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
    var foundOffDiagonal = false
    var voxel = 0
    while voxel < fixed.voxels do
      val expected = independentFixedEffects(direct, sourceColumns, voxel)
      var predictor = 0
      while predictor < sourceColumns.length do
        assertEqualsDouble(fixed.coefficients(predictor, voxel), expected(predictor), 1e-9)
        predictor += 1
      val covariance = fixed.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel)
      if math.abs(covariance(0, 1)) > 1e-8 then foundOffDiagonal = true
      voxel += 1
    assert(foundOffDiagonal, "the qualified fixed-effects result must retain off-diagonal covariance")
  }

  private final case class IndependentGls(
      coefficients: DMat,
      normalizedCovariance: DMat,
      residualVariance: Vector[Double]
  )

  private def fixedAr(rho: Double): AutocorrelationConfig =
    AutocorrelationConfig.unsafe(
      iterations = 0,
      coefficients = ArCoefficientSpec.Rho(rho)
    )

  private def runwise(result: Either[FitError, FmriFitResult]): RunwiseFmriFitResult =
    result match
      case Right(value: RunwiseFmriFitResult) => value
      case Right(value)                       => fail(s"expected runwise result, found ${value.getClass.getSimpleName}")
      case Left(error)                        => fail(error.message)

  private def assertRunwiseClose(actual: RunwiseFmriFitResult, expected: RunwiseFmriFitResult, tolerance: Double): Unit =
    assertEquals(actual.voxelIndices, expected.voxelIndices)
    assertEquals(actual.runs.length, expected.runs.length)
    actual.runs.zip(expected.runs).foreach { case (left, right) =>
      assertMatrixClose(left.coefficients.value, right.coefficients.value, tolerance)
      assertMatrixClose(left.standardErrors.value, right.standardErrors.value, tolerance)
      assertVectorClose(left.residualVariance.toVector, right.residualVariance.toVector, tolerance)
      assertEquals(left.autocorrelation.map(_.runs.map(_.runIndex)), right.autocorrelation.map(_.runs.map(_.runIndex)))
      assertEquals(left.coefficientCovariance.scope, right.coefficientCovariance.scope)
      assertEquals(left.coefficientCovariance.matrixCount, right.coefficientCovariance.matrixCount)
      var voxel = 0
      while voxel < left.coefficientCovariance.matrixCount do
        assertMatrixClose(
          left.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel),
          right.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel),
          tolerance
        )
        voxel += 1
    }

  /** Test-only GLS reference: manually segments and whitens fixed AR(1), then
    * solves the two-predictor normal equations in closed form.
    */
  private def independentAr1Gls(
      design: Vector[Vector[Double]],
      response: Vector[Vector[Double]],
      timepoints: Vector[Int],
      rho: Double
  ): IndependentGls =
    val scale = math.sqrt(1.0 - rho * rho)
    val whitenedX = Vector.tabulate(design.length) { row =>
      val startsSegment = row == 0 || timepoints(row) > timepoints(row - 1) + 1
      Vector.tabulate(2) { col =>
        if startsSegment then design(row)(col) * scale
        else design(row)(col) - rho * design(row - 1)(col)
      }
    }
    val whitenedY = Vector.tabulate(response.length) { row =>
      val startsSegment = row == 0 || timepoints(row) > timepoints(row - 1) + 1
      Vector.tabulate(response.head.length) { voxel =>
        if startsSegment then response(row)(voxel) * scale
        else response(row)(voxel) - rho * response(row - 1)(voxel)
      }
    }
    val xx00 = whitenedX.map(row => row(0) * row(0)).sum
    val xx01 = whitenedX.map(row => row(0) * row(1)).sum
    val xx11 = whitenedX.map(row => row(1) * row(1)).sum
    val inverse = inverse2(xx00, xx01, xx11)
    val coefficients = Vector.tabulate(response.head.length) { voxel =>
      val xy0 = whitenedX.indices.map(row => whitenedX(row)(0) * whitenedY(row)(voxel)).sum
      val xy1 = whitenedX.indices.map(row => whitenedX(row)(1) * whitenedY(row)(voxel)).sum
      Vector(inverse(0) * xy0 + inverse(1) * xy1, inverse(1) * xy0 + inverse(2) * xy1)
    }
    val residualVariance = coefficients.indices.map { voxel =>
      val rss = whitenedX.indices.map { row =>
        val residual = whitenedY(row)(voxel) - whitenedX(row)(0) * coefficients(voxel)(0) -
          whitenedX(row)(1) * coefficients(voxel)(1)
        residual * residual
      }.sum
      rss / (whitenedX.length - 2).toDouble
    }.toVector
    IndependentGls(
      GaleTestMatrix.fromRows(Vector(
        coefficients.map(_(0)).toVector,
        coefficients.map(_(1)).toVector
      )),
      GaleTestMatrix.fromRows(Vector(Vector(inverse(0), inverse(1)), Vector(inverse(1), inverse(2)))),
      residualVariance
    )

  private def independentFixedEffects(
      result: RunwiseFmriFitResult,
      sourceColumns: Vector[Int],
      voxel: Int
  ): Vector[Double] =
    var p00 = 0.0
    var p01 = 0.0
    var p11 = 0.0
    var rhs0 = 0.0
    var rhs1 = 0.0
    result.runs.foreach { run =>
      val positions = sourceColumns.map(run.sourceColumns.indexOf)
      val covariance = run.coefficientCovariance.unsafeMatrixForVoxelPosition(voxel)
      val scale = run.residualVariance(voxel)
      val inverse = inverse2(
        covariance(positions(0), positions(0)) * scale,
        covariance(positions(0), positions(1)) * scale,
        covariance(positions(1), positions(1)) * scale
      )
      val beta0 = run.coefficients(positions(0), voxel)
      val beta1 = run.coefficients(positions(1), voxel)
      p00 += inverse(0)
      p01 += inverse(1)
      p11 += inverse(2)
      rhs0 += inverse(0) * beta0 + inverse(1) * beta1
      rhs1 += inverse(1) * beta0 + inverse(2) * beta1
    }
    val covariance = inverse2(p00, p01, p11)
    Vector(
      covariance(0) * rhs0 + covariance(1) * rhs1,
      covariance(1) * rhs0 + covariance(2) * rhs1
    )

  private def inverse2(a: Double, b: Double, d: Double): Vector[Double] =
    val determinant = a * d - b * b
    assert(math.abs(determinant) > 1e-14, clues(a, b, d, determinant))
    Vector(d / determinant, -b / determinant, a / determinant)

  private def assertMatrixClose(actual: DMat, expected: DMat, tolerance: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        assertEqualsDouble(actual(row, col), expected(row, col), tolerance)
        col += 1
      row += 1

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tolerance: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach { case (left, right) =>
      assertEqualsDouble(left, right, tolerance)
    }

  private final class StructuralFixture(runLengths: Vector[Int], voxels: Int):
    require(runLengths.length == 2, "fixture expects two runs")
    require(voxels >= 1, "fixture needs at least one voxel")
    private val total = runLengths.sum
    private val frame = SamplingFrame(blockLens = runLengths, tr = Vector(1.0, 1.0))
    private val events = DatasetEvents(Vector(
      Map("onset" -> "2.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "10.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "18.0", "cond" -> "A", "run" -> "run-1"),
      Map("onset" -> "2.0", "cond" -> "A", "run" -> "run-2"),
      Map("onset" -> "11.0", "cond" -> "A", "run" -> "run-2"),
      Map("onset" -> "20.0", "cond" -> "A", "run" -> "run-2")
    ))

    private def dataset(id: String, values: Vector[Vector[Double]]): FmriDataset =
      FmriDataset.unsafe(
        InMemoryDatasetBackend(
          DatasetId(id),
          GaleTestMatrix.fromRows(values),
          SampleSpaces(Vector(voxels, 1, 1))
        ),
        frame,
        events
      )

    private val empty = dataset("runwise-gls-structural-empty", Vector.fill(total)(Vector.fill(voxels)(0.0)))
    private val compiled = FmriModelBuilder.buildModel(
      empty,
      ModelBuildSpec(
        formula = "onset ~ hrf(cond)",
        blockColumn = Some("run"),
        baselineIntercept = Intercept.Runwise,
        defaultHrf = Hrfs.fir(nBasis = 2, span = 4.s),
        precision = 0.25.s,
        strategy = FitStrategy.RunwiseGeneralizedLeastSquares(fixedAr(Rho))
      )
    )
    private val taskColumns = compiled.designSchema.get.columns.zipWithIndex.collect {
      case (column, index) if column.origin.isInstanceOf[StructuralColumnOrigin.Event] => index
    }
    private val runStart = Vector(0, runLengths.head)
    private val noise = Vector.tabulate(voxels) { voxel =>
      runLengths.zipWithIndex.flatMap { case (length, run) =>
        val rho = Vector(0.72, -0.48, 0.21, 0.58)(voxel % 4) * (if run == 0 then 1.0 else 0.8)
        val out = Array.ofDim[Double](length)
        var row = 0
        while row < length do
          val innovation = 0.035 * math.sin((row + 1 + voxel * 17 + run * 29).toDouble * 0.79)
          out(row) = innovation + (if row == 0 then 0.0 else rho * out(row - 1))
          row += 1
        out.toVector
      }
    }
    private val values = Vector.tabulate(total) { row =>
      val run = if row < runLengths.head then 0 else 1
      Vector.tabulate(voxels) { voxel =>
        var signal = 0.0
        var column = 0
        while column < compiled.nPredictors do
          val taskPosition = taskColumns.indexOf(column)
          val coefficient =
            if taskPosition >= 0 then
              val base = if run == 0 then Vector(1.4, 3.2)(taskPosition) else Vector(-0.7, 1.8)(taskPosition)
              base * (voxel + 1).toDouble
            else 0.4 * (run + 1) * (voxel + 1).toDouble
          signal += compiled.designMatrix(row, column) * coefficient
          column += 1
        signal + noise(voxel)(row)
      }
    }
    val model: FmriModel = compiled.copy(dataset = dataset("runwise-gls-structural", values))
    val selectedTimepoints: Vector[Int] =
      (0 until total).filterNot { row =>
        row == 5 || row == 14 || row == runLengths.head + 7 || row == runLengths.head + 19
      }.toVector
    val selectedByRun: Vector[Vector[Int]] = runLengths.indices.toVector.map { run =>
      val start = runStart(run)
      val end = start + runLengths(run)
      selectedTimepoints.filter(row => row >= start && row < end)
    }
