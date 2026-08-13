package scalafim.fmri.fit

import scalafim.fmri.fit.GaleTestSyntax.*

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.ar.{ArmaCoefficients, TimeSegment, TimeSegments, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.fit.fixtures.{ArCensorGlsRFixture, FmriArEstimatedGlsFixture, FmriregGlsFixtures}
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FmriModel}
import scalafim.image.{DMat as ImageDMat, NeuroSpace}
import gale.linalg.DMat

class GlsSuite extends munit.FunSuite:

  private def assertMatrixClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows)
    assertEquals(actual.cols, expected.cols)
    var row = 0
    while row < actual.rows do
      var col = 0
      while col < actual.cols do
        val a = actual(row, col)
        val e = expected(row, col)
        assert(math.abs(a - e) <= tol, clues(row, col, a, e))
        col += 1
      row += 1

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def assertFinite(values: Iterable[Double]): Unit =
    assert(values.forall(_.isFinite), clues(values.toVector))

  private val rho = 0.4

  private def samplingFrame(blockLens: Vector[Int]): SamplingFrame =
    SamplingFrame(blockLens = blockLens, tr = Vector.fill(blockLens.length)(1.0))

  private def whitenRows(
      rows: Vector[Vector[Double]],
      phi: Vector[Double],
      resetAfterRows: Set[Int]
  ): Vector[Vector[Double]] =
    var segmentStart = 0
    rows.indices.toVector.map { row =>
      if row > 0 && resetAfterRows.contains(row - 1) then segmentStart = row
      rows(row).indices.toVector.map { col =>
        var value = rows(row)(col)
        var lag = 0
        while lag < phi.length do
          val lagged = row - lag - 1
          if lagged >= segmentStart then value -= phi(lag) * rows(lagged)(col)
          lag += 1
        if row == segmentStart && phi.length == 1 then value * math.sqrt(1.0 - phi.head * phi.head)
        else value
      }
    }

  private def orthogonalInnovation(
      designRows: Vector[Vector[Double]],
      seed: Vector[Double],
      phi: Vector[Double],
      resetAfterRows: Set[Int]
  ): Vector[Double] =
    val whitened = whitenRows(designRows, phi, resetAfterRows)
    val design = DesignMatrix.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(whitened))
    val response = ResponseBlock.unsafe(scalafim.fmri.fit.GaleTestMatrix.fromRows(seed.map(v => Vector(v))))
    val fit = Ols.unsafeFit(design, response)

    seed.zipWithIndex.map { case (value, row) =>
      var fitted = 0.0
      var col = 0
      while col < whitened.head.length do
        fitted += whitened(row)(col) * fit.coefficients(col, 0)
        col += 1
      value - fitted
    }

  private def unwhitenInnovation(
      z: Vector[Double],
      phi: Vector[Double],
      resetAfterRows: Set[Int]
  ): Vector[Double] =
    val out = Array.ofDim[Double](z.length)
    var segmentStart = 0
    var i = 0
    while i < z.length do
      if i > 0 && resetAfterRows.contains(i - 1) then segmentStart = i
      var value =
        if i == segmentStart && phi.length == 1 then z(i) / math.sqrt(1.0 - phi.head * phi.head)
        else z(i)
      var lag = 0
      while lag < phi.length do
        val lagged = i - lag - 1
        if lagged >= segmentStart then value += phi(lag) * out(lagged)
        lag += 1
      out(i) = value
      i += 1
    out.toVector

  private def glsModel: FmriModel =
    glsModelFor(
      x = Vector(0.0, 1.0, 2.0, 0.0, 1.0, 2.0),
      seed = Vector(0.5, -1.0, 0.25, 1.25, -0.75, 0.4),
      phi = Vector(rho)
    )

  private def glsModelFor(
      x: Vector[Double],
      seed: Vector[Double],
      phi: Vector[Double],
      resetAfterRows: Set[Int] = Set.empty
  ): FmriModel =
    glsModelForVoxels(
      x = x,
      seedsByVoxel = Vector(seed),
      phi = phi,
      betas = Vector(2.0 -> 3.0),
      blockLens = Vector(x.length),
      resetAfterRows = resetAfterRows
    )

  private def glsModelForVoxels(
      x: Vector[Double],
      seedsByVoxel: Vector[Vector[Double]],
      phi: Vector[Double],
      betas: Vector[(Double, Double)],
      blockLens: Vector[Int],
      resetAfterRows: Set[Int] = Set.empty
  ): FmriModel =
    require(blockLens.sum == x.length, "block lengths must match rows")
    require(seedsByVoxel.nonEmpty, "test fixture needs at least one voxel")
    require(seedsByVoxel.forall(_.length == x.length), "seed lengths must match rows")
    require(betas.length == seedsByVoxel.length, "beta count must match voxels")

    val designRows = x.map(v => Vector(v, 1.0))
    val residualsByVoxel =
      seedsByVoxel.map { seed =>
        val innovation = orthogonalInnovation(designRows, seed, phi, resetAfterRows)
        unwhitenInnovation(innovation, phi, resetAfterRows)
      }
    val y = x.indices.toVector.map { row =>
      betas.indices.toVector.map { voxel =>
        val (taskBeta, intercept) = betas(voxel)
        taskBeta * x(row) + intercept + residualsByVoxel(voxel)(row)
      }
    }
    modelFromRows(x, y, blockLens)

  private def modelFromRows(
      x: Vector[Double],
      y: Vector[Vector[Double]],
      blockLens: Vector[Int]
  ): FmriModel =
    require(blockLens.sum == x.length, "block lengths must match rows")
    require(y.length == x.length, "response rows must match design rows")
    val frame = samplingFrame(blockLens)
    val dataset =
      FmriDataset.unsafe(
        backend = InMemoryDatasetBackend(DatasetId("gls-demo"), ImageDMat.fromRows(y), NeuroSpace(Vector(y.head.length, 1, 1))),
        samplingFrame = frame
      )
    val eventModel =
      EventModel(
        terms = Vector.empty,
        samplingFrame = frame,
        designMatrix = Mat.fromRows(x.map(v => Vector(v))),
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

  private def ar1Residual(phi: Double, n: Int, offset: Int): Vector[Double] =
    val out = Array.ofDim[Double](n)
    var i = 0
    while i < n do
      val raw = math.sin((i + offset + 1).toDouble * 12.9898 + 78.233) * 43758.5453
      val innovation = (raw - math.floor(raw)) * 2.0 - 1.0
      out(i) = innovation + (if i == 0 then 0.0 else phi * out(i - 1))
      i += 1
    out.toVector

  test("GeneralizedLeastSquares runs AR(1) whitening and reports diagnostics") {
    val plan = FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(rho)))
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.engine, FitEngine.GeneralizedLeastSquares)
    assertEquals(result.autocorrelation.map(_.order), Some(1))
    assertEquals(result.autocorrelation.get.runs.map(_.method), Vector("fixed"))
    assertEquals(result.autocorrelation.get.iterations, 0)
    assertEquals(result.olsDiagnostics.map(_.solveMethod), Some(OlsSolveMethod.QrRankRevealing))
    assertEquals(result.olsDiagnostics.map(_.rank), Some(2))
    assertEqualsDouble(result.autocorrelation.get.runs.head.rho, rho, 1e-12)
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 3.0, 1e-10)
  }

  test("GeneralizedLeastSquares can estimate an AR(1) rho from initial residuals") {
    val plan = FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1)))
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val diag = result.autocorrelation.get.runs.head

    assertEquals(diag.method, "estimated")
    assertEquals(result.autocorrelation.get.iterations, 1)
    assert(diag.rho.isFinite)
    assert(math.abs(diag.rho) < 1.0)
    assertEquals(result.summary.autocorrelated, true)
  }

  test("estimated AR(2) GLS matches the fmriAR 0.3.3 end-to-end receipt") {
    val fixture = FmriArEstimatedGlsFixture
    val frame = samplingFrame(fixture.runLengths)
    val partitions = RunPartition.fromSamplingFrame(frame, (0 until fixture.rows).toVector)
    val fit = Gls
      .fit(
        DesignMatrix.unsafe(fixture.design),
        ResponseBlock.unsafe(fixture.response),
        partitions,
        ArOptions(
          structure = ArStructure.Ar(2),
          global = true,
          exactFirst = false,
          censoredTimepoints = fixture.censoredTimepoints
        )
      )
      .fold(error => fail(error.message), identity)

    assertEquals(fit.diagnostics.order, 2)
    assertEquals(fit.diagnostics.iterations, 1)
    assertEquals(fit.diagnostics.runs.map(_.method), Vector("estimated", "estimated"))
    fit.diagnostics.runs.foreach(run => assertVectorClose(run.phi, fixture.estimatedPhi, 1e-12))
    assertMatrixClose(fit.coefficients.value, fixture.coefficients, 1e-11)
    assertVectorClose(fit.residualVariance.toVector, fixture.residualVariance, 1e-11)
    assertMatrixClose(fit.normalizedCovariance, fixture.normalizedCovariance, 1e-11)
    assertEquals(fit.residualDegreesOfFreedom.value, fixture.residualDegreesOfFreedom)
  }

  test("GeneralizedLeastSquares iteratively re-estimates AR from GLS residuals") {
    val n = 240
    val x =
      (0 until n).toVector.map { i =>
        math.sin(i.toDouble * 0.13) + (if i >= n / 2 then 0.75 else -0.25)
      }
    val residual = ar1Residual(0.82, n, offset = 3000)
    val y =
      x.zip(residual).map { case (task, err) =>
        Vector(1.4 * task - 0.6 + err)
      }
    val model = modelFromRows(x, y, Vector(n))

    def fit(iterations: Int): DenseFmriFitResult =
      FitPlanExecutor
        .unsafeFit(
          FitPlan(
            model,
            engine = FitEngine.GeneralizedLeastSquares,
            config = FitConfig(
              autocorrelation = ArOptions(
                structure = ArStructure.Ar(1),
                iterations = iterations
              )
            )
          )
        )
        .asInstanceOf[DenseFmriFitResult]

    val one = fit(iterations = 1)
    val two = fit(iterations = 2)
    val oneRho = one.autocorrelation.get.runs.head.rho
    val twoRho = two.autocorrelation.get.runs.head.rho

    assertEquals(two.autocorrelation.get.iterations, 2)
    assertEquals(two.autocorrelation.get.runs.map(_.method), Vector("estimated"))
    assert(oneRho.isFinite && twoRho.isFinite, clues(oneRho, twoRho))
    assert(math.abs(twoRho) < 1.0, clues(twoRho))
    assert(math.abs(twoRho - oneRho) > 1e-8, clues(oneRho, twoRho))
    assertFinite(two.coefficients.value.copyData)
    assertFinite(two.standardErrors.value.copyData)
  }

  test("GeneralizedLeastSquares supports fixed AR(2) coefficients") {
    val phi = Vector(0.35, -0.2)
    val model = glsModelFor(
      x = Vector(0.0, 1.0, 2.0, 3.0, 0.0, 1.0, 2.0, 3.0),
      seed = Vector(0.5, -1.0, 0.25, 1.25, -0.75, 0.4, 0.9, -0.2),
      phi = phi
    )
    val plan = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(2), phi = Some(phi)))
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val diag = result.autocorrelation.get.runs.head

    assertEquals(result.autocorrelation.map(_.order), Some(2))
    assertVectorClose(diag.phi, phi, 1e-12)
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 3.0, 1e-10)
  }

  test("GeneralizedLeastSquares keeps finite diagnostics under extreme predictor scales") {
    val x = Vector(-3.0e6, -2.0e6, -1.0e6, 0.0, 1.0e6, 2.0e6, 3.0e6, 4.0e6)
    val model = glsModelForVoxels(
      x = x,
      seedsByVoxel = Vector(
        Vector(0.5, -1.0, 0.25, 1.25, -0.75, 0.4, 0.9, -0.2),
        Vector(-0.3, 0.6, -1.1, 0.2, 1.4, -0.8, 0.1, 0.7)
      ),
      phi = Vector(0.25),
      betas = Vector(2.0e-6 -> 3.0, -1.5e-6 -> -4.0),
      blockLens = Vector(x.length)
    )
    val plan = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(0.25)))
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEqualsDouble(result.coefficient("task", 0).get, 2.0e-6, 1e-14)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 3.0, 1e-8)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.5e-6, 1e-14)
    assertEqualsDouble(result.coefficient("base_constant", 1).get, -4.0, 1e-8)
    assertFinite(result.coefficients.value.copyData)
    assertFinite(result.normalizedCovariance.copyData)
    assertFinite(result.residualVariance.toVector)
    assertFinite(result.standardErrors.value.copyData)
  }

  test("GeneralizedLeastSquares resets AR whitening after censored timepoints") {
    val model = glsModelFor(
      x = Vector(0.0, 1.0, 2.0, 0.0, 1.0, 2.0),
      seed = Vector(0.5, -1.0, 0.25, 1.25, -0.75, 0.4),
      phi = Vector(rho),
      resetAfterRows = Set(2)
    )
    val plan = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(
        autocorrelation = ArOptions(
          structure = ArStructure.Ar(1),
          rho = Some(rho),
          censoredTimepoints = Vector(2)
        )
      )
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]

    assertEquals(result.autocorrelation.get.runs.head.rows, 6)
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 3.0, 1e-10)
  }

  test("estimated GLS excludes explicit censor rows only from noise estimation") {
    val rows = 6
    val partitions = RunPartition.fromSamplingFrame(samplingFrame(Vector(rows)), (0 until rows).toVector)
    val layout = Gls
      .noiseEstimationLayout(partitions, censoredTimepoints = Vector(2))
      .fold(error => fail(error.message), identity)

    assertEquals(layout.rows, rows)
    assertEquals(layout.retainedRows, rows - 1)
    assertEquals(layout.excludedRows, Vector(2))
    assertEquals(
      layout.whiteningSegments,
      Vector(TimeSegment(0, 3, 0), TimeSegment(3, 6, 0))
    )
    assertEquals(
      layout.estimationSegments,
      Vector(TimeSegment(0, 2, 0), TimeSegment(3, 6, 0))
    )
  }

  test("GeneralizedLeastSquares matches fmrireg fixed AR(1) whitening fixture with censor reset") {
    val rows = FmriregGlsFixtures.design.rows
    val x =
      (0 until rows).toVector.map(row => FmriregGlsFixtures.design(row, 0))
    val y =
      (0 until rows).toVector.map { row =>
        (0 until FmriregGlsFixtures.response.cols).toVector.map { col =>
          FmriregGlsFixtures.response(row, col)
        }
      }

    val segments =
      TimeSegments.withCensorResets(
        TimeSegments.continuous(rows),
        FmriregGlsFixtures.censoredTimepoints.toSet
      )
    val whiteningPlan =
      WhiteningPlan.global(
        ArmaCoefficients.ar(FmriregGlsFixtures.rho),
        segments,
        exactFirstAr1 = FmriregGlsFixtures.exactFirst
      )
    val whitened =
      WhiteningTransform(
        whiteningPlan,
        FmriregGlsFixtures.design,
        FmriregGlsFixtures.response
      ).toOption.get

    assertMatrixClose(whitened.design, FmriregGlsFixtures.whitenedDesign, 1e-12)
    assertMatrixClose(whitened.response, FmriregGlsFixtures.whitenedResponse, 1e-12)

    val model = modelFromRows(x, y, Vector(rows))
    val plan =
      FitPlan(
        model,
        engine = FitEngine.GeneralizedLeastSquares,
        config = FitConfig(
          autocorrelation = ArOptions(
            structure = ArStructure.Ar(1),
            rho = Some(FmriregGlsFixtures.rho),
            exactFirst = FmriregGlsFixtures.exactFirst,
            censoredTimepoints = FmriregGlsFixtures.censoredTimepoints
          )
        )
      )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val diagnostics = result.autocorrelation.get

    assertEquals(result.engine, FitEngine.GeneralizedLeastSquares)
    assertEquals(result.columnNames, Vector("task", "base_constant"))
    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(result.timepoints, (0 until rows).toVector)
    assertEquals(result.residualDegreesOfFreedom.value, FmriregGlsFixtures.residualDegreesOfFreedom)
    assertEquals(diagnostics.iterations, 0)
    assertEquals(diagnostics.order, 1)
    assertEquals(diagnostics.runs.map(_.method), Vector("fixed"))
    assertEquals(diagnostics.runs.map(_.rows), Vector(rows))
    assertEqualsDouble(diagnostics.runs.head.rho, FmriregGlsFixtures.rho, 1e-12)
    assertMatrixClose(result.coefficients.value, FmriregGlsFixtures.coefficients, 1e-12)
    assertVectorClose(result.residualVariance.toVector, FmriregGlsFixtures.residualVariance, 1e-12)
    assertMatrixClose(result.normalizedCovariance, FmriregGlsFixtures.normalizedCovariance, 1e-12)
    assertFinite(result.standardErrors.value.copyData)
  }

  test("GeneralizedLeastSquares rejects censored timepoints outside selected partitions") {
    val plan = FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(
        autocorrelation = ArOptions(
          structure = ArStructure.Ar(1),
          rho = Some(rho),
          censoredTimepoints = Vector(2)
        )
      )
    )

    val result = FitPlanExecutor.fit(
      plan,
      DataSelection(time = IndexSelection.indices(3, 4, 5))
    )

    assert(result.left.toOption.exists {
      case FitError.UnsupportedAutocorrelation(msg) => msg.contains("not selected") && msg.contains("2")
      case _                                       => false
    })
  }

  test("GeneralizedLeastSquares resets at run and censor boundaries for multivoxel data") {
    val model = glsModelForVoxels(
      x = Vector(0.0, 1.0, 2.0, 3.0, 0.0, 1.0, 2.0, 3.0),
      seedsByVoxel = Vector(
        Vector(0.5, -1.0, 0.25, 1.25, -0.75, 0.4, 0.9, -0.2),
        Vector(-0.3, 0.6, -1.1, 0.2, 1.4, -0.8, 0.1, 0.7)
      ),
      phi = Vector(rho),
      betas = Vector(2.0 -> 3.0, -1.5 -> 4.0),
      blockLens = Vector(4, 4),
      resetAfterRows = Set(3, 5)
    )
    val plan = FitPlan(
      model,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(
        autocorrelation = ArOptions(
          structure = ArStructure.Ar(1),
          rho = Some(rho),
          censoredTimepoints = Vector(5)
        )
      )
    )
    val result = FitPlanExecutor.unsafeFit(plan).asInstanceOf[DenseFmriFitResult]
    val diag = result.autocorrelation.get

    assertEquals(result.voxelIndices, Vector(0, 1))
    assertEquals(diag.runs.map(_.runIndex), Vector(0, 1))
    assertEquals(diag.runs.map(_.rows), Vector(4, 4))
    assert(diag.runs.forall(_.rho == rho), clues(diag.runs.map(_.rho)))
    assertEqualsDouble(result.coefficient("task", 0).get, 2.0, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 0).get, 3.0, 1e-10)
    assertEqualsDouble(result.coefficient("task", 1).get, -1.5, 1e-10)
    assertEqualsDouble(result.coefficient("base_constant", 1).get, 4.0, 1e-10)
  }

  test("GeneralizedLeastSquares distinguishes run-specific and global estimated AR pooling") {
    val blockLens = Vector(180, 180)
    val x = (0 until blockLens.sum).toVector.map(i => (i % 6).toDouble - 2.5)
    val residual = ar1Residual(0.85, blockLens.head, offset = 0) ++
      ar1Residual(-0.65, blockLens.last, offset = 1000)
    val y = x.zip(residual).map { case (task, err) =>
      Vector(1.25 * task + 2.0 + err)
    }
    val model = modelFromRows(x, y, blockLens)

    val byRun = FitPlanExecutor
      .unsafeFit(
        FitPlan(
          model,
          engine = FitEngine.GeneralizedLeastSquares,
          config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = false))
        )
      )
      .asInstanceOf[DenseFmriFitResult]
    val global = FitPlanExecutor
      .unsafeFit(
        FitPlan(
          model,
          engine = FitEngine.GeneralizedLeastSquares,
          config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = true))
        )
      )
      .asInstanceOf[DenseFmriFitResult]

    val byRunRhos = byRun.autocorrelation.get.runs.map(_.rho)
    val globalRhos = global.autocorrelation.get.runs.map(_.rho)

    assertEquals(byRun.autocorrelation.get.runs.map(_.method), Vector("estimated", "estimated"))
    assert(byRunRhos.head > 0.5, clues(byRunRhos))
    assert(byRunRhos.last < -0.4, clues(byRunRhos))
    assertEqualsDouble(globalRhos.head, globalRhos.last, 1e-12)
  }

  test("GeneralizedLeastSquares supports voxelwise estimated AR with explicit non-shared covariance") {
    val n = 220
    val x =
      (0 until n).toVector.map { i =>
        math.sin(i.toDouble * 0.09) + ((i % 7) - 3).toDouble / 3.0
      }
    val residuals =
      Vector(
        ar1Residual(0.86, n, offset = 2000),
        ar1Residual(-0.64, n, offset = 3000),
        ar1Residual(0.18, n, offset = 4000)
      )
    val betas =
      Vector(1.1 -> 0.5, -0.7 -> 1.25, 0.4 -> -1.0)
    val y =
      x.indices.toVector.map { row =>
        residuals.indices.toVector.map { voxel =>
          val (taskBeta, intercept) = betas(voxel)
          taskBeta * x(row) + intercept + residuals(voxel)(row)
        }
      }
    val model = modelFromRows(x, y, Vector(n))
    val pooled =
      FitPlanExecutor
        .unsafeFit(
          FitPlan(
            model,
            engine = FitEngine.GeneralizedLeastSquares,
            config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), global = false))
          )
        )
        .asInstanceOf[DenseFmriFitResult]
    val voxelwise =
      FitPlanExecutor
        .unsafeFit(
          FitPlan(
            model,
            engine = FitEngine.GeneralizedLeastSquares,
            config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true))
          )
        )
        .asInstanceOf[DenseFmriFitResult]
    val diagnostics = voxelwise.autocorrelation.get
    val voxelRhos = diagnostics.runs.head.voxelwiseCoefficients.map(_.head)

    assertEquals(diagnostics.sharedNormalizedCovariance, false)
    assertEquals(diagnostics.runs.map(_.method), Vector("voxelwise-estimated"))
    assertEquals(voxelRhos.length, 3)
    assert(voxelRhos.head > 0.45, clues(voxelRhos))
    assert(voxelRhos(1) < -0.25, clues(voxelRhos))
    assert(voxelRhos.max - voxelRhos.min > 0.65, clues(voxelRhos))
    assert(math.abs(voxelRhos.head - pooled.autocorrelation.get.runs.head.rho) > 0.1, clues(voxelRhos, pooled.autocorrelation.get.runs.head.rho))
    assertEqualsDouble(diagnostics.runs.head.rho, voxelRhos.sum / voxelRhos.length.toDouble, 1e-12)
    assertFinite(voxelwise.coefficients.value.copyData)
    assertFinite(voxelwise.standardErrors.value.copyData)
    assertEquals(voxelwise.coefficientCovariance.scope, CoefficientCovarianceScope.Voxelwise)
    assertEquals(voxelwise.coefficientCovariance.matrixCount, 3)
    val t = TContrast("task", Map("task" -> 1.0)).evaluate(voxelwise).toOption.get
    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(voxelwise).toOption.get
    assertFinite(t.standardErrors.toVector)
    assertFinite(t.statistics.toVector)
    assertFinite(f.statistics.toVector)
  }

  test("voxelwise GLS retains degenerate voxel status and isolates contrast inference") {
    val n = 96
    val x = (0 until n).toVector.map(i => math.sin(i.toDouble * 0.13))
    val signalResidual = ar1Residual(0.4, n, offset = 5000)
    val y = x.indices.toVector.map { row =>
      Vector(1.0, 0.8 * x(row) + 2.0 + signalResidual(row))
    }
    val model = modelFromRows(x, y, Vector(n))
    val result = FitPlanExecutor
      .unsafeFit(
        FitPlan(
          model,
          engine = FitEngine.GeneralizedLeastSquares,
          config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true))
        )
      )
      .asInstanceOf[DenseFmriFitResult]

    assertEquals(result.resolvedVoxelStatuses, Vector(VoxelFitStatus.Constant, VoxelFitStatus.Estimable))
    val t = TContrast("task", Map("task" -> 1.0)).evaluate(result).toOption.get
    val f = FContrast("task", Vector(Map("task" -> 1.0))).evaluate(result).toOption.get
    assertEquals(t.voxelIndices, Vector(1))
    assertEquals(f.voxelIndices, Vector(1))
    assertEquals(t.excludedVoxels, Vector(VoxelInferenceExclusion(0, VoxelFitStatus.Constant)))
    assertEquals(f.excludedVoxels, Vector(VoxelInferenceExclusion(0, VoxelFitStatus.Constant)))
    assertFinite(t.statistics.toVector)
    assertFinite(f.statistics.toVector)
  }

  test("GeneralizedLeastSquares rejects unsupported AR configurations explicitly") {
    val iid = FitPlan.makeLegacy(glsModel, engine = FitEngine.GeneralizedLeastSquares)
    assert(iid.left.toOption.exists {
      error => error.message.contains("AR(p)") && error.message.contains("not iid")
    })

    val fixedVoxelwise = FitPlan.makeLegacy(
        glsModel,
        engine = FitEngine.GeneralizedLeastSquares,
        config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true, rho = Some(rho)))
    )
    assert(fixedVoxelwise.left.toOption.exists {
      error => error.message.contains("voxelwise") && error.message.contains("estimated")
    })
  }

  test("GeneralizedLeastSquares rejects non-stationary fixed AR coefficients") {
    val plan = FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(
        autocorrelation = ArOptions(
          structure = ArStructure.Ar(2),
          phi = Some(Vector(1.5, 0.0)),
          exactFirst = false
        )
      )
    )

    val result = FitPlanExecutor.fit(plan)

    assert(result.left.toOption.exists {
      case FitError.UnsupportedAutocorrelation(message) => message.contains("not stationary")
      case _                                             => false
    })
  }

  test("GeneralizedLeastSquares treats selected-timepoint gaps as censor boundaries") {
    val plan = FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(rho)))
    )
    val result = FitPlanExecutor
      .fit(plan, DataSelection(time = IndexSelection.indices(0, 2, 3, 4, 5)))
      .fold(error => fail(error.message), identity)
      .asInstanceOf[DenseFmriFitResult]
    val whitening = result.autocorrelation.get.whitening

    assertEquals(result.timepoints, Vector(0, 2, 3, 4, 5))
    assertEquals(
      whitening.segments,
      Vector(
        ArWhiteningSegment(0, 0, 1, 0, 1),
        ArWhiteningSegment(0, 1, 5, 2, 6)
      )
    )
    assertEquals(whitening.censorGaps, Vector(ArCensorGap(0, 1, 2)))
  }

  test("row-deleted whitening matches independent segmented L X and L Y") {
    val fixture = ArCensorGlsRFixture
    val design = GaleTestMatrix.fromRows(fixture.selectedDesignRows)
    val response = GaleTestMatrix.fromRows(fixture.selectedResponseRows)
    val partitions =
      RunPartition.fromSamplingFrame(
        samplingFrame(fixture.blockLengths),
        fixture.selectedTimepoints
      )
    val segments =
      Gls.timeSegments(partitions, Vector.empty)
        .fold(error => fail(error.message), identity)
    val whitening =
      WhiteningPlan.global(
        ArmaCoefficients.ar(fixture.rho),
        segments,
        exactFirstAr1 = fixture.exactFirst
      )
    val transformed =
      WhiteningTransform(whitening, design, response)
        .fold(error => fail(error.message), identity)
    val direct =
      Ols.fit(
        DesignMatrix.unsafe(GaleTestMatrix.fromRows(fixture.whitenedDesignRows)),
        ResponseBlock.unsafe(GaleTestMatrix.fromRows(fixture.whitenedResponseRows))
      ).fold(error => fail(error.message), identity)

    assertMatrixClose(transformed.design, GaleTestMatrix.fromRows(fixture.whitenedDesignRows), 1e-12)
    assertMatrixClose(transformed.response, GaleTestMatrix.fromRows(fixture.whitenedResponseRows), 1e-12)
    assertMatrixClose(direct.coefficients.value, GaleTestMatrix.fromRows(fixture.coefficients), 1e-12)
    assertMatrixClose(direct.normalizedCovariance, GaleTestMatrix.fromRows(fixture.normalizedCovariance), 1e-12)
  }
