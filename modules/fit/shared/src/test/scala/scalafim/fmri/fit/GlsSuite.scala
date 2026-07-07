package scalafim.fmri.fit

import scalafim.dataset.{DataSelection, DatasetId, FmriDataset, IndexSelection, InMemoryDatasetBackend}
import scalafim.fmri.design.baseline.{BaselineBasis, BaselineModel, Intercept}
import scalafim.fmri.design.event.EventModel
import scalafim.fmri.hrf.design.SamplingFrame
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.{ArOptions, ArStructure, FitConfig, FitEngine, FitPlan, FmriModel}
import scalafim.image.{DMat, NeuroSpace}

class GlsSuite extends munit.FunSuite:

  private def assertVectorClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).zipWithIndex.foreach { case ((a, e), i) =>
      assert(math.abs(a - e) <= tol, clues(i, a, e))
    }

  private def assertFinite(values: Iterable[Double]): Unit =
    assert(values.forall(_.isFinite), clues(values.toVector))

  private val rho = 0.4

  private def samplingFrame(length: Int = 6): SamplingFrame =
    samplingFrame(Vector(length))

  private def samplingFrame(blockLens: Vector[Int]): SamplingFrame =
    SamplingFrame(blockLens = blockLens, tr = Vector.fill(blockLens.length)(1.0))

  private def whitenRows(rows: Vector[Vector[Double]], rho: Double): Vector[Vector[Double]] =
    whitenRows(rows, Vector(rho))

  private def orthogonalInnovation(designRows: Vector[Vector[Double]], seed: Vector[Double], rho: Double): Vector[Double] =
    orthogonalInnovation(designRows, seed, Vector(rho))

  private def whitenRows(
      rows: Vector[Vector[Double]],
      phi: Vector[Double],
      resetAfterRows: Set[Int] = Set.empty
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
      resetAfterRows: Set[Int] = Set.empty
  ): Vector[Double] =
    val whitened = whitenRows(designRows, phi, resetAfterRows)
    val design = DesignMatrix.unsafe(scalafim.linalg.DoubleMatrix.fromRows(whitened))
    val response = ResponseBlock.unsafe(scalafim.linalg.DoubleMatrix.fromRows(seed.map(v => Vector(v))))
    val fit = Ols.unsafeFit(design, response)

    seed.zipWithIndex.map { case (value, row) =>
      var fitted = 0.0
      var col = 0
      while col < whitened.head.length do
        fitted += whitened(row)(col) * fit.coefficients(col, 0)
        col += 1
      value - fitted
    }

  private def unwhitenInnovation(z: Vector[Double], rho: Double): Vector[Double] =
    unwhitenInnovation(z, Vector(rho))

  private def unwhitenInnovation(
      z: Vector[Double],
      phi: Vector[Double],
      resetAfterRows: Set[Int] = Set.empty
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
      FmriDataset(
        backend = InMemoryDatasetBackend(DatasetId("gls-demo"), DMat.fromRows(y), NeuroSpace(Vector(y.head.length, 1, 1))),
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
    assert(diag.rho.isFinite)
    assert(math.abs(diag.rho) < 1.0)
    assertEquals(result.summary.autocorrelated, true)
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

  test("GeneralizedLeastSquares rejects unsupported AR configurations explicitly") {
    val iid = FitPlan.makeLegacy(glsModel, engine = FitEngine.GeneralizedLeastSquares)
    assert(iid.left.toOption.exists {
      error => error.message.contains("AR(p)") && error.message.contains("not iid")
    })

    val voxelwise = FitPlanExecutor.fit(
      FitPlan(
        glsModel,
        engine = FitEngine.GeneralizedLeastSquares,
        config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), voxelwise = true))
      )
    )
    assert(voxelwise.left.toOption.exists {
      case FitError.UnsupportedAutocorrelation(msg) => msg.contains("voxelwise")
      case _                                       => false
    })
  }

  test("GeneralizedLeastSquares rejects non-contiguous selected timepoints") {
    val plan = FitPlan(
      glsModel,
      engine = FitEngine.GeneralizedLeastSquares,
      config = FitConfig(autocorrelation = ArOptions(structure = ArStructure.Ar(1), rho = Some(rho)))
    )
    val result = FitPlanExecutor.fit(
      plan,
      DataSelection(time = IndexSelection.indices(0, 2, 3, 4, 5))
    )

    assert(result.left.toOption.exists {
      case FitError.UnsupportedAutocorrelation(msg) => msg.contains("contiguous")
      case _                                       => false
    })
  }
