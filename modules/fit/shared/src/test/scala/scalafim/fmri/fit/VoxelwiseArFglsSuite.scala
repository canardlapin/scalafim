package scalafim.fmri.fit

import gale.linalg.{CholeskyOptions, DMat, Matrix}
import scalafim.fmri.ar.{StationaryArFactor, TimeSegment, TimeSegments}
import scalafim.fmri.fit.fixtures.{KrCase, VoxelwiseKrRFixture}

class VoxelwiseArFglsSuite extends munit.FunSuite:

  private val runs = Vector(TimeSegment(0, 20, 0), TimeSegment(20, 50, 1))
  private val censored = Vector(5, 33)
  private val rows = 50
  private val design: DMat = Matrix.tabulate(rows, 4) { (row, col) =>
    val t = row.toDouble
    col match
      case 0 => math.cos(0.3 * t)
      case 1 => math.sin(0.11 * t) * (t / rows)
      case 2 => if row < 20 then 1.0 else 0.0
      case _ => if row >= 20 then 1.0 else 0.0
  }
  private val response: DMat = Matrix.tabulate(rows, 1) { (row, _) =>
    val t = row.toDouble
    math.sin(0.7 * t) + 0.3 * math.cos(1.3 * math.pow(t, 1.1)) + 0.2 * design(row, 0)
  }
  private val tContrast = Matrix.tabulate(1, 4)((_, col) => if col == 0 then 1.0 else 0.0)
  private val fContrast = Matrix.tabulate(2, 4)((row, col) => if row == col then 1.0 else 0.0)

  private def stationaryCovariance(phi: Vector[Double], segmentsByRun: Vector[TimeSegment]): DMat =
    val gamma = StationaryArFactor.autocovariance(phi, rows).toOption.get
    Matrix.tabulate(rows, rows) { (i, j) =>
      if segmentsByRun.exists(s => s.contains(i) && s.contains(j)) then gamma(math.abs(i - j)) else 0.0
    }

  test("stationary AR autocovariance matches the AR(1) closed form"):
    val gamma = StationaryArFactor.autocovariance(Vector(0.6), 5).toOption.get
    gamma.zipWithIndex.foreach { (value, lag) =>
      assertEqualsDouble(value, math.pow(0.6, lag) / (1.0 - 0.36), 1e-12)
    }

  test("exact stationary whitening is an exact inverse square root for AR(2) through censor gaps"):
    val phi = Vector(0.45, -0.10)
    val model = ArWhiteningModel.build(runs, Vector(phi, phi), 2, CensorContinuity.ContinuousMissing).toOption.get
    val w = model.apply(Matrix.eye(rows))
    val product = w * stationaryCovariance(phi, runs) * w.t
    for i <- 0 until rows; j <- 0 until rows do
      assertEqualsDouble(product(i, j), if i == j then 1.0 else 0.0, 1e-10)

  test("restart whitening is not an inverse square root for AR(2) noise continuous through censored rows"):
    val phi = Vector(0.45, -0.10)
    val segments = TimeSegments.withCensorResets(runs, censored.toSet)
    val model = ArWhiteningModel.build(segments, Vector(phi, phi), 2, CensorContinuity.RestartAfterCensor).toOption.get
    val w = model.apply(Matrix.eye(rows))
    val product = w * stationaryCovariance(phi, runs) * w.t
    // First row of a run is left unscaled (variance gamma_0 > 1); a reset row stays correlated with its predecessor.
    assert(product(0, 0) > 1.2, product(0, 0))
    assert(math.abs(product(6, 5)) > 0.1, product(6, 5))

  test("censor indicators reproduce exact GLS on the retained rows under their marginal covariance"):
    val phi = Vector(0.45, -0.10)
    val spec = VoxelwiseArSpec(
      2,
      VoxelArCoefficients.Known(Vector(phi, phi)),
      CensorContinuity.ContinuousMissing,
      CovarianceUncertainty.Conditional
    )
    val fit = VoxelwiseArFgls.fit(design, response, runs, censored, spec).toOption.get.voxels.head
    val keep = (0 until rows).filterNot(censored.contains).toVector
    val sigma = stationaryCovariance(phi, runs)
    val marginal = Matrix.tabulate(keep.length, keep.length)((i, j) => sigma(keep(i), keep(j)))
    val precision = marginal.cholesky(CholeskyOptions()).flatMap(_.solve(Matrix.eye(keep.length))).toOption.get
    val x = Matrix.tabulate(keep.length, 4)((i, j) => design(keep(i), j))
    val y = Matrix.tabulate(keep.length, 1)((i, _) => response(keep(i), 0))
    val information = x.t * precision * x
    val beta = information.cholesky(CholeskyOptions()).flatMap(_.solve(x.t * precision * y)).toOption.get
    (0 until 4).foreach(i => assertEqualsDouble(fit.coefficients(i), beta(i, 0), 1e-10))
    assertEquals(fit.residualDf, rows - 4 - censored.length)

  test("Kenward-Roger with known coefficients reduces to the exact F test"):
    val phi = Vector(0.45, -0.10)
    def fitWith(uncertainty: CovarianceUncertainty) =
      VoxelwiseArFgls
        .fit(
          design,
          response,
          runs,
          censored,
          VoxelwiseArSpec(2, VoxelArCoefficients.Known(Vector(phi, phi)), CensorContinuity.ContinuousMissing, uncertainty)
        )
        .toOption
        .get
        .voxels
        .head
    val conditional = fitWith(CovarianceUncertainty.Conditional)
    val kr = fitWith(CovarianceUncertainty.KenwardRoger)
    for i <- 0 until 4; j <- 0 until 4 do
      assertEqualsDouble(kr.adjustedCovariance(i, j), conditional.covariance(i, j), 1e-9)
    Vector(tContrast, fContrast).foreach { contrast =>
      val exact = conditional.test(contrast, Vector.fill(contrast.rows)(0.0)).toOption.get
      val adjusted = kr.test(contrast, Vector.fill(contrast.rows)(0.0)).toOption.get
      assertEqualsDouble(adjusted.denominatorDf, (rows - 4 - censored.length).toDouble, 1e-6)
      assertEqualsDouble(adjusted.scale, 1.0, 1e-9)
      assertEqualsDouble(adjusted.statistic, exact.statistic, 1e-9 * exact.statistic)
    }

  private def checkFixture(name: String, expected: KrCase): Unit =
    test(s"Kenward-Roger matches the generic R implementation: $name"):
      val phi =
        if expected.pooled then Vector(Vector(0.42, -0.12), Vector(0.42, -0.12))
        else Vector(Vector(0.42, -0.12), Vector(0.30, 0.05))
      val censor =
        if expected.restart then CensorContinuity.RestartAfterCensor else CensorContinuity.ContinuousMissing
      val scope = if expected.pooled then VoxelArScope.AcrossRuns else VoxelArScope.PerRun
      val spec =
        VoxelwiseArSpec(2, VoxelArCoefficients.Estimated(scope, 25), censor, CovarianceUncertainty.KenwardRoger)
      val augmented = if expected.restart then design else VoxelwiseArFgls.withIndicators(design, censored)
      val segments = if expected.restart then TimeSegments.withCensorResets(runs, censored.toSet) else runs
      val fit = VoxelwiseArFgls.fitVoxel(augmented, 4, response, segments, phi, Vector.empty, spec).toOption.get
      def close(actual: Double, target: Double, label: String): Unit =
        assert(math.abs(actual - target) <= 2e-6 * math.max(1e-3, math.abs(target)), s"$label: $actual vs $target")
      close(fit.residualVariance, expected.sigma2, "sigma2")
      (0 until 4).foreach(i => close(fit.coefficients(i), expected.beta(i), s"beta $i"))
      for i <- 0 until 4; j <- 0 until 4 do
        close(fit.covariance(i, j), expected.conditional(i * 4 + j), s"conditional $i,$j")
        close(fit.adjustedCovariance(i, j), expected.adjusted(i * 4 + j), s"adjusted $i,$j")
      val t = fit.test(tContrast, Vector(0.0)).toOption.get
      val f = fit.test(fContrast, Vector(0.0, 0.0)).toOption.get
      close(t.denominatorDf, expected.tDf, "t df")
      close(t.scale, expected.tScale, "t scale")
      close(f.denominatorDf, expected.fDf, "F df")
      close(f.scale, expected.fScale, "F scale")

  checkFixture("continuous per-run", VoxelwiseKrRFixture.ContinuousPerRun)
  checkFixture("continuous pooled", VoxelwiseKrRFixture.ContinuousPooled)
  checkFixture("restart per-run", VoxelwiseKrRFixture.RestartPerRun)
  checkFixture("restart pooled", VoxelwiseKrRFixture.RestartPooled)

  test("estimated voxelwise fits retain per-run coefficients and refuse invalid censor rows"):
    val spec = VoxelwiseArSpec(
      1,
      VoxelArCoefficients.Estimated(VoxelArScope.PerRun, 25),
      CensorContinuity.ContinuousMissing,
      CovarianceUncertainty.KenwardRoger
    )
    val fit = VoxelwiseArFgls.fit(design, response, runs, censored, spec).toOption.get
    assertEquals(fit.voxels.head.phiByRun.length, 2)
    assert(fit.voxels.head.test(tContrast, Vector(0.0)).isRight)
    assert(VoxelwiseArFgls.fit(design, response, runs, Vector(rows), spec).isLeft)
