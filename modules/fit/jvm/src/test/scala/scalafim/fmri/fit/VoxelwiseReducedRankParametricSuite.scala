package scalafim.fmri.fit

import gale.linalg.Matrix
import scalafim.fmri.ar.{ArmaCoefficients, NoiseEstimationLayout, TimeSegment, WhiteningPlan, WhiteningTransform}
import scalafim.fmri.model.{ArOptions, ArStructure}

class VoxelwiseReducedRankParametricSuite extends munit.FunSuite:
  private def close(actual: Double, expected: Double, tolerance: Double = 1e-12): Unit =
    assertEqualsDouble(actual, expected, tolerance)

  test("stationary covariance satisfies the diagonal AR Lyapunov identity") {
    val rho = Vector(.1, .5, -.2)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val gamma = VoxelwiseReducedRankCalibration.stationaryCovariance(rho, sigma).toOption.get
    for r <- 0 until 3; c <- 0 until 3 do
      close(gamma(r, c) - rho(r) * gamma(r, c) * rho(c), sigma(r, c))
    val lower = gamma.cholesky.toOption.get.lower
    for r <- 0 until 3; c <- 0 until 3 do
      val reconstructed = (0 to math.min(r, c)).map(k => lower(r, k) * lower(c, k)).sum
      close(reconstructed, gamma(r, c))
  }

  test("physical AR generator is deterministic and continues through omitted observations") {
    val rho = Vector(.1, .5, -.2)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val selected = Vector(0, 1, 4, 5)
    val a = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector(selected), Vector(6), 91).toOption.get.head
    val b = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector(selected), Vector(6), 91).toOption.get.head
    assertEquals(a.map(_.toVector), b.map(_.toVector))
    val full = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector((0 until 6).toVector), Vector(6), 91).toOption.get.head
    selected.zipWithIndex.foreach { case (physical, observed) =>
      for v <- 0 until 3 do assertEqualsDouble(a(observed)(v), full(physical)(v), 0.0)
    }
  }

  test("physical runs receive independent stationary starts") {
    val rho = Vector(.1, .5, -.2)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val two = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector(Vector(0), Vector(0)), Vector(1, 1), 19).toOption.get
    assertNotEquals(two(0).head.toVector, two(1).head.toVector)
  }

  test("R oracle stationary factor and directional path convention") {
    val rho = Vector(.1, .5, -.2)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val path = VoxelwiseReducedRankCalibration.pathFromNormals(
      rho, sigma, Vector(.25, -1.0, .75),
      Vector(Vector(1.0, 0.0, -.5), Vector(-.25, .8, 1.2), Vector(.4, -.6, .1), Vector(-1.1, .3, .7), Vector(.2, 1.4, -.9))
    ).toOption.get
    val expected = Vector(
      Vector(.15075567228888181, -.53892243922052852, .3509987712865475),
      Vector(.61507556722888812, .030538780389735731, -.015148728535627334),
      Vector(-.088492443277111171, .35596158401139844, .65447134858059841),
      Vector(.23115075567228888, -.013788353356698702, -.065827523314588757),
      Vector(-.63688492443277112, -.18100960399715041, .078055592879628988),
      Vector(.056311507556722887, .69695653718035322, -.1540321592172551)
    )
    for r <- expected.indices; c <- 0 until 3 do close(path(r)(c), expected(r)(c), 2e-15)
    val gamma = VoxelwiseReducedRankCalibration.stationaryCovariance(rho, sigma).toOption.get
    close(rho(1) * gamma(1, 0), .094736842105263161)
    assertNotEquals(rho(1) * gamma(1, 0), rho(0) * gamma(1, 0))
  }

  test("stochastic generator preserves contemporaneous and directional lag covariance") {
    val rho = Vector(.1, .5, -.2)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val n = 20000
    val path = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector((0 until n).toVector), Vector(n), 7129).toOption.get.head
    def covariance(a: Int, b: Int, lag: Int): Double =
      val pairs = lag until n
      val ma = pairs.map(t => path(t)(a)).sum / pairs.length
      val mb = pairs.map(t => path(t-lag)(b)).sum / pairs.length
      pairs.map(t => (path(t)(a)-ma) * (path(t-lag)(b)-mb)).sum / pairs.length
    val gamma = VoxelwiseReducedRankCalibration.stationaryCovariance(rho, sigma).toOption.get
    for a <- 0 until 3; b <- 0 until 3 do
      assertEqualsDouble(covariance(a,b,0), gamma(a,b), .025)
      assertEqualsDouble(covariance(a,b,1), rho(a) * gamma(a,b), .025)
    assert(math.abs(covariance(1,0,1) - covariance(0,1,1)) > .03)
  }

  test("joint-lag stress law differs from contemporaneous AR noise and has directional lag") {
    val rho = Vector(.1, .5, -.2)
    val lagged = VoxelwiseReducedRankCalibration.jointLagNoise(rho, .5, Vector(12000), 173).head
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val baseline = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector((0 until 12000).toVector), Vector(12000), 173).toOption.get.head
    assertNotEquals(lagged.take(6).map(_.toVector), baseline.take(6).map(_.toVector))
    def lagCov(a: Int, b: Int): Double =
      val points = 1 until lagged.length
      val ma = points.map(t => lagged(t)(a)).sum / points.length
      val mb = points.map(t => lagged(t - 1)(b)).sum / points.length
      points.map(t => (lagged(t)(a)-ma) * (lagged(t-1)(b)-mb)).sum / points.length
    assert(math.abs(lagCov(1, 0) - lagCov(0, 1)) > .04)
  }

  test("estimated GLS rho is invariant to a design mean shift with fixed AR noise") {
    val rho = Vector(.1, .5, -.2)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val n = 80
    val noise = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector((0 until n).toVector), Vector(n), 808).toOption.get.head
    val x = Matrix.tabulate(n, 2)((r, c) => if c == 0 then 1.0 else r.toDouble / (n - 1))
    val first = Matrix.tabulate(2, 3)((r, v) => (r + 1) * (v - 1.0))
    val second = Matrix.tabulate(2, 3)((r, v) => first(r,v) + .7 * (r + 1) * (v + 1))
    val y1 = Matrix.tabulate(n, 3)((r,v) => (0 until 2).map(c => x(r,c) * first(c,v)).sum + noise(r)(v))
    val y2 = Matrix.tabulate(n, 3)((r,v) => (0 until 2).map(c => x(r,c) * second(c,v)).sum + noise(r)(v))
    val runs = Vector(RunPartition(0, (0 until n).toVector, (0 until n).toVector))
    val options = ArOptions(structure = ArStructure.Ar(1), voxelwise = true)
    def rhos(y: gale.linalg.DMat): Vector[Double] =
      val prepared = Gls.prepare(DesignMatrix.unsafe(x), ResponseBlock.unsafe(y), runs, options, Vector(0,1,2)).toOption.get
      prepared.whitening match
        case GlsWhitening.Voxelwise(plans) => plans.map(_.coefficients.head.phi.head)
        case GlsWhitening.Shared(plan) => Vector.fill(3)(plan.coefficients.head.phi.head)
    rhos(y1).zip(rhos(y2)).foreach((a,b) => assertEqualsDouble(a, b, 1e-12))
  }

  test("common rho gives the exact stationary first-row covariance") {
    val rho = Vector.fill(3)(.4)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val gamma = VoxelwiseReducedRankCalibration.stationaryCovariance(rho, sigma).toOption.get
    for r <- 0 until 3; c <- 0 until 3 do
      assertEqualsDouble(gamma(r,c), sigma(r,c) / (1.0 - .16), 1e-14)
  }

  test("exact-first whitening gives innovation covariance at first and later rows") {
    val rho = Vector.fill(3)(.4)
    val sigma = Matrix.tabulate(3, 3)((r, c) => .36 * (if r == c then 1.0 else .5))
    val runs = 3000
    val raw = VoxelwiseReducedRankCalibration.physicalAr1(rho, sigma, Vector.fill(runs)(Vector(0,1)), Vector.fill(runs)(2), 913).toOption.get
    val y = Matrix.tabulate(runs * 2, 3)((r,v) => raw(r / 2)(r % 2)(v))
    val x = Matrix.zeros(runs * 2, 1)
    val segments = Vector.tabulate(runs)(r => TimeSegment(2*r, 2*r+2, r))
    val plan = WhiteningPlan.global(ArmaCoefficients.ar(.4), segments, exactFirstAr1 = true)
    val w = WhiteningTransform(plan, x, y).toOption.get.response
    def cov(rowParity: Int, a: Int, b: Int): Double =
      val values = (0 until runs).map(r => (w(2*r+rowParity,a), w(2*r+rowParity,b)))
      val ma = values.map(_._1).sum / runs; val mb = values.map(_._2).sum / runs
      values.map((a,b) => (a-ma)*(b-mb)).sum / runs
    for row <- 0 to 1; a <- 0 until 3; b <- 0 until 3 do assertEqualsDouble(cov(row,a,b), sigma(a,b), .035)
  }

  test("type-7 percentile matches the R oracle definition") {
    val values = Vector(-2.0, .5, 1.0, 3.0)
    assertEqualsDouble(VoxelwiseReducedRankCalibration.percentile(values, .25), -.125, 1e-15)
    assertEqualsDouble(VoxelwiseReducedRankCalibration.percentile(values, .975), 2.85, 1e-15)
  }

  test("run-specific physical AR laws preserve deliberately different variance and lag") {
    val rho = Vector.fill(3)(.1)
    val highRho = Vector.fill(3)(.7)
    val lowSigma = Matrix.tabulate(3, 3)((r,c) => if r == c then .36 else 0.0)
    val highSigma = Matrix.tabulate(3, 3)((r,c) => if r == c then 1.44 else 0.0)
    val n = 50000
    val paths = VoxelwiseReducedRankCalibration.physicalAr1ByRun(
      Vector(rho, highRho), Vector(lowSigma, highSigma), Vector((0 until n).toVector, (0 until n).toVector), Vector(n,n), 2201
    ).toOption.get
    def variance(path: Vector[Array[Double]]): Double =
      val mean = path.map(_(0)).sum / path.length
      path.map(x => math.pow(x(0)-mean,2)).sum / path.length
    def lag(path: Vector[Array[Double]]): Double =
      val mean = path.map(_(0)).sum / path.length
      (1 until path.length).map(t => (path(t)(0)-mean)*(path(t-1)(0)-mean)).sum / (path.length-1)
    val lowVariance = variance(paths(0)); val highVariance = variance(paths(1))
    assertEqualsDouble(lowVariance, .36 / (1-.01), .025)
    assertEqualsDouble(highVariance, 1.44 / (1-.49), .10)
    assertEqualsDouble(lag(paths(0)) / lowVariance, .1, .025)
    assertEqualsDouble(lag(paths(1)) / highVariance, .7, .035)
  }

  test("HC2 covariance centres once per physical run and uses K minus one") {
    val residuals = Matrix.tabulate(6, 1)((r, _) => Vector(0.0, 1.0, 2.0, 0.0, 11.0, 12.0)(r))
    val leverage = Matrix.tabulate(6, 1)((_, _) => 1.0)
    val layout = NoiseEstimationLayout.allRows(Vector(TimeSegment(0,3,0), TimeSegment(3,6,0)), 6).toOption.get
    val covariance = VoxelwiseReducedRankCalibration.covarianceForRun(residuals, leverage, layout, 0).toOption.get
    assertEqualsDouble(covariance(0,0), (30.25 + 20.25 + 20.25 + 30.25) / 3.0, 1e-14)
  }

  test("arm policies exactly encode the prespecified whitening and generator matrix") {
    val expected = Vector(
      ("residual_frozen_full", false, false, false, false),
      ("residual_refit_full", false, false, false, true),
      ("oracle_known_rank1", true, true, true, false),
      ("oracle_known_full", true, true, true, false),
      ("oracle_refit_rank1", false, true, true, true),
      ("oracle_refit_full", false, true, true, true),
      ("plugin_refit_rank1", false, false, false, true),
      ("plugin_refit_full", false, false, false, true),
      ("true_rho_plugin_sigma_rank1", false, true, false, true),
      ("plugin_rho_true_sigma_rank1", false, false, true, true)
    )
    assertEquals(VoxelwiseReducedRankCalibration.armPolicies, expected)
  }

  test("known full-rank GLS marginal covariance equals the fixed whitened normal expression") {
    val n = 10
    val x = Matrix.tabulate(n, 2)((r, c) => if c == 0 then 1.0 else r.toDouble)
    val y = Matrix.zeros(n, 1)
    val plan = WhiteningPlan.global(ArmaCoefficients.ar(.3), Vector(TimeSegment(0, n, 0)))
    val wx = WhiteningTransform(plan, x, y).toOption.get.design
    val normal = wx.t * wx
    val determinant = normal(0,0) * normal(1,1) - normal(0,1) * normal(1,0)
    val analytic = Matrix.tabulate(2, 2) { (r,c) =>
      (r,c) match
        case (0,0) => normal(1,1) / determinant
        case (1,1) => normal(0,0) / determinant
        case _ => -normal(r,c) / determinant
    }
    val qr = wx.qr
    val gale = qr.normalizedCovariance.toOption.get
    for r <- 0 until 2; c <- 0 until 2 do assertEqualsDouble(gale(r,c), analytic(r,c), 1e-12)
  }

  test("invalid AR coefficient and non-positive-definite innovation covariance are refused") {
    val sigma = Matrix.tabulate(3, 3)((r, c) => if r == c then 1.0 else 2.0)
    assert(VoxelwiseReducedRankCalibration.stationaryCovariance(Vector(1.0, .2, .3), sigma).isLeft)
    assert(VoxelwiseReducedRankCalibration.physicalAr1(Vector(.1, .2, .3), sigma, Vector(Vector(0)), Vector(1), 1).isLeft)
  }
