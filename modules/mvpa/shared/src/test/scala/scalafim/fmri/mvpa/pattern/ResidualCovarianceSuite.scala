package scalafim.fmri.mvpa.pattern

import gale.linalg.DMat
import multivar.core.SpaceRole
import scalafim.fmri.mvpa.AxisRef

class ResidualCovarianceSuite extends munit.FunSuite:
  private def right[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)

  private def axis(size: Int, prefix: String = "v") =
    right(AxisRef.fromStableKeys("neural", SpaceRole.Observed, Vector.tabulate(size)(i => s"$prefix$i"), "voxel", "psc", "raw"))

  private def roiAxis(keys: Vector[String]) =
    right(AxisRef.fromStableKeys("roi", SpaceRole.Observed, keys, "voxel", "psc", "raw"))

  /** A training binding declaring exactly `n` sample rows. */
  private def binding(n: Int) =
    val samples = right(AxisRef.fromStableKeys("samples", SpaceRole.Samples, Vector.tabulate(n)(i => s"t$i"), "trial", "none", "one"))
    right(TrainingBinding(samples.descriptor, "outer-train fold 0", "sha256:fixture"))

  /** Dense `D + U U^T`, built directly as an independent oracle. */
  private def dense(d: Vector[Double], u: Vector[Vector[Double]]): DMat =
    DMat.tabulate(d.length, d.length): (i, j) =>
      (if i == j then d(i) else 0.0) + u(i).indices.map(k => u(i)(k) * u(j)(k)).sum

  private def loadings(u: Vector[Vector[Double]]): DMat =
    DMat.tabulate(u.length, u.head.length)((i, k) => u(i)(k))

  private def assertClose(actual: DMat, expected: DMat, relative: Double)(using munit.Location): Unit =
    assertEquals((actual.rows, actual.cols), (expected.rows, expected.cols))
    var scale = 0.0
    for i <- 0 until expected.rows; j <- 0 until expected.cols do scale = math.max(scale, math.abs(expected(i, j)))
    for i <- 0 until expected.rows; j <- 0 until expected.cols do
      assertEqualsDouble(actual(i, j), expected(i, j), relative * math.max(scale, 1.0), s"entry ($i, $j)")

  test("a two-feature, rank-one covariance matches the hand-derived precision, diagonal and log determinant") {
    // Psi = diag(1, 2) + (1, 1)(1, 1)^T = [[2, 1], [1, 3]]; det 5;
    // Psi^-1 = [[3, -1], [-1, 2]] / 5.
    val psi = right(ResidualCovariance.fromFactors(axis(2), Vector(1.0, 2.0), DMat.dense(2, 1, Vector(1.0, 1.0))))
    assertEqualsDouble(psi.logDeterminant, math.log(5.0), 1e-14)
    val precision = right(psi.applyPrecision(DMat.eye(2)))
    assertClose(precision, DMat.dense(2, 2, Vector(0.6, -0.2, -0.2, 0.4)), 1e-14)
    right(psi.precisionDiagonal).zip(Vector(0.6, 0.4)).foreach((actual, expected) => assertEqualsDouble(actual, expected, 1e-14))
    assertEqualsDouble(right(psi.precisionQuadratic(Vector(1.0, 1.0))), 0.6, 1e-14)
    assertClose(right(psi.applyCovariance(DMat.eye(2))), DMat.dense(2, 2, Vector(2.0, 1.0, 1.0, 3.0)), 1e-15)
    // 2 p + p h + 2 (p + h) h + h = 4 + 2 + 6 + 1 (Gale stores a (p + h) x h R).
    assertEquals(psi.storedCells, 13L)
    assertEqualsDouble(psi.capacitanceConditionEstimate, 1.0, 0.0)
    assertEquals(psi.coordinateGauge, CoordinateGauge.UnfixedBasis)
  }

  // Heterogeneous diagonal over two decades and two nearly collinear loadings.
  private val d5 = Vector(0.1, 0.5, 1.0, 3.0, 10.0)
  private val u5 = Vector(
    Vector(1.0, 1.0 + 1e-6), Vector(2.0, 2.0 - 1e-6), Vector(3.0, 3.0 + 1e-6), Vector(4.0, 4.0 - 1e-6), Vector(5.0, 5.0 + 1e-6)
  )

  test("factorized precision, covariance and log determinant match dense oracles for heterogeneous D and ill-conditioned U") {
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    val full = dense(d5, u5)
    val operand = DMat.tabulate(5, 3)((i, j) => math.sin(1.0 + i + 2.0 * j))
    // Independent routes: a dense LU solve and a dense Cholesky of the 5 x 5 Psi.
    assertClose(right(psi.applyPrecision(operand)), right(full.solve(operand)), 1e-9)
    assertClose(right(psi.applyCovariance(operand)), full * operand, 1e-12)
    val denseCholesky = right(full.cholesky)
    val denseLogDet = (0 until 5).map(i => 2.0 * math.log(denseCholesky.lower(i, i))).sum
    assertEqualsDouble(psi.logDeterminant, denseLogDet, 1e-9 * math.abs(denseLogDet))
    val denseInverse = right(full.solve(DMat.eye(5)))
    right(psi.precisionDiagonal).zipWithIndex.foreach((actual, i) => assertEqualsDouble(actual, denseInverse(i, i), 1e-9 * denseInverse(i, i)))
  }

  test("hard ROI restriction keeps D_R and U_R and differs from cropping the whole-axis precision") {
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    val roi = roiAxis(Vector("v3", "v0", "v4"))
    val restricted = right(psi.restrict(roi))
    val keep = Vector(3, 0, 4)
    assertEquals(restricted.diagonalValues, keep.map(d5))
    assertEquals(restricted.rank, 2)
    val marginal = dense(keep.map(d5), keep.map(u5))
    assertClose(right(restricted.applyPrecision(DMat.eye(3))), right(marginal.solve(DMat.eye(3))), 1e-9)
    val wholeInverse = right(dense(d5, u5).solve(DMat.eye(5)))
    val cropped = DMat.tabulate(3, 3)((i, j) => wholeInverse(keep(i), keep(j)))
    val restrictedPrecision = right(restricted.applyPrecision(DMat.eye(3)))
    val gap = (for i <- 0 until 3; j <- 0 until 3 yield math.abs(restrictedPrecision(i, j) - cropped(i, j))).max
    assert(gap > 1e-2, s"restriction must not equal a cropped precision; gap $gap")
  }

  test("ROI and general measurements outside the supported structure are refused with typed errors") {
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    assertEquals(psi.restrict(roiAxis(Vector("v1", "w9"))).left.toOption, Some(ResidualCovarianceError.UnknownRoiKey("w9")))
    psi.measured(DMat.dense(1, 5, Vector(0.2, 0.2, 0.2, 0.2, 0.2))) match
      case Left(ResidualCovarianceError.UnsupportedMeasurement(_)) => ()
      case other => fail(s"expected UnsupportedMeasurement, got $other")
    assertEquals(ResidualCovariance.fromFactors(axis(2), Vector(1.0, 0.0), DMat.dense(2, 1, Vector(1.0, 1.0))).left.toOption,
      Some(ResidualCovarianceError.NonPositiveDiagonal(1)))
    assertEquals(ResidualCovariance.fromFactors(axis(2), Vector(1.0, 1.0), DMat.dense(3, 1, Vector(1.0, 1.0, 1.0))).left.toOption,
      Some(ResidualCovarianceError.Shape("loadings", 2, 1, 3, 1)))
    assertEquals(psi.applyPrecision(DMat.eye(4)).left.toOption, Some(ResidualCovarianceError.Shape("precision operand", 5, 4, 4, 4)))
  }

  /** Deterministic standard normals (64-bit LCG + Box-Muller), identical on JVM and JS up to libm rounding. */
  private final class Normals(seed: Long):
    private var state = seed
    private def uniform(): Double =
      state = state * 6364136223846793005L + 1442695040888963407L
      ((state >>> 11).toDouble + 0.5) / 9007199254740992.0
    def next(): Double =
      math.sqrt(-2.0 * math.log(uniform())) * math.cos(2.0 * math.Pi * uniform())

  /** `n` draws of `x = U z + sqrt(d) e`, rows are samples, centred per
    * feature as task-model residuals within a training scope would be.
    */
  private def draws(n: Int, d: Vector[Double], u: Vector[Vector[Double]], seed: Long): DMat =
    val normals = new Normals(seed)
    val h = u.head.length
    val rows = Vector.fill(n):
      val z = Vector.fill(h)(normals.next())
      d.indices.toVector.map(j => u(j).indices.map(k => u(j)(k) * z(k)).sum + math.sqrt(d(j)) * normals.next())
    val means = d.indices.map(j => rows.map(_(j)).sum / n)
    DMat.tabulate(n, d.length)((i, j) => rows(i)(j) - means(j))

  private val trueD = Vector(0.5, 1.0, 2.0, 0.8, 1.5, 3.0)
  private val trueU = Vector(Vector(1.2), Vector(-0.8), Vector(2.0), Vector(0.5), Vector(1.5), Vector(-1.0))

  private def secondMoments(residuals: DMat): Vector[Double] =
    Vector.tabulate(residuals.cols)(j => (0 until residuals.rows).map(i => residuals(i, j) * residuals(i, j)).sum / residuals.rows)

  test("factor fit is monotone, satisfies the ML diagonal identity, recovers the generating model and reports its work") {
    val residuals = draws(4000, trueD, trueU, 20261001L)
    val policy = right(ResidualCovarianceFitPolicy(1, maximumIterations = 5000, tolerance = 1e-13, sensitivityRanks = Vector(2)))
    val fitted = right(ResidualCovariance.fit(axis(6), residuals, "task model and run means removed within outer-train fold 0", binding(residuals.rows), policy))
    val receipt = fitted.receipt
    assert(receipt.converged)
    receipt.logLikelihoodTrace.sliding(2).foreach: pair =>
      assert(pair(1) >= pair(0) - 1e-9 * math.abs(pair(0)), s"likelihood decreased: $pair")
    assertEquals(receipt.flooredFeatures, Vector.empty[Int])
    // Necessary ML condition for factor analysis: diag(Psi) = diag(S) at an interior optimum.
    val s = secondMoments(residuals)
    val u = fitted.covariance.loadingsMatrix
    fitted.covariance.diagonalValues.zipWithIndex.foreach: (d, j) =>
      assertEqualsDouble(d + u(j, 0) * u(j, 0), s(j), 1e-4 * s(j))
    // Sampling recovery of the generating Psi (n = 4000; ~2-3% standard error).
    val truth = dense(trueD, trueU)
    val estimate = dense(fitted.covariance.diagonalValues, Vector.tabulate(6)(j => Vector(u(j, 0))))
    for i <- 0 until 6; j <- 0 until 6 do
      assertEqualsDouble(estimate(i, j), truth(i, j), 0.12 * math.sqrt(truth(i, i) * truth(j, j)), s"Psi($i, $j)")
    assertEquals(receipt.rank, 1)
    assertEquals(receipt.samples, 4000)
    assertEquals(receipt.residualReceipt, "task model and run means removed within outer-train fold 0")
    assertEquals(receipt.training.source, "outer-train fold 0")
    assertEquals(receipt.initialization, "gale-partial-svd(k=2)")
    assertEquals(receipt.regularization, "relative unique-variance floor only; no loading or covariance shrinkage")
    assertEquals(receipt.coordinateGauge, CoordinateGauge.UnfixedBasis)
    assert(receipt.centeringResidual <= 1e-8, receipt.centeringResidual.toString)
    assert(receipt.diagonalMomentResidual <= 1e-6, receipt.diagonalMomentResidual.toString)
    assertEquals(receipt.parameterCount, 12L)
    assertEqualsDouble(receipt.logLikelihood, receipt.logLikelihoodTrace.last, 1e-9 * math.abs(receipt.logLikelihood))
    assertEqualsDouble(receipt.bic, -2.0 * receipt.logLikelihood + 12.0 * math.log(4000.0), 1e-9 * math.abs(receipt.bic))
    assert(receipt.minimumUniqueRatio > 0.0 && receipt.minimumUniqueRatio < 1.0)
    assert(receipt.capacitanceConditionEstimate >= 1.0)
    assertEquals(receipt.work.storedCells, 12L + 6L + 14L + 1L)
    // The stable likelihood pass alone allocates 2 (p + h) n cells per rank (ranks 1 and 2), beyond the n p residual copy.
    assert(receipt.work.cumulativeCells >= 4000L * 6 + 2L * 7 * 4000 + 2L * 8 * 4000, receipt.work.toString)
    assert(receipt.work.peakCellsUpperBound >= 4000L * 6L)
    assert(receipt.work.cumulativeCells >= receipt.work.peakCellsUpperBound - receipt.work.storedCells)
    assertEquals((receipt.work.largestDenseRows, receipt.work.largestDenseColumns), (4000, 6))
    assert(receipt.explainedFraction > 0.0 && receipt.explainedFraction < 1.0)
    assertEquals(receipt.sensitivity.map(_.rank), Vector(2))
    assert(receipt.sensitivity.head.logLikelihood >= receipt.logLikelihood - 1e-6 * math.abs(receipt.logLikelihood))
  }

  test("a feature without unique variance hits the declared relative floor and is reported") {
    val base = draws(500, trueD, trueU, 7L)
    // Feature 0 becomes an exact copy of feature 2: rank one explains it fully
    // only if its unique variance collapses, so the floor must bind.
    val residuals = DMat.tabulate(base.rows, base.cols)((i, j) => if j == 0 then base(i, 2) else base(i, j))
    val policy = right(ResidualCovarianceFitPolicy(1, relativeFloor = 1e-2, maximumIterations = 5000, tolerance = 1e-12, convergence = ConvergencePolicy.Record))
    val fitted = right(ResidualCovariance.fit(axis(6), residuals, "fixture residuals", binding(residuals.rows), policy))
    val s = secondMoments(residuals)
    assert(fitted.receipt.flooredFeatures.contains(0), fitted.receipt.flooredFeatures.toString)
    assertEqualsDouble(fitted.covariance.diagonalValues(0), 1e-2 * s(0), 1e-12 * s(0))
  }

  test("non-convergence is refused by default and recorded when declared") {
    val residuals = draws(200, trueD, trueU, 11L)
    val refuse = right(ResidualCovarianceFitPolicy(1, maximumIterations = 1))
    ResidualCovariance.fit(axis(6), residuals, "fixture residuals", binding(residuals.rows), refuse) match
      case Left(ResidualCovarianceError.NotConverged(1, _, _)) => ()
      case other => fail(s"expected NotConverged(1, _, _), got $other")
    val record = right(ResidualCovarianceFitPolicy(1, maximumIterations = 1, convergence = ConvergencePolicy.Record))
    val fitted = right(ResidualCovariance.fit(axis(6), residuals, "fixture residuals", binding(residuals.rows), record))
    assert(!fitted.receipt.converged)
    assertEquals(fitted.receipt.iterations, 1)
  }

  test("fit inputs and policies are validated with specific errors") {
    val residuals = draws(20, trueD, trueU, 3L)
    val policy = right(ResidualCovarianceFitPolicy(1))
    assertEquals(ResidualCovariance.fit(axis(6), residuals, " ", binding(residuals.rows), policy).left.toOption, Some(ResidualCovarianceError.InvalidReceipt))
    // Ledermann bound for p = 6: (6 - 3)^2 >= 9 admits h = 3; h = 4 is unidentified.
    assertEquals(ResidualCovariance.maximumIdentifiableRank(20, 6), 3)
    assertEquals(ResidualCovariance.fit(axis(6), residuals, "r", binding(residuals.rows), right(ResidualCovarianceFitPolicy(4))).left.toOption,
      Some(ResidualCovarianceError.InvalidRank(4, 3)))
    assertEquals(ResidualCovariance.fit(axis(6), residuals, "r", binding(residuals.rows), right(ResidualCovarianceFitPolicy(1, sensitivityRanks = Vector(5)))).left.toOption,
      Some(ResidualCovarianceError.InvalidRank(5, 3)))
    val offset = DMat.tabulate(20, 6)((i, j) => if j == 1 then residuals(i, j) + 0.5 else residuals(i, j))
    ResidualCovariance.fit(axis(6), offset, "r", binding(20), policy) match
      case Left(ResidualCovarianceError.NotCentered(1, mean)) => assert(mean > 1e-8)
      case other => fail(s"expected NotCentered(1, _), got $other")
    val withNaN = DMat.tabulate(20, 6)((i, j) => if i == 3 && j == 2 then Double.NaN else residuals(i, j))
    assertEquals(ResidualCovariance.fit(axis(6), withNaN, "r", binding(20), policy).left.toOption, Some(ResidualCovarianceError.NonFinite("residuals")))
    val zeroColumn = DMat.tabulate(20, 6)((i, j) => if j == 4 then 0.0 else residuals(i, j))
    assertEquals(ResidualCovariance.fit(axis(6), zeroColumn, "r", binding(20), policy).left.toOption, Some(ResidualCovarianceError.ZeroVarianceFeature(4)))
    assertEquals(ResidualCovariance.fit(axis(5), residuals, "r", binding(residuals.rows), policy).left.toOption, Some(ResidualCovarianceError.Shape("residuals", 20, 5, 20, 6)))
    assertEquals(ResidualCovarianceFitPolicy(0).left.toOption, Some(ResidualCovarianceError.InvalidPolicy("rank")))
    assertEquals(ResidualCovarianceFitPolicy(1, relativeFloor = 0.0).left.toOption, Some(ResidualCovarianceError.InvalidPolicy("relativeFloor")))
    assertEquals(ResidualCovarianceFitPolicy(1, tolerance = Double.NaN).left.toOption, Some(ResidualCovarianceError.InvalidPolicy("tolerance")))
    assertEquals(ResidualCovarianceFitPolicy(2, sensitivityRanks = Vector(2)).left.toOption, Some(ResidualCovarianceError.InvalidPolicy("sensitivityRanks")))
    assertEquals(ResidualCovarianceFitPolicy(1, diagonalMomentTolerance = 0.0).left.toOption, Some(ResidualCovarianceError.InvalidPolicy("diagonalMomentTolerance")))
    assertEquals(ResidualCovarianceFitPolicy(1, centeringTolerance = -1.0).left.toOption, Some(ResidualCovarianceError.InvalidPolicy("centeringTolerance")))
  }

  test("an ROI with fewer features than the noise rank keeps every factor and matches its dense marginal") {
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    val single = right(psi.restrict(roiAxis(Vector("v2"))))
    assertEquals(single.rank, 2)
    // Psi_22 = d_2 + |u_2|^2 = 1 + 9 + (3 + 1e-6)^2.
    val marginal = d5(2) + u5(2).map(value => value * value).sum
    assertEqualsDouble(right(single.applyPrecision(DMat.eye(1)))(0, 0), 1.0 / marginal, 1e-12 / marginal)
    assertEqualsDouble(single.logDeterminant, math.log(marginal), 1e-12)
  }

  test("a strongly ill-conditioned Psi still solves with small backward error and an accurate log determinant") {
    // Unique variances span seven decades under loadings of order 100: cond(Psi) ~ 1e10.
    val d = Vector(1e-6, 1e-5, 1e-4, 1.0, 10.0)
    val u = Vector(Vector(100.0, 1.0), Vector(100.0, -1.0), Vector(99.0, 2.0), Vector(-50.0, 3.0), Vector(10.0, 0.5))
    val psi = right(ResidualCovariance.fromFactors(axis(5), d, loadings(u)))
    val full = dense(d, u)
    val rhs = DMat.tabulate(5, 2)((i, j) => math.cos(0.7 * i + j))
    val solved = right(psi.applyPrecision(rhs))
    val reconstructed = full * solved
    // Normwise backward error |Psi x - b| / (|Psi| |x|) at ~1e4 machine epsilon.
    val psiNorm = (0 until 5).map(i => (0 until 5).map(j => math.abs(full(i, j))).sum).max
    val solutionNorm = (for i <- 0 until 5; j <- 0 until 2 yield math.abs(solved(i, j))).max
    for i <- 0 until 5; j <- 0 until 2 do
      assertEqualsDouble(reconstructed(i, j), rhs(i, j), 1e-12 * psiNorm * solutionNorm, s"backward residual ($i, $j)")
    val denseCholesky = right(full.cholesky)
    val denseLogDet = (0 until 5).map(i => 2.0 * math.log(denseCholesky.lower(i, i))).sum
    assertEqualsDouble(psi.logDeterminant, denseLogDet, 1e-9 * math.max(1.0, math.abs(denseLogDet)))
  }

  test("solve work is reported per call and never includes a features-by-features array") {
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    // Stored 2 p + p h + 2 (p + h) h + h = 10 + 10 + 28 + 2 = 50; augmented rows p + h = 7.
    assertEquals(psi.storedCells, 50L)
    // applyPrecision on 3 columns: four (p + h) x 3 blocks plus the 5 x 3 result = 99; peak adds the model.
    assertEquals(psi.precisionWork(3), Right(ResidualCovarianceWork(50L, 50L + 99L, 99L, 7, 3)))
    // applyCovariance on 3 columns: h x 3 projections plus the 5 x 3 result = 21.
    assertEquals(psi.covarianceWork(3), Right(ResidualCovarianceWork(50L, 50L + 21L, 21L, 5, 3)))
    // Diagonal: leading blocks 2 (p + h) h = 28, result 5, all 5 rows falling back 2 (p + h) 5 = 70.
    assertEquals(psi.precisionDiagonalWork, ResidualCovarianceWork(50L, 50L + 33L + 70L, 33L + 70L, 7, 5))
    assertEquals(psi.precisionWork(-1).left.toOption, Some(ResidualCovarianceError.Shape("work columns", 5, 1, 5, -1)))
    // p = 1, h = 2 (an ROI smaller than the rank): the 2 x c projection is the largest covariance-work array.
    // Stored 2 + 2 + 2 (3)(2) + 2 = 18; transient h c + p c = 6 + 3.
    val wide = right(ResidualCovariance.fromFactors(axis(1), Vector(1.0), DMat.dense(1, 2, Vector(1.0, 2.0))))
    assertEquals(wide.covarianceWork(3), Right(ResidualCovarianceWork(18L, 18L + 9L, 9L, 2, 3)))
  }

  test("precision along a dominant loading is computed without cancellation") {
    // p = h = 1, D = 1, U = 1e8: Psi = 1 + 1e16, so Psi^-1 = 1 / (1 + 1e16) ~ 1e-16.
    // The Woodbury difference 1 - 1e16 / (1 + 1e16) cancels to 0 in binary64.
    val psi = right(ResidualCovariance.fromFactors(axis(1), Vector(1.0), DMat.dense(1, 1, Vector(1e8))))
    val exact = 1.0 / (1.0 + 1e16)
    assertEqualsDouble(right(psi.applyPrecision(DMat.eye(1)))(0, 0), exact, 1e-12 * exact)
    assertEqualsDouble(right(psi.precisionDiagonal).head, exact, 1e-12 * exact)
    assertEqualsDouble(right(psi.precisionQuadratic(Vector(1.0))), exact, 1e-12 * exact)
    assertEqualsDouble(psi.logDeterminant, math.log1p(1e16), 1e-12 * math.log1p(1e16))
    // Every precision diagonal of a positive definite model is strictly positive.
    val many = right(ResidualCovariance.fromFactors(axis(3), Vector(1.0, 2.0, 3.0), DMat.dense(3, 2, Vector(1e8, 0.0, 1e8, 1e-3, 0.0, 5.0))))
    assert(right(many.precisionDiagonal).forall(_ > 0.0))
  }

  test("finite inputs whose results overflow are refused, never returned") {
    val huge = right(ResidualCovariance.fromFactors(axis(1), Vector(Double.MaxValue), DMat.dense(1, 1, Vector(0.0))))
    assertEquals(huge.applyCovariance(DMat.dense(1, 1, Vector(2.0))).left.toOption, Some(ResidualCovarianceError.NonFinite("covariance result")))
    val tiny = right(ResidualCovariance.fromFactors(axis(1), Vector(Double.MinPositiveValue), DMat.dense(1, 1, Vector(0.0))))
    assertEquals(tiny.precisionQuadratic(Vector(1e300)).left.toOption, Some(ResidualCovarianceError.NonFinite("quadratic result")))
    assertEquals(tiny.applyPrecision(DMat.dense(1, 1, Vector(1e300))).left.toOption, Some(ResidualCovarianceError.NonFinite("precision result")))
    // Finite residuals whose squares overflow: the first feature's second moment is infinite.
    val wide = DMat.tabulate(6, 4)((i, j) => if j == 0 then (if i % 2 == 0 then 1e200 else -1e200) else math.sin(i + 3.0 * j) - (0 until 6).map(r => math.sin(r + 3.0 * j)).sum / 6.0)
    assertEquals(ResidualCovariance.fit(axis(4), wide, "r", binding(6), right(ResidualCovarianceFitPolicy(1))).left.toOption,
      Some(ResidualCovarianceError.NonFinite("residual second moments")))
  }

  test("residual rows must match the training binding and ROI axes must share basis, units and scale") {
    val residuals = draws(20, trueD, trueU, 5L)
    assertEquals(ResidualCovariance.fit(axis(6), residuals, "r", binding(19), right(ResidualCovarianceFitPolicy(1))).left.toOption,
      Some(ResidualCovarianceError.TrainingRowsMismatch(19, 20)))
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    val otherUnits = right(AxisRef.fromStableKeys("roi", SpaceRole.Observed, Vector("v1"), "voxel", "zscore", "raw"))
    assertEquals(psi.restrict(otherUnits).left.toOption, Some(ResidualCovarianceError.IncompatibleRoi("units", "psc", "zscore")))
    val otherBasis = right(AxisRef.fromStableKeys("roi", SpaceRole.Observed, Vector("v1"), "vertex", "psc", "raw"))
    assertEquals(psi.restrict(otherBasis).left.toOption, Some(ResidualCovarianceError.IncompatibleRoi("basis", "voxel", "vertex")))
  }

  test("dominant loadings: span and complement precision, stacked factors and degenerate columns match analytic values") {
    def rel(actual: Double, expected: Double, tolerance: Double)(using munit.Location): Unit =
      assertEqualsDouble(actual, expected, tolerance * math.abs(expected))
    // D = (1, 4), U = (1e8, 0): Psi = diag(1 + 1e16, 4); the complement entry is untouched.
    val split = right(ResidualCovariance.fromFactors(axis(2), Vector(1.0, 4.0), DMat.dense(2, 1, Vector(1e8, 0.0))))
    val tiny = 1.0 / (1.0 + 1e16)
    val diagonal = right(split.precisionDiagonal)
    rel(diagonal(0), tiny, 1e-12)
    rel(diagonal(1), 0.25, 1e-14)
    val applied = right(split.applyPrecision(DMat.dense(2, 2, Vector(1.0, 0.0, 0.0, 1.0))))
    rel(applied(0, 0), tiny, 1e-12)
    rel(applied(1, 1), 0.25, 1e-14)
    rel(right(split.precisionQuadratic(Vector(1.0, 1.0))), tiny + 0.25, 1e-14)
    // p = 1, h = 2, U = (1e8, 1e8): Psi = 1 + 2e16.
    val stacked = right(ResidualCovariance.fromFactors(axis(1), Vector(1.0), DMat.dense(1, 2, Vector(1e8, 1e8))))
    rel(right(stacked.precisionDiagonal).head, 1.0 / (1.0 + 2e16), 1e-12)
    rel(stacked.logDeterminant, math.log1p(2e16), 1e-14)
    // Rotated span: D = I, U = 1e8 (cos t, sin t). Along u the precision is 1 / (1 + 1e16); orthogonal to u it is 1.
    val (c, s) = (math.cos(0.3), math.sin(0.3))
    val rotated = right(ResidualCovariance.fromFactors(axis(2), Vector(1.0, 1.0), DMat.dense(2, 1, Vector(1e8 * c, 1e8 * s))))
    // Along u, cond(Psi) ~ 1e16: the applied vector is backward stable (absolute
    // error ~ eps |x| / min d, so ~1e-16 here) but not relatively accurate,
    // while the quadratic form comes from an accurately computed coordinate.
    val alongSpan = right(rotated.applyPrecision(DMat.dense(2, 1, Vector(c, s))))
    assertEqualsDouble(alongSpan(0, 0), c * tiny, 1e-15)
    assertEqualsDouble(alongSpan(1, 0), s * tiny, 1e-15)
    rel(right(rotated.precisionQuadratic(Vector(c, s))), tiny, 1e-12)
    val orthogonal = right(rotated.applyPrecision(DMat.dense(2, 1, Vector(-s, c))))
    rel(orthogonal(0, 0), -s, 1e-12)
    rel(orthogonal(1, 0), c, 1e-12)
    // Duplicate and zero columns: D + 2 u u^T, with the zero column contributing nothing.
    val u = Vector(1.0, -2.0, 0.5)
    val degenerate = right(ResidualCovariance.fromFactors(axis(3), Vector(1.0, 2.0, 3.0),
      DMat.tabulate(3, 3)((i, k) => if k < 2 then u(i) else 0.0)))
    val oracle = dense(Vector(1.0, 2.0, 3.0), u.map(value => Vector(value, value)))
    assertClose(right(degenerate.applyPrecision(DMat.eye(3))), right(oracle.solve(DMat.eye(3))), 1e-12)
    val oracleCholesky = right(oracle.cholesky)
    rel(degenerate.logDeterminant, (0 until 3).map(i => 2.0 * math.log(oracleCholesky.lower(i, i))).sum, 1e-12)
  }

  test("the precision bilinear form is symmetric and positive") {
    val psi = right(ResidualCovariance.fromFactors(axis(5), d5, loadings(u5)))
    val x = DMat.tabulate(5, 1)((i, _) => math.sin(1.0 + i))
    val y = DMat.tabulate(5, 1)((i, _) => math.cos(2.0 * i))
    val px = right(psi.applyPrecision(x))
    val py = right(psi.applyPrecision(y))
    val xPy = (0 until 5).map(i => x(i, 0) * py(i, 0)).sum
    val yPx = (0 until 5).map(i => y(i, 0) * px(i, 0)).sum
    assertEqualsDouble(xPy, yPx, 1e-12 * math.max(math.abs(xPy), 1.0))
    val quadratic = right(psi.precisionQuadratic(Vector.tabulate(5)(i => x(i, 0))))
    assert(quadratic > 0.0)
    assertEqualsDouble(quadratic, (0 until 5).map(i => x(i, 0) * px(i, 0)).sum, 1e-12 * quadratic)
  }

  test("more than one chunk of precision-diagonal fallback rows is evaluated and bounded") {
    // p = h = 70, D = I, U = 1e8 I: every row lies in a dominant span, so every row
    // falls back to the direct tail-square form, across two 64-row chunks.
    val p = 70
    val psi = right(ResidualCovariance.fromFactors(axis(p), Vector.fill(p)(1.0), DMat.tabulate(p, p)((i, k) => if i == k then 1e8 else 0.0)))
    val expected = 1.0 / (1.0 + 1e16)
    val values = right(psi.precisionDiagonal)
    assertEquals(values.length, p)
    values.foreach(value => assertEqualsDouble(value, expected, 1e-12 * expected))
    // Stored 2 p + p h + 2 (p + h) h + h = 140 + 4900 + 19600 + 70 = 24710.
    assertEquals(psi.storedCells, 24710L)
    // Leading blocks 2 (p + h) h = 19600 and result 70; fallback 2 (p + h) per row,
    // all 70 rows cumulatively (19600) and one 64-row chunk at peak (17920).
    assertEquals(psi.precisionDiagonalWork, ResidualCovarianceWork(24710L, 24710L + 19670L + 17920L, 19670L + 19600L, 140, 70))
  }
