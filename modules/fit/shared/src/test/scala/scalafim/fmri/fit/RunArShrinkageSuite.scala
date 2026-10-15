package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.fmri.ar.TimeSegments
import scalafim.fmri.fit.fixtures.RunShrunkKrRFixture

class RunArShrinkageSuite extends munit.FunSuite:
  private val lengths = Vector(20, 30)
  private val runs = TimeSegments.fromRunLengths(lengths)
  private val n = lengths.sum
  private val censor = Vector(5, 33)
  private val design: DMat = Matrix.tabulate(n, 4): (row, col) =>
    col match
      case 0 => math.cos(.3 * row)
      case 1 => math.sin(.11 * row) * row / n
      case 2 => if row < 20 then 1.0 else 0.0
      case _ => if row >= 20 then 1.0 else 0.0
  private val response = Matrix.tabulate(n, 1)((row, _) => math.sin(.7 * row) + .3 * math.cos(1.3 * math.pow(row, 1.1)) + .2 * design(row, 0))
  private def right[A](value: Either[FitError, A]): A = value.fold(e => fail(e.message), identity)
  private def spec(retention: Double) = VoxelwiseArSpec(2,
    VoxelArCoefficients.RunShrunk(RunArRetention.unsafe(retention), 25),
    CensorContinuity.ContinuousMissing, CovarianceUncertainty.KenwardRoger)

  test("retention endpoints preserve run-local estimation or impose one voxel filter"):
    val perRun = right(VoxelwiseArFgls.fit(design, response, runs, censor,
      spec(1).copy(coefficients = VoxelArCoefficients.Estimated(VoxelArScope.PerRun, 25)))).voxels.head
    val unchanged = right(VoxelwiseArFgls.fit(design, response, runs, censor, spec(1))).voxels.head
    val pooled = right(VoxelwiseArFgls.fit(design, response, runs, censor, spec(0))).voxels.head
    assertEquals(unchanged.phiByRun, perRun.phiByRun)
    for i <- 0 until 4; j <- 0 until 4 do
      assertEqualsDouble(unchanged.adjustedCovariance(i, j), perRun.adjustedCovariance(i, j), 1e-12)
    assertEquals(pooled.phiByRun(0), pooled.phiByRun(1))
    assert(RunArRetention(-.1).isLeft)
    assert(RunArRetention(1.1).isLeft)
    assert(RunArRetention(Double.NaN).isLeft)

  test("linear covariance propagation retains scale and creates cross-run covariance"):
    val a = KenwardRoger.runShrinkageJacobian(lengths, 2, RunArRetention.unsafe(.5))
    val covariance = a * a.t
    assertEqualsDouble(covariance(0, 0), 1.0, 0.0)
    assertEqualsDouble(covariance(0, 1), 0.0, 0.0)
    assertEqualsDouble(covariance(1, 3), .38, 1e-15)
    assertEqualsDouble(covariance(1, 2), 0.0, 0.0)

  test("shrinkage KR working covariance and contrast df match independent dense R finite differences"):
    val expected = RunShrunkKrRFixture.HalfRetention
    val augmented = Matrix.tabulate(n, 6)((row, col) => if col < 4 then design(row, col) else if row == censor(col - 4) then 1.0 else 0.0)
    val fit = right(VoxelwiseArFgls.fitVoxel(augmented, 4, response, runs,
      Vector(Vector(.384, -.069), Vector(.324, .016)), Vector.empty, spec(.5)))
    assertEqualsDouble(fit.residualVariance, expected.sigma2, 1e-10)
    for i <- 0 until 4 do
      assertEqualsDouble(fit.coefficients(i), expected.beta(i), 1e-10)
      for j <- 0 until 4 do
        assertEqualsDouble(fit.covariance(i, j), expected.conditional(i * 4 + j), 1e-10)
        assertEqualsDouble(fit.adjustedCovariance(i, j), expected.adjusted(i * 4 + j), 2e-6)
    Vector((1, expected.tDf, expected.tScale), (2, expected.fDf, expected.fScale)).foreach: (q, df, scale) =>
      val contrast = Matrix.tabulate(q, 4)((i, j) => if i == j then 1.0 else 0.0)
      val test = right(fit.test(contrast, Vector.fill(q)(0.0)))
      assertEqualsDouble(test.denominatorDf, df, 2e-6)
      assertEqualsDouble(test.scale, scale, 2e-6)
