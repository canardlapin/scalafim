package scalafim.phrfcmp.score

import scalafim.fmri.hrf.{Hrfs, Lag, Seconds}
import scalafim.fmri.hrf.family.{GaussianFamily, NormalizationRule, ShapePoint}

/** E-resp reconstruction against independent direct evaluation of the library kernels. Runs on JVM and JS. */
class ERespSuite extends munit.FunSuite:

  private def ok[A](r: Either[ERespError, A]): A = r.fold(e => fail(e.message), identity)
  private val grid = ok(ResponseGrid.standard())

  test("the protocol grid is 0.1 s over [0, 32]: 321 lags equal to the decimal values"):
    assertEquals(grid.size, 321)
    assertEquals(grid.lag(0), 0.0)
    assertEquals(grid.lag(3), 0.3)
    assertEquals(grid.lag(77), 7.7)
    assertEquals(grid.horizon, 32.0)
    assertEquals(ok(ResponseGrid.standard(48)).size, 481)

  test("grids refuse empty, unordered and negative lags; no exception"):
    assertEquals(ResponseGrid.of(Seq.empty).left.toOption, Some(ERespError.EmptyGrid))
    assert(ResponseGrid.of(Seq(0.0, 1.0, 1.0)).isLeft)
    assert(ResponseGrid.of(Seq(-1.0, 1.0)).isLeft)
    assert(ResponseGrid.standard(0).isLeft)

  test("canonical and informed bases equal the library SPMG kernels evaluated directly on 0-24 s"):
    val coef = Array(2.0, -1.0, 0.5, 3.0, 0.25, -0.75) // 2 conditions x 3 columns
    val r = ok(EResp.fromBasis(grid, KernelBasis.InformedThree, coef, 2))
    var worst = 0.0
    for i <- 0 until grid.size if grid.lag(i) <= 24.0 do
      val h = Hrfs.SPMG3(Lag(grid.lag(i))).data
      for c <- 0 until 2 do
        val expect = (0 until 3).map(k => coef(c * 3 + k) * h(k)).sum
        worst = math.max(worst, math.abs(r.curves(c)(i) - expect))
    assert(worst < 1e-14, s"worst $worst")
    val can = ok(EResp.fromBasis(grid, KernelBasis.Canonical, Array(3.0), 1))
    for i <- 0 until grid.size if grid.lag(i) <= 24.0 do
      assertEqualsDouble(can.curves(0)(i), 3.0 * Hrfs.SPMG1(Lag(grid.lag(i))).data(0), 1e-15)

  test("CAN and INF3 E-resp is exactly zero after the 24 s design span (owner decision 2026-10-02); 24 s itself is kept"):
    assertEquals(KernelBasis.SpmgSpanSeconds, 24.0)
    val canR = ok(EResp.fromBasis(grid, KernelBasis.Canonical, Array(3.0), 1)).curves(0)
    val infR = ok(EResp.fromBasis(grid, KernelBasis.InformedThree, Array(2.0, -1.0, 0.5, 3.0, 0.25, -0.75), 2))
    for i <- 0 until grid.size do
      if grid.lag(i) > 24.0 then
        assertEquals(canR(i), 0.0, s"CAN at ${grid.lag(i)}")
        assertEquals(infR.curves(0)(i), 0.0, s"INF3 c0 at ${grid.lag(i)}")
        assertEquals(infR.curves(1)(i), 0.0, s"INF3 c1 at ${grid.lag(i)}")
    // the raw kernel is not zero there (the deviation from fmrireg fitted_hrf is real), and 24.0 is still inside the span
    assert(scalafim.fmri.hrf.HrfFunctions.spmg1(Lag(28.0)) != 0.0)
    assert(canR(240) != 0.0)

  test("FIR piecewise constant equals the library FIR basis; zero at and beyond the last edge"):
    val fir = Hrfs.fir(nBasis = 32, span = Seconds(32.0))
    val coef = Array.tabulate(64)(k => math.sin(k.toDouble) + 0.1 * k)
    val r = ok(EResp.fromBasis(grid, KernelBasis.Fir(32, 1.0), coef, 2))
    for i <- 0 until grid.size do
      val h = fir(Lag(grid.lag(i))).data
      for c <- 0 until 2 do
        val expect = (0 until 32).map(k => coef(c * 32 + k) * h(k)).sum
        assertEqualsDouble(r.curves(c)(i), expect, 1e-15)
    assertEquals(r.curves(0)(320), 0.0)
    assertEquals(r.curves(1)(319), coef(63)) // lag 31.9 is in the last bin

  test("FIR E-resp is constant within each 1 s bin and jumps at the edges (no interpolation)"):
    val coef = Array.tabulate(32)(k => (k * k + 1).toDouble)
    val g = ok(ResponseGrid.of(Seq(0.0, 0.4, 0.999, 1.0, 1.5, 2.0, 31.5, 31.99, 32.0)))
    val c = ok(EResp.fromBasis(g, KernelBasis.Fir(32, 1.0), coef, 1)).curves(0)
    assertEquals(c.toVector, Vector(1.0, 1.0, 1.0, 2.0, 2.0, 5.0, 962.0, 962.0, 0.0))

  test("Gaussian PHRF reconstruction equals amplitude times the library Gaussian kernel evaluated directly"):
    val fam = GaussianFamily.Default
    val point = ShapePoint.unsafe(Vector(5.3, math.log(1.7)))
    val amps = Vector(3.0, -2.0, 0.5)
    val r = ok(EResp.fromParametric(grid, fam, point, NormalizationRule.Density, amps))
    val hrf = fam.toHrf(point)
    var worst = 0.0
    for i <- 0 until grid.size; c <- amps.indices do
      val direct = amps(c) * hrf(Lag(grid.lag(i))).data(0)
      worst = math.max(worst, math.abs(r.curves(c)(i) - direct))
    assert(worst < 1e-14, s"worst $worst")
    // and it is the density: peak amplitude a / (sd sqrt(2 pi))
    assertEqualsDouble(r.curves(0).max, 3.0 / (1.7 * math.sqrt(2 * math.Pi)), 2e-3)

  test("typed refusals: coefficient count, dimension, normalisation, non-finite"):
    assertEquals(EResp.fromBasis(grid, KernelBasis.Canonical, Array(1.0, 2.0), 1).left.toOption, Some(ERespError.CoefficientCount(1, 2)))
    assert(EResp.fromBasis(grid, KernelBasis.Canonical, Array(Double.NaN), 1).isLeft)
    assert(EResp.fromBasis(grid, KernelBasis.Canonical, Array.empty[Double], 0).isLeft)
    val fam = GaussianFamily.Default
    assert(EResp.fromParametric(grid, fam, ShapePoint.unsafe(Vector(5.0)), NormalizationRule.Density, Vector(1.0)).isLeft)
    assert(EResp.fromParametric(grid, fam, ShapePoint.unsafe(Vector(5.0, 0.1)), NormalizationRule.UnitIntegral, Vector(1.0)).isLeft)
    assert(EResp.fromParametric(grid, fam, ShapePoint.unsafe(Vector(5.0, 0.1)), NormalizationRule.Density, Vector(Double.PositiveInfinity)).isLeft)
