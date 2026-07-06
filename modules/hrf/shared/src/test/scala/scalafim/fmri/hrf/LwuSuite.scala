package scalafim.fmri.hrf

import scalafim.fmri.hrf.*

class LwuSuite extends munit.FunSuite:

  test("lwu height normalization peaks at 1") {
    val t: Seq[Seconds] = (0 to 40).map(i => Seconds(i * 0.5))
    val norm = HrfFunctions.lwuSeries(t, tau = 6.0, sigma = 2.0, rho = 0.4, normalize = HrfFunctions.LwuNormalize.Height)
    val maxAbs = norm.map(math.abs).max
    assert(math.abs(maxAbs - 1.0) < 1e-9)
  }

  test("lwu area normalization currently equals none") {
    val raw = Hrfs.lwu(tau = 6.0, sigma = 2.0, rho = 0.4, normalize = HrfFunctions.LwuNormalize.None)
    val area = Hrfs.lwu(tau = 6.0, sigma = 2.0, rho = 0.4, normalize = HrfFunctions.LwuNormalize.Area)
    val t = (0 to 40).map(_ * 0.5)
    val r0 = raw.evalDoubles(t).data
    val rA = area.evalDoubles(t).data
    assert(TestUtils.maxAbsDiff(r0, rA) < 1e-9)
  }

  test("LwuBasis finite-difference derivative agrees") {
    val theta = LwuParams(tau = 6.0, sigma = 2.0, rho = 0.4)
    val t = (0 to 20).map(_.toDouble)
    val basis = LwuBasis(theta, t.map(Seconds(_)))
    assertEquals(basis.cols, 4)
    assertEquals(basis.rows, t.length)
    val t0 = 4.0
    val delta = 1e-4
    val fdTau =
      (HrfFunctions.lwu(t0.s, theta.tau + delta, theta.sigma, theta.rho) -
        HrfFunctions.lwu(t0.s, theta.tau - delta, theta.sigma, theta.rho)) / (2.0 * delta)
    val idx = t.indexOf(t0)
    val got = basis(idx, 1)
    assert(math.abs(got - fdTau) < 1e-3)
  }
