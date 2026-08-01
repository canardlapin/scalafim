package scalafim.fmri.hrf

import scalafim.fmri.hrf.*

class LwuSuite extends munit.FunSuite:

  test("lwu height normalization peaks at 1") {
    val t: Seq[Lag] = (0 to 40).map(i => Lag(i * 0.5))
    val norm = HrfFunctions.lwuSeries(t, tau = 6.0, sigma = 2.0, rho = 0.4, normalize = HrfFunctions.LwuNormalize.Height)
    val maxAbs = norm.map(math.abs).max
    assert(math.abs(maxAbs - 1.0) < 1e-9)
  }

  test("lwu area normalization gives the kernel unit area") {
    // This used to be a silent no-op, so an area-normalized LWU was in fact
    // unnormalized; the previous test pinned that gap rather than the contract.
    val raw = Hrfs.lwu(tau = 6.0, sigma = 2.0, rho = 0.4, normalize = HrfFunctions.LwuNormalize.None)
    val area = Hrfs.lwu(tau = 6.0, sigma = 2.0, rho = 0.4, normalize = HrfFunctions.LwuNormalize.Area)
    val dt = 0.01
    val t = (0 to (24.0 / dt).toInt).map(_ * dt)

    def integral(hrf: Hrf): Double =
      val v = hrf.evalDoubles(t).data
      var acc = 0.0
      var i = 0
      while i < v.length do
        val w = if i == 0 || i == v.length - 1 then 0.5 else 1.0
        acc += w * v(i)
        i += 1
      acc * dt

    assertEqualsDouble(integral(area), 1.0, 1e-3, "area-normalized LWU should integrate to 1")

    // It is a rescaling of the same shape, not a different kernel: the ratio is
    // constant across lags. (Comparing against a locally recomputed integral
    // would only compare two quadrature estimates.)
    val r0 = raw.evalDoubles(t).data
    val rA = area.evalDoubles(t).data
    val ratios = r0.zip(rA).collect { case (a, b) if math.abs(a) > 1e-6 => b / a }
    assert(ratios.nonEmpty)
    assertEqualsDouble(ratios.max, ratios.min, 1e-9, "area normalization changed the kernel's shape")
    assert(TestUtils.maxAbsDiff(r0, rA) > 1e-6, "area normalization should have changed the scale")
  }

  test("LwuBasis finite-difference derivative agrees") {
    val theta = LwuParams(tau = 6.0, sigma = 2.0, rho = 0.4)
    val t = (0 to 20).map(_.toDouble)
    val basis = LwuBasis(theta, t.map(Lag(_)))
    assertEquals(basis.cols, 4)
    assertEquals(basis.rows, t.length)
    val t0 = 4.0
    val delta = 1e-4
    val fdTau =
      (HrfFunctions.lwu(Lag(t0), theta.tau + delta, theta.sigma, theta.rho) -
        HrfFunctions.lwu(Lag(t0), theta.tau - delta, theta.sigma, theta.rho)) / (2.0 * delta)
    val idx = t.indexOf(t0)
    val got = basis(idx, 1)
    assert(math.abs(got - fdTau) < 1e-3)
  }
