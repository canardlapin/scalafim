package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*

/** The box response is `∫_{l-d}^{l} h(τ) dτ`, so the closed forms are checked
  * against the thing they claim to compute — a refined trapezoid — rather than
  * against each other.
  */
class PrimitiveSuite extends munit.FunSuite:

  private val grid = (0 to 60).map(_ * 0.5)

  /** Kernels that should carry a primitive, with the tolerance at which a
    * refined trapezoid can confirm it. Smooth kernels converge `O(h²)`;
    * kernels with a jump only `O(h)`, so they get a looser reference bound.
    */
  private val exactFamilies: Seq[(String, Hrf, Double, Double)] = Seq(
    ("gamma", Hrfs.gamma(), 6.0, 1e-7),
    // The Gaussian is masked to zero below lag 0 but has `dnorm(0; 6, 2) ≈ 2.2e-3`
    // there, so the kernel jumps at the causal boundary and the *reference*
    // trapezoid is only `O(h)`: at `h = 1e-4` it is itself ~1.1e-7 from the
    // truth. The bound here is the reference's accuracy, not the primitive's.
    ("gaussian", Hrfs.gaussian(), 5.0, 1e-6),
    ("spmg1", Hrfs.SPMG1, 4.0, 1e-7),
    ("spmg2", Hrfs.SPMG2, 4.0, 1e-7),
    ("spmg3", Hrfs.SPMG3, 4.0, 1e-7),
    ("boxcar", Hrfs.boxcar(3.0.s), 2.0, 1e-4),
    ("fir", Hrfs.fir(nBasis = 12, span = 24.0.s), 5.0, 1e-4),
    ("bspline", Hrfs.bspline(nBasis = 5, span = 24.0.s), 5.0, 1e-4),
    ("tent", Hrfs.tent(nBasis = 5, span = 24.0.s), 5.0, 1e-4)
  )

  private def maxAbsDiff(a: Mat, b: Mat): Double =
    assertEquals(a.data.length, b.data.length)
    var worst = 0.0
    var i = 0
    while i < a.data.length do
      worst = math.max(worst, math.abs(a.data(i) - b.data(i)))
      i += 1
    worst

  test("every registered primitive agrees with a refined trapezoid"):
    exactFamilies.foreach { case (name, hrf, duration, tol) =>
      val exact = Evaluate.doubles(hrf, grid, duration = duration, integration = Integration.Exact)
      val refined = Evaluate.doubles(
        hrf,
        grid,
        duration = duration,
        precision = 1e-4,
        integration = Integration.Trapezoid
      )
      val worst = maxAbsDiff(exact, refined)
      assert(worst < tol, s"$name: closed form differs from refined quadrature by $worst (tol $tol)")
    }

  test("a primitive is registered for exactly the families that claim one"):
    exactFamilies.foreach { case (name, hrf, _, _) =>
      assert(
        Primitive.definiteIntegral(hrf, Lag(0.0), Lag(5.0)).isDefined,
        s"$name should have a primitive"
      )
    }
    // Families with no closed form registered must say so rather than guess.
    Seq(
      "mexhat" -> Hrfs.mexhat(),
      "inv_logit" -> Hrfs.invLogit(),
      "lwu" -> Hrfs.lwu(),
      "fourier" -> Hrfs.fourier(),
      "daguerre" -> Hrfs.daguerre()
    ).foreach { case (name, hrf) =>
      assert(
        Primitive.definiteIntegral(hrf, Lag(0.0), Lag(5.0)).isEmpty,
        s"$name should fall back to quadrature"
      )
    }

  test("deriving a kernel clears its primitive rather than inheriting a wrong one"):
    // A lagged, blocked or rescaled SPMG1 is a different function; keeping the
    // canonical primitive would silently integrate the wrong shape.
    val base = Hrfs.SPMG1
    assert(Primitive.definiteIntegral(base, Lag(0.0), Lag(8.0)).isDefined)
    Seq(
      "lagged" -> base.lag(3.0.s),
      "normalized" -> base.normalize(),
      "blocked" -> base.block(2.0.s)
    ).foreach { case (what, derivedHrf) =>
      assert(
        Primitive.definiteIntegral(derivedHrf, Lag(0.0), Lag(8.0)).isEmpty,
        s"$what SPMG1 must not inherit the canonical primitive"
      )
    }

  test("exact integration makes the answer independent of precision"):
    exactFamilies.foreach { case (name, hrf, duration, _) =>
      val reference = Evaluate.doubles(hrf, grid, duration = duration, precision = 1.0, integration = Integration.Exact)
      Seq(0.5, 0.1, 0.01).foreach { p =>
        val other = Evaluate.doubles(hrf, grid, duration = duration, precision = p, integration = Integration.Exact)
        assertEqualsDouble(maxAbsDiff(reference, other), 0.0, 0.0, s"$name moved when precision became $p")
      }
    }

  test("the trapezoid converges to the closed form as precision refines"):
    val hrf = Hrfs.SPMG1
    val exact = Evaluate.doubles(hrf, grid, duration = 4.0, integration = Integration.Exact)
    val gaps = Seq(1.0, 0.5, 0.1, 0.01).map { p =>
      maxAbsDiff(Evaluate.doubles(hrf, grid, duration = 4.0, precision = p, integration = Integration.Trapezoid), exact)
    }
    gaps.zip(gaps.tail).foreach { (coarse, fine) =>
      assert(fine < coarse, s"refining precision did not close the gap to the closed form ($coarse -> $fine)")
    }
    assert(gaps.last < 1e-4, s"trapezoid at precision 0.01 is still ${gaps.last} from the closed form")

  test("the unit-mass box stays the duration average under exact integration"):
    exactFamilies.foreach { case (name, hrf, duration, _) =>
      val height = Evaluate.doubles(hrf, grid, duration = duration, summate = true, integration = Integration.Exact)
      val mass = Evaluate.doubles(hrf, grid, duration = duration, summate = false, integration = Integration.Exact)
      var i = 0
      while i < height.data.length do
        assertEqualsDouble(mass.data(i), height.data(i) / duration, 1e-12, s"$name unit-mass at $i")
        i += 1
    }

  test("an impulse is untouched by the integration mode"):
    exactFamilies.foreach { case (name, hrf, _, _) =>
      val exact = Evaluate.doubles(hrf, grid, duration = 0.0, integration = Integration.Exact)
      val trap = Evaluate.doubles(hrf, grid, duration = 0.0, integration = Integration.Trapezoid)
      assertEqualsDouble(maxAbsDiff(exact, trap), 0.0, 0.0, s"$name impulse response changed")
    }

  test("FIR box response is the analytic overlap of the box with each bin"):
    // 12 bins over 24 s => 2 s bins. A 2 s unit-height box at lag l puts
    // exactly the overlap of [l-2, l] with each bin into that bin's column.
    val nb = 12
    val span = 24.0
    val w = span / nb
    val hrf = Hrfs.fir(nBasis = nb, span = span.s)
    val duration = 2.0
    Seq(0.5, 3.0, 5.5, 10.0, 23.0).foreach { l =>
      val got = Evaluate.doubles(hrf, Seq(l), duration = duration, integration = Integration.Exact)
      var j = 0
      while j < nb do
        val binLo = j * w
        val binHi = (j + 1) * w
        val lo = math.max(l - duration, math.max(binLo, 0.0))
        val hi = math.min(l, math.min(binHi, span))
        val expected = math.max(0.0, hi - lo)
        assertEqualsDouble(got(0, j), expected, 1e-12, s"fir bin $j at lag $l")
        j += 1
    }

  test("boxcar box response is the analytic overlap of two boxes"):
    val width = 3.0
    val hrf = Hrfs.boxcar(width.s)
    val duration = 2.0
    Seq(0.0, 1.0, 2.5, 3.5, 4.9, 6.0).foreach { l =>
      val got = Evaluate.doubles(hrf, Seq(l), duration = duration, integration = Integration.Exact)
      val lo = math.max(l - duration, 0.0)
      val hi = math.min(l, width)
      assertEqualsDouble(got(0, 0), math.max(0.0, hi - lo), 1e-12, s"boxcar at lag $l")
    }

  test("incomplete gamma and erf match known closed forms"):
    // P(1, x) = 1 - e^{-x}
    Seq(0.1, 0.5, 1.0, 2.0, 5.0, 20.0).foreach { x =>
      assertEqualsDouble(Special.lowerGammaP(1.0, x), 1.0 - math.exp(-x), 1e-13, s"P(1,$x)")
    }
    // P(1/2, x) = erf(sqrt(x))
    Seq(0.25, 1.0, 4.0).foreach { x =>
      assertEqualsDouble(Special.lowerGammaP(0.5, x), Special.erf(math.sqrt(x)), 1e-13, s"P(1/2,$x)")
    }
    assertEqualsDouble(Special.erf(0.0), 0.0, 0.0)
    assertEqualsDouble(Special.erf(1.0), 0.8427007929497149, 1e-12)
    assertEqualsDouble(Special.erf(-1.0), -0.8427007929497149, 1e-12)
    assertEqualsDouble(Special.erf(3.0), 0.9999779095030014, 1e-12)
    assertEqualsDouble(Special.normalCdf(0.0), 0.5, 1e-14)
    assertEqualsDouble(Special.normalCdf(1.96), 0.9750021048517795, 1e-12)

  test("the gamma primitive is the gamma CDF"):
    // ∫₀ˣ dgamma(τ; k, r) dτ = pgamma(x; k, r)
    val shape = 6.0
    val rate = 1.0
    val hrf = Hrfs.gamma(shape, rate)
    Seq(1.0, 4.0, 8.0, 15.0).foreach { x =>
      val got = Primitive.definiteIntegral(hrf, Lag(0.0), Lag(x)).get(0)
      assertEqualsDouble(got, Special.lowerGammaP(shape, rate * x), 1e-14, s"gamma CDF at $x")
    }
    // For an integer shape the gamma CDF is Erlang, so it has an elementary
    // closed form to check against that owes nothing to the implementation:
    //   P(k, x) = 1 - e^{-x} Σ_{j<k} x^j / j!
    def erlang(k: Int, x: Double): Double =
      var term = 1.0
      var sum = 1.0
      var j = 1
      while j < k do
        term *= x / j
        sum += term
        j += 1
      1.0 - math.exp(-x) * sum

    Seq(1.0, 4.0, 8.0, 15.0).foreach { x =>
      assertEqualsDouble(
        Primitive.definiteIntegral(hrf, Lag(0.0), Lag(x)).get(0),
        erlang(6, x),
        1e-13,
        s"gamma CDF at $x should match the Erlang form"
      )
    }
