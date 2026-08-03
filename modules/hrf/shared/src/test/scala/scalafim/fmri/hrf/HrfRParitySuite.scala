package scalafim.fmri.hrf

import scalafim.fmri.hrf.fixtures.HrfRParityFixtures
import scalafim.fmri.hrf.fixtures.HrfRParityFixtures.KernelFixture

/** Pins every stock kernel to the R `fmrihrf` reference corpus.
  *
  * Regenerate the corpus with
  * `Rscript tools/r-parity/generate_fmrihrf_r_parity_fixtures.R`.
  *
  * Causal (non-negative) lags are asserted here for every kernel. Negative lags
  * are covered separately by [[HrfCausalitySuite]], because scalafim
  * deliberately diverges from R there — see `docs/plans/hrf-hardening.md` §6.
  */
class HrfRParitySuite extends munit.FunSuite:

  private val tol = 1e-9

  /** Kernels configured to match the R fixture's parameters exactly. */
  private val kernels: Map[String, Hrf] = Map(
    "spmg1" -> Hrfs.SPMG1,
    "spmg2" -> Hrfs.SPMG2,
    "spmg3" -> Hrfs.SPMG3,
    "gamma" -> Hrfs.gamma(shape = 6.0, rate = 1.0),
    "gaussian" -> Hrfs.gaussian(mean = 6.0, sd = 2.0),
    "mexhat" -> Hrfs.mexhat(mean = 6.0, sd = 2.0),
    "inv_logit" -> Hrfs.invLogit(mu1 = 6.0, s1 = 1.0, mu2 = 16.0, s2 = 1.0),
    "lwu" -> Hrfs.lwu(tau = 6.0, sigma = 2.5, rho = 0.35),
    "fourier" -> Hrfs.fourier(nBasis = 5, span = 24.0.s),
    "sine" -> Hrfs.sine(nBasis = 5, span = 24.0.s),
    "bspline" -> Hrfs.bspline(nBasis = 5, span = 24.0.s, degree = 3),
    "tent" -> Hrfs.tent(nBasis = 5, span = 24.0.s),
    "fir" -> Hrfs.fir(nBasis = 12, span = 24.0.s)
  )

  /** Kernels where scalafim deliberately does not reproduce R.
    *
    * `daguerre`: R's `daguerre_basis` rescales each column by the maximum
    * observed over whatever `t` vector the caller happened to pass, so the same
    * kernel returns different values for different query grids — evaluating on
    * a grid that includes negative lags inflates the divisor and shrinks every
    * reported value. scalafim normalizes once at construction against a fixed
    * `[0, span]` grid, which is grid-independent. Filed upstream; see
    * `docs/plans/hrf-hardening.md` §6.
    */
  private val deliberateDivergences = Set("daguerre")

  private def causalIndices(fixture: KernelFixture): Vector[Int] =
    fixture.times.indices.filter(i => fixture.times(i) >= 0.0).toVector

  kernels.foreach { case (name, hrf) =>
    test(s"$name matches R fmrihrf at non-negative lags"):
      val fixture = HrfRParityFixtures.kernel(name)
      assertEquals(hrf.nbasis, fixture.nbasis, s"$name basis count differs from R")
      causalIndices(fixture).foreach { i =>
        val t = fixture.times(i)
        val actual = hrf(Lag(t)).data
        var j = 0
        while j < fixture.nbasis do
          assertEqualsDouble(
            actual(j),
            fixture.at(i, j),
            tol,
            s"$name at t=$t basis=$j"
          )
          j += 1
      }
  }

  test("every generated kernel fixture is accounted for"):
    val generated = HrfRParityFixtures.kernels.map(_.name).toSet
    // half_cosine is generated for reference but scalafim's `halfCosine` derives
    // its own span from h1..h4, so it is asserted in HrfBasicsSuite instead.
    val asserted = kernels.keySet ++ deliberateDivergences ++ Set("half_cosine")
    assertEquals(
      generated -- asserted,
      Set.empty[String],
      "R fixtures exist for kernels with neither a parity assertion nor a recorded divergence"
    )

  test("bspline knots are grid-independent (R's HRF_BSPLINE is not)"):
    // R has two B-spline definitions that disagree. `hrf_bspline()` passes
    // explicit knots derived from the span — quantiles of `seq(0, span)`, i.e.
    // (8, 16) for span 24 — and that is what scalafim implements. But
    // `hrf_bspline_generator()`, which builds `HRF_BSPLINE`, calls
    // `splines::bs()` with no `knots =` argument, so the interior knots become
    // quantiles of *whatever time vector the caller passes*. On the regressor's
    // fine grid `seq(0, 24, by = 0.33)` (which ends at 23.76, not 24) the knots
    // land at (7.92, 15.84) instead, a ~1% difference in the resulting columns.
    //
    // That makes R's HRF_BSPLINE not a function of t: `evaluate(HRF_BSPLINE,
    // 0.33)` alone collapses every knot onto a single point. A pointwise causal
    // kernel cannot express that, and should not — a basis whose definition
    // depends on the sampling grid gives different regressors for the same
    // experiment at different TRs. Filed upstream; see
    // `docs/plans/hrf-hardening.md` §6.
    val hrf = Hrfs.bspline(nBasis = 5, span = 24.0.s, degree = 3)
    val probes = Vector(0.5, 5.0, 12.0, 23.0)
    def sample(grid: Seq[Double]): Vector[Vector[Double]] =
      hrf.evalDoubles(grid) // may not perturb the kernel
      probes.map(t => hrf(Lag(t)).data.toVector)
    val onCoarse = sample((0 to 24).map(_ * 1.0))
    val onFine = sample((0 to 72).map(_ * 0.33))
    val onSingleton = sample(Vector(0.33))
    assertEquals(onFine, onCoarse, "bspline columns changed with the evaluation grid")
    assertEquals(onSingleton, onCoarse, "bspline columns changed for a singleton grid")

  test("daguerre normalization is grid-independent (R's is not)"):
    // The justification for diverging from R: asking for different time points
    // must not change the kernel. R's daguerre_basis fails this.
    val hrf = Hrfs.daguerre(nBasis = 3, scale = 4.0, span = 24.0.s)
    val probes = Vector(0.0, 3.0, 7.5, 18.0)
    val reference = probes.map(t => hrf(Lag(t)).data.toVector)
    // Evaluating over a wider grid, including negative lags, must not rescale.
    hrf.evalDoubles(Vector(-8.0, -2.0) ++ probes ++ Vector(40.0))
    val again = probes.map(t => hrf(Lag(t)).data.toVector)
    assertEquals(again, reference, "daguerre values depend on the query grid")
    assertEqualsDouble(hrf(Lag(0.0)).data(0), 1.0, 1e-12, "daguerre column 0 should peak at 1 at lag 0")

  test("regressor impulse evaluation matches R fmrihrf"):
    import scalafim.fmri.hrf.regressor.{Regressor, evaluate}
    // `bspline_impulse` is excluded deliberately — see the grid-independence
    // test below.
    val cases = Map(
      "spmg1_aligned_impulse" -> Hrfs.SPMG1,
      "spmg1_unaligned_impulse" -> Hrfs.SPMG1,
      "spmg1_amplitude_modulated" -> Hrfs.SPMG1,
      "spmg3_impulse" -> Hrfs.SPMG3
    )
    cases.foreach { case (name, hrf) =>
      val f = HrfRParityFixtures.regressor(name)
      val reg = Regressor(
        onsets = f.onsets,
        hrf = hrf,
        duration = f.durations,
        amplitude = f.amplitudes
      )
      val actual = reg.evaluate(f.grid, precision = f.precision)
      assertEquals(actual.rows, f.grid.length, s"$name row count")
      assertEquals(actual.cols, f.nbasis, s"$name column count")
      var i = 0
      while i < actual.rows do
        var j = 0
        while j < actual.cols do
          assertEqualsDouble(
            actual(i, j),
            f.values(i * f.nbasis + j),
            1e-8,
            s"$name at grid=${f.grid(i)} basis=$j"
          )
          j += 1
        i += 1
    }
