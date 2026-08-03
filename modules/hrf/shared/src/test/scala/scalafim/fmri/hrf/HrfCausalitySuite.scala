package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*

/** Causality and support are invariants of every kernel, enforced once in
  * [[Hrf.apply]] rather than trusted to each constructor.
  *
  * scalafim deliberately diverges from R `fmrihrf` here: `HRF_GAUSSIAN`,
  * `hrf_mexhat`, `hrf_inv_logit` and `hrf_lwu` all return a non-zero response
  * at negative lag in R (e.g. `hrf_mexhat(-1) = 0.0123`), which is unphysical —
  * a haemodynamic kernel cannot respond before its event. Filed upstream; see
  * `docs/plans/hrf-hardening.md` §6.
  */
class HrfCausalitySuite extends munit.FunSuite:

  private val negativeLags = Vector(-100.0, -24.0, -10.0, -5.0, -1.0, -0.1, -1e-9)

  private def allKernels: Vector[(String, Hrf)] = Vector(
    "spmg1" -> Hrfs.SPMG1,
    "spmg2" -> Hrfs.SPMG2,
    "spmg3" -> Hrfs.SPMG3,
    "gamma" -> Hrfs.Gamma,
    "gaussian" -> Hrfs.Gaussian,
    "mexhat" -> Hrfs.mexhat(),
    "inv_logit" -> Hrfs.invLogit(),
    "lwu" -> Hrfs.lwu(),
    "half_cosine" -> Hrfs.halfCosine(),
    "fourier" -> Hrfs.fourier(),
    "sine" -> Hrfs.sine(),
    "bspline" -> Hrfs.bspline(),
    "tent" -> Hrfs.tent(),
    "fir" -> Hrfs.fir(),
    "daguerre" -> Hrfs.daguerre(),
    "boxcar" -> Hrfs.boxcar(Seconds(3.0)),
    "empirical" -> Hrfs.empirical(Vector(0.0, 4.0, 8.0).map(Seconds(_)), Vector(0.0, 1.0, 0.0)),
    "weighted" -> Hrfs.weighted(Vector(0.0, 1.0, 0.5, 0.0), width = Some(Seconds(8.0)))
  )

  allKernels.foreach { case (name, hrf) =>
    test(s"$name is zero at every negative lag"):
      negativeLags.foreach { lag =>
        val v = hrf(Lag(lag)).data
        var j = 0
        while j < hrf.nbasis do
          assertEqualsDouble(v(j), 0.0, 0.0, s"$name responded at lag=$lag basis=$j")
          j += 1
      }
  }

  allKernels.foreach { case (name, hrf) =>
    test(s"$name respects its declared support"):
      hrf.support match
        case Support.Unbounded => // nothing to assert
        case Support.Compact(horizon) =>
          Vector(horizon.value + 1e-9, horizon.value + 0.5, horizon.value + 50.0).foreach { lag =>
            val v = hrf(Lag(lag)).data
            var j = 0
            while j < hrf.nbasis do
              assertEqualsDouble(v(j), 0.0, 0.0, s"$name non-zero past horizon at lag=$lag basis=$j")
              j += 1
          }
  }

  test("compact-support bases are declared compact, decaying ones are not"):
    val compact = allKernels.collect { case (n, h) if h.support.horizonOption.isDefined => n }.toSet
    val unbounded = allKernels.collect { case (n, h) if h.support.horizonOption.isEmpty => n }.toSet
    assertEquals(
      compact,
      Set("half_cosine", "fourier", "sine", "bspline", "tent", "fir", "boxcar", "empirical", "weighted"),
      "compact-support kernel set changed"
    )
    assertEquals(
      unbounded,
      Set("spmg1", "spmg2", "spmg3", "gamma", "gaussian", "mexhat", "inv_logit", "lwu", "daguerre"),
      "unbounded kernel set changed"
    )

  test("decaying kernels are not silently truncated at span"):
    // `span` is a computational horizon for these, not a support claim. R agrees.
    val hrf = Hrfs.SPMG1
    assert(hrf.span.value == 24.0)
    assert(
      math.abs(hrf(Lag(24.5)).data(0)) > 1e-6,
      "SPMG1 should still be non-zero just past its span; span is advisory for a decaying kernel"
    )

  test("blocking a compact basis no longer reads pre-onset values"):
    // Regression: `block` samples `hrf(t - offset)`, which for offsets > t used
    // to reach negative lags. On a Fourier basis that produced values with no
    // relation to the kernel (e.g. -4.25, 7.36, -6.13, 3.54 at lag 0).
    //
    // At lag 0 every offset above zero is now masked by causality, so only the
    // first quadrature node contributes and the result must be exactly its
    // trapezoid weight (dt/2) times the bare kernel.
    val dt = 0.5
    val fourier = Hrfs.fourier(nBasis = 4)
    val blocked = fourier.block(Seconds(4.0), precision = Seconds(dt))
    val atZero = blocked(Lag(0.0)).data
    val direct = fourier(Lag(0.0)).data
    var j = 0
    while j < fourier.nbasis do
      assertEqualsDouble(
        atZero(j),
        (dt / 2.0) * direct(j),
        1e-12,
        s"blocked kernel at lag 0 picked up something other than the offset-0 node (basis $j)"
      )
      j += 1

  test("lag and block shift the declared support"):
    val base = Hrfs.fir(nBasis = 12, span = 24.0.s)
    assertEquals(base.support, Support.Compact(24.0.s))
    assertEquals(base.lag(Seconds(3.0)).support, Support.Compact(27.0.s))
    assertEquals(base.block(Seconds(5.0)).support, Support.Compact(29.0.s))
    // A composite is compact only if every component is.
    assertEquals(HrfCombinators.bindBasis(Seq(base, Hrfs.SPMG1)).support, Support.Unbounded)
    assertEquals(
      HrfCombinators.bindBasis(Seq(base, Hrfs.boxcar(Seconds(40.0)))).support,
      Support.Compact(40.0.s)
    )
