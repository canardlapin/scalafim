package scalafim.fmri.hrf.laws

import scalafim.fmri.hrf.*

/** Runs [[HrfLaws]] against the stock kernel library. */
class HrfLawsSuite extends munit.FunSuite:

  private val grid: Seq[Double] = (0 until 60).map(_ * 2.0)

  private def report(failures: Vector[LawFailure]): Unit =
    if failures.nonEmpty then fail(failures.map(_.message).mkString("\n"))

  private val scalarKernels: Vector[(String, Hrf)] = Vector(
    "spmg1" -> Hrfs.SPMG1,
    "gamma" -> Hrfs.Gamma,
    "gaussian" -> Hrfs.Gaussian,
    "mexhat" -> Hrfs.mexhat(),
    "inv_logit" -> Hrfs.invLogit(),
    "lwu" -> Hrfs.lwu(),
    "half_cosine" -> Hrfs.halfCosine(),
    "boxcar" -> Hrfs.boxcar(Seconds(3.0))
  )

  private val bases: Vector[(String, Hrf)] = Vector(
    "spmg2" -> Hrfs.SPMG2,
    "spmg3" -> Hrfs.SPMG3,
    "fourier" -> Hrfs.fourier(nBasis = 5),
    "sine" -> Hrfs.sine(nBasis = 5),
    "bspline" -> Hrfs.bspline(nBasis = 5),
    "tent" -> Hrfs.tent(nBasis = 5),
    "fir" -> Hrfs.fir(nBasis = 12),
    "daguerre" -> Hrfs.daguerre(nBasis = 3)
  )

  scalarKernels.foreach { case (name, hrf) =>
    test(s"$name obeys every scalar law"):
      report(HrfLaws.allScalar(hrf, grid))
  }

  (scalarKernels ++ bases).foreach { case (name, hrf) =>
    test(s"$name is causal and respects its support"):
      report(HrfLaws.causality(hrf) ++ HrfLaws.support(hrf))
  }

  (scalarKernels ++ bases).foreach { case (name, hrf) =>
    test(s"$name renders additively over events"):
      report(
        HrfLaws.emptyDrive(hrf, grid) ++
          HrfLaws.eventAdditivity(hrf, Seq(5.0, 25.0), Seq(40.0), grid) ++
          HrfLaws.homogeneity(hrf, Seq(10.0, 30.0), grid) ++
          HrfLaws.permutationInvariance(hrf, Seq(10.0, 30.0, 50.0), Seq(1.0, -0.5, 2.0), grid)
      )
  }

  (scalarKernels ++ bases).foreach { case (name, hrf) =>
    test(s"$name box quadrature converges and unit-mass is the average"):
      report(
        HrfLaws.unitMassRelation(hrf, grid) ++
          HrfLaws.quadratureConvergence(hrf, grid) ++
          HrfLaws.impulseIdentity(hrf, grid)
      )
  }

  bases.foreach { case (name, basis) =>
    test(s"$name reconstruction commutes with rendering"):
      val coefficients = Vector.tabulate(basis.nbasis)(i => 1.0 - 0.3 * i)
      report(HrfLaws.reconstructionCommutes(basis, coefficients, Seq(10.0, 34.0), grid))
  }

  bases.foreach { case (name, basis) =>
    test(s"$name is gauge-invariant under peak normalization"):
      val coefficients = Vector.tabulate(basis.nbasis)(i => 1.0 - 0.3 * i)
      val lags = (0 to 48).map(_ * 0.5)
      report(HrfLaws.gaugeInvariance(basis, coefficients, lags))
  }

  test("evaluation plans agree within the stated tolerance"):
    // Grid-aligned onsets: all three plans should agree tightly.
    scalarKernels.foreach { case (name, hrf) =>
      report(HrfLaws.planEquivalence(hrf, Seq(10.0, 30.0), grid, precision = 0.1, tol = 1e-2))
    }

  test("conv and loop diverge for onsets off the microtime grid, and the gap shrinks with precision"):
    // This is the honest statement of a real approximation difference rather
    // than an equality that would have to be fudged. `Conv` bins onsets onto
    // the microtime grid; `Loop` does not. R shows the same behaviour.
    import scalafim.fmri.hrf.regressor.{Regressor, evaluate}
    val onsets = Seq(10.0, 23.5, 44.2)
    val reg = Regressor(onsets, Hrfs.SPMG1)
    def gap(precision: Double): Double =
      val conv = reg.evaluate(grid, precision, Regressor.EvalMethod.Conv)
      val loop = reg.evaluate(grid, precision, Regressor.EvalMethod.Loop)
      conv.data.zip(loop.data).map((a, b) => math.abs(a - b)).max
    val coarse = gap(0.5)
    val fine = gap(0.01)
    assert(fine < coarse, s"refining precision did not shrink the conv/loop gap ($coarse -> $fine)")
    assert(fine < 5e-3, s"conv/loop gap $fine at precision 0.01 is larger than expected")

  test("causality holds even for a kernel whose own shape function ignores it"):
    // `Hrf.apply` is final and masks negative lags before calling
    // `evaluateInSupport`, so a kernel cannot be made non-causal through the
    // public interface even deliberately. This constructs one that tries.
    val tries = Hrf.scalar("tries-to-respond-early", span = Seconds(24.0))(_ => 42.0)
    assertEquals(tries(Lag(-1.0)).data(0), 0.0, "the wrapper let a pre-onset response through")
    assertEquals(tries(Lag(1.0)).data(0), 42.0, "the wrapper suppressed a legitimate response")
    assert(HrfLaws.causality(tries).isEmpty)

  test("support is enforced even when the shape function ignores it"):
    val leaky = Hrf.scalar(
      "leaks-past-horizon",
      span = Seconds(10.0),
      support = Support.Compact(Seconds(10.0))
    )(_ => 7.0)
    assertEquals(leaky(Lag(10.5)).data(0), 0.0, "the wrapper let a past-horizon response through")
    assertEquals(leaky(Lag(9.0)).data(0), 7.0)
    assert(HrfLaws.support(leaky).isEmpty)

  test("the law checkers report violations rather than passing vacuously"):
    // Guard against the checkers silently succeeding: feed them something wrong
    // and require a failure.
    val mismatched = HrfLaws.reconstructionCommutes(Hrfs.SPMG3, Seq(1.0), Seq(10.0), grid)
    assert(mismatched.nonEmpty, "wrong-width coefficients should have been reported")
    // An onset that genuinely does not land on the microtime grid, with the
    // tolerance set to zero, must be reported.
    val impossible = HrfLaws.planEquivalence(Hrfs.SPMG1, Seq(23.5), grid, precision = 0.33, tol = 0.0)
    assert(impossible.nonEmpty, "a zero tolerance should have been reported as a violation")
