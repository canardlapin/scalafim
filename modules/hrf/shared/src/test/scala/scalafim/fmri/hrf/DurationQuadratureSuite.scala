package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfCombinators.*
import scalafim.fmri.hrf.fixtures.HrfRParityFixtures
import scalafim.fmri.hrf.regressor.{Regressor, evaluate}

/** The box response is an integral, so it must converge as `precision` shrinks.
  *
  * Before this was fixed, every duration path in the module summed kernel
  * samples without quadrature weights, so the response scaled with
  * `duration / precision`: a 4 s epoch through SPMG1 peaked at 7.1 at
  * `precision = 1.0` and at 119.9 at `precision = 0.05`. `precision` was a
  * scale factor rather than a tolerance, and the amplitude of every epoch
  * regressor silently depended on it.
  *
  * R `fmrihrf` gets this right in `evaluate.HRF` (trapezoid weights via
  * `.block_offsets_weights`) and wrong in `evaluate.Reg`; scalafim uses the
  * convergent rule in both. See `docs/plans/hrf-hardening.md` §6.
  */
class DurationQuadratureSuite extends munit.FunSuite:

  private val grid = (0 to 40).map(_ * 1.0)

  private def peak(m: Mat): Double = m.data.map(math.abs).max

  private def fixtureHrf(name: String): Hrf =
    name match
      case n if n.startsWith("spmg1") => Hrfs.SPMG1
      case n if n.startsWith("spmg3") => Hrfs.SPMG3
      case n if n.startsWith("gamma") => Hrfs.Gamma
      case other                      => fail(s"unmapped duration fixture '$other'")

  test("Evaluate under Trapezoid matches R evaluate.HRF across precisions"):
    // R's `.block_offsets_weights` is the trapezoid rule at the caller's
    // `precision`, so this is the mode that models R. It is pinned bit-for-bit;
    // the exact mode is a separate contract, checked below.
    HrfRParityFixtures.durations.foreach { f =>
      val hrf = fixtureHrf(f.name)
      val actual = Evaluate.doubles(
        hrf,
        f.grid,
        duration = f.duration,
        precision = f.precision,
        summate = f.summate,
        integration = Integration.Trapezoid
      )
      assertEquals(actual.rows, f.grid.length, s"${f.name} rows")
      assertEquals(actual.cols, f.nbasis, s"${f.name} cols")
      var i = 0
      while i < actual.rows do
        var j = 0
        while j < actual.cols do
          assertEqualsDouble(
            actual(i, j),
            f.values(i * f.nbasis + j),
            1e-9,
            s"${f.name} at t=${f.grid(i)} basis=$j"
          )
          j += 1
        i += 1
    }

  test("Evaluate under Exact is the limit R's trapezoid is converging to"):
    // Same fixtures, default (exact) mode: the closed form should sit where
    // R's rule is heading, so the gap must shrink as the fixture's own
    // precision refines and vanish at the finest one.
    val gaps = Vector("spmg1_dur4_p1.0", "spmg1_dur4_p0.5", "spmg1_dur4_p0.1", "spmg1_dur4_p0.05").map { name =>
      val f = HrfRParityFixtures.duration(name)
      val exact = Evaluate.doubles(fixtureHrf(f.name), f.grid, duration = f.duration, summate = f.summate)
      var worst = 0.0
      var i = 0
      while i < f.values.length do
        worst = math.max(worst, math.abs(exact.data(i) - f.values(i)))
        i += 1
      worst
    }
    gaps.zip(gaps.tail).foreach { (coarse, fine) =>
      assert(fine < coarse, s"R's rule is not approaching the closed form ($coarse -> $fine)")
    }
    assert(
      gaps.last < 5e-4,
      s"at R's finest precision (0.05) the closed form is still ${gaps.last} away"
    )

  test("Evaluate box response converges as precision shrinks"):
    val peaks = Vector(1.0, 0.5, 0.25, 0.1, 0.05, 0.01).map { p =>
      peak(Evaluate.doubles(Hrfs.SPMG1, grid, duration = 4.0, precision = p, integration = Integration.Trapezoid))
    }
    // Successive refinements must approach a limit, not grow without bound.
    val finest = peaks.last
    peaks.zip(Vector(1.0, 0.5, 0.25, 0.1, 0.05, 0.01)).foreach { case (v, p) =>
      assert(
        math.abs(v - finest) < 0.1,
        s"peak $v at precision $p is far from the refined value $finest — quadrature is not converging"
      )
    }
    // And the limit is the integral, not a bin count: R's own converged value
    // for this kernel, duration and grid.
    val rPeak = HrfRParityFixtures.duration("spmg1_dur4_p0.05").values.map(math.abs).max
    assertEqualsDouble(finest, rPeak, 0.01, "converged 4s-epoch SPMG1 peak should match R")

  test("block decorator under Trapezoid matches R gen_hrf(width=)"):
    // R: gen_hrf(HRF_SPMG1, width = 4, precision = p) peaks at
    // 6.06364 / 6.12922 / 6.15001 for p = 1.0 / 0.5 / 0.1.
    val expected = Map(1.0 -> 6.06364, 0.5 -> 6.12922, 0.1 -> 6.15001)
    expected.foreach { case (p, want) =>
      val h = Hrfs.SPMG1.block(Seconds(4.0), precision = Seconds(p), integration = Integration.Trapezoid)
      assertEqualsDouble(
        peak(h.evaluateDoubles(grid, integration = Integration.Trapezoid)),
        want,
        1e-3,
        s"block(width=4, precision=$p) should match R gen_hrf"
      )
    }

  test("block decorator under Exact is precision-free and matches R's limit"):
    val peaks = Vector(1.0, 0.5, 0.1).map { p =>
      peak(Hrfs.SPMG1.block(Seconds(4.0), precision = Seconds(p)).evaluateDoubles(grid))
    }
    peaks.tail.foreach { v =>
      assertEqualsDouble(v, peaks.head, 0.0, "exact block should not depend on precision")
    }
    // R's own sequence 6.06364 -> 6.12922 -> 6.15001 is heading here.
    assertEqualsDouble(peaks.head, 6.15001, 2e-3, "exact block should sit at R's refined value")

  test("regressor epoch amplitude no longer depends on precision"):
    val g = (0 until 60).map(_ * 2.0)
    val reg = Regressor(Seq(10.0, 40.0), Hrfs.SPMG1, duration = Seq(4.0))
    val peaks = Vector(0.5, 0.25, 0.1, 0.05).map { p =>
      peak(reg.evaluate(g, precision = p, method = Regressor.EvalMethod.Loop))
    }
    peaks.zip(peaks.tail).foreach { (coarse, fine) =>
      assert(
        math.abs(coarse - fine) < 0.05,
        s"epoch peak moved from $coarse to $fine when precision halved"
      )
    }

  test("unit-mass box is the duration average of the unit-height box"):
    val height = Evaluate.doubles(Hrfs.SPMG1, grid, duration = 4.0, precision = 0.05, summate = true)
    val mass = Evaluate.doubles(Hrfs.SPMG1, grid, duration = 4.0, precision = 0.05, summate = false)
    var i = 0
    while i < height.rows do
      assertEqualsDouble(mass(i, 0), height(i, 0) / 4.0, 1e-9, s"unit-mass box at t=${grid(i)}")
      i += 1

  test("unit-mass box keeps its integral as duration grows, unit-height does not"):
    val fine = (0 to 800).map(_ * 0.05)
    def area(duration: Double, summate: Boolean): Double =
      Evaluate.doubles(Hrfs.SPMG1, fine, duration = duration, precision = 0.05, summate = summate).data.sum * 0.05
    val massAreas = Vector(1.0, 4.0, 12.0).map(area(_, summate = false))
    massAreas.zip(massAreas.tail).foreach { (a, b) =>
      assertEqualsDouble(b, a, 1e-3, "unit-mass box area should not depend on duration")
    }
    val heightAreas = Vector(1.0, 4.0, 12.0).map(area(_, summate = true))
    assert(
      heightAreas(1) > heightAreas(0) && heightAreas(2) > heightAreas(1),
      "unit-height box area should grow with duration"
    )

  test("impulse response is exactly the kernel, independent of precision"):
    Vector(1.0, 0.33, 0.05).foreach { p =>
      val m = Evaluate.doubles(Hrfs.SPMG1, grid, duration = 0.0, precision = p)
      grid.indices.foreach { i =>
        assertEqualsDouble(
          m(i, 0),
          Hrfs.SPMG1(Lag(grid(i))).data(0),
          0.0,
          s"impulse response perturbed by precision=$p at t=${grid(i)}"
        )
      }
    }
