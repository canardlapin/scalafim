package scalafim.fmri.hrf

import scalafim.fmri.hrf.regressor.{Regressor, evaluate}

/** What each `EvalMethod` actually costs in accuracy.
  *
  * `Conv` and `FFT` build the neural drive on a microtime grid, so an onset
  * that does not land on that grid is quantized to it. `Loop` evaluates the
  * kernel at the true onset. The three therefore do not agree, and the gap is
  * not a bug — it is the discretization the convolution paths are built on.
  * `docs/plans/hrf-hardening.md` §3.3 records that R has the same gap to six
  * digits.
  *
  * These tests pin the shape of that gap so a future change to the default
  * method is made on measured grounds.
  */
class RegressorMethodAccuracySuite extends munit.FunSuite:

  private val grid = (0 until 60).map(_ * 2.0)

  private def maxAbsDiff(a: Mat, b: Mat): Double =
    var worst = 0.0
    var i = 0
    while i < a.data.length do
      worst = math.max(worst, math.abs(a.data(i) - b.data(i)))
      i += 1
    worst

  private def peak(m: Mat): Double = m.data.map(math.abs).max

  test("Conv and FFT are the same algorithm and agree to machine precision"):
    val reg = Regressor(Seq(10.0, 23.5, 44.2), Hrfs.SPMG1)
    Seq(0.33, 0.1, 0.05).foreach { p =>
      val conv = reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Conv)
      val fft = reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.FFT)
      val gap = maxAbsDiff(conv, fft)
      assert(gap < 1e-10, s"Conv and FFT diverged by $gap at precision $p")
    }

  test("grid-aligned onsets leave nothing for the microtime grid to quantize"):
    // Onsets on the microtime grid: the convolution paths and the exact loop
    // should then agree closely, isolating quantization as the cause of the gap.
    val reg = Regressor(Seq(10.0, 30.0, 60.0), Hrfs.SPMG1)
    val conv = reg.evaluate(grid, precision = 0.05, method = Regressor.EvalMethod.Conv)
    val loop = reg.evaluate(grid, precision = 0.05, method = Regressor.EvalMethod.Loop)
    val gap = maxAbsDiff(conv, loop)
    assert(gap < 5e-3, s"aligned onsets still differ by $gap")

  test("Loop is exact for impulses: its answer does not move with precision"):
    val reg = Regressor(Seq(10.0, 23.5, 44.2), Hrfs.SPMG1)
    val reference = reg.evaluate(grid, precision = 0.001, method = Regressor.EvalMethod.Loop)
    Seq(1.0, 0.5, 0.33, 0.1, 0.01).foreach { p =>
      val loop = reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Loop)
      assertEqualsDouble(maxAbsDiff(loop, reference), 0.0, 0.0, s"Loop moved at precision $p")
    }

  test("the Conv/Loop gap is onset quantization, not a convergence error"):
    // Measured, not assumed: the gap is *not* monotone in precision. It is the
    // residual from snapping each onset to the microtime grid, so it vanishes
    // whenever the onsets happen to be representable at that step and jumps
    // back when they are not.
    //
    //   p=1.00 -> 0.340043      p=0.10 -> 0
    //   p=0.50 -> 0.100568      p=0.05 -> 0
    //   p=0.33 -> 0.210628      p=0.01 -> 0
    //   p=0.25 -> 0.100568
    //
    // 23.5 and 44.2 land exactly on the grid at 0.1/0.05/0.01 and not at
    // 0.33/0.25. R has the same 0.210628 at p=0.33 — see hrf-hardening.md §3.3.
    val reg = Regressor(Seq(10.0, 23.5, 44.2), Hrfs.SPMG1)
    def gapAt(p: Double): Double =
      maxAbsDiff(
        reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Conv),
        reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Loop)
      )

    assertEqualsDouble(gapAt(0.33), 0.210628236002806, 1e-9, "the documented R-matching gap")
    Seq(0.1, 0.05, 0.01).foreach { p =>
      assertEqualsDouble(gapAt(p), 0.0, 1e-12, s"onsets are grid-representable at $p, so the gap should vanish")
    }
    assert(gapAt(0.25) > gapAt(0.5) - 1e-12, "gap is not monotone in precision")

  test("the worst case over onset phases does shrink with precision"):
    // Averaging out the alignment luck above: sweep the onset across a whole
    // microtime step and take the worst gap. That envelope is O(dt).
    def worstCase(p: Double): Double =
      (0 until 8).map { k =>
        val reg = Regressor(Seq(10.0 + k * p / 8.0), Hrfs.SPMG1)
        maxAbsDiff(
          reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Conv),
          reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Loop)
        )
      }.max

    val envelope = Seq(1.0, 0.5, 0.25, 0.1, 0.05).map(worstCase)
    envelope.zip(envelope.tail).foreach { (coarse, fine) =>
      assert(fine < coarse, s"worst-case quantization gap did not shrink ($coarse -> $fine)")
    }
    val peakValue = peak(Regressor(Seq(10.0), Hrfs.SPMG1).evaluate(grid, method = Regressor.EvalMethod.Loop))
    assert(envelope.head / peakValue > 0.05, "expected a material worst-case gap at precision 1.0")
    assert(envelope.last / peakValue < 0.02, s"worst case at 0.05 is ${envelope.last / peakValue} of peak")

  test("every method renders an epoch design consistently at fine precision"):
    val reg = Regressor(Seq(10.0, 40.0), Hrfs.SPMG1, duration = Seq(6.0))
    val loop = reg.evaluate(grid, precision = 0.01, method = Regressor.EvalMethod.Loop)
    Seq(Regressor.EvalMethod.Conv, Regressor.EvalMethod.FFT).foreach { m =>
      val other = reg.evaluate(grid, precision = 0.01, method = m)
      val gap = maxAbsDiff(loop, other) / peak(loop)
      assert(gap < 0.02, s"$m differs from the exact loop by ${gap * 100}% of peak")
    }
