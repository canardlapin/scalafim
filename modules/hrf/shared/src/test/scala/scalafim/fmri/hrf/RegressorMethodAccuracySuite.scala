package scalafim.fmri.hrf

import scalafim.fmri.hrf.regressor.{Regressor, evaluate}

/** What each `EvalMethod` actually costs in accuracy.
  *
  * `Conv` and `FFT` project the neural drive onto a linear microtime basis;
  * `Loop` evaluates the kernel at the true onset. The projection removes the
  * old first-order onset snapping, while a small second-order discretization
  * gap remains and converges with precision.
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

  private val triangularHrf =
    Hrf.scalar(
      name = "triangle",
      span = Seconds(1.0),
      support = Support.Compact(Seconds(1.0))
    )(lag => 1.0 - lag.value)

  test("Conv and FFT are the same algorithm and agree to machine precision"):
    val reg = Regressor(Seq(10.0, 23.5, 44.2), Hrfs.SPMG1)
    Seq(0.33, 0.1, 0.05).foreach { p =>
      val conv = reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Conv)
      val fft = reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.FFT)
      val gap = maxAbsDiff(conv, fft)
      assert(gap < 1e-10, s"Conv and FFT diverged by $gap at precision $p")
    }

  test("grid-aligned impulses agree with the exact loop"):
    // Onsets on the microtime grid: the convolution paths and the exact loop
    // should then agree closely, isolating sub-bin projection as the source of the gap.
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

  test("sub-bin impulse projection matches the R reference"):
    // The independent R convolution uses the same linear-hat projection. At
    // p=0.33 its measured gap from R's exact loop is 0.007461368714330785;
    // the former snapped representation had a 0.210628 gap.
    val reg = Regressor(Seq(10.0, 23.5, 44.2), Hrfs.SPMG1)
    def gapAt(p: Double): Double =
      maxAbsDiff(
        reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Conv),
        reg.evaluate(grid, precision = p, method = Regressor.EvalMethod.Loop)
      )

    assertEqualsDouble(gapAt(0.33), 0.007461368714330785, 1e-12, "the R-matching projection gap")
    Seq(0.1, 0.05, 0.01).foreach { p =>
      assertEqualsDouble(gapAt(p), 0.0, 1e-12, s"onsets are grid-representable at $p, so the gap should vanish")
    }

  test("the worst case over onset phases does shrink with precision"):
    // Sweep the onset across a whole microtime step and take the worst gap.
    // The independent R hat-projection envelope is second order in `dt`.
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
    val rEnvelope = Seq(0.05455397, 0.01336092, 0.003128416, 0.0004815086, 0.0001212584)
    envelope.zip(rEnvelope).zipWithIndex.foreach { case ((actual, expected), index) =>
      assertEqualsDouble(actual, expected, 1e-8, s"R projection envelope at index $index")
    }

  test("every method renders an epoch design consistently at fine precision"):
    val reg = Regressor(Seq(10.0, 40.0), Hrfs.SPMG1, duration = Seq(6.0))
    val loop = reg.evaluate(grid, precision = 0.01, method = Regressor.EvalMethod.Loop)
    Seq(Regressor.EvalMethod.Conv, Regressor.EvalMethod.FFT).foreach { m =>
      val other = reg.evaluate(grid, precision = 0.01, method = m)
      val gap = maxAbsDiff(loop, other) / peak(loop)
      assert(gap < 0.02, s"$m differs from the exact loop by ${gap * 100}% of peak")
    }

  test("a block overlapping a one-point grid is retained by every method"):
    val reg = Regressor(Seq(0.0), triangularHrf, duration = Seq(2.0))
    // At lag 2.5 only u in [1.5, 2] contributes:
    // integral_1.5^2 (u - 1.5) du = 0.125.
    val expected = 0.125

    Regressor.EvalMethod.values.foreach { method =>
      val actual = reg.evaluate(Seq(2.5), precision = 0.01, method = method)
      assertEqualsDouble(actual(0, 0), expected, 1e-6, s"$method discarded the overlapping block")
    }

  test("Loop support includes the duration tail of an already-retained block"):
    val reg = Regressor(Seq(0.0), triangularHrf, duration = Seq(2.0))
    val actual = reg.evaluate(Seq(0.0, 2.5), precision = 0.01, method = Regressor.EvalMethod.Loop)

    assertEqualsDouble(actual(1, 0), 0.125, 1e-6)
