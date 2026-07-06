package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.regressor.Regressor

class HrfSuite extends munit.FunSuite:

  test("SPMG1 has expected peak time") {
    val hrf = Hrfs.SPMG1
    val times = (0 to 60).map(_ * 0.5)
    val vals = hrf.evalScalar(times.map(_.s))
    val peakIdx = vals.zipWithIndex.maxBy(_._1)._2
    val peakTime = times(peakIdx)
    assert(peakTime >= 4.0 && peakTime <= 7.0)
  }

  test("SPMG2 returns 2-column basis") {
    val hrf = Hrfs.SPMG2
    val times = (0 to 60).map(_ * 0.5)
    val mat = hrf.evalDoubles(times)
    assertEquals(mat.rows, times.length)
    assertEquals(mat.cols, 2)
  }

  test("FFT regressor evaluation matches conv") {
    val box = Hrfs.boxcar(1.0.s)
    val reg = Regressor(Seq(0.0, 2.0), box, duration = Seq(0.0), amplitude = Seq(1.0), span = Some(1.0))
    val grid = (0 to 8).map(_ * 0.5)
    val conv = Regressor.evaluate(reg, grid, precision = 0.1, method = Regressor.EvalMethod.Conv)
    val fft  = Regressor.evaluate(reg, grid, precision = 0.1, method = Regressor.EvalMethod.FFT)
    val maxDiff = conv.data.zip(fft.data).map((a, b) => math.abs(a - b)).max
    assert(maxDiff < 1e-6)
  }
