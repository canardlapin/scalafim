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

  test("Seconds and refined time wrappers validate domain") {
    assert(Seconds.fromDouble(Double.NaN).isLeft)
    assert(NonNegativeSeconds(-0.1).isLeft)
    assert(PositiveSeconds(0.0).isLeft)

    val duration = NonNegativeSeconds(2.0).fold(err => fail(err.message), identity)
    assertEqualsDouble(duration.value, 2.0, 0.0)
  }

  test("typed HRF specs resolve kinds and scalar capability") {
    val spmg3 = HrfSpec
      .fromName("spmg3", span = 20.0.s)
      .flatMap(_.toHrf)
      .fold(err => fail(err.message), identity)
    assertEquals(spmg3.nbasis, 3)
    assertEqualsDouble(spmg3.span.value, 20.0, 0.0)

    val scalar = HrfSpec(HrfKind.Gamma).flatMap(_.toScalarHrf).fold(err => fail(err.message), identity)
    assertEqualsDouble(scalar.scalarAt(0.0.s), Hrfs.Gamma.evalScalar(Seq(0.0.s)).head, 1e-12)

    assert(HrfSpec(HrfKind.Spmg3).flatMap(_.toScalarHrf).isLeft)
    assert(Registry.getEither("not_a_real_hrf").isLeft)
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
