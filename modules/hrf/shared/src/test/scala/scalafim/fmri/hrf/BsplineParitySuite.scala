package scalafim.fmri.hrf

import scalafim.fmri.hrf.HrfFunctions

class BsplineParitySuite extends munit.FunSuite:

  test("bsplineBasis matches R hrf_bspline for non-integer span") {
    val span = Seconds(24.5)
    val times: Seq[Seconds] = Seq(-1.0, 0.0, 5.0, 12.0, 24.0, 30.0).map(Seconds(_))
    val expected = Vector(
      Array(0.0, 0.0, 0.0, 0.0, 0.0),
      Array(0.0, 0.0, 0.0, 0.0, 0.0),
      Array(0.54443359375, 0.362972337372449, 0.039859693877551, 0.0, 0.0),
      Array(0.03125, 0.478335652442795, 0.461029590899721, 0.0293847566574839, 0.0),
      Array(0.0, 3.63781876386918e-05, 0.00515234452231711, 0.16110478431223, 0.833706492977814),
      Array(0.0, 0.0, 0.0, 0.0, 0.0)
    )

    times.zip(expected).foreach { case (t, exp) =>
      val got = HrfFunctions.bsplineBasis(t, span = span, nBasis = 5, degree = 3)
      assertEquals(got.length, exp.length)
      assert(TestUtils.maxAbsDiff(got, exp) < 1e-9)
    }
  }

  test("bsplineBasis with N < degree+1 matches R minimum-basis behavior") {
    val span = 24.0.s
    val times: Seq[Seconds] = Seq(0.0, 5.0, 12.0, 24.0).map(Seconds(_))
    val expected = Vector(
      Array(1.0, 0.0, 0.0, 0.0),
      Array(0.496166087962963, 0.391710069444444, 0.103081597222222, 0.00904224537037037),
      Array(0.125, 0.375, 0.375, 0.125),
      Array(0.0, 0.0, 0.0, 1.0)
    )

    times.zip(expected).foreach { case (t, exp) =>
      val got = HrfFunctions.bsplineBasis(t, span = span, nBasis = 2, degree = 3)
      assertEquals(got.length, exp.length)
      assert(TestUtils.maxAbsDiff(got, exp) < 1e-9)
    }
  }
