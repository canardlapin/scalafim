package scalafim.fmri.hrf

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.TestUtils.*

class DerivSuite extends munit.FunSuite:

  test("Deriv matches analytic SPMG1 derivative") {
    val t = (0 to 40).map(_ * 0.5)
    val d = Deriv.doubles(Hrfs.SPMG1, t)
    val expected = t.map(x => HrfFunctions.spmg1Deriv(Lag(x))).toArray
    assert(d.cols == 1)
    assert(TestUtils.maxAbsDiff(d.data, expected) < 1e-9)
    assertEquals(d.data.head, 0.0)
  }

  test("Deriv for SPMG2 has 2 columns matching 1st/2nd deriv") {
    val t = (0 to 40).map(_ * 0.5)
    val d = Deriv.doubles(Hrfs.SPMG2, t)
    assertEquals(d.cols, 2)
    val col1 = t.map(x => HrfFunctions.spmg1Deriv(Lag(x))).toArray
    val col2 = t.map(x => HrfFunctions.spmg1SecondDeriv(Lag(x))).toArray
    val got1 = d.col(0).data
    val got2 = d.col(1).data
    assert(TestUtils.maxAbsDiff(got1, col1) < 1e-6)
    assert(TestUtils.maxAbsDiff(got2, col2) < 1e-6)
  }

  test("Deriv for SPMG3 has 3 columns") {
    val t = (0 to 40).map(_ * 0.5)
    val d = Deriv.doubles(Hrfs.SPMG3, t)
    assertEquals(d.cols, 3)
  }

  test("Numeric deriv works for Gaussian") {
    val t = (0 to 20).map(_ * 0.5)
    val d = Deriv.doubles(Hrfs.Gaussian, t)
    assertEquals(d.cols, 1)
    assertEquals(d.rows, t.length)
  }
