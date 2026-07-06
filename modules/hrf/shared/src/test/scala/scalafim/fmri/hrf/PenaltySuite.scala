package scalafim.fmri.hrf

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.TestUtils.*

class PenaltySuite extends munit.FunSuite:

  test("bspline penalty is roughness-based (not identity)") {
    val R = Penalty.penaltyMatrix(Hrfs.bspline())
    val eye = scalafim.fmri.hrf.linalg.Mat.eye(R.rows)
    assert(!R.approxEquals(eye))
  }

  test("SPMG3 penalty shrinks derivatives") {
    val R = Penalty.penaltyMatrix(Hrfs.SPMG3, shrinkDeriv = 4.0)
    assertEquals(R(0, 0), 0.0)
    assertEquals(R(1, 1), 4.0)
    assertEquals(R(2, 2), 4.0)
  }

  test("fourier penalty increases with frequency") {
    val f = Hrfs.fourier(nBasis = 4)
    val R = Penalty.penaltyMatrix(f)
    val diag = (0 until 4).map(i => R(i, i)).toVector
    assertEquals(diag, Vector(1.0, 1.0, 4.0, 4.0))
  }

  test("default penalty is identity") {
    val R = Penalty.penaltyMatrix(Hrfs.Gaussian)
    val eye = scalafim.fmri.hrf.linalg.Mat.eye(R.rows)
    assert(R.approxEquals(eye))
  }
