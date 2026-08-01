package scalafim.fmri.hrf

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.hrf.TestUtils.*

class ToeplitzSuite extends munit.FunSuite:

  private def manualToeplitz(col: Array[Double], row: Array[Double]): Mat =
    val nr = col.length
    val nc = row.length
    val out = Array.fill(nr * nc)(0.0)
    var i = 0
    while i < nr do
      var j = 0
      while j < nc do
        out(i * nc + j) =
          if i >= j then col(i - j) else row(j - i)
        j += 1
      i += 1
    Mat.unsafe(nr, nc, out)

  test("toeplitz matrix matches manual") {
    val box = Hrfs.boxcar(1.0.s)
    val time = Seq(0.0, 1.0, 2.0)
    val len = 5
    val H = Toeplitz.matrix(box, time, len)
    val hreg = box.evalScalar(time.map(Lag(_)))
    val col = hreg ++ Array.fill(len - hreg.length)(0.0)
    val row = Array(hreg.head) ++ Array.fill(len - 1)(0.0)
    val expected = manualToeplitz(col, row)
    assert(H.approxEquals(expected))
  }
