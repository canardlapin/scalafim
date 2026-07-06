package scalafim.fmri.hrf

import scalafim.fmri.hrf.Library
import scalafim.fmri.hrf.TestUtils.*

class LibraryParitySuite extends munit.FunSuite:

  private case class GaussParam(mean: Double, sd: Double)

  private def makeGauss(p: GaussParam, scale: Double): Hrf =
    Hrf.scalar(s"gauss_${p.mean}", span = 1.0.s) { t =>
      scale * HrfFunctions.gaussianPdf(t, p.mean, p.sd)
    }

  test("hrfLibrary forwards extra arguments") {
    val params = Seq(GaussParam(0.0, 1.0), GaussParam(2.0, 1.0))
    val lib = Library.hrfLibrary(params)(p => makeGauss(p, scale = 2.0))
    assertEquals(lib.nbasis, 2)
    val t = Seq(0.0, 1.0)
    val res = lib.evalDoubles(t)
    val expected = scalafim.fmri.hrf.linalg.Mat.fromRows(
      t.map { tt =>
        Seq(
          2.0 * HrfFunctions.gaussianPdf(tt.s, 0.0, 1.0),
          2.0 * HrfFunctions.gaussianPdf(tt.s, 2.0, 1.0)
        )
      }
    )
    assert(res.approxEquals(expected, tol = 1e-12))
  }

  test("hrfLibrary produces distinct basis functions") {
    val shapes = Seq(6.0, 8.0, 10.0)
    val rates = Seq(0.9, 1.0, 1.1)
    val params = for s <- shapes; r <- rates yield (s, r)
    val lib = Library.hrfLibrary(params)(p => Hrfs.gamma(p._1, p._2))
    assertEquals(lib.nbasis, params.size)
    val t = (0 to 20).map(_.toDouble).toVector
    val res = lib.evalDoubles(t)
    assertEquals(res.cols, params.size)

    val cols = (0 until res.cols).map(c => res.col(c).data)
    def corr(x: Array[Double], y: Array[Double]): Double =
      val mx = x.sum / x.length
      val my = y.sum / y.length
      val num = x.zip(y).map { case (xi, yi) => (xi - mx) * (yi - my) }.sum
      val denx = math.sqrt(x.map(xi => (xi - mx) * (xi - mx)).sum)
      val deny = math.sqrt(y.map(yi => (yi - my) * (yi - my)).sum)
      if denx == 0.0 || deny == 0.0 then 0.0 else num / (denx * deny)

    val corrVals =
      for
        i <- cols.indices
        j <- (i + 1) until cols.length
      yield math.abs(corr(cols(i), cols(j)))
    val meanAbsCorr = if corrVals.nonEmpty then corrVals.sum / corrVals.length else 0.0
    assert(meanAbsCorr < 0.99)
  }
