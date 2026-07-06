package scalafim.fmri.hrf

import scalafim.fmri.hrf.linalg.Mat

object TestUtils:

  extension (m: Mat)
    def approxEquals(other: Mat, tol: Double = 1e-6): Boolean =
      if m.rows != other.rows || m.cols != other.cols then false
      else
        m.data.zip(other.data).forall { case (a, b) => math.abs(a - b) <= tol }

  def maxAbsDiff(a: Array[Double], b: Array[Double]): Double =
    a.zip(b).map { case (x, y) => math.abs(x - y) }.maxOption.getOrElse(0.0)
