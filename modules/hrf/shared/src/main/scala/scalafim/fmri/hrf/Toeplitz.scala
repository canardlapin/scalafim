package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Lag}
import scalafim.fmri.hrf.linalg.Mat

object Toeplitz:
  /** @param time lag axis on which the kernel is sampled. */
  def matrix(hrf: Hrf, time: Seq[Double], len: Int): Mat =
    require(hrf.nbasis == 1, "toeplitz currently supports nbasis=1")
    val hreg = hrf.evalScalar(time.iterator.map(Lag(_)))
    require(len >= hreg.length, "`len` must be >= length(time)")
    val col = hreg ++ Array.fill(len - hreg.length)(0.0)
    val row = Array(hreg.headOption.getOrElse(0.0)) ++ Array.fill(len - 1)(0.0)
    val out = new Array[Double](len * len)
    var i = 0
    while i < len do
      var j = 0
      while j < len do
        val v =
          if i >= j then col(i - j)
          else row(j - i)
        out(i * len + j) = v
        j += 1
      i += 1
    Mat.unsafe(len, len, out)
