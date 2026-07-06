package scalafim.fmri.hrf

import scalafim.fmri.hrf.Hrf
import scalafim.fmri.hrf.linalg.Mat

object Penalty:

  private def binom(n: Int, k: Int): Int =
    if k < 0 || k > n then 0
    else if k == 0 || k == n then 1
    else
      var res = 1L
      var i = 1
      while i <= k do
        res = res * (n - (k - i)).toLong / i.toLong
        i += 1
      res.toInt

  private def roughnessPenalty(nBasis: Int, order: Int): Mat =
    if nBasis <= 1 then Mat.eye(nBasis)
    else if nBasis <= order then Mat.eye(nBasis)
    else
      val rows = nBasis - order
      val coeffs = Array.tabulate(order + 1) { j =>
        val sign = if ((order - j) % 2 == 0) 1.0 else -1.0
        sign * binom(order, j).toDouble
      }
      val out = Array.fill(nBasis * nBasis)(0.0)
      var r = 0
      while r < rows do
        var j0 = 0
        while j0 <= order do
          val p = r + j0
          val cp = coeffs(j0)
          var j1 = 0
          while j1 <= order do
            val q = r + j1
            out(p * nBasis + q) += cp * coeffs(j1)
            j1 += 1
          j0 += 1
        r += 1
      Mat.unsafe(nBasis, nBasis, out)

  def penaltyMatrix(
      hrf: Hrf,
      order: Int = 2,
      shrinkDeriv: Double = 2.0
  ): Mat =
    val nb = hrf.nbasis
    hrf.name.toLowerCase match
      case n if n.startsWith("spmg2") =>
        val out = Mat.eye(nb).data
        if nb >= 1 then out(0) = 0.0
        if nb >= 2 then out(1 * nb + 1) = shrinkDeriv
        Mat.unsafe(nb, nb, out)

      case n if n.startsWith("spmg3") =>
        val out = Mat.eye(nb).data
        if nb >= 1 then out(0) = 0.0
        if nb >= 2 then out(1 * nb + 1) = shrinkDeriv
        if nb >= 3 then out(2 * nb + 2) = shrinkDeriv
        Mat.unsafe(nb, nb, out)

      case n if n.startsWith("fir") || n.startsWith("bspline") || n.startsWith("tent") =>
        roughnessPenalty(nb, order)

      case n if n.startsWith("fourier") =>
        val out = Array.fill(nb * nb)(0.0)
        var k = 0
        while k < nb do
          val freq = (k / 2) + 1
          out(k * nb + k) = math.pow(freq.toDouble, order.toDouble)
          k += 1
        Mat.unsafe(nb, nb, out)

      case n if n.startsWith("daguerre") =>
        val out = Array.fill(nb * nb)(0.0)
        var k = 0
        while k < nb do
          out(k * nb + k) = math.pow(k.toDouble, 2.0)
          k += 1
        Mat.unsafe(nb, nb, out)

      case _ =>
        Mat.eye(nb)
