package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}
import scalafim.fmri.hrf.linalg.Mat

object Deriv:

  def apply(
      hrf: Hrf,
      times: Seq[Seconds],
      eps: Seconds = 1e-4.s
  ): Mat =
    val nb = hrf.nbasis
    if times.isEmpty then return Mat.zeros(0, nb)

    val lower = hrf.name.toLowerCase

    if lower.startsWith("spmg1") && nb == 1 then
      val params = hrf.params
      val P1 = params.get("P1").collect { case d: Double => d }.getOrElse(5.0)
      val P2 = params.get("P2").collect { case d: Double => d }.getOrElse(15.0)
      val A1 = params.get("A1").collect { case d: Double => d }.getOrElse(0.0833)
      val out = times.map(t => HrfFunctions.spmg1Deriv(t, P1, P2, A1)).toArray
      Mat.unsafe(out.length, 1, out)

    else if lower.startsWith("spmg2") && nb == 2 then
      val params = hrf.params
      val P1 = params.get("P1").collect { case d: Double => d }.getOrElse(5.0)
      val P2 = params.get("P2").collect { case d: Double => d }.getOrElse(15.0)
      val A1 = params.get("A1").collect { case d: Double => d }.getOrElse(0.0833)
      val out = new Array[Double](times.length * 2)
      var i = 0
      while i < times.length do
        val t = times(i)
        out(i * 2) = HrfFunctions.spmg1Deriv(t, P1, P2, A1)
        out(i * 2 + 1) = HrfFunctions.spmg1SecondDeriv(t, P1, P2, A1)
        i += 1
      Mat.unsafe(times.length, 2, out)

    else if lower.startsWith("spmg3") && nb == 3 then
      val params = hrf.params
      val P1 = params.get("P1").collect { case d: Double => d }.getOrElse(5.0)
      val P2 = params.get("P2").collect { case d: Double => d }.getOrElse(15.0)
      val A1 = params.get("A1").collect { case d: Double => d }.getOrElse(0.0833)
      val out = new Array[Double](times.length * 3)
      val h = eps.value
      var i = 0
      while i < times.length do
        val t = times(i)
        out(i * 3) = HrfFunctions.spmg1Deriv(t, P1, P2, A1)
        out(i * 3 + 1) = HrfFunctions.spmg1SecondDeriv(t, P1, P2, A1)
        val tp = Seconds(t.value + h)
        val tm = Seconds(t.value - h)
        val fp = HrfFunctions.spmg1SecondDeriv(tp, P1, P2, A1)
        val fm = HrfFunctions.spmg1SecondDeriv(tm, P1, P2, A1)
        out(i * 3 + 2) = (fp - fm) / (2.0 * h)
        i += 1
      Mat.unsafe(times.length, 3, out)

    else
      val h = eps.value
      val out = new Array[Double](times.length * nb)
      var i = 0
      while i < times.length do
        val t = times(i).value
        val vp = hrf(Seconds(t + h)).data
        val vm = hrf(Seconds(t - h)).data
        var j = 0
        while j < nb do
          out(i * nb + j) = (vp(j) - vm(j)) / (2.0 * h)
          j += 1
        i += 1
      Mat.unsafe(times.length, nb, out)

  def doubles(hrf: Hrf, times: Seq[Double], eps: Double = 1e-4): Mat =
    apply(hrf, times.map(_.s), eps.s)
