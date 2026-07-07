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

    hrf.descriptor.derivative match
      case DerivativePolicy.Spmg(params, columns) if columns.value == nb =>
        spmg(params, columns, times, eps)
      case _ =>
        numeric(hrf, times, eps)

  private def spmg(params: SpmgParams, columns: BasisCount, times: Seq[Seconds], eps: Seconds): Mat =
    val nb = columns.value
    if nb == 1 then
      val out = times.map(t => HrfFunctions.spmg1Deriv(t, params.p1, params.p2, params.a1)).toArray
      Mat.unsafe(out.length, 1, out)
    else if nb == 2 then
      val out = new Array[Double](times.length * 2)
      var i = 0
      while i < times.length do
        val t = times(i)
        out(i * 2) = HrfFunctions.spmg1Deriv(t, params.p1, params.p2, params.a1)
        out(i * 2 + 1) = HrfFunctions.spmg1SecondDeriv(t, params.p1, params.p2, params.a1)
        i += 1
      Mat.unsafe(times.length, 2, out)
    else
      val out = new Array[Double](times.length * nb)
      val h = eps.value
      var i = 0
      while i < times.length do
        val t = times(i)
        out(i * nb) = HrfFunctions.spmg1Deriv(t, params.p1, params.p2, params.a1)
        out(i * nb + 1) = HrfFunctions.spmg1SecondDeriv(t, params.p1, params.p2, params.a1)
        val tp = Seconds(t.value + h)
        val tm = Seconds(t.value - h)
        val fp = HrfFunctions.spmg1SecondDeriv(tp, params.p1, params.p2, params.a1)
        val fm = HrfFunctions.spmg1SecondDeriv(tm, params.p1, params.p2, params.a1)
        out(i * nb + 2) = (fp - fm) / (2.0 * h)
        i += 1
      Mat.unsafe(times.length, nb, out)

  private def numeric(hrf: Hrf, times: Seq[Seconds], eps: Seconds): Mat =
    val nb = hrf.nbasis
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
