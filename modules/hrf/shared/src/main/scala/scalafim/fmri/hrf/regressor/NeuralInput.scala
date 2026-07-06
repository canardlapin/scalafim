package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.s

object NeuralInput:
  def apply(
      reg: Regressor,
      from: Double = 0.0,
      to: Option[Double] = None,
      resolution: Double = 0.33
  ): (Array[Double], Array[Double]) =
    val end = to.getOrElse {
      if reg.onsets.isEmpty then from
      else reg.onsets.zip(reg.durations).map { case (o, d) => o.value + d.value }.max + 10.0
    }
    val n = math.floor((end - from) / resolution).toInt + 1
    val time = Array.tabulate(n)(i => from + i * resolution)
    val neural = Array.fill(n)(0.0)

    var e = 0
    while e < reg.onsets.length do
      val on = reg.onsets(e).value
      val dur = reg.durations(e).value
      val amp = reg.amplitudes(e)
      val startBin = math.floor((on - from) / resolution).toInt
      if dur > 0.0 then
        val endBin = math.floor((on + dur - from) / resolution).toInt
        var b = startBin
        while b <= endBin && b < n do
          if b >= 0 then neural(b) += amp
          b += 1
      else if startBin >= 0 && startBin < n then
        neural(startBin) += amp
      e += 1

    (time, neural)
