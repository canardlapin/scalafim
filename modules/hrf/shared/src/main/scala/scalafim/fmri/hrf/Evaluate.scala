package scalafim.fmri.hrf

import scalafim.fmri.hrf.{Hrf, Seconds, s}
import scalafim.fmri.hrf.linalg.{Mat, Vec}

object Evaluate:

  def apply(
      hrf: Hrf,
      grid: Seq[Seconds],
      amplitude: Double = 1.0,
      duration: Seconds = 0.0.s,
      precision: Seconds = 0.2.s,
      summate: Boolean = true,
      normalize: Boolean = false
  ): Mat =
    require(grid.nonEmpty, "`grid` must be non-empty")
    require(grid.forall(_.value.isFinite), "`grid` must be finite")
    require(precision.value > 0.0, "`precision` must be > 0")
    val nb = hrf.nbasis
    val base: Seconds => Vec = t => hrf(t) * amplitude

    val out =
      if duration.value < precision.value then
        val vals = grid.map(base).toVector
        val data = new Array[Double](vals.size * nb)
        var i = 0
        while i < vals.size do
          System.arraycopy(vals(i).data, 0, data, i * nb, nb)
          i += 1
        Mat.unsafe(vals.size, nb, data)
      else
        val nOffs = math.floor(duration.value / precision.value).toInt + 1
        val offs = Array.tabulate(nOffs)(i => i * precision.value)
        val data = Array.fill(grid.size * nb)(0.0)
        var gi = 0
        while gi < grid.size do
          val t = grid(gi).value
          if nb == 1 && !summate then
            var maxv = Double.NegativeInfinity
            var k = 0
            while k < nOffs do
              val v = base(Seconds(t - offs(k))).data(0)
              if v > maxv then maxv = v
              k += 1
            data(gi) = maxv
          else
            var k = 0
            while k < nOffs do
              val v = base(Seconds(t - offs(k))).data
              var j = 0
              while j < nb do
                data(gi * nb + j) += v(j)
                j += 1
              k += 1
          gi += 1
        Mat.unsafe(grid.size, nb, data)

    if !normalize then out
    else
      val scaled = out.data.clone
      if nb == 1 then
        val maxAbs = scaled.map(math.abs).maxOption.getOrElse(1.0)
        val s0 = if maxAbs > 1e-10 then maxAbs else 1.0
        var i = 0
        while i < scaled.length do
          scaled(i) /= s0
          i += 1
      else
        var j = 0
        while j < nb do
          var m = 0.0
          var i = 0
          while i < out.rows do
            val a = math.abs(out(i, j))
            if a > m then m = a
            i += 1
          val s0 = if m > 1e-10 then m else 1.0
          i = 0
          while i < out.rows do
            scaled(i * nb + j) /= s0
            i += 1
          j += 1
      Mat.unsafe(out.rows, out.cols, scaled)

  def doubles(
      hrf: Hrf,
      grid: Seq[Double],
      amplitude: Double = 1.0,
      duration: Double = 0.0,
      precision: Double = 0.2,
      summate: Boolean = true,
      normalize: Boolean = false
  ): Mat =
    apply(hrf, grid.map(_.s), amplitude, duration.s, precision.s, summate, normalize)
