package scalafim.fmri.design.hrf

import scalafim.fmri.design.data.DataTable
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.HrfCombinators.*

object HrfGenerators:

  def duration(
      base: Hrf = Hrfs.SPMG1,
      minDuration: Double = 0.0,
      precision: Seconds = 0.1.s,
      summate: Boolean = true
  ): HrfFun =
    require(minDuration.isFinite && minDuration >= 0.0, "`minDuration` must be finite and >= 0")
    require(precision.value.isFinite && precision.value > 0.0, "`precision` must be finite and > 0")
    (d: DataTable) =>
      HrfSelection.perEvent(d.doubles("duration").map { x =>
        val width = math.max(x, minDuration)
        if width <= 0.0 then base
        else base.block(
          width = width.s,
          precision = precision,
          halfLife = Double.PositiveInfinity,
          summate = summate,
          normalize = true
        )
      })

  def boxcar(normalize: Boolean = true, minDuration: Double = 0.1): HrfFun =
    require(minDuration.isFinite && minDuration > 0.0, "`minDuration` must be finite and > 0")
    (d: DataTable) =>
      val dur = d.doubles("duration")
      HrfSelection.perEvent(dur.map { x =>
        val w = math.max(x, minDuration)
        Hrfs.boxcar(width = w.s, normalize = normalize)
      })

  def weighted(
      timesCol: String = "sub_times",
      weightsCol: String = "sub_weights",
      relative: Boolean = false,
      method: String = "constant",
      normalize: Boolean = false
  ): HrfFun =
    val m =
      method.trim.toLowerCase match
        case "constant" => Hrfs.WeightedMethod.Constant
        case "linear"   => Hrfs.WeightedMethod.Linear
        case other      => throw new IllegalArgumentException(s"weighted: unknown method '$other' (expected 'constant' or 'linear')")

    (d: DataTable) =>
      val times = d.doubleLists(timesCol)
      val weights = d.doubleLists(weightsCol)
      val onset = d.doubles("onset")

      require(times.length == d.nrows && weights.length == d.nrows && onset.length == d.nrows, "weighted: column length mismatch")

      val out = Vector.newBuilder[Hrf]
      var i = 0
      while i < d.nrows do
        val t0 = times(i)
        val w0 = weights(i)
        val relT = if relative then t0 else t0.map(_ - onset(i))
        val relSec: Vector[Seconds] = relT.map(Seconds(_))
        out += Hrfs.weighted(weights = w0.toVector, times = Some(relSec), method = m, normalize = normalize)
        i += 1
      HrfSelection.perEvent(out.result())
