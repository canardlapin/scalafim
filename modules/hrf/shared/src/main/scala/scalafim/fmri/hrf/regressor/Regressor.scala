package scalafim.fmri.hrf.regressor

import cats.data.NonEmptyList
import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.linalg.{Mat, Vec, Fft}

sealed trait HrfAssignment:
  def nbasis: Int
  def span: Seconds
  def at(t: Seconds, eventIndex: Int): Vec

object HrfAssignment:
  final case class Shared(hrf: Hrf) extends HrfAssignment:
    def nbasis: Int = hrf.nbasis
    def span: Seconds = hrf.span
    def at(t: Seconds, eventIndex: Int): Vec = hrf(t)

  final case class PerEvent(hrfs: Vector[Hrf]) extends HrfAssignment:
    def nbasis: Int = if hrfs.isEmpty then 1 else hrfs.head.nbasis
    def span: Seconds = if hrfs.isEmpty then Seconds(0.0) else hrfs.map(_.span).max
    def at(t: Seconds, eventIndex: Int): Vec = hrfs(eventIndex)(t)

final case class Regressor(
    onsets: Vector[Seconds],
    durations: Vector[Seconds],
    amplitudes: Vector[Double],
    hrf: HrfAssignment,
    span: Seconds,
    summate: Boolean
):
  require(onsets.length == durations.length && onsets.length == amplitudes.length, "onsets/durations/amplitudes length mismatch")

object Regressor:

  enum EvalMethod:
    case Conv, FFT, Loop

  private def recycleOrError[A](xs: Seq[A], n: Int, name: String): Vector[A] =
    if xs.length == n then xs.toVector
    else if xs.length == 1 then Vector.fill(n)(xs.head)
    else throw new IllegalArgumentException(s"`$name` must have length 1 or $n, not ${xs.length}")

  def apply(
      onsets: Seq[Double],
      hrf: Hrf = Hrfs.SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): Regressor =
    span.foreach(s => require(s.isFinite && s > 0.0, "`span` must be finite and > 0"))
    val ons: Vector[Seconds] = onsets.map(Seconds(_)).toVector
    require(ons.forall(o => o.value >= 0.0 && o.value.isFinite), "`onsets` must be finite and non-negative")
    val durs: Vector[Seconds] = recycleOrError(duration.map(Seconds(_)), ons.length, "duration")
    val amps0 = recycleOrError(amplitude, ons.length, "amplitude")
    require(durs.forall(d => d.value >= 0.0 && d.value.isFinite), "`duration` must be finite and non-negative")
    require(amps0.forall(a => a.isFinite), "`amplitude` must be finite")

    val keep = ons.indices.filter(i => amps0(i) != 0.0)
    val onsF: Vector[Seconds] = keep.map(ons).toVector
    val dursF: Vector[Seconds] = keep.map(durs).toVector
    val ampsF = keep.map(amps0).toVector

    val finalSpan: Seconds = span.map(Seconds(_)).getOrElse(hrf.span)

    Regressor(
      onsets = onsF,
      durations = dursF,
      amplitudes = ampsF,
      hrf = HrfAssignment.Shared(hrf),
      span = finalSpan,
      summate = summate
    )

  def perEvent(
      onsets: Seq[Double],
      hrfs: Seq[Hrf],
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): Regressor =
    span.foreach(s => require(s.isFinite && s > 0.0, "`span` must be finite and > 0"))
    val ons: Vector[Seconds] = onsets.map(Seconds(_)).toVector
    val n = ons.length
    val hrs =
      if hrfs.length == 1 then Vector.fill(n)(hrfs.head)
      else
        require(hrfs.length == n, s"`hrf` list must have length 1 or $n")
        hrfs.toVector

    val durs: Vector[Seconds] = recycleOrError(duration.map(Seconds(_)), n, "duration")
    val amps0 = recycleOrError(amplitude, n, "amplitude")

    val keep = ons.indices.filter(i => amps0(i) != 0.0)
    val onsF: Vector[Seconds] = keep.map(ons).toVector
    val dursF: Vector[Seconds] = keep.map(durs).toVector
    val ampsF = keep.map(amps0).toVector
    val hrsF = keep.map(hrs).toVector

    val maxSpan: Seconds =
      span.map(Seconds(_)).getOrElse(
        if hrsF.nonEmpty then hrsF.map(_.span).max
      else hrs.map(_.span).maxOption.getOrElse(Seconds(0.0))
      )

    Regressor(
      onsets = onsF,
      durations = dursF,
      amplitudes = ampsF,
      hrf = HrfAssignment.PerEvent(hrsF),
      span = maxSpan,
      summate = summate
    )

  def evaluate(
      reg: Regressor,
      grid: Seq[Double],
      precision: Double = 0.33,
      method: EvalMethod = EvalMethod.Conv
  ): Mat =
    val dt = Seconds(precision)
    require(dt.value > 0.0, "`precision` must be > 0")
    require(grid.nonEmpty, "`grid` must be non-empty")
    require(grid.forall(_.isFinite), "`grid` must be finite")
    val gridSec: Array[Seconds] = grid.map(Seconds(_)).toArray
    val sorted: Array[Seconds] = gridSec.sortBy(_.value)(using Ordering.Double.TotalOrdering)

    val nb = reg.hrf.nbasis
    if reg.onsets.isEmpty then return Mat.zeros(sorted.length, nb)

    val onsetMin = sorted.head.value - reg.span.value
    val onsetMax = sorted.last.value
    val keepIdx = reg.onsets.indices.filter(i =>
      reg.onsets(i).value >= onsetMin && reg.onsets(i).value <= onsetMax
    )

    if keepIdx.isEmpty then Mat.zeros(sorted.length, nb)
    else
      val ons = keepIdx.map(reg.onsets).toVector
      val durs = keepIdx.map(reg.durations).toVector
      val amps = keepIdx.map(reg.amplitudes).toVector

      val hrfIsList = reg.hrf.isInstanceOf[HrfAssignment.PerEvent]
      method match
        case EvalMethod.Loop => evalLoop(reg.hrf, reg.span, sorted, ons, durs, amps, dt, reg.summate)
        case _ if hrfIsList =>
          evalLoop(reg.hrf, reg.span, sorted, ons, durs, amps, dt, reg.summate)
        case EvalMethod.Conv =>
          evalConv(reg.hrf.asInstanceOf[HrfAssignment.Shared].hrf, reg.span, sorted, ons, durs, amps, dt)
        case EvalMethod.FFT =>
          evalFft(reg.hrf.asInstanceOf[HrfAssignment.Shared].hrf, reg.span, sorted, ons, durs, amps, dt)

  private def evalHrfEvent(
      hrf: Hrf,
      relTimes: Array[Seconds],
      amplitude: Double,
      duration: Seconds,
      precision: Seconds,
      summate: Boolean
  ): Array[Array[Double]] =
    val nb = hrf.nbasis
    val out = Array.ofDim[Double](relTimes.length, nb)
    if duration.value < precision.value then
      var i = 0
      while i < relTimes.length do
        val v = hrf(relTimes(i)).data
        var j = 0
        while j < nb do
          out(i)(j) = amplitude * v(j)
          j += 1
        i += 1
    else
      val nOffs = math.floor(duration.value / precision.value).toInt + 1
      val offs = Array.tabulate(nOffs)(i => i * precision.value)
      var i = 0
      while i < relTimes.length do
        val t = relTimes(i).value
        if nb == 1 then
          var acc = 0.0
          var maxv = Double.NegativeInfinity
          var k = 0
          while k < nOffs do
            val v = hrf(Seconds(t - offs(k))).data(0) * amplitude
            acc += v
            if v > maxv then maxv = v
            k += 1
          out(i)(0) = if summate then acc else maxv
        else
          val acc = Array.fill(nb)(0.0)
          var k = 0
          while k < nOffs do
            val v = hrf(Seconds(t - offs(k))).data
            var j = 0
            while j < nb do
              acc(j) += amplitude * v(j)
              j += 1
            k += 1
          out(i) = acc
        i += 1
    out

  private def evalLoop(
      hrfAssign: HrfAssignment,
      span: Seconds,
      grid: Array[Seconds],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      amps: Vector[Double],
      precision: Seconds,
      summate: Boolean
  ): Mat =
    val nb = hrfAssign.nbasis
    val out = Array.fill(grid.length * nb)(0.0)
    var e = 0
    while e < onsets.length do
      val onset = onsets(e)
      val rel = grid.map(g => Seconds(g.value - onset.value))
      val validIdx = rel.indices.filter(i => rel(i).value >= 0.0 && rel(i).value <= span.value)
      if validIdx.nonEmpty then
        val relValid = validIdx.map(rel).toArray
        val hrf = hrfAssign match
          case HrfAssignment.Shared(h) => h
          case HrfAssignment.PerEvent(hs) => hs(e)
        val resp = evalHrfEvent(hrf, relValid, amps(e), durations(e), precision, summate)
        var i = 0
        while i < validIdx.length do
          val gIdx = validIdx(i)
          var j = 0
          while j < nb do
            out(gIdx * nb + j) += resp(i)(j)
            j += 1
          i += 1
      e += 1
    Mat.unsafe(grid.length, nb, out)

  private def buildImpulseTrain(
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      amplitudes: Vector[Double],
      t0: Double,
      t1: Double,
      dt: Double
  ): Array[Double] =
    val nBins = math.floor((t1 - t0) / dt).toInt + 1
    val diff = Array.fill(nBins + 1)(0.0)
    var i = 0
    while i < onsets.length do
      val on = onsets(i).value
      val dur = durations(i).value
      val amp = amplitudes(i)
      var a =
        if on <= t0 then 0
        else math.floor((on - t0) / dt).toInt
      if a >= nBins then
        i += 1
      else
        var b = math.floor((on + dur - t0) / dt).toInt
        if b >= nBins then b = nBins - 1
        if a <= b then
          diff(a) += amp
          diff(b + 1) -= amp
        i += 1
    val out = new Array[Double](nBins)
    var acc = 0.0
    i = 0
    while i < nBins do
      acc += diff(i)
      out(i) = acc
      i += 1
    out

  private def hrfFineMatrix(hrf: Hrf, span: Seconds, dt: Double): Array[Array[Double]] =
    val n = math.floor(span.value / dt).toInt + 1
    val nb = hrf.nbasis
    val out = Array.ofDim[Double](n, nb)
    var i = 0
    while i < n do
      val t = Seconds(i * dt)
      val v = hrf(t).data
      System.arraycopy(v, 0, out(i), 0, nb)
      i += 1
    out

  private def evalConv(
      hrf: Hrf,
      span: Seconds,
      grid: Array[Seconds],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      amps: Vector[Double],
      precision: Seconds
  ): Mat =
    val dt = precision.value
    val start = grid.head.value - span.value
    val maxDur = if durations.isEmpty then 0.0 else durations.map(_.value).max
    val lastOnset = if onsets.isEmpty then grid.last.value else onsets.map(_.value).max
    val end = math.max(grid.last.value, lastOnset + maxDur) + span.value

    val neural = buildImpulseTrain(onsets, durations, amps, start, end, dt)
    val hrfFine = hrfFineMatrix(hrf, span, dt)
    val nb = hrf.nbasis
    val nFine = neural.length

    val out = new Array[Double](grid.length * nb)
    var b = 0
    while b < nb do
      val hcol = hrfFine.map(_(b))
      val convFull = new Array[Double](nFine + hcol.length - 1)
      var i = 0
      while i < nFine do
        val ai = neural(i)
        if ai != 0.0 then
          var j = 0
          while j < hcol.length do
            convFull(i + j) += ai * hcol(j)
            j += 1
        i += 1
      val conv = convFull.take(nFine)

      var g = 0
      while g < grid.length do
        val pos = (grid(g).value - start) / dt
        val v =
          if pos <= 0.0 then conv(0)
          else if pos >= nFine - 1 then conv(nFine - 1)
          else
            val lo = math.floor(pos).toInt
            val alpha = pos - lo
            (1.0 - alpha) * conv(lo) + alpha * conv(lo + 1)
        out(g * nb + b) = v
        g += 1
      b += 1
    Mat.unsafe(grid.length, nb, out)

  private def evalFft(
      hrf: Hrf,
      span: Seconds,
      grid: Array[Seconds],
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      amps: Vector[Double],
      precision: Seconds
  ): Mat =
    val dt = precision.value
    val start = grid.head.value - span.value
    val maxDur = if durations.isEmpty then 0.0 else durations.map(_.value).max
    val lastOnset = if onsets.isEmpty then grid.last.value else onsets.map(_.value).max
    val end = math.max(grid.last.value, lastOnset + maxDur) + span.value

    val neural = buildImpulseTrain(onsets, durations, amps, start, end, dt)
    val hrfFine = hrfFineMatrix(hrf, span, dt)
    val nb = hrf.nbasis
    val nFine = neural.length

    val out = new Array[Double](grid.length * nb)
    var b = 0
    while b < nb do
      val hcol = hrfFine.map(_(b))
      val convFull = Fft.convolveReal(neural, hcol)
      val conv = convFull.take(nFine)

      var g = 0
      while g < grid.length do
        val pos = (grid(g).value - start) / dt
        val v =
          if pos <= 0.0 then conv(0)
          else if pos >= nFine - 1 then conv(nFine - 1)
          else
            val lo = math.floor(pos).toInt
            val alpha = pos - lo
            (1.0 - alpha) * conv(lo) + alpha * conv(lo + 1)
        out(g * nb + b) = v
        g += 1
      b += 1
    Mat.unsafe(grid.length, nb, out)

extension (reg: Regressor)
  def shift(amount: Seconds): Regressor =
    reg.copy(onsets = reg.onsets.map(_ + amount))

  def nbasis: Int = reg.hrf.nbasis

  def evaluate(
      grid: Seq[Double],
      precision: Double = 0.33,
      method: Regressor.EvalMethod = Regressor.EvalMethod.Conv
  ): Mat =
    Regressor.evaluate(reg, grid, precision, method)

  def neuralInput(
      from: Double = 0.0,
      to: Option[Double] = None,
      resolution: Double = 0.33
  ): (Array[Double], Array[Double]) =
    NeuralInput(reg, from, to, resolution)
