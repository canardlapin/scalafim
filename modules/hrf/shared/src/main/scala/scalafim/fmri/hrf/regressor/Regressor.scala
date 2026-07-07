package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.*
import scalafim.fmri.hrf.Hrfs
import scalafim.fmri.hrf.linalg.{Mat, Vec, Fft}

enum RegressorError:
  case LengthMismatch(name: String, expected: Int, actual: Int)
  case InvalidOnset(index: Int, error: TimeError)
  case InvalidDuration(index: Int, error: TimeError)
  case InvalidAmplitude(index: Int, value: Double)
  case InvalidSpan(error: TimeError)
  case HrfLengthMismatch(expected: Int, actual: Int)
  case MixedBasisCounts

  def message: String =
    this match
      case LengthMismatch(name, expected, actual) =>
        s"`$name` must have length 1 or $expected, not $actual"
      case InvalidOnset(index, error) =>
        s"invalid onset at event ${index + 1}: ${error.message}"
      case InvalidDuration(index, error) =>
        s"invalid duration at event ${index + 1}: ${error.message}"
      case InvalidAmplitude(index, value) =>
        s"invalid amplitude at event ${index + 1}: amplitude must be finite, got $value"
      case InvalidSpan(error) =>
        error.message
      case HrfLengthMismatch(expected, actual) =>
        s"`hrf` list must have length 1 or $expected, not $actual"
      case MixedBasisCounts =>
        "all per-event HRFs must have the same nbasis"

final case class StimulusEvent private (
    onset: NonNegativeSeconds,
    duration: NonNegativeSeconds,
    amplitude: Double
):
  def onsetSeconds: Seconds = onset.seconds
  def durationSeconds: Seconds = duration.seconds

  def shift(amount: Seconds): Either[RegressorError, StimulusEvent] =
    StimulusEvent(onset.value + amount.value, duration.value, amplitude)

object StimulusEvent:
  def apply(onset: Double, duration: Double = 0.0, amplitude: Double = 1.0): Either[RegressorError, StimulusEvent] =
    for
      onset0 <- NonNegativeSeconds(onset, "onset").left.map(RegressorError.InvalidOnset(0, _))
      duration0 <- NonNegativeSeconds(duration, "duration").left.map(RegressorError.InvalidDuration(0, _))
      amp0 <- finiteAmplitude(amplitude, index = 0)
    yield StimulusEvent(onset0, duration0, amp0)

  private[regressor] def fromSeconds(
      onset: Seconds,
      duration: Seconds,
      amplitude: Double,
      index: Int
  ): Either[RegressorError, StimulusEvent] =
    for
      onset0 <- NonNegativeSeconds.fromSeconds(onset, "onset").left.map(RegressorError.InvalidOnset(index, _))
      duration0 <- NonNegativeSeconds.fromSeconds(duration, "duration").left.map(RegressorError.InvalidDuration(index, _))
      amp0 <- finiteAmplitude(amplitude, index)
    yield StimulusEvent(onset0, duration0, amp0)

  private def finiteAmplitude(amplitude: Double, index: Int): Either[RegressorError, Double] =
    if amplitude.isFinite then Right(amplitude) else Left(RegressorError.InvalidAmplitude(index, amplitude))

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
    require(hrfs.isEmpty || hrfs.forall(_.nbasis == hrfs.head.nbasis), "all per-event HRFs must have the same nbasis")
    def nbasis: Int = if hrfs.isEmpty then 1 else hrfs.head.nbasis
    def span: Seconds = if hrfs.isEmpty then Seconds(0.0) else hrfs.map(_.span).max
    def at(t: Seconds, eventIndex: Int): Vec = hrfs(eventIndex)(t)

final case class Regressor private (
    events: Vector[StimulusEvent],
    hrf: HrfAssignment,
    span: Seconds,
    summate: Boolean
):
  def onsets: Vector[Seconds] = events.map(_.onsetSeconds)
  def durations: Vector[Seconds] = events.map(_.durationSeconds)
  def amplitudes: Vector[Double] = events.map(_.amplitude)

object Regressor:

  enum EvalMethod:
    case Conv, FFT, Loop

  private def recycleOrError[A](xs: Seq[A], n: Int, name: String): Either[RegressorError, Vector[A]] =
    if xs.length == n then Right(xs.toVector)
    else if xs.length == 1 then Right(Vector.fill(n)(xs.head))
    else Left(RegressorError.LengthMismatch(name, n, xs.length))

  private def secondsVector(
      values: Seq[Double],
      label: String,
      err: (Int, TimeError) => RegressorError
  ): Either[RegressorError, Vector[Seconds]] =
    val out = Vector.newBuilder[Seconds]
    var i = 0
    val xs = values.toVector
    while i < xs.length do
      Seconds.fromDouble(xs(i), label) match
        case Left(error) => return Left(err(i, error))
        case Right(seconds) => out += seconds
      i += 1
    Right(out.result())

  private def validateEvents(
      onsets: Vector[Seconds],
      durations: Vector[Seconds],
      amplitudes: Vector[Double]
  ): Either[RegressorError, Vector[StimulusEvent]] =
    if durations.length != onsets.length then Left(RegressorError.LengthMismatch("duration", onsets.length, durations.length))
    else if amplitudes.length != onsets.length then Left(RegressorError.LengthMismatch("amplitude", onsets.length, amplitudes.length))
    else
      val out = Vector.newBuilder[StimulusEvent]
      var i = 0
      while i < onsets.length do
        StimulusEvent.fromSeconds(onsets(i), durations(i), amplitudes(i), i) match
          case Left(err) => return Left(err)
          case Right(event) =>
            out += event
        i += 1
      Right(out.result())

  def fromEvents(
      events: Seq[StimulusEvent],
      hrf: HrfAssignment,
      span: Seconds,
      summate: Boolean
  ): Either[RegressorError, Regressor] =
    Seconds.fromDouble(span.value, "span").left.map(RegressorError.InvalidSpan.apply).flatMap { span0 =>
      val filtered = events.iterator.filter(_.amplitude != 0.0).toVector
      val hrf0: Either[RegressorError, HrfAssignment] =
        hrf match
          case shared: HrfAssignment.Shared => Right(shared)
          case HrfAssignment.PerEvent(hrfs) =>
            if hrfs.length != events.size then Left(RegressorError.HrfLengthMismatch(events.size, hrfs.length))
            else
              val keep = events.iterator.zipWithIndex.collect { case (event, i) if event.amplitude != 0.0 => i }.toVector
              val keptHrfs = keep.map(hrfs)
              if keptHrfs.nonEmpty && keptHrfs.exists(_.nbasis != keptHrfs.head.nbasis) then Left(RegressorError.MixedBasisCounts)
              else Right(HrfAssignment.PerEvent(keptHrfs))
      hrf0.map(hrf1 => Regressor(filtered, hrf1, span0, summate))
    }

  def fromParts(
      onsets: Seq[Seconds],
      durations: Seq[Seconds],
      amplitudes: Seq[Double],
      hrf: HrfAssignment,
      span: Seconds,
      summate: Boolean
  ): Either[RegressorError, Regressor] =
    validateEvents(onsets.toVector, durations.toVector, amplitudes.toVector).flatMap(fromEvents(_, hrf, span, summate))

  def unsafeFromParts(
      onsets: Seq[Seconds],
      durations: Seq[Seconds],
      amplitudes: Seq[Double],
      hrf: HrfAssignment,
      span: Seconds,
      summate: Boolean
  ): Regressor =
    fromParts(onsets, durations, amplitudes, hrf, span, summate)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def validated(
      onsets: Seq[Double],
      hrf: Hrf = Hrfs.SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): Either[RegressorError, Regressor] =
    for
      ons <- secondsVector(onsets, "onset", RegressorError.InvalidOnset.apply)
      durs0 <- recycleOrError(duration, ons.length, "duration")
      amps0 <- recycleOrError(amplitude, ons.length, "amplitude")
      durs <- secondsVector(durs0, "duration", RegressorError.InvalidDuration.apply)
      span0 <- span match
        case None => Right(hrf.span)
        case Some(s) =>
          PositiveSeconds(s, "span").left.map(RegressorError.InvalidSpan.apply).map(_.seconds)
      reg <- fromParts(
        onsets = ons,
        durations = durs,
        amplitudes = amps0,
        hrf = HrfAssignment.Shared(hrf),
        span = span0,
        summate = summate
      )
    yield reg

  def apply(
      onsets: Seq[Double],
      hrf: Hrf = Hrfs.SPMG1,
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): Regressor =
    validated(onsets, hrf, duration, amplitude, span, summate)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def perEvent(
      onsets: Seq[Double],
      hrfs: Seq[Hrf],
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): Regressor =
    perEventValidated(onsets, hrfs, duration, amplitude, span, summate)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

  def perEventValidated(
      onsets: Seq[Double],
      hrfs: Seq[Hrf],
      duration: Seq[Double] = Seq(0.0),
      amplitude: Seq[Double] = Seq(1.0),
      span: Option[Double] = None,
      summate: Boolean = true
  ): Either[RegressorError, Regressor] =
    for
      ons <- secondsVector(onsets, "onset", RegressorError.InvalidOnset.apply)
      hrs <- recycleOrError(hrfs, ons.length, "hrf").left.map {
        case RegressorError.LengthMismatch(_, expected, actual) => RegressorError.HrfLengthMismatch(expected, actual)
        case other => other
      }
      _ <- if hrs.nonEmpty && hrs.exists(_.nbasis != hrs.head.nbasis) then Left(RegressorError.MixedBasisCounts) else Right(())
      durs0 <- recycleOrError(duration, ons.length, "duration")
      amps0 <- recycleOrError(amplitude, ons.length, "amplitude")
      durs <- secondsVector(durs0, "duration", RegressorError.InvalidDuration.apply)
      span0 <- span match
        case Some(s) =>
          PositiveSeconds(s, "span").left.map(RegressorError.InvalidSpan.apply).map(_.seconds)
        case None =>
          Right(hrs.map(_.span).maxOption.getOrElse(Seconds(0.0)))
      reg <- fromParts(
        onsets = ons,
        durations = durs,
        amplitudes = amps0,
        hrf = HrfAssignment.PerEvent(hrs),
        span = span0,
        summate = summate
      )
    yield reg

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
    val shifted = reg.events.map { event =>
      event.shift(amount).fold(err => throw new IllegalArgumentException(err.message), identity)
    }
    Regressor.fromEvents(shifted, reg.hrf, reg.span, reg.summate)
      .fold(err => throw new IllegalArgumentException(err.message), identity)

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
