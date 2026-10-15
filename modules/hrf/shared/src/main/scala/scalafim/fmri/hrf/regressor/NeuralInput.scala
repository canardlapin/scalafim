package scalafim.fmri.hrf.regressor

import scalafim.fmri.hrf.TimeError

/** A refusal to construct the sampled neural-input preview. */
enum NeuralInputError:
  case InvalidFrom(error: TimeError)
  case InvalidTo(error: TimeError)
  case InvalidEventEnd(index: Int, value: Double)
  case InvalidGrid(error: ConvolutionError)

  def message: String = this match
    case InvalidFrom(error) => error.message
    case InvalidTo(error) => error.message
    case InvalidEventEnd(index, value) =>
      s"neural input event ${index + 1} has a non-finite end: $value"
    case InvalidGrid(error) => error.message

object NeuralInput:
  /** The preview shares the portable convolution budget: at most one million
    * intervals plus the initial sample (about 16 MB across the two arrays).
    * Its historical floor grid is retained; precision is never coarsened.
    */
  val MaxGridSamples: Int = ConvolutionDiscretization.MaxGridSamples

  /** Legacy facade; use [[either]] to inspect a refusal without throwing. */
  def apply(
      reg: Regressor,
      from: Double = 0.0,
      to: Option[Double] = None,
      resolution: Double = 0.33
  ): (Array[Double], Array[Double]) =
    either(reg, from, to, resolution)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Preview event amplitudes on an inclusive floor grid. Blocks occupy every
    * bin from floor(onset) through floor(onset + duration), including their end
    * bin. This diagnostic preview retains amplitude directly for both values
    * of `reg.summate`; it is not the convolution drive's quadrature.
    */
  def either(
      reg: Regressor,
      from: Double = 0.0,
      to: Option[Double] = None,
      resolution: Double = 0.33
  ): Either[NeuralInputError, (Array[Double], Array[Double])] =
    for
      _ <- finiteFrom(from)
      lastEventEnd <- eventEnd(reg)
      end <- finiteTo(to.getOrElse(if reg.events.isEmpty then from else lastEventEnd + 10.0))
      count <- ConvolutionDiscretization.sampleCount(end - from, resolution, "neural input grid")
        .left.map(NeuralInputError.InvalidGrid.apply)
      _ <- finiteTo(from + (count - 1).toDouble * resolution)
    yield render(reg, from, resolution, count)

  private def finiteFrom(from: Double): Either[NeuralInputError, Unit] =
    if from.isFinite then Right(())
    else Left(NeuralInputError.InvalidFrom(TimeError.NonFinite("from", from)))

  private def finiteTo(end: Double): Either[NeuralInputError, Double] =
    if end.isFinite then Right(end)
    else Left(NeuralInputError.InvalidTo(TimeError.NonFinite("to", end)))

  private def eventEnd(reg: Regressor): Either[NeuralInputError, Double] =
    var latest = 0.0
    var index = 0
    while index < reg.events.length do
      val event = reg.events(index)
      val end = event.onset.value + event.duration.value
      if !end.isFinite then return Left(NeuralInputError.InvalidEventEnd(index, end))
      latest = math.max(latest, end)
      index += 1
    Right(latest)

  private def render(
      reg: Regressor,
      from: Double,
      resolution: Double,
      count: Int
  ): (Array[Double], Array[Double]) =
    val time = Array.tabulate(count)(i => from + i.toDouble * resolution)
    val neural = new Array[Double](count)
    val lastBin = count - 1

    var index = 0
    while index < reg.events.length do
      val event = reg.events(index)
      // Ratios can overflow even for finite events outside a tiny window.
      // Clip floating bin bounds before conversion and skip negative bins
      // before iteration, so event work never exceeds the admitted grid.
      val startBin = math.floor((event.onset.value - from) / resolution)
      if event.duration.value > 0.0 then
        val endBin = math.floor((event.onset.value + event.duration.value - from) / resolution)
        if startBin <= lastBin.toDouble && endBin >= 0.0 then
          var bin = math.max(0.0, startBin).toInt
          val end = math.min(lastBin.toDouble, endBin).toInt
          while bin <= end do
            neural(bin) += event.amplitude
            bin += 1
      else if startBin >= 0.0 && startBin < count.toDouble then
        // An impulse outside the window contributes nothing, including when
        // its floating position exceeds the integer range.
        neural(startBin.toInt) += event.amplitude
      index += 1

    (time, neural)
