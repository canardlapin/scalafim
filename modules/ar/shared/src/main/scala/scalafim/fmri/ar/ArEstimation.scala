package scalafim.fmri.ar

import scalafim.linalg.DoubleMatrix

enum ArOrder:
  case Fixed(order: ArOrderValue)
  case Auto(maxOrder: ArOrderValue)

  def maxRequested: Int =
    this match
      case Fixed(order)  => order.value
      case Auto(maxOrder) => maxOrder.value

  def maxRequestedOrder: ArOrderValue =
    this match
      case Fixed(order)  => order
      case Auto(maxOrder) => maxOrder

object ArOrder:
  def Fixed(order: Int): ArOrder =
    Fixed(ArOrderValue.unsafe(order))

  def Auto(maxOrder: Int): ArOrder =
    Auto(ArOrderValue.unsafe(maxOrder))

final case class ArFitOptions private (
    order: ArOrder,
    pooling: NoisePooling,
    initialCondition: InitialConditionPolicy,
    stationarity: StationarityBound
):
  def exactFirstAr1: Boolean = initialCondition == InitialConditionPolicy.ExactAr1
  def stationarityBound: Double = stationarity.value

object ArFitOptions:

  def apply(
      order: ArOrder = ArOrder.Auto(6),
      pooling: NoisePooling = NoisePooling.Global,
      exactFirstAr1: Boolean = true,
      stationarityBound: Double = 0.99
  ): ArFitOptions =
    new ArFitOptions(
      order = order,
      pooling = pooling,
      initialCondition = InitialConditionPolicy.fromExactFirstAr1(exactFirstAr1),
      stationarity = StationarityBound.unsafe(stationarityBound)
    )

  def withInitialCondition(
      order: ArOrder = ArOrder.Auto(6),
      pooling: NoisePooling = NoisePooling.Global,
      initialCondition: InitialConditionPolicy = InitialConditionPolicy.ExactAr1,
      stationarity: StationarityBound = StationarityBound.Default
  ): ArFitOptions =
    new ArFitOptions(order, pooling, initialCondition, stationarity)

final case class YuleWalkerEstimate(
    coefficients: ArmaCoefficients,
    innovationVariance: Double
):
  require(innovationVariance >= 0.0 && innovationVariance.isFinite, "innovation variance must be non-negative and finite")

object ArEstimation:

  def fitNoise(
      residuals: DoubleMatrix,
      segments: Vector[TimeSegment],
      options: ArFitOptions = ArFitOptions()
  ): Either[ArError, WhiteningPlan] =
    TimeSegments.validateCoverage(segments, residuals.rows).flatMap { _ =>
      options.pooling match
        case NoisePooling.Global =>
          estimateForSegments(residuals, segments, options).flatMap { estimate =>
            WhiteningPlan.globalWithInitialCondition(
              estimate.coefficients,
              segments,
              initialCondition = options.initialCondition,
              method = WhiteningMethod.Estimated
            )
          }

        case NoisePooling.Run =>
          val runCount = segments.map(_.runIndex).max + 1
          val estimates =
            Vector
              .range(0, runCount)
              .foldLeft[Either[ArError, Vector[ArmaCoefficients]]](Right(Vector.empty)) {
                case (Left(error), _) => Left(error)
                case (Right(acc), run) =>
                  val runSegments = segments.filter(_.runIndex == run)
                  if runSegments.isEmpty then
                    Left(ArError.MissingRunSegments(run))
                  else
                    estimateForSegments(residuals, runSegments, options)
                      .map(result => acc :+ result.coefficients)
              }

          estimates.flatMap { coefficients =>
            WhiteningPlan.byRunWithInitialCondition(
              coefficients,
              segments,
              initialCondition = options.initialCondition,
              method = WhiteningMethod.Estimated
            )
          }
    }

  def estimateForSegments(
      residuals: DoubleMatrix,
      segments: Vector[TimeSegment],
      options: ArFitOptions
  ): Either[ArError, YuleWalkerEstimate] =
    val maxLag = ArLag.unsafe(math.min(options.order.maxRequested, maxEstimableLag(segments).value))
    options.order match
      case ArOrder.Fixed(order) =>
        if order.value > maxLag.value then
          Left(ArError.ArOrderNotEstimable(order, maxLag))
        else
          val gamma = autocovariances(residuals, segments, ArLag.unsafe(order.value))
          yuleWalker(gamma, order, options.stationarity)

      case ArOrder.Auto(_) =>
        val gamma = autocovariances(residuals, segments, maxLag)
        selectByBic(gamma, effectiveObservations(segments), maxLag, options.stationarity)

  def autocovariance(
      residuals: DoubleMatrix,
      segments: Vector[TimeSegment],
      maxLag: Int
  ): Vector[Double] =
    autocovariances(residuals, segments, ArLag.unsafe(maxLag)).toVector

  def autocovariances(
      residuals: DoubleMatrix,
      segments: Vector[TimeSegment],
      maxLag: ArLag
  ): Autocovariances =
    val lagCount = maxLag.value
    val sums = Array.fill(lagCount + 1)(0.0)
    val counts = Array.fill(lagCount + 1)(0)

    segments.foreach { segment =>
      var col = 0
      while col < residuals.cols do
        val mean = segmentMean(residuals, segment, col)
        var lag = 0
        while lag <= lagCount do
          var row = segment.start + lag
          while row < segment.endExclusive do
            sums(lag) += (residuals(row, col) - mean) * (residuals(row - lag, col) - mean)
            counts(lag) += 1
            row += 1
          lag += 1
        col += 1
    }

    val values = sums.indices.map { lag =>
      if counts(lag) == 0 then 0.0 else sums(lag) / counts(lag).toDouble
    }.toVector
    Autocovariances.unsafe(values)

  def yuleWalker(
      gamma: Vector[Double],
      order: Int,
      stationarityBound: Double = 0.99
  ): Either[ArError, YuleWalkerEstimate] =
    for
      typedOrder <- ArOrderValue(order)
      typedGamma <- Autocovariances(gamma)
      bound <- StationarityBound(stationarityBound)
      estimate <- yuleWalker(typedGamma, typedOrder, bound)
    yield estimate

  def yuleWalker(
      gamma: Autocovariances,
      order: ArOrderValue,
      stationarityBound: StationarityBound
  ): Either[ArError, YuleWalkerEstimate] =
    gamma.takeThrough(order).flatMap { scopedGamma =>
      if order.value == 0 then
        Right(YuleWalkerEstimate(ArmaCoefficients.Iid, math.max(0.0, scopedGamma.lagZero)))
      else if scopedGamma.lagZero <= 0.0 || !scopedGamma.lagZero.isFinite then
        Right(YuleWalkerEstimate(ArmaCoefficients.ar(Vector.fill(order.value)(0.0)*), 0.0))
      else
        yuleWalkerNonZero(scopedGamma, order, stationarityBound)
    }

  private def yuleWalkerNonZero(
      gamma: Autocovariances,
      order: ArOrderValue,
      stationarityBound: StationarityBound
  ): Either[ArError, YuleWalkerEstimate] =
    if gamma.lagZero <= 0.0 || !gamma.lagZero.isFinite then
      Right(YuleWalkerEstimate(ArmaCoefficients.ar(Vector.fill(order.value)(0.0)*), 0.0))
    else
      var sigma2 = gamma.lagZero
      var previous = Array.empty[Double]
      var m = 1
      while m <= order.value do
        var acc = gamma.at(ArLag.unsafe(m))
        var j = 1
        while j <= m - 1 do
          acc -= previous(j - 1) * gamma.at(ArLag.unsafe(m - j))
          j += 1

        val kappa =
          if sigma2 <= 0.0 || !sigma2.isFinite then 0.0
          else acc / sigma2

        val current = new Array[Double](m)
        j = 1
        while j <= m - 1 do
          current(j - 1) = previous(j - 1) - kappa * previous((m - j) - 1)
          j += 1
        current(m - 1) = kappa
        sigma2 = math.max(0.0, sigma2 * (1.0 - kappa * kappa))
        previous = current
        m += 1

      val stable = Pacf.enforceStationary(previous.toVector, stationarityBound)
      Right(YuleWalkerEstimate(ArmaCoefficients.ar(stable*), sigma2))

  private def selectByBic(
      gamma: Autocovariances,
      observations: Int,
      maxOrder: ArLag,
      stationarityBound: StationarityBound
  ): Either[ArError, YuleWalkerEstimate] =
    var best: Option[(Double, YuleWalkerEstimate)] = None
    var order = 0
    while order <= maxOrder.value do
      yuleWalker(gamma, ArOrderValue.unsafe(order), stationarityBound) match
        case Left(error) => return Left(error)
        case Right(estimate) =>
          val sigma2 = math.max(estimate.innovationVariance, 1e-12)
          val bic = observations.toDouble * math.log(sigma2) + (order + 1).toDouble * math.log(observations.toDouble)
          best match
            case None => best = Some(bic -> estimate)
            case Some((current, _)) if bic < current => best = Some(bic -> estimate)
            case _ => ()
      order += 1

    best.map(_._2).toRight(ArError.UnableToEstimateArModel)

  private def maxEstimableLag(segments: Vector[TimeSegment]): ArLag =
    ArLag.unsafe(segments.map(_.length - 1).max)

  private def effectiveObservations(segments: Vector[TimeSegment]): Int =
    segments.map(_.length).sum

  private def segmentMean(matrix: DoubleMatrix, segment: TimeSegment, col: Int): Double =
    var sum = 0.0
    var row = segment.start
    while row < segment.endExclusive do
      sum += matrix(row, col)
      row += 1
    sum / segment.length.toDouble
