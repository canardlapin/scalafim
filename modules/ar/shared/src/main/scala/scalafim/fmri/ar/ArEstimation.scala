package scalafim.fmri.ar

import scalafim.linalg.DoubleMatrix

enum ArOrder:
  case Fixed(order: Int)
  case Auto(maxOrder: Int)

  def maxRequested: Int =
    this match
      case Fixed(order)  => order
      case Auto(maxOrder) => maxOrder

final case class ArFitOptions(
    order: ArOrder = ArOrder.Auto(6),
    pooling: NoisePooling = NoisePooling.Global,
    exactFirstAr1: Boolean = true,
    stationarityBound: Double = 0.99
):
  require(order.maxRequested >= 0, "AR order must be non-negative")
  require(stationarityBound > 0.0 && stationarityBound < 1.0 && stationarityBound.isFinite, "stationarity bound must be in (0, 1)")

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
          estimateForSegments(residuals, segments, options).map { estimate =>
            WhiteningPlan.global(
              estimate.coefficients,
              segments,
              exactFirstAr1 = options.exactFirstAr1,
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
                    Left(ArError.CoefficientMismatch(s"run $run has no segments"))
                  else
                    estimateForSegments(residuals, runSegments, options)
                      .map(result => acc :+ result.coefficients)
              }

          estimates.map { coefficients =>
            WhiteningPlan.byRun(
              coefficients,
              segments,
              exactFirstAr1 = options.exactFirstAr1,
              method = WhiteningMethod.Estimated
            )
          }
    }

  def estimateForSegments(
      residuals: DoubleMatrix,
      segments: Vector[TimeSegment],
      options: ArFitOptions
  ): Either[ArError, YuleWalkerEstimate] =
    val maxLag = math.min(options.order.maxRequested, maxEstimableLag(segments))
    options.order match
      case ArOrder.Fixed(order) =>
        if order > maxLag then
          Left(ArError.CoefficientMismatch(s"requested AR($order) but only $maxLag lags are estimable"))
        else
          val gamma = autocovariance(residuals, segments, order)
          yuleWalker(gamma, order, options.stationarityBound)

      case ArOrder.Auto(_) =>
        val gamma = autocovariance(residuals, segments, maxLag)
        selectByBic(gamma, effectiveObservations(segments), maxLag, options.stationarityBound)

  def autocovariance(
      residuals: DoubleMatrix,
      segments: Vector[TimeSegment],
      maxLag: Int
  ): Vector[Double] =
    require(maxLag >= 0, "maxLag must be non-negative")
    val sums = Array.fill(maxLag + 1)(0.0)
    val counts = Array.fill(maxLag + 1)(0)

    segments.foreach { segment =>
      var col = 0
      while col < residuals.cols do
        val mean = segmentMean(residuals, segment, col)
        var lag = 0
        while lag <= maxLag do
          var row = segment.start + lag
          while row < segment.endExclusive do
            sums(lag) += (residuals(row, col) - mean) * (residuals(row - lag, col) - mean)
            counts(lag) += 1
            row += 1
          lag += 1
        col += 1
    }

    sums.indices.map { lag =>
      if counts(lag) == 0 then 0.0 else sums(lag) / counts(lag).toDouble
    }.toVector

  def yuleWalker(
      gamma: Vector[Double],
      order: Int,
      stationarityBound: Double = 0.99
  ): Either[ArError, YuleWalkerEstimate] =
    require(order >= 0, "order must be non-negative")
    require(gamma.length >= order + 1, "gamma must contain lag 0 through order")
    if order == 0 then
      Right(YuleWalkerEstimate(ArmaCoefficients.Iid, math.max(0.0, gamma.headOption.getOrElse(0.0))))
    else if gamma.head <= 0.0 || !gamma.head.isFinite then
      Right(YuleWalkerEstimate(ArmaCoefficients.ar(Vector.fill(order)(0.0)*), 0.0))
    else
      var sigma2 = gamma.head
      var previous = Array.empty[Double]
      var m = 1
      while m <= order do
        var acc = gamma(m)
        var j = 1
        while j <= m - 1 do
          acc -= previous(j - 1) * gamma(m - j)
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
      gamma: Vector[Double],
      observations: Int,
      maxOrder: Int,
      stationarityBound: Double
  ): Either[ArError, YuleWalkerEstimate] =
    var best: Option[(Double, YuleWalkerEstimate)] = None
    var order = 0
    while order <= maxOrder do
      yuleWalker(gamma.take(order + 1), order, stationarityBound) match
        case Left(error) => return Left(error)
        case Right(estimate) =>
          val sigma2 = math.max(estimate.innovationVariance, 1e-12)
          val bic = observations.toDouble * math.log(sigma2) + (order + 1).toDouble * math.log(observations.toDouble)
          best match
            case None => best = Some(bic -> estimate)
            case Some((current, _)) if bic < current => best = Some(bic -> estimate)
            case _ => ()
      order += 1

    best.map(_._2).toRight(ArError.CoefficientMismatch("unable to estimate AR model"))

  private def maxEstimableLag(segments: Vector[TimeSegment]): Int =
    segments.map(_.length - 1).max

  private def effectiveObservations(segments: Vector[TimeSegment]): Int =
    segments.map(_.length).sum

  private def segmentMean(matrix: DoubleMatrix, segment: TimeSegment, col: Int): Double =
    var sum = 0.0
    var row = segment.start
    while row < segment.endExclusive do
      sum += matrix(row, col)
      row += 1
    sum / segment.length.toDouble
