package scalafim.fmri.ar

import gale.linalg.{CholeskyOptions, DMat, Matrix}

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

  private val RelativePositiveDefiniteMargin = 1e-6

  private final case class PooledAutocovariance(
      sums: Array[Double],
      pairCounts: Array[Long]
  ):
    require(sums.length == pairCounts.length, "autocovariance sums and pair counts must align")
    require(sums.nonEmpty, "pooled autocovariance must contain lag zero")

    def maxLag: ArLag =
      var lag = pairCounts.length - 1
      while lag > 0 && pairCounts(lag) == 0L do lag -= 1
      ArLag.unsafe(lag)

    def through(order: ArOrderValue): Either[ArError, Autocovariances] =
      val required = order.value + 1
      if required > sums.length then Left(ArError.InsufficientAutocovariances(required, sums.length))
      else
        var lag = 0
        while lag < required do
          if pairCounts(lag) == 0L then
            return Left(ArError.InsufficientAutocovariances(required, lag))
          lag += 1
        val values = Vector.tabulate(required)(lag => sums(lag) / pairCounts(lag).toDouble)
        Autocovariances(values).map(stabilizeAutocovariances)

  private final case class RunEstimate(
      coefficients: ArmaCoefficients,
      observations: Int
  ):
    require(observations >= 0, "run estimate observations must be non-negative")

  def fitNoise(
      residuals: DMat,
      segments: Vector[TimeSegment],
      options: ArFitOptions = ArFitOptions()
  ): Either[ArError, WhiteningPlan] =
    NoiseEstimationLayout
      .allRows(segments, residuals.rows)
      .flatMap(layout => fitNoise(residuals, layout, options))

  def fitNoise(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      options: ArFitOptions
  ): Either[ArError, WhiteningPlan] =
    for
      _ <- layout.coveredSegments.validateRows(residuals.rows)
      _ <- validateFinite(residuals)
      _ <- if layout.retainedRows > 0 then Right(()) else Left(ArError.NoEstimableRows)
      plan <-
        options.pooling match
          case NoisePooling.Global =>
            for
              estimates <- estimateByRun(residuals, layout, options)
              coefficients <- poolRunCoefficients(estimates, options.stationarity)
              plan <-
                WhiteningPlan.globalWithInitialCondition(
                  coefficients,
                  layout.whiteningSegments,
                  initialCondition = options.initialCondition,
                  method = WhiteningMethod.Estimated
                )
            yield plan

          case NoisePooling.Run =>
            estimateByRun(residuals, layout, options).flatMap { estimates =>
              WhiteningPlan.byRunWithInitialCondition(
                estimates.map(_.coefficients),
                layout.whiteningSegments,
                initialCondition = options.initialCondition,
                method = WhiteningMethod.Estimated
              )
            }
    yield plan

  private def estimateByRun(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      options: ArFitOptions
  ): Either[ArError, Vector[RunEstimate]] =
    val estimates = Vector.newBuilder[RunEstimate]
    var run = 0
    while run < layout.runCount do
      val segments = layout.segmentsForRun(run)
      val observations = effectiveObservations(segments)
      if observations <= 1 then
        estimates += RunEstimate(ArmaCoefficients.Iid, observations)
      else
        estimateForSegmentsUnchecked(residuals, segments, options) match
          case Left(error) => return Left(error)
          case Right(estimate) =>
            estimates += RunEstimate(estimate.coefficients, observations)
      run += 1
    Right(estimates.result())

  private def poolRunCoefficients(
      estimates: Vector[RunEstimate],
      stationarityBound: StationarityBound
  ): Either[ArError, ArmaCoefficients] =
    val observations = estimates.map(_.observations).sum
    if observations <= 0 then Left(ArError.NoEstimableRows)
    else
      val order = estimates.map(_.coefficients.arOrder).maxOption.getOrElse(0)
      if order == 0 then Right(ArmaCoefficients.Iid)
      else
        val pooled = Array.fill(order)(0.0)
        estimates.foreach { estimate =>
          val weight = estimate.observations.toDouble / observations.toDouble
          var lag = 0
          while lag < estimate.coefficients.arOrder do
            pooled(lag) += weight * estimate.coefficients.phi(lag)
            lag += 1
        }
        Pacf
          .enforceStationaryChecked(pooled.toVector, stationarityBound)
          .map(phi => ArmaCoefficients.ar(phi*))

  def estimateForSegments(
      residuals: DMat,
      segments: Vector[TimeSegment],
      options: ArFitOptions
  ): Either[ArError, YuleWalkerEstimate] =
    for
      layout <- NoiseEstimationLayout.allRows(segments, residuals.rows)
      _ <- validateFinite(residuals)
      estimate <- estimateForSegmentsUnchecked(residuals, layout.estimationSegments, options)
    yield estimate

  private def estimateForSegmentsUnchecked(
      residuals: DMat,
      segments: Vector[TimeSegment],
      options: ArFitOptions
  ): Either[ArError, YuleWalkerEstimate] =
    val observations = effectiveObservations(segments)
    if observations <= 1 then
      Right(YuleWalkerEstimate(ArmaCoefficients.Iid, 0.0))
    else
      pooledAutocovariance(residuals, segments, options.order.maxRequestedOrder).flatMap { pooled =>
        val maxLag = ArLag.unsafe(math.min(options.order.maxRequested, pooled.maxLag.value))
        options.order match
          case ArOrder.Fixed(order) =>
            if order.value > maxLag.value then
              Left(ArError.ArOrderNotEstimable(order, maxLag))
            else
              pooled.through(order).flatMap(yuleWalker(_, order, options.stationarity))

          case ArOrder.Auto(_) =>
            selectByBic(pooled, observations, maxLag, options.stationarity)
      }

  def autocovariance(
      residuals: DMat,
      segments: Vector[TimeSegment],
      maxLag: Int
  ): Either[ArError, Vector[Double]] =
    ArLag(maxLag).flatMap(autocovariances(residuals, segments, _)).map(_.toVector)

  def autocovariances(
      residuals: DMat,
      segments: Vector[TimeSegment],
      maxLag: ArLag
  ): Either[ArError, Autocovariances] =
    for
      layout <- NoiseEstimationLayout.allRows(segments, residuals.rows)
      _ <- validateFinite(residuals)
      pooled <- pooledAutocovariance(residuals, layout.estimationSegments, ArOrderValue.unsafe(maxLag.value))
      _ <-
        if maxLag.value <= pooled.maxLag.value then Right(())
        else Left(ArError.ArOrderNotEstimable(ArOrderValue.unsafe(maxLag.value), pooled.maxLag))
      gamma <- pooled.through(ArOrderValue.unsafe(maxLag.value))
    yield gamma

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
        yuleWalkerNonZero(stabilizeAutocovariances(scopedGamma), order, stationarityBound)
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

      Pacf.enforceStationaryChecked(previous.toVector, stationarityBound).map { stable =>
        var innovationVariance = gamma.lagZero
        var lag = 0
        while lag < stable.length do
          innovationVariance -= stable(lag) * gamma.at(ArLag.unsafe(lag + 1))
          lag += 1
        val boundedVariance = math.min(gamma.lagZero, math.max(1e-12, innovationVariance))
        YuleWalkerEstimate(ArmaCoefficients.ar(stable*), boundedVariance)
      }

  private def selectByBic(
      pooled: PooledAutocovariance,
      observations: Int,
      maxOrder: ArLag,
      stationarityBound: StationarityBound
  ): Either[ArError, YuleWalkerEstimate] =
    var best: Option[(Double, YuleWalkerEstimate)] = None
    var order = 0
    val selectionLimit = math.min(maxOrder.value, observations / 5)
    while order <= selectionLimit do
      val estimate =
        pooled
          .through(ArOrderValue.unsafe(order))
          .flatMap(yuleWalker(_, ArOrderValue.unsafe(order), stationarityBound))
      estimate match
        case Left(error) => return Left(error)
        case Right(estimate) =>
          val sigma2 = math.max(estimate.innovationVariance, 1e-12)
          val bic =
            2.0 * observations.toDouble * math.log(sigma2) +
              (order + 1).toDouble * math.log(observations.toDouble)
          best match
            case None => best = Some(bic -> estimate)
            case Some((current, _)) if bic < current => best = Some(bic -> estimate)
            case _ => ()
      order += 1

    best.map(_._2).toRight(ArError.UnableToEstimateArModel)

  private def effectiveObservations(segments: Vector[TimeSegment]): Int =
    segments.map(_.length).sum

  private def pooledAutocovariance(
      residuals: DMat,
      segments: Vector[TimeSegment],
      maxOrder: ArOrderValue
  ): Either[ArError, PooledAutocovariance] =
    if segments.isEmpty then Left(ArError.NoEstimableRows)
    else
      val maxRun = segments.map(_.runIndex).max
      val runRows = Array.fill(maxRun + 1)(0)
      val runSums = Array.fill((maxRun + 1) * residuals.cols)(0.0)
      segments.foreach { segment =>
        var row = segment.start
        while row < segment.endExclusive do
          runRows(segment.runIndex) += 1
          var col = 0
          while col < residuals.cols do
            runSums(segment.runIndex * residuals.cols + col) += residuals(row, col)
            col += 1
          row += 1
      }

      val lagCount = maxOrder.value
      val sums = Array.fill(lagCount + 1)(0.0)
      val pairCounts = Array.fill(lagCount + 1)(0L)
      segments.foreach { segment =>
        var col = 0
        while col < residuals.cols do
          val mean = runSums(segment.runIndex * residuals.cols + col) / runRows(segment.runIndex).toDouble
          var lag = 0
          while lag <= lagCount do
            var row = segment.start + lag
            while row < segment.endExclusive do
              sums(lag) += (residuals(row, col) - mean) * (residuals(row - lag, col) - mean)
              pairCounts(lag) += 1L
              row += 1
            lag += 1
          col += 1
      }
      Right(PooledAutocovariance(sums, pairCounts))

  private def validateFinite(residuals: DMat): Either[ArError, Unit] =
    var row = 0
    while row < residuals.rows do
      var col = 0
      while col < residuals.cols do
        val value = residuals(row, col)
        if !value.isFinite then return Left(ArError.NonFiniteResidual(row, col, value))
        col += 1
      row += 1
    Right(())

  private[ar] def stabilizeAutocovariances(
      gamma: Autocovariances
  ): Autocovariances =
    if gamma.lagZero <= 0.0 || gamma.length <= 1 || isStrictlyPositiveDefinite(gamma.toVector) then gamma
    else
      var lower = 0.0
      var upper = 1.0
      var iteration = 0
      while iteration < 50 do
        val scale = 0.5 * (lower + upper)
        val candidate = gamma.toVector.zipWithIndex.map { case (value, lag) =>
          if lag == 0 then value else value * scale
        }
        if isStrictlyPositiveDefinite(candidate) then lower = scale
        else upper = scale
        iteration += 1
      Autocovariances.unsafe(
        gamma.toVector.zipWithIndex.map { case (value, lag) =>
          if lag == 0 then value else value * lower
        }
      )

  private[ar] def isStrictlyPositiveDefinite(values: Vector[Double]): Boolean =
    if values.isEmpty || !values.forall(_.isFinite) || values.head <= 0.0 then false
    else
      val margin = RelativePositiveDefiniteMargin * values.head
      val toeplitz = Matrix.newBuilder(values.length, values.length)
      var row = 0
      while row < values.length do
        var col = 0
        while col < values.length do
          toeplitz(row, col) = values(math.abs(row - col)) - (if row == col then margin else 0.0)
          col += 1
        row += 1
      toeplitz.result().cholesky(CholeskyOptions(0.0)).isRight
