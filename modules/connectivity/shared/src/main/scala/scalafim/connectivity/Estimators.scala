package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

object ConnectivityEstimators:
  def weightedCorrelation(
      series: ParcelTimeSeries,
      frameWeights: Option[FrameWeights] = None,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    for
      weights <- normalizedWeights(series, frameWeights)
      matrix = weightedCorrelationMatrix(series.values, weights)
      space <- EdgeSpace.undirected(series.nodeAxis, order)
      connectivity <- ConnectivityMatrix.from(
        matrix,
        space,
        measure = ConnectivityMeasure.correlation,
        diagonalPolicy = DiagonalPolicy.Unit
      )
    yield StaticConnectivity(
      matrix = connectivity,
      estimator = Some(EstimatorSpec.weightedCorrelation(useFrameWeights = frameWeights.nonEmpty)),
      diagnostics = ConnectivityDiagnostics.empty
    )

  def diagonalShrinkageCorrelation(
      series: ParcelTimeSeries,
      frameWeights: Option[FrameWeights] = None,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    for
      weights <- normalizedWeights(series, frameWeights)
      correlation = weightedCorrelationMatrix(series.values, weights)
      shrinkage = diagonalShrink(correlation)
      space <- EdgeSpace.undirected(series.nodeAxis, order)
      connectivity <- ConnectivityMatrix.from(
        shrinkage.matrix,
        space,
        measure = ConnectivityMeasure.correlation,
        diagonalPolicy = DiagonalPolicy.Unit
      )
    yield StaticConnectivity(
      matrix = connectivity,
      estimator = Some(EstimatorSpec.diagonalShrinkageCorrelation(useFrameWeights = frameWeights.nonEmpty)),
      diagnostics = ConnectivityDiagnostics(
        warnings = Vector.empty,
        convergence = None,
        metrics = Map("shrinkage.alpha" -> shrinkage.alpha)
      )
    )

  def eventWeightedCorrelation(
      series: ParcelTimeSeries,
      eventWeighting: EventWeighting = EventWeighting.default,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    EventTimeSeries.eventWeightedCorrelation(series, eventWeighting, order)

  def ridgePartialCorrelation(
      series: ParcelTimeSeries,
      lambda: Double,
      frameWeights: Option[FrameWeights] = None,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    PartialCorrelation.ridge(series, lambda, frameWeights, order)

  def pcPartialCorrelation(
      series: ParcelTimeSeries,
      components: Int,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    PartialCorrelation.principalComponent(series, components, order)

  def slidingWindowCorrelation(
      series: ParcelTimeSeries,
      window: WindowSpec,
      frameWeights: Option[FrameWeights] = None,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, DynamicConnectivity] =
    DynamicConnectivityEstimators.slidingWindowCorrelation(series, window, frameWeights, order)

  def instantaneousOuterProductStack(
      series: ParcelTimeSeries,
      centerScale: Boolean = true,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, DynamicConnectivity] =
    DynamicConnectivityEstimators.instantaneousOuterProductStack(series, centerScale, order)

  def ewmaCorrelationStack(
      series: ParcelTimeSeries,
      halfLifeFrames: Double,
      warmupFrames: Int = 0,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, DynamicConnectivity] =
    DynamicConnectivityEstimators.ewmaCorrelationStack(series, halfLifeFrames, warmupFrames, order)

  private final case class ShrinkageResult(matrix: DMat, alpha: Double)

  private[connectivity] def normalizedWeights(
      series: ParcelTimeSeries,
      frameWeights: Option[FrameWeights]
  ): Either[ConnectivityError, Array[Double]] =
    val sampleCount = series.timeAxis.sampleCount
    frameWeights match
      case None =>
        Right(Array.fill(sampleCount)(1.0 / sampleCount.toDouble))
      case Some(weights) =>
        if weights.timeAxis.sampleCount != sampleCount then
          Left(ConnectivityError.AxisMismatch("frame weights must match parcel time series sample count"))
        else
          val out = new Array[Double](sampleCount)
          var i = 0
          var sum = 0.0
          var error = Option.empty[ConnectivityError]
          while i < sampleCount && error.isEmpty do
            val value = weights(i)
            if !value.isFinite || value < 0.0 then error = Some(ConnectivityError.InvalidFrameWeight(i, value))
            else
              out(i) = value
              sum += value
            i += 1
          if error.isEmpty && (!sum.isFinite || sum <= 0.0) then
            error = Some(ConnectivityError.InvalidScalar("frame weight sum", sum, "must be positive"))
          error match
            case Some(value) => Left(value)
            case None =>
              i = 0
              while i < out.length do
                out(i) /= sum
                i += 1
              Right(out)

  private[connectivity] def weightedCorrelationMatrix(values: DMat, weights: Array[Double]): DMat =
    val rows = values.rows
    val cols = values.cols
    val means = new Array[Double](cols)
    var row = 0
    while row < rows do
      val weight = weights(row)
      var col = 0
      while col < cols do
        means(col) += values(row, col) * weight
        col += 1
      row += 1

    val covariance = new Array[Double](cols * cols)
    row = 0
    while row < rows do
      val weight = weights(row)
      var left = 0
      while left < cols do
        val centeredLeft = values(row, left) - means(left)
        var right = 0
        while right < cols do
          covariance(left * cols + right) +=
            centeredLeft * (values(row, right) - means(right)) * weight
          right += 1
        left += 1
      row += 1

    val out = Matrix.newBuilder(cols, cols)
    row = 0
    while row < cols do
      val rowVariance = Math.max(covariance(row * cols + row), 1e-16)
      var col = 0
      while col < cols do
        val colVariance = Math.max(covariance(col * cols + col), 1e-16)
        out(row, col) = covariance(row * cols + col) / (Math.sqrt(rowVariance) * Math.sqrt(colVariance))
        col += 1
      out(row, row) = 1.0
      row += 1
    out.result()

  private def diagonalShrink(correlation: DMat): ShrinkageResult =
    val cols = correlation.cols
    var count = 0
    var sum = 0.0
    var row = 0
    while row < cols do
      var col = row + 1
      while col < cols do
        sum += correlation(row, col)
        count += 1
        col += 1
      row += 1

    val mean = if count == 0 then 0.0 else sum / count.toDouble
    var varianceSum = 0.0
    var meanSquareSum = 0.0
    row = 0
    while row < cols do
      var col = row + 1
      while col < cols do
        val value = correlation(row, col)
        varianceSum += (value - mean) * (value - mean)
        meanSquareSum += value * value
        col += 1
      row += 1

    val variance =
      if count <= 1 then 0.0 else varianceSum / (count - 1).toDouble
    val meanSquare =
      if count == 0 then 0.0 else meanSquareSum / count.toDouble
    val denom = variance + meanSquare
    val alpha =
      if !denom.isFinite || denom <= 0.0 then 0.0
      else Math.max(0.0, Math.min(1.0, variance / denom))

    val out = Matrix.newBuilder(correlation.rows, correlation.cols)
    row = 0
    while row < cols do
      var col = 0
      while col < cols do
        out(row, col) =
          if row == col then 1.0
          else (1.0 - alpha) * correlation(row, col)
        col += 1
      row += 1
    ShrinkageResult(out.result(), alpha)
