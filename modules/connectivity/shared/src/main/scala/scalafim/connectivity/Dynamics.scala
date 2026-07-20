package scalafim.connectivity

import scalafim.linalg.DoubleMatrix

object DynamicConnectivityEstimators:
  def slidingWindowCorrelation(
      series: ParcelTimeSeries,
      window: WindowSpec,
      frameWeights: Option[FrameWeights] = None,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, DynamicConnectivity] =
    for
      axis <- WindowAxis.sliding(series.timeAxis, window)
      slices <- traverseWindows(axis.windows): timeWindow =>
        windowedSeries(series, timeWindow).flatMap: windowSeries =>
          val windowWeights = frameWeights.map(weights => sliceWeights(weights, timeWindow)).sequence
          windowWeights.flatMap: weights =>
            ConnectivityEstimators.weightedCorrelation(windowSeries, weights, order).map: connectivity =>
              DynamicSlice(timeWindow, connectivity.copy(estimator = Some(EstimatorSpec.slidingWindowCorrelation)))
      dynamic <- DynamicConnectivity.from(slices)
    yield dynamic

  def instantaneousOuterProductStack(
      series: ParcelTimeSeries,
      centerScale: Boolean = true,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, DynamicConnectivity] =
    val values = if centerScale then EventTimeSeries.standardized(series.values) else series.values
    for
      space <- EdgeSpace.undirected(series.nodeAxis, order)
      slices <- buildSlices(series.samples): index =>
        val matrix = outerSnapshot(values, index)
        ConnectivityMatrix.from(
          matrix,
          space,
          measure = ConnectivityMeasure.correlation,
          diagonalPolicy = DiagonalPolicy.Unit
        ).map: conn =>
          DynamicSlice.unsafe(
            index,
            StaticConnectivity(
              conn,
              Some(EstimatorSpec.instantaneousOuterProduct),
              ConnectivityDiagnostics(metrics = Map("center.scale" -> (if centerScale then 1.0 else 0.0)), warnings = Vector.empty, convergence = None)
            )
          )
      dynamic <- DynamicConnectivity.from(slices)
    yield dynamic

  def ewmaCorrelationStack(
      series: ParcelTimeSeries,
      halfLifeFrames: Double,
      warmupFrames: Int = 0,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, DynamicConnectivity] =
    if !halfLifeFrames.isFinite || halfLifeFrames <= 0.0 then
      Left(ConnectivityError.InvalidScalar("EWMA half-life frames", halfLifeFrames, "must be finite and positive"))
    else if warmupFrames < 0 then Left(ConnectivityError.InvalidDimension("EWMA warmup frames", warmupFrames))
    else if warmupFrames >= series.samples then
      Left(ConnectivityError.InvalidPlan("EWMA warmup must leave at least one emitted dynamic slice"))
    else
      val values = EventTimeSeries.standardized(series.values)
      for
        space <- EdgeSpace.undirected(series.nodeAxis, order)
        estimator <- EstimatorSpec.ewmaCorrelation(halfLifeFrames)
        slices <- ewmaSlices(values, series.timeAxis, space, estimator, halfLifeFrames, warmupFrames)
        dynamic <- DynamicConnectivity.from(slices)
      yield dynamic

  private def windowedSeries(series: ParcelTimeSeries, window: TimeWindow): Either[ConnectivityError, ParcelTimeSeries] =
    val rows = new Array[Double](window.length * series.nodes)
    var local = 0
    while local < window.length do
      val source = window.start.value + local
      System.arraycopy(series.values.dataArray, source * series.nodes, rows, local * series.nodes, series.nodes)
      local += 1
    val time = TimeAxis.fromSamplePeriod(window.length, series.timeAxis.samplePeriod)
    time.flatMap: axis =>
      ParcelTimeSeries.from(DoubleMatrix.unsafe(window.length, series.nodes, rows), series.nodeAxis, axis)

  private def sliceWeights(weights: FrameWeights, window: TimeWindow): Either[ConnectivityError, FrameWeights] =
    val out = new Array[Double](window.length)
    var i = 0
    while i < window.length do
      out(i) = weights(window.start.value + i)
      i += 1
    TimeAxis.fromSamplePeriod(window.length, weights.timeAxis.samplePeriod).flatMap: timeAxis =>
      FrameWeights.from(out.toVector, timeAxis)

  private def outerSnapshot(values: DoubleMatrix, index: Int): DoubleMatrix =
    val nodes = values.cols
    var denom = 0.0
    var col = 0
    while col < nodes do
      val value = values(index, col)
      denom += value * value
      col += 1
    val scale = if denom.isFinite && denom > 0.0 then 1.0 / denom else 1.0
    val out = new Array[Double](nodes * nodes)
    var row = 0
    while row < nodes do
      col = 0
      while col < nodes do
        out(row * nodes + col) =
          if row == col then 1.0 else values(index, row) * values(index, col) * scale
        col += 1
      row += 1
    DoubleMatrix.unsafe(nodes, nodes, out)

  private def ewmaSlices(
      values: DoubleMatrix,
      timeAxis: TimeAxis,
      space: EdgeSpace,
      estimator: EstimatorSpec,
      halfLifeFrames: Double,
      warmupFrames: Int
  ): Either[ConnectivityError, Vector[DynamicSlice]] =
    val nodes = values.cols
    val eta = Math.log(2.0) / halfLifeFrames
    val decay = Math.exp(-eta)
    val update = 1.0 - decay
    val covariance = new Array[Double](nodes * nodes)
    val diagonal = new Array[Double](nodes)
    val out = Vector.newBuilder[DynamicSlice]
    var t = 0
    var error = Option.empty[ConnectivityError]
    while t < values.rows && error.isEmpty do
      var row = 0
      while row < nodes do
        val x = values(t, row)
        diagonal(row) = decay * diagonal(row) + update * x * x
        var col = 0
        while col < nodes do
          covariance(row * nodes + col) = decay * covariance(row * nodes + col) + update * x * values(t, col)
          col += 1
        row += 1
      if t >= warmupFrames then
        val matrix = ewmaCorrelationMatrix(covariance, diagonal, nodes)
        ConnectivityMatrix.from(
          matrix,
          space,
          measure = ConnectivityMeasure.correlation,
          diagonalPolicy = DiagonalPolicy.Unit
        ) match
          case Left(value) => error = Some(value)
          case Right(conn) =>
            out += DynamicSlice.unsafe(
              t,
              StaticConnectivity(
                conn,
                Some(estimator),
                ConnectivityDiagnostics(
                  warnings = Vector.empty,
                  convergence = None,
                  metrics = Map("ewma.half.life.frames" -> halfLifeFrames, "ewma.warmup.frames" -> warmupFrames.toDouble)
                )
              )
            )
      t += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  private def ewmaCorrelationMatrix(covariance: Array[Double], diagonal: Array[Double], nodes: Int): DoubleMatrix =
    val out = new Array[Double](nodes * nodes)
    var row = 0
    while row < nodes do
      var col = 0
      while col < nodes do
        out(row * nodes + col) =
          if row == col then 1.0
          else
            val denom = Math.sqrt(Math.max(diagonal(row), 1e-12)) * Math.sqrt(Math.max(diagonal(col), 1e-12))
            covariance(row * nodes + col) / denom
        col += 1
      row += 1
    ConnectivityNumerics.symmetrize(DoubleMatrix.unsafe(nodes, nodes, out))

  private def traverseWindows(
      windows: Vector[TimeWindow]
  )(f: TimeWindow => Either[ConnectivityError, DynamicSlice]): Either[ConnectivityError, Vector[DynamicSlice]] =
    val out = Vector.newBuilder[DynamicSlice]
    var i = 0
    var error = Option.empty[ConnectivityError]
    while i < windows.length && error.isEmpty do
      f(windows(i)) match
        case Left(value)  => error = Some(value)
        case Right(value) => out += value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  private def buildSlices(count: Int)(f: Int => Either[ConnectivityError, DynamicSlice]): Either[ConnectivityError, Vector[DynamicSlice]] =
    val out = Vector.newBuilder[DynamicSlice]
    var i = 0
    var error = Option.empty[ConnectivityError]
    while i < count && error.isEmpty do
      f(i) match
        case Left(value)  => error = Some(value)
        case Right(value) => out += value
      i += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  extension [A](value: Option[Either[ConnectivityError, A]])
    private def sequence: Either[ConnectivityError, Option[A]] =
      value match
        case None               => Right(None)
        case Some(Left(error))  => Left(error)
        case Some(Right(value)) => Right(Some(value))
