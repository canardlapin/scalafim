package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

final case class EventWeighting private (quantile: Double, power: Double):
  def description: String =
    s"q=${ConnectivityText.double(quantile)}:power=${ConnectivityText.double(power)}"

object EventWeighting:
  val default: EventWeighting =
    unsafe(0.97, 1.0)

  def from(quantile: Double, power: Double = 1.0): Either[ConnectivityError, EventWeighting] =
    if !quantile.isFinite || quantile <= 0.0 || quantile >= 1.0 then
      Left(ConnectivityError.InvalidScalar("event quantile", quantile, "must lie strictly between 0 and 1"))
    else if !power.isFinite || power <= 0.0 then
      Left(ConnectivityError.InvalidScalar("event weighting power", power, "must be finite and positive"))
    else Right(EventWeighting(quantile, power))

  def unsafe(quantile: Double, power: Double = 1.0): EventWeighting =
    from(quantile, power).fold(error => throw new IllegalArgumentException(error.message), identity)

object EventTimeSeries:
  def standardized(values: DMat): DMat =
    val rows = values.rows
    val cols = values.cols
    val means = new Array[Double](cols)
    var row = 0
    while row < rows do
      var col = 0
      while col < cols do
        means(col) += values(row, col)
        col += 1
      row += 1
    var col = 0
    while col < cols do
      means(col) /= rows.toDouble
      col += 1

    val variances = new Array[Double](cols)
    row = 0
    while row < rows do
      col = 0
      while col < cols do
        val centered = values(row, col) - means(col)
        variances(col) += centered * centered
        col += 1
      row += 1

    val scales = new Array[Double](cols)
    col = 0
    while col < cols do
      val variance = if rows <= 1 then 0.0 else variances(col) / (rows - 1).toDouble
      val sd = Math.sqrt(Math.max(variance, 0.0))
      scales(col) = if sd.isFinite && sd > 0.0 then sd else 1.0
      col += 1

    val out = Matrix.newBuilder(rows, cols)
    row = 0
    while row < rows do
      col = 0
      while col < cols do
        out(row, col) = (values(row, col) - means(col)) / scales(col)
        col += 1
      row += 1
    out.result()

  def energy(standardizedValues: DMat): Either[ConnectivityError, Vector[Double]] =
    if standardizedValues.rows <= 0 then Left(ConnectivityError.InvalidDimension("ETS sample count", standardizedValues.rows))
    else if standardizedValues.cols < 2 then Left(ConnectivityError.InvalidDimension("ETS node count", standardizedValues.cols))
    else
      firstNonFinite(standardizedValues, "ETS values") match
        case Some(error) => Left(error)
        case None =>
          val out = Vector.newBuilder[Double]
          out.sizeHint(standardizedValues.rows)
          var row = 0
          while row < standardizedValues.rows do
            var sum2 = 0.0
            var sum4 = 0.0
            var col = 0
            while col < standardizedValues.cols do
              val value = standardizedValues(row, col)
              val square = value * value
              sum2 += square
              sum4 += square * square
              col += 1
            out += 0.5 * (sum2 * sum2 - sum4)
            row += 1
          Right(out.result())

  def eventIndices(energy: Vector[Double], quantile: Double): Either[ConnectivityError, Vector[SampleIndex]] =
    ConnectivityNumerics.quantile(energy.toArray, quantile).map: threshold =>
      val out = Vector.newBuilder[SampleIndex]
      var i = 0
      while i < energy.length do
        if energy(i) >= threshold then out += SampleIndex.unsafe(i)
        i += 1
      out.result()

  def connectivity(
      series: ParcelTimeSeries,
      eventIndices: Iterable[SampleIndex] = Vector.empty,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    val z = standardized(series.values)
    val selected = eventIndices.toVector
    if selected.exists(index => index.value >= series.samples) then
      Left(ConnectivityError.InvalidPlan("ETS event index exceeds sample count"))
    else
      for
        matrix <- meanOuterProduct(z, selected, lag = 0)
        space <- EdgeSpace.undirected(series.nodeAxis, order)
        conn <- ConnectivityMatrix.from(
          matrix,
          space,
          measure = ConnectivityMeasure.covariance,
          diagonalPolicy = DiagonalPolicy.Observed
        )
        estimator <- EstimatorSpec.external(
          "ets-connectivity",
          EstimatorOutput.Static(EdgeTopology.Undirected),
          ConnectivityMeasure.covariance,
          Set.empty,
          Vector(DiagnosticKind.EffectiveSampleSize, DiagnosticKind.NumericalWarnings)
        )
      yield StaticConnectivity(
        conn,
        Some(estimator),
        ConnectivityDiagnostics.empty
      )

  def directedConnectivity(
      series: ParcelTimeSeries,
      lag: Int,
      eventIndices: Iterable[SampleIndex] = Vector.empty,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    if lag < 0 then Left(ConnectivityError.InvalidDimension("DETS lag", lag))
    else
      val z = standardized(series.values)
      val selected = eventIndices.toVector
      if selected.exists(index => index.value >= series.samples) then
        Left(ConnectivityError.InvalidPlan("DETS event index exceeds sample count"))
      else
        for
          measure <- ConnectivityMeasure.external(
            "directed-covariance",
            ConnectivityValueScale.Covariance,
            symmetric = false,
            supportsRectangular = false
          )
          matrix <- meanOuterProduct(z, selected, lag)
          space <- EdgeSpace.directed(series.nodeAxis, order)
          conn <- ConnectivityMatrix.from(
            matrix,
            space,
            measure = measure,
            diagonalPolicy = DiagonalPolicy.Observed
          )
          estimator <- EstimatorSpec.external(
            s"dets-connectivity-$lag",
            EstimatorOutput.Static(EdgeTopology.Directed),
            measure,
            Set.empty,
            Vector(DiagnosticKind.EffectiveSampleSize, DiagnosticKind.NumericalWarnings)
          )
        yield StaticConnectivity(conn, Some(estimator), ConnectivityDiagnostics.empty)

  def eventWeightedCorrelation(
      series: ParcelTimeSeries,
      eventWeighting: EventWeighting,
      order: VectorizationOrder
  ): Either[ConnectivityError, StaticConnectivity] =
    val z = standardized(series.values)
    for
      energyValues <- energy(z)
      weights <- eventWeights(energyValues, eventWeighting)
      frameWeights <- FrameWeights.from(weights, series.timeAxis)
      connectivity <- ConnectivityEstimators.weightedCorrelation(series, Some(frameWeights), order)
    yield
      val eventCount = weights.count(_ > 0.0).toDouble
      val threshold = ConnectivityNumerics.quantile(energyValues.toArray, eventWeighting.quantile).toOption.get
      connectivity.copy(
        estimator = Some(EstimatorSpec.eventWeightedCorrelation),
        diagnostics = ConnectivityDiagnostics(
          warnings = connectivity.diagnostics.warnings,
          convergence = None,
          metrics = Map(
            "event.count" -> eventCount,
            "event.energy.threshold" -> threshold,
            "event.quantile" -> eventWeighting.quantile,
            "event.power" -> eventWeighting.power
          )
        )
      )

  private def eventWeights(
      energy: Vector[Double],
      eventWeighting: EventWeighting
  ): Either[ConnectivityError, Vector[Double]] =
    ConnectivityNumerics.quantile(energy.toArray, eventWeighting.quantile).map: threshold =>
      val out = Array.fill(energy.length)(0.0)
      var sum = 0.0
      var i = 0
      while i < energy.length do
        val value = Math.pow(Math.max(energy(i) - threshold, 0.0), eventWeighting.power)
        out(i) = value
        sum += value
        i += 1
      if sum <= 0.0 then
        i = 0
        while i < energy.length do
          out(i) = if energy(i) >= threshold then 1.0 else 0.0
          i += 1
      out.toVector

  private def meanOuterProduct(
      z: DMat,
      eventIndices: Vector[SampleIndex],
      lag: Int
  ): Either[ConnectivityError, DMat] =
    val rows = z.rows
    val cols = z.cols
    val selected =
      if eventIndices.isEmpty then
        val start = if lag == 0 then 0 else lag
        (start until rows).map(SampleIndex.unsafe).toVector
      else eventIndices.filter(_.value >= lag)
    if selected.isEmpty then Left(ConnectivityError.InvalidPlan("no frames remain after event/lag filtering"))
    else
      val out = Matrix.newBuilder(cols, cols)
      var idx = 0
      while idx < selected.length do
        val t = selected(idx).value
        val tp = t - lag
        var row = 0
        while row < cols do
          val left = z(t, row)
          var col = 0
          while col < cols do
            out(row, col) = out(row, col) + left * z(tp, col)
            col += 1
          row += 1
        idx += 1
      val scale = 1.0 / selected.length.toDouble
      var row = 0
      while row < cols do
        var col = 0
        while col < cols do
          out(row, col) = out(row, col) * scale
          col += 1
        row += 1
      Right(out.result())
