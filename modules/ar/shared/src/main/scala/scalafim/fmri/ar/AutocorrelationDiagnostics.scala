package scalafim.fmri.ar

import gale.linalg.{DMat, Matrix}

enum AcfAggregation:
  case Mean
  case Median
  case None

final case class AcorrDiagnostics(
    lags: Vector[Int],
    acf: DMat,
    confidenceInterval: Double,
    aggregation: AcfAggregation
):
  require(lags.nonEmpty, "diagnostics must contain at least one lag")
  require(acf.rows == lags.length, "ACF rows must match lag count")

object AcorrDiagnostics:

  def compute(
      residuals: DMat,
      maxLag: Int = 20,
      aggregation: AcfAggregation = AcfAggregation.Mean
  ): AcorrDiagnostics =
    require(residuals.rows > 1, "ACF diagnostics require at least two rows")
    require(residuals.cols > 0, "ACF diagnostics require at least one column")
    require(maxLag >= 1, "maxLag must be at least one")
    val lagCount = math.min(maxLag, residuals.rows - 1)
    val values =
      aggregation match
        case AcfAggregation.None =>
          val out = DMat.newBuilder(lagCount, residuals.cols)
          var col = 0
          while col < residuals.cols do
            val series = column(residuals, col)
            val acf = acfSeries(series, lagCount)
            var lag = 0
            while lag < lagCount do
              out(lag, col) = acf(lag)
              lag += 1
            col += 1
          out.result()

        case AcfAggregation.Mean =>
          val series = rowAggregate(residuals, median = false)
          columnMatrix(acfSeries(series, lagCount))

        case AcfAggregation.Median =>
          val series = rowAggregate(residuals, median = true)
          columnMatrix(acfSeries(series, lagCount))

    AcorrDiagnostics(
      lags = (1 to lagCount).toVector,
      acf = values,
      confidenceInterval = 1.96 / math.sqrt(residuals.rows.toDouble),
      aggregation = aggregation
    )

  /** Run-aware diagnostics. Means are removed once per run while lag products
    * stay inside the supplied contiguous segments, so neither run boundaries
    * nor censor resets create artificial autocorrelation.
    */
  def compute(
      residuals: DMat,
      segments: Vector[TimeSegment],
      maxLag: Int,
      aggregation: AcfAggregation
  ): Either[ArError, AcorrDiagnostics] =
    if residuals.rows <= 1 then Left(ArError.NonPositiveRows(residuals.rows))
    else if maxLag < 1 then Left(ArError.InvalidArLag(maxLag))
    else
      val maxEstimable = segments.map(_.length - 1).maxOption.getOrElse(0)
      val lagCount = math.min(maxLag, residuals.rows - 1)
      val covarianceLag = math.min(lagCount, maxEstimable)
      if lagCount < 1 then Left(ArError.ArOrderNotEstimable(ArOrderValue.unsafe(1), ArLag.Zero))
      else
        val source =
          aggregation match
            case AcfAggregation.None   => residuals
            case AcfAggregation.Mean   => columnMatrix(rowAggregate(residuals, median = false))
            case AcfAggregation.Median => columnMatrix(rowAggregate(residuals, median = true))
        val out = Matrix.newBuilder(lagCount, source.cols)
        var col = 0
        var error = Option.empty[ArError]
        while col < source.cols && error.isEmpty do
          val series = Matrix.tabulate(source.rows, 1)((row, _) => source(row, col))
          ArEstimation.autocovariances(series, segments, ArLag.unsafe(covarianceLag)) match
            case Left(value) => error = Some(value)
            case Right(gamma) =>
              val gamma0 = gamma.lagZero
              var lag = 1
              while lag <= lagCount do
                out(lag - 1, col) =
                  if gamma0 <= 0.0 then 0.0
                  else if lag > covarianceLag then 0.0
                  else gamma.at(ArLag.unsafe(lag)) / gamma0
                lag += 1
          col += 1
        error match
          case Some(value) => Left(value)
          case None =>
            Right(
              AcorrDiagnostics(
                lags = (1 to lagCount).toVector,
                acf = out.result(),
                confidenceInterval = 1.96 / math.sqrt(residuals.rows.toDouble),
                aggregation = aggregation
              )
            )

  private def acfSeries(values: Vector[Double], maxLag: Int): Vector[Double] =
    val mean = values.sum / values.length.toDouble
    var denom = 0.0
    values.foreach { value =>
      val centered = value - mean
      denom += centered * centered
    }
    (1 to maxLag).map { lag =>
      if denom == 0.0 then 0.0
      else
        var num = 0.0
        var row = lag
        while row < values.length do
          num += (values(row) - mean) * (values(row - lag) - mean)
          row += 1
        num / denom
    }.toVector

  private def columnMatrix(values: Vector[Double]): DMat =
    Matrix.tabulate(values.length, 1)((row, _) => values(row))

  private def column(matrix: DMat, col: Int): Vector[Double] =
    val out = Vector.newBuilder[Double]
    out.sizeHint(matrix.rows)
    var row = 0
    while row < matrix.rows do
      out += matrix(row, col)
      row += 1
    out.result()

  private def rowAggregate(matrix: DMat, median: Boolean): Vector[Double] =
    val out = Vector.newBuilder[Double]
    out.sizeHint(matrix.rows)
    var row = 0
    while row < matrix.rows do
      if median then
        val values = new Array[Double](matrix.cols)
        var col = 0
        while col < matrix.cols do
          values(col) = matrix(row, col)
          col += 1
        scala.util.Sorting.quickSort(values)
        val mid = values.length / 2
        out += (
          if values.length % 2 == 1 then values(mid)
          else (values(mid - 1) + values(mid)) / 2.0
        )
      else
        var sum = 0.0
        var col = 0
        while col < matrix.cols do
          sum += matrix(row, col)
          col += 1
        out += sum / matrix.cols.toDouble
      row += 1
    out.result()
