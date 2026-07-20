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
