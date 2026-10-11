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
    compute(residuals, TimeSegments.continuous(residuals.rows), maxLag, aggregation)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  /** Per-voxel autocorrelations, centered once per run and confined to each
    * segment. Every lag uses the same lag-zero energy denominator, matching
    * stats::acf and fmriAR 0.4.1. Aggregation follows normalization, so shared
    * fluctuations do not replace the voxel-level noise estimand.
    *
    * Constant columns report zero in None mode and are excluded from mean and
    * median aggregation; an entirely constant block reports finite zeros.
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
      for
        layout <- NoiseEstimationLayout.allRows(segments, residuals.rows)
        _ <- ArEstimation.validateInputs(residuals, layout)
        acf <- correlations(residuals, layout, math.min(maxLag, residuals.rows - 1), aggregation)
      yield AcorrDiagnostics(
        (1 to acf.rows).toVector,
        acf,
        1.96 / math.sqrt(residuals.rows.toDouble),
        aggregation
      )

  private def correlations(
      residuals: DMat,
      layout: NoiseEstimationLayout,
      lags: Int,
      aggregation: AcfAggregation
  ): Either[ArError, DMat] =
    val out = Matrix.newBuilder(lags, residuals.cols)
    val usable = new Array[Boolean](residuals.cols)
    val means = new Array[Double](layout.runCount)
    var col = 0
    while col < residuals.cols do
      var run = 0
      while run < layout.runCount do
        var total = 0.0
        var count = 0
        layout.segmentsForRun(run).foreach: segment =>
          var row = segment.start
          while row < segment.endExclusive do
            total += residuals(row, col)
            count += 1
            row += 1
        means(run) = total / count.toDouble
        run += 1
      var energy = 0.0
      layout.estimationSegments.foreach: segment =>
        var row = segment.start
        while row < segment.endExclusive do
          val centered = residuals(row, col) - means(segment.runIndex)
          energy += centered * centered
          row += 1
      if !energy.isFinite then return Left(ArError.NonFiniteAutocovariance(ArLag.Zero, energy))
      usable(col) = energy > 0.0
      var lag = 1
      while lag <= lags do
        var product = 0.0
        layout.estimationSegments.foreach: segment =>
          var row = segment.start + lag
          while row < segment.endExclusive do
            val mean = means(segment.runIndex)
            product += (residuals(row, col) - mean) * (residuals(row - lag, col) - mean)
            row += 1
        out(lag - 1, col) = if usable(col) then product / energy else 0.0
        lag += 1
      col += 1
    val perVoxel = out.result()
    aggregation match
      case AcfAggregation.None => Right(perVoxel)
      case other =>
        val count = usable.count(identity)
        val values = new Array[Double](count)
        Right(Matrix.tabulate(lags, 1): (lag, _) =>
          if count == 0 then 0.0
          else
            var index = 0
            var col = 0
            while col < residuals.cols do
              if usable(col) then
                values(index) = perVoxel(lag, col)
                index += 1
              col += 1
            if other == AcfAggregation.Mean then values.sum / count.toDouble
            else
              scala.util.Sorting.quickSort(values)
              val mid = count / 2
              if count % 2 == 1 then values(mid)
              else (values(mid - 1) + values(mid)) / 2.0
        )
