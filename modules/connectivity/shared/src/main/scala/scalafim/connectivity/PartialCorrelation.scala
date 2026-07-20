package scalafim.connectivity

import scalafim.linalg.DoubleMatrix

object PartialCorrelation:
  def ridge(
      series: ParcelTimeSeries,
      lambda: Double,
      frameWeights: Option[FrameWeights] = None,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    if !lambda.isFinite || lambda < 0.0 then
      Left(ConnectivityError.InvalidScalar("ridge partial-correlation lambda", lambda, "must be finite and non-negative"))
    else
      for
        weights <- ConnectivityEstimators.normalizedWeights(series, frameWeights)
        correlation = ConnectivityEstimators.weightedCorrelationMatrix(series.values, weights)
        precision <- ridgePrecision(correlation, lambda)
        partial = precisionToPartial(precision)
        space <- EdgeSpace.undirected(series.nodeAxis, order)
        matrix <- ConnectivityMatrix.from(
          partial,
          space,
          measure = ConnectivityMeasure.partialCorrelation,
          diagonalPolicy = DiagonalPolicy.Unit
        )
        estimator <- EstimatorSpec.ridgePartialCorrelation(lambda)
      yield StaticConnectivity(
        matrix,
        Some(estimator),
        ConnectivityDiagnostics(
          warnings = Vector.empty,
          convergence = None,
          metrics = Map("partial.lambda" -> lambda)
        )
      )

  def principalComponent(
      series: ParcelTimeSeries,
      components: Int,
      order: VectorizationOrder = VectorizationOrder.ScalaNative
  ): Either[ConnectivityError, StaticConnectivity] =
    if components <= 0 then Left(ConnectivityError.InvalidDimension("PC partial-correlation components", components))
    else if series.samples < 2 then Left(ConnectivityError.InvalidDimension("PC partial-correlation sample count", series.samples))
    else if series.nodes < 2 then Left(ConnectivityError.InvalidDimension("PC partial-correlation node count", series.nodes))
    else
      val z = EventTimeSeries.standardized(series.values)
      val effective = Math.max(1, Math.min(components, Math.min(series.nodes - 1, series.samples - 1)))
      for
        coeff <- pcRegressionCoefficients(z, effective)
        partial = coefficientsToSymmetricPartial(coeff)
        space <- EdgeSpace.undirected(series.nodeAxis, order)
        matrix <- ConnectivityMatrix.from(
          partial,
          space,
          measure = ConnectivityMeasure.partialCorrelation,
          diagonalPolicy = DiagonalPolicy.Unit
        )
        estimator <- EstimatorSpec.pcPartialCorrelation(effective)
      yield StaticConnectivity(
        matrix,
        Some(estimator),
        ConnectivityDiagnostics(
          warnings = Vector.empty,
          convergence = None,
          metrics = Map("partial.components" -> effective.toDouble)
        )
      )

  private def ridgePrecision(correlation: DoubleMatrix, lambda: Double): Either[ConnectivityError, DoubleMatrix] =
    val n = correlation.rows
    val out = ConnectivityNumerics.symmetrize(correlation).copyData
    var i = 0
    while i < n do
      out(i * n + i) += lambda
      i += 1
    ConnectivityNumerics.invertSymmetricPositiveDefinite(DoubleMatrix.unsafe(n, n, out))

  private def precisionToPartial(precision: DoubleMatrix): DoubleMatrix =
    val n = precision.rows
    val out = new Array[Double](n * n)
    var row = 0
    while row < n do
      val rowScale = Math.sqrt(Math.max(precision(row, row), 1e-16))
      var col = 0
      while col < n do
        val colScale = Math.sqrt(Math.max(precision(col, col), 1e-16))
        out(row * n + col) =
          if row == col then 1.0
          else -precision(row, col) / (rowScale * colScale)
        col += 1
      row += 1
    DoubleMatrix.unsafe(n, n, out)

  private def pcRegressionCoefficients(z: DoubleMatrix, components: Int): Either[ConnectivityError, DoubleMatrix] =
    val samples = z.rows
    val nodes = z.cols
    val out = new Array[Double](nodes * nodes)
    var target = 0
    var error = Option.empty[ConnectivityError]
    while target < nodes && error.isEmpty do
      val controls = controlMatrix(z, target)
      val p = controls.cols
      val k = Math.min(components, p)
      val gram = DoubleMatrix.transposeMultiply(controls, controls)
      ConnectivityNumerics.symmetricEigen(gram) match
        case Left(value) => error = Some(value)
        case Right(eigen) =>
          val vectors = firstColumns(eigen.vectors, k)
          val scores = DoubleMatrix.multiply(controls, vectors)
          val scoreGram = DoubleMatrix.transposeMultiply(scores, scores)
          ConnectivityNumerics.invertSymmetricPositiveDefinite(scoreGram) match
            case Left(value) => error = Some(value)
            case Right(inv) =>
              val y = targetColumn(z, target)
              val xty = DoubleMatrix.transposeMultiply(scores, y)
              val betaPc = DoubleMatrix.multiply(inv, xty)
              val beta = DoubleMatrix.multiply(vectors, betaPc)
              var control = 0
              var original = 0
              while original < nodes do
                if original != target then
                  out(original * nodes + target) = beta(control, 0)
                  control += 1
                original += 1
      target += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(DoubleMatrix.unsafe(nodes, nodes, out))

  private def coefficientsToSymmetricPartial(coefficients: DoubleMatrix): DoubleMatrix =
    val n = coefficients.rows
    val out = new Array[Double](n * n)
    var row = 0
    while row < n do
      var col = 0
      while col < n do
        out(row * n + col) =
          if row == col then 1.0
          else 0.5 * (coefficients(row, col) + coefficients(col, row))
        col += 1
      row += 1
    DoubleMatrix.unsafe(n, n, out)

  private def controlMatrix(values: DoubleMatrix, excludedCol: Int): DoubleMatrix =
    val out = new Array[Double](values.rows * (values.cols - 1))
    var row = 0
    while row < values.rows do
      var sourceCol = 0
      var targetCol = 0
      while sourceCol < values.cols do
        if sourceCol != excludedCol then
          out(row * (values.cols - 1) + targetCol) = values(row, sourceCol)
          targetCol += 1
        sourceCol += 1
      row += 1
    DoubleMatrix.unsafe(values.rows, values.cols - 1, out)

  private def targetColumn(values: DoubleMatrix, col: Int): DoubleMatrix =
    val out = new Array[Double](values.rows)
    var row = 0
    while row < values.rows do
      out(row) = values(row, col)
      row += 1
    DoubleMatrix.unsafe(values.rows, 1, out)

  private def firstColumns(values: DoubleMatrix, count: Int): DoubleMatrix =
    val out = new Array[Double](values.rows * count)
    var row = 0
    while row < values.rows do
      var col = 0
      while col < count do
        out(row * count + col) = values(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(values.rows, count, out)
