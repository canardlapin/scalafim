package scalafim.connectivity

import gale.linalg.{DMat, Matrix}

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

  private def ridgePrecision(correlation: DMat, lambda: Double): Either[ConnectivityError, DMat] =
    val n = correlation.rows
    val symmetric = ConnectivityNumerics.symmetrize(correlation)
    val out = Matrix.newBuilder(n, n)
    var row = 0
    while row < n do
      var col = 0
      while col < n do
        out(row, col) = symmetric(row, col) + (if row == col then lambda else 0.0)
        col += 1
      row += 1
    ConnectivityNumerics.invertSymmetricPositiveDefinite(out.result())

  private def precisionToPartial(precision: DMat): DMat =
    val n = precision.rows
    val out = Matrix.newBuilder(n, n)
    var row = 0
    while row < n do
      val rowScale = Math.sqrt(Math.max(precision(row, row), 1e-16))
      var col = 0
      while col < n do
        val colScale = Math.sqrt(Math.max(precision(col, col), 1e-16))
        out(row, col) =
          if row == col then 1.0
          else -precision(row, col) / (rowScale * colScale)
        col += 1
      row += 1
    out.result()

  private def pcRegressionCoefficients(z: DMat, components: Int): Either[ConnectivityError, DMat] =
    val samples = z.rows
    val nodes = z.cols
    val out = Matrix.newBuilder(nodes, nodes)
    var target = 0
    var error = Option.empty[ConnectivityError]
    while target < nodes && error.isEmpty do
      val controls = controlMatrix(z, target)
      val p = controls.cols
      val k = Math.min(components, p)
      val gram = controls.t * controls
      ConnectivityNumerics.symmetricEigen(gram) match
        case Left(value) => error = Some(value)
        case Right(eigen) =>
          // Gale returns symmetric eigenpairs in ascending algebraic order;
          // principal-component regression needs the largest-eigenvalue subspace.
          val vectors = lastColumns(eigen.eigenvectors, k)
          val scores = controls * vectors
          val scoreGram = scores.t * scores
          ConnectivityNumerics.invertSymmetricPositiveDefinite(scoreGram) match
            case Left(value) => error = Some(value)
            case Right(inv) =>
              val y = targetColumn(z, target)
              val xty = scores.t * y
              val betaPc = inv * xty
              val beta = vectors * betaPc
              var control = 0
              var original = 0
              while original < nodes do
                if original != target then
                  out(original, target) = beta(control, 0)
                  control += 1
                original += 1
      target += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  private def coefficientsToSymmetricPartial(coefficients: DMat): DMat =
    val n = coefficients.rows
    val out = Matrix.newBuilder(n, n)
    var row = 0
    while row < n do
      var col = 0
      while col < n do
        out(row, col) =
          if row == col then 1.0
          else 0.5 * (coefficients(row, col) + coefficients(col, row))
        col += 1
      row += 1
    out.result()

  private def controlMatrix(values: DMat, excludedCol: Int): DMat =
    val out = Matrix.newBuilder(values.rows, values.cols - 1)
    var row = 0
    while row < values.rows do
      var sourceCol = 0
      var targetCol = 0
      while sourceCol < values.cols do
        if sourceCol != excludedCol then
          out(row, targetCol) = values(row, sourceCol)
          targetCol += 1
        sourceCol += 1
      row += 1
    out.result()

  private def targetColumn(values: DMat, col: Int): DMat =
    val out = Matrix.newBuilder(values.rows, 1)
    var row = 0
    while row < values.rows do
      out(row, 0) = values(row, col)
      row += 1
    out.result()

  private def lastColumns(values: DMat, count: Int): DMat =
    val out = Matrix.newBuilder(values.rows, count)
    val start = values.cols - count
    var row = 0
    while row < values.rows do
      var col = 0
      while col < count do
        out(row, col) = values(row, start + col)
        col += 1
      row += 1
    out.result()
