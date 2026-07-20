package scalafim.connectivity

import scalafim.linalg.DoubleMatrix
import scalafim.linalg.DoubleVector
import scalafim.linalg.QrDecomposition

final class ConnectivityDesign private (
    val effectLabel: String,
    val effect: DoubleMatrix,
    val nuisance: DoubleMatrix
):
  def rows: Int =
    effect.rows

  def effectDf: Int =
    effect.cols

  def nuisanceDf: Int =
    nuisance.cols

object ConnectivityDesign:
  def from(
      effectLabel: String,
      effect: DoubleMatrix,
      nuisance: Option[DoubleMatrix] = None
  ): Either[ConnectivityError, ConnectivityDesign] =
    val label = effectLabel.trim
    val z = nuisance.getOrElse(DoubleMatrix.zeros(effect.rows, 0))
    if label.isEmpty then Left(ConnectivityError.InvalidId("connectivity effect", effectLabel, "must be non-empty"))
    else if effect.rows <= 0 then Left(ConnectivityError.InvalidDimension("connectivity design rows", effect.rows))
    else if effect.cols <= 0 then Left(ConnectivityError.InvalidDimension("connectivity effect columns", effect.cols))
    else if z.rows != effect.rows then
      Left(ConnectivityError.MatrixShapeMismatch(s"connectivity nuisance rows ${z.rows} did not match effect rows ${effect.rows}"))
    else
      firstNonFinite(effect, "connectivity effect design") match
        case Some(error) => Left(error)
        case None =>
          firstNonFinite(z, "connectivity nuisance design") match
            case Some(error) => Left(error)
            case None        => Right(new ConnectivityDesign(label, effect, z))

final case class EdgewiseConnectivityTestResult(
    effectLabel: String,
    df1: Int,
    df2: Int,
    statistics: EdgeVector,
    pValues: EdgeVector,
    fdr: EdgeVector
):
  require(df1 > 0 && df2 > 0, "edgewise test degrees of freedom must be positive")

final case class GlobalConnectivityTestResult(
    method: String,
    effectLabel: String,
    statistic: Double,
    rank: Option[Int],
    r2: Option[Double]
):
  require(statistic.isFinite, "global connectivity statistic must be finite")
  require(r2.forall(value => value.isFinite && value >= 0.0), "global R2 must be finite and non-negative")

object ConnectivityInference:
  def edgewiseF(
      set: ConnectivitySet,
      design: ConnectivityDesign
  ): Either[ConnectivityError, EdgewiseConnectivityTestResult] =
    for
      y <- responseMatrix(set)
      _ <- requireRows(y.rows, design)
      residualized <- residualizeDesign(y, design)
      fit <- fitEffect(residualized.effect, residualized.response, residualized.nuisanceRank)
      stats <- EdgeVector.from(set.edgeSpace, fit.fStatistics.toVector)
      p <- EdgeVector.from(set.edgeSpace, fit.pValues)
      fdr <- EdgeVector.from(set.edgeSpace, ConnectivityNumerics.bhAdjust(fit.pValues))
    yield EdgewiseConnectivityTestResult(
      design.effectLabel,
      fit.df1,
      fit.df2,
      stats,
      p,
      fdr
    )

  def kernelMachine(
      set: ConnectivitySet,
      design: ConnectivityDesign,
      centerKernel: Boolean = true
  ): Either[ConnectivityError, GlobalConnectivityTestResult] =
    for
      y <- responseMatrix(set)
      _ <- requireRows(y.rows, design)
      residualized <- residualizeDesign(y, design)
      projector <- effectProjector(residualized.effect)
    yield
      val rawKernel = subjectKernel(residualized.response)
      val kernel = if centerKernel then ConnectivityNumerics.doubleCenter(rawKernel) else rawKernel
      val stat = sumProduct(projector, kernel)
      val denom = trace(kernel)
      val r2 = if denom > 0.0 then Some(stat / denom) else None
      GlobalConnectivityTestResult("kernel-machine", design.effectLabel, stat, None, r2)

  def pcManova(
      set: ConnectivitySet,
      design: ConnectivityDesign,
      rank: Option[Int] = None,
      tolerance: Double = 1e-10
  ): Either[ConnectivityError, GlobalConnectivityTestResult] =
    for
      y <- responseMatrix(set)
      _ <- requireRows(y.rows, design)
      residualized <- residualizeDesign(y, design)
      scores <- principalScores(residualized.response, rank, tolerance)
      fit <- fitMultivariateEffect(residualized.effect, scores, residualized.nuisanceRank)
    yield GlobalConnectivityTestResult("pc-manova", design.effectLabel, fit.statistic, Some(scores.cols), None)

  private final case class ResidualizedDesign(response: DoubleMatrix, effect: DoubleMatrix, nuisanceRank: Int)
  private final case class EdgewiseFit(fStatistics: Array[Double], pValues: Vector[Double], df1: Int, df2: Int)
  private final case class GlobalFit(statistic: Double)

  private def responseMatrix(set: ConnectivitySet): Either[ConnectivityError, DoubleMatrix] =
    val out = new Array[Double](set.subjectCount * set.edgeCount)
    var subjectIndex = 0
    var error = Option.empty[ConnectivityError]
    while subjectIndex < set.subjects.length && error.isEmpty do
      set.subjects(subjectIndex).connectivity.matrix.edgeVector match
        case Left(value) => error = Some(value)
        case Right(vector) =>
          var edge = 0
          while edge < set.edgeCount do
            out(subjectIndex * set.edgeCount + edge) = vector(edge)
            edge += 1
      subjectIndex += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(DoubleMatrix.unsafe(set.subjectCount, set.edgeCount, out))

  private def requireRows(subjects: Int, design: ConnectivityDesign): Either[ConnectivityError, Unit] =
    if design.rows != subjects then
      Left(ConnectivityError.MatrixShapeMismatch(s"connectivity design expected $subjects rows, got ${design.rows}"))
    else Right(())

  private def residualizeDesign(
      response: DoubleMatrix,
      design: ConnectivityDesign
  ): Either[ConnectivityError, ResidualizedDesign] =
    if design.nuisance.cols == 0 then Right(ResidualizedDesign(response, design.effect, 0))
    else
      val qr = QrDecomposition.decompose(design.nuisance)
      if qr.rank < design.nuisance.cols then
        Left(ConnectivityError.InvalidPlan(s"nuisance design is rank deficient: ${qr.rank} < ${design.nuisance.cols}"))
      else
        Right(ResidualizedDesign(qr.residualize(response), qr.residualize(design.effect), qr.rank))

  private def fitEffect(
      effect: DoubleMatrix,
      response: DoubleMatrix,
      nuisanceRank: Int
  ): Either[ConnectivityError, EdgewiseFit] =
    val q = effect.cols
    val df2 = response.rows - nuisanceRank - q
    if df2 <= 0 then Left(ConnectivityError.InvalidPlan(s"edgewise test has non-positive residual df: $df2"))
    else
      val qr = QrDecomposition.decompose(effect)
      if qr.rank < q then Left(ConnectivityError.InvalidPlan(s"effect design is rank deficient: ${qr.rank} < $q"))
      else
        val xtx = DoubleMatrix.transposeMultiply(effect, effect)
        val xty = DoubleMatrix.transposeMultiply(effect, response)
        ConnectivityNumerics.invertSymmetricPositiveDefinite(xtx).map: inv =>
          val beta = DoubleMatrix.multiply(inv, xty)
          val fitted = DoubleMatrix.multiply(effect, beta)
          val stats = new Array[Double](response.cols)
          val pValues = Vector.newBuilder[Double]
          var col = 0
          while col < response.cols do
            var ssr = 0.0
            var row = 0
            while row < q do
              ssr += beta(row, col) * xty(row, col)
              row += 1
            var sse = 0.0
            row = 0
            while row < response.rows do
              val residual = response(row, col) - fitted(row, col)
              sse += residual * residual
              row += 1
            val f = (Math.max(ssr, 0.0) / q.toDouble) / (Math.max(sse, 1e-16) / df2.toDouble)
            stats(col) = f
            pValues += ConnectivityNumerics.fUpperTail(f, q, df2)
            col += 1
          EdgewiseFit(stats, pValues.result(), q, df2)

  private def effectProjector(effect: DoubleMatrix): Either[ConnectivityError, DoubleMatrix] =
    val qr = QrDecomposition.decompose(effect)
    if qr.rank < effect.cols then Left(ConnectivityError.InvalidPlan(s"effect design is rank deficient: ${qr.rank} < ${effect.cols}"))
    else
      val xtx = DoubleMatrix.transposeMultiply(effect, effect)
      ConnectivityNumerics.invertSymmetricPositiveDefinite(xtx).map: inv =>
        DoubleMatrix.multiply(DoubleMatrix.multiply(effect, inv), effect.transpose)

  private def subjectKernel(response: DoubleMatrix): DoubleMatrix =
    val raw = DoubleMatrix.multiply(response, response.transpose)
    val scale = 1.0 / Math.max(1, response.cols).toDouble
    val out = raw.copyData
    var i = 0
    while i < out.length do
      out(i) *= scale
      i += 1
    DoubleMatrix.unsafe(raw.rows, raw.cols, out)

  private def principalScores(
      response: DoubleMatrix,
      rank: Option[Int],
      tolerance: Double
  ): Either[ConnectivityError, DoubleMatrix] =
    val kernel = DoubleMatrix.multiply(response, response.transpose)
    ConnectivityNumerics.symmetricEigen(kernel, tolerance).flatMap: eigen =>
      val positive = countAbove(eigen.values, tolerance)
      val requested = rank.getOrElse(Math.min(positive, Math.min(response.rows, 50)))
      val k = Math.min(requested, positive)
      if k <= 0 then Left(ConnectivityError.InvalidPlan("residualized connectivity response has zero numerical rank"))
      else
        val out = new Array[Double](response.rows * k)
        var col = 0
        while col < k do
          val scale = Math.sqrt(Math.max(eigen.values(col), 0.0))
          var row = 0
          while row < response.rows do
            out(row * k + col) = eigen.vectors(row, col) * scale
            row += 1
          col += 1
        Right(DoubleMatrix.unsafe(response.rows, k, out))

  private def countAbove(values: DoubleVector, threshold: Double): Int =
    var count = 0
    var i = 0
    while i < values.length do
      if values(i) > threshold then count += 1
      i += 1
    count

  private def fitMultivariateEffect(
      effect: DoubleMatrix,
      response: DoubleMatrix,
      nuisanceRank: Int
  ): Either[ConnectivityError, GlobalFit] =
    val q = effect.cols
    val df2 = response.rows - nuisanceRank - q
    if df2 <= 0 then Left(ConnectivityError.InvalidPlan(s"PC-MANOVA has non-positive residual df: $df2"))
    else
      val qr = QrDecomposition.decompose(effect)
      if qr.rank < q then Left(ConnectivityError.InvalidPlan(s"effect design is rank deficient: ${qr.rank} < $q"))
      else
        val xtx = DoubleMatrix.transposeMultiply(effect, effect)
        val xty = DoubleMatrix.transposeMultiply(effect, response)
        ConnectivityNumerics.invertSymmetricPositiveDefinite(xtx).flatMap: inv =>
          val beta = DoubleMatrix.multiply(inv, xty)
          val fitted = DoubleMatrix.multiply(effect, beta)
          val residual = subtract(response, fitted)
          val h = DoubleMatrix.transposeMultiply(fitted, fitted)
          val e = DoubleMatrix.transposeMultiply(residual, residual)
          ConnectivityNumerics.invertSymmetricPositiveDefinite(add(h, e)).map: invTotal =>
            GlobalFit(trace(DoubleMatrix.multiply(h, invTotal)))

  private def subtract(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    val out = left.copyData
    var i = 0
    while i < out.length do
      out(i) -= right.dataArray(i)
      i += 1
    DoubleMatrix.unsafe(left.rows, left.cols, out)

  private def add(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    val out = left.copyData
    var i = 0
    while i < out.length do
      out(i) += right.dataArray(i)
      i += 1
    DoubleMatrix.unsafe(left.rows, left.cols, out)

  private def trace(matrix: DoubleMatrix): Double =
    var out = 0.0
    var i = 0
    while i < Math.min(matrix.rows, matrix.cols) do
      out += matrix(i, i)
      i += 1
    out

  private def sumProduct(left: DoubleMatrix, right: DoubleMatrix): Double =
    var out = 0.0
    var i = 0
    while i < left.dataArray.length do
      out += left.dataArray(i) * right.dataArray(i)
      i += 1
    out
