package scalafim.connectivity

import gale.linalg.{DMat, Matrix, QROptions, QRPivoting}
import gale.linalg.{DVec, Vec}

final class ConnectivityDesign private (
    val effectLabel: String,
    val effect: DMat,
    val nuisance: DMat
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
      effect: DMat,
      nuisance: Option[DMat] = None
  ): Either[ConnectivityError, ConnectivityDesign] =
    val label = effectLabel.trim
    val z = nuisance.getOrElse(Matrix.zeros(effect.rows, 0))
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

  private final case class ResidualizedDesign(response: DMat, effect: DMat, nuisanceRank: Int)
  private final case class EdgewiseFit(fStatistics: Array[Double], pValues: Vector[Double], df1: Int, df2: Int)
  private final case class GlobalFit(statistic: Double)

  private def responseMatrix(set: ConnectivitySet): Either[ConnectivityError, DMat] =
    val out = Matrix.newBuilder(set.subjectCount, set.edgeCount)
    var subjectIndex = 0
    var error = Option.empty[ConnectivityError]
    while subjectIndex < set.subjects.length && error.isEmpty do
      set.subjects(subjectIndex).connectivity.matrix.edgeVector match
        case Left(value) => error = Some(value)
        case Right(vector) =>
          var edge = 0
          while edge < set.edgeCount do
            out(subjectIndex, edge) = vector(edge)
            edge += 1
      subjectIndex += 1
    error match
      case Some(value) => Left(value)
      case None        => Right(out.result())

  private def requireRows(subjects: Int, design: ConnectivityDesign): Either[ConnectivityError, Unit] =
    if design.rows != subjects then
      Left(ConnectivityError.MatrixShapeMismatch(s"connectivity design expected $subjects rows, got ${design.rows}"))
    else Right(())

  private def residualizeDesign(
      response: DMat,
      design: ConnectivityDesign
  ): Either[ConnectivityError, ResidualizedDesign] =
    if design.nuisance.cols == 0 then Right(ResidualizedDesign(response, design.effect, 0))
    else
      val qr = design.nuisance.qr(QROptions(QRPivoting.Column, Some(1e-7)))
      val rank = qr.diagnostics.rank.getOrElse(math.min(design.nuisance.rows, design.nuisance.cols))
      if rank < design.nuisance.cols then
        Left(ConnectivityError.InvalidPlan(s"nuisance design is rank deficient: $rank < ${design.nuisance.cols}"))
      else
        for
          residualResponse <- qr.residualize(response).left.map(error => ConnectivityError.InvalidPlan(error.getMessage))
          residualEffect <- qr.residualize(design.effect).left.map(error => ConnectivityError.InvalidPlan(error.getMessage))
        yield ResidualizedDesign(residualResponse, residualEffect, rank)

  private def fitEffect(
      effect: DMat,
      response: DMat,
      nuisanceRank: Int
  ): Either[ConnectivityError, EdgewiseFit] =
    val q = effect.cols
    val df2 = response.rows - nuisanceRank - q
    if df2 <= 0 then Left(ConnectivityError.InvalidPlan(s"edgewise test has non-positive residual df: $df2"))
    else
      val qr = effect.qr(QROptions(QRPivoting.Column, Some(1e-7)))
      val rank = qr.diagnostics.rank.getOrElse(math.min(effect.rows, effect.cols))
      if rank < q then Left(ConnectivityError.InvalidPlan(s"effect design is rank deficient: $rank < $q"))
      else
        val xtx = effect.t * effect
        val xty = effect.t * response
        ConnectivityNumerics.invertSymmetricPositiveDefinite(xtx).map: inv =>
          val beta = inv * xty
          val fitted = effect * beta
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

  private def effectProjector(effect: DMat): Either[ConnectivityError, DMat] =
    val qr = effect.qr(QROptions(QRPivoting.Column, Some(1e-7)))
    val rank = qr.diagnostics.rank.getOrElse(math.min(effect.rows, effect.cols))
    if rank < effect.cols then Left(ConnectivityError.InvalidPlan(s"effect design is rank deficient: $rank < ${effect.cols}"))
    else
      val xtx = effect.t * effect
      ConnectivityNumerics.invertSymmetricPositiveDefinite(xtx).map: inv =>
        (effect * inv) * effect.t

  private def subjectKernel(response: DMat): DMat =
    val raw = response * response.t
    val scale = 1.0 / Math.max(1, response.cols).toDouble
    val out = Matrix.newBuilder(raw.rows, raw.cols)
    var row = 0
    while row < raw.rows do
      var col = 0
      while col < raw.cols do
        out(row, col) = raw(row, col) * scale
        col += 1
      row += 1
    out.result()

  private def principalScores(
      response: DMat,
      rank: Option[Int],
      tolerance: Double
  ): Either[ConnectivityError, DMat] =
    val kernel = response * response.t
    ConnectivityNumerics.symmetricEigen(kernel, tolerance).flatMap: eigen =>
      val positive = countAbove(eigen.eigenvalues, tolerance)
      val requested = rank.getOrElse(Math.min(positive, Math.min(response.rows, 50)))
      val k = Math.min(requested, positive)
      if k <= 0 then Left(ConnectivityError.InvalidPlan("residualized connectivity response has zero numerical rank"))
      else
        val out = Matrix.newBuilder(response.rows, k)
        var col = 0
        while col < k do
          // Gale's symmetric eigenpairs are ascending; preserve the previous
          // largest-first principal-score contract explicitly.
          val sourceCol = eigen.eigenvalues.length - 1 - col
          val scale = Math.sqrt(Math.max(eigen.eigenvalues(sourceCol), 0.0))
          var row = 0
          while row < response.rows do
            out(row, col) = eigen.eigenvectors(row, sourceCol) * scale
            row += 1
          col += 1
        Right(out.result())

  private def countAbove(values: DVec, threshold: Double): Int =
    var count = 0
    var i = 0
    while i < values.length do
      if values(i) > threshold then count += 1
      i += 1
    count

  private def fitMultivariateEffect(
      effect: DMat,
      response: DMat,
      nuisanceRank: Int
  ): Either[ConnectivityError, GlobalFit] =
    val q = effect.cols
    val df2 = response.rows - nuisanceRank - q
    if df2 <= 0 then Left(ConnectivityError.InvalidPlan(s"PC-MANOVA has non-positive residual df: $df2"))
    else
      val qr = effect.qr(QROptions(QRPivoting.Column, Some(1e-7)))
      val rank = qr.diagnostics.rank.getOrElse(math.min(effect.rows, effect.cols))
      if rank < q then Left(ConnectivityError.InvalidPlan(s"effect design is rank deficient: $rank < $q"))
      else
        val xtx = effect.t * effect
        val xty = effect.t * response
        ConnectivityNumerics.invertSymmetricPositiveDefinite(xtx).flatMap: inv =>
          val beta = inv * xty
          val fitted = effect * beta
          val residual = subtract(response, fitted)
          val h = fitted.t * fitted
          val e = residual.t * residual
          ConnectivityNumerics.invertSymmetricPositiveDefinite(add(h, e)).map: invTotal =>
            GlobalFit(trace(h * invTotal))

  private def subtract(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) - right(row, col)
        col += 1
      row += 1
    out.result()

  private def add(left: DMat, right: DMat): DMat =
    val out = Matrix.newBuilder(left.rows, left.cols)
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out(row, col) = left(row, col) + right(row, col)
        col += 1
      row += 1
    out.result()

  private def trace(matrix: DMat): Double =
    var out = 0.0
    var i = 0
    while i < Math.min(matrix.rows, matrix.cols) do
      out += matrix(i, i)
      i += 1
    out

  private def sumProduct(left: DMat, right: DMat): Double =
    var out = 0.0
    var row = 0
    while row < left.rows do
      var col = 0
      while col < left.cols do
        out += left(row, col) * right(row, col)
        col += 1
      row += 1
    out
