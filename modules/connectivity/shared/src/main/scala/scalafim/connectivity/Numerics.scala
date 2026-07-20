package scalafim.connectivity

import scala.util.Sorting

import gale.linalg.{CholeskyOptions, DMat, Matrix}
import gale.spectral.{Eigen, EigenDecomposition, EigenSelection}

private[connectivity] object ConnectivityNumerics:
  def invertSymmetricPositiveDefinite(
      matrix: DMat,
      tolerance: Double = 1e-10
  ): Either[ConnectivityError, DMat] =
    if matrix.rows != matrix.cols then
      Left(ConnectivityError.MatrixShapeMismatch(s"SPD inverse expected a square matrix, got ${matrix.rows}x${matrix.cols}"))
    else
      val sym = symmetrize(matrix)
      val eye = Matrix.eye(matrix.rows)
      var attempt = 0
      var jitter = 0.0
      var out = Option.empty[DMat]
      var last = Option.empty[String]
      while attempt < 8 && out.isEmpty do
        val candidate =
          if jitter == 0.0 then sym
          else
            val builder = Matrix.newBuilder(sym.rows, sym.cols)
            var row = 0
            while row < sym.rows do
              var col = 0
              while col < sym.cols do
                builder(row, col) = sym(row, col) + (if row == col then jitter else 0.0)
                col += 1
              row += 1
            builder.result()
        candidate.cholesky(CholeskyOptions(tolerance)) match
          case Right(chol) =>
            chol.solve(eye) match
              case Right(value) => out = Some(value)
              case Left(error)  =>
                last = Some(error.getMessage)
                jitter = if jitter == 0.0 then tolerance else jitter * 10.0
          case Left(error) =>
            last = Some(error.getMessage)
            jitter = if jitter == 0.0 then tolerance else jitter * 10.0
        attempt += 1
      out match
        case Some(value) => Right(symmetrize(value))
        case None        => Left(ConnectivityError.InvalidPlan(s"could not invert SPD matrix: ${last.getOrElse("unknown failure")}"))

  def symmetricEigen(
      matrix: DMat,
      tolerance: Double = 1e-10,
      maxSweeps: Int = 100
  ): Either[ConnectivityError, EigenDecomposition] =
    if matrix.rows != matrix.cols then
      Left(ConnectivityError.MatrixShapeMismatch(s"symmetric eigen expected a square matrix, got ${matrix.rows}x${matrix.cols}"))
    else if maxSweeps < 0 then
      Left(ConnectivityError.InvalidScalar("max eigensolver sweeps", maxSweeps.toDouble, "must be non-negative"))
    else
      if !tolerance.isFinite || tolerance < 0.0 then
        Left(ConnectivityError.InvalidScalar("eigensolver tolerance", tolerance, "must be finite and non-negative"))
      else
        Eigen
          .eigSymmetric(symmetrize(matrix), EigenSelection.All)
          .left
          .map(error => ConnectivityError.InvalidPlan(s"symmetric eigensolver failed: ${error.getMessage}"))

  def symmetrize(matrix: DMat): DMat =
    require(matrix.rows == matrix.cols, "matrix must be square")
    val n = matrix.rows
    val out = Matrix.newBuilder(n, n)
    var row = 0
    while row < n do
      var col = row + 1
      while col < n do
        val value = 0.5 * (matrix(row, col) + matrix(col, row))
        out(row, col) = value
        out(col, row) = value
        col += 1
      out(row, row) = matrix(row, row)
      row += 1
    out.result()

  def quantile(values: Array[Double], p: Double): Either[ConnectivityError, Double] =
    if values.isEmpty then Left(ConnectivityError.InvalidDimension("quantile sample count", 0))
    else if !p.isFinite || p < 0.0 || p > 1.0 then
      Left(ConnectivityError.InvalidScalar("quantile probability", p, "must lie in [0, 1]"))
    else
      val sorted = values.clone
      Sorting.quickSort(sorted)
      val h = (sorted.length - 1).toDouble * p
      val lo = Math.floor(h).toInt
      val hi = Math.ceil(h).toInt
      val frac = h - lo.toDouble
      Right(sorted(lo) * (1.0 - frac) + sorted(hi) * frac)

  def fUpperTail(f: Double, df1: Int, df2: Int): Double =
    if !f.isFinite || f < 0.0 || df1 <= 0 || df2 <= 0 then Double.NaN
    else
      val x = df2.toDouble / (df2.toDouble + df1.toDouble * f)
      regularizedIncompleteBeta(df2.toDouble * 0.5, df1.toDouble * 0.5, x)

  def bhAdjust(pValues: Vector[Double]): Vector[Double] =
    val m = pValues.length
    val order = pValues.indices.toArray
    Sorting.stableSort(order, (left: Int, right: Int) => pValues(left) < pValues(right))
    val adjusted = Array.fill(m)(Double.NaN)
    var previous = 1.0
    var rank = m
    var pos = m - 1
    while pos >= 0 do
      val index = order(pos)
      val p = pValues(index)
      val value =
        if !p.isFinite then Double.NaN
        else Math.min(previous, Math.min(1.0, p * m.toDouble / rank.toDouble))
      adjusted(index) = value
      if value.isFinite then previous = value
      rank -= 1
      pos -= 1
    adjusted.toVector

  def doubleCenter(matrix: DMat): DMat =
    val rows = matrix.rows
    val cols = matrix.cols
    val rowMeans = new Array[Double](rows)
    val colMeans = new Array[Double](cols)
    var grand = 0.0
    var row = 0
    while row < rows do
      var col = 0
      while col < cols do
        val value = matrix(row, col)
        rowMeans(row) += value
        colMeans(col) += value
        grand += value
        col += 1
      row += 1
    row = 0
    while row < rows do
      rowMeans(row) /= cols.toDouble
      row += 1
    var col = 0
    while col < cols do
      colMeans(col) /= rows.toDouble
      col += 1
    grand /= (rows * cols).toDouble

    val out = Matrix.newBuilder(rows, cols)
    row = 0
    while row < rows do
      col = 0
      while col < cols do
        out(row, col) = matrix(row, col) - rowMeans(row) - colMeans(col) + grand
        col += 1
      row += 1
    out.result()

  private def regularizedIncompleteBeta(a: Double, b: Double, x: Double): Double =
    if x <= 0.0 then 0.0
    else if x >= 1.0 then 1.0
    else
      val bt =
        Math.exp(logGamma(a + b) - logGamma(a) - logGamma(b) + a * Math.log(x) + b * Math.log1p(-x))
      val result =
        if x < (a + 1.0) / (a + b + 2.0) then
          bt * betaContinuedFraction(a, b, x) / a
        else
          1.0 - bt * betaContinuedFraction(b, a, 1.0 - x) / b
      Math.max(0.0, Math.min(1.0, result))

  private def betaContinuedFraction(a: Double, b: Double, x: Double): Double =
    val maxIterations = 200
    val epsilon = 3e-14
    val tiny = 1e-300
    var c = 1.0
    var d = 1.0 - (a + b) * x / (a + 1.0)
    if Math.abs(d) < tiny then d = tiny
    d = 1.0 / d
    var h = d
    var m = 1
    var done = false
    while m <= maxIterations && !done do
      val m2 = 2 * m
      var aa = m.toDouble * (b - m.toDouble) * x / ((a + m2.toDouble - 1.0) * (a + m2.toDouble))
      d = 1.0 + aa * d
      if Math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if Math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      h *= d * c

      aa = -(a + m.toDouble) * (a + b + m.toDouble) * x / ((a + m2.toDouble) * (a + m2.toDouble + 1.0))
      d = 1.0 + aa * d
      if Math.abs(d) < tiny then d = tiny
      c = 1.0 + aa / c
      if Math.abs(c) < tiny then c = tiny
      d = 1.0 / d
      val delta = d * c
      h *= delta
      done = Math.abs(delta - 1.0) < epsilon
      m += 1
    h

  private def logGamma(x: Double): Double =
    val coefficients = Array(
      676.5203681218851,
      -1259.1392167224028,
      771.32342877765313,
      -176.61502916214059,
      12.507343278686905,
      -0.13857109526572012,
      9.9843695780195716e-6,
      1.5056327351493116e-7
    )
    if x < 0.5 then
      Math.log(Math.PI) - Math.log(Math.sin(Math.PI * x)) - logGamma(1.0 - x)
    else
      val z = x - 1.0
      var a = 0.99999999999980993
      var i = 0
      while i < coefficients.length do
        a += coefficients(i) / (z + i.toDouble + 1.0)
        i += 1
      val t = z + coefficients.length.toDouble - 0.5
      0.5 * Math.log(2.0 * Math.PI) + (z + 0.5) * Math.log(t) - t + Math.log(a)
