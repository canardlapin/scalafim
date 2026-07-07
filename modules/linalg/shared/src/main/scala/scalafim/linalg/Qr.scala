package scalafim.linalg

private final case class Householder(v: Array[Double], beta: Double)

final class QrDecomposition private (
    val shape: MatrixShape,
    private val reflectors: Array[Householder],
    private val diagR: Array[Double],
    private val packed: Array[Double],
    private val perm: Array[Int],
    val rank: Int
):
  def rows: Int =
    shape.rows

  def cols: Int =
    shape.cols

  def applyQtInPlace(y: Array[Double], yCols: Int): Unit =
    require(yCols >= 0, "yCols must be non-negative")
    require(y.length == rows * yCols, "y length mismatch")

    var k = 0
    while k < reflectors.length do
      val Householder(v, beta) = reflectors(k)
      if beta != 0.0 then
        val len = v.length
        var col = 0
        while col < yCols do
          var dot = 0.0
          var i = 0
          while i < len do
            dot += v(i) * y((k + i) * yCols + col)
            i += 1
          val scale = beta * dot
          i = 0
          while i < len do
            val row = k + i
            y(row * yCols + col) -= scale * v(i)
            i += 1
          col += 1
      k += 1

  def applyQInPlace(y: Array[Double], yCols: Int): Unit =
    require(yCols >= 0, "yCols must be non-negative")
    require(y.length == rows * yCols, "y length mismatch")

    var k = reflectors.length - 1
    while k >= 0 do
      val Householder(v, beta) = reflectors(k)
      if beta != 0.0 then
        val len = v.length
        var col = 0
        while col < yCols do
          var dot = 0.0
          var i = 0
          while i < len do
            dot += v(i) * y((k + i) * yCols + col)
            i += 1
          val scale = beta * dot
          i = 0
          while i < len do
            val row = k + i
            y(row * yCols + col) -= scale * v(i)
            i += 1
          col += 1
      k -= 1

  def residualize(data: DoubleMatrix): DoubleMatrix =
    require(data.rows == rows, s"data rows ${data.rows} != QR rows $rows")
    val out = data.copyData
    applyQtInPlace(out, yCols = data.cols)

    var row = 0
    while row < rank do
      var col = 0
      while col < data.cols do
        out(row * data.cols + col) = 0.0
        col += 1
      row += 1

    applyQInPlace(out, yCols = data.cols)
    DoubleMatrix.unsafe(data.rows, data.cols, out)

  def solveFullRank(rhs: DoubleMatrix): Either[LinearAlgebraError, QrLeastSquaresSolution] =
    if rhs.rows != rows then
      Left(LinearAlgebraError.DimensionMismatch(DimensionRole.RightHandSideRows, rows, rhs.rows))
    else if rank < cols then
      Left(LinearAlgebraError.RankDeficient(cols, rank))
    else
      val qtb = rhs.copyData
      applyQtInPlace(qtb, yCols = rhs.cols)

      val pivotedCoefficients = new Array[Double](cols * rhs.cols)
      var rhsCol = 0
      while rhsCol < rhs.cols do
        var row = cols - 1
        while row >= 0 do
          var value = qtb(row * rhs.cols + rhsCol)
          var col = row + 1
          while col < cols do
            value -= upper(row, col) * pivotedCoefficients(col * rhs.cols + rhsCol)
            col += 1
          pivotedCoefficients(row * rhs.cols + rhsCol) = value / upper(row, row)
          row -= 1
        rhsCol += 1

      val coefficients = new Array[Double](cols * rhs.cols)
      var pivotedCol = 0
      while pivotedCol < cols do
        val originalCol = perm(pivotedCol)
        rhsCol = 0
        while rhsCol < rhs.cols do
          coefficients(originalCol * rhs.cols + rhsCol) =
            pivotedCoefficients(pivotedCol * rhs.cols + rhsCol)
          rhsCol += 1
        pivotedCol += 1

      normalizedCovarianceFullRank.map { covariance =>
        QrLeastSquaresSolution(
          coefficients = DoubleMatrix.unsafe(cols, rhs.cols, coefficients),
          normalizedCovariance = covariance,
          rank = rank
        )
      }

  def normalizedCovarianceFullRank: Either[LinearAlgebraError, DoubleMatrix] =
    if rank < cols then Left(LinearAlgebraError.RankDeficient(cols, rank))
    else
      val invR = new Array[Double](cols * cols)
      var rhsCol = 0
      while rhsCol < cols do
        var row = cols - 1
        while row >= 0 do
          var value = if row == rhsCol then 1.0 else 0.0
          var col = row + 1
          while col < cols do
            value -= upper(row, col) * invR(col * cols + rhsCol)
            col += 1
          invR(row * cols + rhsCol) = value / upper(row, row)
          row -= 1
        rhsCol += 1

      val pivotedCovariance = new Array[Double](cols * cols)
      var row = 0
      while row < cols do
        var col = 0
        while col < cols do
          var acc = 0.0
          var k = 0
          while k < cols do
            acc += invR(row * cols + k) * invR(col * cols + k)
            k += 1
          pivotedCovariance(row * cols + col) = acc
          col += 1
        row += 1

      val covariance = new Array[Double](cols * cols)
      var pivotedRow = 0
      while pivotedRow < cols do
        var pivotedCol = 0
        while pivotedCol < cols do
          val originalRow = perm(pivotedRow)
          val originalCol = perm(pivotedCol)
          covariance(originalRow * cols + originalCol) =
            pivotedCovariance(pivotedRow * cols + pivotedCol)
          pivotedCol += 1
        pivotedRow += 1

      Right(DoubleMatrix.unsafe(cols, cols, covariance))

  private inline def upper(row: Int, col: Int): Double =
    packed(row * cols + col)

final case class QrLeastSquaresSolution(
    coefficients: DoubleMatrix,
    normalizedCovariance: DoubleMatrix,
    rank: Int
):
  require(coefficients.rows == normalizedCovariance.rows, "coefficient rows must match covariance rows")
  require(normalizedCovariance.rows == normalizedCovariance.cols, "normalized covariance must be square")

object QrDecomposition:
  def decompose(
      matrix: DoubleMatrix,
      pivoting: Boolean = true,
      tol: Double = 1e-7
  ): QrDecomposition =
    decompose(
      matrix,
      pivoting = Pivoting.fromBoolean(pivoting),
      tolerance = requireTolerance(tol)
    )

  def decompose(
      matrix: DoubleMatrix,
      pivoting: Pivoting,
      tolerance: Tolerance
  ): QrDecomposition =
    decompose(matrix.dataArray, matrix.shape, pivoting, tolerance)

  def decompose(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean,
      tol: Double
  ): QrDecomposition =
    val shape = requireShape(rows, cols)
    require(a0.length == shape.entries, "data length mismatch")
    decompose(a0, shape, Pivoting.fromBoolean(pivoting), requireTolerance(tol))

  def decompose(
      a0: Array[Double],
      shape: MatrixShape,
      pivoting: Pivoting,
      tolerance: Tolerance
  ): QrDecomposition =
    require(a0.length == shape.entries, "data length mismatch")

    val nReflectors = math.min(shape.rows, shape.cols)
    val a = a0.clone
    val reflectors = new Array[Householder](nReflectors)
    val diagR = new Array[Double](nReflectors)
    val perm = Array.tabulate(shape.cols)(identity)

    val colNorm2 = new Array[Double](shape.cols)
    var col = 0
    while col < shape.cols do
      var acc = 0.0
      var row = 0
      while row < shape.rows do
        val value = a(row * shape.cols + col)
        acc += value * value
        row += 1
      colNorm2(col) = acc
      col += 1

    var k = 0
    while k < nReflectors do
      if pivoting.enabled then
        var best = k
        var bestNorm = colNorm2(k)
        col = k + 1
        while col < shape.cols do
          if colNorm2(col) > bestNorm then
            best = col
            bestNorm = colNorm2(col)
          col += 1
        if best != k then
          swapColumns(a, shape.rows, shape.cols, k, best)
          val normTmp = colNorm2(k)
          colNorm2(k) = colNorm2(best)
          colNorm2(best) = normTmp
          val permTmp = perm(k)
          perm(k) = perm(best)
          perm(best) = permTmp

      var norm2 = 0.0
      var row = k
      while row < shape.rows do
        val value = a(row * shape.cols + k)
        norm2 += value * value
        row += 1

      val norm = math.sqrt(norm2)
      if norm == 0.0 then
        reflectors(k) = Householder(Array.emptyDoubleArray, 0.0)
        diagR(k) = 0.0
      else
        val x0 = a(k * shape.cols + k)
        val alpha = if x0 >= 0.0 then -norm else norm
        val len = shape.rows - k
        val v = new Array[Double](len)
        v(0) = x0 - alpha

        row = k + 1
        var i = 1
        while row < shape.rows do
          v(i) = a(row * shape.cols + k)
          row += 1
          i += 1

        var vTv = 0.0
        i = 0
        while i < len do
          vTv += v(i) * v(i)
          i += 1

        val beta = if vTv == 0.0 then 0.0 else 2.0 / vTv
        reflectors(k) = Householder(v, beta)

        var updateCol = k
        while updateCol < shape.cols do
          var dot = 0.0
          row = k
          i = 0
          while row < shape.rows do
            dot += v(i) * a(row * shape.cols + updateCol)
            row += 1
            i += 1
          val scale = beta * dot
          row = k
          i = 0
          while row < shape.rows do
            a(row * shape.cols + updateCol) -= scale * v(i)
            row += 1
            i += 1
          updateCol += 1

        diagR(k) = a(k * shape.cols + k)

      if pivoting.enabled then
        col = k + 1
        while col < shape.cols do
          var acc = 0.0
          row = k + 1
          while row < shape.rows do
            val value = a(row * shape.cols + col)
            acc += value * value
            row += 1
          colNorm2(col) = acc
          col += 1

      k += 1

    val rank =
      if nReflectors == 0 then 0
      else
        val scale = math.abs(diagR(0))
        if scale == 0.0 then 0
        else
          var r = 0
          while r < nReflectors && math.abs(diagR(r)) > tolerance.value * scale do r += 1
          r

    unsafe(shape, reflectors, diagR, a, perm, rank)

  private def requireShape(rows: Int, cols: Int): MatrixShape =
    MatrixShape.from(rows, cols) match
      case Right(shape) => shape
      case Left(error)  => throw new IllegalArgumentException(error.message)

  private[linalg] def requireTolerance(tol: Double): Tolerance =
    Tolerance(tol) match
      case Right(tolerance) => tolerance
      case Left(error)      => throw new IllegalArgumentException(error.message)

  private def unsafe(
      shape: MatrixShape,
      reflectors: Array[Householder],
      diagR: Array[Double],
      packed: Array[Double],
      perm: Array[Int],
      rank: Int
  ): QrDecomposition =
    val nReflectors = math.min(shape.rows, shape.cols)
    require(reflectors.length == nReflectors, "reflector count mismatch")
    require(diagR.length == nReflectors, "diagR length mismatch")
    require(packed.length == shape.entries, "packed QR length mismatch")
    require(perm.length == shape.cols, "perm length mismatch")
    require(rank >= 0 && rank <= nReflectors, "rank out of bounds")
    require(isPermutation(perm), "perm must be a zero-based column permutation")
    var k = 0
    while k < reflectors.length do
      val reflector = reflectors(k)
      require(reflector != null, s"reflector $k is not initialized")
      require(reflector.beta.isFinite, s"reflector $k beta must be finite")
      require(
        reflector.v.length == 0 || reflector.v.length == shape.rows - k,
        s"reflector $k length mismatch"
      )
      k += 1
    new QrDecomposition(shape, reflectors, diagR, packed, perm, rank)

  private def isPermutation(perm: Array[Int]): Boolean =
    val seen = Array.fill(perm.length)(false)
    var i = 0
    var valid = true
    while i < perm.length && valid do
      val value = perm(i)
      if value < 0 || value >= perm.length || seen(value) then valid = false
      else seen(value) = true
      i += 1
    valid

  private def swapColumns(a: Array[Double], rows: Int, cols: Int, c1: Int, c2: Int): Unit =
    var row = 0
    while row < rows do
      val i1 = row * cols + c1
      val i2 = row * cols + c2
      val tmp = a(i1)
      a(i1) = a(i2)
      a(i2) = tmp
      row += 1

final case class ResidualizedMatrix(value: DoubleMatrix, designRank: Int)

object MatrixResidualizer:
  def residualize(
      design: DoubleMatrix,
      data: DoubleMatrix,
      tol: Double = 1e-7,
      pivoting: Boolean = true
  ): ResidualizedMatrix =
    residualize(
      design,
      data,
      tolerance = QrDecomposition.requireTolerance(tol),
      pivoting = Pivoting.fromBoolean(pivoting)
    )

  def residualize(
      design: DoubleMatrix,
      data: DoubleMatrix,
      tolerance: Tolerance,
      pivoting: Pivoting
  ): ResidualizedMatrix =
    require(design.rows == data.rows, s"data rows ${data.rows} != design rows ${design.rows}")
    if design.cols == 0 || data.cols == 0 then
      ResidualizedMatrix(DoubleMatrix.unsafe(data.rows, data.cols, data.copyData), designRank = 0)
    else
      val qr = QrDecomposition.decompose(design, pivoting, tolerance)
      ResidualizedMatrix(qr.residualize(data), designRank = qr.rank)
