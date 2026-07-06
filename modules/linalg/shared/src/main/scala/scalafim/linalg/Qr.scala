package scalafim.linalg

final case class Householder(v: Array[Double], beta: Double)

final case class QrDecomposition(
    rows: Int,
    cols: Int,
    reflectors: Array[Householder],
    diagR: Array[Double],
    perm: Array[Int],
    rank: Int
):
  require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
  require(perm.length == cols, "perm length mismatch")
  require(diagR.length == reflectors.length, "diagR/reflectors length mismatch")
  require(rank >= 0 && rank <= math.min(rows, cols), "rank out of bounds")

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

object QrDecomposition:
  def decompose(
      matrix: DoubleMatrix,
      pivoting: Boolean = true,
      tol: Double = 1e-7
  ): QrDecomposition =
    decompose(matrix.dataArray, matrix.rows, matrix.cols, pivoting = pivoting, tol = tol)

  def decompose(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean,
      tol: Double
  ): QrDecomposition =
    require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
    require(a0.length == rows * cols, "data length mismatch")
    require(tol >= 0.0 && tol.isFinite, "tol must be non-negative and finite")

    val nReflectors = math.min(rows, cols)
    val a = a0.clone
    val reflectors = new Array[Householder](nReflectors)
    val diagR = new Array[Double](nReflectors)
    val perm = Array.tabulate(cols)(identity)

    val colNorm2 = new Array[Double](cols)
    var col = 0
    while col < cols do
      var acc = 0.0
      var row = 0
      while row < rows do
        val value = a(row * cols + col)
        acc += value * value
        row += 1
      colNorm2(col) = acc
      col += 1

    var k = 0
    while k < nReflectors do
      if pivoting then
        var best = k
        var bestNorm = colNorm2(k)
        col = k + 1
        while col < cols do
          if colNorm2(col) > bestNorm then
            best = col
            bestNorm = colNorm2(col)
          col += 1
        if best != k then
          swapColumns(a, rows, cols, k, best)
          val normTmp = colNorm2(k)
          colNorm2(k) = colNorm2(best)
          colNorm2(best) = normTmp
          val permTmp = perm(k)
          perm(k) = perm(best)
          perm(best) = permTmp

      var norm2 = 0.0
      var row = k
      while row < rows do
        val value = a(row * cols + k)
        norm2 += value * value
        row += 1

      val norm = math.sqrt(norm2)
      if norm == 0.0 then
        reflectors(k) = Householder(Array.emptyDoubleArray, 0.0)
        diagR(k) = 0.0
      else
        val x0 = a(k * cols + k)
        val alpha = if x0 >= 0.0 then -norm else norm
        val len = rows - k
        val v = new Array[Double](len)
        v(0) = x0 - alpha

        row = k + 1
        var i = 1
        while row < rows do
          v(i) = a(row * cols + k)
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
        while updateCol < cols do
          var dot = 0.0
          row = k
          i = 0
          while row < rows do
            dot += v(i) * a(row * cols + updateCol)
            row += 1
            i += 1
          val scale = beta * dot
          row = k
          i = 0
          while row < rows do
            a(row * cols + updateCol) -= scale * v(i)
            row += 1
            i += 1
          updateCol += 1

        diagR(k) = a(k * cols + k)

      if pivoting then
        col = k + 1
        while col < cols do
          var acc = 0.0
          row = k + 1
          while row < rows do
            val value = a(row * cols + col)
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
          while r < nReflectors && math.abs(diagR(r)) > tol * scale do r += 1
          r

    QrDecomposition(rows, cols, reflectors, diagR, perm, rank)

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
    require(design.rows == data.rows, s"data rows ${data.rows} != design rows ${design.rows}")
    if design.cols == 0 || data.cols == 0 then
      ResidualizedMatrix(DoubleMatrix.unsafe(data.rows, data.cols, data.copyData), designRank = 0)
    else
      val qr = QrDecomposition.decompose(design, pivoting = pivoting, tol = tol)
      ResidualizedMatrix(qr.residualize(data), designRank = qr.rank)
