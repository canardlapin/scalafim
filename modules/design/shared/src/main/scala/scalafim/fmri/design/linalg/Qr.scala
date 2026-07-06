package scalafim.fmri.design.linalg

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

  /** Apply Qᵀ to Y in-place, where Y is row-major (rows x yCols). */
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
            val r = k + i
            dot += v(i) * y(r * yCols + col)
            i += 1
          val s = beta * dot
          i = 0
          while i < len do
            val r = k + i
            y(r * yCols + col) -= s * v(i)
            i += 1
          col += 1
      k += 1

  /** Apply Q to Y in-place, where Y is row-major (rows x yCols). */
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
            val r = k + i
            dot += v(i) * y(r * yCols + col)
            i += 1
          val s = beta * dot
          i = 0
          while i < len do
            val r = k + i
            y(r * yCols + col) -= s * v(i)
            i += 1
          col += 1
      k -= 1

object QrDecomposition:

  def decompose(
      a0: Array[Double],
      rows: Int,
      cols: Int,
      pivoting: Boolean = true,
      tol: Double = 1e-7
  ): QrDecomposition =
    require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
    require(a0.length == rows * cols, "data length mismatch")
    require(tol >= 0.0, "tol must be non-negative")

    val m = math.min(rows, cols)
    val a = a0.clone
    val reflectors = new Array[Householder](m)
    val diagR = new Array[Double](m)
    val perm = Array.tabulate(cols)(identity)

    val colNorm2 = new Array[Double](cols)
    var c = 0
    while c < cols do
      var acc = 0.0
      var r = 0
      while r < rows do
        val v = a(r * cols + c)
        acc += v * v
        r += 1
      colNorm2(c) = acc
      c += 1

    var k = 0
    while k < m do
      if pivoting then
        var best = k
        var bestNorm = colNorm2(k)
        c = k + 1
        while c < cols do
          val n2 = colNorm2(c)
          if n2 > bestNorm then
            best = c
            bestNorm = n2
          c += 1
        if best != k then
          swapColumns(a, rows, cols, k, best)
          val tmpN = colNorm2(k)
          colNorm2(k) = colNorm2(best)
          colNorm2(best) = tmpN
          val tmpP = perm(k)
          perm(k) = perm(best)
          perm(best) = tmpP

      var norm2 = 0.0
      var r = k
      while r < rows do
        val v = a(r * cols + k)
        norm2 += v * v
        r += 1

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
        r = k + 1
        var i = 1
        while r < rows do
          v(i) = a(r * cols + k)
          r += 1
          i += 1

        var vTv = 0.0
        i = 0
        while i < len do
          vTv += v(i) * v(i)
          i += 1
        val beta = if vTv == 0.0 then 0.0 else 2.0 / vTv
        reflectors(k) = Householder(v, beta)

        var cc = k
        while cc < cols do
          var dot = 0.0
          i = 0
          r = k
          while r < rows do
            dot += v(i) * a(r * cols + cc)
            i += 1
            r += 1
          val s = beta * dot
          i = 0
          r = k
          while r < rows do
            a(r * cols + cc) -= s * v(i)
            i += 1
            r += 1
          cc += 1

        diagR(k) = a(k * cols + k)

      // Update remaining-column norms for pivoting (norms of the trailing subvector).
      if pivoting then
        c = k + 1
        while c < cols do
          var acc = 0.0
          r = k + 1
          while r < rows do
            val v = a(r * cols + c)
            acc += v * v
            r += 1
          colNorm2(c) = acc
          c += 1

      k += 1

    val rank =
      if m == 0 then 0
      else
        val d0 = math.abs(diagR(0))
        if d0 == 0.0 then 0
        else
          var r = 0
          while r < m && math.abs(diagR(r)) > tol * d0 do r += 1
          r

    QrDecomposition(rows, cols, reflectors, diagR, perm, rank)

  private def swapColumns(a: Array[Double], rows: Int, cols: Int, c1: Int, c2: Int): Unit =
    var r = 0
    while r < rows do
      val i1 = r * cols + c1
      val i2 = r * cols + c2
      val tmp = a(i1)
      a(i1) = a(i2)
      a(i2) = tmp
      r += 1
