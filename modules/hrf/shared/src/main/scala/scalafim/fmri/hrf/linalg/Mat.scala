package scalafim.fmri.hrf.linalg

import scala.annotation.targetName

final case class Mat private (rows: Int, cols: Int, data: Array[Double]):
  require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
  require(rows * cols == data.length, s"data length ${data.length} != rows*cols ${rows * cols}")

  inline private def idx(r: Int, c: Int): Int = r * cols + c

  def apply(r: Int, c: Int): Double = data(idx(r, c))

  def updated(r: Int, c: Int, v: Double): Mat =
    val out = data.clone
    out(idx(r, c)) = v
    Mat.unsafe(rows, cols, out)

  def row(r: Int): Vec =
    val start = r * cols
    Vec.unsafe(data.slice(start, start + cols))

  def col(c: Int): Vec =
    val out = new Array[Double](rows)
    var r = 0
    while r < rows do
      out(r) = data(idx(r, c))
      r += 1
    Vec.unsafe(out)

  @targetName("cbind")
  def ++(other: Mat): Mat =
    require(rows == other.rows, s"Row mismatch: $rows vs ${other.rows}")
    val out = new Array[Double](rows * (cols + other.cols))
    var r = 0
    while r < rows do
      val offsetOut = r * (cols + other.cols)
      System.arraycopy(data, r * cols, out, offsetOut, cols)
      System.arraycopy(other.data, r * other.cols, out, offsetOut + cols, other.cols)
      r += 1
    Mat.unsafe(rows, cols + other.cols, out)

  def map(f: Double => Double): Mat =
    val out = new Array[Double](data.length)
    var i = 0
    while i < data.length do
      out(i) = f(data(i))
      i += 1
    Mat.unsafe(rows, cols, out)

object Mat:
  def zeros(rows: Int, cols: Int): Mat = unsafe(rows, cols, Array.fill(rows * cols)(0.0))

  def eye(n: Int): Mat =
    val out = Array.fill(n * n)(0.0)
    var i = 0
    while i < n do
      out(i * n + i) = 1.0
      i += 1
    unsafe(n, n, out)

  def fromRows(rowsData: Seq[Seq[Double]]): Mat =
    val rows = rowsData.length
    val cols = if rows == 0 then 0 else rowsData.head.length
    require(rowsData.forall(_.length == cols), "ragged rows")
    val out = new Array[Double](rows * cols)
    var r = 0
    while r < rows do
      var c = 0
      while c < cols do
        out(r * cols + c) = rowsData(r)(c)
        c += 1
      r += 1
    unsafe(rows, cols, out)

  def unsafe(rows: Int, cols: Int, data: Array[Double]): Mat = new Mat(rows, cols, data)
