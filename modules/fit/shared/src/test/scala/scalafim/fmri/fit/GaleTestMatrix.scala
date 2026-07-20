package scalafim.fmri.fit

import gale.linalg.{DMat, DVec, Matrix}

/** Readable construction helpers for fixture matrices. Production hot paths use
  * Gale's ownership-safe builders directly; tests generally start from rows.
  */
object GaleTestMatrix:
  def fromRows(rows: Seq[Seq[Double]]): DMat =
    require(rows.nonEmpty, "test matrix rows must be non-empty")
    val cols = rows.head.length
    require(rows.forall(_.length == cols), "test matrix rows must have equal length")
    val out = Matrix.newBuilder(rows.length, cols)
    var row = 0
    while row < rows.length do
      var col = 0
      while col < cols do
        out(row, col) = rows(row)(col)
        col += 1
      row += 1
    out.result()

  def fromArray(rows: Int, cols: Int, values: Array[Double]): DMat =
    require(values.length == rows * cols, s"expected ${rows * cols} values, got ${values.length}")
    val out = Matrix.newBuilder(rows, cols)
    var index = 0
    while index < values.length do
      out.updateRowMajor(index, values(index))
      index += 1
    out.result()

object GaleTestSyntax:
  extension (matrix: DMat)
    def copyData: Array[Double] = matrix.valuesRowMajor.toArray

    def toRows: Vector[Vector[Double]] =
      Vector.tabulate(matrix.rows) { row =>
        Vector.tabulate(matrix.cols) { col => matrix(row, col) }
      }

  extension (vector: DVec)
    def copyData: Array[Double] = vector.toSeq.toArray
    def toVector: Vector[Double] = vector.toSeq.toVector
