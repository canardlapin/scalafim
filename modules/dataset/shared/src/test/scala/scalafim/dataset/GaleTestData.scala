package scalafim.dataset

import gale.linalg.DMat
import gale.linalg.Matrix
import gale.sparse.CSR
import gale.sparse.Sparse

private[dataset] object GaleTestData:
  def matrixFromRows(rows: Seq[Seq[Double]]): DMat =
    val rowCount = rows.length
    val colCount = rows.headOption.fold(0)(_.length)
    require(rows.forall(_.length == colCount), "matrix rows must have equal lengths")
    val out = Matrix.newBuilder(rowCount, colCount)
    var row = 0
    rows.foreach { values =>
      var col = 0
      values.foreach { value =>
        out(row, col) = value
        col += 1
      }
      row += 1
    }
    out.result()

  def toRows(matrix: DMat): Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows) { row =>
      Vector.tabulate(matrix.cols) { col => matrix(row, col) }
    }

  def csrFromTriplets(
      rows: Int,
      cols: Int,
      rowIndices: Array[Int],
      colIndices: Array[Int],
      values: Array[Double]
  ): CSR =
    require(rowIndices.length == colIndices.length && rowIndices.length == values.length)
    val out = Sparse.coo(rows, cols)
    var index = 0
    while index < values.length do
      out.add(rowIndices(index), colIndices(index), values(index))
      index += 1
    out.toCSR()
