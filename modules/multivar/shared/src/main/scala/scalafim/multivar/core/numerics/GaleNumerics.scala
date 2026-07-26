package scalafim.multivar
package core

import gale.backend.Backend.given
import gale.linalg.DMat
import gale.linalg.DVec
import gale.linalg.Matrix
import gale.linalg.Vec

/** Allocation-aware carrier adaptations used while multivar keeps its
  * domain-specific matrix-view and metric APIs over Gale values.
  */
private[multivar] object GaleNumerics:
  def matrixFromRowMajor(rows: Int, cols: Int, values: Array[Double]): DMat =
    require(values.length == rows * cols, "row-major data length must match matrix shape")
    val out = Matrix.newBuilder(rows, cols)
    var index = 0
    while index < values.length do
      out.updateRowMajor(index, values(index))
      index += 1
    out.result()

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

  def vectorFromArray(values: Array[Double]): DVec =
    val out = Vec.newBuilder(values.length)
    var index = 0
    while index < values.length do
      out(index) = values(index)
      index += 1
    out.result()

  def multiply(left: DMat, right: DMat): DMat =
    left * right

  def transposeMultiply(left: DMat, right: DMat): DMat =
    left.t * right

  def crossProduct(matrix: DMat): DMat =
    matrix.t * matrix

  def selectRows(matrix: DMat, indices: IndexedSeq[Int]): DMat =
    val out = Matrix.newBuilder(indices.length, matrix.cols)
    var row = 0
    while row < indices.length do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(indices(row), col)
        col += 1
      row += 1
    out.result()

  def selectColumns(matrix: DMat, indices: IndexedSeq[Int]): DMat =
    val out = Matrix.newBuilder(matrix.rows, indices.length)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < indices.length do
        out(row, col) = matrix(row, indices(col))
        col += 1
      row += 1
    out.result()

extension (matrix: DMat)
  private[multivar] def copyData: Array[Double] =
    matrix.valuesRowMajor.toArray

  private[multivar] def toRows: Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows) { row =>
      Vector.tabulate(matrix.cols) { col => matrix(row, col) }
    }

  private[multivar] def transpose: DMat =
    matrix.t

  private[multivar] def selectRows(indices: IndexedSeq[Int]): DMat =
    GaleNumerics.selectRows(matrix, indices)

  private[multivar] def selectColumns(indices: IndexedSeq[Int]): DMat =
    GaleNumerics.selectColumns(matrix, indices)

  private[multivar] def addToDiagonal(amount: Double): DMat =
    require(matrix.rows == matrix.cols, "diagonal update requires a square matrix")
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(row, col) + (if row == col then amount else 0.0)
        col += 1
      row += 1
    out.result()

extension (vector: DVec)
  private[multivar] def copyData: Array[Double] =
    vector.toSeq.toArray

  private[multivar] def toVector: Vector[Double] =
    vector.toSeq.toVector
