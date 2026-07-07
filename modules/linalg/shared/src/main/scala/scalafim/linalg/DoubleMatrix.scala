package scalafim.linalg

import scala.annotation.targetName

final class DoubleMatrix private (
    val rows: Int,
    val cols: Int,
    private[scalafim] val dataArray: Array[Double]
):
  require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
  require(rows * cols == dataArray.length, s"data length ${dataArray.length} != rows*cols ${rows * cols}")

  inline private def index(row: Int, col: Int): Int =
    row * cols + col

  def shape: MatrixShape =
    MatrixShape.unsafe(rows, cols)

  def apply(row: Int, col: Int): Double =
    dataArray(index(row, col))

  @targetName("applyAt")
  def apply(row: RowIndex, col: ColIndex): Double =
    dataArray(index(row.value, col.value))

  def copyData: Array[Double] =
    dataArray.clone

  def row(rowIndex: Int): DoubleVector =
    val out = new Array[Double](cols)
    System.arraycopy(dataArray, rowIndex * cols, out, 0, cols)
    DoubleVector.unsafe(out)

  @targetName("rowAt")
  def row(rowIndex: RowIndex): DoubleVector =
    row(rowIndex.value)

  def col(colIndex: Int): DoubleVector =
    val out = new Array[Double](rows)
    var rowIndex = 0
    while rowIndex < rows do
      out(rowIndex) = dataArray(index(rowIndex, colIndex))
      rowIndex += 1
    DoubleVector.unsafe(out)

  @targetName("colAt")
  def col(colIndex: ColIndex): DoubleVector =
    col(colIndex.value)

  def updated(row: Int, col: Int, value: Double): DoubleMatrix =
    val out = dataArray.clone
    out(index(row, col)) = value
    DoubleMatrix.unsafe(rows, cols, out)

  @targetName("updatedAt")
  def updated(row: RowIndex, col: ColIndex, value: Double): DoubleMatrix =
    updated(row.value, col.value, value)

  def transpose: DoubleMatrix =
    val out = new Array[Double](rows * cols)
    var row = 0
    while row < rows do
      var col = 0
      while col < cols do
        out(col * rows + row) = dataArray(index(row, col))
        col += 1
      row += 1
    DoubleMatrix.unsafe(cols, rows, out)

  def addToDiagonal(value: Double): DoubleMatrix =
    require(rows == cols, s"matrix must be square, got ${rows}x${cols}")
    require(value.isFinite, "diagonal increment must be finite")
    val out = dataArray.clone
    var i = 0
    while i < rows do
      out(i * cols + i) += value
      i += 1
    DoubleMatrix.unsafe(rows, cols, out)

  def selectRows(rowIndices: IndexedSeq[Int]): DoubleMatrix =
    require(rowIndices.nonEmpty, "row selection must be non-empty")
    val out = new Array[Double](rowIndices.length * cols)
    var outRow = 0
    while outRow < rowIndices.length do
      val sourceRow = rowIndices(outRow)
      require(sourceRow >= 0 && sourceRow < rows, s"row index $sourceRow out of bounds")
      System.arraycopy(dataArray, sourceRow * cols, out, outRow * cols, cols)
      outRow += 1
    DoubleMatrix.unsafe(rowIndices.length, cols, out)

  def toRows: Vector[Vector[Double]] =
    val out = Vector.newBuilder[Vector[Double]]
    out.sizeHint(rows)
    var rowIndex = 0
    while rowIndex < rows do
      out += row(rowIndex).toVector
      rowIndex += 1
    out.result()

object DoubleMatrix:
  def zeros(shape: MatrixShape): DoubleMatrix =
    unsafe(shape, new Array[Double](shape.entries))

  def zeros(rows: Int, cols: Int): DoubleMatrix =
    require(rows >= 0 && cols >= 0, "rows/cols must be non-negative")
    unsafe(rows, cols, new Array[Double](rows * cols))

  def eye(size: Int): DoubleMatrix =
    require(size >= 0, "size must be non-negative")
    val out = new Array[Double](size * size)
    var i = 0
    while i < size do
      out(i * size + i) = 1.0
      i += 1
    unsafe(size, size, out)

  def fromRows(rowsData: Seq[Seq[Double]]): DoubleMatrix =
    val indexedRows = rowsData.map(_.toIndexedSeq).toIndexedSeq
    val rowCount = indexedRows.length
    val colCount = if rowCount == 0 then 0 else indexedRows.head.length
    require(indexedRows.forall(_.length == colCount), "ragged rows")
    val out = new Array[Double](rowCount * colCount)
    var rowIndex = 0
    while rowIndex < rowCount do
      var colIndex = 0
      val row = indexedRows(rowIndex)
      while colIndex < colCount do
        out(rowIndex * colCount + colIndex) = row(colIndex)
        colIndex += 1
      rowIndex += 1
    unsafe(rowCount, colCount, out)

  def fromRowMajor(shape: MatrixShape, data: Array[Double]): Either[LinearAlgebraError, DoubleMatrix] =
    if data.length != shape.entries then Left(LinearAlgebraError.MatrixStorageLengthMismatch(shape, data.length))
    else Right(unsafe(shape, data.clone))

  private[scalafim] def unsafe(shape: MatrixShape, data: Array[Double]): DoubleMatrix =
    new DoubleMatrix(shape.rows, shape.cols, data)

  private[scalafim] def unsafe(rows: Int, cols: Int, data: Array[Double]): DoubleMatrix =
    new DoubleMatrix(rows, cols, data)

  def multiply(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    require(left.cols == right.rows, s"matrix dimension mismatch: ${left.rows}x${left.cols} times ${right.rows}x${right.cols}")
    val out = new Array[Double](left.rows * right.cols)
    var row = 0
    while row < left.rows do
      var k = 0
      while k < left.cols do
        val x = left.dataArray(row * left.cols + k)
        var col = 0
        while col < right.cols do
          out(row * right.cols + col) += x * right.dataArray(k * right.cols + col)
          col += 1
        k += 1
      row += 1
    unsafe(left.rows, right.cols, out)

  def transposeMultiply(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    require(left.rows == right.rows, s"row mismatch: ${left.rows} vs ${right.rows}")
    val out = new Array[Double](left.cols * right.cols)
    var row = 0
    while row < left.rows do
      val leftOffset = row * left.cols
      val rightOffset = row * right.cols
      var leftCol = 0
      while leftCol < left.cols do
        val x = left.dataArray(leftOffset + leftCol)
        val outOffset = leftCol * right.cols
        var rightCol = 0
        while rightCol < right.cols do
          out(outOffset + rightCol) += x * right.dataArray(rightOffset + rightCol)
          rightCol += 1
        leftCol += 1
      row += 1
    unsafe(left.cols, right.cols, out)

  def crossProduct(matrix: DoubleMatrix): DoubleMatrix =
    transposeMultiply(matrix, matrix)
