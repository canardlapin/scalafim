package scalafim.fmri.fit

import gale.linalg.{DMat, Matrix}
import scalafim.dataset.FmriSeries
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.FmriModel
import scalafim.image.DMat as ImageDMat

object MatrixAdapters:
  def fromHrfMatrix(matrix: Mat): DMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var i = 0
    while i < matrix.data.length do
      out.updateRowMajor(i, matrix.data(i))
      i += 1
    out.result()

  def fromHrfMatrixRows(matrix: Mat, rows: IndexedSeq[Int]): DMat =
    require(rows.nonEmpty, "row selection must be non-empty")
    val out = Matrix.newBuilder(rows.length, matrix.cols)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      require(sourceRow >= 0 && sourceRow < matrix.rows, s"row index $sourceRow out of bounds")
      var col = 0
      while col < matrix.cols do
        out(outRow, col) = matrix.data(sourceRow * matrix.cols + col)
        col += 1
      outRow += 1
    out.result()

  def fromHrfMatrixRowsCols(matrix: Mat, rows: IndexedSeq[Int], cols: IndexedSeq[Int]): DMat =
    require(rows.nonEmpty, "row selection must be non-empty")
    cols.foreach { col =>
      require(col >= 0 && col < matrix.cols, s"column index $col out of bounds")
    }
    val out = Matrix.newBuilder(rows.length, cols.length)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      require(sourceRow >= 0 && sourceRow < matrix.rows, s"row index $sourceRow out of bounds")
      var outCol = 0
      while outCol < cols.length do
        out(outRow, outCol) = matrix.data(sourceRow * matrix.cols + cols(outCol))
        outCol += 1
      outRow += 1
    out.result()

  def bindColumns(left: DMat, right: DMat): DMat =
    require(left.rows == right.rows, s"row mismatch: ${left.rows} vs ${right.rows}")
    if left.cols == 0 then Matrix.tabulate(right.rows, right.cols)(right.apply)
    else if right.cols == 0 then Matrix.tabulate(left.rows, left.cols)(left.apply)
    else
      val outCols = left.cols + right.cols
      val out = Matrix.newBuilder(left.rows, outCols)
      var row = 0
      while row < left.rows do
        var col = 0
        while col < left.cols do
          out(row, col) = left(row, col)
          col += 1
        col = 0
        while col < right.cols do
          out(row, left.cols + col) = right(row, col)
          col += 1
        row += 1
      out.result()

  def fromDMat(matrix: ImageDMat): DMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(row, col)
        col += 1
      row += 1
    out.result()

  def designMatrix(model: FmriModel, timepoints: IndexedSeq[Int]): Either[FitError, DesignMatrix] =
    DesignMatrix.fromMatrix(fromHrfMatrixRows(model.designMatrix, timepoints))

  def responseBlock(series: FmriSeries): Either[FitError, ResponseBlock] =
    ResponseBlock.fromMatrix(fromDMat(series.data))
