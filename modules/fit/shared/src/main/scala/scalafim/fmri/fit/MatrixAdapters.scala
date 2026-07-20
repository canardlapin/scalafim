package scalafim.fmri.fit

import gale.linalg.{DMat as GaleDMat, Matrix}
import scalafim.dataset.FmriSeries
import scalafim.fmri.hrf.linalg.Mat
import scalafim.fmri.model.FmriModel
import scalafim.image.DMat
import scalafim.linalg.DoubleMatrix

object MatrixAdapters:
  /** Temporary carrier bridge for the ordered Gale migration.
    *
    * Deletion gate: remove `toGaleMatrix` and `fromGaleMatrix` once `DesignMatrix`
    * and `ResponseBlock` carry Gale `DMat` directly. Each direction allocates
    * exactly one destination carrier and performs one primitive-loop traversal;
    * no `Seq` boxing or intermediate row collection is involved.
    */
  private[fit] def toGaleMatrix(matrix: DoubleMatrix): GaleDMat =
    val out = Matrix.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row, col) = matrix(row, col)
        col += 1
      row += 1
    out.result()

  private[fit] def fromGaleMatrix(matrix: GaleDMat): DoubleMatrix =
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = matrix(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def fromHrfMatrix(matrix: Mat): DoubleMatrix =
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, matrix.data.clone)

  def fromHrfMatrixRows(matrix: Mat, rows: IndexedSeq[Int]): DoubleMatrix =
    require(rows.nonEmpty, "row selection must be non-empty")
    val out = new Array[Double](rows.length * matrix.cols)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      require(sourceRow >= 0 && sourceRow < matrix.rows, s"row index $sourceRow out of bounds")
      System.arraycopy(matrix.data, sourceRow * matrix.cols, out, outRow * matrix.cols, matrix.cols)
      outRow += 1
    DoubleMatrix.unsafe(rows.length, matrix.cols, out)

  def fromHrfMatrixRowsCols(matrix: Mat, rows: IndexedSeq[Int], cols: IndexedSeq[Int]): DoubleMatrix =
    require(rows.nonEmpty, "row selection must be non-empty")
    cols.foreach { col =>
      require(col >= 0 && col < matrix.cols, s"column index $col out of bounds")
    }
    val out = new Array[Double](rows.length * cols.length)
    var outRow = 0
    while outRow < rows.length do
      val sourceRow = rows(outRow)
      require(sourceRow >= 0 && sourceRow < matrix.rows, s"row index $sourceRow out of bounds")
      var outCol = 0
      while outCol < cols.length do
        out(outRow * cols.length + outCol) = matrix.data(sourceRow * matrix.cols + cols(outCol))
        outCol += 1
      outRow += 1
    DoubleMatrix.unsafe(rows.length, cols.length, out)

  def bindColumns(left: DoubleMatrix, right: DoubleMatrix): DoubleMatrix =
    require(left.rows == right.rows, s"row mismatch: ${left.rows} vs ${right.rows}")
    if left.cols == 0 then DoubleMatrix.unsafe(right.rows, right.cols, right.copyData)
    else if right.cols == 0 then DoubleMatrix.unsafe(left.rows, left.cols, left.copyData)
    else
      val outCols = left.cols + right.cols
      val out = new Array[Double](left.rows * outCols)
      var row = 0
      while row < left.rows do
        System.arraycopy(left.dataArray, row * left.cols, out, row * outCols, left.cols)
        System.arraycopy(right.dataArray, row * right.cols, out, row * outCols + left.cols, right.cols)
        row += 1
      DoubleMatrix.unsafe(left.rows, outCols, out)

  def fromDMat(matrix: DMat): DoubleMatrix =
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = matrix(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def designMatrix(model: FmriModel, timepoints: IndexedSeq[Int]): Either[FitError, DesignMatrix] =
    DesignMatrix.fromMatrix(fromHrfMatrixRows(model.designMatrix, timepoints))

  def responseBlock(series: FmriSeries): Either[FitError, ResponseBlock] =
    ResponseBlock.fromMatrix(fromDMat(series.data))
