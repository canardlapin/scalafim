package scalafim.spatial

import neurosurf4s.routing.*

import gale.linalg.{DMat, LinAlgError}
import gale.sparse.{COO, CSR, Sparse, SparseValuePolicy}

private[spatial] type DoubleMatrix = DMat
private[spatial] type LinearMapError = LinAlgError
private[spatial] type CsrMatrix = CSR
private[spatial] type SparseTriplets = COO

private[spatial] object DoubleMatrix:
  def zeros(rows: Int, cols: Int): DMat =
    DMat.zeros(rows, cols)

  def fromRows(rows: Seq[Seq[Double]]): DMat =
    val nCols = rows.headOption.fold(0)(_.length)
    require(rows.forall(_.length == nCols), "ragged rows")
    DMat.dense(rows.length, nCols, rows.flatten)

  def unsafe(rows: Int, cols: Int, values: Array[Double]): DMat =
    DMat.dense(rows, cols, values.toIndexedSeq)

private[spatial] object SparseTriplets:
  def apply(
    rows: Int,
    cols: Int,
    rowIndices: Array[Int],
    colIndices: Array[Int],
    values: Array[Double]
  ): Either[LinAlgError, COO] =
    if rowIndices.length != colIndices.length || rowIndices.length != values.length then
      Left(LinAlgError.InvalidArgument("sparse row, column, and value lengths must match"))
    else Sparse.cooChecked(rows, cols, SparseValuePolicy.RequireFinite).flatMap: builder =>
      var i = 0
      var error = Option.empty[LinAlgError]
      while i < values.length && error.isEmpty do
        builder.tryAdd(rowIndices(i), colIndices(i), values(i)) match
          case Left(cause) => error = Some(cause)
          case Right(()) => ()
        i += 1
      error match
        case Some(cause) => Left(cause)
        case None => builder.tryToCOO()

extension (matrix: DMat)
  private[spatial] def copyData: Array[Double] =
    Array.tabulate(matrix.rows * matrix.cols)(index => matrix(index / matrix.cols, index % matrix.cols))

  private[spatial] def toRows: Vector[Vector[Double]] =
    Vector.tabulate(matrix.rows)(row => Vector.tabulate(matrix.cols)(col => matrix(row, col)))
