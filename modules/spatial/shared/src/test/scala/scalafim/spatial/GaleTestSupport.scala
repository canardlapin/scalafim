package scalafim.spatial

import gale.linalg.{DMat, LinAlgError}
import gale.sparse.{COO, CSR}

private[spatial] type DoubleMatrix = DMat
private[spatial] type LinearMapError = LinAlgError
private[spatial] type CsrMatrix = CSR
private[spatial] type SparseTriplets = COO

private[spatial] object DoubleMatrix:
  def zeros(rows: Int, cols: Int): DMat =
    DMat.zeros(rows, cols)

  def fromRows(rows: Seq[Seq[Double]]): DMat =
    GaleSpatialSupport.matrixFromRows(rows)

  def unsafe(rows: Int, cols: Int, values: Array[Double]): DMat =
    GaleSpatialSupport.unsafeOwnedMatrix(rows, cols, values)

private[spatial] object SparseTriplets:
  def apply(
    rows: Int,
    cols: Int,
    rowIndices: Array[Int],
    colIndices: Array[Int],
    values: Array[Double]
  ): Either[LinAlgError, COO] =
    GaleSpatialSupport.sparseTriplets(rows, cols, rowIndices, colIndices, values)
