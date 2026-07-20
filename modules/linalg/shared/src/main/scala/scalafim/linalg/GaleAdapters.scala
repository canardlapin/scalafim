package scalafim.linalg

import gale.backend.Backend.given
import gale.linalg.DMat

/** Explicit compatibility boundary between Scalafim's legacy matrix carrier and
  * Gale's canonical dense linear-algebra type.
  */
object GaleMatrixBridge:
  def toGale(matrix: DoubleMatrix): DMat =
    val builder = DMat.newBuilder(matrix.rows, matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        builder(row, col) = matrix(row, col)
        col += 1
      row += 1
    builder.result()

  def fromGale(matrix: DMat): DoubleMatrix =
    val out = new Array[Double](matrix.rows * matrix.cols)
    var row = 0
    while row < matrix.rows do
      var col = 0
      while col < matrix.cols do
        out(row * matrix.cols + col) = matrix(row, col)
        col += 1
      row += 1
    DoubleMatrix.unsafe(matrix.rows, matrix.cols, out)

  def leftMultiply(left: DMat, right: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    if left.cols != right.rows then Left(LinearMapError.DimensionMismatch(left.cols, right.rows))
    else Right(fromGale(left * toGale(right)))

  def rightMultiply(left: DoubleMatrix, right: DMat): Either[LinearMapError, DoubleMatrix] =
    if left.cols != right.rows then Left(LinearMapError.DimensionMismatch(right.rows, left.cols))
    else Right(fromGale(toGale(left) * right))

final class GaleLinearMap private (val matrix: DMat) extends LinearMap:
  override def rows: Int =
    matrix.rows

  override def cols: Int =
    matrix.cols

  override def forward(input: DoubleMatrix): Either[LinearMapError, DoubleMatrix] =
    GaleMatrixBridge.leftMultiply(matrix, input)

  override def adjoint: LinearMap =
    new GaleLinearMap(matrix.t)

object GaleLinearMap:
  def apply(matrix: DMat): GaleLinearMap =
    new GaleLinearMap(matrix)
