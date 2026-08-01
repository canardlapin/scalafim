package scalafim.response

import ravel.{Array1, NDArray, Shape}

final class ResponseBlock private (
    private[response] val ownedRowMajor: Array1[Double],
    val rows: Int,
    val columns: Int,
    val selection: ResolvedResponseSelection
):
  inline def apply(row: Int, column: Int): Double =
    ownedRowMajor(row * columns + column)

  def rowMajorCopy: Array[Double] =
    Array.tabulate(ownedRowMajor.size)(index => ownedRowMajor(index))

  def sameRawBits(other: ResponseBlock): Boolean =
    if rows != other.rows || columns != other.columns || selection != other.selection then
      false
    else
      var index = 0
      while index < ownedRowMajor.size do
        val left = java.lang.Double.doubleToRawLongBits(ownedRowMajor(index))
        val right = java.lang.Double.doubleToRawLongBits(other.ownedRowMajor(index))
        if left != right then return false
        index += 1
      true

object ResponseBlock:
  def copyFromRowMajor(
      values: Array[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock] =
    checkedValueCount(selection).flatMap: expected =>
      if values.length != expected then
        Left(ResponseShapeError.ValueCountMismatch(expected, values.length))
      else
        Right(
          new ResponseBlock(
            NDArray.fromSeq(Shape(values.length), values),
            selection.rows,
            selection.columns,
            selection
          )
        )

  def copyFromRowMajor(
      values: Array1[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock] =
    fromOwnedRowMajor(values, selection)

  private[response] def fromOwnedRowMajor(
      values: Array1[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock] =
    checkedValueCount(selection).flatMap: expected =>
      if values.size != expected then
        Left(ResponseShapeError.ValueCountMismatch(expected, values.size))
      else
        Right(new ResponseBlock(values, selection.rows, selection.columns, selection))

  private[scalafim] def unsafeFromOwnedRowMajor(
      values: Array1[Double],
      selection: ResolvedResponseSelection
  ): ResponseBlock =
    val expected =
      checkedValueCount(selection)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    require(
      values.size == expected,
      s"owned response buffer length ${values.size} must equal $expected"
    )
    new ResponseBlock(values, selection.rows, selection.columns, selection)

  private[scalafim] def unsafeFromOwnedRowMajor(
      values: Array[Double],
      selection: ResolvedResponseSelection
  ): ResponseBlock =
    unsafeFromOwnedRowMajor(
      NDArray.fromSeq(Shape(values.length), values),
      selection
    )

  private def checkedValueCount(
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, Int] =
    val expected = selection.rows.toLong * selection.columns.toLong
    if expected > Int.MaxValue.toLong then
      Left(ResponseShapeError.ValueCountOverflow(selection.rows, selection.columns))
    else
      Right(expected.toInt)
