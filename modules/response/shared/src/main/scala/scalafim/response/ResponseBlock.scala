package scalafim.response

import narr.NArray

final class ResponseBlock private (
    private[response] val ownedRowMajor: NArray[Double],
    val rows: Int,
    val columns: Int,
    val selection: ResolvedResponseSelection
):
  inline def apply(row: Int, column: Int): Double =
    ownedRowMajor(row * columns + column)

  def rowMajorCopy: NArray[Double] =
    val copied = NArray.ofSize[Double](ownedRowMajor.length)
    var index = 0
    while index < ownedRowMajor.length do
      copied(index) = ownedRowMajor(index)
      index += 1
    copied

  def sameRawBits(other: ResponseBlock): Boolean =
    if rows != other.rows || columns != other.columns || selection != other.selection then
      false
    else
      var index = 0
      while index < ownedRowMajor.length do
        val left = java.lang.Double.doubleToRawLongBits(ownedRowMajor(index))
        val right = java.lang.Double.doubleToRawLongBits(other.ownedRowMajor(index))
        if left != right then return false
        index += 1
      true

object ResponseBlock:
  def copyFromRowMajor(
      values: NArray[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock] =
    checkedValueCount(selection).flatMap: expected =>
      if values.length != expected then
        Left(ResponseShapeError.ValueCountMismatch(expected, values.length))
      else
        val copied = NArray.ofSize[Double](values.length)
        var index = 0
        while index < values.length do
          copied(index) = values(index)
          index += 1
        Right(new ResponseBlock(copied, selection.rows, selection.columns, selection))

  private[response] def fromOwnedRowMajor(
      values: NArray[Double],
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, ResponseBlock] =
    checkedValueCount(selection).flatMap: expected =>
      if values.length != expected then
        Left(ResponseShapeError.ValueCountMismatch(expected, values.length))
      else
        Right(new ResponseBlock(values, selection.rows, selection.columns, selection))

  private[scalafim] def unsafeFromOwnedRowMajor(
      values: NArray[Double],
      selection: ResolvedResponseSelection
  ): ResponseBlock =
    val expected =
      checkedValueCount(selection)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    require(
      values.length == expected,
      s"owned response buffer length ${values.length} must equal $expected"
    )
    new ResponseBlock(values, selection.rows, selection.columns, selection)

  private def checkedValueCount(
      selection: ResolvedResponseSelection
  ): Either[ResponseShapeError, Int] =
    val expected = selection.rows.toLong * selection.columns.toLong
    if expected > Int.MaxValue.toLong then
      Left(ResponseShapeError.ValueCountOverflow(selection.rows, selection.columns))
    else
      Right(expected.toInt)
