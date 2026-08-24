package scalafim.archive.lna

import scalafim.archive.ArchiveError
import gale.linalg.DMat

object Delta:
  final case class Encoded(
      deltas: Payload.DoubleMatrix,
      firstValues: Payload.DoubleMatrix,
      params: DeltaParams
  )

  def encode(data: DMat, params: DeltaParams = DeltaParams()): Either[ArchiveError, Encoded] =
    params.axis match
      case DeltaAxis.Time =>
        encodeTime(data, params)
      case DeltaAxis.Feature =>
        Left(ArchiveError.UnsupportedTransform("delta axis=feature"))

  def decode(encoded: Encoded): DMat =
    decode(encoded.deltas.data, encoded.firstValues.data, encoded.params)

  def decode(deltas: DMat, firstValues: DMat, params: DeltaParams = DeltaParams()): DMat =
    decodeChecked(deltas, firstValues, params).fold(err => throw IllegalArgumentException(err.message), identity)

  def decodeChecked(encoded: Encoded): Either[ArchiveError, DMat] =
    decodeChecked(encoded.deltas.data, encoded.firstValues.data, encoded.params)

  def decodeChecked(deltas: DMat, firstValues: DMat, params: DeltaParams = DeltaParams()): Either[ArchiveError, DMat] =
    params.axis match
      case DeltaAxis.Time =>
        decodeTimeChecked(deltas, firstValues)
      case DeltaAxis.Feature =>
        Left(ArchiveError.UnsupportedTransform("delta axis=feature"))

  private def encodeTime(data: DMat, params: DeltaParams): Either[ArchiveError, Encoded] =
    if data.rows < 2 then
      Left(ArchiveError.ShapeMismatch("delta transform requires at least two timepoints"))
    else if params.codingMethod != DeltaCodingMethod.None then
      Left(ArchiveError.UnsupportedTransform(s"delta coding_method=${params.codingMethod.value}"))
    else
      val first = DMat.tabulate(1, data.cols)((_, column) => data(0, column))
      val deltas = DMat.tabulate(data.rows - 1, data.cols): (row, column) =>
        data(row + 1, column) - data(row, column)
      Right(
        Encoded(
          deltas = Payload.DoubleMatrix(deltas),
          firstValues = Payload.DoubleMatrix(first),
          params = params
        )
      )

  private def decodeTime(deltas: DMat, firstValues: DMat): DMat =
    decodeTimeChecked(deltas, firstValues).fold(err => throw IllegalArgumentException(err.message), identity)

  private def decodeTimeChecked(deltas: DMat, firstValues: DMat): Either[ArchiveError, DMat] =
    if firstValues.rows != 1 then Left(ArchiveError.ShapeMismatch("delta first values must have one row"))
    else if firstValues.cols != deltas.cols then Left(ArchiveError.ShapeMismatch("delta first values must match delta columns"))
    else
      Right(decodeTimeUnchecked(deltas, firstValues))

  private def decodeTimeUnchecked(deltas: DMat, firstValues: DMat): DMat =
    val out = DMat.newBuilder(deltas.rows + 1, deltas.cols)
    var column = 0
    while column < firstValues.cols do
      out(0, column) = firstValues(0, column)
      column += 1
    var row = 0
    while row < deltas.rows do
      column = 0
      while column < deltas.cols do
        out(row + 1, column) = out(row, column) + deltas(row, column)
        column += 1
      row += 1
    out.result()
