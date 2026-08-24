package scalafim.image

import image4s.geometry.D3
import image4s.geometry.Frame
import locus4s.Index

enum SelectedVolumeWindowError:
  case CenterPositionOutOfBounds(position: Int, size: Int)
  case CenterMismatch(position: Int, expected: Int, actual: Int)

  def message: String =
    this match
      case CenterPositionOutOfBounds(position, size) =>
        s"selected window center position $position is outside [0, $size)"
      case CenterMismatch(position, expected, actual) =>
        s"selected window position $position names voxel $actual, not center $expected"

/** A selected scalar image with one certified center position.
  *
  * Support and sample order remain owned exclusively by `values.selection`;
  * the window adds only the scientific role of one selected position.
  */
final class SelectedVolumeWindow[
    F <: Frame[D3],
    S,
    A,
    Sem
] private (
    val values: SelectedVolume[F, S, A, Sem],
    val center: Index[S],
    val centerPosition: Int
):
  inline def size: Int =
    values.selection.size

  inline def apply(position: Int): A =
    values.data(position)

object SelectedVolumeWindow:
  def make[F <: Frame[D3], S, A, Sem](
      values: SelectedVolume[F, S, A, Sem],
      center: Index[S],
      centerPosition: Int
  ): Either[
    SelectedVolumeWindowError,
    SelectedVolumeWindow[F, S, A, Sem]
  ] =
    if centerPosition < 0 || centerPosition >= values.selection.size then
      Left(
        SelectedVolumeWindowError.CenterPositionOutOfBounds(
          centerPosition,
          values.selection.size
        )
      )
    else
      val position =
        values.selection.positions.indexAtValidatedOrdinal(centerPosition)
      val actual = values.selection(position).ordinal
      if actual != center.ordinal then
        Left(
          SelectedVolumeWindowError.CenterMismatch(
            centerPosition,
            center.ordinal,
            actual
          )
        )
      else
        Right(new SelectedVolumeWindow(values, center, centerPosition))
