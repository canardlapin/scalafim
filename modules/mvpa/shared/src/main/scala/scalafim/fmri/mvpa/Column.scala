package scalafim.fmri.mvpa

import multivar.core.SemanticSpace
import multivar.core.SpaceEvidence
import resample4s.core.Reindexing
import resample4s.core.pullFrom

import scala.reflect.ClassTag

enum ColumnError:
  case Axis(error: AxisRefError)
  case LengthMismatch(expected: Int, actual: Int)
  case PositionOutOfBounds(position: Int, size: Int)
  case OwnerMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case NominalWitnessMismatch
  case ReindexingFailure(detail: String)

  def message: String =
    this match
      case Axis(error) =>
        error.message
      case LengthMismatch(expected, actual) =>
        s"column contains $actual values, expected $expected for its row axis"
      case PositionOutOfBounds(position, size) =>
        s"column position $position is outside [0, $size)"
      case OwnerMismatch(expected, actual) =>
        s"reindexing belongs to ${actual.value}, but column rows belong to ${expected.value}"
      case NominalWitnessMismatch =>
        "reindexing and column have different nominal row witnesses"
      case ReindexingFailure(detail) =>
        detail

/** Values whose row ownership is an exact nominal scientific space. */
final class Column[S <: SemanticSpace, A] private (
    val rowIdentity: AxisIdentity,
    val rows: SpaceEvidence[S],
    val values: IArray[A]
):
  def size: Int =
    values.length

  def at(position: Int): Either[ColumnError, A] =
    if position < 0 || position >= size then Left(ColumnError.PositionOutOfBounds(position, size))
    else Right(values(position))

  def toVector: Vector[A] =
    values.toVector

  def map[B: ClassTag](function: A => B): Column[S, B] =
    val mapped = new Array[B](size)
    var position = 0
    while position < size do
      mapped(position) = function(values(position))
      position += 1
    new Column(rowIdentity, rows, IArray.unsafeFromArray(mapped))

  def reindex[K, C, R <: Reindexing](
      by: ReindexingLeg[S, K, C, R]
  )(using ClassTag[A]): Either[ColumnError, Column[by.child.Id, A]] =
    if rowIdentity != by.parentIdentity then
      Left(
        ColumnError.OwnerMismatch(
          rowIdentity.fingerprint,
          by.parentIdentity.fingerprint
        )
      )
    else if !(rows eq by.parentEvidence) then Left(ColumnError.NominalWitnessMismatch)
    else
      by.reindexing
        .pullFrom(values)
        .left
        .map(error =>
          ColumnError.ReindexingFailure(
            s"column length ${error.left} does not match reindexing population ${error.right}"
          )
        )
        .map(pulled => new Column(by.child.identity, by.child.evidence, pulled))

object Column:
  def apply[K, A: ClassTag](
      rows: AxisRef[K],
      values: Seq[A]
  ): Either[ColumnError, Column[rows.Id, A]] =
    val owned = values.toArray
    fromOwnedChecked(rows, IArray.unsafeFromArray(owned))

  def fromIArray[K, A](
      rows: AxisRef[K],
      values: IArray[A]
  ): Either[ColumnError, Column[rows.Id, A]] =
    fromOwnedChecked(rows, values)

  /** Runtime boundary: declared ownership is validated against the supplied nominal axis before values become
    * accessible as a typed column.
    */
  def decode[K, A](
      expectedRows: AxisRef[K],
      declaredRows: AxisIdentityRecord,
      values: IArray[A]
  ): Either[ColumnError, Column[expectedRows.Id, A]] =
    expectedRows
      .bind(declaredRows)
      .left
      .map(ColumnError.Axis.apply)
      .flatMap(_ => fromIArray(expectedRows, values))

  private def fromOwnedChecked[K, A](
      rows: AxisRef[K],
      values: IArray[A]
  ): Either[ColumnError, Column[rows.Id, A]] =
    if values.length != rows.size then Left(ColumnError.LengthMismatch(rows.size, values.length))
    else Right(new Column(rows.identity, rows.evidence, values))
