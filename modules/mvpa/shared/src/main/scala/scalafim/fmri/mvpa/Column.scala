package scalafim.fmri.mvpa

import multivar.core.{SemanticSpace, SpaceEvidence, ValueIdentity}
import resample4s.core.Reindexing

final case class ColumnIdentity(
    rows: AxisDescriptor,
    values: ValueIdentity
)

final case class ColumnRecord[A](
    rows: AxisRecord,
    values: Vector[A],
    valueIdentity: ValueIdentity
)

/** Values whose row compatibility is carried by a nominal Multivar space and
  * a fully verified ScalaFIM axis descriptor.
  */
final class Column[S <: SemanticSpace, A] private (
    val rows: SpaceEvidence[S],
    val rowAxis: AxisDescriptor,
    private val rowRecord: AxisRecord,
    private val storedValues: Vector[A],
    val valueIdentity: ValueIdentity
):
  val identity: ColumnIdentity =
    ColumnIdentity(rowAxis, valueIdentity)

  def size: Int =
    storedValues.length

  def apply(ordinal: Int): Either[EvidenceError, A] =
    if ordinal < 0 || ordinal >= storedValues.length then
      Left(EvidenceError.InvalidOrdinal(ordinal, storedValues.length))
    else Right(storedValues(ordinal))

  def values: Vector[A] =
    storedValues

  def stableRowKeys: Vector[String] =
    rowRecord.stableKeys

  def toRecord: ColumnRecord[A] =
    ColumnRecord(rowRecord, storedValues, valueIdentity)

  def reindex[K, R <: Reindexing](by: ReindexingLeg[S, K, R]): Column[by.Child, A] =
    val selected = by.ordinals.toVector.map(storedValues)
    new Column(
      by.child.evidence,
      by.child.descriptor,
      by.child.toRecord,
      selected,
      ValueIdentity.compose(valueIdentity, by.leg.valueIdentity)
    )

object Column:
  def fromValues[K, A](
      rows: AxisRef[K],
      values: Vector[A],
      valueIdentity: ValueIdentity
  ): Either[EvidenceError, Column[rows.Id, A]] =
    bind(rows, rows.toRecord, values, valueIdentity)

  /** Bind a decoded column only after its complete row record has been
    * verified against the caller's nominal axis.
    */
  def bind[K, A](
      rows: AxisRef[K],
      declaredRows: AxisRecord,
      values: Vector[A],
      valueIdentity: ValueIdentity
  ): Either[EvidenceError, Column[rows.Id, A]] =
    for
      _ <- rows.validateDeclared("column rows", declaredRows)
      _ <-
        if values.length == rows.size then Right(())
        else Left(EvidenceError.ShapeMismatch("column values", rows.size, values.length))
    yield new Column(
      rows.evidence,
      rows.descriptor,
      rows.toRecord,
      values,
      valueIdentity
    )

  def decode[K, A](
      rows: AxisRef[K],
      record: ColumnRecord[A]
  ): Either[EvidenceError, Column[rows.Id, A]] =
    bind(rows, record.rows, record.values, record.valueIdentity)
