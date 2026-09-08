package scalafim.fmri.mvpa

import gale.linalg.DMat
import gale.linalg.DoubleLinearOperator
import gale.linalg.LinAlgError
import gale.linalg.LinearOperator
import multivar.core.CoordinateEvidence
import multivar.core.DenseMatrixView
import multivar.core.Lin
import multivar.core.OperatorRepresentation
import multivar.core.SemanticError
import multivar.core.SemanticProvenance
import multivar.core.SemanticProvenanceEvent
import multivar.core.SemanticSpace
import multivar.core.SpaceEvidence
import multivar.core.Table
import multivar.core.ValueId
import multivar.core.ValueIdentity
import resample4s.core.Reindexing

opaque type MaterializationBudget = Long

object MaterializationBudget:
  def apply(maxElements: Long): Either[EvidenceTableError, MaterializationBudget] =
    if maxElements <= 0L then Left(EvidenceTableError.InvalidMaterializationBudget(maxElements))
    else Right(maxElements)

  private[mvpa] def unsafe(maxElements: Long): MaterializationBudget =
    maxElements

  extension (budget: MaterializationBudget) inline def maxElements: Long = budget

enum MaterializationPolicy:
  case Reject
  case Allow(budget: MaterializationBudget)

final case class MaterializationReceipt(
    rowIdentity: AxisFingerprint,
    columnIdentity: AxisFingerprint,
    sourceRepresentation: OperatorRepresentation,
    rows: Int,
    columns: Int,
    elements: Long,
    budget: MaterializationBudget,
    valueIdentity: ValueIdentity
)

enum EvidenceTableError:
  case ShapeMismatch(expectedRows: Int, expectedColumns: Int, actualRows: Int, actualColumns: Int)
  case AxisOwnerMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case NominalWitnessMismatch(axis: String)
  case Semantic(error: SemanticError)
  case Linear(error: LinAlgError)
  case InvalidMaterializationBudget(value: Long)
  case MaterializationRejected
  case MaterializationBudgetExceeded(required: Long, budget: MaterializationBudget)
  case NonFiniteValue(label: String, position: Int, value: Double)
  case EmptyStack
  case StackColumnMismatch(part: Int, expected: AxisFingerprint, actual: AxisFingerprint)
  case StackRowKeysMismatch(expected: Vector[String], actual: Vector[String])
  case StackRowPurposeMismatch(part: Int, expected: AxisPurpose, actual: AxisPurpose)
  case MeasurementSourceMismatch(expected: AxisFingerprint, actual: AxisFingerprint)
  case MeasurementWitnessMismatch

  def message: String =
    this match
      case ShapeMismatch(expectedRows, expectedColumns, actualRows, actualColumns) =>
        s"evidence table expected ${expectedRows}x$expectedColumns storage, got ${actualRows}x$actualColumns"
      case AxisOwnerMismatch(expected, actual) =>
        s"operation belongs to ${actual.value}, but evidence belongs to ${expected.value}"
      case NominalWitnessMismatch(axis) =>
        s"operation and evidence use different nominal $axis witnesses"
      case Semantic(error) =>
        error.message
      case Linear(error) =>
        error.getMessage
      case InvalidMaterializationBudget(value) =>
        s"materialization budget must be positive, obtained $value"
      case MaterializationRejected =>
        "dense materialization is forbidden by policy"
      case MaterializationBudgetExceeded(required, budget) =>
        s"dense materialization requires $required elements, exceeding budget ${budget.maxElements}"
      case NonFiniteValue(label, position, value) =>
        s"$label contains non-finite value $value at row-major position $position"
      case EmptyStack =>
        "evidence table stack must contain at least one part"
      case StackColumnMismatch(part, expected, actual) =>
        s"stack part $part has column identity ${actual.value}, expected ${expected.value}"
      case StackRowKeysMismatch(expected, actual) =>
        s"stacked row keys ${actual.mkString("[", ",", "]")} do not match part rows ${expected.mkString("[", ",", "]")}"
      case StackRowPurposeMismatch(part, expected, actual) =>
        s"stack part $part has row purpose ${actual.value}, expected ${expected.value}"
      case MeasurementSourceMismatch(expected, actual) =>
        s"measurement source ${actual.value} does not match evidence columns ${expected.value}"
      case MeasurementWitnessMismatch =>
        "measurement and evidence use different nominal column witnesses"

/** One scientific row-by-column table, independent of its dense or matrix-free execution representation.
  */
final class EvidenceTable[
    Rows <: SemanticSpace,
    Columns <: SemanticSpace,
    RowKey,
    ColumnKey
] private (
    val rows: AxisRef.Aux[RowKey, Rows],
    val columns: AxisRef.Aux[ColumnKey, Columns],
    private[mvpa] val operator: DoubleLinearOperator
)(
    val table: Table[Rows, Columns]
):
  def rowCount: Int =
    rows.size

  def columnCount: Int =
    columns.size

  def representation: OperatorRepresentation =
    table.descriptor.representation

  /** Apply the evidence table to one or more column-space weight vectors. */
  def rightMultiply(weights: DMat): Either[EvidenceTableError, DMat] =
    for
      _ <- EvidenceTable.validateFinite(weights, "evidence weights")
      output <- table.apply(weights).left.map(EvidenceTableError.Semantic.apply)
      _ <- EvidenceTable.validateFinite(output, "evidence output")
    yield output

  /** Apply the structural adjoint to one or more row-space score vectors. */
  def transposeMultiply(scores: DMat): Either[EvidenceTableError, DMat] =
    for
      _ <- EvidenceTable.validateFinite(scores, "evidence row scores")
      output <- table.star.apply(scores).left.map(EvidenceTableError.Semantic.apply)
      _ <- EvidenceTable.validateFinite(output, "evidence adjoint output")
    yield output

  /** Restrict or redraw rows using the same axis-bound Resample4s relation used by targets and metadata.
    */
  def restrictRows[ChildKey, R <: Reindexing](
      by: ReindexingLeg[Rows, RowKey, ChildKey, R]
  ): Either[
    EvidenceTableError,
    EvidenceTable[by.child.Id, Columns, ChildKey, ColumnKey]
  ] =
    if rows.identity != by.parentIdentity then
      Left(
        EvidenceTableError.AxisOwnerMismatch(
          rows.identity.fingerprint,
          by.parentIdentity.fingerprint
        )
      )
    else if !(rows.evidence eq by.parentEvidence) then Left(EvidenceTableError.NominalWitnessMismatch("row"))
    else
      operator
        .andThen(by.operator)
        .left
        .map(EvidenceTableError.Linear.apply)
        .map: restricted =>
          new EvidenceTable(by.child, columns, restricted)(table.andThen(by.leg))

  /** Measure the column boundary without materializing the source table. The measurement is accepted only when both its
    * stable source identity and its nominal Multivar witness are the exact column boundary of this table.
    */
  def measureColumns[LocalKey](
      by: Measurement[Columns, ColumnKey, LocalKey]
  ): Either[
    EvidenceTableError,
    EvidenceTable[Rows, by.local.Id, RowKey, LocalKey]
  ] =
    if columns.identity != by.source.identity then
      Left(
        EvidenceTableError.MeasurementSourceMismatch(
          columns.identity.fingerprint,
          by.source.identity.fingerprint
        )
      )
    else if !(columns.evidence eq by.source.evidence) then Left(EvidenceTableError.MeasurementWitnessMismatch)
    else
      by.operator.adjoint
        .andThen(operator)
        .left
        .map(EvidenceTableError.Linear.apply)
        .map: measured =>
          new EvidenceTable(rows, by.local, measured)(by.leg.star.andThen(table))

  /** Dense storage is reachable only through an explicit positive budget and always returns a receipt tied to exact
    * row/column identities.
    */
  def materialize(
      policy: MaterializationPolicy
  ): Either[EvidenceTableError, EvidenceTable.Materialized[Rows, Columns]] =
    policy match
      case MaterializationPolicy.Reject =>
        Left(EvidenceTableError.MaterializationRejected)
      case MaterializationPolicy.Allow(budget) =>
        val elements = rowCount.toLong * columnCount.toLong
        if elements > budget.maxElements then Left(EvidenceTableError.MaterializationBudgetExceeded(elements, budget))
        else
          rightMultiply(DMat.eye(columnCount)).map: dense =>
            EvidenceTable.materialized(
              rows.identity,
              columns.identity,
              rows.evidence,
              columns.evidence,
              dense,
              MaterializationReceipt(
                rows.identity.fingerprint,
                columns.identity.fingerprint,
                representation,
                rowCount,
                columnCount,
                elements,
                budget,
                table.valueIdentity
              )
            )

object EvidenceTable:
  /** Dense evidence and its exact materialization record. Construction remains inside the evidence-table companion so a
    * downstream same-package source cannot assemble an inconsistent identity, value, and receipt tuple.
    */
  final class Materialized[
      Rows <: SemanticSpace,
      Columns <: SemanticSpace
  ] private[EvidenceTable] (
      val rowIdentity: AxisIdentity,
      val columnIdentity: AxisIdentity,
      val rows: SpaceEvidence[Rows],
      val columns: SpaceEvidence[Columns],
      val value: DMat,
      val receipt: MaterializationReceipt
  )

  private def materialized[R <: SemanticSpace, C <: SemanticSpace](
      rowIdentity: AxisIdentity,
      columnIdentity: AxisIdentity,
      rows: SpaceEvidence[R],
      columns: SpaceEvidence[C],
      value: DMat,
      receipt: MaterializationReceipt
  ): Materialized[R, C] =
    new Materialized(
      rowIdentity,
      columnIdentity,
      rows,
      columns,
      value,
      receipt
    )

  def dense[R, C](
      rows: AxisRef[R],
      columns: AxisRef[C],
      value: DMat,
      valueId: ValueId,
      provenance: SemanticProvenance = SemanticProvenance.source("dense-scientific-evidence")
  ): Either[
    EvidenceTableError,
    EvidenceTable[rows.Id, columns.Id, R, C]
  ] =
    for
      _ <- validateShape(rows, columns, value)
      _ <- validateFinite(value, "dense evidence")
      table <- Table
        .fromMatrixView(
          DenseMatrixView(value),
          rows.evidence,
          columns.evidence,
          ValueIdentity.source(valueId),
          provenance
        )
        .left
        .map(EvidenceTableError.Semantic.apply)
    yield new EvidenceTable[rows.Id, columns.Id, R, C](rows, columns, value)(table)

  def operator[R, C](
      rows: AxisRef[R],
      columns: AxisRef[C],
      value: DoubleLinearOperator,
      valueId: ValueId,
      provenance: SemanticProvenance = SemanticProvenance.source("operator-scientific-evidence")
  ): Either[
    EvidenceTableError,
    EvidenceTable[rows.Id, columns.Id, R, C]
  ] =
    for
      _ <- validateShape(rows, columns, value)
      table <- Lin
        .fromLinearMap(
          value,
          CoordinateEvidence.dual(columns.evidence),
          CoordinateEvidence.primal(rows.evidence),
          ValueIdentity.source(valueId),
          provenance
        )
        .left
        .map(EvidenceTableError.Semantic.apply)
    yield new EvidenceTable[rows.Id, columns.Id, R, C](rows, columns, value)(table)

  /** Stack row blocks without opening Multivar internals. The caller supplies the authoritative combined row axis; its
    * exact key order must equal the concatenated part order.
    */
  def stackRows[R, C](
      stackedRows: AxisRef[R],
      columns: AxisRef[C],
      parts: IndexedSeq[EvidenceTable[?, ?, R, C]]
  )(using
      AxisKeyCodec[R]
  ): Either[
    EvidenceTableError,
    EvidenceTable[stackedRows.Id, columns.Id, R, C]
  ] =
    if parts.isEmpty then Left(EvidenceTableError.EmptyStack)
    else
      val expectedColumnIdentity = columns.identity.fingerprint
      var part = 0
      while part < parts.length do
        val current = parts(part)
        if current.columns.identity != columns.identity then
          return Left(
            EvidenceTableError.StackColumnMismatch(
              part,
              expectedColumnIdentity,
              current.columns.identity.fingerprint
            )
          )
        if current.rows.identity.purpose != stackedRows.identity.purpose then
          return Left(
            EvidenceTableError.StackRowPurposeMismatch(
              part,
              stackedRows.identity.purpose,
              current.rows.identity.purpose
            )
          )
        part += 1

      val expectedKeys = parts.iterator.flatMap(_.rows.keys).toVector
      if expectedKeys != stackedRows.keys then
        Left(
          EvidenceTableError.StackRowKeysMismatch(
            expectedKeys.map(summon[AxisKeyCodec[R]].encode(_).value),
            stackedRows.keys.map(summon[AxisKeyCodec[R]].encode(_).value)
          )
        )
      else
        LinearOperator
          .block(parts.map(value => IndexedSeq(value.operator)))
          .left
          .map(EvidenceTableError.Linear.apply)
          .flatMap: stacked =>
            val inputs = parts.map(_.table.valueIdentity).toVector
            val provenance = parts
              .map(_.table.provenance)
              .reduce(_ ++ _)
              .append(SemanticProvenanceEvent.Derived("stack-rows", inputs))
            operator(
              stackedRows,
              columns,
              stacked,
              ValueId.unsafe(ValueIdentity.derived("stack-rows", inputs*).stableKey),
              provenance
            )

  private def validateShape(
      rows: AxisRef[?],
      columns: AxisRef[?],
      value: DoubleLinearOperator
  ): Either[EvidenceTableError, Unit] =
    if value.rows != rows.size || value.cols != columns.size then
      Left(
        EvidenceTableError.ShapeMismatch(
          rows.size,
          columns.size,
          value.rows,
          value.cols
        )
      )
    else Right(())

  private[mvpa] def validateFinite(
      matrix: DMat,
      label: String
  ): Either[EvidenceTableError, Unit] =
    var row = 0
    while row < matrix.rows do
      var column = 0
      while column < matrix.cols do
        val value = matrix(row, column)
        if !value.isFinite then
          return Left(
            EvidenceTableError.NonFiniteValue(
              label,
              row * matrix.cols + column,
              value
            )
          )
        column += 1
      row += 1
    Right(())
