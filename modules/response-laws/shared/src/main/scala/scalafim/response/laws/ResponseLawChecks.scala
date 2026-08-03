package scalafim.response.laws

import cats.Monad
import cats.data.{EitherT, NonEmptyVector}
import cats.syntax.all.*
import scalafim.response.{
  DecodeConsistency,
  OpenedLayout,
  Provenance,
  ProvenanceId,
  ReadCapabilities,
  ReadError,
  ReadResult,
  ReceiptConformance,
  ResolvedResponseSelection,
  ResponseBlock,
  ResponseSource,
  SelectionAxes
}

enum ResponseLawFailure:
  case SelectionMismatch(
      law: String,
      expected: ResolvedResponseSelection,
      actual: ResolvedResponseSelection
  )
  case ShapeMismatch(
      law: String,
      expectedRows: Int,
      expectedColumns: Int,
      actualRows: Int,
      actualColumns: Int
  )
  case MissingCoordinate(
      law: String,
      axis: SelectionAxes,
      coordinate: Int
  )
  case OrderMismatch(
      law: String,
      axis: SelectionAxes,
      expected: Vector[Int],
      actual: Vector[Int]
  )
  case ScalarMismatch(
      law: String,
      row: Int,
      column: Int,
      expectedBits: Long,
      actualBits: Long
  )
  case ReceiptMismatch(
      law: String,
      detail: String
  )
  case MissingProvenanceNode(
      law: String,
      node: ProvenanceId
  )
  case ChangedProvenanceNode(
      law: String,
      node: ProvenanceId
  )
  case MissingProvenanceParent(
      law: String,
      derivedRoot: ProvenanceId,
      parentRoot: ProvenanceId
  )

  def message: String =
    this match
      case SelectionMismatch(law, expected, actual) =>
        s"$law selection $actual does not match $expected"
      case ShapeMismatch(
            law,
            expectedRows,
            expectedColumns,
            actualRows,
            actualColumns
          ) =>
        s"$law shape ${actualRows}x$actualColumns does not match " +
          s"${expectedRows}x$expectedColumns"
      case MissingCoordinate(law, axis, coordinate) =>
        s"$law ${axis.label} coordinate $coordinate is absent from the reference"
      case OrderMismatch(law, axis, expected, actual) =>
        s"$law ${axis.label} order $actual does not match $expected"
      case ScalarMismatch(
            law,
            row,
            column,
            expectedBits,
            actualBits
          ) =>
        s"$law scalar ($row,$column) raw bits " +
          s"${java.lang.Long.toHexString(actualBits)} do not agree with " +
          java.lang.Long.toHexString(expectedBits)
      case ReceiptMismatch(law, detail) =>
        s"$law receipt does not conform: $detail"
      case MissingProvenanceNode(law, node) =>
        s"$law removed provenance node '${node.value}'"
      case ChangedProvenanceNode(law, node) =>
        s"$law changed provenance node '${node.value}'"
      case MissingProvenanceParent(law, derivedRoot, parentRoot) =>
        s"$law root '${derivedRoot.value}' does not reference parent root " +
          s"'${parentRoot.value}'"

final case class ResponseLawResult(
    failures: Vector[ResponseLawFailure]
):
  def isSuccess: Boolean =
    failures.isEmpty

  def ++(other: ResponseLawResult): ResponseLawResult =
    ResponseLawResult(failures ++ other.failures)

  def toEither: Either[NonEmptyVector[ResponseLawFailure], Unit] =
    NonEmptyVector.fromVector(failures).toLeft(())

object ResponseLawResult:
  val Success: ResponseLawResult =
    ResponseLawResult(Vector.empty)

object ResponseLawChecks:
  def orderingAndShape(
      law: String,
      requested: ResolvedResponseSelection,
      actual: ResponseBlock
  ): ResponseLawResult =
    val failures = Vector.newBuilder[ResponseLawFailure]
    if actual.selection != requested then
      failures += ResponseLawFailure.SelectionMismatch(
        law,
        requested,
        actual.selection
      )
    if actual.rows != requested.rows ||
        actual.columns != requested.columns
    then
      failures += ResponseLawFailure.ShapeMismatch(
        law,
        requested.rows,
        requested.columns,
        actual.rows,
        actual.columns
      )
    ResponseLawResult(failures.result())

  def blockAgreement(
      law: String,
      expected: ResponseBlock,
      actual: ResponseBlock,
      consistency: DecodeConsistency
  ): ResponseLawResult =
    val failures = Vector.newBuilder[ResponseLawFailure]
    val structure =
      orderingAndShape(law, expected.selection, actual)
    failures ++= structure.failures
    val rows = math.min(expected.rows, actual.rows)
    val columns = math.min(expected.columns, actual.columns)
    var row = 0
    while row < rows do
      var column = 0
      while column < columns do
        val expectedValue = expected(row, column)
        val actualValue = actual(row, column)
        if !consistency.agrees(expectedValue, actualValue) then
          failures += scalarMismatch(
            law,
            row,
            column,
            expectedValue,
            actualValue
          )
        column += 1
      row += 1
    ResponseLawResult(failures.result())

  def rawBitPersistence(
      law: String,
      expected: ResponseBlock,
      actual: ResponseBlock
  ): ResponseLawResult =
    blockAgreement(
      law,
      expected,
      actual,
      DecodeConsistency.ExactBits
    )

  def selection(
      law: String,
      selected: ResponseBlock,
      whole: ResponseBlock,
      consistency: DecodeConsistency
  ): ResponseLawResult =
    val failures = Vector.newBuilder[ResponseLawFailure]
    failures ++=
      orderingAndShape(law, selected.selection, selected).failures
    val wholeRows =
      whole.selection.timepoints.values.zipWithIndex.toMap
    val wholeColumns =
      whole.selection.samples.values.zipWithIndex.toMap
    var row = 0
    while row < selected.rows do
      val coordinate = selected.selection.timepoints.values(row)
      wholeRows.get(coordinate) match
        case None =>
          failures += ResponseLawFailure.MissingCoordinate(
            law,
            SelectionAxes.Time,
            coordinate
          )
        case Some(referenceRow) =>
          var column = 0
          while column < selected.columns do
            val sample = selected.selection.samples.values(column)
            wholeColumns.get(sample) match
              case None =>
                failures += ResponseLawFailure.MissingCoordinate(
                  law,
                  SelectionAxes.Samples,
                  sample
                )
              case Some(referenceColumn) =>
                val expected = whole(referenceRow, referenceColumn)
                val actual = selected(row, column)
                if !consistency.agrees(expected, actual) then
                  failures += scalarMismatch(
                    law,
                    row,
                    column,
                    expected,
                    actual
                  )
            column += 1
      row += 1
    ResponseLawResult(failures.result())

  def partition(
      law: String,
      combined: ResponseBlock,
      parts: Vector[ResponseBlock],
      consistency: DecodeConsistency
  ): ResponseLawResult =
    val failures = Vector.newBuilder[ResponseLawFailure]
    val expectedRows = parts.map(_.rows).sum
    if combined.rows != expectedRows then
      failures += ResponseLawFailure.ShapeMismatch(
        law,
        expectedRows,
        combined.columns,
        combined.rows,
        combined.columns
      )
    val expectedTimes =
      parts.flatMap(_.selection.timepoints.values)
    if combined.selection.timepoints.values != expectedTimes then
      failures += ResponseLawFailure.OrderMismatch(
        law,
        SelectionAxes.Time,
        expectedTimes,
        combined.selection.timepoints.values
      )
    var outputRow = 0
    var partIndex = 0
    while partIndex < parts.length do
      val part = parts(partIndex)
      if part.selection.schema != combined.selection.schema ||
          part.selection.samples != combined.selection.samples
      then
        failures += ResponseLawFailure.SelectionMismatch(
          law,
          combined.selection,
          part.selection
        )
      var row = 0
      while row < part.rows && outputRow < combined.rows do
        val columns = math.min(part.columns, combined.columns)
        var column = 0
        while column < columns do
          val expected = part(row, column)
          val actual = combined(outputRow, column)
          if !consistency.agrees(expected, actual) then
            failures += scalarMismatch(
              law,
              outputRow,
              column,
              expected,
              actual
            )
          column += 1
        row += 1
        outputRow += 1
      partIndex += 1
    ResponseLawResult(failures.result())

  def receipt(
      law: String,
      result: ReadResult,
      capabilities: ReadCapabilities,
      layout: OpenedLayout
  ): ResponseLawResult =
    ReceiptConformance
      .check(capabilities, result.receipt, layout)
      .fold(
        error =>
          ResponseLawResult(Vector(
            ResponseLawFailure.ReceiptMismatch(law, error.message)
          )),
        _ => ResponseLawResult.Success
      )

  def provenance(
      law: String,
      parent: Provenance,
      derived: Provenance
  ): ResponseLawResult =
    val failures = Vector.newBuilder[ResponseLawFailure]
    parent.nodes.foreach: node =>
      derived.node(node.id) match
        case None =>
          failures += ResponseLawFailure.MissingProvenanceNode(
            law,
            node.id
          )
        case Some(found) if found != node =>
          failures += ResponseLawFailure.ChangedProvenanceNode(
            law,
            node.id
          )
        case Some(_) =>
          ()
    derived.roots.foreach: root =>
      derived.node(root) match
        case None =>
          failures += ResponseLawFailure.MissingProvenanceNode(
            law,
            root
          )
        case Some(node) =>
          parent.roots.foreach: parentRoot =>
            if !node.parents.contains(parentRoot) then
              failures += ResponseLawFailure.MissingProvenanceParent(
                law,
                root,
                parentRoot
              )
    ResponseLawResult(failures.result())

  def sourceReadLaws[F[_]: Monad](
      law: String,
      source: ResponseSource[F],
      wholeSelection: ResolvedResponseSelection,
      selectedSelection: ResolvedResponseSelection,
      partitionSelections: Vector[ResolvedResponseSelection]
  ): EitherT[F, ReadError, ResponseLawResult] =
    for
      whole <- source.read(wholeSelection)
      selected <- source.read(selectedSelection)
      parts <- partitionSelections.traverse(source.read(_))
    yield
      selection(
        s"$law selection",
        selected.block,
        whole.block,
        source.consistency
      ) ++
        partition(
          s"$law partition",
          selected.block,
          parts.map(_.block),
          source.consistency
        ) ++
        orderingAndShape(
          s"$law ordering-shape",
          selectedSelection,
          selected.block
        ) ++
        receipt(
          s"$law selected receipt",
          selected,
          source.capabilities,
          source.layout
        ) ++
        parts.foldLeft(ResponseLawResult.Success): (state, part) =>
          state ++ receipt(
            s"$law partition receipt",
            part,
            source.capabilities,
            source.layout
          )

  private def scalarMismatch(
      law: String,
      row: Int,
      column: Int,
      expected: Double,
      actual: Double
  ): ResponseLawFailure =
    ResponseLawFailure.ScalarMismatch(
      law,
      row,
      column,
      java.lang.Double.doubleToRawLongBits(expected),
      java.lang.Double.doubleToRawLongBits(actual)
    )
