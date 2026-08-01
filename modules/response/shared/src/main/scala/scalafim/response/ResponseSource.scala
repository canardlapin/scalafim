package scalafim.response

import cats.{Applicative, Monad}
import cats.data.EitherT
import ravel.{Array1, NDArray, Shape}

final case class ReadPlanSummary(
    source: SourceId,
    schema: ResponseSchemaId,
    axes: SelectionAxes,
    rows: Int,
    columns: Int,
    localityByAxes: Vector[AxisLocality]
):
  def localityFor(
      selectedAxes: SelectionAxes
  ): Option[PhysicalLocality] =
    localityByAxes.find(_.axes == selectedAxes).map(_.guarantee)

trait ResponseSource[F[_]]:
  type Plan

  def sourceId: SourceId
  def schema: ResponseSchema
  def capabilities: ReadCapabilities
  def consistency: DecodeConsistency
  def layout: OpenedLayout

  def plan(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, Plan]

  def summarize(plan: Plan): ReadPlanSummary

  def execute(
      plan: Plan
  ): EitherT[F, ReadError, ReadResult]

  final def planned(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, PlannedRead[F]] =
    plan(selection).map(found => PlannedRead(this)(found))

  final def read(
      selection: ResolvedResponseSelection
  )(using Monad[F]): EitherT[F, ReadError, ReadResult] =
    EitherT
      .fromEither[F](plan(selection).left.map(ReadError.Planning.apply))
      .flatMap(execute)

sealed trait PlannedRead[F[_]]:
  type P
  val source: ResponseSource[F] { type Plan = P }
  val plan: P

  final def summary: ReadPlanSummary =
    source.summarize(plan)

  final def execute: EitherT[F, ReadError, ReadResult] =
    source.execute(plan)

object PlannedRead:
  def apply[F[_]](
      requestedSource: ResponseSource[F]
  )(
      requestedPlan: requestedSource.Plan
  ): PlannedRead[F] =
    new PlannedRead[F]:
      type P = requestedSource.Plan
      val source: ResponseSource[F] { type Plan = P } =
        requestedSource
      val plan: P =
        requestedPlan

final class ReadResult private (
    val block: ResponseBlock,
    val provenance: Provenance,
    val receipt: ReadReceipt
)

object ReadResult:
  def make(
      block: ResponseBlock,
      provenance: Provenance,
      receipt: ReadReceipt,
      capabilities: ReadCapabilities,
      layout: OpenedLayout
  ): Either[ReadError, ReadResult] =
    val logical = receipt.logical
    if logical.schema != block.selection.schema then
      Left(ReadError.ResultMismatch(
        ReadResultMismatch.ReceiptSchema(block.selection.schema, logical.schema)
      ))
    else if logical.timepoints != block.selection.timepoints.values then
      Left(ReadError.ResultMismatch(
        ReadResultMismatch.TimepointOrder(
          block.selection.timepoints.values,
          logical.timepoints
        )
      ))
    else if logical.samples != block.selection.samples.values then
      Left(ReadError.ResultMismatch(
        ReadResultMismatch.SampleOrder(
          block.selection.samples.values,
          logical.samples
        )
      ))
    else
      ReceiptConformance
        .check(capabilities, receipt, layout)
        .left
        .map(ReadError.InvalidReceipt.apply)
        .map(_ => new ReadResult(block, provenance, receipt))

final class InMemoryResponseSource[F[_]] private (
    val sourceId: SourceId,
    val schema: ResponseSchema,
    private val ownedFullRowMajor: Array1[Double],
    val consistency: DecodeConsistency,
    val provenance: Provenance
)(using Applicative[F]) extends ResponseSource[F]:
  final case class DenseReadPlan private[response] (
      selection: ResolvedResponseSelection
  )

  type Plan = DenseReadPlan

  val capabilities: ReadCapabilities =
    ReadCapabilities.Resident

  val layout: OpenedLayout =
    OpenedLayout.Resident

  def plan(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, DenseReadPlan] =
    ResolvedResponseSelection
      .validateFor(schema, selection)
      .left
      .map(error => ReadPlanningError.InvalidSelection(error))
      .map(_ => DenseReadPlan(selection))

  def summarize(plan: DenseReadPlan): ReadPlanSummary =
    ReadPlanSummary(
      sourceId,
      schema.id,
      plan.selection.selectedAxes,
      plan.selection.rows,
      plan.selection.columns,
      capabilities.localityByAxes
    )

  def execute(
      plan: DenseReadPlan
  ): EitherT[F, ReadError, ReadResult] =
    val selection = plan.selection
    val values =
      NDArray.tabulate[Double](selection.rows * selection.columns): outputIndex =>
        val outputRow = outputIndex / selection.columns
        val outputColumn = outputIndex % selection.columns
        val sourceRow = selection.timepoints(outputRow).value
        val sourceColumn = selection.samples(outputColumn).value
        ownedFullRowMajor(sourceRow * schema.samples.count + sourceColumn)

    val result =
      for
        block <- ResponseBlock
          .fromOwnedRowMajor(values, selection)
          .left
          .map(ReadError.InvalidBlock.apply)
        read <- ReadResult.make(
          block,
          provenance,
          ReadReceipt.resident(selection),
          capabilities,
          layout
        )
      yield read
    EitherT.fromEither[F](result)

object InMemoryResponseSource:
  def copyFromRowMajor[F[_]: Applicative](
      sourceId: SourceId,
      schema: ResponseSchema,
      values: Array[Double],
      consistency: DecodeConsistency = DecodeConsistency.ExactBits,
      provenanceNode: Option[ProvenanceId] = None
  ): Either[ResponseShapeError, InMemoryResponseSource[F]] =
    val expected = schema.time.count.toLong * schema.samples.count.toLong
    if expected > Int.MaxValue.toLong then
      Left(ResponseShapeError.ValueCountOverflow(schema.time.count, schema.samples.count))
    else if values.length != expected.toInt then
      Left(ResponseShapeError.ValueCountMismatch(expected.toInt, values.length))
    else
      var index = 0
      while index < values.length do
        val value = values(index)
        if schema.signal.nonFinite == NonFinitePolicy.Reject && !value.isFinite then
          return Left(ResponseShapeError.NonFiniteValue(index, value))
        index += 1
      val node = provenanceNode.getOrElse(ProvenanceId.unsafe(s"${sourceId.value}:root"))
      Right(
        new InMemoryResponseSource(
          sourceId,
          schema,
          NDArray.fromSeq(Shape(values.length), values),
          consistency,
          Provenance.source(node, sourceId)
        )
      )

  def copyFromRowMajor[F[_]: Applicative](
      sourceId: SourceId,
      schema: ResponseSchema,
      values: Array1[Double],
      consistency: DecodeConsistency,
      provenanceNode: Option[ProvenanceId]
  ): Either[ResponseShapeError, InMemoryResponseSource[F]] =
    val expected = schema.time.count.toLong * schema.samples.count.toLong
    if expected > Int.MaxValue.toLong then
      Left(ResponseShapeError.ValueCountOverflow(schema.time.count, schema.samples.count))
    else if values.size != expected.toInt then
      Left(ResponseShapeError.ValueCountMismatch(expected.toInt, values.size))
    else
      var index = 0
      while index < values.size do
        val value = values(index)
        if schema.signal.nonFinite == NonFinitePolicy.Reject && !value.isFinite then
          return Left(ResponseShapeError.NonFiniteValue(index, value))
        index += 1
      val node = provenanceNode.getOrElse(ProvenanceId.unsafe(s"${sourceId.value}:root"))
      Right(
        new InMemoryResponseSource(
          sourceId,
          schema,
          values,
          consistency,
          Provenance.source(node, sourceId)
        )
      )
