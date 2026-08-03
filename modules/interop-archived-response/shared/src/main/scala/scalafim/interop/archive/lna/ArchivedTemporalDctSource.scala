package scalafim.interop.archive.lna

import cats.data.{EitherT, WriterT}
import cats.effect.Async
import cats.syntax.all.*
import scalafim.archive.{
  ArchiveError,
  ArchiveReadReceipt,
  PayloadExecutor,
  SelectionAxes as ArchiveSelectionAxes
}
import scalafim.archive.lna.{
  LnaDoubleMatrixPlan,
  LnaDoubleVectorPlan,
  Payload
}
import scalafim.latent.{
  DecodePlan,
  LogicalPayloadRead,
  RepresentationError,
  SampleOffsetValues,
  SpatialLoadingValues,
  TemporalBasisValues,
  TemporalDctRead,
  TemporalDctRepresentation
}
import scalafim.interop.archive.ArchiveResponseAccess
import scalafim.response.{
  AxisReadEvidence,
  DecodeConsistency,
  DomainReference,
  IntegrityEvidence,
  LogicalReadSummary,
  OpenedLayout,
  PayloadId,
  PayloadLayout,
  PhysicalByteEvidence,
  PhysicalLocality,
  PhysicalReadSummary,
  PhysicalReadUnit,
  Provenance,
  ProvenanceEvidence,
  ProvenanceId,
  ReadCapabilities,
  ReadError,
  ReadPlanSummary,
  ReadPlanningError,
  ReadReceipt,
  ReadResult,
  ResolvedResponseSelection,
  ResponseBlock,
  ResponseSource,
  SelectionAxes,
  SourceId
}

final class ArchivedTemporalDctRead private[interop] (
    val result: ReadResult,
    val nativeReceipts: Vector[ArchiveReadReceipt]
)

final class ArchivedTemporalDctSource[F[_]] private (
    val sourceId: SourceId,
    val model: TemporalDctRepresentation,
    archive: ArchiveResponseAccess[F],
    binding: TemporalDctArchiveBinding[F],
    val provenance: Provenance
)(using F: Async[F]) extends ResponseSource[F]:
  final case class TemporalDctReadPlan private[interop] (
      selection: ResolvedResponseSelection,
      decode: DecodePlan[ResponseBlock]
  )

  type Plan = TemporalDctReadPlan

  val schema = model.schema

  val capabilities: ReadCapabilities =
    binding.capabilities

  val layout: OpenedLayout =
    binding.layout

  val consistency: DecodeConsistency =
    model.decodeConsistency

  def plan(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, TemporalDctReadPlan] =
    model
      .compile(selection)
      .left
      .map:
        case RepresentationError.InvalidSelection(error) =>
          ReadPlanningError.InvalidSelection(error)
        case error =>
          ReadPlanningError.Unsupported(error.message)
      .map(TemporalDctReadPlan(selection, _))

  def summarize(plan: TemporalDctReadPlan): ReadPlanSummary =
    ReadPlanSummary(
      sourceId,
      schema.id,
      plan.selection.selectedAxes,
      plan.selection.rows,
      plan.selection.columns,
      capabilities.localityByAxes
    )

  def execute(
      plan: TemporalDctReadPlan
  ): EitherT[F, ReadError, ReadResult] =
    executeObserved(plan)
      .leftMap(error => ReadError.SourceFailure(sourceId, error.message))
      .map(_.result)

  def executeObserved(
      plan: TemporalDctReadPlan
  ): EitherT[F, ArchiveError, ArchivedTemporalDctRead] =
    type ArchiveF[A] = EitherT[F, ArchiveError, A]
    type Collected[A] = WriterT[ArchiveF, Vector[ArchiveReadReceipt], A]

    val decoded =
      DecodePlan.runApplicative[Collected, ResponseBlock](
        plan.decode,
        binding.interpreter(model)
      ) match
        case Left(error) =>
          WriterT.liftF[ArchiveF, Vector[ArchiveReadReceipt], ResponseBlock](
            EitherT.leftT(ArchiveError.InvalidArchive(error.message))
          )
        case Right(value) =>
          value

    val verified =
      WriterT.liftF[ArchiveF, Vector[ArchiveReadReceipt], Unit](
        archive.validateContents.void
      )

    (verified *> decoded).run.flatMap: (native, block) =>
      for
        receipt <- EitherT.fromEither[F](
          binding.receipt(plan.selection, native)
        )
        result <- EitherT.fromEither[F](
          ReadResult
            .make(block, provenance, receipt, capabilities, layout)
            .left
            .map(error => ArchiveError.InvalidArchive(error.message))
        )
      yield new ArchivedTemporalDctRead(result, native)

object ArchivedTemporalDctSource:
  def make[F[_]: Async](
      sourceId: SourceId,
      model: TemporalDctRepresentation,
      archive: ArchiveResponseAccess[F],
      lnaLayout: TemporalDctLnaLayout
  ): Either[ArchiveError, ArchivedTemporalDctSource[F]] =
    for
      layout <- responseLayout(lnaLayout)
      binding = new TemporalDctLnaBinding(
        archive.payloads,
        lnaLayout,
        layout
      )
      source <- bound(sourceId, model, archive, binding)
    yield source

  private[archive] def bound[F[_]: Async](
      sourceId: SourceId,
      model: TemporalDctRepresentation,
      archive: ArchiveResponseAccess[F],
      binding: TemporalDctArchiveBinding[F]
  ): Either[ArchiveError, ArchivedTemporalDctSource[F]] =
    val revisionEvidence =
      Vector(ProvenanceEvidence.External(
        DomainReference.unsafe(
          "archive-digest",
          archive.rootDigest.render
        )
      ))
    val provenance = Provenance.source(
      ProvenanceId.unsafe(s"${sourceId.value}:archive-root"),
      sourceId,
      revisionEvidence
    )
    Right(new ArchivedTemporalDctSource(
      sourceId,
      model,
      archive,
      binding,
      provenance
    ))

  private def responseLayout(
      layout: TemporalDctLnaLayout
  ): Either[ArchiveError, OpenedLayout] =
    val payloads =
      layout.paths.map(path =>
        PayloadLayout(PayloadId.unsafe(path.value), Vector.empty, Vector.empty)
      )
    OpenedLayout
      .make(payloads)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))

private[archive] type TemporalDctCollected[F[_], A] =
  WriterT[
    [X] =>> EitherT[F, ArchiveError, X],
    Vector[ArchiveReadReceipt],
    A
  ]

private[archive] trait TemporalDctArchiveBinding[F[_]]:
  def capabilities: ReadCapabilities
  def layout: OpenedLayout

  def interpreter(
      model: TemporalDctRepresentation
  ): DecodePlan.Interpreter[[A] =>> TemporalDctCollected[F, A]]

  def receipt(
      selection: ResolvedResponseSelection,
      native: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, ReadReceipt]

private final class TemporalDctLnaBinding[F[_]](
    executor: PayloadExecutor[F],
    lnaLayout: TemporalDctLnaLayout,
    val layout: OpenedLayout
)(using F: Async[F]) extends TemporalDctArchiveBinding[F]:
  val capabilities: ReadCapabilities =
    ReadCapabilities.uniform(PhysicalLocality.WholePayload)

  def interpreter(
      model: TemporalDctRepresentation
  ): DecodePlan.Interpreter[[A] =>> TemporalDctCollected[F, A]] =
    new TemporalDctLnaInterpreter[F](
      model,
      executor,
      lnaLayout
    )

  def receipt(
      selection: ResolvedResponseSelection,
      native: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, ReadReceipt] =
    if native.isEmpty then
      Left(ArchiveError.ReceiptMismatch(
        "temporal DCT execution produced no native payload receipts"
      ))
    else
      for
        physicalBytes <- sumBytes(native)
        cacheHits <- sumCacheHits(native)
      yield
        val payloads =
          native
            .map(receipt => PayloadId.unsafe(receipt.plan.payload.value))
            .distinct
            .map(PhysicalReadUnit.Payload.apply)
        ReadReceipt(
          LogicalReadSummary(
            selection.schema,
            selection.selectedAxes,
            selection.timepoints.values,
            selection.samples.values,
            selection.rows.toLong *
              selection.columns.toLong *
              java.lang.Double.BYTES.toLong
          ),
          PhysicalReadSummary(
            Vector(AxisReadEvidence(
              selection.selectedAxes,
              PhysicalLocality.WholePayload,
              payloads,
              payloads
            )),
            PhysicalByteEvidence.Known(physicalBytes),
            cacheHits
          ),
          Vector(IntegrityEvidence.Verified(
            DomainReference.unsafe("archive-validation", "contents")
          )),
          Vector.empty
        )

  private def sumBytes(
      receipts: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, Long] =
    var total = 0L
    var index = 0
    while index < receipts.length do
      val value = receipts(index).physicalBytes
      if total > Long.MaxValue - value then
        return Left(ArchiveError.ReceiptMismatch(
          "temporal DCT physical byte count overflows Long"
        ))
      total += value
      index += 1
    Right(total)

  private def sumCacheHits(
      receipts: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, Int] =
    var total = 0
    var index = 0
    while index < receipts.length do
      val value = receipts(index).cacheHits
      if total > Int.MaxValue - value then
        return Left(ArchiveError.ReceiptMismatch(
          "temporal DCT cache-hit count overflows Int"
        ))
      total += value
      index += 1
    Right(total)

private final class TemporalDctLnaInterpreter[F[_]](
    model: TemporalDctRepresentation,
    executor: PayloadExecutor[F],
    layout: TemporalDctLnaLayout
)(using F: Async[F]) extends DecodePlan.Interpreter[
      [A] =>> WriterT[
        [X] =>> EitherT[F, ArchiveError, X],
        Vector[ArchiveReadReceipt],
        A
      ]
    ]:
  private type ArchiveF[A] = EitherT[F, ArchiveError, A]
  private type Collected[A] =
    WriterT[ArchiveF, Vector[ArchiveReadReceipt], A]

  def apply[A](
      request: LogicalPayloadRead[A]
  ): Collected[A] =
    request match
      case TemporalDctRead.BasisRows(slot, timepoints) =>
        if slot != model.basisSlot then
          failure(s"unexpected temporal basis slot '${slot.id.value}'")
        else
          collect(
            executor.execute(
              LnaDoubleMatrixPlan.full(layout.basis, ArchiveSelectionAxes.Time)
            )
          ): payload =>
            selectedBasis(payload, timepoints.values)
      case TemporalDctRead.LoadingRows(slot, samples) =>
        if slot != model.loadingSlot then
          failure(s"unexpected spatial loading slot '${slot.id.value}'")
        else
          collect(
            executor.execute(
              LnaDoubleMatrixPlan.full(layout.loadings, ArchiveSelectionAxes.Sample)
            )
          ): payload =>
            selectedLoadings(payload, samples.values)
      case TemporalDctRead.OffsetEntries(slot, samples) =>
        (model.offsetSlot, layout.offset) match
          case (Some(expected), Some(path)) if slot == expected =>
            collect(
              executor.execute(
                LnaDoubleVectorPlan.full(path, ArchiveSelectionAxes.Sample)
              )
            ): payload =>
              selectedOffsets(payload, samples.values)
          case _ =>
            failure(s"unexpected sample offset slot '${slot.id.value}'")
      case other =>
        failure(s"unsupported logical LNA read '${other.slot.id.value}'")

  private def collect[P, A](
      execution: EitherT[F, ArchiveError, scalafim.archive.Observed[P]]
  )(
      convert: P => Either[ArchiveError, A]
  ): Collected[A] =
    WriterT:
      execution.flatMap: observed =>
        EitherT
          .fromEither[F](convert(observed.value))
          .map(value => (Vector(observed.receipt), value))

  private def failure[A](detail: String): Collected[A] =
    WriterT.liftF(EitherT.leftT(ArchiveError.InvalidArchive(detail)))

  private def selectedBasis(
      payload: Payload.DoubleMatrix,
      indices: Vector[Int]
  ): Either[ArchiveError, TemporalBasisValues] =
    val data = payload.data
    val values = new Array[Double](indices.length * data.cols)
    copyRows(data, indices, values)
    TemporalBasisValues
      .copyFromRowMajor(indices.length, data.cols, values)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))

  private def selectedLoadings(
      payload: Payload.DoubleMatrix,
      indices: Vector[Int]
  ): Either[ArchiveError, SpatialLoadingValues] =
    val data = payload.data
    val values = new Array[Double](indices.length * data.cols)
    copyRows(data, indices, values)
    SpatialLoadingValues
      .copyFromRowMajor(indices.length, data.cols, values)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))

  private def selectedOffsets(
      payload: Payload.DoubleVector,
      indices: Vector[Int]
  ): Either[ArchiveError, SampleOffsetValues] =
    val values = new Array[Double](indices.length)
    var index = 0
    while index < indices.length do
      values(index) = payload.values(indices(index))
      index += 1
    SampleOffsetValues
      .copyFrom(values)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))

  private def copyRows(
      data: scalafim.image.DMat,
      indices: Vector[Int],
      output: Array[Double]
  ): Unit =
    var row = 0
    while row < indices.length do
      var column = 0
      while column < data.cols do
        output(row * data.cols + column) = data(indices(row), column)
        column += 1
      row += 1
