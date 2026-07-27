package scalafim.interop.archive.zarr

import cats.data.EitherT
import cats.effect.{Async, Resource}
import narr.NArray
import scalafim.archive.{
  ArchiveError,
  ArchiveReadReceipt,
  ByteInterval as ArchiveByteInterval,
  ObjectRange,
  PhysicalLocality as ArchivePhysicalLocality,
  PhysicalObjectId,
  ReadObservation,
  SelectionAxes as ArchiveSelectionAxes
}
import scalafim.archive.zarr.{
  DenseBoldRevisionMetadata,
  ZarrPayloadPlan
}
import scalafim.interop.archive.{
  ArchiveResponseAccess,
  ArchivedResponseFamily,
  RepresentationEnvelope
}
import scalafim.response.{
  AxisReadEvidence,
  ByteRange,
  ChunkId,
  DecodeConsistency,
  DomainReference,
  IntegrityEvidence,
  LogicalReadSummary,
  ObjectId,
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
  ReadResult as ResponseReadResult,
  ResolvedResponseSelection,
  ResponseBlock,
  ResponseSource,
  SelectionAxes,
  SourceId
}
import scalafim.zarr.{
  ArraySelection,
  ChunkPlanner,
  Coordinate,
  CoordinateBatch,
  PhysicalLayout,
  PrimitiveBlock,
  ReadLimits,
  ReadResult as ZarrReadResult,
  ShardPlanner,
  StoreKey
}

final class DenseBoldZarrRead private[interop] (
    val result: ResponseReadResult,
    val nativeReceipt: ArchiveReadReceipt
)

final class DenseBoldZarrSource[F[_]] private (
    val sourceId: SourceId,
    descriptor: DenseBoldZarrDescriptor,
    archive: ArchiveResponseAccess[F],
    val layout: OpenedLayout,
    val provenance: Provenance,
    limits: ReadLimits
)(using F: Async[F]) extends ResponseSource[F]:
  final case class DenseZarrReadPlan private[interop] (
      selection: ResolvedResponseSelection,
      payload: ZarrPayloadPlan
  )

  type Plan = DenseZarrReadPlan

  val schema = descriptor.schema

  val consistency: DecodeConsistency =
    DecodeConsistency.ExactBits

  val capabilities: ReadCapabilities =
    descriptor.array.layout match
      case PhysicalLayout.Direct(_) =>
        ReadCapabilities.uniform(PhysicalLocality.ChunkBounded)
      case PhysicalLayout.Sharded(_, _, _, _, _) =>
        ReadCapabilities.uniform(PhysicalLocality.ByteRangeBounded)

  def plan(
      selection: ResolvedResponseSelection
  ): Either[ReadPlanningError, DenseZarrReadPlan] =
    val planned =
      for
        _ <- ResolvedResponseSelection
          .validateFor(schema, selection)
          .left
          .map(error => ArchiveError.InvalidArchive(error.message))
        points <- DenseBoldZarrSource.canonicalPoints(
          descriptor,
          selection
        )
        arraySelection = ArraySelection.PointSelection(points)
        covering <- DenseBoldZarrSource.coveringObjects(
          descriptor,
          arraySelection
        )
        payloadPlan <- descriptor.array.layout match
          case PhysicalLayout.Direct(_) =>
            ZarrPayloadPlan.chunkBounded(
              descriptor.payload.id,
              arraySelection,
              DenseBoldZarrSource.archiveAxes(selection.selectedAxes),
              covering.map(_.id),
              limits
            )
          case PhysicalLayout.Sharded(_, _, _, _, _) =>
            val ranges = covering.map: value =>
              ObjectRange(
                value.id,
                ArchiveByteInterval.unsafe(0L, value.length)
              )
            ZarrPayloadPlan.byteRangeBounded(
              descriptor.payload.id,
              arraySelection,
              DenseBoldZarrSource.archiveAxes(selection.selectedAxes),
              ranges,
              limits
            )
      yield DenseZarrReadPlan(selection, payloadPlan)
    planned.left.map(error =>
      ReadPlanningError.Unsupported(error.message)
    )

  def summarize(plan: DenseZarrReadPlan): ReadPlanSummary =
    ReadPlanSummary(
      sourceId,
      schema.id,
      plan.selection.selectedAxes,
      plan.selection.rows,
      plan.selection.columns,
      capabilities.localityByAxes
    )

  def execute(
      plan: DenseZarrReadPlan
  ): EitherT[F, ReadError, ResponseReadResult] =
    executeObserved(plan)
      .leftMap(error => ReadError.SourceFailure(sourceId, error.message))
      .map(_.result)

  def executeObserved(
      plan: DenseZarrReadPlan
  ): EitherT[F, ArchiveError, DenseBoldZarrRead] =
    archive.payloads.execute(plan.payload).flatMap: observed =>
      EitherT.fromEither[F]:
        for
          block <- DenseBoldZarrSource.responseBlock(
            descriptor,
            plan.selection,
            observed.value
          )
          receipt <- DenseBoldZarrSource.responseReceipt(
            descriptor,
            plan.selection,
            observed.receipt
          )
          result <- ResponseReadResult
            .make(block, provenance, receipt, capabilities, layout)
            .left
            .map(error => ArchiveError.ReceiptMismatch(error.message))
        yield new DenseBoldZarrRead(result, observed.receipt)

object DenseBoldZarrSource:
  def make[F[_]: Async](
      sourceId: SourceId,
      descriptor: DenseBoldZarrDescriptor,
      archive: ArchiveResponseAccess[F],
      limits: ReadLimits = ReadLimits(maxConcurrentRequests = 1)
  ): Either[ArchiveError, DenseBoldZarrSource[F]] =
    if limits.maxConcurrentRequests != 1 then
      Left(ArchiveError.InvalidArchive(
        "dense Zarr receipt evidence requires maxConcurrentRequests = 1"
      ))
    else
      for
        openedLayout <- responseLayout(descriptor)
        provenance =
          Provenance.source(
            ProvenanceId.unsafe(s"${sourceId.value}:archive-root"),
            sourceId,
            Vector(ProvenanceEvidence.External(
              DomainReference.unsafe(
                "archive-digest",
                archive.rootDigest.render
              )
            ))
          )
      yield new DenseBoldZarrSource(
        sourceId,
        descriptor,
        archive,
        openedLayout,
        provenance,
        limits
      )

  private def responseLayout(
      descriptor: DenseBoldZarrDescriptor
  ): Either[ArchiveError, OpenedLayout] =
    val payload = PayloadId.unsafe(descriptor.payload.id.value)
    val objects = descriptor.objects.map(value =>
      ObjectId.unsafe(value.id.value)
    )
    val chunks = descriptor.objects.map(value =>
      ChunkId.unsafe(value.id.value)
    )
    OpenedLayout
      .make(Vector(PayloadLayout(payload, objects, chunks)))
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))

  private def canonicalPoints(
      descriptor: DenseBoldZarrDescriptor,
      selection: ResolvedResponseSelection
  ): Either[ArchiveError, CoordinateBatch] =
    val shape = descriptor.array.shape
    val zSize = shape.axis(1)
    val ySize = shape.axis(2)
    val xSize = shape.axis(3)
    val plane =
      checkedProduct(xSize, ySize, "canonical x by y spatial plane") match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          value
    val spatial =
      checkedProduct(plane, zSize, "canonical spatial sample count") match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          value
    val requested =
      selection.rows.toLong * selection.columns.toLong
    if requested > Int.MaxValue.toLong then
      Left(ArchiveError.ShapeMismatch(
        "dense Zarr selected response value count exceeds Int"
      ))
    else
      val coordinates = Vector.newBuilder[Coordinate]
      coordinates.sizeHint(requested.toInt)
      var row = 0
      while row < selection.rows do
        val time = selection.timepoints(row).value.toLong
        var column = 0
        while column < selection.columns do
          val sample = selection.samples(column).value.toLong
          if sample < 0L || sample >= spatial then
            return Left(ArchiveError.ShapeMismatch(
              s"dense Zarr sample index $sample lies outside [0,$spatial)"
            ))
          val z = sample / plane
          val withinPlane = sample % plane
          val y = withinPlane / xSize
          val x = withinPlane % xSize
          Coordinate(time, z, y, x) match
            case Left(error) =>
              return Left(ArchiveError.InvalidArchive(error.message))
            case Right(value) =>
              coordinates += value
          column += 1
        row += 1
      CoordinateBatch
        .within(shape, coordinates.result())
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))

  private def coveringObjects(
      descriptor: DenseBoldZarrDescriptor,
      selection: ArraySelection
  ): Either[ArchiveError, Vector[DenseBoldZarrObject]] =
    val coordinates =
      descriptor.array.layout match
        case PhysicalLayout.Direct(_) =>
          ChunkPlanner
            .plan(descriptor.array.grid, selection)
            .left
            .map(error => ArchiveError.InvalidArchive(error.message))
            .map(_.demands.map(_.coordinate))
        case PhysicalLayout.Sharded(grid, _, _, _, _) =>
          for
            inner <- ChunkPlanner
              .plan(grid.globalInnerGrid, selection)
              .left
              .map(error => ArchiveError.InvalidArchive(error.message))
            grouped <- ShardPlanner
              .group(grid, inner)
              .left
              .map(error => ArchiveError.InvalidArchive(error.message))
          yield grouped.shards.map(_.coordinate)
    coordinates.flatMap(resolveCoveringObjects(descriptor, _))

  private def resolveCoveringObjects(
      descriptor: DenseBoldZarrDescriptor,
      values: Vector[scalafim.zarr.ChunkCoordinate]
  ): Either[ArchiveError, Vector[DenseBoldZarrObject]] =
    val result = Vector.newBuilder[DenseBoldZarrObject]
    val seen = scala.collection.mutable.HashSet.empty[String]
    var index = 0
    while index < values.length do
      val relative =
        descriptor.array.chunkKeyEncoding.encode(values(index)).value
      val key =
        descriptor.canonicalPath.key(relative) match
          case Left(error) =>
            return Left(ArchiveError.InvalidArchive(error.message))
          case Right(found) =>
            found
      val id = PhysicalObjectId.unsafe(key.value)
      descriptor.objectLength(id) match
        case None =>
          return Left(ArchiveError.InvalidArchive(
            s"dense Zarr plan covers unpublished object '${id.value}'"
          ))
        case Some(length) if seen.add(id.value) =>
          result += DenseBoldZarrObject(id, length)
        case Some(_) =>
          ()
      index += 1
    val found = result.result()
    if found.isEmpty then
      Left(ArchiveError.InvalidArchive(
        "dense Zarr selection has no covering payload object"
      ))
    else Right(found)

  private def responseBlock(
      descriptor: DenseBoldZarrDescriptor,
      selection: ResolvedResponseSelection,
      read: ZarrReadResult
  ): Either[ArchiveError, ResponseBlock] =
    val expected =
      selection.rows.toLong * selection.columns.toLong
    if expected > Int.MaxValue.toLong then
      Left(ArchiveError.ShapeMismatch(
        "dense response value count exceeds Int"
      ))
    else if read.block.elementCount != expected.toInt then
      Left(ArchiveError.ShapeMismatch(
        s"dense Zarr returned ${read.block.elementCount} values, expected $expected"
      ))
    else
      val values = NArray.ofSize[Double](expected.toInt)
      var index = 0
      while index < values.length do
        val calibrated =
          primitiveDouble(read.block, index) * descriptor.scale +
            descriptor.offset
        if
          descriptor.schema.signal.nonFinite ==
            scalafim.response.NonFinitePolicy.Reject &&
            !calibrated.isFinite
        then
          return Left(ArchiveError.InvalidArchive(
            s"dense Zarr calibration produced non-finite value at $index"
          ))
        values(index) = calibrated
        index += 1
      Right(ResponseBlock.unsafeFromOwnedRowMajor(values, selection))

  private def primitiveDouble(
      block: PrimitiveBlock,
      index: Int
  ): Double =
    block match
      case PrimitiveBlock.Bool(values) =>
        if values(index) then 1.0 else 0.0
      case PrimitiveBlock.Int8(values) =>
        values(index).toDouble
      case PrimitiveBlock.UInt8(values) =>
        (values(index) & 0xff).toDouble
      case PrimitiveBlock.Int16(values) =>
        values(index).toDouble
      case PrimitiveBlock.UInt16(values) =>
        (values(index) & 0xffff).toDouble
      case PrimitiveBlock.Int32(values) =>
        values(index).toDouble
      case PrimitiveBlock.UInt32(values) =>
        java.lang.Integer.toUnsignedLong(values(index)).toDouble
      case PrimitiveBlock.Int64(values) =>
        values(index).toDouble
      case PrimitiveBlock.UInt64(values) =>
        val value = values(index)
        if value >= 0L then value.toDouble
        else
          (value & Long.MaxValue).toDouble + 9223372036854775808.0
      case PrimitiveBlock.Float32(values) =>
        values(index).toDouble
      case PrimitiveBlock.Float64(values) =>
        values(index)

  private def responseReceipt(
      descriptor: DenseBoldZarrDescriptor,
      selection: ResolvedResponseSelection,
      native: ArchiveReadReceipt
  ): Either[ArchiveError, ReadReceipt] =
    val payload = PayloadId.unsafe(descriptor.payload.id.value)
    val axes = selection.selectedAxes
    for
      evidence <- native.observedLocality match
        case ArchivePhysicalLocality.ChunkBounded =>
          val touched =
            native.observations
              .flatMap(observationObject)
              .distinct
              .map(id =>
                PhysicalReadUnit.Chunk(payload, ChunkId.unsafe(id.value))
              )
          val covering =
            native.plan.locality.coveringObjects.map(id =>
              PhysicalReadUnit.Chunk(payload, ChunkId.unsafe(id.value))
            )
          Right(AxisReadEvidence(
            axes,
            PhysicalLocality.ChunkBounded,
            touched,
            covering
          ))
        case ArchivePhysicalLocality.ByteRangeBounded =>
          for
            touched <- responseObservationRanges(native.observations)
            covering <- responseCoveringRanges(
              native.plan.locality.coveringRanges
            )
          yield AxisReadEvidence(
            axes,
            PhysicalLocality.ByteRangeBounded,
            touched,
            covering
          )
        case other =>
          Left(ArchiveError.ReceiptMismatch(
            s"dense Zarr source cannot adapt observed locality $other"
          ))
    yield ReadReceipt(
      LogicalReadSummary(
        selection.schema,
        axes,
        selection.timepoints.values,
        selection.samples.values,
        selection.rows.toLong *
          selection.columns.toLong *
          java.lang.Double.BYTES.toLong
      ),
      PhysicalReadSummary(
        Vector(evidence),
        PhysicalByteEvidence.Known(native.physicalBytes),
        native.cacheHits
      ),
      Vector(IntegrityEvidence.Verified(
        DomainReference.unsafe("zarr-codec", "crc32c")
      )),
      Vector.empty
    )

  private def responseObservationRanges(
      observations: Vector[ReadObservation]
  ): Either[ArchiveError, Vector[PhysicalReadUnit]] =
    val result = Vector.newBuilder[PhysicalReadUnit]
    var index = 0
    while index < observations.length do
      observations(index) match
        case ReadObservation.ByteRange(objectId, requested, _) =>
          responseRange(objectId, requested) match
            case Left(error) =>
              return Left(error)
            case Right(value) =>
              result += value
        case other =>
          return Left(ArchiveError.ReceiptMismatch(
            s"byte-range dense Zarr receipt contains $other"
          ))
      index += 1
    Right(result.result())

  private def responseCoveringRanges(
      ranges: Vector[ObjectRange]
  ): Either[ArchiveError, Vector[PhysicalReadUnit]] =
    val result = Vector.newBuilder[PhysicalReadUnit]
    var index = 0
    while index < ranges.length do
      responseRange(ranges(index).objectId, ranges(index).interval) match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          result += value
      index += 1
    Right(result.result())

  private def responseRange(
      objectId: PhysicalObjectId,
      interval: ArchiveByteInterval
  ): Either[ArchiveError, PhysicalReadUnit] =
    ByteRange
      .make(interval.offset, interval.length)
      .left
      .map(error => ArchiveError.ReceiptMismatch(error.message))
      .map(bytes =>
        PhysicalReadUnit.Range(ObjectId.unsafe(objectId.value), bytes)
      )

  private def observationObject(
      observation: ReadObservation
  ): Option[PhysicalObjectId] =
    observation match
      case ReadObservation.WholeObject(objectId, _) =>
        Some(objectId)
      case ReadObservation.ByteRange(objectId, _, _) =>
        Some(objectId)
      case ReadObservation.ObjectLength(objectId, _) =>
        Some(objectId)
      case _ =>
        None

  private def checkedProduct(
      left: Long,
      right: Long,
      label: String
  ): Either[ArchiveError, Long] =
    if left != 0L && right > Long.MaxValue / left then
      Left(ArchiveError.ShapeMismatch(s"$label overflows Long"))
    else Right(left * right)

  private def archiveAxes(
      axes: SelectionAxes
  ): ArchiveSelectionAxes =
    axes match
      case SelectionAxes.Time =>
        ArchiveSelectionAxes.Time
      case SelectionAxes.Samples =>
        ArchiveSelectionAxes.Sample
      case SelectionAxes.TimeAndSamples =>
        ArchiveSelectionAxes.TimeAndSample

final class DenseBoldZarrRepresentationFamily[F[_]] private (
    limits: ReadLimits
)(using F: Async[F]) extends ArchivedResponseFamily[F]:
  val key =
    DenseBoldRevisionMetadata.Representation

  def open(
      envelope: RepresentationEnvelope,
      archive: ArchiveResponseAccess[F]
  ): scalafim.archive.ArchiveResource[F, ResponseSource[F]] =
    Resource.eval(archive.validateContents).flatMap: _ =>
      Resource.eval(EitherT.fromEither[F]:
        for
          descriptor <- DenseBoldZarrDescriptor.decode(envelope)
          source <- DenseBoldZarrSource.make(
            SourceId.unsafe(s"zarr-dense:${archive.revisionId.value}"),
            descriptor,
            archive,
            limits
          )
        yield source: ResponseSource[F]
      )

object DenseBoldZarrRepresentationFamily:
  def apply[F[_]: Async](
      limits: ReadLimits = ReadLimits(maxConcurrentRequests = 1)
  ): DenseBoldZarrRepresentationFamily[F] =
    new DenseBoldZarrRepresentationFamily[F](limits)
