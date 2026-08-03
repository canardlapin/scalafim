package scalafim.interop.archive.zarr

import cats.data.{EitherT, WriterT}
import cats.effect.Async
import scalafim.archive.{
  ArchiveError,
  ArchiveReadReceipt,
  ByteInterval as ArchiveByteInterval,
  ObjectRange,
  Observed,
  PayloadExecutor,
  PayloadId as ArchivePayloadId,
  PhysicalLocality as ArchivePhysicalLocality,
  PhysicalObjectId,
  ReadObservation,
  SelectionAxes as ArchiveSelectionAxes
}
import scalafim.archive.zarr.ZarrPayloadPlan
import scalafim.interop.archive.ArchiveResponseAccess
import scalafim.interop.archive.lna.{
  ArchivedTemporalDctSource,
  TemporalDctArchiveBinding,
  TemporalDctCollected
}
import scalafim.latent.{
  DecodePlan,
  LogicalPayloadRead,
  SampleOffsetValues,
  SpatialLoadingValues,
  TemporalBasisValues,
  TemporalDctRead,
  TemporalDctRepresentation
}
import scalafim.response.{
  AxisLocality,
  AxisReadEvidence,
  ByteRange,
  ChunkId,
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
  ReadCapabilities,
  ReadReceipt,
  ResolvedResponseSelection,
  SelectionAxes,
  SourceId
}
import zarr4s.{
  ArrayDescriptor,
  ArraySelection,
  ChunkCoordinate,
  ChunkPlanner,
  Coordinate,
  CoordinateBatch,
  PhysicalLayout,
  PrimitiveBlock,
  ReadLimits,
  ReadResult as ZarrReadResult,
  ShardPlanner,
  ZarrPath
}

final case class TemporalDctZarrObject(
    id: PhysicalObjectId,
    length: Long
)

final class TemporalDctZarrPayload private (
    val payload: ArchivePayloadId,
    val array: ArrayDescriptor,
    val path: ZarrPath,
    val objects: Vector[TemporalDctZarrObject],
    val locality: ArchivePhysicalLocality
):
  private val objectLengths: Map[String, Long] =
    objects.iterator.map(value => value.id.value -> value.length).toMap

  private[archive] def plan(
      selection: ArraySelection,
      axes: ArchiveSelectionAxes,
      limits: ReadLimits
  ): Either[ArchiveError, ZarrPayloadPlan] =
    for
      covering <- TemporalDctZarrPayload.coveringObjects(this, selection)
      plan <- locality match
        case ArchivePhysicalLocality.ChunkBounded =>
          ZarrPayloadPlan.chunkBounded(
            payload,
            selection,
            axes,
            covering.map(_.id),
            limits
          )
        case ArchivePhysicalLocality.ByteRangeBounded =>
          ZarrPayloadPlan.byteRangeBounded(
            payload,
            selection,
            axes,
            covering.map(value =>
              ObjectRange(
                value.id,
                ArchiveByteInterval.unsafe(0L, value.length)
              )
            ),
            limits
          )
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"typed temporal-DCT Zarr payload cannot claim $other"
          ))
    yield plan

  private[archive] def objectLength(
      id: PhysicalObjectId
  ): Option[Long] =
    objectLengths.get(id.value)

object TemporalDctZarrPayload:
  def make(
      payload: ArchivePayloadId,
      array: ArrayDescriptor,
      path: ZarrPath,
      objects: Vector[TemporalDctZarrObject],
      locality: ArchivePhysicalLocality
  ): Either[ArchiveError, TemporalDctZarrPayload] =
    if array.dataType.name != "float64" then
      Left(ArchiveError.InvalidArchive(
        s"temporal-DCT Zarr payload '${payload.value}' must store float64"
      ))
    else if objects.isEmpty then
      Left(ArchiveError.InvalidArchive(
        s"temporal-DCT Zarr payload '${payload.value}' has no physical objects"
      ))
    else if objects.exists(_.length <= 0L) then
      Left(ArchiveError.InvalidArchive(
        s"temporal-DCT Zarr payload '${payload.value}' has a non-positive object length"
      ))
    else if objects.map(_.id.value).distinct.length != objects.length then
      Left(ArchiveError.InvalidArchive(
        s"temporal-DCT Zarr payload '${payload.value}' has duplicate objects"
      ))
    else
      locality match
        case ArchivePhysicalLocality.ChunkBounded =>
          Right(new TemporalDctZarrPayload(
            payload,
            array,
            path,
            objects.sortBy(_.id.value),
            locality
          ))
        case ArchivePhysicalLocality.ByteRangeBounded =>
          array.layout match
            case PhysicalLayout.Sharded(_, _, _, _, _) =>
              Right(new TemporalDctZarrPayload(
                payload,
                array,
                path,
                objects.sortBy(_.id.value),
                locality
              ))
            case PhysicalLayout.Direct(_) =>
              Left(ArchiveError.InvalidArchive(
                s"direct temporal-DCT Zarr payload '${payload.value}' cannot " +
                  "promise byte-range reads"
              ))
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"temporal-DCT Zarr payload '${payload.value}' cannot claim $other"
          ))

  private def coveringObjects(
      payload: TemporalDctZarrPayload,
      selection: ArraySelection
  ): Either[ArchiveError, Vector[TemporalDctZarrObject]] =
    val coordinates =
      payload.array.layout match
        case PhysicalLayout.Direct(_) =>
          ChunkPlanner
            .plan(payload.array.grid, selection)
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
    coordinates.flatMap(resolveObjects(payload, _))

  private def resolveObjects(
      payload: TemporalDctZarrPayload,
      coordinates: Vector[ChunkCoordinate]
  ): Either[ArchiveError, Vector[TemporalDctZarrObject]] =
    val result = Vector.newBuilder[TemporalDctZarrObject]
    val seen = scala.collection.mutable.HashSet.empty[String]
    var index = 0
    while index < coordinates.length do
      val relative =
        payload.array.chunkKeyEncoding.encode(coordinates(index)).value
      val key =
        payload.path.key(relative) match
          case Left(error) =>
            return Left(ArchiveError.InvalidArchive(error.message))
          case Right(found) =>
            found
      val id = PhysicalObjectId.unsafe(key.value)
      payload.objectLength(id) match
        case None =>
          return Left(ArchiveError.InvalidArchive(
            s"temporal-DCT Zarr plan covers unpublished object '${id.value}'"
          ))
        case Some(length) if seen.add(id.value) =>
          result += TemporalDctZarrObject(id, length)
        case Some(_) =>
          ()
      index += 1
    val found = result.result()
    if found.isEmpty then
      Left(ArchiveError.InvalidArchive(
        s"temporal-DCT Zarr selection for '${payload.payload.value}' has no covering object"
      ))
    else Right(found)

final class TemporalDctZarrLayout private (
    val basis: TemporalDctZarrPayload,
    val loadings: TemporalDctZarrPayload,
    val offset: Option[TemporalDctZarrPayload]
):
  def payloads: Vector[TemporalDctZarrPayload] =
    Vector(basis, loadings) ++ offset.toVector

object TemporalDctZarrLayout:
  def make(
      model: TemporalDctRepresentation,
      basis: TemporalDctZarrPayload,
      loadings: TemporalDctZarrPayload,
      offset: Option[TemporalDctZarrPayload]
  ): Either[ArchiveError, TemporalDctZarrLayout] =
    for
      _ <- shape(
        basis,
        Vector(
          model.schema.time.count.toLong,
          model.spec.components.toLong
        ),
        "temporal basis"
      )
      _ <- shape(
        loadings,
        Vector(
          model.schema.samples.count.toLong,
          model.spec.components.toLong
        ),
        "spatial loadings"
      )
      _ <- (model.offsetSlot, offset) match
        case (Some(_), Some(found)) =>
          shape(
            found,
            Vector(model.schema.samples.count.toLong),
            "sample offset"
          )
        case (Some(_), None) =>
          Left(ArchiveError.InvalidArchive(
            "centered temporal DCT Zarr binding is missing sample offsets"
          ))
        case (None, Some(_)) =>
          Left(ArchiveError.InvalidArchive(
            "uncentered temporal DCT Zarr binding has unexpected sample offsets"
          ))
        case (None, None) =>
          Right(())
      payloadIds = (Vector(basis, loadings) ++ offset.toVector)
        .map(_.payload.value)
      _ <-
        if payloadIds.distinct.length == payloadIds.length then Right(())
        else Left(ArchiveError.InvalidArchive(
          "temporal-DCT Zarr payload ids must be unique"
        ))
      objectIds = (Vector(basis, loadings) ++ offset.toVector)
        .flatMap(_.objects)
        .map(_.id.value)
      _ <-
        if objectIds.distinct.length == objectIds.length then Right(())
        else Left(ArchiveError.InvalidArchive(
          "temporal-DCT Zarr physical objects must belong to one payload"
        ))
      _ <- offset match
        case Some(found) if found.locality != loadings.locality =>
          Left(ArchiveError.InvalidArchive(
            "loading and offset payloads must provide the same sample-axis locality"
          ))
        case _ =>
          Right(())
    yield new TemporalDctZarrLayout(basis, loadings, offset)

  private def shape(
      payload: TemporalDctZarrPayload,
      expected: Vector[Long],
      label: String
  ): Either[ArchiveError, Unit] =
    if payload.array.shape.toVector == expected then Right(())
    else Left(ArchiveError.ShapeMismatch(
      s"temporal-DCT Zarr $label expected ${expected.mkString("x")} but " +
        s"found ${payload.array.shape.toVector.mkString("x")}"
    ))

object TemporalDctZarrSource:
  def make[F[_]: Async](
      sourceId: SourceId,
      model: TemporalDctRepresentation,
      archive: ArchiveResponseAccess[F],
      zarrLayout: TemporalDctZarrLayout,
      limits: ReadLimits = ReadLimits(maxConcurrentRequests = 1)
  ): Either[ArchiveError, ArchivedTemporalDctSource[F]] =
    for
      binding <- TemporalDctZarrBinding.make(
        archive.payloads,
        zarrLayout,
        limits
      )
      source <- ArchivedTemporalDctSource.bound(
        sourceId,
        model,
        archive,
        binding
      )
    yield source

private final class TemporalDctZarrBinding[F[_]](
    executor: PayloadExecutor[F],
    zarrLayout: TemporalDctZarrLayout,
    val capabilities: ReadCapabilities,
    val layout: OpenedLayout,
    limits: ReadLimits
)(using F: Async[F]) extends TemporalDctArchiveBinding[F]:
  def interpreter(
      model: TemporalDctRepresentation
  ): DecodePlan.Interpreter[[A] =>> TemporalDctCollected[F, A]] =
    new TemporalDctZarrInterpreter(
      model,
      executor,
      zarrLayout,
      limits
    )

  def receipt(
      selection: ResolvedResponseSelection,
      native: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, ReadReceipt] =
    if native.isEmpty then
      Left(ArchiveError.ReceiptMismatch(
        "temporal-DCT Zarr execution produced no native receipts"
      ))
    else
      for
        time <- axisEvidence(
          ArchiveSelectionAxes.Time,
          SelectionAxes.Time,
          zarrLayout.basis.locality,
          native
        )
        samples <- axisEvidence(
          ArchiveSelectionAxes.Sample,
          SelectionAxes.Samples,
          zarrLayout.loadings.locality,
          native
        )
        physicalBytes <- sumBytes(native)
        cacheHits <- sumCacheHits(native)
      yield ReadReceipt(
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
          Vector(time, samples),
          PhysicalByteEvidence.Known(physicalBytes),
          cacheHits
        ),
        Vector(IntegrityEvidence.Verified(
          DomainReference.unsafe("zarr-codec", "crc32c")
        )),
        Vector.empty
      )

  private def axisEvidence(
      archiveAxes: ArchiveSelectionAxes,
      responseAxes: SelectionAxes,
      expected: ArchivePhysicalLocality,
      native: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, AxisReadEvidence] =
    val receipts =
      native.filter(_.plan.locality.axes == archiveAxes)
    if receipts.isEmpty then
      Left(ArchiveError.ReceiptMismatch(
        s"temporal-DCT Zarr receipt has no ${responseAxes.label} evidence"
      ))
    else if receipts.exists(_.observedLocality != expected) then
      Left(ArchiveError.ReceiptMismatch(
        s"temporal-DCT Zarr ${responseAxes.label} locality differs across receipts"
      ))
    else
      expected match
        case ArchivePhysicalLocality.ChunkBounded =>
          val touched =
            receipts.flatMap: receipt =>
              val payload = PayloadId.unsafe(receipt.plan.payload.value)
              receipt.observations
                .flatMap(observationObject)
                .distinct
                .map(objectId =>
                  PhysicalReadUnit.Chunk(
                    payload,
                    ChunkId.unsafe(objectId.value)
                  )
                )
          val covering =
            receipts.flatMap: receipt =>
              val payload = PayloadId.unsafe(receipt.plan.payload.value)
              receipt.plan.locality.coveringObjects.map(objectId =>
                PhysicalReadUnit.Chunk(
                  payload,
                  ChunkId.unsafe(objectId.value)
                )
              )
          Right(AxisReadEvidence(
            responseAxes,
            PhysicalLocality.ChunkBounded,
            touched,
            covering
          ))
        case ArchivePhysicalLocality.ByteRangeBounded =>
          for
            touched <- observationRanges(receipts.flatMap(_.observations))
            covering <- coveringRanges(
              receipts.flatMap(_.plan.locality.coveringRanges)
            )
          yield AxisReadEvidence(
            responseAxes,
            PhysicalLocality.ByteRangeBounded,
            touched,
            covering
          )
        case other =>
          Left(ArchiveError.ReceiptMismatch(
            s"temporal-DCT Zarr cannot adapt $other"
          ))

  private def observationRanges(
      values: Vector[ReadObservation]
  ): Either[ArchiveError, Vector[PhysicalReadUnit]] =
    val result = Vector.newBuilder[PhysicalReadUnit]
    var index = 0
    while index < values.length do
      values(index) match
        case ReadObservation.ByteRange(objectId, interval, _) =>
          range(objectId, interval) match
            case Left(error) =>
              return Left(error)
            case Right(found) =>
              result += found
        case other =>
          return Left(ArchiveError.ReceiptMismatch(
            s"byte-range temporal-DCT Zarr receipt contains $other"
          ))
      index += 1
    Right(result.result())

  private def coveringRanges(
      values: Vector[ObjectRange]
  ): Either[ArchiveError, Vector[PhysicalReadUnit]] =
    val result = Vector.newBuilder[PhysicalReadUnit]
    var index = 0
    while index < values.length do
      range(values(index).objectId, values(index).interval) match
        case Left(error) =>
          return Left(error)
        case Right(found) =>
          result += found
      index += 1
    Right(result.result())

  private def range(
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

  private def sumBytes(
      receipts: Vector[ArchiveReadReceipt]
  ): Either[ArchiveError, Long] =
    var total = 0L
    var index = 0
    while index < receipts.length do
      val value = receipts(index).physicalBytes
      if total > Long.MaxValue - value then
        return Left(ArchiveError.ReceiptMismatch(
          "temporal-DCT Zarr physical byte total overflows Long"
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
          "temporal-DCT Zarr cache-hit total overflows Int"
        ))
      total += value
      index += 1
    Right(total)

private object TemporalDctZarrBinding:
  def make[F[_]: Async](
      executor: PayloadExecutor[F],
      zarrLayout: TemporalDctZarrLayout,
      limits: ReadLimits
  ): Either[ArchiveError, TemporalDctZarrBinding[F]] =
    if limits.maxConcurrentRequests != 1 then
      Left(ArchiveError.InvalidArchive(
        "temporal-DCT Zarr receipt evidence requires maxConcurrentRequests = 1"
      ))
    else
      for
        timeLocality <- responseLocality(zarrLayout.basis.locality)
        sampleLocality <- responseLocality(zarrLayout.loadings.locality)
        capabilities <- ReadCapabilities
          .make(Vector(
            AxisLocality(
              SelectionAxes.Time,
              timeLocality
            ),
            AxisLocality(
              SelectionAxes.Samples,
              sampleLocality
            )
          ))
          .left
          .map(error => ArchiveError.InvalidArchive(error.message))
        layout <- responseLayout(zarrLayout)
      yield new TemporalDctZarrBinding(
        executor,
        zarrLayout,
        capabilities,
        layout,
        limits
      )

  private def responseLayout(
      zarrLayout: TemporalDctZarrLayout
  ): Either[ArchiveError, OpenedLayout] =
    val payloads =
      zarrLayout.payloads.map: value =>
        val payload = PayloadId.unsafe(value.payload.value)
        PayloadLayout(
          payload,
          value.objects.map(objectValue =>
            ObjectId.unsafe(objectValue.id.value)
          ),
          value.objects.map(objectValue =>
            ChunkId.unsafe(objectValue.id.value)
          )
        )
    OpenedLayout
      .make(payloads)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))

  private def responseLocality(
      locality: ArchivePhysicalLocality
  ): Either[ArchiveError, PhysicalLocality] =
    locality match
      case ArchivePhysicalLocality.ChunkBounded =>
        Right(PhysicalLocality.ChunkBounded)
      case ArchivePhysicalLocality.ByteRangeBounded =>
        Right(PhysicalLocality.ByteRangeBounded)
      case other =>
        Left(ArchiveError.InvalidArchive(
          s"typed temporal-DCT Zarr payload cannot claim $other"
        ))

private final class TemporalDctZarrInterpreter[F[_]](
    model: TemporalDctRepresentation,
    executor: PayloadExecutor[F],
    layout: TemporalDctZarrLayout,
    limits: ReadLimits
)(using F: Async[F])
    extends DecodePlan.Interpreter[
      [A] =>> TemporalDctCollected[F, A]
    ]:
  private type ArchiveF[A] =
    EitherT[F, ArchiveError, A]

  private type Collected[A] =
    TemporalDctCollected[F, A]

  def apply[A](
      request: LogicalPayloadRead[A]
  ): Collected[A] =
    request match
      case TemporalDctRead.BasisRows(slot, timepoints) =>
        if slot != model.basisSlot then
          failure(s"unexpected temporal basis slot '${slot.id.value}'")
        else
          readMatrix(
            layout.basis,
            timepoints.values,
            ArchiveSelectionAxes.Time
          ).flatMap: values =>
            lift(
              TemporalBasisValues
                .copyFromRowMajor(
                  timepoints.size,
                  model.spec.components,
                  values
                )
                .left
                .map(error => ArchiveError.InvalidArchive(error.message))
            )
      case TemporalDctRead.LoadingRows(slot, samples) =>
        if slot != model.loadingSlot then
          failure(s"unexpected spatial loading slot '${slot.id.value}'")
        else
          readMatrix(
            layout.loadings,
            samples.values,
            ArchiveSelectionAxes.Sample
          ).flatMap: values =>
            lift(
              SpatialLoadingValues
                .copyFromRowMajor(
                  samples.size,
                  model.spec.components,
                  values
                )
                .left
                .map(error => ArchiveError.InvalidArchive(error.message))
            )
      case TemporalDctRead.OffsetEntries(slot, samples) =>
        (model.offsetSlot, layout.offset) match
          case (Some(expected), Some(payload)) if slot == expected =>
            readVector(
              payload,
              samples.values,
              ArchiveSelectionAxes.Sample
            ).flatMap: values =>
              lift(
                SampleOffsetValues
                  .copyFrom(values)
                  .left
                  .map(error => ArchiveError.InvalidArchive(error.message))
              )
          case _ =>
            failure(s"unexpected sample offset slot '${slot.id.value}'")
      case other =>
        failure(s"unsupported logical Zarr read '${other.slot.id.value}'")

  private def readMatrix(
      payload: TemporalDctZarrPayload,
      rows: Vector[Int],
      axes: ArchiveSelectionAxes
  ): Collected[Array[Double]] =
    val points =
      matrixPoints(payload.array, rows) match
        case Left(error) =>
          return failure(error.message)
        case Right(found) =>
          found
    collect(
      payload
        .plan(ArraySelection.PointSelection(points), axes, limits)
        .fold(
          error => EitherT.leftT[F, Observed[ZarrReadResult]](error),
          executor.execute
        )
    )(doubleValues)

  private def readVector(
      payload: TemporalDctZarrPayload,
      indices: Vector[Int],
      axes: ArchiveSelectionAxes
  ): Collected[Array[Double]] =
    val points =
      vectorPoints(payload.array, indices) match
        case Left(error) =>
          return failure(error.message)
        case Right(found) =>
          found
    collect(
      payload
        .plan(ArraySelection.PointSelection(points), axes, limits)
        .fold(
          error => EitherT.leftT[F, Observed[ZarrReadResult]](error),
          executor.execute
        )
    )(doubleValues)

  private def collect[A](
      execution: EitherT[F, ArchiveError, Observed[ZarrReadResult]]
  )(
      convert: ZarrReadResult => Either[ArchiveError, A]
  ): Collected[A] =
    WriterT:
      execution.flatMap: observed =>
        EitherT
          .fromEither[F](convert(observed.value))
          .map(value => Vector(observed.receipt) -> value)

  private def lift[A](
      value: Either[ArchiveError, A]
  ): Collected[A] =
    WriterT.liftF(EitherT.fromEither[F](value))

  private def failure[A](
      detail: String
  ): Collected[A] =
    WriterT.liftF(EitherT.leftT(ArchiveError.InvalidArchive(detail)))

  private def matrixPoints(
      array: ArrayDescriptor,
      rows: Vector[Int]
  ): Either[ArchiveError, CoordinateBatch] =
    if array.shape.rank.toInt != 2 then
      Left(ArchiveError.ShapeMismatch(
        s"temporal-DCT matrix payload has rank ${array.shape.rank.toInt}"
      ))
    else
      val columns = array.shape.axis(1)
      if columns > Int.MaxValue.toLong ||
          rows.length.toLong * columns > Int.MaxValue.toLong
      then
        Left(ArchiveError.ShapeMismatch(
          "temporal-DCT matrix selection exceeds Int"
        ))
      else
        val coordinates = Vector.newBuilder[Coordinate]
        coordinates.sizeHint(rows.length * columns.toInt)
        var row = 0
        while row < rows.length do
          var column = 0
          while column < columns.toInt do
            Coordinate(rows(row).toLong, column.toLong) match
              case Left(error) =>
                return Left(ArchiveError.InvalidArchive(error.message))
              case Right(found) =>
                coordinates += found
            column += 1
          row += 1
        CoordinateBatch
          .within(array.shape, coordinates.result())
          .left
          .map(error => ArchiveError.InvalidArchive(error.message))

  private def vectorPoints(
      array: ArrayDescriptor,
      indices: Vector[Int]
  ): Either[ArchiveError, CoordinateBatch] =
    if array.shape.rank.toInt != 1 then
      Left(ArchiveError.ShapeMismatch(
        s"temporal-DCT vector payload has rank ${array.shape.rank.toInt}"
      ))
    else
      val coordinates = Vector.newBuilder[Coordinate]
      coordinates.sizeHint(indices.length)
      var index = 0
      while index < indices.length do
        Coordinate(indices(index).toLong) match
          case Left(error) =>
            return Left(ArchiveError.InvalidArchive(error.message))
          case Right(found) =>
            coordinates += found
        index += 1
      CoordinateBatch
        .within(array.shape, coordinates.result())
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))

  private def doubleValues(
      read: ZarrReadResult
  ): Either[ArchiveError, Array[Double]] =
    read.block match
      case PrimitiveBlock.Float64(values) =>
        Right(values.toArray)
      case other =>
        Left(ArchiveError.InvalidArchive(
          s"temporal-DCT Zarr executor returned $other instead of float64"
        ))
