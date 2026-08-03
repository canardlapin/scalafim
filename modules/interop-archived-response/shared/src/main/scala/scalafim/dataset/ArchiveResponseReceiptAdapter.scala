package scalafim.dataset

import scalafim.archive.{
  ArchiveReadReceipt,
  ByteInterval as ArchiveByteInterval,
  LocalityClaim,
  PayloadId as ArchivePayloadId,
  PhysicalLocality as ArchivePhysicalLocality,
  PhysicalObjectId,
  ReadObservation as ArchiveReadObservation,
  SelectionAxes as ArchiveSelectionAxes
}
import scalafim.response.{
  AxisLocality,
  AxisReadEvidence,
  ByteRange as ResponseByteRange,
  ChunkId,
  IdentityError,
  IntegrityEvidence,
  LogicalReadSummary,
  ObjectId,
  OpenedLayout,
  PayloadId as ResponsePayloadId,
  PayloadLayout,
  PhysicalByteEvidence,
  PhysicalLocality as ResponsePhysicalLocality,
  PhysicalReadSummary,
  PhysicalReadUnit,
  ReadCapabilities,
  ReadReceipt,
  ReceiptConformance,
  ReceiptConformanceError,
  ResolvedResponseSelection,
  SelectionAxes as ResponseSelectionAxes
}

enum ArchiveResponseIdentityKind(val label: String):
  case Payload extends ArchiveResponseIdentityKind("payload id")
  case Object extends ArchiveResponseIdentityKind("object id")
  case ChunkCover extends ArchiveResponseIdentityKind("chunk-cover id")

enum ArchiveResponseReceiptError:
  case EmptyReceipts
  case UnsupportedAxes(values: Vector[String])
  case AxisCoverageMismatch(
      expected: ResponseSelectionAxes,
      actual: Vector[ResponseSelectionAxes]
  )
  case InvalidIdentity(
      kind: ArchiveResponseIdentityKind,
      value: String,
      error: IdentityError
  )
  case ObservationMismatch(
      expected: ArchivePhysicalLocality,
      observation: ArchiveReadObservation
  )
  case LogicalByteCountOverflow(rows: Int, columns: Int)
  case PhysicalByteCountOverflow
  case CacheHitCountOverflow
  case ResponseContract(error: ReceiptConformanceError)

  def message: String =
    this match
      case EmptyReceipts =>
        "archive receipt adaptation requires at least one native receipt"
      case UnsupportedAxes(values) =>
        s"archive axes ${values.mkString("[", ",", "]")} have no response-axis mapping"
      case AxisCoverageMismatch(expected, actual) =>
        s"archive receipt axes ${actual.map(_.label).mkString("[", ",", "]")} " +
          s"do not cover the response selection '${expected.label}'"
      case InvalidIdentity(kind, value, error) =>
        s"archive ${kind.label} '$value' is not a valid response identity: ${error.message}"
      case ObservationMismatch(expected, observation) =>
        s"archive observation $observation cannot represent declared locality $expected"
      case LogicalByteCountOverflow(rows, columns) =>
        s"logical response byte count overflows Long for shape ${rows}x$columns"
      case PhysicalByteCountOverflow =>
        "combined archive physical byte count overflows Long"
      case CacheHitCountOverflow =>
        "combined archive cache-hit count overflows Int"
      case ResponseContract(error) =>
        error.message

final case class AdaptedArchiveReceipt(
    capabilities: ReadCapabilities,
    layout: OpenedLayout,
    response: ReadReceipt,
    native: Vector[ArchiveReadReceipt]
)

object ArchiveResponseReceiptAdapter:
  def adapt(
      selection: ResolvedResponseSelection,
      receipt: ArchiveReadReceipt
  ): Either[ArchiveResponseReceiptError, AdaptedArchiveReceipt] =
    adapt(selection, Vector(receipt))

  def adapt(
      selection: ResolvedResponseSelection,
      receipts: Vector[ArchiveReadReceipt]
  ): Either[ArchiveResponseReceiptError, AdaptedArchiveReceipt] =
    if receipts.isEmpty then Left(ArchiveResponseReceiptError.EmptyReceipts)
    else
      for
        logicalBytes <- logicalByteCount(selection)
        fragments <- mapEither(receipts)(adaptFragment)
        capabilities <- ReadCapabilities
          .make(fragments.map(fragment =>
            AxisLocality(fragment.axes, fragment.locality)
          ))
          .left
          .map(ArchiveResponseReceiptError.ResponseContract.apply)
        _ <- checkCoverage(selection.selectedAxes, fragments.map(_.axes))
        layout <- openedLayout(fragments)
        physicalBytes <- sumPhysicalBytes(receipts)
        cacheHits <- sumCacheHits(receipts)
        common = ReadReceipt(
          LogicalReadSummary(
            selection.schema,
            selection.selectedAxes,
            selection.timepoints.values,
            selection.samples.values,
            logicalBytes
          ),
          PhysicalReadSummary(
            fragments.map(_.evidence),
            PhysicalByteEvidence.Known(physicalBytes),
            cacheHits
          ),
          Vector(IntegrityEvidence.NotChecked),
          Vector.empty
        )
        _ <- ReceiptConformance
          .check(capabilities, common, layout)
          .left
          .map(ArchiveResponseReceiptError.ResponseContract.apply)
      yield AdaptedArchiveReceipt(capabilities, layout, common, receipts)

  private final case class AdaptedFragment(
      axes: ResponseSelectionAxes,
      locality: ResponsePhysicalLocality,
      evidence: AxisReadEvidence,
      layout: PayloadLayout
  )

  private def adaptFragment(
      receipt: ArchiveReadReceipt
  ): Either[ArchiveResponseReceiptError, AdaptedFragment] =
    val claim = receipt.plan.locality
    for
      axes <- responseAxes(claim.axes)
      locality = responseLocality(receipt.observedLocality)
      payload <- responsePayload(receipt.plan.payload)
      touched <- touchedUnits(
        receipt.observedLocality,
        payload,
        receipt.observations
      )
      covering <- coveringUnits(claim, payload)
      layout <- payloadLayout(claim, payload)
    yield AdaptedFragment(
      axes,
      locality,
      AxisReadEvidence(axes, locality, touched, covering),
      layout
    )

  private def responseAxes(
      axes: ArchiveSelectionAxes
  ): Either[ArchiveResponseReceiptError, ResponseSelectionAxes] =
    if axes == ArchiveSelectionAxes.Time then Right(ResponseSelectionAxes.Time)
    else if axes == ArchiveSelectionAxes.Sample then
      Right(ResponseSelectionAxes.Samples)
    else if axes == ArchiveSelectionAxes.TimeAndSample then
      Right(ResponseSelectionAxes.TimeAndSamples)
    else
      Left(ArchiveResponseReceiptError.UnsupportedAxes(
        axes.values.map(_.value)
      ))

  private def responseLocality(
      locality: ArchivePhysicalLocality
  ): ResponsePhysicalLocality =
    locality match
      case ArchivePhysicalLocality.Resident =>
        ResponsePhysicalLocality.Resident
      case ArchivePhysicalLocality.WholePayload =>
        ResponsePhysicalLocality.WholePayload
      case ArchivePhysicalLocality.WholeObject =>
        ResponsePhysicalLocality.WholeObject
      case ArchivePhysicalLocality.ChunkBounded =>
        ResponsePhysicalLocality.ChunkBounded
      case ArchivePhysicalLocality.ByteRangeBounded =>
        ResponsePhysicalLocality.ByteRangeBounded

  private def touchedUnits(
      locality: ArchivePhysicalLocality,
      payload: ResponsePayloadId,
      observations: Vector[ArchiveReadObservation]
  ): Either[ArchiveResponseReceiptError, Vector[PhysicalReadUnit]] =
    locality match
      case ArchivePhysicalLocality.Resident =>
        mapEither(observations):
          case ArchiveReadObservation.Resident(_) =>
            Right(Option.empty[PhysicalReadUnit])
          case observation =>
            observationMismatch(locality, observation)
        .map(_.flatten)
      case ArchivePhysicalLocality.WholePayload =>
        mapEither(observations):
          case ArchiveReadObservation.WholePayload(_, _) =>
            Right(Some(PhysicalReadUnit.Payload(payload)))
          case observation =>
            observationMismatch(locality, observation)
        .map(_.flatten)
      case ArchivePhysicalLocality.WholeObject =>
        mapEither(observations):
          case ArchiveReadObservation.WholeObject(objectId, _) =>
            responseObject(objectId).map(value =>
              Some(PhysicalReadUnit.Object(value))
            )
          case ArchiveReadObservation.ObjectLength(objectId, _) =>
            responseObject(objectId).map(value =>
              Some(PhysicalReadUnit.Object(value))
            )
          case observation =>
            observationMismatch(locality, observation)
        .map(_.flatten)
      case ArchivePhysicalLocality.ChunkBounded =>
        mapEither(observations):
          case ArchiveReadObservation.WholeObject(objectId, _) =>
            responseChunk(objectId).map(value =>
              Some(PhysicalReadUnit.Chunk(payload, value))
            )
          case ArchiveReadObservation.ByteRange(objectId, _, _) =>
            responseChunk(objectId).map(value =>
              Some(PhysicalReadUnit.Chunk(payload, value))
            )
          case ArchiveReadObservation.ObjectLength(objectId, _) =>
            responseChunk(objectId).map(value =>
              Some(PhysicalReadUnit.Chunk(payload, value))
            )
          case observation =>
            observationMismatch(locality, observation)
        .map(_.flatten)
      case ArchivePhysicalLocality.ByteRangeBounded =>
        mapEither(observations):
          case ArchiveReadObservation.ByteRange(objectId, interval, _) =>
            responseRange(objectId, interval).map(value =>
              Some(value)
            )
          case observation =>
            observationMismatch(locality, observation)
        .map(_.flatten)

  private def coveringUnits(
      claim: LocalityClaim,
      payload: ResponsePayloadId
  ): Either[ArchiveResponseReceiptError, Vector[PhysicalReadUnit]] =
    claim.locality match
      case ArchivePhysicalLocality.Resident =>
        Right(Vector.empty)
      case ArchivePhysicalLocality.WholePayload =>
        Right(Vector(PhysicalReadUnit.Payload(payload)))
      case ArchivePhysicalLocality.WholeObject =>
        mapEither(claim.coveringObjects)(responseObject)
          .map(_.map(PhysicalReadUnit.Object.apply))
      case ArchivePhysicalLocality.ChunkBounded =>
        mapEither(claim.coveringObjects)(responseChunk)
          .map(_.map(chunk => PhysicalReadUnit.Chunk(payload, chunk)))
      case ArchivePhysicalLocality.ByteRangeBounded =>
        mapEither(claim.coveringRanges)(range =>
          responseRange(range.objectId, range.interval)
        )

  private def payloadLayout(
      claim: LocalityClaim,
      payload: ResponsePayloadId
  ): Either[ArchiveResponseReceiptError, PayloadLayout] =
    for
      objects <- mapEither(claim.coveringObjects)(responseObject)
      chunks <-
        if claim.locality == ArchivePhysicalLocality.ChunkBounded then
          mapEither(claim.coveringObjects)(responseChunk)
        else Right(Vector.empty)
    yield PayloadLayout(payload, objects, chunks)

  private def openedLayout(
      fragments: Vector[AdaptedFragment]
  ): Either[ArchiveResponseReceiptError, OpenedLayout] =
    var merged = Vector.empty[PayloadLayout]
    var index = 0
    while index < fragments.length do
      val incoming = fragments(index).layout
      val found = merged.indexWhere(_.id == incoming.id)
      if found < 0 then merged = merged :+ incoming
      else
        val current = merged(found)
        merged = merged.updated(
          found,
          PayloadLayout(
            current.id,
            (current.objects ++ incoming.objects).distinct,
            (current.chunks ++ incoming.chunks).distinct
          )
        )
      index += 1
    OpenedLayout
      .make(merged)
      .left
      .map(ArchiveResponseReceiptError.ResponseContract.apply)

  private def checkCoverage(
      expected: ResponseSelectionAxes,
      actual: Vector[ResponseSelectionAxes]
  ): Either[ArchiveResponseReceiptError, Unit] =
    val valid =
      expected match
        case ResponseSelectionAxes.Time =>
          actual == Vector(ResponseSelectionAxes.Time)
        case ResponseSelectionAxes.Samples =>
          actual == Vector(ResponseSelectionAxes.Samples)
        case ResponseSelectionAxes.TimeAndSamples =>
          actual == Vector(ResponseSelectionAxes.TimeAndSamples) ||
            actual.toSet == Set(
              ResponseSelectionAxes.Time,
              ResponseSelectionAxes.Samples
            ) && actual.length == 2
    if valid then Right(())
    else Left(ArchiveResponseReceiptError.AxisCoverageMismatch(expected, actual))

  private def logicalByteCount(
      selection: ResolvedResponseSelection
  ): Either[ArchiveResponseReceiptError, Long] =
    val cells = selection.rows.toLong * selection.columns.toLong
    if cells > Long.MaxValue / java.lang.Double.BYTES.toLong then
      Left(ArchiveResponseReceiptError.LogicalByteCountOverflow(
        selection.rows,
        selection.columns
      ))
    else Right(cells * java.lang.Double.BYTES.toLong)

  private def sumPhysicalBytes(
      receipts: Vector[ArchiveReadReceipt]
  ): Either[ArchiveResponseReceiptError, Long] =
    var total = 0L
    var index = 0
    while index < receipts.length do
      val bytes = receipts(index).physicalBytes
      if total > Long.MaxValue - bytes then
        return Left(ArchiveResponseReceiptError.PhysicalByteCountOverflow)
      total += bytes
      index += 1
    Right(total)

  private def sumCacheHits(
      receipts: Vector[ArchiveReadReceipt]
  ): Either[ArchiveResponseReceiptError, Int] =
    var total = 0
    var index = 0
    while index < receipts.length do
      val hits = receipts(index).cacheHits
      if total > Int.MaxValue - hits then
        return Left(ArchiveResponseReceiptError.CacheHitCountOverflow)
      total += hits
      index += 1
    Right(total)

  private def responsePayload(
      payload: ArchivePayloadId
  ): Either[ArchiveResponseReceiptError, ResponsePayloadId] =
    ResponsePayloadId
      .fromString(payload.value)
      .left
      .map(error =>
        ArchiveResponseReceiptError.InvalidIdentity(
          ArchiveResponseIdentityKind.Payload,
          payload.value,
          error
        )
      )

  private def responseObject(
      objectId: PhysicalObjectId
  ): Either[ArchiveResponseReceiptError, ObjectId] =
    ObjectId
      .fromString(objectId.value)
      .left
      .map(error =>
        ArchiveResponseReceiptError.InvalidIdentity(
          ArchiveResponseIdentityKind.Object,
          objectId.value,
          error
        )
      )

  private def responseChunk(
      objectId: PhysicalObjectId
  ): Either[ArchiveResponseReceiptError, ChunkId] =
    ChunkId
      .fromString(objectId.value)
      .left
      .map(error =>
        ArchiveResponseReceiptError.InvalidIdentity(
          ArchiveResponseIdentityKind.ChunkCover,
          objectId.value,
          error
        )
      )

  private def responseRange(
      objectId: PhysicalObjectId,
      interval: ArchiveByteInterval
  ): Either[ArchiveResponseReceiptError, PhysicalReadUnit] =
    for
      responseObjectId <- responseObject(objectId)
      bytes <- ResponseByteRange
        .make(interval.offset, interval.length)
        .left
        .map(ArchiveResponseReceiptError.ResponseContract.apply)
    yield PhysicalReadUnit.Range(responseObjectId, bytes)

  private def observationMismatch[A](
      locality: ArchivePhysicalLocality,
      observation: ArchiveReadObservation
  ): Either[ArchiveResponseReceiptError, Option[A]] =
    Left(ArchiveResponseReceiptError.ObservationMismatch(
      locality,
      observation
    ))

  private def mapEither[A, B](
      values: Vector[A]
  )(
      function: A => Either[ArchiveResponseReceiptError, B]
  ): Either[ArchiveResponseReceiptError, Vector[B]] =
    val result = Vector.newBuilder[B]
    result.sizeHint(values.length)
    var index = 0
    while index < values.length do
      function(values(index)) match
        case Left(error) =>
          return Left(error)
        case Right(value) =>
          result += value
      index += 1
    Right(result.result())
