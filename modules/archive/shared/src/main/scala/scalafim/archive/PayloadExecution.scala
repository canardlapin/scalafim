package scalafim.archive

import cats.data.EitherT
import cats.effect.Resource

opaque type ArchiveLocation = String

object ArchiveLocation:
  def fromString(value: String): Either[ArchiveError, ArchiveLocation] =
    val normalized = value.trim
    if normalized.isEmpty then Left(ArchiveError.InvalidArchive("archive location must be non-empty"))
    else Right(normalized)

  def unsafe(value: String): ArchiveLocation =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (location: ArchiveLocation)
    inline def value: String = location

opaque type SelectionAxis = String

object SelectionAxis:
  val Time: SelectionAxis = "time"
  val Sample: SelectionAxis = "sample"

  def fromString(value: String): Either[ArchiveError, SelectionAxis] =
    ArchiveIdentity.checked(value, "selection axis")

  def unsafe(value: String): SelectionAxis =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (axis: SelectionAxis)
    inline def value: String = axis

final class SelectionAxes private (
    val values: Vector[SelectionAxis]
):
  def contains(axis: SelectionAxis): Boolean = values.contains(axis)

  override def equals(other: Any): Boolean =
    other match
      case that: SelectionAxes => values == that.values
      case _ => false

  override def hashCode(): Int = values.hashCode()

object SelectionAxes:
  val Time: SelectionAxes = new SelectionAxes(Vector(SelectionAxis.Time))
  val Sample: SelectionAxes = new SelectionAxes(Vector(SelectionAxis.Sample))
  val TimeAndSample: SelectionAxes =
    new SelectionAxes(Vector(SelectionAxis.Sample, SelectionAxis.Time))

  def from(values: Iterable[SelectionAxis]): Either[ArchiveError, SelectionAxes] =
    val normalized = values.toVector.distinct.sortBy(_.value)
    if normalized.isEmpty then
      Left(ArchiveError.InvalidArchive("payload plan must name at least one selection axis"))
    else Right(new SelectionAxes(normalized))

opaque type PhysicalObjectId = String

object PhysicalObjectId:
  def fromString(value: String): Either[ArchiveError, PhysicalObjectId] =
    if value.isEmpty then Left(ArchiveError.InvalidArchive("physical object id must be non-empty"))
    else if value.exists(_.isControl) then
      Left(ArchiveError.InvalidArchive("physical object id must not contain control characters"))
    else Right(value)

  def unsafe(value: String): PhysicalObjectId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: PhysicalObjectId)
    inline def value: String = id

final class ByteInterval private (
    val offset: Long,
    val length: Long
):
  val endExclusive: Long = offset + length

  def contains(other: ByteInterval): Boolean =
    other.offset >= offset && other.endExclusive <= endExclusive

  override def equals(other: Any): Boolean =
    other match
      case that: ByteInterval => offset == that.offset && length == that.length
      case _ => false

  override def hashCode(): Int =
    31 * java.lang.Long.hashCode(offset) + java.lang.Long.hashCode(length)

object ByteInterval:
  def from(offset: Long, length: Long): Either[ArchiveError, ByteInterval] =
    if offset < 0L then Left(ArchiveError.InvalidArchive("byte interval offset must be non-negative"))
    else if length <= 0L then Left(ArchiveError.InvalidArchive("byte interval length must be positive"))
    else if offset > Long.MaxValue - length then
      Left(ArchiveError.InvalidArchive("byte interval end overflows Long"))
    else Right(new ByteInterval(offset, length))

  def unsafe(offset: Long, length: Long): ByteInterval =
    from(offset, length).fold(error => throw new IllegalArgumentException(error.message), identity)

enum PhysicalLocality:
  case Resident
  case WholePayload
  case WholeObject
  case ChunkBounded
  case ByteRangeBounded

final case class ObjectRange(
    objectId: PhysicalObjectId,
    interval: ByteInterval
)

final class LocalityClaim private (
    val axes: SelectionAxes,
    val locality: PhysicalLocality,
    val coveringObjects: Vector[PhysicalObjectId],
    val coveringRanges: Vector[ObjectRange]
):
  override def equals(other: Any): Boolean =
    other match
      case that: LocalityClaim =>
        axes == that.axes &&
          locality == that.locality &&
          coveringObjects == that.coveringObjects &&
          coveringRanges == that.coveringRanges
      case _ => false

  override def hashCode(): Int =
    var result = axes.hashCode()
    result = 31 * result + locality.hashCode()
    result = 31 * result + coveringObjects.hashCode()
    31 * result + coveringRanges.hashCode()

object LocalityClaim:
  def resident(axes: SelectionAxes): LocalityClaim =
    new LocalityClaim(axes, PhysicalLocality.Resident, Vector.empty, Vector.empty)

  def wholePayload(axes: SelectionAxes): LocalityClaim =
    new LocalityClaim(axes, PhysicalLocality.WholePayload, Vector.empty, Vector.empty)

  def wholeObjects(
      axes: SelectionAxes,
      objects: Iterable[PhysicalObjectId]
  ): Either[ArchiveError, LocalityClaim] =
    checkedObjects(axes, PhysicalLocality.WholeObject, objects)

  def chunkBounded(
      axes: SelectionAxes,
      objects: Iterable[PhysicalObjectId]
  ): Either[ArchiveError, LocalityClaim] =
    checkedObjects(axes, PhysicalLocality.ChunkBounded, objects)

  def byteRangeBounded(
      axes: SelectionAxes,
      ranges: Iterable[ObjectRange]
  ): Either[ArchiveError, LocalityClaim] =
    val copied = ranges.toVector
    if copied.isEmpty then
      Left(ArchiveError.InvalidArchive("byte-range locality requires at least one range"))
    else if copied.distinct.length != copied.length then
      Left(ArchiveError.InvalidArchive("byte-range locality contains duplicate ranges"))
    else Right(new LocalityClaim(
      axes,
      PhysicalLocality.ByteRangeBounded,
      copied.map(_.objectId).distinct.sortBy(_.value),
      copied.sortBy(value => (value.objectId.value, value.interval.offset))
    ))

  private def checkedObjects(
      axes: SelectionAxes,
      locality: PhysicalLocality,
      objects: Iterable[PhysicalObjectId]
  ): Either[ArchiveError, LocalityClaim] =
    val copied = objects.toVector.distinct.sortBy(_.value)
    if copied.isEmpty then
      Left(ArchiveError.InvalidArchive(s"$locality locality requires at least one object"))
    else Right(new LocalityClaim(axes, locality, copied, Vector.empty))

final case class PayloadPlanSummary(
    payload: PayloadId,
    operation: String,
    locality: LocalityClaim
):
  require(operation.trim.nonEmpty, "payload plan operation must be non-empty")

trait PayloadPlan[A]:
  def summary: PayloadPlanSummary

sealed trait ReadObservation:
  def physicalBytes: Long

object ReadObservation:
  final case class Resident private[archive] (
      payload: PayloadId
  ) extends ReadObservation:
    val physicalBytes: Long = 0L

  final case class WholePayload private[archive] (
      payload: PayloadId,
      physicalBytes: Long
  ) extends ReadObservation

  final case class WholeObject private[archive] (
      objectId: PhysicalObjectId,
      physicalBytes: Long
  ) extends ReadObservation

  final case class ByteRange private[archive] (
      objectId: PhysicalObjectId,
      requested: ByteInterval,
      physicalBytes: Long
  ) extends ReadObservation

  final case class ObjectLength private[archive] (
      objectId: PhysicalObjectId,
      length: Long
  ) extends ReadObservation:
    val physicalBytes: Long = 0L

  def resident(payload: PayloadId): ReadObservation =
    Resident(payload)

  def wholePayload(
      payload: PayloadId,
      bytes: Long
  ): Either[ArchiveError, ReadObservation] =
    nonNegativeBytes(bytes).map(WholePayload(payload, _))

  def wholeObject(
      objectId: PhysicalObjectId,
      bytes: Long
  ): Either[ArchiveError, ReadObservation] =
    nonNegativeBytes(bytes).map(WholeObject(objectId, _))

  def byteRange(
      objectId: PhysicalObjectId,
      requested: ByteInterval,
      bytes: Long
  ): Either[ArchiveError, ReadObservation] =
    nonNegativeBytes(bytes).flatMap: checked =>
      if checked > requested.length then
        Left(ArchiveError.InvalidArchive(
          s"range read returned $checked bytes for requested length ${requested.length}"
        ))
      else Right(ByteRange(objectId, requested, checked))

  def objectLength(
      objectId: PhysicalObjectId,
      length: Long
  ): Either[ArchiveError, ReadObservation] =
    if length < 0L then Left(ArchiveError.InvalidArchive("object length must be non-negative"))
    else Right(ObjectLength(objectId, length))

  private def nonNegativeBytes(bytes: Long): Either[ArchiveError, Long] =
    if bytes < 0L then Left(ArchiveError.InvalidArchive("physical bytes must be non-negative"))
    else Right(bytes)

final class ReceiptAccumulator private (
    val observations: Vector[ReadObservation]
):
  def append(observation: ReadObservation): ReceiptAccumulator =
    new ReceiptAccumulator(observations :+ observation)

  def appendAll(values: Iterable[ReadObservation]): ReceiptAccumulator =
    new ReceiptAccumulator(observations ++ values)

  def combine(other: ReceiptAccumulator): ReceiptAccumulator =
    new ReceiptAccumulator(observations ++ other.observations)

object ReceiptAccumulator:
  val Empty: ReceiptAccumulator = new ReceiptAccumulator(Vector.empty)

  def from(observations: Iterable[ReadObservation]): ReceiptAccumulator =
    new ReceiptAccumulator(observations.toVector)

final class ArchiveReadReceipt private[archive] (
    val plan: PayloadPlanSummary,
    val observedLocality: PhysicalLocality,
    val observations: Vector[ReadObservation],
    val cacheHits: Int,
    val physicalBytes: Long
)

object ArchiveReadReceipt:
  def from(
      plan: PayloadPlanSummary,
      observedLocality: PhysicalLocality,
      accumulator: ReceiptAccumulator,
      cacheHits: Int = 0
  ): Either[ArchiveError, ArchiveReadReceipt] =
    if cacheHits < 0 then
      Left(ArchiveError.ReceiptMismatch("cache hit count must be non-negative"))
    else
      physicalBytes(accumulator.observations).flatMap: bytes =>
        val receipt = new ArchiveReadReceipt(
          plan,
          observedLocality,
          accumulator.observations,
          cacheHits,
          bytes
        )
        ReceiptConformance.check(receipt).map(_ => receipt)

  private def physicalBytes(
      observations: Vector[ReadObservation]
  ): Either[ArchiveError, Long] =
    var total = 0L
    var index = 0
    while index < observations.length do
      val bytes = observations(index).physicalBytes
      if total > Long.MaxValue - bytes then
        return Left(ArchiveError.ReceiptMismatch("physical byte total overflows Long"))
      total += bytes
      index += 1
    Right(total)

object ReceiptConformance:
  def check(receipt: ArchiveReadReceipt): Either[ArchiveError, Unit] =
    val claim = receipt.plan.locality
    if receipt.observedLocality != claim.locality then
      Left(ArchiveError.ReceiptMismatch(
        s"declared ${claim.locality} but observed ${receipt.observedLocality}"
      ))
    else if receipt.observations.isEmpty &&
        claim.locality != PhysicalLocality.Resident
    then
      Left(ArchiveError.ReceiptMismatch(
        s"${claim.locality} receipt must contain observed physical evidence"
      ))
    else
      val invalid = receipt.observations.collectFirst:
        case observation if !conforms(observation, receipt.plan.payload, claim) =>
          observation
      invalid match
        case Some(observation) =>
          Left(ArchiveError.ReceiptMismatch(
            s"$observation does not conform to ${claim.locality} for axes " +
              claim.axes.values.map(_.value).mkString("[", ",", "]")
          ))
        case None =>
          Right(())

  private def conforms(
      observation: ReadObservation,
      payload: PayloadId,
      claim: LocalityClaim
  ): Boolean =
    claim.locality match
      case PhysicalLocality.Resident =>
        observation match
          case ReadObservation.Resident(found) => found == payload
          case _ => false
      case PhysicalLocality.WholePayload =>
        observation match
          case ReadObservation.WholePayload(found, _) => found == payload
          case _ => false
      case PhysicalLocality.WholeObject =>
        observation match
          case ReadObservation.WholeObject(objectId, _) =>
            claim.coveringObjects.contains(objectId)
          case ReadObservation.ObjectLength(objectId, _) =>
            claim.coveringObjects.contains(objectId)
          case _ => false
      case PhysicalLocality.ChunkBounded =>
        observationObject(observation).exists(claim.coveringObjects.contains)
      case PhysicalLocality.ByteRangeBounded =>
        observation match
          case ReadObservation.ByteRange(objectId, requested, _) =>
            claim.coveringRanges.exists: covering =>
              covering.objectId == objectId && covering.interval.contains(requested)
          case _ => false

  private def observationObject(
      observation: ReadObservation
  ): Option[PhysicalObjectId] =
    observation match
      case ReadObservation.WholeObject(objectId, _) => Some(objectId)
      case ReadObservation.ByteRange(objectId, _, _) => Some(objectId)
      case ReadObservation.ObjectLength(objectId, _) => Some(objectId)
      case _ => None

final case class Observed[A](
    value: A,
    receipt: ArchiveReadReceipt
)

enum ArchiveValidationScope:
  case Structure
  case Contents

final case class ArchiveValidation(
    scope: ArchiveValidationScope,
    checkedPayloads: Vector[PayloadId],
    verifiedIntegrity: Vector[PayloadId]
)

trait PayloadExecutor[F[_]]:
  def execute[A](
      plan: PayloadPlan[A]
  ): EitherT[F, ArchiveError, Observed[A]]

trait OpenArchive[F[_]]:
  def revision: ArchiveRevision
  def structure: ArchiveValidation
  def payloads: PayloadExecutor[F]
  def validateContents: EitherT[F, ArchiveError, ArchiveValidation]

type ArchiveResource[F[_], A] =
  Resource[[X] =>> EitherT[F, ArchiveError, X], A]

trait ArchiveDriver[F[_]]:
  def open(location: ArchiveLocation): ArchiveResource[F, OpenArchive[F]]
