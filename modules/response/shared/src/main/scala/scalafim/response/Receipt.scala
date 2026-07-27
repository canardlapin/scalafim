package scalafim.response

opaque type PayloadId = String

object PayloadId:
  def fromString(value: String): Either[IdentityError, PayloadId] =
    ResponseIdentity.validate("payload id", value)

  def unsafe(value: String): PayloadId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: PayloadId)
    inline def value: String =
      id

opaque type ObjectId = String

object ObjectId:
  def fromString(value: String): Either[IdentityError, ObjectId] =
    ResponseIdentity.validate("object id", value)

  def unsafe(value: String): ObjectId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ObjectId)
    inline def value: String =
      id

opaque type ChunkId = String

object ChunkId:
  def fromString(value: String): Either[IdentityError, ChunkId] =
    ResponseIdentity.validate("chunk id", value)

  def unsafe(value: String): ChunkId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ChunkId)
    inline def value: String =
      id

final class ByteRange private (
    val start: Long,
    val length: Long
):
  val endExclusive: Long =
    start + length

  def contains(other: ByteRange): Boolean =
    other.start >= start && other.endExclusive <= endExclusive

  override def equals(other: Any): Boolean =
    other match
      case that: ByteRange =>
        start == that.start && length == that.length
      case _ =>
        false

  override def hashCode(): Int =
    31 * java.lang.Long.hashCode(start) + java.lang.Long.hashCode(length)

object ByteRange:
  def make(start: Long, length: Long): Either[ReceiptConformanceError, ByteRange] =
    if start < 0L then
      Left(ReceiptConformanceError.InvalidEvidence(
        SelectionAxes.TimeAndSamples,
        s"byte-range start must be non-negative; got $start"
      ))
    else if length <= 0L then
      Left(ReceiptConformanceError.InvalidEvidence(
        SelectionAxes.TimeAndSamples,
        s"byte-range length must be positive; got $length"
      ))
    else if start > Long.MaxValue - length then
      Left(ReceiptConformanceError.InvalidEvidence(
        SelectionAxes.TimeAndSamples,
        s"byte range $start + $length overflows Long"
      ))
    else
      Right(new ByteRange(start, length))

  def unsafe(start: Long, length: Long): ByteRange =
    make(start, length).fold(error => throw new IllegalArgumentException(error.message), identity)

enum PhysicalLocality:
  case Resident
  case WholeObject
  case WholePayload
  case ChunkBounded
  case ByteRangeBounded

final case class AxisLocality(
    axes: SelectionAxes,
    guarantee: PhysicalLocality
)

final class ReadCapabilities private (
    val localityByAxes: Vector[AxisLocality]
):
  def forAxes(axes: SelectionAxes): Option[PhysicalLocality] =
    localityByAxes.find(_.axes == axes).map(_.guarantee)

object ReadCapabilities:
  def make(
      claims: Vector[AxisLocality]
  ): Either[ReceiptConformanceError, ReadCapabilities] =
    if claims.isEmpty then Left(ReceiptConformanceError.EmptyCapabilities)
    else
      val seen = scala.collection.mutable.HashSet.empty[SelectionAxes]
      var index = 0
      while index < claims.length do
        val axes = claims(index).axes
        if seen.contains(axes) then
          return Left(ReceiptConformanceError.DuplicateCapability(axes))
        seen += axes
        index += 1
      Right(new ReadCapabilities(claims))

  def uniform(locality: PhysicalLocality): ReadCapabilities =
    new ReadCapabilities(
      Vector(
        AxisLocality(SelectionAxes.Time, locality),
        AxisLocality(SelectionAxes.Samples, locality),
        AxisLocality(SelectionAxes.TimeAndSamples, locality)
      )
    )

  val Resident: ReadCapabilities =
    uniform(PhysicalLocality.Resident)

final case class PayloadLayout(
    id: PayloadId,
    objects: Vector[ObjectId],
    chunks: Vector[ChunkId]
)

final class OpenedLayout private (
    val payloads: Vector[PayloadLayout]
):
  private val payloadById: Map[String, PayloadLayout] =
    payloads.iterator.map(payload => payload.id.value -> payload).toMap

  private val objectIds: Set[String] =
    payloads.iterator.flatMap(_.objects.iterator.map(_.value)).toSet

  def containsPayload(payload: PayloadId): Boolean =
    payloadById.contains(payload.value)

  def containsObject(objectId: ObjectId): Boolean =
    objectIds.contains(objectId.value)

  def containsChunk(payload: PayloadId, chunk: ChunkId): Boolean =
    payloadById
      .get(payload.value)
      .exists(_.chunks.exists(_.value == chunk.value))

object OpenedLayout:
  val Resident: OpenedLayout =
    new OpenedLayout(Vector.empty)

  def make(
      payloads: Vector[PayloadLayout]
  ): Either[ReceiptConformanceError, OpenedLayout] =
    val seenPayloads = scala.collection.mutable.HashSet.empty[String]
    val seenObjects = scala.collection.mutable.HashSet.empty[String]
    var payloadIndex = 0
    while payloadIndex < payloads.length do
      val payload = payloads(payloadIndex)
      if seenPayloads.contains(payload.id.value) then
        return Left(ReceiptConformanceError.InvalidEvidence(
          SelectionAxes.TimeAndSamples,
          s"duplicate payload '${payload.id.value}'"
        ))
      seenPayloads += payload.id.value
      var objectIndex = 0
      while objectIndex < payload.objects.length do
        val objectId = payload.objects(objectIndex)
        if seenObjects.contains(objectId.value) then
          return Left(ReceiptConformanceError.InvalidEvidence(
            SelectionAxes.TimeAndSamples,
            s"object '${objectId.value}' belongs to more than one payload"
          ))
        seenObjects += objectId.value
        objectIndex += 1
      if payload.chunks.map(_.value).distinct.length != payload.chunks.length then
        return Left(ReceiptConformanceError.InvalidEvidence(
          SelectionAxes.TimeAndSamples,
          s"payload '${payload.id.value}' contains duplicate chunks"
        ))
      payloadIndex += 1
    Right(new OpenedLayout(payloads))

enum PhysicalReadUnit:
  case Object(objectId: ObjectId)
  case Payload(payload: PayloadId)
  case Chunk(payload: PayloadId, chunk: ChunkId)
  case Range(objectId: ObjectId, bytes: ByteRange)

final case class AxisReadEvidence(
    axes: SelectionAxes,
    observed: PhysicalLocality,
    touched: Vector[PhysicalReadUnit],
    covering: Vector[PhysicalReadUnit]
)

final case class LogicalReadSummary(
    schema: ResponseSchemaId,
    axes: SelectionAxes,
    timepoints: Vector[Int],
    samples: Vector[Int],
    logicalBytes: Long
)

enum PhysicalByteEvidence:
  case Known(bytes: Long)
  case Unavailable(reason: OperationId)

final case class PhysicalReadSummary(
    byAxes: Vector[AxisReadEvidence],
    physicalBytes: PhysicalByteEvidence,
    cacheHits: Int
)

enum IntegrityEvidence:
  case NotChecked
  case Verified(reference: DomainReference)

final case class ReadFallback(
    axes: SelectionAxes,
    reason: OperationId,
    from: PhysicalLocality,
    to: PhysicalLocality
)

final case class ReadReceipt(
    logical: LogicalReadSummary,
    physical: PhysicalReadSummary,
    integrity: Vector[IntegrityEvidence],
    fallbacks: Vector[ReadFallback]
)

object ReadReceipt:
  def resident(
      selection: ResolvedResponseSelection
  ): ReadReceipt =
    val logicalBytes =
      selection.rows.toLong * selection.columns.toLong * java.lang.Double.BYTES.toLong
    val axes = selection.selectedAxes
    ReadReceipt(
      LogicalReadSummary(
        selection.schema,
        axes,
        selection.timepoints.values,
        selection.samples.values,
        logicalBytes
      ),
      PhysicalReadSummary(
        Vector(AxisReadEvidence(axes, PhysicalLocality.Resident, Vector.empty, Vector.empty)),
        physicalBytes = PhysicalByteEvidence.Known(0L),
        cacheHits = 0
      ),
      Vector(IntegrityEvidence.NotChecked),
      Vector.empty
    )

object ReceiptConformance:
  def check(
      capabilities: ReadCapabilities,
      receipt: ReadReceipt,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Unit] =
    if receipt.logical.logicalBytes < 0L then
      Left(ReceiptConformanceError.InvalidByteCount(
        "logical",
        receipt.logical.logicalBytes
      ))
    else if receipt.physical.physicalBytes match
        case PhysicalByteEvidence.Known(bytes) =>
          bytes < 0L
        case PhysicalByteEvidence.Unavailable(_) =>
          false
    then
      val bytes = receipt.physical.physicalBytes match
        case PhysicalByteEvidence.Known(value) =>
          value
        case PhysicalByteEvidence.Unavailable(_) =>
          0L
      Left(ReceiptConformanceError.InvalidByteCount("physical", bytes))
    else if receipt.physical.cacheHits < 0 then
      Left(ReceiptConformanceError.InvalidEvidence(
        SelectionAxes.TimeAndSamples,
        s"cache hits must be non-negative; got ${receipt.physical.cacheHits}"
      ))
    else
      val evidenceByAxes = scala.collection.mutable.HashMap.empty[SelectionAxes, AxisReadEvidence]
      var evidenceIndex = 0
      while evidenceIndex < receipt.physical.byAxes.length do
        val evidence = receipt.physical.byAxes(evidenceIndex)
        if evidenceByAxes.contains(evidence.axes) then
          return Left(ReceiptConformanceError.DuplicateEvidence(evidence.axes))
        evidenceByAxes.update(evidence.axes, evidence)
        evidenceIndex += 1

      val axesToCheck =
        if receipt.logical.axes == SelectionAxes.TimeAndSamples &&
            capabilities.forAxes(SelectionAxes.TimeAndSamples).isEmpty &&
            evidenceByAxes.contains(SelectionAxes.Time) &&
            evidenceByAxes.contains(SelectionAxes.Samples)
        then Vector(SelectionAxes.Time, SelectionAxes.Samples)
        else Vector(receipt.logical.axes)

      var axisIndex = 0
      while axisIndex < axesToCheck.length do
        val axes = axesToCheck(axisIndex)
        val declared = capabilities.forAxes(axes) match
          case Some(value) =>
            value
          case None =>
            return Left(ReceiptConformanceError.MissingEvidence(axes))
        val evidence = evidenceByAxes.get(axes) match
          case Some(value) =>
            value
          case None =>
            return Left(ReceiptConformanceError.MissingEvidence(axes))
        if evidence.observed != declared then
          return Left(ReceiptConformanceError.LocalityMismatch(
            axes,
            declared,
            evidence.observed
          ))
        validateEvidence(evidence, layout) match
          case Left(error) =>
            return Left(error)
          case Right(_) =>
            ()
        axisIndex += 1
      Right(())

  private def validateEvidence(
      evidence: AxisReadEvidence,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Unit] =
    evidence.observed match
      case PhysicalLocality.Resident =>
        if evidence.touched.nonEmpty || evidence.covering.nonEmpty then
          Left(ReceiptConformanceError.InvalidEvidence(
            evidence.axes,
            "resident evidence must not name physical units"
          ))
        else Right(())
      case PhysicalLocality.WholeObject =>
        validateOnlyObjects(evidence, layout)
      case PhysicalLocality.WholePayload =>
        validateOnlyPayloads(evidence, layout)
      case PhysicalLocality.ChunkBounded =>
        validateChunks(evidence, layout)
      case PhysicalLocality.ByteRangeBounded =>
        validateRanges(evidence, layout)

  private def validateOnlyObjects(
      evidence: AxisReadEvidence,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Unit] =
    for
      touched <- objectIds(evidence.touched, evidence.axes, "evidence", layout)
      covering <- objectIds(evidence.covering, evidence.axes, "cover", layout)
      _ <-
        val outside = touched.diff(covering)
        if outside.isEmpty then Right(())
        else
          Left(ReceiptConformanceError.TouchOutsideCover(
            evidence.axes,
            outside.toVector.sorted.mkString(",")
          ))
    yield ()

  private def validateOnlyPayloads(
      evidence: AxisReadEvidence,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Unit] =
    for
      touched <- payloadIds(evidence.touched, evidence.axes, "evidence", layout)
      covering <- payloadIds(evidence.covering, evidence.axes, "cover", layout)
      _ <-
        val outside = touched.diff(covering)
        if outside.isEmpty then Right(())
        else
          Left(ReceiptConformanceError.TouchOutsideCover(
            evidence.axes,
            outside.toVector.sorted.mkString(",")
          ))
    yield ()

  private def objectIds(
      units: Vector[PhysicalReadUnit],
      axes: SelectionAxes,
      role: String,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Set[String]] =
    if units.isEmpty then
      Left(ReceiptConformanceError.InvalidEvidence(
        axes,
        s"whole-object $role is empty"
      ))
    else
      val ids = scala.collection.mutable.HashSet.empty[String]
      var index = 0
      while index < units.length do
        units(index) match
          case PhysicalReadUnit.Object(id) =>
            if !layout.containsObject(id) then
              return Left(ReceiptConformanceError.UnknownObject(id))
            ids += id.value
          case _ =>
            return Left(ReceiptConformanceError.InvalidEvidence(
              axes,
              s"whole-object $role contains a non-object unit"
            ))
        index += 1
      Right(ids.toSet)

  private def payloadIds(
      units: Vector[PhysicalReadUnit],
      axes: SelectionAxes,
      role: String,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Set[String]] =
    if units.isEmpty then
      Left(ReceiptConformanceError.InvalidEvidence(
        axes,
        s"whole-payload $role is empty"
      ))
    else
      val ids = scala.collection.mutable.HashSet.empty[String]
      var index = 0
      while index < units.length do
        units(index) match
          case PhysicalReadUnit.Payload(id) =>
            if !layout.containsPayload(id) then
              return Left(ReceiptConformanceError.UnknownPayload(id))
            ids += id.value
          case _ =>
            return Left(ReceiptConformanceError.InvalidEvidence(
              axes,
              s"whole-payload $role contains a non-payload unit"
            ))
        index += 1
      Right(ids.toSet)

  private def validateChunks(
      evidence: AxisReadEvidence,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Unit] =
    if evidence.touched.isEmpty || evidence.covering.isEmpty then
      Left(ReceiptConformanceError.InvalidEvidence(
        evidence.axes,
        "chunk-bounded evidence and cover must both be non-empty"
      ))
    else
      val touched = scala.collection.mutable.HashSet.empty[(String, String)]
      val covering = scala.collection.mutable.HashSet.empty[(String, String)]
      var touchedIndex = 0
      while touchedIndex < evidence.touched.length do
        evidence.touched(touchedIndex) match
          case PhysicalReadUnit.Chunk(payload, chunk) =>
            if !layout.containsChunk(payload, chunk) then
              return Left(ReceiptConformanceError.UnknownChunk(payload, chunk))
            touched += payload.value -> chunk.value
          case _ =>
            return Left(ReceiptConformanceError.InvalidEvidence(
              evidence.axes,
              "chunk-bounded evidence contains a non-chunk unit"
            ))
        touchedIndex += 1
      var coveringIndex = 0
      while coveringIndex < evidence.covering.length do
        evidence.covering(coveringIndex) match
          case PhysicalReadUnit.Chunk(payload, chunk) =>
            if !layout.containsChunk(payload, chunk) then
              return Left(ReceiptConformanceError.UnknownChunk(payload, chunk))
            covering += payload.value -> chunk.value
          case _ =>
            return Left(ReceiptConformanceError.InvalidEvidence(
              evidence.axes,
              "chunk cover contains a non-chunk unit"
            ))
        coveringIndex += 1
      val outside = touched.diff(covering)
      if outside.nonEmpty then
        Left(ReceiptConformanceError.TouchOutsideCover(
          evidence.axes,
          outside.toVector.sorted.mkString(",")
        ))
      else Right(())

  private def validateRanges(
      evidence: AxisReadEvidence,
      layout: OpenedLayout
  ): Either[ReceiptConformanceError, Unit] =
    if evidence.touched.isEmpty || evidence.covering.isEmpty then
      Left(ReceiptConformanceError.InvalidEvidence(
        evidence.axes,
        "byte-range evidence and cover must both be non-empty"
      ))
    else
      val covers = Vector.newBuilder[(ObjectId, ByteRange)]
      var coveringIndex = 0
      while coveringIndex < evidence.covering.length do
        evidence.covering(coveringIndex) match
          case PhysicalReadUnit.Range(objectId, bytes) =>
            if !layout.containsObject(objectId) then
              return Left(ReceiptConformanceError.UnknownObject(objectId))
            covers += objectId -> bytes
          case _ =>
            return Left(ReceiptConformanceError.InvalidEvidence(
              evidence.axes,
              "byte-range cover contains a non-range unit"
            ))
        coveringIndex += 1
      val coverValues = covers.result()
      var touchedIndex = 0
      while touchedIndex < evidence.touched.length do
        evidence.touched(touchedIndex) match
          case PhysicalReadUnit.Range(objectId, bytes) =>
            if !layout.containsObject(objectId) then
              return Left(ReceiptConformanceError.UnknownObject(objectId))
            val isCovered = coverValues.exists: (coverObject, coverBytes) =>
              coverObject == objectId && coverBytes.contains(bytes)
            if !isCovered then
              return Left(ReceiptConformanceError.TouchOutsideCover(
                evidence.axes,
                s"${objectId.value}:${bytes.start}+${bytes.length}"
              ))
          case _ =>
            return Left(ReceiptConformanceError.InvalidEvidence(
              evidence.axes,
              "byte-range evidence contains a non-range unit"
            ))
        touchedIndex += 1
      Right(())
