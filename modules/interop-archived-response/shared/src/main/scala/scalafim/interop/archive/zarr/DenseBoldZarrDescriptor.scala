package scalafim.interop.archive.zarr

import scalafim.archive.{
  ArchiveError,
  CanonicalKey,
  CanonicalValue,
  PayloadDescriptor,
  PayloadId,
  PhysicalObjectId
}
import scalafim.archive.zarr.{
  DenseBoldRevisionMetadata,
  NeuroArchiveManifest
}
import scalafim.interop.archive.RepresentationEnvelope
import scalafim.response.{
  CalibrationState,
  DomainId,
  DomainReference,
  NonFinitePolicy,
  ReferenceNamespace,
  ResponseSchema,
  ResponseSchemaId,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  SignalSchema,
  TimeAxis,
  TimeDomain,
  UnitId
}
import zarr4s.{
  ArrayDescriptor,
  ZarrMetadata,
  ZarrNodeMetadata,
  ZarrPath
}

final case class DenseBoldZarrObject(
    id: PhysicalObjectId,
    length: Long
)

final class DenseBoldZarrDescriptor private (
    val schema: ResponseSchema,
    val payload: PayloadDescriptor,
    val array: ArrayDescriptor,
    val canonicalPath: ZarrPath,
    val scale: Double,
    val offset: Double,
    val objects: Vector[DenseBoldZarrObject]
):
  private val objectLengths: Map[String, Long] =
    objects.iterator.map(value => value.id.value -> value.length).toMap

  def objectLength(id: PhysicalObjectId): Option[Long] =
    objectLengths.get(id.value)

object DenseBoldZarrDescriptor:
  def decode(
      envelope: RepresentationEnvelope
  ): Either[ArchiveError, DenseBoldZarrDescriptor] =
    for
      _ <-
        if envelope.key == DenseBoldRevisionMetadata.Representation then
          Right(())
        else
          Left(ArchiveError.InvalidArchive(
            s"dense Zarr binding received '${envelope.key.value}'"
          ))
      descriptor <- canonicalObject(
        envelope.descriptor,
        "dense Zarr representation descriptor"
      )
      _ <- requireString(
        descriptor,
        "format",
        DenseBoldRevisionMetadata.DescriptorFormat
      )
      _ <- requireString(
        descriptor,
        "profile",
        NeuroArchiveManifest.profileId
      )
      payloadIdText <- string(descriptor, "payload-id")
      payload <- uniquePayload(envelope, payloadIdText)
      metadata <- string(descriptor, "array-metadata")
      array <- decodeArray(metadata)
      _ <- validateArrayPayload(array, payload)
      pathText <- string(descriptor, "canonical-path")
      path <- ZarrPath(pathText)
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      calibration <- objectField(descriptor, "calibration")
      storedDataType <- string(calibration, "stored-data-type")
      _ <-
        if storedDataType == array.dataType.name then Right(())
        else Left(ArchiveError.InvalidArchive(
          s"dense Zarr calibration names '$storedDataType' but array stores " +
            s"'${array.dataType.name}'"
        ))
      physicalDataType <- string(calibration, "physical-data-type")
      _ <-
        if physicalDataType == "float64" then Right(())
        else Left(ArchiveError.InvalidArchive(
          s"dense Zarr output must be float64, found '$physicalDataType'"
        ))
      scale <- double(calibration, "scale")
      offset <- double(calibration, "offset")
      _ <-
        if scale.isFinite && scale != 0.0 && offset.isFinite then Right(())
        else Left(ArchiveError.InvalidArchive(
          "dense Zarr calibration scale must be finite and non-zero and offset finite"
        ))
      calibrationUnits <- string(calibration, "units")
      objects <- objectInventory(descriptor)
      schema <- decodeSchema(envelope.outputSchema)
      _ <- validateSchema(schema, array, calibrationUnits)
    yield new DenseBoldZarrDescriptor(
      schema,
      payload,
      array,
      path,
      scale,
      offset,
      objects
    )

  private def uniquePayload(
      envelope: RepresentationEnvelope,
      payloadId: String
  ): Either[ArchiveError, PayloadDescriptor] =
    envelope.payloads.values.filter(_.id.value == payloadId) match
      case Vector(found)
          if found.role == DenseBoldRevisionMetadata.PayloadRole =>
        Right(found)
      case Vector(found) =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr payload '${found.id.value}' has role " +
            s"'${found.role.value}', expected " +
            s"'${DenseBoldRevisionMetadata.PayloadRole.value}'"
        ))
      case Vector() =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr descriptor references missing payload '$payloadId'"
        ))
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr descriptor has duplicate payload '$payloadId'"
        ))

  private def decodeArray(
      metadata: String
  ): Either[ArchiveError, ArrayDescriptor] =
    ZarrMetadata
      .parse(metadata)
      .left
      .map(error => ArchiveError.InvalidArchive(error.message))
      .flatMap:
        case ZarrNodeMetadata.Array(found) =>
          ArrayDescriptor
            .compile(found)
            .left
            .map(error => ArchiveError.InvalidArchive(error.message))
        case _ =>
          Left(ArchiveError.InvalidArchive(
            "dense Zarr descriptor metadata must describe an array"
          ))

  private def validateArrayPayload(
      array: ArrayDescriptor,
      payload: PayloadDescriptor
  ): Either[ArchiveError, Unit] =
    if array.shape.rank.toInt != 4 then
      Left(ArchiveError.ShapeMismatch(
        "dense canonical Zarr response must have rank four [t,z,y,x]"
      ))
    else if
      array.dimensionNames.getOrElse(Vector.empty) !=
        NeuroArchiveManifest.canonicalAxes.map(Some(_))
    then
      Left(ArchiveError.ShapeMismatch(
        "dense canonical Zarr response axes must be [t,z,y,x]"
      ))
    else if payload.shape != array.shape.toVector then
      Left(ArchiveError.ShapeMismatch(
        s"dense payload shape ${payload.shape.mkString("x")} differs from " +
          s"Zarr shape ${array.shape.toVector.mkString("x")}"
      ))
    else if payload.scalarType.value != array.dataType.name then
      Left(ArchiveError.InvalidArchive(
        s"dense payload scalar '${payload.scalarType.value}' differs from " +
          s"Zarr scalar '${array.dataType.name}'"
      ))
    else Right(())

  private def objectInventory(
      descriptor: CanonicalValue.Object
  ): Either[ArchiveError, Vector[DenseBoldZarrObject]] =
    array(descriptor, "objects").flatMap(decodeObjectInventory)

  private def decodeObjectInventory(
      entries: Vector[CanonicalValue]
  ): Either[ArchiveError, Vector[DenseBoldZarrObject]] =
    if entries.isEmpty then
      Left(ArchiveError.InvalidArchive(
        "dense Zarr descriptor contains no published payload objects"
      ))
    else
      val result = Vector.newBuilder[DenseBoldZarrObject]
      val seen = scala.collection.mutable.HashSet.empty[String]
      var index = 0
      while index < entries.length do
        canonicalObject(entries(index), s"dense Zarr object $index") match
          case Left(error) =>
            return Left(error)
          case Right(value) =>
            (for
              idText <- string(value, "id")
              id <- PhysicalObjectId.fromString(idText)
              length <- int64(value, "length")
              _ <-
                if length > 0L then Right(())
                else Left(ArchiveError.InvalidArchive(
                  s"dense Zarr object '$idText' must have positive length"
                ))
              _ <-
                if seen.add(id.value) then Right(())
                else Left(ArchiveError.InvalidArchive(
                  s"dense Zarr descriptor contains duplicate object '${id.value}'"
                ))
            yield DenseBoldZarrObject(id, length)) match
              case Left(error) =>
                return Left(error)
              case Right(found) =>
                result += found
        index += 1
      Right(result.result().sortBy(_.id.value))

  private def decodeSchema(
      value: CanonicalValue
  ): Either[ArchiveError, ResponseSchema] =
    for
      root <- canonicalObject(value, "dense Zarr response schema")
      _ <- requireString(
        root,
        "format",
        DenseBoldRevisionMetadata.OutputSchemaFormat
      )
      schemaIdText <- string(root, "id")
      schemaId <- ResponseSchemaId
        .fromString(schemaIdText)
        .left
        .map(asArchive)
      timeValue <- objectField(root, "time")
      time <- decodeTime(timeValue)
      sampleValue <- objectField(root, "samples")
      samples <- decodeSamples(sampleValue)
      signalValue <- objectField(root, "signal")
      signal <- decodeSignal(signalValue)
      schema <- ResponseSchema
        .make(schemaId, time, samples, signal)
        .left
        .map(asArchive)
    yield schema

  private def decodeTime(
      value: CanonicalValue.Object
  ): Either[ArchiveError, TimeDomain] =
    for
      idText <- string(value, "id")
      id <- DomainId.fromString[TimeAxis](idText).left.map(asArchive)
      unitsText <- string(value, "units")
      units <- UnitId.fromString(unitsText).left.map(asArchive)
      count <- positiveInt(value, "count")
      kind <- string(value, "kind")
      time <- kind match
        case "regular" =>
          for
            origin <- double(value, "origin")
            step <- double(value, "step")
            result <- TimeDomain
              .regular(id, origin, step, count, units)
              .left
              .map(asArchive)
          yield result
        case "explicit" =>
          for
            values <- array(value, "coordinates")
            coordinates <- doubles(values, "time coordinates")
            _ <-
              if coordinates.length == count then Right(())
              else Left(ArchiveError.ShapeMismatch(
                s"dense Zarr time count $count differs from " +
                  s"${coordinates.length} coordinates"
              ))
            result <- TimeDomain
              .explicit(id, coordinates, units)
              .left
              .map(asArchive)
          yield result
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"unsupported dense Zarr time kind '$other'"
          ))
    yield time

  private def decodeSamples(
      value: CanonicalValue.Object
  ): Either[ArchiveError, SampleDomain] =
    for
      idText <- string(value, "id")
      id <- DomainId.fromString[SampleAxis](idText).left.map(asArchive)
      count <- positiveInt(value, "count")
      _ <- requireString(value, "kind", "volume")
      space <- reference(value, "space")
      mask <- optionalReference(value, "mask")
      ordering <- reference(value, "ordering")
      samples <- SampleDomain
        .make(
          id,
          count,
          SampleDomainKind.Volume(space, mask, ordering)
        )
        .left
        .map(asArchive)
    yield samples

  private def decodeSignal(
      value: CanonicalValue.Object
  ): Either[ArchiveError, SignalSchema] =
    for
      unitsText <- string(value, "units")
      units <- UnitId.fromString(unitsText).left.map(asArchive)
      _ <- requireString(value, "calibration", "applied")
      nonFiniteText <- string(value, "non-finite")
      nonFinite <- nonFiniteText match
        case "reject" =>
          Right(NonFinitePolicy.Reject)
        case "preserve" =>
          Right(NonFinitePolicy.Preserve)
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"unsupported dense Zarr non-finite policy '$other'"
          ))
    yield SignalSchema(units, CalibrationState.Applied, nonFinite)

  private def validateSchema(
      schema: ResponseSchema,
      array: ArrayDescriptor,
      calibrationUnits: String
  ): Either[ArchiveError, Unit] =
    val shape = array.shape.toVector
    val spatial = checkedProduct(shape.drop(1))
    if shape.exists(_ > Int.MaxValue.toLong) then
      Left(ArchiveError.ShapeMismatch(
        "dense Zarr dimensions exceed the response Int boundary"
      ))
    else
      spatial.flatMap: samples =>
        if schema.time.count != shape.head.toInt then
          Left(ArchiveError.ShapeMismatch(
            s"response time count ${schema.time.count} differs from Zarr " +
              s"time count ${shape.head}"
          ))
        else if schema.samples.count.toLong != samples then
          Left(ArchiveError.ShapeMismatch(
            s"response sample count ${schema.samples.count} differs from Zarr " +
              s"spatial count $samples"
          ))
        else if schema.signal.units.value != calibrationUnits then
          Left(ArchiveError.InvalidArchive(
            s"response units '${schema.signal.units.value}' differ from " +
              s"calibration units '$calibrationUnits'"
          ))
        else Right(())

  private def checkedProduct(
      values: Vector[Long]
  ): Either[ArchiveError, Long] =
    var result = 1L
    var index = 0
    while index < values.length do
      val value = values(index)
      if value != 0L && result > Long.MaxValue / value then
        return Left(ArchiveError.ShapeMismatch(
          "dense Zarr spatial sample count overflows Long"
        ))
      result *= value
      index += 1
    Right(result)

  private def optionalReference(
      value: CanonicalValue.Object,
      prefix: String
  ): Either[ArchiveError, Option[DomainReference]] =
    field(value, prefix) match
      case Some(CanonicalValue.Null) =>
        Right(None)
      case Some(_) =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr schema '$prefix' must be null or a reference"
        ))
      case None =>
        for
          found <- reference(value, prefix)
        yield Some(found)

  private def reference(
      value: CanonicalValue.Object,
      prefix: String
  ): Either[ArchiveError, DomainReference] =
    for
      namespaceText <- string(value, s"$prefix-namespace")
      namespace <- ReferenceNamespace
        .fromString(namespaceText)
        .left
        .map(asArchive)
      referenceValue <- string(value, s"$prefix-value")
      result <- DomainReference
        .make(namespace, referenceValue)
        .left
        .map(asArchive)
    yield result

  private def canonicalObject(
      value: CanonicalValue,
      label: String
  ): Either[ArchiveError, CanonicalValue.Object] =
    value match
      case found: CanonicalValue.Object =>
        Right(found)
      case CanonicalValue.Null =>
        Left(ArchiveError.InvalidArchive(s"$label is missing"))
      case _ =>
        Left(ArchiveError.InvalidArchive(s"$label must be a canonical object"))

  private def objectField(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, CanonicalValue.Object] =
    required(value, key).flatMap(canonicalObject(_, s"'$key'"))

  private def string(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, String] =
    required(value, key).flatMap:
      case CanonicalValue.String(found) =>
        Right(found)
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr metadata '$key' must be a string"
        ))

  private def int64(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, Long] =
    required(value, key).flatMap:
      case CanonicalValue.Int64(found) =>
        Right(found)
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr metadata '$key' must be an int64"
        ))

  private def positiveInt(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, Int] =
    int64(value, key).flatMap: found =>
      if found > 0L && found <= Int.MaxValue.toLong then
        Right(found.toInt)
      else
        Left(ArchiveError.ShapeMismatch(
          s"dense Zarr metadata '$key' must fit a positive Int; got $found"
        ))

  private def double(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, Double] =
    required(value, key).flatMap:
      case CanonicalValue.Float64Bits(bits) =>
        Right(java.lang.Double.longBitsToDouble(bits))
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr metadata '$key' must be float64 bits"
        ))

  private def array(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, Vector[CanonicalValue]] =
    required(value, key).flatMap:
      case CanonicalValue.Array(found) =>
        Right(found)
      case _ =>
        Left(ArchiveError.InvalidArchive(
          s"dense Zarr metadata '$key' must be an array"
        ))

  private def doubles(
      values: Vector[CanonicalValue],
      label: String
  ): Either[ArchiveError, Vector[Double]] =
    val result = Vector.newBuilder[Double]
    var index = 0
    while index < values.length do
      values(index) match
        case CanonicalValue.Float64Bits(bits) =>
          result += java.lang.Double.longBitsToDouble(bits)
        case _ =>
          return Left(ArchiveError.InvalidArchive(
            s"$label entry $index must be float64 bits"
          ))
      index += 1
    Right(result.result())

  private def requireString(
      value: CanonicalValue.Object,
      key: String,
      expected: String
  ): Either[ArchiveError, Unit] =
    string(value, key).flatMap: found =>
      if found == expected then Right(())
      else Left(ArchiveError.InvalidArchive(
        s"dense Zarr metadata '$key' must be '$expected', found '$found'"
      ))

  private def required(
      value: CanonicalValue.Object,
      key: String
  ): Either[ArchiveError, CanonicalValue] =
    field(value, key).toRight(ArchiveError.InvalidArchive(
      s"dense Zarr metadata is missing '$key'"
    ))

  private def field(
      value: CanonicalValue.Object,
      key: String
  ): Option[CanonicalValue] =
    value.entries.find(_._1 == CanonicalKey.unsafe(key)).map(_._2)

  private def asArchive(
      error: scalafim.response.ResponseFailure
  ): ArchiveError =
    ArchiveError.InvalidArchive(error.message)
