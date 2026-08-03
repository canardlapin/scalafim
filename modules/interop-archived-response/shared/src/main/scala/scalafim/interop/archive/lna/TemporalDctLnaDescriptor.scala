package scalafim.interop.archive.lna

import scalafim.archive.{ArchiveError, CanonicalValue}
import scalafim.interop.archive.RepresentationEnvelope
import scalafim.latent.{
  DctNorm,
  DctSpec,
  RepresentationInstanceId,
  RidgePenalty,
  TemporalDctRepresentation
}
import scalafim.response.{
  CalibrationState,
  DecodeConsistency,
  DomainId,
  DomainReference,
  NonFinitePolicy,
  ReconstructionContract,
  ReconstructionErrorBounds,
  ReferenceNamespace,
  ResponseFailure,
  ResponseSchema,
  ResponseSchemaId,
  SampleAxis,
  SampleDomain,
  SampleDomainKind,
  ScientificProfileId,
  SignalSchema,
  TimeAxis,
  TimeDomain,
  UnitId,
  ValidationReportRef
}

private[lna] object TemporalDctLnaDescriptor:
  private val SchemaFormat = "org.scalafim/response-schema@1"
  private val ModelFormat = "org.scalafim/temporal-dct-descriptor@1"

  def encodeSchema(schema: ResponseSchema): String =
    val fields = Vector.newBuilder[(String, String)]
    fields += "format" -> SchemaFormat
    fields += "schema.id" -> schema.id.value
    fields += "time.id" -> schema.time.id.value
    fields += "time.units" -> schema.time.units.value
    schema.time match
      case regular: TimeDomain.Regular =>
        fields += "time.kind" -> "regular"
        fields += "time.count" -> regular.count.toString
        fields += "time.origin-bits" -> bits(regular.origin.value)
        fields += "time.interval-bits" -> bits(regular.interval.value)
      case explicit: TimeDomain.Explicit =>
        fields += "time.kind" -> "explicit"
        fields += "time.count" -> explicit.count.toString
        fields += "time.coordinate-bits" ->
          explicit.rawCoordinateBits.map(java.lang.Long.toHexString).mkString(",")
    fields += "samples.id" -> schema.samples.id.value
    fields += "samples.count" -> schema.samples.count.toString
    schema.samples.kind match
      case SampleDomainKind.Volume(space, mask, ordering) =>
        fields += "samples.kind" -> "volume"
        addReference(fields, "samples.space", space)
        fields += "samples.mask" -> mask.fold("none")(_ => "some")
        mask.foreach(addReference(fields, "samples.mask-ref", _))
        addReference(fields, "samples.ordering", ordering)
      case SampleDomainKind.Surface(surface, topology, ordering) =>
        fields += "samples.kind" -> "surface"
        addReference(fields, "samples.surface", surface)
        addReference(fields, "samples.topology", topology)
        addReference(fields, "samples.ordering", ordering)
    fields += "signal.units" -> schema.signal.units.value
    fields += "signal.calibration" -> "applied"
    fields += "signal.non-finite" ->
      (schema.signal.nonFinite match
        case NonFinitePolicy.Reject => "reject"
        case NonFinitePolicy.Preserve => "preserve")
    FramedRecord.encode(fields.result())

  def encodeModel(model: TemporalDctRepresentation): String =
    val fields = Vector.newBuilder[(String, String)]
    fields += "format" -> ModelFormat
    fields += "instance.id" -> model.id.value
    fields += "dct.timepoints" -> model.spec.timepoints.toString
    fields += "dct.components" -> model.spec.components.toString
    fields += "dct.norm" -> model.spec.norm.metadataValue
    fields += "dct.center" -> model.center.toString
    fields += "dct.ridge-bits" -> bits(model.ridge.value)
    fields += "model.fingerprint" -> TemporalDctLnaModel.fingerprint(model)
    fields += "dependencies" -> TemporalDctLnaProfile.EmbeddedDependencies
    model.reconstructionContract match
      case ReconstructionContract.Exact =>
        fields += "reconstruction.kind" -> "exact"
      case ReconstructionContract.DeterministicBounded(bounds) =>
        fields += "reconstruction.kind" -> "bounded"
        fields += "reconstruction.absolute-bits" -> bits(bounds.absolute)
        fields += "reconstruction.relative-bits" -> bits(bounds.relative)
      case ReconstructionContract.ValidatedScientific(profile, report) =>
        fields += "reconstruction.kind" -> "validated"
        fields += "reconstruction.profile" -> profile.value
        fields += "reconstruction.report" -> report.value
    model.decodeConsistency match
      case DecodeConsistency.ExactBits =>
        fields += "consistency.kind" -> "exact-bits"
      case value: DecodeConsistency.UlpBounded =>
        fields += "consistency.kind" -> "ulp"
        fields += "consistency.max-ulps" -> value.maxUlps.toString
      case value: DecodeConsistency.AbsoluteRelative =>
        fields += "consistency.kind" -> "absolute-relative"
        fields += "consistency.absolute-bits" -> bits(value.absolute)
        fields += "consistency.relative-bits" -> bits(value.relative)
    FramedRecord.encode(fields.result())

  def decode(
      envelope: RepresentationEnvelope
  ): Either[ArchiveError, TemporalDctRepresentation] =
    for
      schemaText <- canonicalString(envelope.outputSchema, "response schema")
      modelText <- canonicalString(
        envelope.descriptor,
        "temporal DCT representation descriptor"
      )
      schema <- decodeSchema(schemaText)
      model <- decodeModel(modelText, schema)
    yield model

  private def decodeSchema(
      value: String
  ): Either[ArchiveError, ResponseSchema] =
    for
      fields <- FramedRecord.decode(value)
      _ <- requireValue(fields, "format", SchemaFormat)
      schemaIdText <- required(fields, "schema.id")
      schemaId <- ResponseSchemaId
        .fromString(schemaIdText)
        .left
        .map(asArchive)
      timeIdText <- required(fields, "time.id")
      timeId <- DomainId
        .fromString[TimeAxis](timeIdText)
        .left
        .map(asArchive)
      timeUnitsText <- required(fields, "time.units")
      timeUnits <- UnitId
        .fromString(timeUnitsText)
        .left
        .map(asArchive)
      time <- required(fields, "time.kind").flatMap:
        case "regular" =>
          for
            count <- positiveInt(fields, "time.count")
            origin <- rawDouble(fields, "time.origin-bits")
            interval <- rawDouble(fields, "time.interval-bits")
            domain <- TimeDomain
              .regular(timeId, origin, interval, count, timeUnits)
              .left
              .map(asArchive)
          yield domain
        case "explicit" =>
          for
            count <- positiveInt(fields, "time.count")
            encoded <- required(fields, "time.coordinate-bits")
            coordinates <- decodeRawDoubles(encoded, count)
            domain <- TimeDomain
              .explicit(timeId, coordinates, timeUnits)
              .left
              .map(asArchive)
          yield domain
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"unsupported response time-domain kind '$other'"
          ))
      sampleIdText <- required(fields, "samples.id")
      sampleId <- DomainId
        .fromString[SampleAxis](sampleIdText)
        .left
        .map(asArchive)
      sampleCount <- positiveInt(fields, "samples.count")
      sampleKind <- required(fields, "samples.kind").flatMap:
        case "volume" =>
          for
            space <- reference(fields, "samples.space")
            mask <- required(fields, "samples.mask").flatMap:
              case "none" =>
                Right(None)
              case "some" =>
                reference(fields, "samples.mask-ref").map(Some(_))
              case other =>
                Left(ArchiveError.InvalidArchive(
                  s"invalid response sample-mask marker '$other'"
                ))
            ordering <- reference(fields, "samples.ordering")
          yield SampleDomainKind.Volume(space, mask, ordering)
        case "surface" =>
          for
            surface <- reference(fields, "samples.surface")
            topology <- reference(fields, "samples.topology")
            ordering <- reference(fields, "samples.ordering")
          yield SampleDomainKind.Surface(surface, topology, ordering)
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"unsupported response sample-domain kind '$other'"
          ))
      samples <- SampleDomain
        .make(sampleId, sampleCount, sampleKind)
        .left
        .map(asArchive)
      signalUnitsText <- required(fields, "signal.units")
      signalUnits <- UnitId
        .fromString(signalUnitsText)
        .left
        .map(asArchive)
      _ <- requireValue(fields, "signal.calibration", "applied")
      nonFinite <- required(fields, "signal.non-finite").flatMap:
        case "reject" =>
          Right(NonFinitePolicy.Reject)
        case "preserve" =>
          Right(NonFinitePolicy.Preserve)
        case other =>
          Left(ArchiveError.InvalidArchive(
            s"unsupported non-finite response policy '$other'"
          ))
      schema <- ResponseSchema
        .make(
          schemaId,
          time,
          samples,
          SignalSchema(signalUnits, CalibrationState.Applied, nonFinite)
        )
        .left
        .map(asArchive)
    yield schema

  private def decodeModel(
      value: String,
      schema: ResponseSchema
  ): Either[ArchiveError, TemporalDctRepresentation] =
    for
      fields <- FramedRecord.decode(value)
      _ <- requireValue(fields, "format", ModelFormat)
      idText <- required(fields, "instance.id")
      id <- RepresentationInstanceId
        .fromString(idText)
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      timepoints <- positiveInt(fields, "dct.timepoints")
      components <- positiveInt(fields, "dct.components")
      norm <- required(fields, "dct.norm").flatMap:
        case DctNorm.Ortho.metadataValue =>
          Right(DctNorm.Ortho)
        case DctNorm.None.metadataValue =>
          Right(DctNorm.None)
        case other =>
          Left(ArchiveError.InvalidArchive(s"unsupported DCT norm '$other'"))
      spec <- DctSpec(timepoints, components, norm)
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      center <- boolean(fields, "dct.center")
      ridgeValue <- rawDouble(fields, "dct.ridge-bits")
      ridge <- RidgePenalty(ridgeValue)
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      reconstruction <- reconstructionContract(fields)
      consistency <- decodeConsistency(fields)
      model <- TemporalDctRepresentation
        .make(
          id,
          schema,
          spec,
          center,
          ridge,
          reconstruction,
          consistency
        )
        .left
        .map(error => ArchiveError.InvalidArchive(error.message))
      storedFingerprint <- required(fields, "model.fingerprint")
      _ <-
        if storedFingerprint == TemporalDctLnaModel.fingerprint(model) then
          Right(())
        else Left(ArchiveError.InvalidArchive(
          "temporal DCT descriptor fingerprint does not match its model fields"
        ))
      dependencies <- required(fields, "dependencies")
      _ <-
        if dependencies == TemporalDctLnaProfile.EmbeddedDependencies then
          Right(())
        else Left(ArchiveError.InvalidArchive(
          s"unsupported temporal DCT dependency mode '$dependencies'"
        ))
    yield model

  private def reconstructionContract(
      fields: Map[String, String]
  ): Either[ArchiveError, ReconstructionContract] =
    required(fields, "reconstruction.kind").flatMap:
      case "exact" =>
        Right(ReconstructionContract.Exact)
      case "bounded" =>
        for
          absolute <- rawDouble(fields, "reconstruction.absolute-bits")
          relative <- rawDouble(fields, "reconstruction.relative-bits")
          bounds <- ReconstructionErrorBounds
            .make(absolute, relative)
            .left
            .map(asArchive)
        yield ReconstructionContract.DeterministicBounded(bounds)
      case "validated" =>
        for
          profileText <- required(fields, "reconstruction.profile")
          profile <- ScientificProfileId
            .fromString(profileText)
            .left
            .map(asArchive)
          reportText <- required(fields, "reconstruction.report")
          report <- ValidationReportRef
            .fromString(reportText)
            .left
            .map(asArchive)
        yield ReconstructionContract.ValidatedScientific(profile, report)
      case other =>
        Left(ArchiveError.InvalidArchive(
          s"unsupported reconstruction contract '$other'"
        ))

  private def decodeConsistency(
      fields: Map[String, String]
  ): Either[ArchiveError, DecodeConsistency] =
    required(fields, "consistency.kind").flatMap:
      case "exact-bits" =>
        Right(DecodeConsistency.ExactBits)
      case "ulp" =>
        for
          maxUlps <- nonNegativeInt(fields, "consistency.max-ulps")
          consistency <- DecodeConsistency
            .ulpBounded(maxUlps)
            .left
            .map(asArchive)
        yield consistency
      case "absolute-relative" =>
        for
          absolute <- rawDouble(fields, "consistency.absolute-bits")
          relative <- rawDouble(fields, "consistency.relative-bits")
          consistency <- DecodeConsistency
            .absoluteRelative(absolute, relative)
            .left
            .map(asArchive)
        yield consistency
      case other =>
        Left(ArchiveError.InvalidArchive(
          s"unsupported decode consistency '$other'"
        ))

  private def canonicalString(
      value: CanonicalValue,
      label: String
  ): Either[ArchiveError, String] =
    value match
      case CanonicalValue.String(found) =>
        Right(found)
      case CanonicalValue.Null =>
        Left(ArchiveError.InvalidArchive(s"$label is missing"))
      case _ =>
        Left(ArchiveError.InvalidArchive(s"$label must be a canonical string"))

  private def reference(
      fields: Map[String, String],
      prefix: String
  ): Either[ArchiveError, DomainReference] =
    for
      namespaceText <- required(fields, s"$prefix.namespace")
      namespace <- ReferenceNamespace
        .fromString(namespaceText)
        .left
        .map(asArchive)
      value <- required(fields, s"$prefix.value")
      result <- DomainReference
        .make(namespace, value)
        .left
        .map(asArchive)
    yield result

  private def addReference(
      fields: scala.collection.mutable.Builder[
        (String, String),
        Vector[(String, String)]
      ],
      prefix: String,
      reference: DomainReference
  ): Unit =
    fields += s"$prefix.namespace" -> reference.namespace.value
    fields += s"$prefix.value" -> reference.value

  private def required(
      fields: Map[String, String],
      key: String
  ): Either[ArchiveError, String] =
    fields
      .get(key)
      .toRight(ArchiveError.InvalidArchive(
        s"representation descriptor is missing '$key'"
      ))

  private def requireValue(
      fields: Map[String, String],
      key: String,
      expected: String
  ): Either[ArchiveError, Unit] =
    required(fields, key).flatMap: actual =>
      if actual == expected then Right(())
      else Left(ArchiveError.InvalidArchive(
        s"representation descriptor '$key' expected '$expected' but found '$actual'"
      ))

  private def positiveInt(
      fields: Map[String, String],
      key: String
  ): Either[ArchiveError, Int] =
    integer(fields, key).flatMap: value =>
      if value > 0 then Right(value)
      else Left(ArchiveError.InvalidArchive(s"'$key' must be positive"))

  private def nonNegativeInt(
      fields: Map[String, String],
      key: String
  ): Either[ArchiveError, Int] =
    integer(fields, key).flatMap: value =>
      if value >= 0 then Right(value)
      else Left(ArchiveError.InvalidArchive(s"'$key' must be non-negative"))

  private def integer(
      fields: Map[String, String],
      key: String
  ): Either[ArchiveError, Int] =
    required(fields, key).flatMap: value =>
      value.toIntOption.toRight(
        ArchiveError.InvalidArchive(s"'$key' must be an integer")
      )

  private def boolean(
      fields: Map[String, String],
      key: String
  ): Either[ArchiveError, Boolean] =
    required(fields, key).flatMap:
      case "true" => Right(true)
      case "false" => Right(false)
      case _ => Left(ArchiveError.InvalidArchive(s"'$key' must be a boolean"))

  private def rawDouble(
      fields: Map[String, String],
      key: String
  ): Either[ArchiveError, Double] =
    required(fields, key).flatMap: value =>
      rawBits(value)
        .map(java.lang.Double.longBitsToDouble)
        .toRight(ArchiveError.InvalidArchive(
          s"'$key' must contain raw hexadecimal Double bits"
        ))

  private def decodeRawDoubles(
      encoded: String,
      expected: Int
  ): Either[ArchiveError, Vector[Double]] =
    val fields =
      if encoded.isEmpty then Vector.empty
      else encoded.split(',').toVector
    if fields.length != expected then
      Left(ArchiveError.InvalidArchive(
        s"explicit response time coordinates expected $expected values but found ${fields.length}"
      ))
    else
      val values = Vector.newBuilder[Double]
      var index = 0
      while index < fields.length do
        rawBits(fields(index)) match
          case Some(bits) =>
            values += java.lang.Double.longBitsToDouble(bits)
          case None =>
            return Left(ArchiveError.InvalidArchive(
              s"explicit response time coordinate $index has invalid raw bits"
            ))
        index += 1
      Right(values.result())

  private def rawBits(value: String): Option[Long] =
    try Some(java.lang.Long.parseUnsignedLong(value, 16))
    catch
      case _: NumberFormatException =>
        None

  private def bits(value: Double): String =
    java.lang.Long.toHexString(java.lang.Double.doubleToRawLongBits(value))

  private def asArchive(error: ResponseFailure): ArchiveError =
    ArchiveError.InvalidArchive(error.message)

private object FramedRecord:
  def encode(values: Vector[(String, String)]): String =
    values.flatMap((key, value) => Vector(key, value))
      .map(value => s"${value.length}:$value")
      .mkString

  def decode(
      value: String
  ): Either[ArchiveError, Map[String, String]] =
    val fields = Vector.newBuilder[String]
    var offset = 0
    while offset < value.length do
      val colon = value.indexOf(':', offset)
      if colon < 0 then
        return Left(ArchiveError.InvalidArchive(
          "length-framed representation descriptor is missing ':'"
        ))
      val lengthText = value.substring(offset, colon)
      if lengthText.isEmpty ||
          !lengthText.forall(_.isDigit) ||
          (lengthText.length > 1 && lengthText.head == '0')
      then
        return Left(ArchiveError.InvalidArchive(
          "length-framed representation descriptor has an invalid length"
        ))
      val length =
        lengthText.toIntOption match
          case Some(found) =>
            found
          case None =>
            return Left(ArchiveError.InvalidArchive(
              "length-framed representation descriptor length overflows Int"
            ))
      val start = colon + 1
      if length > value.length - start then
        return Left(ArchiveError.InvalidArchive(
          "length-framed representation descriptor is truncated"
        ))
      fields += value.substring(start, start + length)
      offset = start + length
    val decoded = fields.result()
    if decoded.length % 2 != 0 then
      Left(ArchiveError.InvalidArchive(
        "length-framed representation descriptor has an unmatched key"
      ))
    else
      val pairs = decoded.grouped(2).map(values => values(0) -> values(1)).toVector
      val duplicate =
        pairs.groupBy(_._1).collectFirst:
          case (key, values) if values.lengthCompare(1) > 0 =>
            key
      duplicate match
        case Some(key) =>
          Left(ArchiveError.InvalidArchive(
            s"representation descriptor contains duplicate key '$key'"
          ))
        case None =>
          Right(pairs.toMap)
