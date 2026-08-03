package scalafim.archive

object CanonicalArchiveManifestCodec:
  val Schema: String =
    "org.scalafim/neuroarchive-manifest@1"

  private val Prefix =
    "NAM1"

  def render(manifest: ArchiveManifest): String =
    val out = new StringBuilder
    out.append(Prefix)
    appendString(out, manifest.format.value)
    appendString(out, manifest.key.objectType.value)
    appendLong(out, manifest.key.schemaMajor.toLong)
    manifest.representation match
      case Some(representation) =>
        out.append('1')
        appendString(out, representation.key.value)
        appendValue(out, representation.descriptor)
        appendValue(out, representation.outputSchema)
      case None =>
        out.append('0')
    appendValue(out, manifest.attributes)
    appendLong(out, manifest.payloads.length.toLong)
    manifest.payloads.foreach: payload =>
      appendString(out, payload.id.value)
      appendString(out, payload.role.value)
      appendString(out, payload.scalarType.value)
      appendLong(out, payload.shape.length.toLong)
      payload.shape.foreach(appendLong(out, _))
      appendValue(out, payload.attributes)
    appendLong(out, manifest.integrity.entries.length.toLong)
    manifest.integrity.entries.foreach: entry =>
      appendString(out, entry.payload.value)
      appendString(out, entry.digest.algorithm)
      appendString(out, entry.digest.value)
    out.toString

  def parse(text: String): Either[ArchiveError, ArchiveManifest] =
    val parser = Parser(text)
    parser.parseManifest.flatMap: manifest =>
      if !parser.atEnd then
        Left(ArchiveError.InvalidArchive(
          s"canonical manifest has trailing input at offset ${parser.offset}"
        ))
      else if render(manifest) != text then
        Left(ArchiveError.InvalidArchive(
          "manifest input is structurally valid but not canonically encoded"
        ))
      else Right(manifest)

  private def appendValue(
      out: StringBuilder,
      value: CanonicalValue
  ): Unit =
    value match
      case CanonicalValue.Null =>
        out.append('N')
      case CanonicalValue.Boolean(found) =>
        out.append(if found then 'T' else 'F')
      case CanonicalValue.String(found) =>
        out.append('S')
        appendString(out, found)
      case CanonicalValue.Int64(found) =>
        out.append('I')
        appendLong(out, found)
      case CanonicalValue.Float64Bits(bits) =>
        out.append('D')
        val hex = java.lang.Long.toHexString(bits)
        var padding = hex.length
        while padding < 16 do
          out.append('0')
          padding += 1
        out.append(hex)
      case CanonicalValue.Array(values) =>
        out.append('A')
        appendLong(out, values.length.toLong)
        values.foreach(appendValue(out, _))
      case value: CanonicalValue.Object =>
        out.append('O')
        appendLong(out, value.entries.length.toLong)
        value.entries.foreach: (key, entry) =>
          appendString(out, key.value)
          appendValue(out, entry)

  private def appendString(
      out: StringBuilder,
      value: String
  ): Unit =
    out.append(value.length)
    out.append(':')
    out.append(value)

  private def appendLong(
      out: StringBuilder,
      value: Long
  ): Unit =
    out.append(value)
    out.append(';')

  private final class Parser(input: String):
    private var index = 0

    def offset: Int =
      index

    def atEnd: Boolean =
      index == input.length

    def parseManifest: Either[ArchiveError, ArchiveManifest] =
      for
        _ <- consumePrefix()
        formatText <- parseString()
        format <- ArchiveFormatKey.fromString(formatText)
        objectTypeText <- parseString()
        objectType <- ObjectTypeId.fromString(objectTypeText)
        schemaMajor <- parsePositiveInt("object schema major")
        representation <- parseRepresentation()
        key <- ObjectKey.from(
          objectType,
          schemaMajor,
          representation.map(_.key)
        )
        attributes <- parseValue()
        payloads <- parsePayloads()
        integrity <- parseIntegrity()
        manifest <- ArchiveManifest.from(
          format,
          key,
          representation,
          attributes,
          payloads,
          integrity
        )
      yield manifest

    private def consumePrefix(): Either[ArchiveError, Unit] =
      if input.startsWith(Prefix, index) then
        index += Prefix.length
        Right(())
      else
        Left(ArchiveError.InvalidArchive(
          s"canonical manifest must start with '$Prefix'"
        ))

    private def parseRepresentation()
        : Either[ArchiveError, Option[PersistedRepresentation]] =
      consumeChar().flatMap:
        case '0' =>
          Right(None)
        case '1' =>
          for
            keyText <- parseString()
            key <- RepresentationKey.fromString(keyText)
            descriptor <- parseValue()
            outputSchema <- parseValue()
          yield Some(PersistedRepresentation(
            key,
            descriptor,
            outputSchema
          ))
        case marker =>
          Left(ArchiveError.InvalidArchive(
            s"invalid representation marker '$marker' at offset ${index - 1}"
          ))

    private def parsePayloads()
        : Either[ArchiveError, Vector[PayloadDescriptor]] =
      parseCount("payload count").flatMap: count =>
        val values = Vector.newBuilder[PayloadDescriptor]
        var current = 0
        var failure = Option.empty[ArchiveError]
        while current < count && failure.isEmpty do
          parsePayload() match
            case Right(value) =>
              values += value
            case Left(error) =>
              failure = Some(error)
          current += 1
        failure.fold[Either[ArchiveError, Vector[PayloadDescriptor]]](
          Right(values.result())
        )(Left(_))

    private def parsePayload(): Either[ArchiveError, PayloadDescriptor] =
      for
        idText <- parseString()
        id <- PayloadId.fromString(idText)
        roleText <- parseString()
        role <- PayloadRoleId.fromString(roleText)
        scalarText <- parseString()
        scalarType <- ScalarTypeId.fromString(scalarText)
        rank <- parseCount("payload rank")
        shape <- parseShape(rank)
        attributes <- parseValue()
        descriptor <- PayloadDescriptor.from(
          id,
          role,
          scalarType,
          shape,
          attributes
        )
      yield descriptor

    private def parseShape(
        rank: Int
    ): Either[ArchiveError, Vector[Long]] =
      val values = Vector.newBuilder[Long]
      var current = 0
      while current < rank do
        parseLong() match
          case Right(value) =>
            values += value
          case Left(error) =>
            return Left(error)
        current += 1
      Right(values.result())

    private def parseIntegrity()
        : Either[ArchiveError, IntegrityManifest] =
      parseCount("integrity-entry count").flatMap: count =>
        val values = Vector.newBuilder[IntegrityEntry]
        var current = 0
        var failure = Option.empty[ArchiveError]
        while current < count && failure.isEmpty do
          val entry =
            for
              payloadText <- parseString()
              payload <- PayloadId.fromString(payloadText)
              algorithm <- parseString()
              digestText <- parseString()
              digest <- ContentDigest.from(algorithm, digestText)
            yield IntegrityEntry(payload, digest)
          entry match
            case Right(value) =>
              values += value
            case Left(error) =>
              failure = Some(error)
          current += 1
        failure match
          case Some(error) =>
            Left(error)
          case None =>
            IntegrityManifest.from(values.result())

    private def parseValue(): Either[ArchiveError, CanonicalValue] =
      consumeChar().flatMap:
        case 'N' =>
          Right(CanonicalValue.Null)
        case 'T' =>
          Right(CanonicalValue.boolean(true))
        case 'F' =>
          Right(CanonicalValue.boolean(false))
        case 'S' =>
          parseString().map(CanonicalValue.string)
        case 'I' =>
          parseLong().map(CanonicalValue.int64)
        case 'D' =>
          parseFloatBits().map(CanonicalValue.float64Bits)
        case 'A' =>
          parseArray()
        case 'O' =>
          parseObject()
        case marker =>
          Left(ArchiveError.InvalidArchive(
            s"invalid canonical-value marker '$marker' at offset ${index - 1}"
          ))

    private def parseArray(): Either[ArchiveError, CanonicalValue] =
      parseCount("canonical array length").flatMap: count =>
        val values = Vector.newBuilder[CanonicalValue]
        var current = 0
        var failure = Option.empty[ArchiveError]
        while current < count && failure.isEmpty do
          parseValue() match
            case Right(value) =>
              values += value
            case Left(error) =>
              failure = Some(error)
          current += 1
        failure match
          case Some(error) =>
            Left(error)
          case None =>
            Right(CanonicalValue.array(values.result()))

    private def parseObject(): Either[ArchiveError, CanonicalValue] =
      parseCount("canonical object length").flatMap: count =>
        val values = Vector.newBuilder[(CanonicalKey, CanonicalValue)]
        var current = 0
        var failure = Option.empty[ArchiveError]
        while current < count && failure.isEmpty do
          val entry =
            for
              keyText <- parseString()
              key <- CanonicalKey.fromString(keyText)
              value <- parseValue()
            yield key -> value
          entry match
            case Right(value) =>
              values += value
            case Left(error) =>
              failure = Some(error)
          current += 1
        failure match
          case Some(error) =>
            Left(error)
          case None =>
            CanonicalValue.Object.from(values.result())

    private def parseFloatBits(): Either[ArchiveError, Long] =
      if index + 16 > input.length then
        Left(ArchiveError.InvalidArchive(
          s"short float64 bit pattern at offset $index"
        ))
      else
        val text = input.substring(index, index + 16)
        if !text.forall(character =>
            character.isDigit ||
              (character >= 'a' && character <= 'f')
          )
        then
          Left(ArchiveError.InvalidArchive(
            s"invalid float64 bit pattern '$text'"
          ))
        else
          index += 16
          try
            Right(java.lang.Long.parseUnsignedLong(text, 16))
          catch
            case _: NumberFormatException =>
              Left(ArchiveError.InvalidArchive(
                s"invalid float64 bit pattern '$text'"
              ))

    private def parsePositiveInt(
        label: String
    ): Either[ArchiveError, Int] =
      parseLong().flatMap: value =>
        if value <= 0L || value > Int.MaxValue.toLong then
          Left(ArchiveError.InvalidArchive(
            s"$label must be a positive Int"
          ))
        else Right(value.toInt)

    private def parseCount(
        label: String
    ): Either[ArchiveError, Int] =
      parseLong().flatMap: value =>
        if value < 0L || value > Int.MaxValue.toLong then
          Left(ArchiveError.InvalidArchive(
            s"$label must be a non-negative Int"
          ))
        else Right(value.toInt)

    private def parseLong(): Either[ArchiveError, Long] =
      val end = input.indexOf(';', index)
      if end < 0 then
        Left(ArchiveError.InvalidArchive(
          s"unterminated integer at offset $index"
        ))
      else
        val text = input.substring(index, end)
        if text.isEmpty then
          Left(ArchiveError.InvalidArchive(
            s"empty integer at offset $index"
          ))
        else
          text.toLongOption match
            case Some(value) =>
              index = end + 1
              Right(value)
            case None =>
              Left(ArchiveError.InvalidArchive(
                s"invalid integer '$text' at offset $index"
              ))

    private def parseString(): Either[ArchiveError, String] =
      val separator = input.indexOf(':', index)
      if separator < 0 then
        Left(ArchiveError.InvalidArchive(
          s"unterminated string length at offset $index"
        ))
      else
        val lengthText = input.substring(index, separator)
        lengthText.toIntOption match
          case Some(length)
              if length >= 0 &&
                (length == 0 || !lengthText.startsWith("0")) &&
                separator + 1 <= input.length - length =>
            val start = separator + 1
            val end = start + length
            index = end
            Right(input.substring(start, end))
          case _ =>
            Left(ArchiveError.InvalidArchive(
              s"invalid string length '$lengthText' at offset $index"
            ))

    private def consumeChar(): Either[ArchiveError, Char] =
      if index >= input.length then
        Left(ArchiveError.InvalidArchive(
          "unexpected end of canonical manifest"
        ))
      else
        val value = input.charAt(index)
        index += 1
        Right(value)
