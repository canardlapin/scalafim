package scalafim.locus.laws

final case class LawBounds(
    maxRegionSize: Int = 12,
    maxMapSourceSize: Int = 7,
    maxMapTargetSize: Int = 7,
    maxRelationPairs: Int = 20,
    maxParcellationAmbientSize: Int = 9,
    maxParcelCount: Int = 5,
    maxCases: Long = 65536L
):
  require(maxRegionSize >= 0, "maximum region size must be non-negative")
  require(maxMapSourceSize >= 0, "maximum map source size must be non-negative")
  require(maxMapTargetSize >= 0, "maximum map target size must be non-negative")
  require(maxRelationPairs >= 0, "maximum relation pair count must be non-negative")
  require(maxParcellationAmbientSize >= 0, "maximum ambient size must be non-negative")
  require(maxParcelCount >= 0, "maximum parcel count must be non-negative")
  require(maxCases > 0L, "maximum case count must be positive")

enum EnumerationError:
  case NegativeSize(kind: String, size: Int)
  case BoundExceeded(kind: String, requested: Long, maximum: Long)
  case NoTotalMap(sourceSize: Int, targetSize: Int)

  def message: String =
    this match
      case NegativeSize(kind, size) =>
        s"$kind size must be non-negative, found $size"
      case BoundExceeded(kind, requested, maximum) =>
        s"$kind enumeration requires $requested cases, above bound $maximum"
      case NoTotalMap(sourceSize, targetSize) =>
        s"no total map exists from non-empty size $sourceSize to empty size $targetSize"

object Exhaustive:
  def regions(
      size: Int,
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[ReferenceRegion]] =
    for
      _ <- validateSize("region", size)
      _ <- validateMaximum("region size", size.toLong, bounds.maxRegionSize.toLong)
      count <- powerWithin(2, size, bounds.maxCases, "region")
    yield Vector.tabulate(count.toInt): mask =>
      ReferenceRegion(size, (0 until size).filter(i => (mask & (1 << i)) != 0).toSet)

  def totalMaps(
      fromSize: Int,
      toSize: Int,
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[ReferenceTotalMap]] =
    for
      _ <- validateSize("map source", fromSize)
      _ <- validateSize("map target", toSize)
      _ <- validateMaximum("map source size", fromSize.toLong, bounds.maxMapSourceSize.toLong)
      _ <- validateMaximum("map target size", toSize.toLong, bounds.maxMapTargetSize.toLong)
      result <-
        if fromSize > 0 && toSize == 0 then
          Left(EnumerationError.NoTotalMap(fromSize, toSize))
        else
          powerWithin(toSize, fromSize, bounds.maxCases, "total map").map: count =>
            Vector.tabulate(count.toInt): encoded =>
              ReferenceTotalMap(fromSize, toSize, decode(encoded, toSize, fromSize))
    yield result

  def relations(
      fromSize: Int,
      toSize: Int,
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[ReferenceRelation]] =
    for
      _ <- validateSize("relation source", fromSize)
      _ <- validateSize("relation target", toSize)
      pairCount = fromSize.toLong * toSize.toLong
      _ <- validateMaximum("relation pair count", pairCount, bounds.maxRelationPairs.toLong)
      count <- powerWithin(2, pairCount.toInt, bounds.maxCases, "relation")
    yield Vector.tabulate(count.toInt): mask =>
      val pairs =
        for
          source <- 0 until fromSize
          target <- 0 until toSize
          bit = source * toSize + target
          if (mask & (1 << bit)) != 0
        yield (source, target)
      ReferenceRelation(fromSize, toSize, pairs.toSet)

  def parcellations(
      ambientSize: Int,
      parcelCount: Int,
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[ReferenceParcellation]] =
    for
      _ <- validateSize("parcellation ambient", ambientSize)
      _ <- validateSize("parcellation parcels", parcelCount)
      _ <- validateMaximum(
        "parcellation ambient size",
        ambientSize.toLong,
        bounds.maxParcellationAmbientSize.toLong
      )
      _ <- validateMaximum(
        "parcel count",
        parcelCount.toLong,
        bounds.maxParcelCount.toLong
      )
      count <- powerWithin(parcelCount + 1, ambientSize, bounds.maxCases, "parcellation")
    yield
      val candidates = Vector.tabulate(count.toInt): encoded =>
        val digits = decode(encoded, parcelCount + 1, ambientSize)
        digits.map(digit => Option.when(digit > 0)(digit - 1))
      candidates
        .filter(assignments => assignments.flatten.toSet == (0 until parcelCount).toSet)
      .map(assignments => ReferenceParcellation(ambientSize, parcelCount, assignments))

  def fields[A](
      size: Int,
      alphabet: Vector[A],
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[ReferenceField[A]]] =
    for
      _ <- validateSize("field", size)
      count <- powerWithin(alphabet.size, size, bounds.maxCases, "field")
    yield Vector.tabulate(count.toInt): encoded =>
      ReferenceField(decode(encoded, alphabet.size, size).map(alphabet))

  def sections[A](
      fields: Vector[ReferenceField[A]],
      bounds: LawBounds = LawBounds()
  ): Either[EnumerationError, Vector[ReferenceSection[A]]] =
    if fields.isEmpty then Right(Vector.empty)
    else
      val size = fields.head.size
      if fields.exists(_.size != size) then
        throw new IllegalArgumentException("reference fields must share one size")
      regions(size, bounds).flatMap: supports =>
        val count = fields.size.toLong * supports.size.toLong
        validateMaximum("section", count, bounds.maxCases).map: _ =>
          for
            field <- fields
            support <- supports
          yield ReferenceSection(field, support)

  private def validateSize(kind: String, size: Int): Either[EnumerationError, Unit] =
    if size < 0 then Left(EnumerationError.NegativeSize(kind, size)) else Right(())

  private def validateMaximum(
      kind: String,
      requested: Long,
      maximum: Long
  ): Either[EnumerationError, Unit] =
    if requested > maximum then Left(EnumerationError.BoundExceeded(kind, requested, maximum))
    else Right(())

  private def powerWithin(
      base: Int,
      exponent: Int,
      maximum: Long,
      kind: String
  ): Either[EnumerationError, Long] =
    if exponent == 0 then Right(1L)
    else if base == 0 then Right(0L)
    else
      var result = 1L
      var i = 0
      var exceeded = false
      while i < exponent && !exceeded do
        if result > maximum / base then exceeded = true
        else result *= base
        i += 1
      if exceeded || result > maximum then
        Left(EnumerationError.BoundExceeded(kind, maximum + 1L, maximum))
      else Right(result)

  private def decode(encoded: Int, base: Int, length: Int): Vector[Int] =
    if length == 0 then Vector.empty
    else
      var value = encoded
      Vector.tabulate(length): _ =>
        val digit = value % base
        value /= base
        digit
