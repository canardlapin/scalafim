package scalafim.response

opaque type ResponseSchemaId = String

object ResponseSchemaId:
  def fromString(value: String): Either[IdentityError, ResponseSchemaId] =
    ResponseIdentity.validate("response schema id", value)

  def unsafe(value: String): ResponseSchemaId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: ResponseSchemaId)
    inline def value: String =
      id

opaque type UnitId = String

object UnitId:
  def fromString(value: String): Either[IdentityError, UnitId] =
    ResponseIdentity.validate("unit id", value)

  def unsafe(value: String): UnitId =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (id: UnitId)
    inline def value: String =
      id

final class TimeCoordinate private (val rawBits: Long):
  def value: Double =
    java.lang.Double.longBitsToDouble(rawBits)

  override def equals(other: Any): Boolean =
    other match
      case that: TimeCoordinate =>
        rawBits == that.rawBits
      case _ =>
        false

  override def hashCode(): Int =
    (rawBits ^ (rawBits >>> 32)).toInt

  override def toString: String =
    value.toString

object TimeCoordinate:
  def finite(
      label: String,
      value: Double
  ): Either[SchemaError, TimeCoordinate] =
    if value.isNaN || value.isInfinity then
      Left(SchemaError.InvalidCoordinate(label, value))
    else
      Right(new TimeCoordinate(java.lang.Double.doubleToRawLongBits(value)))

  private[response] def unsafe(value: Double): TimeCoordinate =
    new TimeCoordinate(java.lang.Double.doubleToRawLongBits(value))

sealed trait TimeDomain:
  def id: DomainId[TimeAxis]
  def count: Int
  def units: UnitId
  def coordinate(index: AxisIndex[TimeAxis]): Either[IndexError, TimeCoordinate]

object TimeDomain:
  final class Regular private[response] (
      val id: DomainId[TimeAxis],
      val origin: TimeCoordinate,
      val interval: TimeCoordinate,
      val count: Int,
      val units: UnitId
  ) extends TimeDomain:
    def coordinate(index: AxisIndex[TimeAxis]): Either[IndexError, TimeCoordinate] =
      if index.value >= count then Left(IndexError.OutOfBounds(index.value, count))
      else
        Right(TimeCoordinate.unsafe(origin.value + index.value.toDouble * interval.value))

    override def equals(other: Any): Boolean =
      other match
        case that: Regular =>
          id == that.id &&
            origin == that.origin &&
            interval == that.interval &&
            count == that.count &&
            units == that.units
        case _ =>
          false

    override def hashCode(): Int =
      var hash = id.value.hashCode
      hash = 31 * hash + origin.hashCode
      hash = 31 * hash + interval.hashCode
      hash = 31 * hash + count
      31 * hash + units.value.hashCode

  final class Explicit private[response] (
      val id: DomainId[TimeAxis],
      private val coordinates: Vector[TimeCoordinate],
      val units: UnitId
  ) extends TimeDomain:
    val count: Int =
      coordinates.length

    def coordinate(index: AxisIndex[TimeAxis]): Either[IndexError, TimeCoordinate] =
      if index.value >= count then Left(IndexError.OutOfBounds(index.value, count))
      else Right(coordinates(index.value))

    def values: Vector[Double] =
      coordinates.map(_.value)

    def rawCoordinateBits: Vector[Long] =
      coordinates.map(_.rawBits)

    override def equals(other: Any): Boolean =
      other match
        case that: Explicit =>
          id == that.id &&
            coordinates == that.coordinates &&
            units == that.units
        case _ =>
          false

    override def hashCode(): Int =
      var hash = id.value.hashCode
      hash = 31 * hash + coordinates.hashCode
      31 * hash + units.value.hashCode

  def regular(
      id: DomainId[TimeAxis],
      origin: Double,
      interval: Double,
      count: Int,
      units: UnitId
  ): Either[SchemaError, TimeDomain] =
    if count <= 0 then Left(SchemaError.InvalidCount("time domain", count))
    else
      for
        checkedOrigin <- TimeCoordinate.finite("time origin", origin)
        checkedInterval <- TimeCoordinate.finite("time interval", interval)
        _ <-
          if checkedInterval.value > 0.0 then Right(())
          else Left(SchemaError.InvalidCoordinate("time interval", interval))
        _ <- TimeCoordinate.finite(
          "last time coordinate",
          checkedOrigin.value + (count - 1).toDouble * checkedInterval.value
        )
      yield new Regular(id, checkedOrigin, checkedInterval, count, units)

  def explicit(
      id: DomainId[TimeAxis],
      values: Vector[Double],
      units: UnitId
  ): Either[SchemaError, TimeDomain] =
    if values.isEmpty then Left(SchemaError.InvalidCount("time domain", 0))
    else
      val coordinates = Vector.newBuilder[TimeCoordinate]
      coordinates.sizeHint(values.length)
      var index = 0
      var previous = Option.empty[Double]
      while index < values.length do
        val value = values(index)
        TimeCoordinate.finite(s"time coordinate $index", value) match
          case Left(error) =>
            return Left(error)
          case Right(coordinate) =>
            previous match
              case Some(found) if value <= found =>
                return Left(SchemaError.NonIncreasingCoordinate(found, value))
              case _ =>
                coordinates += coordinate
                previous = Some(value)
        index += 1
      Right(new Explicit(id, coordinates.result(), units))

opaque type ReferenceNamespace = String

object ReferenceNamespace:
  def fromString(value: String): Either[IdentityError, ReferenceNamespace] =
    ResponseIdentity.validate("reference namespace", value)

  def unsafe(value: String): ReferenceNamespace =
    fromString(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension (namespace: ReferenceNamespace)
    inline def value: String =
      namespace

final class DomainReference private (
    val namespace: ReferenceNamespace,
    val value: String
):
  override def equals(other: Any): Boolean =
    other match
      case that: DomainReference =>
        namespace.value == that.namespace.value && value == that.value
      case _ =>
        false

  override def hashCode(): Int =
    31 * namespace.value.hashCode + value.hashCode

  override def toString: String =
    s"${namespace.value}:$value"

object DomainReference:
  def make(
      namespace: ReferenceNamespace,
      value: String
  ): Either[IdentityError, DomainReference] =
    ResponseIdentity
      .validate("domain reference", value)
      .map(new DomainReference(namespace, _))

  def unsafe(
      namespace: String,
      value: String
  ): DomainReference =
    (for
      checkedNamespace <- ReferenceNamespace.fromString(namespace)
      reference <- make(checkedNamespace, value)
    yield reference) match
      case Left(error) =>
        throw new IllegalArgumentException(error.message)
      case Right(reference) =>
        reference

sealed trait SampleDomainKind

object SampleDomainKind:
  final case class Volume(
      space: DomainReference,
      mask: Option[DomainReference],
      ordering: DomainReference
  ) extends SampleDomainKind

  final case class Surface(
      surface: DomainReference,
      topology: DomainReference,
      ordering: DomainReference
  ) extends SampleDomainKind

final class SampleDomain private (
    val id: DomainId[SampleAxis],
    val count: Int,
    val kind: SampleDomainKind
):
  override def equals(other: Any): Boolean =
    other match
      case that: SampleDomain =>
        id.value == that.id.value && count == that.count && kind == that.kind
      case _ =>
        false

  override def hashCode(): Int =
    31 * (31 * id.value.hashCode + count) + kind.hashCode

object SampleDomain:
  def make(
      id: DomainId[SampleAxis],
      count: Int,
      kind: SampleDomainKind
  ): Either[SchemaError, SampleDomain] =
    if count <= 0 then Left(SchemaError.InvalidCount("sample domain", count))
    else Right(new SampleDomain(id, count, kind))

enum CalibrationState:
  case Applied

enum NonFinitePolicy:
  case Reject
  case Preserve

final case class SignalSchema(
    units: UnitId,
    calibration: CalibrationState,
    nonFinite: NonFinitePolicy
)

final class ResponseSchema private (
    val id: ResponseSchemaId,
    val time: TimeDomain,
    val samples: SampleDomain,
    val signal: SignalSchema
):
  override def equals(other: Any): Boolean =
    other match
      case that: ResponseSchema =>
        id == that.id &&
          time == that.time &&
          samples == that.samples &&
          signal == that.signal
      case _ =>
        false

  override def hashCode(): Int =
    var hash = id.value.hashCode
    hash = 31 * hash + time.hashCode
    hash = 31 * hash + samples.hashCode
    31 * hash + signal.hashCode

object ResponseSchema:
  def make(
      id: ResponseSchemaId,
      time: TimeDomain,
      samples: SampleDomain,
      signal: SignalSchema
  ): Either[SchemaError, ResponseSchema] =
    if time.count <= 0 then Left(SchemaError.InvalidCount("time domain", time.count))
    else if samples.count <= 0 then Left(SchemaError.InvalidCount("sample domain", samples.count))
    else Right(new ResponseSchema(id, time, samples, signal))
