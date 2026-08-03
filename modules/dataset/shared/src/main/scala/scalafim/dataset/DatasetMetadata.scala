package scalafim.dataset

import scalafim.fmri.hrf.{NonNegativeSeconds, Seconds}

trait DatasetProvenance:
  def source: String

enum DatasetValue:
  case Text(value: String)
  case Number(value: Double, source: Option[String] = None)
  case Integer(value: Int, source: Option[String] = None)
  case Bool(value: Boolean, source: Option[String] = None)

  def asString: String =
    this match
      case Text(value) =>
        value
      case Number(value, source) =>
        source.getOrElse(value.toString)
      case Integer(value, source) =>
        source.getOrElse(value.toString)
      case Bool(value, source) =>
        source.getOrElse(value.toString)

  def asDouble: Option[Double] =
    this match
      case Number(value, _)  => Some(value)
      case Integer(value, _) => Some(value.toDouble)
      case _                 => None

  def asInt: Option[Int] =
    this match
      case Integer(value, _) => Some(value)
      case _                 => None

  def asBoolean: Option[Boolean] =
    this match
      case Bool(value, _) => Some(value)
      case _              => None

object DatasetValue:
  def fromString(value: String): DatasetValue =
    val trimmed = value.trim
    parseInt(trimmed) match
      case Some(parsed) =>
        DatasetValue.Integer(parsed, Some(value))
      case None =>
        parseDouble(trimmed) match
          case Some(parsed) =>
            DatasetValue.Number(parsed, Some(value))
          case None =>
            parseBoolean(trimmed) match
              case Some(parsed) => DatasetValue.Bool(parsed, Some(value))
              case None         => DatasetValue.Text(value)

  private def parseInt(value: String): Option[Int] =
    try Some(value.toInt)
    catch case _: NumberFormatException => None

  private def parseDouble(value: String): Option[Double] =
    try
      val parsed = value.toDouble
      if parsed.isFinite then Some(parsed) else None
    catch case _: NumberFormatException => None

  private def parseBoolean(value: String): Option[Boolean] =
    value.toLowerCase match
      case "true" | "t"  => Some(true)
      case "false" | "f" => Some(false)
      case _             => None

final class DatasetMetadata private (
    val typedValues: Map[DatasetFieldId, DatasetValue],
    val provenance: Option[DatasetProvenance]
):
  def values: Map[String, String] =
    typedValues.iterator.map { case (key, value) => key.value -> value.asString }.toMap

  def get(key: String): Option[String] =
    DatasetFieldId.make(key).toOption.flatMap(typedValues.get).map(_.asString)

  def getValue(key: DatasetFieldId): Option[DatasetValue] =
    typedValues.get(key)

  def updated(key: String, value: String): DatasetMetadata =
    DatasetMetadata
      .fromStrings(values.updated(key, value), provenance)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def updatedValue(key: DatasetFieldId, value: DatasetValue): DatasetMetadata =
    DatasetMetadata
      .fromValues(typedValues.updated(key, value), provenance)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  def withProvenance(value: DatasetProvenance): DatasetMetadata =
    new DatasetMetadata(typedValues, Some(value))

  override def equals(other: Any): Boolean =
    other match
      case that: DatasetMetadata =>
        typedValues == that.typedValues && provenance == that.provenance
      case _                     => false

  override def hashCode(): Int =
    31 * typedValues.hashCode() + provenance.hashCode()

  override def toString: String =
    s"DatasetMetadata(${values.toString}, provenance=${provenance.map(_.source)})"

object DatasetMetadata:
  val Empty: DatasetMetadata = new DatasetMetadata(Map.empty, None)

  def apply(values: Map[String, String] = Map.empty): DatasetMetadata =
    fromStrings(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromStrings(
      values: Map[String, String],
      provenance: Option[DatasetProvenance] = None
  ): Either[DatasetError, DatasetMetadata] =
    parseStringMap(values).map(new DatasetMetadata(_, provenance))

  def fromValues(
      values: Map[DatasetFieldId, DatasetValue],
      provenance: Option[DatasetProvenance] = None
  ): Either[DatasetError, DatasetMetadata] =
    Right(new DatasetMetadata(values, provenance))

private object DatasetEventFields:
  val Onset: DatasetFieldId = DatasetFieldId.unsafe("onset")
  val Duration: DatasetFieldId = DatasetFieldId.unsafe("duration")
  val Run: DatasetFieldId = DatasetFieldId.unsafe("run")
  val Session: DatasetFieldId = DatasetFieldId.unsafe("session")
  val Condition: DatasetFieldId = DatasetFieldId.unsafe("condition")

final case class DatasetEventRow private (
    values: Map[DatasetFieldId, DatasetValue],
    onset: Option[Seconds],
    duration: Option[NonNegativeSeconds],
    run: Option[RunId],
    session: Option[SessionId],
    condition: Option[ConditionLabel]
):
  require(values.nonEmpty, "dataset event row must contain at least one field")

  def fields: Set[DatasetFieldId] =
    values.keySet

  def get(field: DatasetFieldId): Option[DatasetValue] =
    values.get(field)

  def contains(field: DatasetFieldId): Boolean =
    values.contains(field)

  def stringValues: Map[String, String] =
    values.iterator.map { case (field, value) => field.value -> value.asString }.toMap

object DatasetEventRow:
  def fromStrings(values: Map[String, String]): Either[DatasetError, DatasetEventRow] =
    parseStringMap(values).flatMap(fromValues)

  def fromValues(values: Map[DatasetFieldId, DatasetValue]): Either[DatasetError, DatasetEventRow] =
    if values.isEmpty then Left(DatasetError.InvalidEventRow(0, "event row must contain at least one field"))
    else
      for
        onset <- optionalSeconds(values, DatasetEventFields.Onset)
        duration <- optionalNonNegativeSeconds(values, DatasetEventFields.Duration)
        run <- optionalRun(values)
        session <- optionalSession(values)
        condition <- optionalCondition(values)
      yield new DatasetEventRow(values, onset, duration, run, session, condition)

  def unsafe(values: Map[DatasetFieldId, DatasetValue]): DatasetEventRow =
    fromValues(values).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def optionalSeconds(
      values: Map[DatasetFieldId, DatasetValue],
      field: DatasetFieldId
  ): Either[DatasetError, Option[Seconds]] =
    values.get(field) match
      case None => Right(None)
      case Some(value) =>
        value.asDouble match
          case None =>
            Left(DatasetError.InvalidDatasetValue(field.value, value.asString, "must be numeric seconds"))
          case Some(raw) =>
            Seconds.fromDouble(raw, field.value).left.map(error => DatasetError.InvalidDatasetValue(field.value, value.asString, error.message)).map(Some(_))

  private def optionalNonNegativeSeconds(
      values: Map[DatasetFieldId, DatasetValue],
      field: DatasetFieldId
  ): Either[DatasetError, Option[NonNegativeSeconds]] =
    values.get(field) match
      case None => Right(None)
      case Some(value) =>
        value.asDouble match
          case None =>
            Left(DatasetError.InvalidDatasetValue(field.value, value.asString, "must be numeric seconds"))
          case Some(raw) =>
            NonNegativeSeconds(raw, field.value).left.map(error => DatasetError.InvalidDatasetValue(field.value, value.asString, error.message)).map(Some(_))

  private def optionalRun(values: Map[DatasetFieldId, DatasetValue]): Either[DatasetError, Option[RunId]] =
    optionalLabel(values, DatasetEventFields.Run)(RunId.make)

  private def optionalSession(values: Map[DatasetFieldId, DatasetValue]): Either[DatasetError, Option[SessionId]] =
    optionalLabel(values, DatasetEventFields.Session)(SessionId.make)

  private def optionalCondition(values: Map[DatasetFieldId, DatasetValue]): Either[DatasetError, Option[ConditionLabel]] =
    optionalLabel(values, DatasetEventFields.Condition)(ConditionLabel.make)

  private def optionalLabel[A](
      values: Map[DatasetFieldId, DatasetValue],
      field: DatasetFieldId
  )(make: String => Either[DatasetError, A]): Either[DatasetError, Option[A]] =
    values.get(field) match
      case None => Right(None)
      case Some(value) =>
        make(value.asString).left.map(error => DatasetError.InvalidDatasetValue(field.value, value.asString, error.message)).map(Some(_))

final class DatasetEvents private (val typedRows: Vector[DatasetEventRow]):
  def rows: Vector[Map[String, String]] =
    typedRows.map(_.stringValues)

  def nrows: Int =
    typedRows.length

  def isEmpty: Boolean =
    typedRows.isEmpty

  def columns: Vector[DatasetFieldId] =
    if typedRows.isEmpty then Vector.empty
    else typedRows.head.values.keys.toVector.sortBy(_.value)

  def column(field: DatasetFieldId): Vector[DatasetValue] =
    columnEither(field).fold(error => throw new IllegalArgumentException(error.message), identity)

  def columnEither(field: DatasetFieldId): Either[DatasetError, Vector[DatasetValue]] =
    if columns.contains(field) then Right(typedRows.map(_.values(field)))
    else Left(DatasetError.DatasetColumnNotFound(field.value))

  def validateAgainst(timeAxis: DatasetTimeAxis): Either[DatasetError, Unit] =
    val validRuns = timeAxis.runIds.map(_.value).toSet
    var row = 0
    while row < typedRows.length do
      typedRows(row).run match
        case Some(run) if !validRuns.contains(run.value) =>
          return Left(DatasetError.InvalidEventRow(row, s"run '${run.value}' is not present in the dataset time axis"))
        case _ =>
      row += 1
    Right(())

  def alignedTo(timeAxis: DatasetTimeAxis): Either[DatasetError, DatasetEvents] =
    validateAgainst(timeAxis).map(_ => this)

  override def equals(other: Any): Boolean =
    other match
      case that: DatasetEvents => typedRows == that.typedRows
      case _                   => false

  override def hashCode(): Int =
    typedRows.hashCode()

  override def toString: String =
    s"DatasetEvents(${rows.toString})"

object DatasetEvents:
  val Empty: DatasetEvents = new DatasetEvents(Vector.empty)

  def apply(rows: Vector[Map[String, String]] = Vector.empty): DatasetEvents =
    fromRows(rows).fold(error => throw new IllegalArgumentException(error.message), identity)

  def fromRows(rows: Vector[Map[String, String]]): Either[DatasetError, DatasetEvents] =
    val typed = Vector.newBuilder[DatasetEventRow]
    typed.sizeHint(rows.length)
    var row = 0
    var error = Option.empty[DatasetError]
    while row < rows.length && error.isEmpty do
      DatasetEventRow.fromStrings(rows(row)) match
        case Left(DatasetError.InvalidEventRow(_, detail)) => error = Some(DatasetError.InvalidEventRow(row, detail))
        case Left(err) => error = Some(DatasetError.InvalidEventRow(row, err.message))
        case Right(value) => typed += value
      row += 1
    error match
      case Some(err) => Left(err)
      case None      => fromTypedRows(typed.result())

  def fromTypedRows(rows: Vector[DatasetEventRow]): Either[DatasetError, DatasetEvents] =
    validateRows(rows).map(_ => new DatasetEvents(rows))

  def unsafeTyped(rows: Vector[DatasetEventRow]): DatasetEvents =
    fromTypedRows(rows).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validateRows(rows: Vector[DatasetEventRow]): Either[DatasetError, Unit] =
    if rows.isEmpty then Right(())
    else
      val expected = rows.head.fields
      var row = 0
      while row < rows.length do
        val actual = rows(row).fields
        if actual != expected then
          val actualLabel = actual.toVector.map(_.value).sorted.mkString(", ")
          val expectedLabel = expected.toVector.map(_.value).sorted.mkString(", ")
          return Left(DatasetError.InvalidEventRow(row, s"columns $actualLabel; expected $expectedLabel"))
        row += 1
      Right(())

private def parseStringMap(values: Map[String, String]): Either[DatasetError, Map[DatasetFieldId, DatasetValue]] =
  val out = Map.newBuilder[DatasetFieldId, DatasetValue]
  val iterator = values.iterator
  var error = Option.empty[DatasetError]
  while iterator.hasNext && error.isEmpty do
    val (key, value) = iterator.next()
    DatasetFieldId.make(key) match
      case Left(err) =>
        error = Some(err)
      case Right(field) =>
        out += field -> DatasetValue.fromString(value)
  error match
    case Some(err) => Left(err)
    case None      => Right(out.result())
