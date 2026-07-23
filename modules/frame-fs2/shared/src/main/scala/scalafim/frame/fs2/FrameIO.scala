package scalafim.frame.fs2

import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scalafim.frame.*

enum PushdownFeature:
  case Projection
  case Predicate
  case Limit
  case BatchSize

final case class SourceCapabilities(
    projection: Boolean,
    predicate: Boolean,
    limit: Boolean,
    batchSize: Boolean,
    streaming: Boolean
):
  def supports(feature: PushdownFeature): Boolean = feature match
    case PushdownFeature.Projection => projection
    case PushdownFeature.Predicate => predicate
    case PushdownFeature.Limit => limit
    case PushdownFeature.BatchSize => batchSize

enum PortablePredicate:
  case Equal(column: String, value: LiteralValue)
  case IsNull(column: String)
  case And(left: PortablePredicate, right: PortablePredicate)
  case Or(left: PortablePredicate, right: PortablePredicate)

final case class ScanRequest(
    columns: Vector[String] = Vector.empty,
    predicate: Option[PortablePredicate] = None,
    limit: Option[Long] = None,
    batchSize: Option[Int] = None
):
  def requestedFeatures: Vector[PushdownFeature] =
    Vector(
      Option.when(columns.nonEmpty)(PushdownFeature.Projection),
      predicate.map(_ => PushdownFeature.Predicate),
      limit.map(_ => PushdownFeature.Limit),
      batchSize.map(_ => PushdownFeature.BatchSize)
    ).flatten

final case class PushdownReceipt(
    requested: Vector[PushdownFeature],
    accepted: Vector[PushdownFeature],
    residual: Vector[PushdownFeature],
    columnsRead: Vector[String]
)

enum SourceError:
  case InvalidRequest(detail: String)
  case MissingColumn(name: String)
  case SchemaMismatch(detail: String)
  case Decode(row: Int, column: Int, value: String, expected: DataType)
  case MalformedCsv(row: Int, detail: String)
  case Storage(error: StorageError)
  case Open(detail: String)
  case Close(detail: String)

  def message: String = this match
    case InvalidRequest(value) => value
    case MissingColumn(name) => s"source column '$name' does not exist"
    case SchemaMismatch(value) => value
    case Decode(row, column, value, expected) =>
      s"row $row column $column value '$value' cannot be decoded as $expected"
    case MalformedCsv(row, value) => s"malformed CSV row $row: $value"
    case Storage(error) => error.message
    case Open(value) => s"source open failed: $value"
    case Close(value) => s"source close failed: $value"

final case class SourceFailure(error: SourceError)
    extends RuntimeException(error.message)

final case class SourceInspection(
    schema: Schema,
    capabilities: SourceCapabilities
)

final case class PlannedScan[F[_]](
    schema: Schema,
    receipt: PushdownReceipt,
    batches: Stream[F, RecordBatch]
)

trait FrameSource[F[_]]:
  def inspect: F[Either[SourceError, SourceInspection]]
  def plan(request: ScanRequest): F[Either[SourceError, PlannedScan[F]]]
  def close: F[Either[SourceError, Unit]]

enum SinkError:
  case SchemaMismatch(expected: Schema, actual: Schema)
  case Storage(error: StorageError)
  case Encode(row: Long, column: Int, value: ScalarValue)
  case Write(detail: String)
  case Close(detail: String)

  def message: String = this match
    case SchemaMismatch(expected, actual) =>
      s"sink expected $expected but received $actual"
    case Storage(error) => error.message
    case Encode(row, column, value) =>
      s"cannot encode row $row column $column value $value"
    case Write(value) => s"sink write failed: $value"
    case Close(value) => s"sink close failed: $value"

final case class SinkReceipt(
    rows: Long,
    batches: Long,
    bytes: Long
)

trait FrameSink[F[_], Result]:
  def write(schema: Schema, batches: Stream[F, RecordBatch]): F[Either[SinkError, Result]]

final class InMemoryFrameSource[F[_]] private (
    schema: Schema,
    sourceBatches: Vector[RecordBatch],
    ownsBatches: Boolean
)(using F: Async[F]) extends FrameSource[F]:
  private var closed = false

  val capabilities: SourceCapabilities =
    SourceCapabilities(
      projection = true,
      predicate = false,
      limit = true,
      batchSize = false,
      streaming = true
    )

  def inspect: F[Either[SourceError, SourceInspection]] =
    F.delay:
      synchronized:
        if closed then Left(SourceError.Open("source is closed"))
        else Right(SourceInspection(schema, capabilities))

  def plan(request: ScanRequest): F[Either[SourceError, PlannedScan[F]]] =
    F.delay:
      synchronized:
        if closed then Left(SourceError.Open("source is closed"))
        else
          validate(request).map: selection =>
            val requested = request.requestedFeatures
            val accepted = requested.filter(capabilities.supports)
            val residual = requested.filterNot(capabilities.supports)
            val selectedFields = selection.map(schema.fields)
            val output = Schema.unsafe(selectedFields)
            PlannedScan(
              output,
              PushdownReceipt(
                requested,
                accepted,
                residual,
                selectedFields.map(_.name)
              ),
              scan(selection, output, request.limit)
            )

  def close: F[Either[SourceError, Unit]] =
    F.delay:
      synchronized:
        if !closed then
          closed = true
          if ownsBatches then sourceBatches.foreach(_.close())
        Right(())

  private def validate(request: ScanRequest): Either[SourceError, Vector[Int]] =
    if request.limit.exists(_ < 0) then
      Left(SourceError.InvalidRequest("scan limit must be non-negative"))
    else if request.batchSize.exists(_ <= 0) then
      Left(SourceError.InvalidRequest("scan batch size must be positive"))
    else
      val names =
        if request.columns.isEmpty then schema.fields.map(_.name)
        else request.columns
      names.foldLeft[Either[SourceError, Vector[Int]]](Right(Vector.empty)):
        case (result, name) =>
          result.flatMap: indexes =>
            val index = schema.fields.indexWhere(_.name == name)
            if index < 0 then Left(SourceError.MissingColumn(name))
            else Right(indexes :+ index)

  private def scan(
      selection: Vector[Int],
      output: Schema,
      limit: Option[Long]
  ): Stream[F, RecordBatch] =
    def loop(index: Int, remaining: Long): Stream[F, RecordBatch] =
      if index >= sourceBatches.length || remaining == 0L then Stream.empty
      else
        val source = sourceBatches(index)
        val count = math.min(source.rowCount.toLong, remaining).toInt
        Stream
          .bracket(
            F.fromEither(
              project(source, selection, output, count)
                .leftMap(SourceFailure.apply)
            )
          )(batch => F.delay(batch.close()))
          .flatMap(Stream.emit) ++
          loop(index + 1, remaining - count.toLong)
    loop(0, limit.getOrElse(Long.MaxValue))

  private def project(
      source: RecordBatch,
      selection: Vector[Int],
      output: Schema,
      count: Int
  ): Either[SourceError, RecordBatch] =
    val columns = ArrayBuffer.empty[ColumnArray]
    var index = 0
    var error: Option[SourceError] = None
    while index < selection.length && error.isEmpty do
      source.columns(selection(index)).slice(0, count) match
        case Right(column) => columns += column
        case Left(value) => error = Some(SourceError.Storage(value))
      index += 1
    error match
      case Some(value) =>
        columns.foreach(_.close())
        Left(value)
      case None =>
        RecordBatch(output, columns.toVector)
          .leftMap(SourceError.Storage.apply)

object InMemoryFrameSource:
  def apply[F[_]: Async](schema: Schema, batches: Vector[RecordBatch]): InMemoryFrameSource[F] =
    new InMemoryFrameSource(schema, batches, ownsBatches = false)

  private[fs2] def owned[F[_]: Async](
      schema: Schema,
      batches: Vector[RecordBatch]
  ): InMemoryFrameSource[F] =
    new InMemoryFrameSource(schema, batches, ownsBatches = true)

enum CsvCoercion:
  case Strict
  case TrimWhitespace

final case class CsvReadOptions(
    schema: Schema,
    delimiter: Char = ',',
    header: Boolean = true,
    nullTokens: Set[String] = Set("", "null"),
    coercion: CsvCoercion = CsvCoercion.Strict,
    batchSize: Int = 1024
)

final class CsvFrameSource[F[_]] private (
    delegate: InMemoryFrameSource[F]
) extends FrameSource[F]:
  def inspect: F[Either[SourceError, SourceInspection]] = delegate.inspect
  def plan(request: ScanRequest): F[Either[SourceError, PlannedScan[F]]] =
    delegate.plan(request)
  def close: F[Either[SourceError, Unit]] = delegate.close

object CsvFrameSource:
  def fromString[F[_]: Async](
      input: String,
      options: CsvReadOptions
  ): Either[SourceError, CsvFrameSource[F]] =
    CsvCodec.decode(input, options).map: batches =>
      new CsvFrameSource(InMemoryFrameSource.owned(options.schema, batches))

final case class CsvWriteResult(
    text: String,
    receipt: SinkReceipt
)

final class CsvFrameSink[F[_]](
    delimiter: Char = ',',
    includeHeader: Boolean = true,
    nullValue: String = ""
)(using F: Async[F]) extends FrameSink[F, CsvWriteResult]:
  def write(
      schema: Schema,
      batches: Stream[F, RecordBatch]
  ): F[Either[SinkError, CsvWriteResult]] =
    batches
      .evalMap: batch =>
        F.delay(CsvCodec.encode(schema, Vector(batch), delimiter, includeHeader = false, nullValue))
      .compile
      .toVector
      .map: encoded =>
        encoded.foldLeft[Either[SinkError, Vector[CsvWriteResult]]](Right(Vector.empty)):
          case (result, value) => result.flatMap(values => value.map(values :+ _))
      .map:
        _.map: parts =>
          val header =
            if includeHeader then CsvCodec.header(schema, delimiter)
            else ""
          val text = header + parts.map(_.text).mkString
          CsvWriteResult(
            text,
            SinkReceipt(
              parts.map(_.receipt.rows).sum,
              parts.map(_.receipt.batches).sum,
              text.length.toLong
            )
          )

trait ArrowIpcPlatform[F[_]] extends FrameSource[F]:
  def write(
      schema: Schema,
      batches: Stream[F, RecordBatch]
  ): F[Either[SinkError, SinkReceipt]]

private object CsvCodec:
  def header(schema: Schema, delimiter: Char): String =
    schema.fields.map(field => quote(field.name, delimiter)).mkString(delimiter.toString) + "\n"

  def decode(
      input: String,
      options: CsvReadOptions
  ): Either[SourceError, Vector[RecordBatch]] =
    if options.batchSize <= 0 then
      Left(SourceError.InvalidRequest("CSV batch size must be positive"))
    else
      parse(input, options.delimiter).flatMap: parsed =>
        val data =
          if options.header then
            parsed.headOption match
              case None => Left(SourceError.MalformedCsv(1, "missing header"))
              case Some(header) =>
                val expected = options.schema.fields.map(_.name)
                if header != expected then
                  Left(
                    SourceError.SchemaMismatch(
                      s"CSV header ${header.mkString(",")} does not match ${expected.mkString(",")}"
                    )
                  )
                else Right(parsed.tail)
          else Right(parsed)
        data.flatMap:
          _.zipWithIndex
            .grouped(options.batchSize)
            .foldLeft[Either[SourceError, Vector[RecordBatch]]](Right(Vector.empty)):
              case (result, chunk) =>
                result.flatMap: batches =>
                  buildBatch(options, chunk.toVector).map(batches :+ _)

  private def parse(input: String, delimiter: Char): Either[SourceError, Vector[Vector[String]]] =
    val rows = ArrayBuffer.empty[Vector[String]]
    val row = ArrayBuffer.empty[String]
    val field = new StringBuilder
    var quoted = false
    var index = 0
    var rowNumber = 1
    while index < input.length do
      val current = input.charAt(index)
      if quoted then
        if current == '"' && index + 1 < input.length && input.charAt(index + 1) == '"' then
          field.append('"')
          index += 1
        else if current == '"' then quoted = false
        else field.append(current)
      else
        current match
          case '"' if field.isEmpty => quoted = true
          case value if value == delimiter =>
            row += field.result()
            field.clear()
          case '\n' =>
            row += field.result()
            field.clear()
            rows += row.toVector
            row.clear()
            rowNumber += 1
          case '\r' => ()
          case other => field.append(other)
      index += 1
    if quoted then Left(SourceError.MalformedCsv(rowNumber, "unterminated quoted field"))
    else
      if field.nonEmpty || row.nonEmpty then
        row += field.result()
        rows += row.toVector
      Right(rows.toVector)

  private def buildBatch(
      options: CsvReadOptions,
      rows: Vector[(Vector[String], Int)]
  ): Either[SourceError, RecordBatch] =
    rows.find(_._1.length != options.schema.size) match
      case Some((values, row)) =>
        Left(
          SourceError.MalformedCsv(
            row + 2,
            s"expected ${options.schema.size} fields but found ${values.length}"
          )
        )
      case None =>
        val columns = options.schema.fields.zipWithIndex.map: (field, column) =>
          val values = rows.map: (row, index) =>
            val raw = row(column)
            val value =
              if options.coercion == CsvCoercion.TrimWhitespace then raw.trim
              else raw
            (value, index + 2)
          decodeColumn(field, column, values, options.nullTokens)
        sequence(columns).flatMap: decoded =>
          RecordBatch(options.schema, decoded).leftMap(SourceError.Storage.apply)

  private def decodeColumn(
      field: Field,
      column: Int,
      values: Vector[(String, Int)],
      nullTokens: Set[String]
  ): Either[SourceError, ColumnArray] =
    val valid = values.map((value, _) => !nullTokens.contains(value)).toArray
    def decode[A: ClassTag](
        expected: DataType
    )(parser: String => Option[A]): Either[SourceError, Array[A]] =
      val output = new Array[A](values.length)
      var index = 0
      var error: Option[SourceError] = None
      while index < values.length && error.isEmpty do
        val (value, row) = values(index)
        if valid(index) then
          parser(value) match
            case Some(decoded) => output(index) = decoded
            case None => error = Some(SourceError.Decode(row, column + 1, value, expected))
        index += 1
      error match
        case Some(value) => Left(value)
        case None => Right(output)

    field.dataType match
      case DataType.Bool =>
        decode(DataType.Bool):
          case "true" => Some(true)
          case "false" => Some(false)
          case _ => None
        .flatMap(values => ColumnArray.bool(values, valid).leftMap(SourceError.Storage.apply))
      case DataType.Int32 =>
        decode(DataType.Int32)(_.toIntOption)
          .flatMap(values => ColumnArray.int32(values, valid).leftMap(SourceError.Storage.apply))
      case DataType.Int64 =>
        decode(DataType.Int64)(_.toLongOption)
          .flatMap(values => ColumnArray.int64(values, valid).leftMap(SourceError.Storage.apply))
      case DataType.Float32 =>
        decode(DataType.Float32)(_.toFloatOption)
          .flatMap(values => ColumnArray.float32(values, valid).leftMap(SourceError.Storage.apply))
      case DataType.Float64 =>
        decode(DataType.Float64)(_.toDoubleOption)
          .flatMap(values => ColumnArray.float64(values, valid).leftMap(SourceError.Storage.apply))
      case DataType.Utf8 =>
        val decoded = values.map(_._1).toArray
        ColumnArray.utf8(decoded, valid).leftMap(SourceError.Storage.apply)
      case DataType.Timestamp(unit) =>
        decode(DataType.Timestamp(unit))(_.toLongOption)
          .flatMap(values =>
            ColumnArray.timestamp(values, unit, valid).leftMap(SourceError.Storage.apply)
          )

  def encode(
      schema: Schema,
      batches: Vector[RecordBatch],
      delimiter: Char,
      includeHeader: Boolean,
      nullValue: String
  ): Either[SinkError, CsvWriteResult] =
    batches.find(_.schema != schema) match
      case Some(batch) => Left(SinkError.SchemaMismatch(schema, batch.schema))
      case None =>
        val output = new StringBuilder
        if includeHeader then
          output.append(header(schema, delimiter))
        var rows = 0L
        var batchIndex = 0
        var error: Option[SinkError] = None
        while batchIndex < batches.length && error.isEmpty do
          val batch = batches(batchIndex)
          var row = 0
          while row < batch.rowCount && error.isEmpty do
            val encoded = new Array[String](schema.size)
            var column = 0
            while column < schema.size && error.isEmpty do
              batch.columns(column).scalar(row) match
                case Right(ScalarValue.Null) => encoded(column) = nullValue
                case Right(value) => encoded(column) = quote(render(value), delimiter)
                case Left(value) => error = Some(SinkError.Storage(value))
              column += 1
            if error.isEmpty then
              output.append(encoded.mkString(delimiter.toString))
              output.append('\n')
              rows += 1
            row += 1
          batchIndex += 1
        error match
          case Some(value) => Left(value)
          case None =>
            val text = output.result()
            Right(CsvWriteResult(text, SinkReceipt(rows, batches.length.toLong, text.length.toLong)))

  private def render(value: ScalarValue): String = value match
    case ScalarValue.Null => ""
    case ScalarValue.Bool(actual) => actual.toString
    case ScalarValue.Int32(actual) => actual.toString
    case ScalarValue.Int64(actual) => actual.toString
    case ScalarValue.Float32(actual) => actual.toString
    case ScalarValue.Float64(actual) => actual.toString
    case ScalarValue.Utf8(actual) => actual
    case ScalarValue.Timestamp(actual, _) => actual.toString

  private def quote(value: String, delimiter: Char): String =
    if value.exists(character =>
        character == delimiter || character == '"' || character == '\n' || character == '\r'
      )
    then s"\"${value.replace("\"", "\"\"")}\""
    else value

  private def sequence[A](values: Vector[Either[SourceError, A]]): Either[SourceError, Vector[A]] =
    values.foldLeft[Either[SourceError, Vector[A]]](Right(Vector.empty)):
      case (result, value) => result.flatMap(current => value.map(current :+ _))
