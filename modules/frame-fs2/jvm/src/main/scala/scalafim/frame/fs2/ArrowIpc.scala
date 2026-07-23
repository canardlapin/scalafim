package scalafim.frame.fs2

import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets
import java.util.Collections
import org.apache.arrow.memory.RootAllocator
import org.apache.arrow.vector.*
import org.apache.arrow.vector.ipc.ArrowStreamReader
import org.apache.arrow.vector.ipc.ArrowStreamWriter
import org.apache.arrow.vector.types.FloatingPointPrecision
import org.apache.arrow.vector.types.TimeUnit as ArrowTimeUnit
import org.apache.arrow.vector.types.pojo.ArrowType
import org.apache.arrow.vector.types.pojo.Field as ArrowField
import org.apache.arrow.vector.types.pojo.FieldType as ArrowFieldType
import org.apache.arrow.vector.types.pojo.Schema as ArrowSchema
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*
import scalafim.frame.*

final class ArrowIpcFrameSource[F[_]] private (
    delegate: InMemoryFrameSource[F]
)(using F: Async[F]) extends ArrowIpcPlatform[F]:
  def inspect: F[Either[SourceError, SourceInspection]] = delegate.inspect

  def plan(request: ScanRequest): F[Either[SourceError, PlannedScan[F]]] =
    delegate.plan(request)

  def close: F[Either[SourceError, Unit]] = delegate.close

  def write(
      schema: Schema,
      batches: Stream[F, RecordBatch]
  ): F[Either[SinkError, SinkReceipt]] =
    new ArrowIpcFrameSink[F]().write(schema, batches).map(_.map(_.receipt))

object ArrowIpcFrameSource:
  def fromBytes[F[_]: Async](
      bytes: Array[Byte]
  ): Either[SourceError, ArrowIpcFrameSource[F]] =
    ArrowIpcCodec.decode(bytes).map: (schema, batches) =>
      new ArrowIpcFrameSource(InMemoryFrameSource.owned(schema, batches))

final case class ArrowIpcWriteResult(
    bytes: Array[Byte],
    receipt: SinkReceipt
)

final class ArrowIpcFrameSink[F[_]](using F: Async[F])
    extends FrameSink[F, ArrowIpcWriteResult]:
  def write(
      schema: Schema,
      batches: Stream[F, RecordBatch]
  ): F[Either[SinkError, ArrowIpcWriteResult]] =
    batches
      .evalMap(batch => F.delay(batch.slice(0, batch.rowCount)))
      .compile
      .toVector
      .map: retained =>
        val valuesToClose = retained.collect:
          case Right(value) => value
        val copies = retained.foldLeft[Either[SinkError, Vector[RecordBatch]]](Right(Vector.empty)):
          case (result, value) =>
            result.flatMap(current =>
              value.leftMap(SinkError.Storage.apply).map(current :+ _)
            )
        try copies.flatMap(ArrowIpcCodec.encode(schema, _))
        finally valuesToClose.foreach(_.close())

private object ArrowIpcCodec:
  def encode(
      schema: Schema,
      batches: Vector[RecordBatch]
  ): Either[SinkError, ArrowIpcWriteResult] =
    batches.find(_.schema != schema) match
      case Some(batch) => Left(SinkError.SchemaMismatch(schema, batch.schema))
      case None =>
        val allocator = new RootAllocator()
        val output = new ByteArrayOutputStream
        val root = VectorSchemaRoot.create(arrowSchema(schema), allocator)
        val writer = new ArrowStreamWriter(root, null, Channels.newChannel(output))
        try
          writer.start()
          var batchIndex = 0
          var rows = 0L
          while batchIndex < batches.length do
            val batch = batches(batchIndex)
            root.allocateNew()
            root.setRowCount(batch.rowCount)
            var column = 0
            while column < schema.size do
              val vector = root.getVector(column)
              var row = 0
              while row < batch.rowCount do
                batch.columns(column).scalar(row) match
                  case Right(value) => set(vector, row, value)
                  case Left(error) => return Left(SinkError.Storage(error))
                row += 1
              vector.setValueCount(batch.rowCount)
              column += 1
            writer.writeBatch()
            rows += batch.rowCount.toLong
            root.clear()
            batchIndex += 1
          writer.end()
          val bytes = output.toByteArray
          Right(
            ArrowIpcWriteResult(
              bytes,
              SinkReceipt(rows, batches.length.toLong, bytes.length.toLong)
            )
          )
        catch
          case error: Throwable => Left(SinkError.Write(error.getMessage))
        finally
          writer.close()
          root.close()
          allocator.close()

  def decode(bytes: Array[Byte]): Either[SourceError, (Schema, Vector[RecordBatch])] =
    val allocator = new RootAllocator()
    val reader = new ArrowStreamReader(new ByteArrayInputStream(bytes), allocator)
    try
      val root = reader.getVectorSchemaRoot
      frameSchema(root.getSchema).flatMap: schema =>
        val batches = ArrayBuffer.empty[RecordBatch]
        var error: Option[SourceError] = None
        while reader.loadNextBatch() && error.isEmpty do
          decodeBatch(schema, root) match
            case Right(batch) => batches += batch
            case Left(value) => error = Some(value)
        error match
          case Some(value) =>
            batches.foreach(_.close())
            Left(value)
          case None => Right((schema, batches.toVector))
    catch
      case error: Throwable => Left(SourceError.Open(error.getMessage))
    finally
      reader.close()
      allocator.close()

  private def arrowSchema(schema: Schema): ArrowSchema =
    new ArrowSchema(
      schema.fields.map: field =>
        new ArrowField(
          field.name,
          new ArrowFieldType(field.nullable, arrowType(field.dataType), null, null),
          Collections.emptyList()
        )
      .asJava
    )

  private def arrowType(dataType: DataType): ArrowType = dataType match
    case DataType.Bool => ArrowType.Bool.INSTANCE
    case DataType.Int32 => new ArrowType.Int(32, true)
    case DataType.Int64 => new ArrowType.Int(64, true)
    case DataType.Float32 => new ArrowType.FloatingPoint(FloatingPointPrecision.SINGLE)
    case DataType.Float64 => new ArrowType.FloatingPoint(FloatingPointPrecision.DOUBLE)
    case DataType.Utf8 => ArrowType.Utf8.INSTANCE
    case DataType.Timestamp(unit) =>
      new ArrowType.Timestamp(arrowTimeUnit(unit), null)

  private def arrowTimeUnit(unit: TimeUnit): ArrowTimeUnit = unit match
    case TimeUnit.Second => ArrowTimeUnit.SECOND
    case TimeUnit.Millisecond => ArrowTimeUnit.MILLISECOND
    case TimeUnit.Microsecond => ArrowTimeUnit.MICROSECOND
    case TimeUnit.Nanosecond => ArrowTimeUnit.NANOSECOND

  private def frameSchema(schema: ArrowSchema): Either[SourceError, Schema] =
    val fields = schema.getFields.asScala.toVector.map: field =>
      frameType(field.getType).map: dataType =>
        DynamicFrame.field(field.getName, dataType, field.isNullable)
    fields
      .foldLeft[Either[SourceError, Vector[Field]]](Right(Vector.empty)):
        case (result, value) => result.flatMap(current => value.map(current :+ _))
      .flatMap(values => Schema(values).leftMap(error => SourceError.SchemaMismatch(error.message)))

  private def frameType(dataType: ArrowType): Either[SourceError, DataType] = dataType match
    case _: ArrowType.Bool => Right(DataType.Bool)
    case value: ArrowType.Int if value.getBitWidth == 32 && value.getIsSigned =>
      Right(DataType.Int32)
    case value: ArrowType.Int if value.getBitWidth == 64 && value.getIsSigned =>
      Right(DataType.Int64)
    case value: ArrowType.FloatingPoint
        if value.getPrecision == FloatingPointPrecision.SINGLE =>
      Right(DataType.Float32)
    case value: ArrowType.FloatingPoint
        if value.getPrecision == FloatingPointPrecision.DOUBLE =>
      Right(DataType.Float64)
    case _: ArrowType.Utf8 => Right(DataType.Utf8)
    case value: ArrowType.Timestamp if value.getTimezone == null =>
      value.getUnit match
        case ArrowTimeUnit.SECOND => Right(DataType.Timestamp(TimeUnit.Second))
        case ArrowTimeUnit.MILLISECOND => Right(DataType.Timestamp(TimeUnit.Millisecond))
        case ArrowTimeUnit.MICROSECOND => Right(DataType.Timestamp(TimeUnit.Microsecond))
        case ArrowTimeUnit.NANOSECOND => Right(DataType.Timestamp(TimeUnit.Nanosecond))
    case other =>
      Left(SourceError.SchemaMismatch(s"unsupported Arrow IPC type $other"))

  private def set(vector: FieldVector, row: Int, value: ScalarValue): Unit =
    (vector, value) match
      case (current: BitVector, ScalarValue.Bool(actual)) =>
        current.setSafe(row, if actual then 1 else 0)
      case (current: IntVector, ScalarValue.Int32(actual)) => current.setSafe(row, actual)
      case (current: BigIntVector, ScalarValue.Int64(actual)) => current.setSafe(row, actual)
      case (current: Float4Vector, ScalarValue.Float32(actual)) => current.setSafe(row, actual)
      case (current: Float8Vector, ScalarValue.Float64(actual)) => current.setSafe(row, actual)
      case (current: VarCharVector, ScalarValue.Utf8(actual)) =>
        current.setSafe(row, actual.getBytes(StandardCharsets.UTF_8))
      case (current: TimeStampSecVector, ScalarValue.Timestamp(actual, TimeUnit.Second)) =>
        current.setSafe(row, actual)
      case (current: TimeStampMilliVector, ScalarValue.Timestamp(actual, TimeUnit.Millisecond)) =>
        current.setSafe(row, actual)
      case (current: TimeStampMicroVector, ScalarValue.Timestamp(actual, TimeUnit.Microsecond)) =>
        current.setSafe(row, actual)
      case (current: TimeStampNanoVector, ScalarValue.Timestamp(actual, TimeUnit.Nanosecond)) =>
        current.setSafe(row, actual)
      case (current, ScalarValue.Null) => setNull(current, row)
      case _ =>
        throw new IllegalArgumentException(s"value $value is incompatible with ${vector.getField}")

  private def setNull(vector: FieldVector, row: Int): Unit = vector match
    case current: BitVector => current.setNull(row)
    case current: IntVector => current.setNull(row)
    case current: BigIntVector => current.setNull(row)
    case current: Float4Vector => current.setNull(row)
    case current: Float8Vector => current.setNull(row)
    case current: VarCharVector => current.setNull(row)
    case current: TimeStampSecVector => current.setNull(row)
    case current: TimeStampMilliVector => current.setNull(row)
    case current: TimeStampMicroVector => current.setNull(row)
    case current: TimeStampNanoVector => current.setNull(row)
    case _ => throw new IllegalArgumentException(s"unsupported Arrow vector ${vector.getClass}")

  private def decodeBatch(
      schema: Schema,
      root: VectorSchemaRoot
  ): Either[SourceError, RecordBatch] =
    val columns = schema.fields.zipWithIndex.map: (field, index) =>
      decodeColumn(field.dataType, root.getVector(index), root.getRowCount)
    columns
      .foldLeft[Either[SourceError, Vector[ColumnArray]]](Right(Vector.empty)):
        case (result, value) => result.flatMap(current => value.map(current :+ _))
      .flatMap(values => RecordBatch(schema, values).leftMap(SourceError.Storage.apply))

  private def decodeColumn(
      dataType: DataType,
      vector: FieldVector,
      length: Int
  ): Either[SourceError, ColumnArray] =
    val valid = Array.tabulate(length)(index => !vector.isNull(index))
    def invalid: Left[SourceError, Nothing] =
      Left(SourceError.SchemaMismatch(s"vector ${vector.getClass.getName} does not match $dataType"))
    val decoded: Either[SourceError, Either[StorageError, ColumnArray]] =
      (dataType, vector) match
      case (DataType.Bool, current: BitVector) =>
        Right(
          ColumnArray.bool(
            Array.tabulate(length)(index => valid(index) && current.get(index) != 0),
            valid
          )
        )
      case (DataType.Int32, current: IntVector) =>
        Right(ColumnArray.int32(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0), valid))
      case (DataType.Int64, current: BigIntVector) =>
        Right(ColumnArray.int64(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0L), valid))
      case (DataType.Float32, current: Float4Vector) =>
        Right(ColumnArray.float32(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0.0f), valid))
      case (DataType.Float64, current: Float8Vector) =>
        Right(ColumnArray.float64(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0.0), valid))
      case (DataType.Utf8, current: VarCharVector) =>
        Right(
          ColumnArray.utf8(
            Array.tabulate(length): index =>
              if valid(index) then new String(current.get(index), StandardCharsets.UTF_8)
              else "",
            valid
          )
        )
      case (DataType.Timestamp(TimeUnit.Second), current: TimeStampSecVector) =>
        Right(ColumnArray.timestamp(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0L), TimeUnit.Second, valid))
      case (DataType.Timestamp(TimeUnit.Millisecond), current: TimeStampMilliVector) =>
        Right(ColumnArray.timestamp(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0L), TimeUnit.Millisecond, valid))
      case (DataType.Timestamp(TimeUnit.Microsecond), current: TimeStampMicroVector) =>
        Right(ColumnArray.timestamp(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0L), TimeUnit.Microsecond, valid))
      case (DataType.Timestamp(TimeUnit.Nanosecond), current: TimeStampNanoVector) =>
        Right(ColumnArray.timestamp(Array.tabulate(length)(index => if valid(index) then current.get(index) else 0L), TimeUnit.Nanosecond, valid))
      case _ => invalid
    decoded.flatMap(_.leftMap(SourceError.Storage.apply))
