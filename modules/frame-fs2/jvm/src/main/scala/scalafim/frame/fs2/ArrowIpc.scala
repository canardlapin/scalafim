package scalafim.frame.fs2

import cats.effect.Async
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.kernel.Outcome
import cats.effect.syntax.all.*
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
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import scalafim.frame.*

final class ArrowIpcFrameSource[F[_]] private (
    delegate: InMemoryFrameSource[F]
)(using F: Async[F]) extends ArrowIpcPlatform[F]:
  def inspect: F[Either[SourceError, SourceInspection]] = delegate.inspect

  def plan(request: ScanRequest): F[Either[SourceError, PlannedScan[F]]] =
    delegate.plan(request)

  private[fs2] def close: F[Either[SourceError, Unit]] = delegate.close

  def write(
      schema: Schema,
      batches: Stream[F, RecordBatch]
  ): F[Either[SinkError, SinkReceipt]] =
    new ArrowIpcFrameSink[F]().write(schema, batches).map(_.map(_.receipt))

object ArrowIpcFrameSource:
  def resource[F[_]](
      bytes: Array[Byte]
  )(using F: Async[F]): Resource[F, ArrowIpcFrameSource[F]] =
    FrameSource.owningResource:
      ArrowIpcCodec.decode[F](bytes)
        .flatMap(result => F.fromEither(result.leftMap(SourceFailure.apply)))
        .map: (schema, batches) =>
          new ArrowIpcFrameSource(InMemoryFrameSource.owned(schema, batches))

final case class ArrowIpcWriteResult(
    bytes: Array[Byte],
    receipt: SinkReceipt
)

private final case class SinkFailure(error: SinkError)
    extends RuntimeException(error.message)

final class ArrowIpcFrameSink[F[_]](using F: Async[F])
    extends FrameSink[F, ArrowIpcWriteResult]:
  def write(
      schema: Schema,
      batches: Stream[F, RecordBatch]
  ): F[Either[SinkError, ArrowIpcWriteResult]] =
    ArrowIpcCodec.encode(schema, batches)

private object ArrowIpcCodec:
  private final case class WriterContext(
      output: ByteArrayOutputStream,
      root: VectorSchemaRoot,
      writer: ArrowStreamWriter
  )

  def encode[F[_]](
      schema: Schema,
      batches: Stream[F, RecordBatch]
  )(using F: Async[F]): F[Either[SinkError, ArrowIpcWriteResult]] =
    writerResource(schema)
      .use: context =>
        val initialize = F.blocking(context.writer.start())
        val encoded = batches
          .evalMap: batch =>
            if batch.schema != schema then
              F.raiseError[Long](SinkFailure(SinkError.SchemaMismatch(schema, batch.schema)))
            else
              F.blocking(writeBatch(schema, context, batch))
                .flatMap(result => F.fromEither(result.leftMap(SinkFailure.apply)))
          .compile
          .fold((0L, 0L)): (state, rows) =>
            (state._1 + rows, state._2 + 1L)
        initialize *> encoded.flatMap: (rows, batchCount) =>
          F.blocking:
            context.writer.end()
            val bytes = context.output.toByteArray
            ArrowIpcWriteResult(
              bytes,
              SinkReceipt(rows, batchCount, bytes.length.toLong)
            )
      .attempt
      .map:
        case Right(result) => Right(result)
        case Left(SinkFailure(error)) => Left(error)
        case Left(error) => Left(SinkError.Write(exceptionDetail(error)))

  def decode[F[_]](
      bytes: Array[Byte]
  )(using F: Async[F]): F[Either[SourceError, (Schema, Vector[RecordBatch])]] =
    Ref.of[F, Vector[RecordBatch]](Vector.empty).flatMap: retained =>
      def closeRetained: F[Unit] =
        retained.get.flatMap(_.traverse_(batch => F.delay(batch.close())))

      val decoded = readerResource(bytes)
        .use: reader =>
          for
            root <- F.blocking(reader.getVectorSchemaRoot)
            schema <- F.fromEither(
              frameSchema(root.getSchema).leftMap(SourceFailure.apply)
            )
            _ <- readBatches(reader, root, schema, retained)
            batches <- retained.get
          yield (schema, batches)
      decoded
        .guaranteeCase:
          case Outcome.Succeeded(_) => F.unit
          case _ => closeRetained
        .attempt
        .map:
          case Right(result) => Right(result)
          case Left(SourceFailure(error)) => Left(error)
          case Left(error) => Left(SourceError.Open(exceptionDetail(error)))

  private def writerResource[F[_]](
      schema: Schema
  )(using F: Async[F]): Resource[F, WriterContext] =
    Resource
      .fromAutoCloseable(F.blocking(new RootAllocator()))
      .flatMap: allocator =>
        Resource
          .fromAutoCloseable(
            F.blocking(VectorSchemaRoot.create(arrowSchema(schema), allocator))
          )
          .flatMap: root =>
            val output = new ByteArrayOutputStream
            Resource
              .fromAutoCloseable(
                F.blocking(
                  new ArrowStreamWriter(
                    root,
                    null,
                    Channels.newChannel(output)
                  )
                )
              )
              .map(writer => WriterContext(output, root, writer))

  private def readerResource[F[_]](
      bytes: Array[Byte]
  )(using F: Async[F]): Resource[F, ArrowStreamReader] =
    Resource
      .fromAutoCloseable(F.blocking(new RootAllocator()))
      .flatMap: allocator =>
        Resource.fromAutoCloseable:
          F.blocking(
            new ArrowStreamReader(
              new ByteArrayInputStream(bytes),
              allocator
            )
          )

  private def readBatches[F[_]](
      reader: ArrowStreamReader,
      root: VectorSchemaRoot,
      schema: Schema,
      retained: Ref[F, Vector[RecordBatch]]
  )(using F: Async[F]): F[Unit] =
    F.blocking(reader.loadNextBatch()).flatMap:
      case false => F.unit
      case true =>
        (
          F.uncancelable: _ =>
            F.blocking(decodeBatch(schema, root))
              .flatMap(result => F.fromEither(result.leftMap(SourceFailure.apply)))
              .flatMap(batch => retained.update(_ :+ batch))
        ) *> readBatches(reader, root, schema, retained)

  private def writeBatch(
      schema: Schema,
      context: WriterContext,
      batch: RecordBatch
  ): Either[SinkError, Long] =
    val root = context.root
    try
      root.allocateNew()
      root.setRowCount(batch.rowCount)
      var column = 0
      var error: Option[SinkError] = None
      while column < schema.size && error.isEmpty do
        val vector = root.getVector(column)
        var row = 0
        while row < batch.rowCount && error.isEmpty do
          batch.columns(column).scalar(row) match
            case Right(value) =>
              set(vector, column, row, value) match
                case Left(value) => error = Some(value)
                case Right(_) => ()
            case Left(value) => error = Some(SinkError.Storage(value))
          row += 1
        vector.setValueCount(batch.rowCount)
        column += 1
      error match
        case Some(value) => Left(value)
        case None =>
          context.writer.writeBatch()
          Right(batch.rowCount.toLong)
    catch
      case NonFatal(error) => Left(SinkError.Write(exceptionDetail(error)))
    finally root.clear()

  private def exceptionDetail(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.toString)

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

  private def set(
      vector: FieldVector,
      column: Int,
      row: Int,
      value: ScalarValue
  ): Either[SinkError, Unit] =
    (vector, value) match
      case (current: BitVector, ScalarValue.Bool(actual)) =>
        current.setSafe(row, if actual then 1 else 0)
        Right(())
      case (current: IntVector, ScalarValue.Int32(actual)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: BigIntVector, ScalarValue.Int64(actual)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: Float4Vector, ScalarValue.Float32(actual)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: Float8Vector, ScalarValue.Float64(actual)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: VarCharVector, ScalarValue.Utf8(actual)) =>
        current.setSafe(row, actual.getBytes(StandardCharsets.UTF_8))
        Right(())
      case (current: TimeStampSecVector, ScalarValue.Timestamp(actual, TimeUnit.Second)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: TimeStampMilliVector, ScalarValue.Timestamp(actual, TimeUnit.Millisecond)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: TimeStampMicroVector, ScalarValue.Timestamp(actual, TimeUnit.Microsecond)) =>
        current.setSafe(row, actual)
        Right(())
      case (current: TimeStampNanoVector, ScalarValue.Timestamp(actual, TimeUnit.Nanosecond)) =>
        current.setSafe(row, actual)
        Right(())
      case (current, ScalarValue.Null) => setNull(current, column, row, value)
      case _ => Left(SinkError.Encode(row.toLong, column, value))

  private def setNull(
      vector: FieldVector,
      column: Int,
      row: Int,
      value: ScalarValue
  ): Either[SinkError, Unit] =
    vector match
      case current: BitVector =>
        current.setNull(row)
        Right(())
      case current: IntVector =>
        current.setNull(row)
        Right(())
      case current: BigIntVector =>
        current.setNull(row)
        Right(())
      case current: Float4Vector =>
        current.setNull(row)
        Right(())
      case current: Float8Vector =>
        current.setNull(row)
        Right(())
      case current: VarCharVector =>
        current.setNull(row)
        Right(())
      case current: TimeStampSecVector =>
        current.setNull(row)
        Right(())
      case current: TimeStampMilliVector =>
        current.setNull(row)
        Right(())
      case current: TimeStampMicroVector =>
        current.setNull(row)
        Right(())
      case current: TimeStampNanoVector =>
        current.setNull(row)
        Right(())
      case _ => Left(SinkError.Encode(row.toLong, column, value))

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
