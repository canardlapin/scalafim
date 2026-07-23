package scalafim.frame

import scala.NamedTuple
import scala.collection.mutable.ArrayBuffer

enum StorageError:
  case BufferClosed
  case InvalidRange(offset: Int, length: Int, available: Int)
  case InvalidValidityLength(expected: Int, actual: Int)
  case ColumnLengthMismatch(expected: Int, actual: Int, column: Int)
  case ColumnCountMismatch(expected: Int, actual: Int)
  case ColumnTypeMismatch(column: Int, expected: DataType, actual: DataType)
  case RequiredColumnContainsNull(column: Int, nullCount: Int)
  case SchemaMismatch(expected: Schema, actual: Schema)
  case ColumnNotFound(name: String)
  case NullValue(index: Int)
  case InvalidUtf8Offsets(index: Int, start: Int, end: Int, available: Int)
  case InvalidDictionaryIndex(index: Int, value: Int, dictionarySize: Int)
  case SourceAlreadyOpened
  case SourceClosed
  case Unexpected(error: String)

  def message: String = this match
    case BufferClosed => "buffer is closed"
    case InvalidRange(offset, length, available) =>
      s"range offset=$offset length=$length exceeds available length $available"
    case InvalidValidityLength(expected, actual) =>
      s"validity length $actual does not match value length $expected"
    case ColumnLengthMismatch(expected, actual, column) =>
      s"column $column length $actual does not match batch length $expected"
    case ColumnCountMismatch(expected, actual) =>
      s"column count $actual does not match schema field count $expected"
    case ColumnTypeMismatch(column, expected, actual) =>
      s"column $column has type $actual; expected $expected"
    case RequiredColumnContainsNull(column, nullCount) =>
      s"required column $column contains $nullCount null values"
    case SchemaMismatch(expected, actual) =>
      s"batch schema $actual does not match table schema $expected"
    case ColumnNotFound(name) => s"column '$name' does not exist"
    case NullValue(index) => s"value at index $index is null"
    case InvalidUtf8Offsets(index, start, end, available) =>
      s"UTF-8 offsets at $index are invalid: $start..$end within $available bytes"
    case InvalidDictionaryIndex(index, value, dictionarySize) =>
      s"dictionary index at $index is $value; dictionary size is $dictionarySize"
    case SourceAlreadyOpened => "owned batch source has already been opened"
    case SourceClosed => "batch source is closed"
    case Unexpected(error) => error

enum BufferOwnership:
  case Owned
  case Borrowed

final case class BufferSnapshot(activeOwners: Int, activeViews: Int, releasedOwners: Long)

final class BufferTracker:
  private var owners = 0
  private var views = 0
  private var released = 0L

  private[frame] def ownerOpened(): Unit = synchronized:
    owners += 1

  private[frame] def ownerReleased(): Unit = synchronized:
    owners -= 1
    released += 1

  private[frame] def viewOpened(): Unit = synchronized:
    views += 1

  private[frame] def viewClosed(): Unit = synchronized:
    views -= 1

  def snapshot: BufferSnapshot = synchronized:
    BufferSnapshot(owners, views, released)

private final class BufferState(
    val bytes: Array[Byte],
    val ownership: BufferOwnership,
    tracker: BufferTracker,
    releaseExternal: () => Unit
):
  private var references = 1
  private var released = false

  tracker.ownerOpened()

  def retain(): Either[StorageError, Unit] = synchronized:
    if released then Left(StorageError.BufferClosed)
    else
      references += 1
      Right(())

  def release(): Unit = synchronized:
    if !released then
      references -= 1
      if references == 0 then
        released = true
        releaseExternal()
        tracker.ownerReleased()

  def isReleased: Boolean = synchronized(released)

final class Buffer private (
    private val state: BufferState,
    val offset: Int,
    val length: Int,
    val ownership: BufferOwnership,
    private val tracker: BufferTracker
):
  private var closed = false
  tracker.viewOpened()

  def isClosed: Boolean = synchronized(closed || state.isReleased)

  def slice(relativeOffset: Int, sliceLength: Int): Either[StorageError, Buffer] = synchronized:
    if isClosed then Left(StorageError.BufferClosed)
    else if relativeOffset < 0 || sliceLength < 0 || relativeOffset + sliceLength > length then
      Left(StorageError.InvalidRange(relativeOffset, sliceLength, length))
    else
      state.retain().map: _ =>
        new Buffer(state, offset + relativeOffset, sliceLength, ownership, tracker)

  private[frame] def read[A](operation: (Array[Byte], Int) => A): Either[StorageError, A] = synchronized:
    if isClosed then Left(StorageError.BufferClosed)
    else Right(operation(state.bytes, offset))

  def byteAt(index: Int): Either[StorageError, Byte] =
    if index < 0 || index >= length then Left(StorageError.InvalidRange(index, 1, length))
    else read((bytes, start) => bytes(start + index))

  def copyBytes: Either[StorageError, Array[Byte]] =
    read((bytes, start) => java.util.Arrays.copyOfRange(bytes, start, start + length))

  def close(): Unit = synchronized:
    if !closed then
      closed = true
      state.release()
      tracker.viewClosed()

object Buffer:
  def owned(bytes: Array[Byte], tracker: BufferTracker = new BufferTracker): Buffer =
    val copied = bytes.clone()
    val state = new BufferState(copied, BufferOwnership.Owned, tracker, () => ())
    new Buffer(state, 0, copied.length, BufferOwnership.Owned, tracker)

  def borrowed(
      bytes: Array[Byte],
      release: () => Unit,
      tracker: BufferTracker = new BufferTracker
  ): Buffer =
    val state = new BufferState(bytes, BufferOwnership.Borrowed, tracker, release)
    new Buffer(state, 0, bytes.length, BufferOwnership.Borrowed, tracker)

enum PhysicalEncoding:
  case Plain
  case Dictionary(indexType: DataType, valueType: DataType)

enum BufferRole:
  case Validity
  case Offsets
  case Values
  case DictionaryIndices
  case DictionaryValues

final case class BufferLayout(role: BufferRole, byteLength: Int, bitWidth: Int)

final case class ArrayLayout(
    dataType: DataType,
    encoding: PhysicalEncoding,
    logicalOffset: Int,
    length: Int,
    buffers: Vector[BufferLayout]
)

enum ScalarValue:
  case Null
  case Bool(value: Boolean)
  case Int32(value: Int)
  case Int64(value: Long)
  case Float32(value: Float)
  case Float64(value: Double)
  case Utf8(value: String)
  case Timestamp(value: Long, unit: TimeUnit)

private object LittleEndian:
  def int(bytes: Array[Byte], offset: Int): Int =
    (bytes(offset) & 0xff) |
      ((bytes(offset + 1) & 0xff) << 8) |
      ((bytes(offset + 2) & 0xff) << 16) |
      ((bytes(offset + 3) & 0xff) << 24)

  def long(bytes: Array[Byte], offset: Int): Long =
    (bytes(offset).toLong & 0xffL) |
      ((bytes(offset + 1).toLong & 0xffL) << 8) |
      ((bytes(offset + 2).toLong & 0xffL) << 16) |
      ((bytes(offset + 3).toLong & 0xffL) << 24) |
      ((bytes(offset + 4).toLong & 0xffL) << 32) |
      ((bytes(offset + 5).toLong & 0xffL) << 40) |
      ((bytes(offset + 6).toLong & 0xffL) << 48) |
      ((bytes(offset + 7).toLong & 0xffL) << 56)

  def putInt(bytes: Array[Byte], offset: Int, value: Int): Unit =
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte

  def putLong(bytes: Array[Byte], offset: Int, value: Long): Unit =
    bytes(offset) = value.toByte
    bytes(offset + 1) = (value >>> 8).toByte
    bytes(offset + 2) = (value >>> 16).toByte
    bytes(offset + 3) = (value >>> 24).toByte
    bytes(offset + 4) = (value >>> 32).toByte
    bytes(offset + 5) = (value >>> 40).toByte
    bytes(offset + 6) = (value >>> 48).toByte
    bytes(offset + 7) = (value >>> 56).toByte

sealed trait Validity:
  def length: Int
  def nullCount: Int
  def isValid(index: Int): Either[StorageError, Boolean]
  private[frame] def slice(offset: Int, length: Int): Either[StorageError, Validity]
  private[frame] def layouts: Vector[BufferLayout]
  private[frame] def copyBuffers: Either[StorageError, Vector[Array[Byte]]]
  def close(): Unit

object Validity:
  private final class Required(val length: Int) extends Validity:
    val nullCount = 0

    def isValid(index: Int): Either[StorageError, Boolean] =
      if index < 0 || index >= length then Left(StorageError.InvalidRange(index, 1, length))
      else Right(true)

    private[frame] def slice(offset: Int, sliceLength: Int): Either[StorageError, Validity] =
      if offset < 0 || sliceLength < 0 || offset + sliceLength > length then
        Left(StorageError.InvalidRange(offset, sliceLength, length))
      else Right(new Required(sliceLength))

    private[frame] val layouts = Vector.empty
    private[frame] val copyBuffers = Right(Vector.empty)

    def close(): Unit = ()

  private final class Bitmap(
      buffer: Buffer,
      bitOffset: Int,
      val length: Int,
      val nullCount: Int
  ) extends Validity:
    def isValid(index: Int): Either[StorageError, Boolean] =
      if index < 0 || index >= length then Left(StorageError.InvalidRange(index, 1, length))
      else
        val absolute = bitOffset + index
        buffer.byteAt(absolute >>> 3).map: value =>
          ((value.toInt >>> (absolute & 7)) & 1) == 1

    private[frame] def slice(offset: Int, sliceLength: Int): Either[StorageError, Validity] =
      if offset < 0 || sliceLength < 0 || offset + sliceLength > length then
        Left(StorageError.InvalidRange(offset, sliceLength, length))
      else
        var nulls = 0
        var index = 0
        var error: Option[StorageError] = None
        while index < sliceLength && error.isEmpty do
          isValid(offset + index) match
            case Right(false) => nulls += 1
            case Right(true) => ()
            case Left(value) => error = Some(value)
          index += 1
        error match
          case Some(value) => Left(value)
          case None => buffer.slice(0, buffer.length).map(new Bitmap(_, bitOffset + offset, sliceLength, nulls))

    private[frame] def layouts = Vector(BufferLayout(BufferRole.Validity, buffer.length, 1))
    private[frame] def copyBuffers = buffer.copyBytes.map(Vector(_))

    def close(): Unit = buffer.close()

  def fromFlags(
      valid: Array[Boolean],
      tracker: BufferTracker = new BufferTracker
  ): Validity =
    var nulls = 0
    var index = 0
    while index < valid.length do
      if !valid(index) then nulls += 1
      index += 1
    if nulls == 0 then new Required(valid.length)
    else
      val bytes = new Array[Byte]((valid.length + 7) >>> 3)
      index = 0
      while index < valid.length do
        if valid(index) then
          val byteIndex = index >>> 3
          bytes(byteIndex) = (bytes(byteIndex) | (1 << (index & 7))).toByte
        index += 1
      new Bitmap(Buffer.owned(bytes, tracker), 0, valid.length, nulls)

sealed trait ColumnArray:
  def dataType: DataType
  def encoding: PhysicalEncoding
  def length: Int
  def nullCount: Int
  def layout: ArrayLayout
  def copyPhysicalBuffers: Either[StorageError, Vector[Array[Byte]]]
  def scalar(index: Int): Either[StorageError, ScalarValue]
  def slice(offset: Int, length: Int): Either[StorageError, ColumnArray]
  def close(): Unit

private abstract class FixedWidthArray(
    val dataType: DataType,
    val length: Int,
    protected val logicalOffset: Int,
    protected val width: Int,
    protected val values: Buffer,
    protected val validity: Validity
) extends ColumnArray:
  val encoding = PhysicalEncoding.Plain
  def nullCount: Int = validity.nullCount
  def layout: ArrayLayout = ArrayLayout(
    dataType,
    encoding,
    logicalOffset,
    length,
    validity.layouts :+ BufferLayout(BufferRole.Values, values.length, width * 8)
  )

  def copyPhysicalBuffers: Either[StorageError, Vector[Array[Byte]]] =
    validity.copyBuffers.flatMap: validityBuffers =>
      values.copyBytes.map(validityBuffers :+ _)

  protected def validIndex(index: Int): Either[StorageError, Int] =
    if index < 0 || index >= length then Left(StorageError.InvalidRange(index, 1, length))
    else Right(logicalOffset + index)

  protected def readValue(index: Int): Either[StorageError, ScalarValue]

  def scalar(index: Int): Either[StorageError, ScalarValue] =
    validIndex(index).flatMap: _ =>
      validity.isValid(index).flatMap: valid =>
        if valid then readValue(index) else Right(ScalarValue.Null)

  protected def sliced(
      offset: Int,
      sliceLength: Int,
      retainedValues: Buffer,
      retainedValidity: Validity
  ): ColumnArray

  def slice(offset: Int, sliceLength: Int): Either[StorageError, ColumnArray] =
    if offset < 0 || sliceLength < 0 || offset + sliceLength > length then
      Left(StorageError.InvalidRange(offset, sliceLength, length))
    else
      values.slice(0, values.length).flatMap: retainedValues =>
        validity.slice(offset, sliceLength) match
          case Right(retainedValidity) =>
            Right(sliced(offset, sliceLength, retainedValues, retainedValidity))
          case Left(error) =>
            retainedValues.close()
            Left(error)

  def close(): Unit =
    validity.close()
    values.close()

final class Int32Array private[frame] (
    length: Int,
    logicalOffset: Int,
    values: Buffer,
    validity: Validity
) extends FixedWidthArray(DataType.Int32, length, logicalOffset, 4, values, validity):
  def value(index: Int): Either[StorageError, Int] =
    validIndex(index).flatMap: absolute =>
      validity.isValid(index).flatMap: valid =>
        if !valid then Left(StorageError.NullValue(index))
        else values.read((bytes, start) => LittleEndian.int(bytes, start + absolute * 4))

  protected def readValue(index: Int) = value(index).map(ScalarValue.Int32.apply)

  protected def sliced(offset: Int, sliceLength: Int, retainedValues: Buffer, retainedValidity: Validity) =
    new Int32Array(sliceLength, logicalOffset + offset, retainedValues, retainedValidity)

final class Int64Array private[frame] (
    length: Int,
    logicalOffset: Int,
    values: Buffer,
    validity: Validity
) extends FixedWidthArray(DataType.Int64, length, logicalOffset, 8, values, validity):
  def value(index: Int): Either[StorageError, Long] =
    validIndex(index).flatMap: absolute =>
      validity.isValid(index).flatMap: valid =>
        if !valid then Left(StorageError.NullValue(index))
        else values.read((bytes, start) => LittleEndian.long(bytes, start + absolute * 8))

  protected def readValue(index: Int) = value(index).map(ScalarValue.Int64.apply)

  protected def sliced(offset: Int, sliceLength: Int, retainedValues: Buffer, retainedValidity: Validity) =
    new Int64Array(sliceLength, logicalOffset + offset, retainedValues, retainedValidity)

final class Float32Array private[frame] (
    length: Int,
    logicalOffset: Int,
    values: Buffer,
    validity: Validity
) extends FixedWidthArray(DataType.Float32, length, logicalOffset, 4, values, validity):
  def value(index: Int): Either[StorageError, Float] =
    validIndex(index).flatMap: absolute =>
      validity.isValid(index).flatMap: valid =>
        if !valid then Left(StorageError.NullValue(index))
        else values.read((bytes, start) => java.lang.Float.intBitsToFloat(LittleEndian.int(bytes, start + absolute * 4)))

  protected def readValue(index: Int) = value(index).map(ScalarValue.Float32.apply)

  protected def sliced(offset: Int, sliceLength: Int, retainedValues: Buffer, retainedValidity: Validity) =
    new Float32Array(sliceLength, logicalOffset + offset, retainedValues, retainedValidity)

final class Float64Array private[frame] (
    length: Int,
    logicalOffset: Int,
    values: Buffer,
    validity: Validity
) extends FixedWidthArray(DataType.Float64, length, logicalOffset, 8, values, validity):
  def value(index: Int): Either[StorageError, Double] =
    validIndex(index).flatMap: absolute =>
      validity.isValid(index).flatMap: valid =>
        if !valid then Left(StorageError.NullValue(index))
        else values.read((bytes, start) => java.lang.Double.longBitsToDouble(LittleEndian.long(bytes, start + absolute * 8)))

  protected def readValue(index: Int) = value(index).map(ScalarValue.Float64.apply)

  protected def sliced(offset: Int, sliceLength: Int, retainedValues: Buffer, retainedValidity: Validity) =
    new Float64Array(sliceLength, logicalOffset + offset, retainedValues, retainedValidity)

final class TimestampArray private[frame] (
    val unit: TimeUnit,
    length: Int,
    logicalOffset: Int,
    values: Buffer,
    validity: Validity
) extends FixedWidthArray(DataType.Timestamp(unit), length, logicalOffset, 8, values, validity):
  def value(index: Int): Either[StorageError, Long] =
    validIndex(index).flatMap: absolute =>
      validity.isValid(index).flatMap: valid =>
        if !valid then Left(StorageError.NullValue(index))
        else values.read((bytes, start) => LittleEndian.long(bytes, start + absolute * 8))

  protected def readValue(index: Int) = value(index).map(ScalarValue.Timestamp(_, unit))

  protected def sliced(offset: Int, sliceLength: Int, retainedValues: Buffer, retainedValidity: Validity) =
    new TimestampArray(unit, sliceLength, logicalOffset + offset, retainedValues, retainedValidity)

final class BooleanArray private[frame] (
    val length: Int,
    private val logicalOffset: Int,
    private val values: Buffer,
    private val validity: Validity
) extends ColumnArray:
  val dataType = DataType.Bool
  val encoding = PhysicalEncoding.Plain
  def nullCount: Int = validity.nullCount
  def layout: ArrayLayout = ArrayLayout(
    dataType,
    encoding,
    logicalOffset,
    length,
    validity.layouts :+ BufferLayout(BufferRole.Values, values.length, 1)
  )

  def copyPhysicalBuffers: Either[StorageError, Vector[Array[Byte]]] =
    validity.copyBuffers.flatMap: validityBuffers =>
      values.copyBytes.map(validityBuffers :+ _)

  def value(index: Int): Either[StorageError, Boolean] =
    if index < 0 || index >= length then Left(StorageError.InvalidRange(index, 1, length))
    else
      validity.isValid(index).flatMap: valid =>
        if !valid then Left(StorageError.NullValue(index))
        else
          val absolute = logicalOffset + index
          values.byteAt(absolute >>> 3).map(byte => ((byte.toInt >>> (absolute & 7)) & 1) == 1)

  def scalar(index: Int): Either[StorageError, ScalarValue] =
    validity.isValid(index).flatMap: valid =>
      if valid then value(index).map(ScalarValue.Bool.apply) else Right(ScalarValue.Null)

  def slice(offset: Int, sliceLength: Int): Either[StorageError, ColumnArray] =
    if offset < 0 || sliceLength < 0 || offset + sliceLength > length then
      Left(StorageError.InvalidRange(offset, sliceLength, length))
    else
      values.slice(0, values.length).flatMap: retainedValues =>
        validity.slice(offset, sliceLength) match
          case Right(retainedValidity) =>
            Right(new BooleanArray(sliceLength, logicalOffset + offset, retainedValues, retainedValidity))
          case Left(error) =>
            retainedValues.close()
            Left(error)

  def close(): Unit =
    validity.close()
    values.close()

final class Utf8Array private[frame] (
    val length: Int,
    private val logicalOffset: Int,
    private val offsets: Buffer,
    private val values: Buffer,
    private val validity: Validity
) extends ColumnArray:
  val dataType = DataType.Utf8
  val encoding = PhysicalEncoding.Plain
  def nullCount: Int = validity.nullCount
  def layout: ArrayLayout = ArrayLayout(
    dataType,
    encoding,
    logicalOffset,
    length,
    validity.layouts ++ Vector(
      BufferLayout(BufferRole.Offsets, offsets.length, 32),
      BufferLayout(BufferRole.Values, values.length, 8)
    )
  )

  def copyPhysicalBuffers: Either[StorageError, Vector[Array[Byte]]] =
    validity.copyBuffers.flatMap: validityBuffers =>
      offsets.copyBytes.flatMap: offsetBytes =>
        values.copyBytes.map(valueBytes => validityBuffers ++ Vector(offsetBytes, valueBytes))

  private def bounds(index: Int): Either[StorageError, (Int, Int)] =
    if index < 0 || index >= length then Left(StorageError.InvalidRange(index, 1, length))
    else
      offsets.read: (bytes, start) =>
        val absolute = logicalOffset + index
        (LittleEndian.int(bytes, start + absolute * 4), LittleEndian.int(bytes, start + (absolute + 1) * 4))
      .flatMap: (from, until) =>
        if from < 0 || until < from || until > values.length then
          Left(StorageError.InvalidUtf8Offsets(index, from, until, values.length))
        else Right((from, until))

  def value(index: Int): Either[StorageError, String] =
    validity.isValid(index).flatMap: valid =>
      if !valid then Left(StorageError.NullValue(index))
      else
        bounds(index).flatMap: (from, until) =>
          values.read: (bytes, start) =>
            new String(bytes, start + from, until - from, "UTF-8")

  def scalar(index: Int): Either[StorageError, ScalarValue] =
    validity.isValid(index).flatMap: valid =>
      if valid then value(index).map(ScalarValue.Utf8.apply) else Right(ScalarValue.Null)

  def slice(offset: Int, sliceLength: Int): Either[StorageError, ColumnArray] =
    if offset < 0 || sliceLength < 0 || offset + sliceLength > length then
      Left(StorageError.InvalidRange(offset, sliceLength, length))
    else
      offsets.slice(0, offsets.length).flatMap: retainedOffsets =>
        values.slice(0, values.length).flatMap: retainedValues =>
          validity.slice(offset, sliceLength) match
            case Right(retainedValidity) =>
              Right(new Utf8Array(sliceLength, logicalOffset + offset, retainedOffsets, retainedValues, retainedValidity))
            case Left(error) =>
              retainedOffsets.close()
              retainedValues.close()
              Left(error)

  def close(): Unit =
    validity.close()
    offsets.close()
    values.close()

final class DictionaryArray private[frame] (
    private val indices: Int32Array,
    private val dictionary: ColumnArray
) extends ColumnArray:
  val dataType = dictionary.dataType
  val encoding = PhysicalEncoding.Dictionary(DataType.Int32, dictionary.dataType)
  def length: Int = indices.length
  def nullCount: Int = indices.nullCount
  def layout: ArrayLayout = ArrayLayout(
    dataType,
    encoding,
    0,
    length,
    indices.layout.buffers.map(_.copy(role = BufferRole.DictionaryIndices)) ++
      dictionary.layout.buffers.map(_.copy(role = BufferRole.DictionaryValues))
  )

  def copyPhysicalBuffers: Either[StorageError, Vector[Array[Byte]]] =
    indices.copyPhysicalBuffers.flatMap: indexBuffers =>
      dictionary.copyPhysicalBuffers.map(indexBuffers ++ _)

  def scalar(index: Int): Either[StorageError, ScalarValue] =
    indices.scalar(index).flatMap:
      case ScalarValue.Null => Right(ScalarValue.Null)
      case ScalarValue.Int32(value) =>
        if value < 0 || value >= dictionary.length then
          Left(StorageError.InvalidDictionaryIndex(index, value, dictionary.length))
        else dictionary.scalar(value)
      case other => Left(StorageError.Unexpected(s"dictionary index had unexpected scalar $other"))

  def slice(offset: Int, sliceLength: Int): Either[StorageError, ColumnArray] =
    indices.slice(offset, sliceLength).flatMap:
      case retainedIndices: Int32Array =>
        dictionary.slice(0, dictionary.length) match
          case Right(retainedDictionary) => Right(new DictionaryArray(retainedIndices, retainedDictionary))
          case Left(error) =>
            retainedIndices.close()
            Left(error)
      case other =>
        other.close()
        Left(StorageError.Unexpected("Int32 slice changed its physical type"))

  def close(): Unit =
    indices.close()
    dictionary.close()

object ColumnArray:
  private def validity(
      length: Int,
      valid: Array[Boolean],
      tracker: BufferTracker
  ): Either[StorageError, Validity] =
    if valid.length != length then Left(StorageError.InvalidValidityLength(length, valid.length))
    else Right(Validity.fromFlags(valid, tracker))

  private def allValid(length: Int): Array[Boolean] = Array.fill(length)(true)

  def int32(
      input: Array[Int],
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, Int32Array] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val bytes = new Array[Byte](input.length * 4)
      var index = 0
      while index < input.length do
        LittleEndian.putInt(bytes, index * 4, input(index))
        index += 1
      new Int32Array(input.length, 0, Buffer.owned(bytes, tracker), validity)

  def int64(
      input: Array[Long],
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, Int64Array] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val bytes = new Array[Byte](input.length * 8)
      var index = 0
      while index < input.length do
        LittleEndian.putLong(bytes, index * 8, input(index))
        index += 1
      new Int64Array(input.length, 0, Buffer.owned(bytes, tracker), validity)

  def float32(
      input: Array[Float],
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, Float32Array] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val bytes = new Array[Byte](input.length * 4)
      var index = 0
      while index < input.length do
        LittleEndian.putInt(bytes, index * 4, java.lang.Float.floatToRawIntBits(input(index)))
        index += 1
      new Float32Array(input.length, 0, Buffer.owned(bytes, tracker), validity)

  def float64(
      input: Array[Double],
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, Float64Array] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val bytes = new Array[Byte](input.length * 8)
      var index = 0
      while index < input.length do
        LittleEndian.putLong(bytes, index * 8, java.lang.Double.doubleToRawLongBits(input(index)))
        index += 1
      new Float64Array(input.length, 0, Buffer.owned(bytes, tracker), validity)

  def bool(
      input: Array[Boolean],
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, BooleanArray] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val bytes = new Array[Byte]((input.length + 7) >>> 3)
      var index = 0
      while index < input.length do
        if input(index) then
          val byteIndex = index >>> 3
          bytes(byteIndex) = (bytes(byteIndex) | (1 << (index & 7))).toByte
        index += 1
      new BooleanArray(input.length, 0, Buffer.owned(bytes, tracker), validity)

  def utf8(
      input: Array[String],
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, Utf8Array] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val encoded = new Array[Array[Byte]](input.length)
      var total = 0
      var index = 0
      while index < input.length do
        val bytes = input(index).getBytes("UTF-8")
        encoded(index) = bytes
        total += bytes.length
        index += 1
      val offsets = new Array[Byte]((input.length + 1) * 4)
      val values = new Array[Byte](total)
      var cursor = 0
      index = 0
      while index < input.length do
        LittleEndian.putInt(offsets, index * 4, cursor)
        val bytes = encoded(index)
        Array.copy(bytes, 0, values, cursor, bytes.length)
        cursor += bytes.length
        index += 1
      LittleEndian.putInt(offsets, input.length * 4, cursor)
      new Utf8Array(
        input.length,
        0,
        Buffer.owned(offsets, tracker),
        Buffer.owned(values, tracker),
        validity
      )

  def timestamp(
      input: Array[Long],
      unit: TimeUnit,
      valid: Array[Boolean] = Array.emptyBooleanArray,
      tracker: BufferTracker = new BufferTracker
  ): Either[StorageError, TimestampArray] =
    val flags = if valid.isEmpty then allValid(input.length) else valid
    validity(input.length, flags, tracker).map: validity =>
      val bytes = new Array[Byte](input.length * 8)
      var index = 0
      while index < input.length do
        LittleEndian.putLong(bytes, index * 8, input(index))
        index += 1
      new TimestampArray(unit, input.length, 0, Buffer.owned(bytes, tracker), validity)

  def dictionary(indices: Int32Array, values: ColumnArray): DictionaryArray =
    new DictionaryArray(indices, values)

final class RecordBatch private (
    val schema: Schema,
    val columns: Vector[ColumnArray],
    val rowCount: Int
):
  private var closed = false

  def isClosed: Boolean = synchronized(closed)

  def column(name: String): Either[StorageError, ColumnArray] =
    val index = schema.fields.indexWhere(_.name == name)
    if index < 0 then Left(StorageError.ColumnNotFound(name))
    else if isClosed then Left(StorageError.BufferClosed)
    else Right(columns(index))

  def slice(offset: Int, length: Int): Either[StorageError, RecordBatch] =
    if isClosed then Left(StorageError.BufferClosed)
    else if offset < 0 || length < 0 || offset + length > rowCount then
      Left(StorageError.InvalidRange(offset, length, rowCount))
    else
      val retained = ArrayBuffer.empty[ColumnArray]
      var index = 0
      var error: Option[StorageError] = None
      while index < columns.length && error.isEmpty do
        columns(index).slice(offset, length) match
          case Right(column) => retained += column
          case Left(value) => error = Some(value)
        index += 1
      error match
        case Some(value) =>
          retained.foreach(_.close())
          Left(value)
        case None => Right(new RecordBatch(schema, retained.toVector, length))

  def close(): Unit = synchronized:
    if !closed then
      closed = true
      columns.foreach(_.close())

object RecordBatch:
  def apply(schema: Schema, columns: Vector[ColumnArray]): Either[StorageError, RecordBatch] =
    if columns.length != schema.size then
      Left(StorageError.ColumnCountMismatch(schema.size, columns.length))
    else
      val rowCount = columns.headOption.fold(0)(_.length)
      var index = 0
      var error: Option[StorageError] = None
      while index < columns.length && error.isEmpty do
        val column = columns(index)
        val field = schema.fields(index)
        if column.length != rowCount then
          error = Some(StorageError.ColumnLengthMismatch(rowCount, column.length, index))
        else if column.dataType != field.dataType then
          error = Some(StorageError.ColumnTypeMismatch(index, field.dataType, column.dataType))
        else if !field.nullable && column.nullCount > 0 then
          error = Some(StorageError.RequiredColumnContainsNull(index, column.nullCount))
        index += 1
      error match
        case Some(value) => Left(value)
        case None => Right(new RecordBatch(schema, columns, rowCount))

final class Table[S <: NamedTuple.AnyNamedTuple] private (
    val schema: Schema,
    val batches: Vector[RecordBatch]
):
  private var closed = false

  val rowCount: Long = batches.foldLeft(0L)(_ + _.rowCount.toLong)

  def isClosed: Boolean = synchronized(closed)

  def close(): Unit = synchronized:
    if !closed then
      closed = true
      batches.foreach(_.close())

object Table:
  def apply[S <: NamedTuple.AnyNamedTuple](
      batches: Vector[RecordBatch]
  )(using descriptor: SchemaDescriptor[S]): Either[StorageError, Table[S]] =
    val expected = descriptor.schema
    batches.find(_.schema != expected) match
      case Some(batch) => Left(StorageError.SchemaMismatch(expected, batch.schema))
      case None => Right(new Table(expected, batches))

trait BatchCursor:
  def nextBatch(): Either[StorageError, Option[RecordBatch]]
  def close(): Unit

trait BatchSource:
  def schema: Schema
  def open(): Either[StorageError, BatchCursor]

  final def use[A](operation: BatchCursor => Either[StorageError, A]): Either[StorageError, A] =
    open().flatMap: cursor =>
      try operation(cursor)
      catch case error: Throwable => Left(StorageError.Unexpected(error.getMessage))
      finally cursor.close()

  final def collect[S <: NamedTuple.AnyNamedTuple](using
      descriptor: SchemaDescriptor[S]
  ): Either[StorageError, Table[S]] =
    use: cursor =>
      val batches = ArrayBuffer.empty[RecordBatch]
      var done = false
      var error: Option[StorageError] = None
      while !done && error.isEmpty do
        cursor.nextBatch() match
          case Right(Some(batch)) => batches += batch
          case Right(None) => done = true
          case Left(value) => error = Some(value)
      error match
        case Some(value) =>
          batches.foreach(_.close())
          Left(value)
        case None =>
          Table[S](batches.toVector) match
            case right @ Right(_) => right
            case left @ Left(_) =>
              batches.foreach(_.close())
              left

final class OwnedBatchSource private (
    val schema: Schema,
    private val input: Vector[RecordBatch]
) extends BatchSource:
  private var opened = false

  def open(): Either[StorageError, BatchCursor] = synchronized:
    if opened then Left(StorageError.SourceAlreadyOpened)
    else
      opened = true
      Right:
        new BatchCursor:
          private var index = 0
          private var closed = false

          def nextBatch(): Either[StorageError, Option[RecordBatch]] = synchronized:
            if closed then Left(StorageError.SourceClosed)
            else if index >= input.length then Right(None)
            else
              val batch = input(index)
              index += 1
              Right(Some(batch))

          def close(): Unit = synchronized:
            if !closed then
              closed = true
              while index < input.length do
                input(index).close()
                index += 1

object OwnedBatchSource:
  def apply(schema: Schema, batches: Vector[RecordBatch]): Either[StorageError, OwnedBatchSource] =
    batches.find(_.schema != schema) match
      case Some(batch) => Left(StorageError.SchemaMismatch(schema, batch.schema))
      case None => Right(new OwnedBatchSource(schema, batches))
