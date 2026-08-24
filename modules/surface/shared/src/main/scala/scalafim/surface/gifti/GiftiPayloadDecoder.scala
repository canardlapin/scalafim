package scalafim.surface.gifti

import java.nio.ByteBuffer
import java.nio.ByteOrder
import java.util.Base64
import scala.util.control.NonFatal

private[surface] object GiftiPayloadDecoder:

  enum EncodedData:
    case Ascii
    case Binary(bytes: Array[Byte])
    case Compressed(bytes: Array[Byte])

  def encodedData(array: GiftiDataArray): Either[GiftiError, EncodedData] =
    array.encoding match
      case GiftiEncoding.Ascii => Right(EncodedData.Ascii)
      case GiftiEncoding.Base64Binary => decodeBase64(array.dataText).map(EncodedData.Binary.apply)
      case GiftiEncoding.GZipBase64Binary => decodeBase64(array.dataText).map(EncodedData.Compressed.apply)
      case other => Left(GiftiError.UnsupportedEncoding(other))

  def expectedByteCount(array: GiftiDataArray): Either[GiftiError, Int] =
    val bytesPerValue =
      array.dataType match
        case GiftiDataType.Float32 | GiftiDataType.Int32 => Right(4L)
        case GiftiDataType.UInt8 => Right(1L)
        case other => Left(GiftiError.UnsupportedDataType(other))
    for
      width <- bytesPerValue
      count <- expectedElementCount(array, Int.MaxValue.toLong / width)
    yield (count * width).toInt

  private def expectedElementCount(
    array: GiftiDataArray,
    maximum: Long = Int.MaxValue.toLong
  ): Either[GiftiError, Long] =
    var count = 1L
    var index = 0
    while index < array.dims.length do
      val dimension = array.dims(index).toLong
      if count > maximum / dimension then
        return Left(
          GiftiError.InvalidDataArray(
            s"${array.intent.code} dimensions exceed the supported in-memory size"
          )
        )
      count *= dimension
      index += 1
    Right(count)

  def doublePayload(
    array: GiftiDataArray,
    data: EncodedData
  ): Either[GiftiError, GiftiPayload[Double]] =
    doubleValues(array, data).flatMap(values => GiftiPayload.from(array, values.toVector))

  def intPayload(
    array: GiftiDataArray,
    data: EncodedData
  ): Either[GiftiError, GiftiPayload[Int]] =
    intValues(array, data).flatMap(values => GiftiPayload.from(array, values.toVector))

  def bytePayload(
    array: GiftiDataArray,
    data: EncodedData
  ): Either[GiftiError, GiftiPayload[Int]] =
    byteValues(array, data).flatMap(values => GiftiPayload.from(array, values.toVector))

  private def doubleValues(
    array: GiftiDataArray,
    data: EncodedData
  ): Either[GiftiError, Array[Double]] =
    data match
      case EncodedData.Ascii =>
        parseAscii(array).map(_.map(_.toDouble))
      case EncodedData.Binary(bytes) =>
        decodeDoubles(array, bytes).flatMap(validateCount(array, _))
      case EncodedData.Compressed(_) =>
        Left(GiftiError.DecodeFailure("compressed GIFTI payload was not inflated before numeric decoding"))

  private def intValues(
    array: GiftiDataArray,
    data: EncodedData
  ): Either[GiftiError, Array[Int]] =
    data match
      case EncodedData.Ascii =>
        parseAscii(array).flatMap { values =>
          val out = Array.ofDim[Int](values.length)
          var error: GiftiError | Null = null
          var i = 0
          while i < values.length && error == null do
            val value = values(i)
            if !value.isWhole || value < Int.MinValue.toDouble || value > Int.MaxValue.toDouble then
              error = GiftiError.DecodeFailure(s"${array.intent.code} contains non-integer ASCII value $value")
            else out(i) = value.toInt
            i += 1
          error match
            case null => validateCount(array, out)
            case found => Left(found)
        }
      case EncodedData.Binary(bytes) =>
        decodeInts(array, bytes).flatMap(validateCount(array, _))
      case EncodedData.Compressed(_) =>
        Left(GiftiError.DecodeFailure("compressed GIFTI payload was not inflated before numeric decoding"))

  private def byteValues(
    array: GiftiDataArray,
    data: EncodedData
  ): Either[GiftiError, Array[Int]] =
    array.dataType match
      case GiftiDataType.UInt8 =>
        data match
          case EncodedData.Ascii =>
            intValues(array, data).flatMap { values =>
              val invalid = values.indexWhere(value => value < 0 || value > 255)
              if invalid < 0 then Right(values)
              else Left(GiftiError.DecodeFailure(s"${array.intent.code} contains UINT8 value ${values(invalid)} outside 0..255"))
            }
          case EncodedData.Binary(bytes) =>
            validateByteCount(array, bytes).map(_.map(_ & 0xff))
          case EncodedData.Compressed(_) =>
            Left(GiftiError.DecodeFailure("compressed GIFTI payload was not inflated before numeric decoding"))
      case other => Left(GiftiError.UnsupportedDataType(other))

  private def parseAscii(array: GiftiDataArray): Either[GiftiError, Array[Double]] =
    array.dataType match
      case GiftiDataType.Float32 | GiftiDataType.Int32 | GiftiDataType.UInt8 =>
        parseDoubles(array.dataText, array.intent.code).flatMap(validateCount(array, _))
      case other => Left(GiftiError.UnsupportedDataType(other))

  private def parseDoubles(text: String, label: String): Either[GiftiError, Array[Double]] =
    try Right(splitFields(text).map(_.toDouble))
    catch case _: NumberFormatException => Left(GiftiError.DecodeFailure(s"$label contains non-numeric ASCII data"))

  private def decodeDoubles(
    array: GiftiDataArray,
    bytes: Array[Byte]
  ): Either[GiftiError, Array[Double]] =
    array.dataType match
      case GiftiDataType.Float32 =>
        withBuffer(array, bytes, 4) { buffer =>
          val out = Array.ofDim[Double](bytes.length / 4)
          var i = 0
          while i < out.length do
            out(i) = buffer.getFloat().toDouble
            i += 1
          out
        }
      case GiftiDataType.Int32 =>
        withBuffer(array, bytes, 4) { buffer =>
          val out = Array.ofDim[Double](bytes.length / 4)
          var i = 0
          while i < out.length do
            out(i) = buffer.getInt().toDouble
            i += 1
          out
        }
      case GiftiDataType.UInt8 =>
        validateByteCount(array, bytes).map(_.map(byte => (byte & 0xff).toDouble))
      case other => Left(GiftiError.UnsupportedDataType(other))

  private def decodeInts(
    array: GiftiDataArray,
    bytes: Array[Byte]
  ): Either[GiftiError, Array[Int]] =
    array.dataType match
      case GiftiDataType.Int32 =>
        withBuffer(array, bytes, 4) { buffer =>
          val out = Array.ofDim[Int](bytes.length / 4)
          var i = 0
          while i < out.length do
            out(i) = buffer.getInt()
            i += 1
          out
        }
      case GiftiDataType.UInt8 =>
        validateByteCount(array, bytes).map(_.map(_ & 0xff))
      case other => Left(GiftiError.UnsupportedDataType(other))

  private def decodeBase64(text: String): Either[GiftiError, Array[Byte]] =
    try Right(Base64.getMimeDecoder.decode(text))
    catch
      case NonFatal(error) =>
        Left(GiftiError.DecodeFailure(s"invalid Base64 payload: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"))

  private def withBuffer[A](
    array: GiftiDataArray,
    bytes: Array[Byte],
    bytesPerValue: Int
  )(decode: ByteBuffer => A): Either[GiftiError, A] =
    if bytes.length % bytesPerValue != 0 then
      Left(GiftiError.DecodeFailure(s"${array.intent.code} byte length ${bytes.length} is not divisible by $bytesPerValue"))
    else
      for
        _ <- validateByteCount(array, bytes)
        order <- byteOrder(array.endian)
      yield decode(ByteBuffer.wrap(bytes).order(order))

  private def byteOrder(endian: GiftiEndian): Either[GiftiError, ByteOrder] =
    endian match
      case GiftiEndian.Little => Right(ByteOrder.LITTLE_ENDIAN)
      case GiftiEndian.Big => Right(ByteOrder.BIG_ENDIAN)
      case other => Left(GiftiError.UnsupportedEndian(other))

  private def validateByteCount(
    array: GiftiDataArray,
    bytes: Array[Byte]
  ): Either[GiftiError, Array[Byte]] =
    expectedByteCount(array).flatMap { expected =>
      if bytes.length == expected then Right(bytes)
      else
        Left(
          GiftiError.DecodeFailure(
            s"${array.intent.code} decoded ${bytes.length} bytes but dimensions and data type require $expected"
          )
        )
    }

  private def validateCount[A](
    array: GiftiDataArray,
    values: Array[A]
  ): Either[GiftiError, Array[A]] =
    expectedElementCount(array).flatMap { expected =>
      if values.length == expected.toInt then Right(values)
      else
        Left(
          GiftiError.DecodeFailure(
            s"${array.intent.code} decoded ${values.length} values but dimensions require $expected"
          )
        )
    }

  private def splitFields(text: String): Array[String] =
    text.trim.split("\\s+").filter(_.nonEmpty)
