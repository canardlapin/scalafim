package scalafim.surface.io

import scalafim.surface.gifti.*

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.InputStream
import java.nio.file.{Files, Path}
import java.util.zip.GZIPInputStream
import java.util.zip.InflaterInputStream
import scala.io.Source
import scala.util.control.NonFatal

object GiftiReader:

  def read(path: Path): Either[GiftiError, GiftiDocument] =
    val input = open(path)
    val source = Source.fromInputStream(input, "UTF-8")
    try GiftiXmlParser.parse(source)
    finally
      source.close()
      input.close()

  def parseString(xml: String): Either[GiftiError, GiftiDocument] =
    GiftiXmlParser.parseString(xml)

  def doublePayload(array: GiftiDataArray): Either[GiftiError, GiftiPayload[Double]] =
    resolvedData(array).flatMap(GiftiPayloadDecoder.doublePayload(array, _))

  def doubleMatrix(array: GiftiDataArray): Either[GiftiError, GiftiMatrix[Double]] =
    doublePayload(array).flatMap(GiftiPayload.requireMatrix)

  def intPayload(array: GiftiDataArray): Either[GiftiError, GiftiPayload[Int]] =
    resolvedData(array).flatMap(GiftiPayloadDecoder.intPayload(array, _))

  def intVector(array: GiftiDataArray): Either[GiftiError, GiftiVector[Int]] =
    intPayload(array).flatMap(GiftiPayload.requireVector)

  def intMatrix(array: GiftiDataArray): Either[GiftiError, GiftiMatrix[Int]] =
    intPayload(array).flatMap(GiftiPayload.requireMatrix)

  def bytePayload(array: GiftiDataArray): Either[GiftiError, GiftiPayload[Int]] =
    resolvedData(array).flatMap(GiftiPayloadDecoder.bytePayload(array, _))

  def doubleData(array: GiftiDataArray): Either[GiftiError, Array[Double]] =
    doublePayload(array).map(_.values.toArray)

  def intData(array: GiftiDataArray): Either[GiftiError, Array[Int]] =
    intPayload(array).map(_.values.toArray)

  def byteData(array: GiftiDataArray): Either[GiftiError, Array[Int]] =
    bytePayload(array).map(_.values.toArray)

  private def resolvedData(
    array: GiftiDataArray
  ): Either[GiftiError, GiftiPayloadDecoder.EncodedData] =
    GiftiPayloadDecoder.encodedData(array).flatMap {
      case GiftiPayloadDecoder.EncodedData.Compressed(bytes) =>
        GiftiPayloadDecoder.expectedByteCount(array).flatMap { expected =>
          decompressGifti(bytes, expected).map(GiftiPayloadDecoder.EncodedData.Binary.apply)
        }
      case ready => Right(ready)
    }

  private def open(path: Path): InputStream =
    val input = Files.newInputStream(path)
    if path.getFileName.toString.endsWith(".gz") then new GZIPInputStream(input) else input

  /** FreeSurfer GIFTI writers commonly label RFC 1950 zlib payloads as
    * `GZipBase64Binary`. Accept both the actual gzip stream named by the
    * attribute and that established zlib variant.
    */
  private def decompressGifti(
    bytes: Array[Byte],
    expectedBytes: Int
  ): Either[GiftiError, Array[Byte]] =
    val isGzip = bytes.length >= 2 && (bytes(0) & 0xff) == 0x1f && (bytes(1) & 0xff) == 0x8b
    if isGzip then decompress(new GZIPInputStream(_), bytes, expectedBytes, "GZip")
    else decompress(new InflaterInputStream(_), bytes, expectedBytes, "zlib")

  private def decompress(
    stream: ByteArrayInputStream => InputStream,
    bytes: Array[Byte],
    expectedBytes: Int,
    label: String
  ): Either[GiftiError, Array[Byte]] =
    try
      val input = stream(new ByteArrayInputStream(bytes))
      try
        val output = new ByteArrayOutputStream(math.min(expectedBytes, 8192))
        val buffer = Array.ofDim[Byte](8192)
        var total = 0
        var error: GiftiError | Null = null
        var n = input.read(buffer)
        while n >= 0 && error == null do
          if total > expectedBytes - n then
            error = GiftiError.DecodeFailure(
              s"invalid $label payload: decompressed data exceed the expected $expectedBytes bytes"
            )
          else
            output.write(buffer, 0, n)
            total += n
            n = input.read(buffer)
        error match
          case found: GiftiError => Left(found)
          case null =>
            val result = output.toByteArray
            if result.length == expectedBytes then Right(result)
            else
              Left(
                GiftiError.DecodeFailure(
                  s"invalid $label payload: decompressed ${result.length} bytes but expected $expectedBytes"
                )
              )
      finally input.close()
    catch
      case NonFatal(error) =>
        Left(
          GiftiError.DecodeFailure(
            s"invalid $label payload: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
          )
        )
