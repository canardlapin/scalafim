package scalafim.surface.io

import scalafim.surface.gifti.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.control.NonFatal

object GiftiReader:
  val MaximumXmlBytes: Int = GiftiXmlParser.MaximumXmlCharacters

  def parseString(xml: String): Either[GiftiError, GiftiDocument] =
    GiftiXmlParser.parseString(xml)

  /** Parse raw `.gii` bytes or an outer gzip-compressed `.gii.gz` stream.
    * The asynchronous boundary is required by the browser-native
    * `DecompressionStream` used in workers and window contexts.
    */
  def read(bytes: Uint8Array): Future[Either[GiftiError, GiftiDocument]] =
    if isGzip(bytes) then
      GiftiJsCompression
        .decompressGzip(bytes, MaximumXmlBytes, "outer GZip")
        .map(_.flatMap(parseUtf8Bytes))
    else Future.successful(parseUtf8(bytes))

  def doublePayload(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiPayload[Double]]] =
    resolvedData(array).map(_.flatMap(GiftiPayloadDecoder.doublePayload(array, _)))

  def doubleMatrix(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiMatrix[Double]]] =
    doublePayload(array).map(_.flatMap(GiftiPayload.requireMatrix))

  def intPayload(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiPayload[Int]]] =
    resolvedData(array).map(_.flatMap(GiftiPayloadDecoder.intPayload(array, _)))

  def intVector(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiVector[Int]]] =
    intPayload(array).map(_.flatMap(GiftiPayload.requireVector))

  def intMatrix(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiMatrix[Int]]] =
    intPayload(array).map(_.flatMap(GiftiPayload.requireMatrix))

  def bytePayload(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiPayload[Int]]] =
    resolvedData(array).map(_.flatMap(GiftiPayloadDecoder.bytePayload(array, _)))

  def doubleData(array: GiftiDataArray): Future[Either[GiftiError, Array[Double]]] =
    doublePayload(array).map(_.map(_.values.toArray))

  def intData(array: GiftiDataArray): Future[Either[GiftiError, Array[Int]]] =
    intPayload(array).map(_.map(_.values.toArray))

  def byteData(array: GiftiDataArray): Future[Either[GiftiError, Array[Int]]] =
    bytePayload(array).map(_.map(_.values.toArray))

  private def resolvedData(
    array: GiftiDataArray
  ): Future[Either[GiftiError, GiftiPayloadDecoder.EncodedData]] =
    GiftiPayloadDecoder.encodedData(array) match
      case Left(error) => Future.successful(Left(error))
      case Right(GiftiPayloadDecoder.EncodedData.Compressed(bytes)) =>
        GiftiPayloadDecoder.expectedByteCount(array) match
          case Left(error) => Future.successful(Left(error))
          case Right(expected) =>
            GiftiJsCompression
              .decompressGifti(bytes, expected)
              .map(_.map(GiftiPayloadDecoder.EncodedData.Binary.apply))
      case Right(ready) => Future.successful(Right(ready))

  private def parseUtf8Bytes(bytes: Array[Byte]): Either[GiftiError, GiftiDocument] =
    parseUtf8(uint8Array(bytes))

  private def parseUtf8(bytes: Uint8Array): Either[GiftiError, GiftiDocument] =
    if bytes.length > MaximumXmlBytes then
      Left(GiftiError.InvalidDocument(s"GIFTI XML input exceeds the $MaximumXmlBytes byte limit"))
    else
      try
        val constructor = js.Dynamic.global.selectDynamic("TextDecoder")
        if js.isUndefined(constructor) || constructor == null then
          Left(GiftiError.Xml("TextDecoder is unavailable in this Scala.js runtime"))
        else
          val decoder = js.Dynamic.newInstance(constructor)(
            "utf-8",
            js.Dynamic.literal(fatal = true)
          )
          val xml = decoder.applyDynamic("decode")(bytes).asInstanceOf[String]
          parseString(xml)
      catch
        case NonFatal(error) =>
          Left(
            GiftiError.Xml(
              s"invalid UTF-8 input: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
            )
          )

  private def isGzip(bytes: Uint8Array): Boolean =
    bytes.length >= 2 && bytes(0) == 0x1f && bytes(1) == 0x8b

  private def uint8Array(bytes: Array[Byte]): Uint8Array =
    val out = new Uint8Array(bytes.length)
    var index = 0
    while index < bytes.length do
      out(index) = ((bytes(index) & 0xff).toShort)
      index += 1
    out
