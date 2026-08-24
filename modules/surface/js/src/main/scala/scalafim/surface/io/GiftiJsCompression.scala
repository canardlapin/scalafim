package scalafim.surface.io

import scalafim.surface.gifti.GiftiError

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.scalajs.js
import scala.scalajs.js.Thenable.Implicits.*
import scala.scalajs.js.typedarray.Uint8Array
import scala.util.control.NonFatal

private[io] object GiftiJsCompression:

  def decompressGifti(
    bytes: Array[Byte],
    expectedBytes: Int
  ): Future[Either[GiftiError, Array[Byte]]] =
    val input = uint8Array(bytes)
    val isGzip = input.length >= 2 && input(0) == 0x1f && input(1) == 0x8b
    val format = if isGzip then "gzip" else "deflate"
    val label = if isGzip then "GZip" else "zlib"
    decompress(input, format, expectedBytes, Some(expectedBytes), label)

  def decompressGzip(
    bytes: Uint8Array,
    maximumBytes: Int,
    label: String
  ): Future[Either[GiftiError, Array[Byte]]] =
    decompress(bytes, "gzip", maximumBytes, None, label)

  private def decompress(
    bytes: Uint8Array,
    format: String,
    maximumBytes: Int,
    exactBytes: Option[Int],
    label: String
  ): Future[Either[GiftiError, Array[Byte]]] =
    try
      val constructor = js.Dynamic.global.selectDynamic("DecompressionStream")
      if js.isUndefined(constructor) || constructor == null then
        Future.successful(
          Left(GiftiError.DecodeFailure(s"invalid $label payload: DecompressionStream is unavailable"))
        )
      else
        val stream = js.Dynamic.newInstance(constructor)(format)
        val writer = stream.selectDynamic("writable").applyDynamic("getWriter")()
        val reader = stream.selectDynamic("readable").applyDynamic("getReader")()
        val writing =
          promise[js.Any](writer.applyDynamic("write")(bytes))
            .flatMap(_ => promise[js.Any](writer.applyDynamic("close")()))
        val chunks = ArrayBuffer.empty[Uint8Array]
        val reading = readAll(reader, writer, chunks, 0, maximumBytes, exactBytes, label)
        reading
          .recover { case NonFatal(error) => Left(decodeFailure(label, error)) }
          .flatMap {
            case failure @ Left(_) =>
              writing
                .map[Either[GiftiError, Array[Byte]]](_ => failure)
                .recover { case NonFatal(_) => failure }
            case success @ Right(_) =>
              writing.map[Either[GiftiError, Array[Byte]]](_ => success)
          }
          .recover { case NonFatal(error) => Left(decodeFailure(label, error)) }
    catch
      case NonFatal(error) => Future.successful(Left(decodeFailure(label, error)))

  private def readAll(
    reader: js.Dynamic,
    writer: js.Dynamic,
    chunks: ArrayBuffer[Uint8Array],
    total: Int,
    maximumBytes: Int,
    exactBytes: Option[Int],
    label: String
  ): Future[Either[GiftiError, Array[Byte]]] =
    promise[js.Dynamic](reader.applyDynamic("read")()).flatMap { result =>
      if result.selectDynamic("done").asInstanceOf[Boolean] then
        exactBytes match
          case Some(expected) if total != expected =>
            Future.successful(
              Left(
                GiftiError.DecodeFailure(
                  s"invalid $label payload: decompressed $total bytes but expected $expected"
                )
              )
            )
          case _ => Future.successful(Right(copyChunks(chunks, total)))
      else
        val chunk = result.selectDynamic("value").asInstanceOf[Uint8Array]
        if chunk.length > maximumBytes - total then
          val reason = s"$label output limit exceeded"
          settle(reader.applyDynamic("cancel")(reason))
            .zip(settle(writer.applyDynamic("abort")(reason)))
            .map: _ =>
              Left(
                GiftiError.DecodeFailure(
                  s"invalid $label payload: decompressed data exceed the $maximumBytes byte limit"
                )
              )
        else
          chunks += chunk
          readAll(reader, writer, chunks, total + chunk.length, maximumBytes, exactBytes, label)
    }

  private def copyChunks(chunks: ArrayBuffer[Uint8Array], total: Int): Array[Byte] =
    val out = Array.ofDim[Byte](total)
    var offset = 0
    var chunkIndex = 0
    while chunkIndex < chunks.length do
      val chunk = chunks(chunkIndex)
      var index = 0
      while index < chunk.length do
        out(offset + index) = chunk(index).toByte
        index += 1
      offset += chunk.length
      chunkIndex += 1
    out

  private def uint8Array(bytes: Array[Byte]): Uint8Array =
    val out = new Uint8Array(bytes.length)
    var index = 0
    while index < bytes.length do
      out(index) = ((bytes(index) & 0xff).toShort)
      index += 1
    out

  private def promise[A](value: js.Dynamic): Future[A] =
    value.asInstanceOf[js.Promise[A]].toFuture

  private def settle(value: js.Dynamic): Future[Unit] =
    promise[js.Any](value).map(_ => ()).recover { case NonFatal(_) => () }

  private def decodeFailure(label: String, error: Throwable): GiftiError =
    GiftiError.DecodeFailure(
      s"invalid $label payload: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"
    )
