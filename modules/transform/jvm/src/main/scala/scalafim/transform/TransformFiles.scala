package scalafim.transform

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.zip.GZIPInputStream

import scalafim.transform.itk.ItkHdf5Container
import scalafim.transform.x5.X5Container

import scala.util.control.NonFatal

/** A transform file read from disk: its decoded native model and the provenance of the exact bytes. */
final case class LoadedTransformFile(native: NativeTransform, asset: AssetRef):
  def format: TransformFormat = native.format

/** JVM file entry point: read, gunzip when compressed, detect by content, and decode (HDF5 through jHDF). */
object TransformFiles:
  def load(path: Path): Either[TransformIoError, LoadedTransformFile] =
    load(path, None)

  /** Load with an explicit format, e.g. when content and name leave the format ambiguous. */
  def load(path: Path, format: Option[TransformFormat]): Either[TransformIoError, LoadedTransformFile] =
    for
      stored <- read(path)
      bytes <- gunzipIfNeeded(stored)
      name = Option(path.getFileName).map(_.toString.stripSuffix(".gz"))
      source = TransformSource.Binary(IArray.unsafeFromArray(bytes))
      detected <- format.fold(TransformDetection.detect(source, name))(Right(_))
      native <- detected match
        case TransformFormat.ItkHdf5 => ItkHdf5Container.read(bytes).map(NativeTransform.ItkHdf5(_))
        case TransformFormat.X5      => X5Container.read(bytes).map(NativeTransform.X5(_))
        case other                   => Transforms.decode(source, other)
    yield LoadedTransformFile(native, AssetRef(path.toString, Some(sha256(stored))))

  private def read(path: Path): Either[TransformIoError, Array[Byte]] =
    try Right(Files.readAllBytes(path))
    catch case NonFatal(error) => Left(TransformIoError.Undetectable(s"cannot read $path: ${Option(error.getMessage).getOrElse(error.getClass.getSimpleName)}"))

  private def gunzipIfNeeded(bytes: Array[Byte]): Either[TransformIoError, Array[Byte]] =
    if bytes.length >= 2 && (bytes(0) & 0xff) == 0x1f && (bytes(1) & 0xff) == 0x8b then
      try
        val stream = new GZIPInputStream(new ByteArrayInputStream(bytes))
        try Right(stream.readAllBytes())
        finally stream.close()
      catch case NonFatal(error) => Left(TransformIoError.Malformed("gzip", Option(error.getMessage).getOrElse("corrupt stream")))
    else Right(bytes)

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
