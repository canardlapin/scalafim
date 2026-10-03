package scalafim.surface.scenarios

import java.io.ByteArrayInputStream
import java.security.MessageDigest
import java.util.zip.GZIPInputStream

private[scenarios] object SurfaceRealFixturePlatform:
  def read(resource: String): Array[Byte] =
    val stream = Option(getClass.getResourceAsStream(s"/$resource"))
      .getOrElse(throw new java.io.FileNotFoundException(s"missing surface fixture: $resource"))
    try stream.readAllBytes()
    finally stream.close()

  def gunzip(bytes: Array[Byte]): Array[Byte] =
    val stream = new GZIPInputStream(new ByteArrayInputStream(bytes))
    try stream.readAllBytes()
    finally stream.close()

  def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 0xff}%02x").mkString
