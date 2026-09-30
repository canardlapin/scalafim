package scalafim.transform.oracle

import java.io.ByteArrayInputStream
import java.util.zip.GZIPInputStream

private[oracle] object OracleFixturePlatform:
  def read(resource: String): Array[Byte] =
    val stream = Option(getClass.getResourceAsStream(s"/$resource"))
      .getOrElse(throw new java.io.FileNotFoundException(s"missing oracle fixture on the test classpath: $resource"))
    try stream.readAllBytes()
    finally stream.close()

  def gunzip(bytes: Array[Byte]): Array[Byte] =
    val stream = new GZIPInputStream(new ByteArrayInputStream(bytes))
    try stream.readAllBytes()
    finally stream.close()
