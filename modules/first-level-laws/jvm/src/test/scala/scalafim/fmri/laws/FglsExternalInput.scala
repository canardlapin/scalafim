package scalafim.fmri.laws

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}

private[laws] object FglsExternalInput:
  def text(path: String): String = Files.readString(Path.of(path))
  def doubles(path: String): Array[Double] =
    val bytes = Files.readAllBytes(Path.of(path))
    require(bytes.length % 8 == 0)
    val buffer = ByteBuffer.wrap(bytes).order(ByteOrder.LITTLE_ENDIAN)
    Array.fill(bytes.length / 8)(buffer.getDouble())
