package scalafim.dataset.io

import java.nio.{ByteBuffer, ByteOrder}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

private[io] object NiftiByteFixture:
  def write(
      path: Path,
      datatype: Int,
      bitpix: Int,
      rawValues: Vector[Double],
      dims: Vector[Int] = Vector(2, 1, 1, 2),
      byteOrder: ByteOrder = ByteOrder.LITTLE_ENDIAN,
      slope: Float = 1.0f,
      intercept: Float = 0.0f,
      voxOffset: Int = 352
  ): Path =
    require(dims.length == 3 || dims.length == 4)
    require(voxOffset >= 352)
    val bytesPerValue = bitpix / 8
    val bytes = new Array[Byte](voxOffset + rawValues.length * bytesPerValue)
    val buffer = ByteBuffer.wrap(bytes).order(byteOrder)

    buffer.putInt(0, 348)
    buffer.putShort(40, dims.length.toShort)
    var index = 0
    while index < 7 do
      buffer.putShort(42 + index * 2, (if index < dims.length then dims(index) else 1).toShort)
      index += 1
    buffer.putShort(70, datatype.toShort)
    buffer.putShort(72, bitpix.toShort)
    buffer.putFloat(76, 1.0f)
    buffer.putFloat(80, 1.0f)
    buffer.putFloat(84, 1.0f)
    buffer.putFloat(88, 1.0f)
    if dims.length == 4 then buffer.putFloat(92, 1.0f)
    buffer.putFloat(108, voxOffset.toFloat)
    buffer.putFloat(112, slope)
    buffer.putFloat(116, intercept)
    buffer.putShort(254, 1.toShort)
    index = 0
    while index < 4 do
      buffer.putFloat(280 + index * 4, if index == 0 then 1.0f else 0.0f)
      buffer.putFloat(296 + index * 4, if index == 1 then 1.0f else 0.0f)
      buffer.putFloat(312 + index * 4, if index == 2 then 1.0f else 0.0f)
      index += 1
    val magic = "n+1".getBytes(StandardCharsets.US_ASCII)
    buffer.put(344, magic(0))
    buffer.put(345, magic(1))
    buffer.put(346, magic(2))

    buffer.position(voxOffset)
    rawValues.foreach { value =>
      datatype match
        case 2  => buffer.put(value.toInt.toByte)
        case 4  => buffer.putShort(value.toInt.toShort)
        case 8  => buffer.putInt(value.toInt)
        case 16 => buffer.putFloat(value.toFloat)
        case 64 => buffer.putDouble(value)
        case _ =>
          var byte = 0
          while byte < bytesPerValue do
            buffer.put(0.toByte)
            byte += 1
    }

    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, bytes)
    path
