package scalafim.surface.view.raster.spike

import java.awt.image.BufferedImage
import java.io.ByteArrayOutputStream
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import javax.imageio.ImageIO

import intaglio.*

/** PNG and hashing helpers shared by the spike mains. */
object SpikeImages:
  def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  /** SHA-256 over the RGBA32 packed pixels in scanline order. */
  def pixelSha256(image: RasterImage): String =
    val digest = MessageDigest.getInstance("SHA-256")
    val buffer = ByteBuffer.allocate(image.width * 4).order(ByteOrder.BIG_ENDIAN)
    var y = 0
    while y < image.height do
      buffer.clear()
      var x = 0
      while x < image.width do
        buffer.putInt(image.pixelUnsafe(x, y).toPackedInt)
        x += 1
      digest.update(buffer.array())
      y += 1
    digest.digest().map(byte => f"${byte & 0xff}%02x").mkString

  def toArgb(image: RasterImage): Array[Int] =
    val pixels = new Array[Int](image.width * image.height)
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        val pixel = image.pixelUnsafe(x, y)
        pixels(y * image.width + x) = (pixel.alpha << 24) | (pixel.red << 16) | (pixel.green << 8) | pixel.blue
        x += 1
      y += 1
    pixels

  def pngBytes(width: Int, height: Int, argb: Array[Int]): Array[Byte] =
    val buffered = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    buffered.setRGB(0, 0, width, height, argb, 0, width)
    val output = new ByteArrayOutputStream(width * height)
    require(ImageIO.write(buffered, "png", output), "PNG writer unavailable")
    output.toByteArray

  def pngBytes(image: RasterImage): Array[Byte] = pngBytes(image.width, image.height, toArgb(image))

  def writePng(path: Path, image: RasterImage): Array[Byte] =
    val bytes = pngBytes(image)
    Files.write(path, bytes)
    bytes

  def readArgb(path: Path): (Int, Int, Array[Int]) =
    val image = ImageIO.read(path.toFile)
    require(image != null, s"cannot read $path")
    val width = image.getWidth
    val height = image.getHeight
    (width, height, image.getRGB(0, 0, width, height, null, 0, width))

  def rasterFromArgb(width: Int, height: Int, argb: Array[Int]): RasterImage =
    RasterImage.tabulate(RasterDimensions.unsafe(width, height)): (x, y) =>
      val value = argb(y * width + x)
      Rgba32.unsafe((value >>> 16) & 0xff, (value >>> 8) & 0xff, value & 0xff, (value >>> 24) & 0xff)
