package scalafim.surface.view.raster

import intaglio.*

/** Reference trilinear sampler. Mips use area averaging in premultiplied byte
  * space; native automatic mip generation must be qualified against fixtures
  * on each admitted device, rather than assumed to use this reduction kernel.
  */
private[raster] final class SurfaceTextureSampler(image: RasterImage):
  private final case class Level(width: Int, height: Int, pixels: Array[Double])
  private val levels: Vector[Level] =
    val pixels = new Array[Double](image.width * image.height * 4)
    var index = 0
    while index < image.width * image.height do
      val color = image.pixelUnsafe(index % image.width, index / image.width)
      pixels(index * 4) = math.round(color.red * color.alpha / 255.0).toDouble
      pixels(index * 4 + 1) = math.round(color.green * color.alpha / 255.0).toDouble
      pixels(index * 4 + 2) = math.round(color.blue * color.alpha / 255.0).toDouble
      pixels(index * 4 + 3) = color.alpha
      index += 1
    var chain = Vector(Level(image.width, image.height, pixels))
    while chain.last.width > 1 || chain.last.height > 1 do chain :+= reduce(chain.last)
    chain

  def sample(u: Double, v: Double, duDx: Double, duDy: Double, dvDx: Double, dvDy: Double): Rgba32 =
    val footprint = math.max(math.hypot(duDx * image.width, dvDx * image.height),
      math.hypot(duDy * image.width, dvDy * image.height))
    val lod = (math.log(math.max(1.0, footprint)) / math.log(2.0)).min(levels.length - 1)
    val lower = math.floor(lod).toInt
    val upper = (lower + 1).min(levels.length - 1)
    val fraction = lod - lower
    def channel(c: Int): Double = bilinear(levels(lower), u, v, c) * (1 - fraction) + bilinear(levels(upper), u, v, c) * fraction
    val alpha = channel(3)
    def byte(value: Double) = math.round(value).toInt.max(0).min(255)
    if alpha <= 0 then Rgba32.unsafe(0, 0, 0, 0)
    else Rgba32.unsafe(byte(channel(0) * 255 / alpha), byte(channel(1) * 255 / alpha),
      byte(channel(2) * 255 / alpha), byte(alpha))

  private def bilinear(level: Level, u: Double, v: Double, channel: Int): Double =
    val px = u * level.width - .5
    val py = v * level.height - .5
    val x = math.floor(px).toInt
    val y = math.floor(py).toInt
    val fx = px - x
    val fy = py - y
    def at(x: Int, y: Int): Double =
      val xx = ((x % level.width) + level.width) % level.width
      val yy = ((y % level.height) + level.height) % level.height
      level.pixels((yy * level.width + xx) * 4 + channel)
    (at(x, y) * (1 - fx) + at(x + 1, y) * fx) * (1 - fy) +
      (at(x, y + 1) * (1 - fx) + at(x + 1, y + 1) * fx) * fy

  private def reduce(source: Level): Level =
    val width = (source.width / 2).max(1)
    val height = (source.height / 2).max(1)
    val pixels = new Array[Double](width * height * 4)
    var y = 0
    while y < height do
      val top = y.toDouble * source.height / height
      val bottom = (y + 1).toDouble * source.height / height
      var x = 0
      while x < width do
        val left = x.toDouble * source.width / width
        val right = (x + 1).toDouble * source.width / width
        var row = math.floor(top).toInt
        while row < math.ceil(bottom).toInt do
          val dy = math.min(bottom, row + 1) - math.max(top, row)
          var column = math.floor(left).toInt
          while column < math.ceil(right).toInt do
            val weight = dy * (math.min(right, column + 1) - math.max(left, column)) / ((right - left) * (bottom - top))
            var channel = 0
            while channel < 4 do
              pixels((y * width + x) * 4 + channel) += source.pixels((row * source.width + column) * 4 + channel) * weight
              channel += 1
            column += 1
          row += 1
        var channel = 0
        while channel < 4 do
          val index = (y * width + x) * 4 + channel
          pixels(index) = math.round(pixels(index)).toDouble
          channel += 1
        x += 1
      y += 1
    Level(width, height, pixels)
