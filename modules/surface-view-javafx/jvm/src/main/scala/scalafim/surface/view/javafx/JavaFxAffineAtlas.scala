package scalafim.surface.view.javafx

import java.nio.IntBuffer
import scalafim.surface.view.*

/** Opaque final-colour interpolation, subdivided only in the native adapter.
  * Four child tiles share a 4x4 block. Their fourth texels are original colours
  * or unrounded edge averages, so bilinear interpolation is affine before the
  * final 8-bit quantization. This encoding still requires a no-mipmap sampler
  * and centroid texture interpolation when multisample antialiasing is enabled.
  */
private[javafx] object JavaFxAffineAtlas:
  private val childCorners = Array(0, 3, 5, 1, 4, 3, 2, 5, 4, 3, 4, 5)

  def opaque(colors: Map[SurfaceId, Array[Int]]): Boolean =
    colors.valuesIterator.forall(_.forall(color => (color & 0xff) == 255))

  private def argb(color: Int): Int = (color >>> 8) | 0xff000000

  private def midpoint(a: Int, b: Int): Int =
    def channel(shift: Int): Int =
      // Sum the original channels before the only rounding step.
      (((a >>> shift) & 0xff) + ((b >>> shift) & 0xff) + 1) / 2
    0xff000000 | (channel(24) << 16) | (channel(16) << 8) | channel(8)

  def write(pixels: IntBuffer, width: Int, x: Int, y: Int, a: Int, b: Int, c: Int): Unit =
    val ca = argb(a)
    val cb = argb(b)
    val cc = argb(c)
    val ab = midpoint(a, b)
    val bc = midpoint(b, c)
    val ac = midpoint(a, c)
    def tile(tx: Int, ty: Int, p: Int, q: Int, r: Int, d: Int): Unit =
      pixels.put(ty * width + tx, r)
      pixels.put(ty * width + tx + 1, d)
      pixels.put((ty + 1) * width + tx, p)
      pixels.put((ty + 1) * width + tx + 1, q)
    tile(x, y, ca, ab, ac, bc)
    tile(x + 2, y, cb, bc, ab, ac)
    tile(x, y + 2, cc, ac, bc, ab)
    tile(x + 2, y + 2, ab, bc, ac, cc)

  def coordinates(packet: SurfaceMeshPacket, first: Int, count: Int,
      atlas: JavaFxFaceAtlas, colors: Array[Int]): Array[Float] =
    val result = new Array[Float](count * 24)
    val columns = atlas.width / 4
    var face = 0
    while face < count do
      val offset = (first + face) * 3
      val a = colors(packet.indices(offset))
      val constant = a == colors(packet.indices(offset + 1)) && a == colors(packet.indices(offset + 2))
      var child = 0
      while child < 4 do
        val x = (face % columns) * 4 + (child % 2) * 2
        val y = (face / columns) * 4 + (child / 2) * 2
        val start = (face * 4 + child) * 6
        val left = (x + 0.5f) / atlas.width
        val right = (x + 1.5f) / atlas.width
        val top = (y + 0.5f) / atlas.height
        val bottom = (y + 1.5f) / atlas.height
        result(start) = left
        result(start + 1) = bottom
        result(start + 2) = right
        result(start + 3) = bottom
        result(start + 4) = left
        result(start + 5) = top
        if constant then
          var corner = 0
          while corner < 3 do
            result(start + corner * 2) = (x + 1f) / atlas.width
            result(start + corner * 2 + 1) = (y + 1f) / atlas.height
            corner += 1
        child += 1
      face += 1
    result

  /** Six local vertices per source face: a,b,c,ab,bc,ca. Shared child points
    * retain the source triangle's winding and affine geometry/normal field.
    */
  def attributes(source: FloatBufferView, indices: IntBufferView, first: Int, count: Int): Array[Float] =
    val result = new Array[Float](count * 18)
    var face = 0
    while face < count do
      val offset = (first + face) * 3
      val a = indices(offset) * 3
      val b = indices(offset + 1) * 3
      val c = indices(offset + 2) * 3
      val target = face * 18
      var axis = 0
      while axis < 3 do
        result(target + axis) = source(a + axis)
        result(target + 3 + axis) = source(b + axis)
        result(target + 6 + axis) = source(c + axis)
        result(target + 9 + axis) = ((source(a + axis).toDouble + source(b + axis)) * 0.5).toFloat
        result(target + 12 + axis) = ((source(b + axis).toDouble + source(c + axis)) * 0.5).toFloat
        result(target + 15 + axis) = ((source(c + axis).toDouble + source(a + axis)) * 0.5).toFloat
        axis += 1
      face += 1
    result

  def faces(count: Int): Array[Int] =
    val result = new Array[Int](count * 36)
    var face = 0
    while face < count do
      var corner = 0
      while corner < 12 do
        val vertex = face * 6 + childCorners(corner)
        val target = face * 36 + corner * 3
        result(target) = vertex
        result(target + 1) = vertex
        result(target + 2) = face * 12 + corner
        corner += 1
      face += 1
    result
