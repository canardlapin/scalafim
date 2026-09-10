package scalafim.surface.view.javafx

import java.nio.IntBuffer
import scalafim.surface.view.*

/** A fixed native topology. Colour anchors may rotate without changing it.
  * Original vertices stay shared; only fallback faces add three edge midpoints.
  */
private[javafx] final class JavaFxAdaptiveLayout private[javafx] (
    private val starts: Array[Int], private val splitIndices: Array[Int], val splitCount: Int):
  def renderedFaces: Int = starts.last
  def start(face: Int): Int = starts(face)
  def split(face: Int): Boolean = splitIndices(face) >= 0
  def midpointOffset(face: Int): Int = splitIndices(face) * 3
  def sourceFace(rendered: Int): Int =
    var lo = 0
    var hi = starts.length - 1
    while lo + 1 < hi do
      val mid = (lo + hi) >>> 1
      if starts(mid) <= rendered then lo = mid else hi = mid
    lo

private[javafx] object JavaFxAdaptiveAtlas:
  /** The fourth texel must be representable in every channel, with no clamp. */
  private def fits(p: Int, q: Int, r: Int): Boolean =
    var shift = 8
    while shift <= 24 do
      val d = ((q >>> shift) & 255) + ((r >>> shift) & 255) - ((p >>> shift) & 255)
      if d < 0 || d > 255 then return false
      shift += 8
    true

  def anchor(a: Int, b: Int, c: Int): Int =
    if fits(a,b,c) then 0 else if fits(b,c,a) then 1 else if fits(c,a,b) then 2 else -1

  private def faceAnchor(indices: IntBufferView, colors: Array[Int], face: Int): Int =
    val i = face * 3
    anchor(colors(indices(i)), colors(indices(i+1)), colors(indices(i+2)))

  def renderedFaces(indices: IntBufferView, colors: Array[Int]): Long =
    var face = 0
    var count = 0L
    while face < indices.length / 3 do
      count += (if faceAnchor(indices, colors, face) < 0 then 4 else 1)
      face += 1
    count

  def layout(indices: IntBufferView, colors: Array[Int], first: Int, count: Int): JavaFxAdaptiveLayout =
    val starts = new Array[Int](count + 1)
    val splits = Array.fill(count)(-1)
    var splitCount = 0
    var face = 0
    while face < count do
      val split = faceAnchor(indices, colors, first + face) < 0
      if split then
        splits(face) = splitCount
        splitCount += 1
      starts(face + 1) = starts(face) + (if split then 4 else 1)
      face += 1
    new JavaFxAdaptiveLayout(starts, splits, splitCount)

  def matches(layout: JavaFxAdaptiveLayout, indices: IntBufferView, colors: Array[Int], first: Int, count: Int): Boolean =
    var face = 0
    while face < count do
      if layout.split(face) != (faceAnchor(indices, colors, first + face) < 0) then return false
      face += 1
    true

  def write(pixels: IntBuffer, width: Int, x: Int, y: Int, a: Int, b: Int, c: Int): Unit =
    val pivot = anchor(a,b,c)
    if pivot < 0 then JavaFxAffineAtlas.write(pixels,width,x,y,a,b,c)
    else
      val p = if pivot == 0 then a else if pivot == 1 then b else c
      val q = if pivot == 0 then b else if pivot == 1 then c else a
      val r = if pivot == 0 then c else if pivot == 1 then a else b
      def fourth(shift: Int): Int = ((q >>> shift) & 255) + ((r >>> shift) & 255) - ((p >>> shift) & 255)
      val d = 0xff000000 | (fourth(24) << 16) | (fourth(16) << 8) | fourth(8)
      // The unused part of the fixed 4x4 block is edge-extended. No extra atlas
      // allocation is needed when this face subsequently requires subdivision.
      var row = 0
      while row < 4 do
        var col = 0
        while col < 4 do
          val value = if row == 0 then (if col == 0 then (r >>> 8) | 0xff000000 else d)
            else if col == 0 then (p >>> 8) | 0xff000000 else (q >>> 8) | 0xff000000
          pixels.put((y+row)*width+x+col,value)
          col += 1
        row += 1

  def coordinates(packet: SurfaceMeshPacket, first: Int, count: Int, atlas: JavaFxFaceAtlas,
      colors: Array[Int], layout: JavaFxAdaptiveLayout): Array[Float] =
    val result = new Array[Float](layout.renderedFaces * 6)
    val columns = atlas.width / 4
    var face = 0
    while face < count do
      val pivot = faceAnchor(packet.indices,colors,first+face)
      val offset = (first+face)*3
      val constant = colors(packet.indices(offset)) == colors(packet.indices(offset+1)) &&
        colors(packet.indices(offset)) == colors(packet.indices(offset+2))
      var child = 0
      val children = if layout.split(face) then 4 else 1
      while child < children do
        val x = (face % columns)*4 + (child % 2)*2
        val y = (face / columns)*4 + (child / 2)*2
        var corner = 0
        while corner < 3 do
          val mapped = if children == 1 then (corner - pivot + 3) % 3 else corner
          val target = (layout.start(face)+child)*6 + corner*2
          result(target) = (x + (if constant then 1f else if mapped == 1 then 1.5f else 0.5f)) / atlas.width
          result(target+1) = (y + (if constant then 1f else if mapped == 2 then 0.5f else 1.5f)) / atlas.height
          corner += 1
        child += 1
      face += 1
    result

  private def baseVertices(packet: SurfaceMeshPacket, count: Int): Int =
    if packet.constantPartition.isEmpty then packet.positions.length / 3 else count * 3

  def attributes(packet: SurfaceMeshPacket, source: FloatBufferView, first: Int, count: Int,
      layout: JavaFxAdaptiveLayout): Array[Float] =
    val base = baseVertices(packet,count)
    val result = new Array[Float]((base + layout.splitCount*3)*3)
    val sourceStart = if packet.constantPartition.isEmpty then 0 else first*9
    Array.copy(source.unsafeArray,sourceStart,result,0,base*3)
    var face = 0
    while face < count do
      if layout.split(face) then
        var edge = 0
        while edge < 3 do
          val a = packet.indices((first+face)*3+edge)*3
          val b = packet.indices((first+face)*3+(edge+1)%3)*3
          val target = (base+layout.midpointOffset(face)+edge)*3
          var axis = 0
          while axis < 3 do
            result(target+axis) = ((source(a+axis).toDouble+source(b+axis))*0.5).toFloat
            axis += 1
          edge += 1
      face += 1
    result

  def faces(packet: SurfaceMeshPacket, first: Int, count: Int, layout: JavaFxAdaptiveLayout): Array[Int] =
    val result = new Array[Int](layout.renderedFaces*9)
    val base = baseVertices(packet,count)
    val sourceOffset = if packet.constantPartition.isEmpty then 0 else first*3
    val children = Array(0,3,5,1,4,3,2,5,4,3,4,5)
    var face = 0
    while face < count do
      val corners = if layout.split(face) then 12 else 3
      var corner = 0
      while corner < corners do
        val local = if corners == 3 then corner else children(corner)
        val vertex = if local < 3 then packet.indices((first+face)*3+local)-sourceOffset
          else base+layout.midpointOffset(face)+local-3
        val target = layout.start(face)*9+corner*3
        result(target) = vertex
        result(target+1) = vertex
        result(target+2) = layout.start(face)*3+corner
        corner += 1
      face += 1
    result
