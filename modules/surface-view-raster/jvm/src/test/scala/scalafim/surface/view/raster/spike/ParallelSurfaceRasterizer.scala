package scalafim.surface.view.raster.spike

import java.util.concurrent.{Callable, Executors}
import scala.jdk.CollectionConverters.*

import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

final case class ParallelRasterResult(image: RasterImage, receipt: SurfaceRasterReceipt)

/** Spike-only banded parallel variant of `SurfaceRasterizer` for the legacy
  * vertex-colour path (no fragment surfaces, no render partitions).
  *
  * Phase 1 transforms vertices once per mesh and sets up triangles in
  * contiguous face chunks (parallel); phase 2 rasterises horizontal bands of
  * scanlines (parallel) over the same triangle order. Per-pixel arithmetic
  * and triangle order are identical to the sequential interpreter, so the
  * output pixels are byte-identical to `SurfaceRasterizer.render`; the suite
  * checks this on a fixture and the figure harness records digests.
  */
object ParallelSurfaceRasterizer:
  private val Fields = 33 // 3 screen vertices x 11 doubles

  def render(plan: SurfaceRenderPlan, dimensions: RasterDimensions, threads: Int): ParallelRasterResult =
    require(plan.fragmentSurfaces.isEmpty, "banded parallel spike supports the vertex-colour path only")
    require(plan.meshes.forall(m => m.nearestPartition.isEmpty && m.constantPartition.isEmpty && m.sourceVertices.isEmpty),
      "banded parallel spike supports unpartitioned meshes only")
    require(plan.slots.length == plan.meshes.length && plan.validCameras, "invalid plan")
    val workers = threads.max(1)
    val pool = Executors.newFixedThreadPool(workers)
    try renderWith(plan, dimensions, pool, workers)
    finally pool.shutdown()

  private def renderWith(
    plan: SurfaceRenderPlan,
    dimensions: RasterDimensions,
    pool: java.util.concurrent.ExecutorService,
    workers: Int
  ): ParallelRasterResult =
    val style = SurfaceRasterStyle()
    val setupStarted = System.nanoTime()
    val width = dimensions.width
    val height = dimensions.height
    val pixelCount = dimensions.pixelCount
    val pixels = Array.fill(pixelCount)(style.background.toPackedInt)
    val depths = Array.fill(pixelCount)(Double.PositiveInfinity)
    val faceAt = Array.fill(pixelCount)(-1)
    val slotAt = Array.fill(pixelCount)(-1)
    val vertexAt = Array.fill(pixelCount)(-1)
    val baryA = new Array[Float](pixelCount)
    val baryB = new Array[Float](pixelCount)
    val baryC = new Array[Float](pixelCount)
    val fittedSlots = plan.viewportFit.resolve(plan.slots, width.toDouble, height.toDouble)

    def run[A](tasks: Seq[() => A]): Vector[A] =
      pool.invokeAll(tasks.map(task => new Callable[A] { def call(): A = task() }).asJava).asScala.toVector.map(_.get())

    // Phase 0: composed vertex colours, per mesh (same as the sequential interpreter).
    val composedColors = plan.meshes.map: mesh =>
      val colors = Array.fill(mesh.positions.length / 3)(style.surfaceBase.toPackedInt)
      var layerIndex = 0
      while layerIndex < plan.layers.length do
        val layer = plan.layers(layerIndex)
        if layer.surface == mesh.surface then
          var vertex = 0
          while vertex < colors.length do
            val under = Rgba32.fromPackedInt(colors(vertex))
            val over = Rgba32.fromPackedInt(layer.colors(vertex))
            colors(vertex) = layer.blendMode.composite(under, over, layer.opacity).toPackedInt
            vertex += 1
        layerIndex += 1
      colors
    val setupNanos = System.nanoTime() - setupStarted
    val renderStarted = System.nanoTime()

    // Phase 1a: per-vertex clip coordinates and lit colours (parallel over vertex ranges).
    val vertexBuffers = plan.meshes.zipWithIndex.map: (mesh, slotIndex) =>
      val count = mesh.positions.length / 3
      val buffers = new VertexBuffers(count)
      val slot = plan.slots(slotIndex)
      val camera = plan.cameraFor(slot.surface)
      val view = camera.viewMatrix
      val projection = camera.projectionMatrix
      val colors = composedColors(slotIndex)
      val chunk = math.max(1, (count + workers - 1) / workers)
      val tasks = (0 until workers).map: worker =>
        () =>
          var vertex = worker * chunk
          val end = math.min(count, vertex + chunk)
          while vertex < end do
            val offset = vertex * 3
            val x = mesh.positions(offset).toDouble
            val y = mesh.positions(offset + 1).toDouble
            val z = mesh.positions(offset + 2).toDouble
            val wx = x + slot.worldOffsetX
            val wy = y + slot.worldOffsetY
            val wz = z + slot.worldOffsetZ
            val vx = view(0) * wx + view(1) * wy + view(2) * wz + view(3) * 1.0
            val vy = view(4) * wx + view(5) * wy + view(6) * wz + view(7) * 1.0
            val vz = view(8) * wx + view(9) * wy + view(10) * wz + view(11) * 1.0
            val vw = view(12) * wx + view(13) * wy + view(14) * wz + view(15) * 1.0
            buffers.cx(vertex) = projection(0) * vx + projection(1) * vy + projection(2) * vz + projection(3) * vw
            buffers.cy(vertex) = projection(4) * vx + projection(5) * vy + projection(6) * vz + projection(7) * vw
            buffers.cz(vertex) = projection(8) * vx + projection(9) * vy + projection(10) * vz + projection(11) * vw
            buffers.cw(vertex) = projection(12) * vx + projection(13) * vy + projection(14) * vz + projection(15) * vw
            val color = Rgba32.fromPackedInt(colors(vertex))
            val light = plan.lighting match
              case SurfaceLighting.Unlit => 1.0
              case SurfaceLighting.Directional(ambient, diffuse, dx, dy, dz) =>
                val dot = math.max(0.0, mesh.normals(offset) * dx + mesh.normals(offset + 1) * dy + mesh.normals(offset + 2) * dz)
                math.min(1.0, ambient.value + diffuse.value * dot)
            buffers.red(vertex) = color.red.toDouble * light
            buffers.green(vertex) = color.green.toDouble * light
            buffers.blue(vertex) = color.blue.toDouble * light
            buffers.alpha(vertex) = color.alpha.toDouble
            vertex += 1
          ()
      run(tasks)
      buffers

    // Phase 1b: triangle setup in contiguous face chunks (parallel), preserving order.
    final class Chunk(capacity: Int):
      var data = new Array[Double](capacity * Fields)
      var meta = new Array[Int](capacity * 5) // slot, face, ia, ib, ic
      var size = 0
      var input = 0; var afterClipping = 0; var clippedAway = 0; var culled = 0
      def ensure(): Unit =
        if (size + 1) * Fields > data.length then
          data = java.util.Arrays.copyOf(data, data.length * 2)
          meta = java.util.Arrays.copyOf(meta, meta.length * 2)
      def add(slot: Int, face: Int, ia: Int, ib: Int, ic: Int, a: Array[Double], b: Array[Double], c: Array[Double]): Unit =
        ensure()
        val base = size * Fields
        System.arraycopy(a, 0, data, base, 11)
        System.arraycopy(b, 0, data, base + 11, 11)
        System.arraycopy(c, 0, data, base + 22, 11)
        val m = size * 5
        meta(m) = slot; meta(m + 1) = face; meta(m + 2) = ia; meta(m + 3) = ib; meta(m + 4) = ic
        size += 1

    val faceOffsets = plan.meshes.scanLeft(0)((total, mesh) => total + mesh.indices.length / 3)
    val totalFaces = faceOffsets.last
    val facesPerChunk = math.max(1, (totalFaces + workers * 4 - 1) / (workers * 4))
    val chunkTasks = (0 until (totalFaces + facesPerChunk - 1) / facesPerChunk).map: chunkIndex =>
      () =>
        val chunk = new Chunk(facesPerChunk + 16)
        val a = new Array[Double](11); val b = new Array[Double](11); val c = new Array[Double](11)
        var global = chunkIndex * facesPerChunk
        val end = math.min(totalFaces, global + facesPerChunk)
        while global < end do
          var slotIndex = 0
          while faceOffsets(slotIndex + 1) <= global do slotIndex += 1
          val face = global - faceOffsets(slotIndex)
          val mesh = plan.meshes(slotIndex)
          val buffers = vertexBuffers(slotIndex)
          val viewport = fittedSlots(slotIndex).viewport
          val ia = mesh.indices(face * 3); val ib = mesh.indices(face * 3 + 1); val ic = mesh.indices(face * 3 + 2)
          chunk.input += 1
          if inside(buffers, ia) && inside(buffers, ib) && inside(buffers, ic) then
            screen(buffers, ia, 1.0, 0.0, 0.0, viewport, width, height, a)
            screen(buffers, ib, 0.0, 1.0, 0.0, viewport, width, height, b)
            screen(buffers, ic, 0.0, 0.0, 1.0, viewport, width, height, c)
            chunk.afterClipping += 1
            if culledArea(a, b, c) then chunk.culled += 1 else chunk.add(slotIndex, face, ia, ib, ic, a, b, c)
          else
            val polygon = clipPolygon(plan, buffers, mesh, slotIndex, ia, ib, ic)
            if polygon.length < 3 then chunk.clippedAway += 1
            else
              var fan = 1
              while fan + 1 < polygon.length do
                chunk.afterClipping += 1
                toScreen(polygon(0), viewport, width, height, a)
                toScreen(polygon(fan), viewport, width, height, b)
                toScreen(polygon(fan + 1), viewport, width, height, c)
                if culledArea(a, b, c) then chunk.culled += 1 else chunk.add(slotIndex, face, ia, ib, ic, a, b, c)
                fan += 1
          global += 1
        chunk
    val chunks = run(chunkTasks)
    val triangleCount = chunks.iterator.map(_.size).sum
    val data = new Array[Double](triangleCount * Fields)
    val meta = new Array[Int](triangleCount * 5)
    var cursor = 0
    chunks.foreach: chunk =>
      System.arraycopy(chunk.data, 0, data, cursor * Fields, chunk.size * Fields)
      System.arraycopy(chunk.meta, 0, meta, cursor * 5, chunk.size * 5)
      cursor += chunk.size

    // Phase 2: bin triangles into scanline bands, then rasterise bands in parallel.
    val bands = math.max(1, math.min(height, workers * 8))
    val rowsPerBand = (height + bands - 1) / bands
    val bandCounts = new Array[Int](bands)
    val minRows = new Array[Int](triangleCount)
    val maxRows = new Array[Int](triangleCount)
    var t = 0
    while t < triangleCount do
      val base = t * Fields
      val ay = data(base + 1); val by = data(base + 12); val cy = data(base + 23)
      val minY = math.max(0, math.floor(math.min(ay, math.min(by, cy))).toInt)
      val maxY = math.min(height - 1, math.ceil(math.max(ay, math.max(by, cy))).toInt)
      minRows(t) = minY; maxRows(t) = maxY
      if minY <= maxY then
        var band = minY / rowsPerBand
        while band <= maxY / rowsPerBand do
          bandCounts(band) += 1
          band += 1
      t += 1
    val bandStarts = bandCounts.scanLeft(0)(_ + _)
    val bandLists = new Array[Int](bandStarts.last)
    val fill = bandStarts.clone()
    t = 0
    while t < triangleCount do
      if minRows(t) <= maxRows(t) then
        var band = minRows(t) / rowsPerBand
        while band <= maxRows(t) / rowsPerBand do
          bandLists(fill(band)) = t
          fill(band) += 1
          band += 1
      t += 1
    val bandTasks = (0 until bands).map: band =>
      () =>
        val rowStart = band * rowsPerBand
        val rowEnd = math.min(height - 1, rowStart + rowsPerBand - 1)
        var shaded = 0
        var rejected = 0
        var index = bandStarts(band)
        while index < bandStarts(band + 1) do
          val triangle = bandLists(index)
          val counts = rasterTriangle(triangle, data, meta, math.max(rowStart, minRows(triangle)), math.min(rowEnd, maxRows(triangle)),
            width, pixels, depths, faceAt, slotAt, vertexAt, baryA, baryB, baryC)
          shaded += counts._1
          rejected += counts._2
          index += 1
        (shaded, rejected)
    val bandResults = run(bandTasks)
    val renderNanos = System.nanoTime() - renderStarted
    val receipt = SurfaceRasterReceipt(
      chunks.iterator.map(_.input).sum,
      chunks.iterator.map(_.afterClipping).sum,
      chunks.iterator.map(_.clippedAway).sum,
      chunks.iterator.map(_.culled).sum,
      bandResults.iterator.map(_._1).sum,
      bandResults.iterator.map(_._2).sum,
      plan.layers.iterator.map(_.colors.length).sum,
      setupNanos,
      renderNanos
    )
    ParallelRasterResult(RasterImage.unsafeFromOwnedPackedArray(dimensions, pixels), receipt)

  private inline def edge(ax: Double, ay: Double, bx: Double, by: Double, px: Double, py: Double): Double =
    (px - ax) * (by - ay) - (py - ay) * (bx - ax)

  private def culledArea(a: Array[Double], b: Array[Double], c: Array[Double]): Boolean =
    val area = edge(a(0), a(1), b(0), b(1), c(0), c(1))
    area == 0.0 || area <= 0.0

  private def inside(buffers: VertexBuffers, vertex: Int): Boolean =
    val x = buffers.cx(vertex); val y = buffers.cy(vertex); val z = buffers.cz(vertex); val w = buffers.cw(vertex)
    x + w >= 0.0 && w - x >= 0.0 && y + w >= 0.0 && w - y >= 0.0 && z + w >= 0.0 && w - z >= 0.0

  // Field layout per screen vertex: x, y, depth, inverseW, rOverW, gOverW, bOverW, aOverW, baryAOverW, baryBOverW, baryCOverW
  private def screen(
    buffers: VertexBuffers, vertex: Int, ba: Double, bb: Double, bc: Double,
    viewport: SurfaceViewport, width: Int, height: Int, target: Array[Double]
  ): Unit =
    fill(buffers.cx(vertex), buffers.cy(vertex), buffers.cz(vertex), buffers.cw(vertex),
      buffers.red(vertex), buffers.green(vertex), buffers.blue(vertex), buffers.alpha(vertex), ba, bb, bc,
      viewport, width, height, target)

  private def toScreen(vertex: ClipVertex, viewport: SurfaceViewport, width: Int, height: Int, target: Array[Double]): Unit =
    fill(vertex.x, vertex.y, vertex.z, vertex.w, vertex.red, vertex.green, vertex.blue, vertex.alpha,
      vertex.baryA, vertex.baryB, vertex.baryC, viewport, width, height, target)

  private def fill(
    x: Double, y: Double, z: Double, w: Double,
    red: Double, green: Double, blue: Double, alpha: Double, ba: Double, bb: Double, bc: Double,
    viewport: SurfaceViewport, width: Int, height: Int, target: Array[Double]
  ): Unit =
    val inverseW = 1.0 / w
    val ndcX = x * inverseW
    val ndcY = y * inverseW
    val ndcZ = z * inverseW
    target(0) = (viewport.x + (ndcX + 1.0) * 0.5 * viewport.width) * width
    target(1) = (viewport.y + (1.0 - (ndcY + 1.0) * 0.5) * viewport.height) * height
    target(2) = (ndcZ + 1.0) * 0.5
    target(3) = inverseW
    target(4) = red * inverseW; target(5) = green * inverseW; target(6) = blue * inverseW; target(7) = alpha * inverseW
    target(8) = ba * inverseW; target(9) = bb * inverseW; target(10) = bc * inverseW

  private final class VertexBuffers(count: Int):
    val cx = new Array[Double](count); val cy = new Array[Double](count)
    val cz = new Array[Double](count); val cw = new Array[Double](count)
    val red = new Array[Double](count); val green = new Array[Double](count)
    val blue = new Array[Double](count); val alpha = new Array[Double](count)

  private final case class ClipVertex(
    worldX: Double, worldY: Double, worldZ: Double,
    x: Double, y: Double, z: Double, w: Double,
    red: Double, green: Double, blue: Double, alpha: Double,
    baryA: Double, baryB: Double, baryC: Double
  ):
    def interpolate(that: ClipVertex, fraction: Double): ClipVertex =
      def mix(a: Double, b: Double): Double = a + (b - a) * fraction
      ClipVertex(
        mix(worldX, that.worldX), mix(worldY, that.worldY), mix(worldZ, that.worldZ),
        mix(x, that.x), mix(y, that.y), mix(z, that.z), mix(w, that.w),
        mix(red, that.red), mix(green, that.green), mix(blue, that.blue), mix(alpha, that.alpha),
        mix(baryA, that.baryA), mix(baryB, that.baryB), mix(baryC, that.baryC)
      )

  private def clipPolygon(
    plan: SurfaceRenderPlan, buffers: VertexBuffers, mesh: SurfaceMeshPacket, slotIndex: Int, ia: Int, ib: Int, ic: Int
  ): Vector[ClipVertex] =
    def vertex(index: Int, ba: Double, bb: Double, bc: Double): ClipVertex =
      val offset = index * 3
      ClipVertex(
        mesh.positions(offset).toDouble, mesh.positions(offset + 1).toDouble, mesh.positions(offset + 2).toDouble,
        buffers.cx(index), buffers.cy(index), buffers.cz(index), buffers.cw(index),
        buffers.red(index), buffers.green(index), buffers.blue(index), buffers.alpha(index), ba, bb, bc
      )
    var polygon = Vector(vertex(ia, 1.0, 0.0, 0.0), vertex(ib, 0.0, 1.0, 0.0), vertex(ic, 0.0, 0.0, 1.0))
    plan.clipping match
      case SurfaceClipping.WorldPlanes(planes) =>
        var planeIndex = 0
        while planeIndex < planes.length && polygon.nonEmpty do
          val plane = planes(planeIndex)
          polygon = clipAgainst(polygon, v => plane.orientedDistance(v.worldX, v.worldY, v.worldZ))
          planeIndex += 1
      case _ => ()
    var plane = 0
    while plane < 6 && polygon.nonEmpty do
      val current = plane
      polygon = clipAgainst(polygon, v => current match
        case 0 => v.x + v.w
        case 1 => v.w - v.x
        case 2 => v.y + v.w
        case 3 => v.w - v.y
        case 4 => v.z + v.w
        case _ => v.w - v.z)
      plane += 1
    polygon

  private def clipAgainst(polygon: Vector[ClipVertex], distance: ClipVertex => Double): Vector[ClipVertex] =
    val output = Vector.newBuilder[ClipVertex]
    var previous = polygon.last
    var previousDistance = distance(previous)
    var index = 0
    while index < polygon.length do
      val current = polygon(index)
      val currentDistance = distance(current)
      val previousInside = previousDistance >= 0.0
      val currentInside = currentDistance >= 0.0
      if previousInside != currentInside then
        val fraction = previousDistance / (previousDistance - currentDistance)
        output += previous.interpolate(current, fraction)
      if currentInside then output += current
      previous = current
      previousDistance = currentDistance
      index += 1
    output.result()

  private def rasterTriangle(
    triangle: Int, data: Array[Double], meta: Array[Int], minY: Int, maxY: Int, width: Int,
    pixels: Array[Int], depths: Array[Double], faceAt: Array[Int], slotAt: Array[Int], vertexAt: Array[Int],
    baryA: Array[Float], baryB: Array[Float], baryC: Array[Float]
  ): (Int, Int) =
    val base = triangle * Fields
    val ax = data(base); val ay = data(base + 1); val aDepth = data(base + 2); val aInv = data(base + 3)
    val bx = data(base + 11); val by = data(base + 12); val bDepth = data(base + 13); val bInv = data(base + 14)
    val cx = data(base + 22); val cy = data(base + 23); val cDepth = data(base + 24); val cInv = data(base + 25)
    val area = edge(ax, ay, bx, by, cx, cy)
    val m = triangle * 5
    val slot = meta(m); val face = meta(m + 1); val ia = meta(m + 2); val ib = meta(m + 3); val ic = meta(m + 4)
    val minX = math.max(0, math.floor(math.min(ax, math.min(bx, cx))).toInt)
    val maxX = math.min(width - 1, math.ceil(math.max(ax, math.max(bx, cx))).toInt)
    var shaded = 0
    var rejected = 0
    var y = minY
    while y <= maxY do
      var x = minX
      while x <= maxX do
        val px = x + 0.5
        val py = y + 0.5
        val wa = edge(bx, by, cx, cy, px, py) / area
        val wb = edge(cx, cy, ax, ay, px, py) / area
        val wc = 1.0 - wa - wb
        if wa >= -1e-12 && wb >= -1e-12 && wc >= -1e-12 then
          val depth = wa * aDepth + wb * bDepth + wc * cDepth
          val pixelIndex = y * width + x
          if depth < depths(pixelIndex) then
            val reciprocal = wa * aInv + wb * bInv + wc * cInv
            val inv = 1.0 / reciprocal
            val ba = (wa * data(base + 8) + wb * data(base + 19) + wc * data(base + 30)) * inv
            val bb = (wa * data(base + 9) + wb * data(base + 20) + wc * data(base + 31)) * inv
            val bc = (wa * data(base + 10) + wb * data(base + 21) + wc * data(base + 32)) * inv
            val red = clampByte((wa * data(base + 4) + wb * data(base + 15) + wc * data(base + 26)) * inv)
            val green = clampByte((wa * data(base + 5) + wb * data(base + 16) + wc * data(base + 27)) * inv)
            val blue = clampByte((wa * data(base + 6) + wb * data(base + 17) + wc * data(base + 28)) * inv)
            val alpha = clampByte((wa * data(base + 7) + wb * data(base + 18) + wc * data(base + 29)) * inv)
            if alpha > 0 then
              pixels(pixelIndex) = compositeOverOpaque(pixels(pixelIndex), red, green, blue, alpha)
              depths(pixelIndex) = depth
              faceAt(pixelIndex) = face
              slotAt(pixelIndex) = slot
              baryA(pixelIndex) = ba.toFloat
              baryB(pixelIndex) = bb.toFloat
              baryC(pixelIndex) = bc.toFloat
              vertexAt(pixelIndex) = SurfaceNearestPartition.nearestVertex(ia, ib, ic, ba, bb, bc)
              shaded += 1
          else rejected += 1
        x += 1
      y += 1
    (shaded, rejected)

  private def clampByte(value: Double): Int =
    math.round(value).toInt.max(0).min(255)

  private def compositeOverOpaque(under: Int, red: Int, green: Int, blue: Int, alpha: Int): Int =
    if alpha == 255 then (red << 24) | (green << 16) | (blue << 8) | 255
    else
      val inverse = 255 - alpha
      val underRed = (under >>> 24) & 0xff
      val underGreen = (under >>> 16) & 0xff
      val underBlue = (under >>> 8) & 0xff
      val outRed = (red * alpha + underRed * inverse + 127) / 255
      val outGreen = (green * alpha + underGreen * inverse + 127) / 255
      val outBlue = (blue * alpha + underBlue * inverse + 127) / 255
      (outRed << 24) | (outGreen << 16) | (outBlue << 8) | 255
