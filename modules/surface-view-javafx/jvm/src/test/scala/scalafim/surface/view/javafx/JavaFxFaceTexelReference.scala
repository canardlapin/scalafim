package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.awt.image.BufferedImage
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

/** Reference exports for the face-texel oracles, from raster picks only.
  *
  * Geometry files (one per scene or case and camera): global face id per pixel
  * centre (`faceid-1x`) and at the centres of a 4 x 4 sub-pixel grid (`faceid-4x`,
  * which contains the standard 4-sample multisample positions), the distance in
  * pixels from each of those samples to the nearest edge of its face (`edge-1x`,
  * `edge-4x`), and edge adjacency (`adjacency`, three neighbour ids per face).
  * Rule colours are exported per mode as `colours-*` indexed by global face id, so
  * one geometry export serves every face rule, cutoff and palette. Global ids
  * number faces in plan mesh order. Files are little-endian int32 / float32.
  */
object JavaFxFaceTexelReference:
  def offsets(plan: SurfaceRenderPlan): Map[SurfaceId, Int] =
    plan.meshes.scanLeft(0)((sum, mesh) => sum + mesh.indices.length / 3).zip(plan.meshes).map((offset, mesh) => mesh.surface -> offset).toMap

  def faceIds(result: SurfaceRasterResult, width: Int, height: Int, offsets: Map[SurfaceId, Int]): Array[Int] =
    val out = new Array[Int](width * height)
    var index = 0
    while index < out.length do
      out(index) = result.pick(index % width, index / width).toOption.flatten.fold(-1)(pick => offsets(pick.surface) + pick.face)
      index += 1
    out

  def adjacency(plan: SurfaceRenderPlan, offsets: Map[SurfaceId, Int]): Array[Int] =
    val out = Array.fill(plan.meshes.map(_.indices.length / 3).sum * 3)(-1)
    plan.meshes.foreach: mesh =>
      val base = offsets(mesh.surface)
      val faces = mesh.indices.length / 3
      val open = new java.util.HashMap[java.lang.Long, java.lang.Integer](faces * 2)
      var face = 0
      while face < faces do
        var k = 0
        while k < 3 do
          val a = mesh.indices(face * 3 + k)
          val b = mesh.indices(face * 3 + (k + 1) % 3)
          val key = java.lang.Long.valueOf((math.min(a, b).toLong << 32) | math.max(a, b).toLong)
          val other = open.remove(key)
          if other == null then open.put(key, Integer.valueOf(face * 3 + k)): Unit
          else
            val otherFace = other.intValue / 3
            out((base + face) * 3 + k) = base + otherFace
            out((base + otherFace) * 3 + other.intValue % 3) = base + face
          k += 1
        face += 1
    out

  def globalColors(plan: SurfaceRenderPlan, colors: Map[SurfaceId, Array[Int]], offsets: Map[SurfaceId, Int]): Array[Int] =
    val out = new Array[Int](plan.meshes.map(_.indices.length / 3).sum)
    plan.meshes.foreach(mesh => System.arraycopy(colors(mesh.surface), 0, out, offsets(mesh.surface), colors(mesh.surface).length))
    out

  /** Opaque ARGB from ids and packed RGBA colours; white where no face is hit. */
  def flat(ids: Array[Int], colors: Array[Int]): Array[Int] =
    ids.map(id => if id < 0 then 0xffffffff else 0xff000000 | (colors(id) >>> 8))

  /** Distance in output pixels divided by `scale` from each covered sample to the nearest
    * edge of its picked face (-1 where none). Each surface's world-to-pixel map is the
    * affine least-squares fit to the raster's own picks, exact for orthographic views;
    * the largest fit residual (in output pixels) is returned. Inside a planar face the
    * nearest mesh edge is one of that face's edges.
    */
  def edgeDistances(result: SurfaceRasterResult, width: Int, height: Int, meshes: Vector[SurfaceMeshPacket],
      scale: Double = 1.0, stride: Int = 3): (Array[Float], Double) =
    val bySurface = meshes.map(mesh => mesh.surface -> mesh).toMap
    def world(pick: SurfacePick, k: Int): Double =
      val mesh = bySurface(pick.surface)
      val (a, b, c) = (mesh.indices(pick.face * 3), mesh.indices(pick.face * 3 + 1), mesh.indices(pick.face * 3 + 2))
      pick.barycentricA * mesh.positions(a * 3 + k) + pick.barycentricB * mesh.positions(b * 3 + k) + pick.barycentricC * mesh.positions(c * 3 + k)
    val normal = scala.collection.mutable.Map.empty[SurfaceId, Array[Double]]
    val samples = scala.collection.mutable.ArrayBuffer.empty[(SurfaceId, Array[Double], Double, Double)]
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        result.pick(x, y).toOption.flatten.foreach: pick =>
          val w = Array(world(pick, 0), world(pick, 1), world(pick, 2), 1.0)
          val acc = normal.getOrElseUpdate(pick.surface, new Array[Double](24))
          for r <- 0 until 4 do
            for c <- 0 until 4 do acc(r * 4 + c) += w(r) * w(c)
            acc(16 + r) += w(r) * (x + 0.5)
            acc(20 + r) += w(r) * (y + 0.5)
          if samples.length < 200000 then samples += ((pick.surface, w, x + 0.5, y + 0.5))
        x += stride
      y += stride
    def solve(acc: Array[Double], offset: Int): Array[Double] =
      // A tiny ridge keeps the system solvable when a world axis is constant (planar z = 0).
      val ridge = 1e-12 * (acc(0) + acc(5) + acc(10) + acc(15))
      val m = Array.tabulate(4)(r => Array.tabulate(5)(c => if c < 4 then acc(r * 4 + c) + (if r == c then ridge else 0.0) else acc(offset + r)))
      for col <- 0 until 4 do
        val pivot = (col until 4).maxBy(r => math.abs(m(r)(col)))
        val t = m(col); m(col) = m(pivot); m(pivot) = t
        for r <- 0 until 4 if r != col do
          val f = m(r)(col) / m(col)(col)
          for c <- col until 5 do m(r)(c) -= f * m(col)(c)
      Array.tabulate(4)(r => m(r)(4) / m(r)(r))
    val maps = normal.map((surface, acc) => surface -> (solve(acc, 16), solve(acc, 20))).toMap
    val residual = samples.iterator.map: (surface, w, px, py) =>
      val (mx, my) = maps(surface)
      math.max(math.abs(mx(0) * w(0) + mx(1) * w(1) + mx(2) * w(2) + mx(3) - px), math.abs(my(0) * w(0) + my(1) * w(1) + my(2) * w(2) + my(3) - py))
    .foldLeft(0.0)(math.max)
    val out = new Array[Float](width * height)
    val cx = new Array[Double](3)
    val cy = new Array[Double](3)
    y = 0
    while y < height do
      var x = 0
      while x < width do
        out(y * width + x) = result.pick(x, y).toOption.flatten match
          case None => -1f
          case Some(pick) =>
            val mesh = bySurface(pick.surface)
            val (mx, my) = maps(pick.surface)
            var k = 0
            while k < 3 do
              val v = mesh.indices(pick.face * 3 + k)
              val (wx, wy, wz) = (mesh.positions(v * 3).toDouble, mesh.positions(v * 3 + 1).toDouble, mesh.positions(v * 3 + 2).toDouble)
              cx(k) = mx(0) * wx + mx(1) * wy + mx(2) * wz + mx(3)
              cy(k) = my(0) * wx + my(1) * wy + my(2) * wz + my(3)
              k += 1
            val qx = x + 0.5
            val qy = y + 0.5
            var best = Double.MaxValue
            k = 0
            while k < 3 do
              val j = (k + 1) % 3
              val length = math.hypot(cx(j) - cx(k), cy(j) - cy(k))
              val d = if length == 0 then 0.0 else math.abs((cx(j) - cx(k)) * (qy - cy(k)) - (cy(j) - cy(k)) * (qx - cx(k))) / length
              if d < best then best = d
              k += 1
            (best / scale).toFloat
        x += 1
      y += 1
    (out, residual)

  def writeInts(path: Path, values: Array[Int]): Unit =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    buffer.asIntBuffer().put(values)
    Files.write(path, buffer.array()): Unit

  def writeFloats(path: Path, values: Array[Float]): Unit =
    val buffer = ByteBuffer.allocate(values.length * 4).order(ByteOrder.LITTLE_ENDIAN)
    buffer.asFloatBuffer().put(values)
    Files.write(path, buffer.array()): Unit

  def writeRgb(path: Path, argb: Array[Int], width: Int, height: Int): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.setRGB(0, 0, width, height, argb, 0, width)
    require(ImageIO.write(image, "png", path.toFile), s"could not write $path")

  def rasterArgb(result: SurfaceRasterResult, width: Int, height: Int): Array[Int] =
    Array.tabulate(width * height): index =>
      val c = result.image.pixelUnsafe(index % width, index / width)
      0xff000000 | (c.red << 16) | (c.green << 8) | c.blue

  /** Writes the geometry files for `plan` at `width x height` and 4x; returns the fit residuals. */
  def exportGeometry(dir: Path, prefix: String, plan: SurfaceRenderPlan, width: Int, height: Int): (Double, Double) =
    val style = SurfaceRasterStyle(culling = TriangleCulling.None)
    val ids = offsets(plan)
    val one = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(width, height), style).fold(e => throw new IllegalStateException(e.toString), identity)
    writeInts(dir.resolve(s"$prefix-faceid-1x.i32"), faceIds(one, width, height, ids))
    val (edgeOne, residualOne) = edgeDistances(one, width, height, plan.meshes)
    writeFloats(dir.resolve(s"$prefix-edge-1x.f32"), edgeOne)
    val four = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(width * 4, height * 4), style).fold(e => throw new IllegalStateException(e.toString), identity)
    writeInts(dir.resolve(s"$prefix-faceid-4x.i32"), faceIds(four, width * 4, height * 4, ids))
    val (edgeFour, residualFour) = edgeDistances(four, width * 4, height * 4, plan.meshes, scale = 4.0, stride = 12)
    writeFloats(dir.resolve(s"$prefix-edge-4x.f32"), edgeFour)
    writeInts(dir.resolve(s"$prefix-adjacency.i32"), adjacency(plan, ids))
    (residualOne, residualFour / 4.0)
