package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javax.imageio.ImageIO
import _root_.javafx.application.Platform
import _root_.javafx.scene.{Group, Scene, SceneAntialiasing}
import _root_.javafx.scene.image.{PixelFormat, WritableImage}

/** Planar oracle for the flat per-face texel encodings.
  *
  * Each case is an n x n grid (two faces per cell) carrying raw scalar samples,
  * rotated by 7 degrees and shifted by a small offset so that face edges do not
  * systematically pass through pixel centres. References use only the portable
  * raster's face picks and the declared face rule, never the native texture,
  * its coordinates or its sampler:
  *
  *  - `ref-flat`: each pixel centre takes the rule colour of the face it hits;
  *  - `ref-flat-4x`: the same at the centres of a 4 x 4 sub-pixel grid, which
  *    contains the standard 4-sample multisample positions;
  *  - `cover-4x`: which of those sub-pixel centres hit a face;
  *  - `ref-interp`: the raster's interpolated-scalar rendering (faceting only).
  *
  * The rule colours are also recomputed directly from the recipe (mean of the
  * three samples, any missing corner hides the layer) and compared.
  * Metrics are computed from the PNGs by tools/face_metrics.py.
  *
  * args: output [sizes=64,128,256] [pixels=256] [encodings=FaceTexelFlat,FaceTexelFlatSingle]
  */
object JavaFxFaceTexelPlanarProbe:
  private val surface = SurfaceId.unsafe("face-planar")
  private val base = SurfaceFaceTexels.Base
  private val gray = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.linear(Rgba32.unsafe(32, 32, 32), Rgba32.unsafe(224, 224, 224))))
  private val diverging = ScalarMapping(ScalarScale.diverging(DisplayWindow.unsafe(-1.0, 1.0), 0.0,
    ScalarRamp.linear(Rgba32.unsafe(20, 40, 200), Rgba32.unsafe(245, 245, 245)),
    ScalarRamp.linear(Rgba32.unsafe(245, 245, 245), Rgba32.unsafe(200, 30, 20))).toOption.get)
  private val thresholded = diverging.resolve(threshold = Some(DisplayThreshold.transparentBand(-0.3, 0.3).toOption.get)).toOption.get
  private val curvature = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.linear(Rgba32.unsafe(48, 48, 48), Rgba32.unsafe(208, 208, 208))))
  // The PLS Neuro recipe: transparent below the cutoff, then a saturated hue onset
  // ramping to the window end, over a binary sulcal/gyral underlay.
  private val onset = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-1.0, 1.0), 0.0, -0.25, 0.25,
    ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
    ScalarRamp.linear(Rgba32.unsafe(255, 64, 0), Rgba32.unsafe(255, 255, 0))).toOption.get)
  private val binary = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.make(Vector(0.0 -> Rgba32.unsafe(202, 205, 209), 0.5 -> Rgba32.unsafe(202, 205, 209),
      0.5000001 -> Rgba32.unsafe(65, 72, 82), 1.0 -> Rgba32.unsafe(65, 72, 82))).toOption.get))
  private val Angle = math.toRadians(7.0)
  private val OffsetX = 0.0137
  private val OffsetY = -0.0071

  final case class Case(name: String, n: Int, overlay: (Double, Double) => Double, mapping: ScalarMapping,
      opacity: Double, underlay: Option[(Double, Double) => Double], underlayMapping: ScalarMapping = curvature)

  private def hashNoise(index: Int): Double =
    var h = index * 0x9E3779B1
    h ^= h >>> 15; h *= 0x85EBCA6B; h ^= h >>> 13; h *= 0xC2B2AE35; h ^= h >>> 16
    (h & 0xffff).toDouble / 0xffff * 2.0 - 1.0

  private def cases(n: Int): Vector[Case] =
    val c = math.cos(math.toRadians(30)); val s = math.sin(math.toRadians(30))
    Vector(
      Case(s"ramp-$n", n, (x, _) => x, gray, 1.0, None),
      Case(s"threshold-$n", n, (x, _) => x, thresholded, 1.0, None),
      Case(s"steep-underlay-$n", n, (x, _) => x * 8.0, thresholded, 0.65, Some((_, y) => y)),
      Case(s"oblique-underlay-$n", n, (x, y) => (x * c + y * s) * 8.0, thresholded, 0.65, Some((x, y) => -x * s + y * c)),
      Case(s"noisy-underlay-$n", n, (x, _) => x * 8.0, thresholded, 0.65, Some((_, y) => y)),
      Case(s"missing-underlay-$n", n, (x, y) =>
        if (x - 0.3) * (x - 0.3) + (y + 0.2) * (y + 0.2) < 0.15 || math.abs(y - 0.5) < 0.02 then Double.NaN else x * 8.0,
        thresholded, 0.65, Some((_, y) => y)),
      Case(s"onset-binary-$n", n, (x, y) => 1.6 * x + 0.35 * math.sin(6.0 * y), onset, 1.0,
        Some((x, y) => math.sin(5.0 * x + 3.0 * y)), binary),
      Case(s"onset-noisy-binary-$n", n, (x, y) => 0.8 * math.sin(3.0 * x) * math.cos(2.0 * y), onset, 1.0,
        Some((x, y) => math.sin(5.0 * x + 3.0 * y)), binary)
    )

  final case class Built(viewer: SurfaceViewerModel, overlay: Array[Double], underlay: Option[Array[Double]], faces: Array[Int])

  private def model(kase: Case): Built =
    val n = kase.n
    val vertices = (n + 1) * (n + 1)
    val coordinates = new Array[Double](vertices * 3)
    val overlay = new Array[Double](vertices)
    val underlay = new Array[Double](vertices)
    val cos = math.cos(Angle); val sin = math.sin(Angle)
    var j = 0
    while j <= n do
      var i = 0
      while i <= n do
        val index = j * (n + 1) + i
        val x = 2.0 * i / n - 1.0
        val y = 2.0 * j / n - 1.0
        coordinates(index * 3) = x * cos - y * sin + OffsetX
        coordinates(index * 3 + 1) = x * sin + y * cos + OffsetY
        val noise = if kase.name.contains("noisy") then hashNoise(index) * 0.4 else 0.0
        overlay(index) = kase.overlay(x, y) + noise
        underlay(index) = kase.underlay.fold(0.0)(_(x, y))
        i += 1
      j += 1
    val faces = new Array[Int](n * n * 6)
    var cell = 0
    while cell < n * n do
      val a = (cell / n) * (n + 1) + cell % n
      faces(cell * 6) = a; faces(cell * 6 + 1) = a + 1; faces(cell * 6 + 2) = a + n + 2
      faces(cell * 6 + 3) = a; faces(cell * 6 + 4) = a + n + 2; faces(cell * 6 + 5) = a + n + 1
      cell += 1
    val geometry = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), Hemisphere.Left, SurfaceKind.Inflated)
    val layers = kase.underlay.toVector.map(_ => SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("underlay"), surface, geometry, underlay, kase.underlayMapping).toOption.get) :+
      SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("overlay"), surface, geometry, overlay, kase.mapping,
        opacity = DisplayOpacity.unsafe(kase.opacity)).toOption.get
    Built(SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), layers).toOption.get,
      overlay, kase.underlay.map(_ => underlay), faces)

  /** The declared rule, written directly from the recipe without the fragment evaluator. */
  private def directColors(kase: Case, built: Built): Array[Int] =
    def mean(samples: Array[Double], a: Int, b: Int, c: Int): Double =
      if samples(a).isFinite && samples(b).isFinite && samples(c).isFinite then (samples(a) + samples(b) + samples(c)) / 3 else Double.NaN
    Array.tabulate(built.faces.length / 3): face =>
      val (a, b, c) = (built.faces(face * 3), built.faces(face * 3 + 1), built.faces(face * 3 + 2))
      val under = built.underlay.fold(base)(u =>
        DisplayBlendMode.Normal.composite(base, kase.underlayMapping.color(mean(u, a, b, c)), DisplayOpacity.Opaque))
      DisplayBlendMode.Normal.composite(under, kase.mapping.color(mean(built.overlay, a, b, c)), DisplayOpacity.unsafe(kase.opacity)).toPackedInt

  private def channelError(a: Int, b: Int): Int =
    math.max(math.abs(((a >>> 24) & 255) - ((b >>> 24) & 255)),
      math.max(math.abs(((a >>> 16) & 255) - ((b >>> 16) & 255)), math.abs(((a >>> 8) & 255) - ((b >>> 8) & 255))))

  private[javafx] def writeRgb(path: Path, argb: Array[Int], width: Int, height: Int): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    image.setRGB(0, 0, width, height, argb, 0, width)
    require(ImageIO.write(image, "png", path.toFile), s"could not write $path")

  /** Face-rule colour at each pixel centre from raster picks; white where no face is hit. */
  private[javafx] def flat(result: SurfaceRasterResult, width: Int, height: Int,
      colors: Map[SurfaceId, Array[Int]]): (Array[Int], Array[Int]) =
    val argb = new Array[Int](width * height)
    val cover = new Array[Int](width * height)
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        result.pick(x, y).toOption.flatten match
          case Some(pick) =>
            argb(y * width + x) = 0xff000000 | (colors(pick.surface)(pick.face) >>> 8)
            cover(y * width + x) = 0xffffffff
          case None =>
            argb(y * width + x) = 0xffffffff
            cover(y * width + x) = 0xff000000
        x += 1
      y += 1
    (argb, cover)

  /** Distance in pixels from each covered pixel centre to the nearest edge of the face
    * the raster picked there (-1 where no face is hit), little-endian float32. Each
    * surface's world-to-pixel map is the affine least-squares fit to the raster's own
    * picks (world point = barycentric combination of the face corners), which is exact
    * for orthographic views; the largest fit residual in pixels is returned. The nearest
    * mesh edge to a point inside a planar face is one of that face's edges.
    */
  private[javafx] def edgeDistances(result: SurfaceRasterResult, width: Int, height: Int,
      meshes: Vector[SurfaceMeshPacket]): (Array[Float], Double) =
    val bySurface = meshes.map(mesh => mesh.surface -> mesh).toMap
    def world(pick: SurfacePick): Array[Double] =
      val mesh = bySurface(pick.surface)
      val p = mesh.positions
      val (a, b, c) = (mesh.indices(pick.face * 3), mesh.indices(pick.face * 3 + 1), mesh.indices(pick.face * 3 + 2))
      Array.tabulate(3)(k => pick.barycentricA * p(a * 3 + k) + pick.barycentricB * p(b * 3 + k) + pick.barycentricC * p(c * 3 + k))
    val normal = scala.collection.mutable.Map.empty[SurfaceId, Array[Double]]
    val samples = scala.collection.mutable.ArrayBuffer.empty[(SurfaceId, Array[Double], Double, Double)]
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        result.pick(x, y).toOption.flatten.foreach: pick =>
          val w = Array(world(pick)(0), world(pick)(1), world(pick)(2), 1.0)
          val acc = normal.getOrElseUpdate(pick.surface, new Array[Double](24))
          for r <- 0 until 4 do
            for c <- 0 until 4 do acc(r * 4 + c) += w(r) * w(c)
            acc(16 + r) += w(r) * (x + 0.5)
            acc(20 + r) += w(r) * (y + 0.5)
          if samples.length < 200000 then samples += ((pick.surface, w, x + 0.5, y + 0.5))
        x += 3
      y += 3
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
    def project(surface: SurfaceId, w: Array[Double]): (Double, Double) =
      val (mx, my) = maps(surface)
      (mx(0) * w(0) + mx(1) * w(1) + mx(2) * w(2) + mx(3), my(0) * w(0) + my(1) * w(1) + my(2) * w(2) + my(3))
    val residual = samples.iterator.map: (surface, w, px, py) =>
      val (qx, qy) = project(surface, w)
      math.max(math.abs(qx - px), math.abs(qy - py))
    .foldLeft(0.0)(math.max)
    val out = new Array[Float](width * height)
    y = 0
    while y < height do
      var x = 0
      while x < width do
        out(y * width + x) = result.pick(x, y).toOption.flatten match
          case None => -1f
          case Some(pick) =>
            val mesh = bySurface(pick.surface)
            val corners = Vector(0, 1, 2).map: k =>
              val v = mesh.indices(pick.face * 3 + k)
              project(pick.surface, Array(mesh.positions(v * 3).toDouble, mesh.positions(v * 3 + 1).toDouble, mesh.positions(v * 3 + 2).toDouble))
            val (qx, qy) = (x + 0.5, y + 0.5)
            Vector((0, 1), (1, 2), (2, 0)).map: (i, j) =>
              val (ax, ay) = corners(i); val (bx, by) = corners(j)
              val length = math.hypot(bx - ax, by - ay)
              if length == 0 then 0.0 else math.abs((bx - ax) * (qy - ay) - (by - ay) * (qx - ax)) / length
            .min.toFloat
        x += 1
      y += 1
    (out, residual)

  private[javafx] def writeFloats(path: Path, values: Array[Float]): Unit =
    val buffer = java.nio.ByteBuffer.allocate(values.length * 4).order(java.nio.ByteOrder.LITTLE_ENDIAN)
    values.foreach(v => buffer.putFloat(v))
    Files.write(path, buffer.array()): Unit

  def main(args: Array[String]): Unit =
    val output = Path.of(args(0))
    Files.createDirectories(output)
    val sizes = args.lift(1).fold(Vector(64, 128, 256))(_.split(",").toVector.map(_.toInt))
    val pixels = args.lift(2).fold(256)(_.toInt)
    val encodings = args.lift(3).fold(Vector(JavaFxAtlasEncoding.FaceTexelFlat, JavaFxAtlasEncoding.FaceTexelFlatSingle))(
      _.split(",").toVector.map(JavaFxAtlasEncoding.valueOf))
    val only = Option(System.getProperty("probe.cases")).map(_.split(",").toSet)
    val material = Class.forName("com.sun.prism.es2.ES2PhongMaterial").getProtectionDomain.getCodeSource.getLocation.toString
    val shader = Class.forName("com.sun.prism.es2.ES2PhongShader").getProtectionDomain.getCodeSource.getLocation.toString
    Option(System.getProperty("probe.expectedOrigin")).foreach(expected =>
      require(material == expected && shader == expected, s"unexpected material/shader origin: $material / $shader"))
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    val rows = Vector.newBuilder[String]
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        for n <- sizes; kase <- cases(n) if only.forall(_(kase.name)) do
          val built = model(kase)
          val plan = SurfaceCompiler.compile(built.viewer, SurfaceFaceFixture.state(built.viewer)).toOption.get
          require(plan.fragmentSurfaces(surface))
          val mesh = plan.meshes.head
          require(java.util.Arrays.equals(mesh.indices.unsafeArray, built.faces), "compiled faces are not the model faces")
          val rule = SurfaceFaceTexels.colors(plan, mesh).fold(e => throw new IllegalStateException(e.message), identity)
          val direct = directColors(kase, built)
          val ruleErrors = rule.indices.map(i => channelError(rule(i), direct(i)))
          require(ruleErrors.max <= 1, s"${kase.name}: face rule differs from the recipe by ${ruleErrors.max}")
          val colors = Map(surface -> rule)
          val style = SurfaceRasterStyle(culling = TriangleCulling.None)
          val one = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(pixels, pixels), style).toOption.get
          val four = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(pixels * 4, pixels * 4), style).toOption.get
          val (flatOne, _) = flat(one, pixels, pixels, colors)
          val (flatFour, coverFour) = flat(four, pixels * 4, pixels * 4, colors)
          writeRgb(output.resolve(s"${kase.name}-ref-flat.png"), flatOne, pixels, pixels)
          writeRgb(output.resolve(s"${kase.name}-ref-flat-4x.png"), flatFour, pixels * 4, pixels * 4)
          writeRgb(output.resolve(s"${kase.name}-cover-4x.png"), coverFour, pixels * 4, pixels * 4)
          writeRgb(output.resolve(s"${kase.name}-ref-interp.png"), Array.tabulate(pixels * pixels): index =>
            val c = one.image.pixelUnsafe(index % pixels, index / pixels)
            0xff000000 | (c.red << 16) | (c.green << 8) | c.blue, pixels, pixels)
          val (distances, residual) = edgeDistances(one, pixels, pixels, plan.meshes)
          writeFloats(output.resolve(s"${kase.name}-edge-distance.f32"), distances)
          println(s"edge_distance_fit ${kase.name} maxResidualPx=$residual")
          val referenceOnly = System.getProperty("probe.referenceOnly", "false") == "true"
          for encoding <- encodings if !referenceOnly; aa <- Vector(SceneAntialiasing.DISABLED, SceneAntialiasing.BALANCED) do
            val config = JavaFxAtlasConfig.make(encoding = encoding).toOption.get
            val backend = JavaFxSurfaceBackend.create(config).toOption.get
            val name = s"${kase.name}-$encoding-$aa"
            try
              val started = System.nanoTime()
              backend.render(plan).fold(e => throw new IllegalArgumentException(e.message), identity)
              val settings = JavaFxSnapshotConfig.make(pixels, pixels, aa).toOption.get
              val sub = backend.newSubScene(settings).toOption.get
              val host = new Scene(new Group(sub), pixels, pixels)
              host.getRoot.applyCss()
              host.getRoot.layout()
              val image: WritableImage = backend.snapshot(settings).toOption.get
              val renderMs = (System.nanoTime() - started) / 1e6
              val actual = new Array[Int](pixels * pixels)
              image.getPixelReader.getPixels(0, 0, pixels, pixels, PixelFormat.getIntArgbInstance(), actual, 0, pixels)
              require(actual.distinct.length > 4, s"$name: blank frame")
              writeRgb(output.resolve(s"$name.png"), actual, pixels, pixels)
              val atlas = backend.chunks.head.atlas
              val row = s"""{"case":"${kase.name}","encoding":"$encoding","antialiasing":"$aa","triangles":${rule.length},"pixels":$pixels,""" +
                s""""ruleMaxDifference":${ruleErrors.max},"ruleOffByOne":${ruleErrors.count(_ == 1)},"texture":"${atlas.width}x${atlas.height}",""" +
                s""""renderedFaces":${backend.chunks.map(_.renderedFaceCount).sum},"renderMs":$renderMs}"""
              rows += row
              println(s"face_planar=$row")
            finally backend.dispose()
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(1200, TimeUnit.SECONDS), "planar face probe timed out")
      failure.foreach(throw _)
      val runtime = s"""{"javaVersion":"${System.getProperty("java.version")}","javafxVersion":"${System.getProperty("javafx.runtime.version")}","prismOrder":"${System.getProperty("prism.order")}","materialOrigin":"$material","shaderOrigin":"$shader","affineAtlasSwitch":"${System.getProperty("scalafim.javafx.affineAtlas")}"}"""
      Files.writeString(output.resolve("results.json"), s"""{"runtime":$runtime,"cases":[\n${rows.result().mkString(",\n")}\n]}\n"""): Unit
      println("PASS planar face fixtures written")
    finally Platform.exit()
