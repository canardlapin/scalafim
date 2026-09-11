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
import _root_.javafx.scene.image.WritableImage

/** Interpolation-aware planar oracle for the scalar lookup encoding. Every case is
  * an n x n grid over [-1, 1]^2 carrying raw scalar samples; the reference is the
  * portable raster's fragment path (interpolate the scalar, then map, then
  * composite). Errors are reported separately for pixels at least two pixels from
  * any reference colour edge and for the edge band itself, because the native
  * sampler renders threshold contours as short in-face ramps by design.
  */
object JavaFxScalarLutPlanarProbe:
  private val surface = SurfaceId.unsafe("lut-planar")
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
      // Missing samples: a disc and a thin strip. The reference hides every face touching one.
      Case(s"missing-underlay-$n", n, (x, y) =>
        if (x - 0.3) * (x - 0.3) + (y + 0.2) * (y + 0.2) < 0.15 || math.abs(y - 0.5) < 0.02 then Double.NaN else x * 8.0,
        thresholded, 0.65, Some((_, y) => y)),
      Case(s"onset-binary-$n", n, (x, y) => 1.6 * x + 0.35 * math.sin(6.0 * y), onset, 1.0,
        Some((x, y) => math.sin(5.0 * x + 3.0 * y)), binary),
      Case(s"onset-noisy-binary-$n", n, (x, y) => 0.8 * math.sin(3.0 * x) * math.cos(2.0 * y), onset, 1.0,
        Some((x, y) => math.sin(5.0 * x + 3.0 * y)), binary)
    // Per-vertex noise on a grid at one cell per pixel leaves no smooth reference pixels.
    ).filter(kase => !kase.name.contains("onset-noisy") || n <= 64)

  private def model(kase: Case): SurfaceViewerModel =
    val n = kase.n
    val vertices = (n + 1) * (n + 1)
    val coordinates = new Array[Double](vertices * 3)
    val overlay = new Array[Double](vertices)
    val underlay = new Array[Double](vertices)
    var j = 0
    while j <= n do
      var i = 0
      while i <= n do
        val index = j * (n + 1) + i
        val x = 2.0 * i / n - 1.0
        val y = 2.0 * j / n - 1.0
        coordinates(index * 3) = x
        coordinates(index * 3 + 1) = y
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
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), layers).toOption.get

  private def write(path: Path, argb: Array[Int], width: Int, height: Int): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, width, height, argb, 0, width)
    ImageIO.write(image, "png", path.toFile): Unit

  private def channelError(a: Int, b: Int): Int =
    math.max(math.abs(((a >>> 16) & 255) - ((b >>> 16) & 255)),
      math.max(math.abs(((a >>> 8) & 255) - ((b >>> 8) & 255)), math.abs((a & 255) - (b & 255))))

  def main(args: Array[String]): Unit =
    val output = Path.of(args(0))
    Files.createDirectories(output)
    val sizes = args.lift(1).fold(Vector(64, 128, 256))(_.split(",").toVector.map(_.toInt))
    val pixels = args.lift(2).fold(256)(_.toInt)
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
        for n <- sizes; kase <- cases(n); aa <- Vector(SceneAntialiasing.DISABLED, SceneAntialiasing.BALANCED) do
          val viewer = model(kase)
          val state = SurfaceFaceFixture.state(viewer)
          val plan = SurfaceCompiler.compile(viewer, state).toOption.get
          require(plan.fragmentSurfaces(surface))
          val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(pixels, pixels),
            SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
          val config = JavaFxAtlasConfig.make(encoding = JavaFxAtlasEncoding.ScalarLutInterpolated).toOption.get
          val backend = JavaFxSurfaceBackend.create(config).toOption.get
          val name = s"${kase.name}-$aa"
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
            image.getPixelReader.getPixels(0, 0, pixels, pixels, _root_.javafx.scene.image.PixelFormat.getIntArgbInstance(), actual, 0, pixels)
            val expected = new Array[Int](pixels * pixels)
            val covered = new Array[Boolean](pixels * pixels)
            for y <- 0 until pixels; x <- 0 until pixels do
              val c = reference.image.pixelUnsafe(x, y)
              expected(y * pixels + x) = (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue
              covered(y * pixels + x) = reference.pick(x, y).toOption.flatten.nonEmpty
            def at(x: Int, y: Int): Int = y * pixels + x
            val interior = Array.tabulate(pixels * pixels): index =>
              val x = index % pixels; val y = index / pixels
              x >= 1 && y >= 1 && x < pixels - 1 && y < pixels - 1 &&
                (for dy <- -1 to 1; dx <- -1 to 1 yield covered(at(x + dx, y + dy))).forall(identity)
            val edgeSeed = Array.tabulate(pixels * pixels): index =>
              val x = index % pixels; val y = index / pixels
              interior(index) && (for dy <- -1 to 1; dx <- -1 to 1 yield channelError(expected(index), expected(at(x + dx, y + dy)))).max > 6
            val edge = Array.tabulate(pixels * pixels): index =>
              val x = index % pixels; val y = index / pixels
              (for dy <- -2 to 2; dx <- -2 to 2 if x + dx >= 0 && y + dy >= 0 && x + dx < pixels && y + dy < pixels
                yield edgeSeed(at(x + dx, y + dy))).exists(identity)
            val smoothErrors = Vector.newBuilder[Int]
            val edgeErrors = Vector.newBuilder[Int]
            val allErrors = Vector.newBuilder[Int]
            val diff = new Array[Int](pixels * pixels)
            for index <- 0 until pixels * pixels if interior(index) do
              val error = channelError(actual(index), expected(index))
              allErrors += error
              if edge(index) then edgeErrors += error else smoothErrors += error
              val shade = math.min(255, error * 4)
              diff(index) = 0xff000000 | (shade << 16) | (if edge(index) then 0x40 << 8 else 0) | shade
            val smooth = smoothErrors.result().sorted
            val band = edgeErrors.result().sorted
            val all = allErrors.result()
            def rms(values: Vector[Int]): Double = if values.isEmpty then 0.0 else math.sqrt(values.map(v => v.toDouble * v).sum / values.length)
            def percentile(values: Vector[Int], p: Double): Int = if values.isEmpty then 0 else values(math.min(values.length - 1, (values.length * p).toInt))
            // Column constancy of the native ramp, the planar oracle's original statistic.
            val columnRange = (0 until pixels).map: x =>
              val grays = (0 until pixels).filter(y => interior(at(x, y)) && !edge(at(x, y))).map(y => (actual(at(x, y)) >>> 16) & 255)
              if grays.isEmpty then 0 else grays.max - grays.min
            .max
            require(smooth.length > 1000, s"$name: insufficient smooth coverage ${smooth.length}")
            write(output.resolve(s"$name.png"), actual, pixels, pixels)
            write(output.resolve(s"$name-reference.png"), expected, pixels, pixels)
            write(output.resolve(s"$name-diff.png"), diff, pixels, pixels)
            val row = s"""{"case":"${kase.name}","triangles":${kase.n * kase.n * 2},"antialiasing":"$aa","pixels":$pixels,"interior":${all.length},"smooth":${smooth.length},"smoothMax":${smooth.last},"smoothRms":${rms(smooth)},"smoothP99":${percentile(smooth, 0.99)},"smoothOver2":${smooth.count(_ > 2)},"edge":${band.length},"edgeMax":${band.lastOption.getOrElse(0)},"edgeMean":${if band.isEmpty then 0.0 else band.sum.toDouble / band.length},"edgeP50":${percentile(band, 0.5)},"allRms":${rms(all)},"maxColumnRange":$columnRange,"renderMs":$renderMs,"lookup":"${backend.chunks.head.atlas.width}x${backend.chunks.head.atlas.height}"}"""
            rows += row
            println(s"scalar_lut_planar=$row")
          finally backend.dispose()
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(600, TimeUnit.SECONDS), "planar lookup probe timed out")
      failure.foreach(throw _)
      val runtime = s"""{"javaVersion":"${System.getProperty("java.version")}","javafxVersion":"${System.getProperty("javafx.runtime.version")}","prismOrder":"${System.getProperty("prism.order")}","materialOrigin":"$material","shaderOrigin":"$shader","affineAtlasSwitch":"${System.getProperty("scalafim.javafx.affineAtlas")}"}"""
      Files.writeString(output.resolve("results.json"), s"""{"runtime":$runtime,"cases":[\n${rows.result().mkString(",\n")}\n]}\n"""): Unit
      println("PASS planar lookup fixtures written")
    finally Platform.exit()
