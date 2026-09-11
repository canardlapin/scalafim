package scalafim.surface.view.javafx

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import java.util.concurrent.{CountDownLatch, TimeUnit}
import scala.concurrent.duration.*
import _root_.javafx.application.{ConditionalFeature, Platform}
import _root_.javafx.scene.{Group, Scene, SceneAntialiasing}
import _root_.javafx.scene.image.PixelFormat

/** Renders face-flat planes through the GPU of whichever JavaFX runtime the test
  * JVM carries and requires exact interior colours: with multisampling off, every
  * covered pixel at least two pixels (Chebyshev) from a reference colour step of
  * more than 6 levels must equal the raster's flat point reference. A runtime
  * whose sampler blurs, mixes mip levels or extrapolates constant texture
  * coordinates fails here. Skipped, with its reason, only when the toolkit or 3D
  * support is unavailable (for example on a headless machine).
  */
class JavaFxFaceTexelGpuSuite extends munit.FunSuite:
  override val munitTimeout: Duration = 180.seconds
  private val Pixels = 128
  private val surface = SurfaceId.unsafe("gpu-face")
  private val onset = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-1.0, 1.0), 0.0, -0.25, 0.25,
    ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
    ScalarRamp.linear(Rgba32.unsafe(255, 64, 0), Rgba32.unsafe(255, 255, 0))).toOption.get)
  private val binary = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.make(Vector(0.0 -> Rgba32.unsafe(202, 205, 209), 0.5 -> Rgba32.unsafe(202, 205, 209),
      0.5000001 -> Rgba32.unsafe(65, 72, 82), 1.0 -> Rgba32.unsafe(65, 72, 82))).toOption.get))
  private val gray = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.linear(Rgba32.unsafe(32, 32, 32), Rgba32.unsafe(224, 224, 224))))

  private lazy val toolkit: Either[String, String] =
    try
      val started = new CountDownLatch(1)
      Platform.startup(() => started.countDown())
      if !started.await(30, TimeUnit.SECONDS) then Left("JavaFX toolkit did not start")
      else if !Platform.isSupported(ConditionalFeature.SCENE3D) then Left("JavaFX reports no SCENE3D support")
      else Right(s"${System.getProperty("javafx.runtime.version")} ${Class.forName("com.sun.prism.es2.ES2PhongMaterial").getProtectionDomain.getCodeSource.getLocation}")
    catch case error: Throwable => Left(s"JavaFX toolkit unavailable: $error")

  private def onFx[A](body: => A): A =
    val done = new CountDownLatch(1)
    var result: Either[Throwable, A] = Left(new IllegalStateException("not run"))
    Platform.runLater: () =>
      try result = Right(body)
      catch case error: Throwable => result = Left(error)
      finally done.countDown()
    require(done.await(120, TimeUnit.SECONDS), "FX task timed out")
    result.fold(throw _, identity)

  /** 64 x 64 cells, rotated 7 degrees and offset so edges avoid systematic pixel-centre ties. */
  private def plan(overlay: (Double, Double) => Double, mapping: ScalarMapping, underlay: Boolean,
      reduction: SurfaceFaceReduction): SurfaceRenderPlan =
    val n = 64
    val coordinates = new Array[Double]((n + 1) * (n + 1) * 3)
    val over = new Array[Double]((n + 1) * (n + 1))
    val under = new Array[Double]((n + 1) * (n + 1))
    val (c, s) = (math.cos(math.toRadians(7.0)), math.sin(math.toRadians(7.0)))
    for j <- 0 to n; i <- 0 to n do
      val index = j * (n + 1) + i
      val x = 2.0 * i / n - 1.0
      val y = 2.0 * j / n - 1.0
      coordinates(index * 3) = x * c - y * s + 0.0137
      coordinates(index * 3 + 1) = x * s + y * c - 0.0071
      over(index) = overlay(x, y)
      under(index) = math.sin(5.0 * x + 3.0 * y)
    val faces = new Array[Int](n * n * 6)
    for cell <- 0 until n * n do
      val a = (cell / n) * (n + 1) + cell % n
      faces(cell * 6) = a; faces(cell * 6 + 1) = a + 1; faces(cell * 6 + 2) = a + n + 2
      faces(cell * 6 + 3) = a; faces(cell * 6 + 4) = a + n + 2; faces(cell * 6 + 5) = a + n + 1
    val geometry = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), Hemisphere.Left, SurfaceKind.Inflated)
    val layers = Option.when(underlay)(SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("under"), surface, geometry, under, binary,
      SurfaceFaceReduction.Mean).toOption.get).toVector :+
      SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("over"), surface, geometry, over, mapping, reduction).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), layers).toOption.get
    SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get

  private def native(plan: SurfaceRenderPlan, encoding: JavaFxAtlasEncoding): Array[Int] = onFx:
    val backend = JavaFxSurfaceBackend.create(JavaFxAtlasConfig.make(encoding = encoding).toOption.get).toOption.get
    try
      backend.render(plan).fold(e => throw new IllegalStateException(e.message), identity)
      val settings = JavaFxSnapshotConfig.make(Pixels, Pixels, SceneAntialiasing.DISABLED).toOption.get
      val sub = backend.newSubScene(settings).toOption.get
      val host = new Scene(new Group(sub), Pixels.toDouble, Pixels.toDouble)
      host.getRoot.applyCss()
      host.getRoot.layout()
      val image = backend.snapshot(settings).toOption.get
      val out = new Array[Int](Pixels * Pixels)
      image.getPixelReader.getPixels(0, 0, Pixels, Pixels, PixelFormat.getIntArgbInstance(), out, 0, Pixels)
      out
    finally backend.dispose(): Unit

  private def channelError(a: Int, b: Int): Int =
    Vector(16, 8, 0).map(shift => math.abs(((a >>> shift) & 255) - ((b >>> shift) & 255))).max

  test("face-flat texels render exact colours on the available JavaFX runtime, up to proven rasterization ties"):
    val runtime = toolkit.fold(reason => { assume(false, reason); "" }, identity)
    val cases = Vector(
      ("ramp", plan((x, _) => x, gray, underlay = false, SurfaceFaceReduction.Mean)),
      ("onset-mean", plan((x, y) => 1.6 * x + 0.35 * math.sin(6.0 * y), onset, underlay = true, SurfaceFaceReduction.Mean)),
      ("onset-max", plan((x, y) => 1.6 * x + 0.35 * math.sin(6.0 * y), onset, underlay = true, SurfaceFaceReduction.MaxMagnitude)))
    for (name, kase) <- cases do
      val reference = SurfaceRasterizer.render(kase, RasterDimensions.unsafe(Pixels, Pixels), SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
      val mesh = kase.meshes.head
      val colours = SurfaceFaceTexels.colors(kase, mesh).toOption.get
      val faceAt = JavaFxFaceTexelReference.faceIds(reference, Pixels, Pixels, JavaFxFaceTexelReference.offsets(kase))
      val (edge, residual) = JavaFxFaceTexelReference.edgeDistances(reference, Pixels, Pixels, kase.meshes)
      assert(residual < 1e-3, s"$name: world-to-pixel fit residual $residual px")
      // Faces sharing a vertex with each face: the only colours a rasterization tie can substitute.
      val ring = Array.fill(mesh.positions.length / 3)(List.empty[Int])
      for face <- colours.indices; corner <- 0 until 3 do ring(mesh.indices(face * 3 + corner)) ::= face
      def neighbours(face: Int): Set[Int] = (0 until 3).flatMap(corner => ring(mesh.indices(face * 3 + corner))).toSet
      def rgb(packed: Int): Int = packed >>> 8
      def at(x: Int, y: Int): Int = y * Pixels + x
      val covered = (0 until Pixels * Pixels).filter(index => faceAt(index) >= 0 &&
        index % Pixels >= 1 && index / Pixels >= 1 && index % Pixels < Pixels - 1 && index / Pixels < Pixels - 1)
      // Uniform band: every pixel of the 5 x 5 neighbourhood shows the same reference colour.
      val uniform = covered.filter: index =>
        val (x, y) = (index % Pixels, index / Pixels)
        x >= 2 && y >= 2 && x < Pixels - 2 && y < Pixels - 2 &&
          (for dy <- -2 to 2; dx <- -2 to 2 yield faceAt(at(x + dx, y + dy)) >= 0 &&
            rgb(colours(faceAt(at(x + dx, y + dy)))) == rgb(colours(faceAt(index)))).forall(identity)
      if name != "ramp" then assert(uniform.length > 1000, s"$name: only ${uniform.length} uniform-band pixels")
      val frames = Vector(JavaFxAtlasEncoding.FaceTexelFlat, JavaFxAtlasEncoding.FaceTexelFlatSingle).map: encoding =>
        val actual = native(kase, encoding)
        def exact(index: Int): Boolean = (actual(index) & 0xffffff) == rgb(colours(faceAt(index)))
        val uniformWrong = uniform.filterNot(exact)
        assertEquals(uniformWrong.length, 0, s"$name $encoding on $runtime: ${uniformWrong.length}/${uniform.length} uniform-band pixels differ")
        val mismatched = covered.filterNot(exact)
        val unexplained = mismatched.filterNot: index =>
          edge(index) < 0.002f && neighbours(faceAt(index)).exists(face => rgb(colours(face)) == (actual(index) & 0xffffff))
        assertEquals(unexplained.length, 0, s"$name $encoding on $runtime: ${unexplained.length} of ${mismatched.length} mismatched pixels " +
          s"(of ${covered.length}) are not ties; edge distances ${unexplained.take(5).map(edge(_))}")
        actual
      assert(java.util.Arrays.equals(frames(0), frames(1)), s"$name: 2x2 and 1x1 blocks render different frames on $runtime")
