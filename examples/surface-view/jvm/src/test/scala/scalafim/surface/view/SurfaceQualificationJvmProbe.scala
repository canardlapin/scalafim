package scalafim.surface.view

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javax.imageio.ImageIO
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.jdk.CollectionConverters.*
import _root_.javafx.application.Platform
import _root_.javafx.geometry.Rectangle2D
import _root_.javafx.scene.{Group, Scene, SnapshotParameters}
import _root_.javafx.scene.image.WritableImage
import _root_.javafx.scene.paint.Color
import _root_.javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.javafx.*
import scalafim.surface.view.raster.*

/** Actual JavaFX native admission and ImageView fallback using frozen source
  * JSON. Snapshot/readback, pick and resize-back checks run on owned hosts.
  */
object SurfaceQualificationJvmProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(e => throw new IllegalStateException(s"$e"), identity)
  private def quoted(value: String): String = SceneJson.Str(value).render
  private def hash(bytes: Array[Byte]): String = MessageDigest.getInstance("SHA-256").digest(bytes).map(b => f"${b & 255}%02x").mkString
  private def fx[A](body: => A): A =
    if Platform.isFxApplicationThread then return body
    val latch = new CountDownLatch(1)
    var result: Either[Throwable, A] = Left(new IllegalStateException("FX callback missing"))
    Platform.runLater: () =>
      try result = Right(body)
      catch case error: Throwable => result = Left(error)
      finally latch.countDown()
    require(latch.await(60, TimeUnit.SECONDS), "FX callback timed out")
    result.fold(throw _, identity)

  /** Bind the classes actually loaded by this JVM, including module helpers. */
  private def runtimeIdentity: String =
    val origins = Vector(SurfaceQualificationFixture.getClass, SurfaceRasterizer.getClass,
      SurfaceDecorationRaster.getClass, JavaFxRasterSurfaceHost.getClass, SurfaceViewerModel.getClass,
      scalafim.surface.SurfaceGeometry.getClass, RasterImage.getClass).map: owner =>
        Path.of(owner.getProtectionDomain.getCodeSource.getLocation.toURI)
    val artifacts = origins.distinct.map: path =>
      if Files.isDirectory(path) then
        val stream = Files.walk(path)
        val files = try stream.iterator.asScala.filter(p => Files.isRegularFile(p) && p.toString.endsWith(".class"))
          .toVector.sortBy(p => path.relativize(p).toString)
        finally stream.close()
        val entries = files.map(p => s"${quoted(path.relativize(p).toString)}:${quoted(hash(Files.readAllBytes(p)))}")
        s"""{"path":${quoted(path.toString)},"kind":"class-directory","classes":{${entries.mkString(",")}}}"""
      else s"""{"path":${quoted(path.toString)},"kind":"artifact","sha256":${quoted(hash(Files.readAllBytes(path)))}}"""
    val properties = Vector("java.version", "java.vm.name", "os.name", "os.arch").map: name =>
      s"${quoted(name)}:${quoted(System.getProperty(name))}"
    s"""{"schema":"surface-jvm-runtime.v1","environment":{${properties.mkString(",")}},"artifacts":[${artifacts.mkString(",")}]}"""

  def main(args: Array[String]): Unit =
    require(args.length == 2, "usage: input-directory output-directory")
    val input = Path.of(args(0))
    val output = Path.of(args(1))
    Files.createDirectories(output)
    Files.writeString(output.resolve("runtime.json"), runtimeIdentity + "\n")
    val paths = Files.list(input)
    val cases = try paths.iterator.asScala.filter(p => Files.isRegularFile(p.resolve("input.json"))).toVector.sortBy(_.toString)
      finally paths.close()
    require(cases.nonEmpty, "no prepared inputs")
    Platform.startup(() => ())
    fx(Platform.setImplicitExit(false))
    val stage = fx(new Stage())
    val rows = Vector.newBuilder[String]
    try
      for directory <- cases do
        val destination = output.resolve(directory.getFileName)
        Files.createDirectories(destination)
        val bytes = Files.readAllBytes(directory.resolve("input.json"))
        val sourceHash = hash(bytes)
        val receipt = try
          val prepared = ok(SurfaceQualificationFixture.decode(new String(bytes, java.nio.charset.StandardCharsets.UTF_8)))
          run(prepared, destination, stage, sourceHash)
        catch case error: Exception => s"""{"inputSha256":"$sourceHash","status":"fail","failure":${quoted(error.toString)}}"""
        Files.writeString(destination.resolve("candidate.json"), receipt + "\n")
        rows += s"""{"case":${quoted(directory.getFileName.toString)},"receipt":$receipt}"""
        println(s"JavaFX qualification ${directory.getFileName}: ${if receipt.contains("\"failure\"") then "failed" else "captured"}")
      Files.writeString(output.resolve("results.json"), s"""{"schema":"surface-qualification-jvm.v1","cases":[${rows.result().mkString(",")}]}\n""")
    finally
      try fx(stage.close())
      finally Platform.exit()

  private def run(prepared: SurfaceQualificationFixture.Prepared, output: Path, stage: Stage, inputHash: String): String =
    val size = RasterDimensions.unsafe(prepared.width, prepared.height)
    val labels = new JavaFxSurfaceLabelRasterizer("Arial")
    val geometry = fx(ok(SurfaceLabelLayout.resolve(prepared.decorations, prepared.plan, labels)))
    val host = fx(JavaFxRasterSurfaceHost.create())
    try
      fx:
        stage.setScene(new Scene(host.root, size.width, size.height, Color.WHITE))
        stage.show()
      val coldStart = System.nanoTime()
      val cold = ok(Await.result(host.submit(prepared.plan, size, Some(geometry), prepared.style), 60.seconds))
      snapshot(host, size)
      val coldTotal = System.nanoTime() - coldStart
      val warmSamples = Vector.fill(5):
        val started = System.nanoTime()
        val receipt = ok(Await.result(host.submit(prepared.plan, size, Some(geometry), prepared.style), 60.seconds))
        val pixels = snapshot(host, size)
        (receipt, pixels, System.nanoTime() - started)
      val (warm, pixels, warmTotal) = warmSamples.last
      require(warmSamples.forall(_._2.sameElements(pixels)), "repeated warm host snapshots changed pixels")
      save(pixels, size.width, size.height, output.resolve("raster.png"))
      val picks = fx:
        (for row <- 1 to 5; column <- 1 to 5 yield
          val x = size.width * column / 6.0
          val y = size.height * row / 6.0
          val value = ok(host.pickDisplay(x, y)) match
            case Some(SurfaceDisplayHit.Scientific(hit)) => s"""{"surface":${quoted(hit.surface.value)},"vertex":${hit.vertex},"face":${hit.face},"view":${hit.origin.map(o => quoted(o.view.value)).getOrElse("null")}}"""
            case Some(SurfaceDisplayHit.Decoration(hit)) => s"""{"decoration":${quoted(hit.id.decoration.value)},"element":${quoted(hit.id.suffix)}}"""
            case None => "null"
          s"""{"x":$x,"y":$y,"raster":$value}""").mkString("[", ",", "]")
      val rebuild: JavaFxRasterSurfaceHost.DecorationLowering = (plan, dimensions) =>
        prepared.lowerDecorations(plan, dimensions.width, dimensions.height)
          .flatMap(geometry => fx(SurfaceLabelLayout.resolve(geometry, plan, labels)))
      ok(Await.result(host.submitDecorated(prepared.plan, size, rebuild, prepared.style), 60.seconds))
      val small = RasterDimensions.unsafe(math.max(1, size.width / 2), math.max(1, size.height / 2))
      ok(Await.result(host.resize(small), 60.seconds))
      snapshot(host, small)
      ok(Await.result(host.resize(size), 60.seconds))
      require(pixels.sameElements(snapshot(host, size)), "actual host resize-back changed pixels")
      val native = fx:
        JavaFxSurfaceComposition.create() match
          case Left(error) => s"""{"status":"refused","reason":${quoted(error.message)}}"""
          case Right(value) =>
            try
              val dimensions = ok(JavaFxSnapshotConfig.make(size.width, size.height))
              def render() = if geometry.meshes.isEmpty then value.render(prepared.plan, dimensions)
                else value.renderDecorated(prepared.plan, geometry, dimensions)
              render() match
                case Left(error) => s"""{"status":"refused","reason":${quoted(error.message)}}"""
                case Right(first) =>
                  stage.setScene(new Scene(value.root, size.width, size.height, Color.WHITE))
                  val second = ok(render())
                  val image = ok(value.snapshot())
                  save(read(image, size), size.width, size.height, output.resolve("native.png"))
                  s"""{"status":"captured","coldNanos":${first.native.elapsedNanos},"warmNanos":${second.native.elapsedNanos}}"""
            finally ok(value.dispose())
      val camera = prepared.plan.views.head.camera
      val view = (0 until 16).map(camera.viewMatrix(_)).mkString("[", ",", "]")
      val projection = (0 until 16).map(camera.projectionMatrix(_)).mkString("[", ",", "]")
      val numericReadouts = prepared.numericReadouts.map: (surface, layer, values) =>
        val samples = values.map(v => if v.isFinite then v.toString else "null").mkString("[", ",", "]")
        s"""{"surface":${quoted(surface.value)},"layer":${quoted(layer.value)},"values":$samples}"""
      .mkString("[", ",", "]")
      val warmTimings = warmSamples.map(_._1.renderNanos).mkString("[", ",", "]")
      val warmSnapshots = warmSamples.map(_._3).mkString("[", ",", "]")
      s"""{"status":"captured","inputSha256":"$inputHash","native":$native,"raster":{"status":"captured","provider":"actual JavaFX ImageView raster host","coldNanos":${cold.renderNanos},"warmNanos":${warm.renderNanos},"coldSnapshotNanos":$coldTotal,"warmSnapshotNanos":$warmTotal,"warmSamplesNanos":$warmTimings,"warmSnapshotSamplesNanos":$warmSnapshots,"warmImagesIdentical":true,"snapshotPixelsEqualPresented":true,"resizeBackExact":true},"viewMatrix":$view,"projectionMatrix":$projection,"matrixOrder":"row-major","picks":$picks,"numericReadouts":$numericReadouts,"disposal":{"canvasPresentedFrameReleased":true}}"""
    finally fx:
      host.dispose()
      require(host.presentedFrame.isEmpty, "disposed JavaFX host retained its presented frame")

  private def snapshot(host: JavaFxRasterSurfaceHost, size: RasterDimensions): Array[Int] = fx:
    val parameters = new SnapshotParameters()
    parameters.setFill(Color.WHITE)
    parameters.setViewport(new Rectangle2D(0, 0, size.width, size.height))
    val image = host.root.snapshot(parameters, new WritableImage(size.width, size.height))
    val actual = read(image, size)
    val frame = host.presentedFrame.get
    require(frame.dimensions == size, "presented dimensions are stale")
    val source = frame.decorations.fold(frame.scientific.image)(_.image)
    for index <- actual.indices do
      val color = source.pixelUnsafe(index % size.width, index / size.width)
      val expected = (color.alpha << 24) | (color.red << 16) | (color.green << 8) | color.blue
      require(actual(index) == expected, s"host pixel $index changed at publication")
    actual

  private def read(image: WritableImage, size: RasterDimensions): Array[Int] =
    Array.tabulate(size.pixelCount)(index => image.getPixelReader.getArgb(index % size.width, index / size.width))

  private def save(pixels: Array[Int], width: Int, height: Int, path: Path): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, width, height, pixels, 0, width)
    require(ImageIO.write(image, "png", path.toFile), "PNG writer unavailable")
