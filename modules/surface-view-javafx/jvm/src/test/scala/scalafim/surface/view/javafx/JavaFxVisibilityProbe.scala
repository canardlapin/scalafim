package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.stage.Stage
import scala.concurrent.Await
import scala.concurrent.duration.*
import intaglio.*
import scalafim.surface.view.*

/** Actual JavaFX ImageView/scene publication of the shared visibility specimens. */
object JavaFxVisibilityProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new AssertionError(s"$error"), identity)
  private def fx[A](body: => A): A =
    val latch = new CountDownLatch(1)
    var result: Either[Throwable, A] = Left(new AssertionError("FX callback missing"))
    Platform.runLater: () =>
      try result = Right(body)
      catch case error: Throwable => result = Left(error)
      finally latch.countDown()
    require(latch.await(30, TimeUnit.SECONDS), "FX callback timed out")
    result.fold(throw _, identity)

  def main(args: Array[String]): Unit =
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-visibility"))
    Files.createDirectories(output)
    Platform.startup(() => ())
    fx(Platform.setImplicitExit(false))
    val host = fx(JavaFxRasterSurfaceHost.create())
    val stage = fx:
      val value = new Stage()
      value.setScene(new Scene(new Group(host.imageView), 256, 256, Color.WHITE))
      value.show()
      value
    val scenes = SurfaceVisibilityFixture.Cases.map(name => name -> ok(SurfaceCompositionPlan.fromLegacy(SurfaceVisibilityFixture.plan(name)))) ++
      (for opacity <- Vector(.5, 0.0); reverse <- Vector(false, true) yield
        s"surface-alpha-$opacity-$reverse" -> SurfaceVisibilityFixture.alphaComposition(opacity, reverse))
    val rows = Vector.newBuilder[String]
    try
      for (name, plan) <- scenes do
        val receipt = ok(Await.result(host.submit(plan, RasterDimensions.unsafe(256, 256)), 30.seconds))
        val pixels = fx:
          val image = stage.getScene.snapshot(new WritableImage(256, 256))
          val frame = host.presentedFrame.get
          val values = Array.tabulate(256 * 256)(index => image.getPixelReader.getArgb(index % 256, index / 256))
          var index = 0
          while index < values.length do
            val color = frame.scientific.image.pixelUnsafe(index % 256, index / 256)
            val expected = (color.alpha << 24) | (color.red << 16) | (color.green << 8) | color.blue
            assert(values(index) == expected, s"$name pixel $index differs after JavaFX publication")
            index += 1
          assert(ok(host.pickDisplay(128, 128)).nonEmpty)
          values
        val image = new java.awt.image.BufferedImage(256, 256, java.awt.image.BufferedImage.TYPE_INT_ARGB)
        image.setRGB(0, 0, 256, 256, pixels, 0, 256)
        require(javax.imageio.ImageIO.write(image, "png", output.resolve(s"$name-javafx.png").toFile))
        rows += s"""{"name":"$name","pixelsVerified":65536,"renderNanos":${receipt.renderNanos}}"""
      Files.writeString(output.resolve("javafx-visibility.json"),
        s"""{"provider":"JavaFX-hosted raster","passed":true,"cases":[${rows.result().mkString(",")}]}\n""")
      println(s"JavaFX visibility: ${scenes.length} scene snapshots agree with published scientific pixels; $output")
    finally
      fx:
        host.dispose()
        stage.close()
      Platform.exit()
