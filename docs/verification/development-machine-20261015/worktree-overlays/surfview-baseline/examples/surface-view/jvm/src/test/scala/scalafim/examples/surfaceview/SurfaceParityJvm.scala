package scalafim.examples.surfaceview

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.{CompletableFuture, TimeUnit}
import javax.imageio.ImageIO
import javafx.application.Platform
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.javafx.*
import scalafim.surface.view.raster.SurfaceRasterizer

/** Actual production JavaFX and portable raster captures of the same baseline inputs. */
object SurfaceParityJvm:
  def main(args: Array[String]): Unit =
    require(args.length == 1, "usage: SurfaceParityJvm OUTPUT_DIRECTORY")
    val output = Path.of(args(0))
    Files.createDirectories(output)
    val completed = new CompletableFuture[Unit]()
    Platform.startup(() =>
      try
        for id <- SurfaceParityFixture.CaseIds do
          val directory = output.resolve(id)
          Files.createDirectories(directory)
          val example = SurfaceParityFixture.example(id)
          val raster = SurfaceRasterizer.render(example.plan, RasterDimensions.unsafe(512, 512))
            .fold(error => throw new IllegalStateException(error.message), identity)
          val portable = new BufferedImage(512, 512, BufferedImage.TYPE_INT_ARGB)
          for y <- 0 until 512; x <- 0 until 512 do
            val color = raster.image.pixelUnsafe(x, y)
            portable.setRGB(x, y, (color.alpha << 24) | (color.red << 16) | (color.green << 8) | color.blue)
          require(ImageIO.write(portable, "png", directory.resolve("scalafim-raster.png").toFile))
          val backend = JavaFxSurfaceBackend.create()
            .fold(error => throw new IllegalStateException(error.message), identity)
          val stage = new Stage()
          try
            val render = backend.render(example.plan)
              .fold(error => throw new IllegalStateException(error.message), identity)
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(512, 512).toOption.get)
              .fold(error => throw new IllegalStateException(error.message), identity)
            stage.setScene(new Scene(new Group(scene), 512.0, 512.0))
            stage.show()
            val snapshot = scene.snapshot(null, new WritableImage(512, 512))
            val image = new BufferedImage(512, 512, BufferedImage.TYPE_INT_ARGB)
            for y <- 0 until 512; x <- 0 until 512 do image.setRGB(x, y, snapshot.getPixelReader.getArgb(x, y))
            require(ImageIO.write(image, "png", directory.resolve("scalafim-javafx.png").toFile))
            val receipt = s"""{"case":"$id","route":"jvm-viewer","renderer":"javafx-scene3d","constructor":"production-default","javaVersion":"${System.getProperty("java.version")}","renderNanos":${render.elapsedNanos},"rasterNanos":${raster.receipt.renderNanos},"width":512,"height":512,"qualification":"baseline-only"}"""
            Files.writeString(directory.resolve("jvm-receipt.json"), receipt + "\n")
          finally
            stage.close()
            backend.dispose().fold(error => throw new IllegalStateException(error.message), identity)
        completed.complete(())
      catch case error: Throwable => completed.completeExceptionally(error)
    )
    try completed.get(60, TimeUnit.SECONDS)
    finally Platform.exit()
