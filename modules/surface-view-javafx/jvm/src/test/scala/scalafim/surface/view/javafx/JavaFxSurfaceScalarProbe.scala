package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing}
import javafx.scene.image.WritableImage
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Experimental finite-envelope scalar lookup, isolated from production admission.
  * Budget: <=2 channel values away from discontinuities, >=1000 interior samples.
  * Full-domain errors are reported, never hidden by the interior exclusion.
  */
object JavaFxSurfaceScalarProbe:
  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      val stage = new Stage()
      val rows = Vector.newBuilder[String]
      try
        for antialiasing <- Vector(false, true); perspective <- Vector(false, true); size <- Vector(128, 256); mode <- Vector("affine", "piecewise", "threshold"); width <- Vector(256, 1024, 4096) do
          val threshold = mode == "threshold"
          val mapping = if mode == "affine" then ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-2, 4),
            ScalarRamp.linear(Rgba32.unsafe(0, 0, 0), Rgba32.unsafe(240, 240, 240))))
            else if threshold then SurfaceScalarFixture.thresholded else SurfaceScalarFixture.mapping
          val model = SurfaceScalarFixture.model(mapping)
          val initial = SurfaceFaceFixture.state(model)
          val projected = if !perspective then initial else
            val state = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
              CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
            SurfaceViewer.reduce(model, state, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
          val plan = SurfaceCompiler.compile(model, projected).toOption.get
          val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(size, size),
            SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
          // Use the admitted legacy path only for camera/mesh setup, then replace
          // its UVs/material with this explicitly experimental lowering.
          val layer = SurfaceLayer.scalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, mapping.colorizer).toOption.get
          val legacyModel = SurfaceViewerModel.make(model.surfaces, Vector(layer)).toOption.get
          val legacy = SurfaceCompiler.compile(legacyModel, projected).toOption.get
          val backend = JavaFxSurfaceBackend.create().toOption.get
          try
            backend.render(legacy).toOption.get
            val lookup = new WritableImage(width, 4)
            for x <- 0 until width do
              val sample = -2.0 + 6.0 * x / (width - 1)
              val color = DisplayBlendMode.Normal.composite(Rgba32.unsafe(184, 184, 184), mapping.color(sample), DisplayOpacity.Opaque)
              val argb = (255 << 24) | (color.red << 16) | (color.green << 8) | color.blue
              for y <- 0 until 4 do lookup.getPixelWriter.setArgb(x, y, argb)
            for chunk <- backend.chunks do
              val packet = plan.meshes.find(_.surface == chunk.surface).get
              val uv = new Array[Float](chunk.faceCount * 6)
              for face <- 0 until chunk.faceCount; corner <- 0 until 3 do
                val vertex = packet.indices((chunk.faceStart + face) * 3 + corner)
                val fraction = (SurfaceScalarFixture.Values(vertex) + 2.0) / 6.0
                uv(face * 6 + corner * 2) = ((0.5 + fraction * (width - 1)) / width).toFloat
                uv(face * 6 + corner * 2 + 1) = 0.5f
              chunk.mesh.getTexCoords.setAll(uv, 0, uv.length)
              JavaFxSurfaceProbe.configureMaterial(chunk.material, lookup, JavaFxMaterialMode.Unlit)
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(size, size,
              if antialiasing then SceneAntialiasing.BALANCED else SceneAntialiasing.DISABLED).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(scene), size, size))
            stage.show()
            val image = new WritableImage(size, size)
            scene.snapshot(null, image)
            var checked = 0
            var away = 0
            var maxError = 0
            var awayError = 0
            var bad = 0
            for y <- 0 until size; x <- 0 until size; pick <- reference.pick(x, y).toOption.flatten do
              if math.min(pick.barycentricA, math.min(pick.barycentricB, pick.barycentricC)) > 0.04 then
                val vertices = if pick.face == 0 then Vector(0, 1, 2) else Vector(0, 2, 3)
                val sample = pick.barycentricA * SurfaceScalarFixture.Values(vertices(0)) +
                  pick.barycentricB * SurfaceScalarFixture.Values(vertices(1)) + pick.barycentricC * SurfaceScalarFixture.Values(vertices(2))
                val actual = image.getPixelReader.getArgb(x, y)
                val expected = reference.image.pixelUnsafe(x, y)
                val error = Vector(math.abs(((actual >>> 16) & 255) - expected.red),
                  math.abs(((actual >>> 8) & 255) - expected.green), math.abs((actual & 255) - expected.blue)).max
                checked += 1
                maxError = math.max(maxError, error)
                if error > 2 then bad += 1
                if !threshold || math.min(math.abs(sample + 0.25), math.abs(sample - 0.25)) > 12.0 / (width - 1) then
                  away += 1
                  awayError = math.max(awayError, error)
            require(checked > 1000 && away > 1000, s"insufficient coverage: $checked/$away")
            // Record the outcome even if the candidate misses its accuracy budget.
            val admitted = awayError <= 2
            val row = s"{\"mappingMode\":\"$mode\",\"antialiasing\":$antialiasing,\"perspective\":$perspective,\"size\":$size,\"threshold\":$threshold,\"textureWidth\":$width,\"checked\":$checked,\"awayChecked\":$away,\"maximumError\":$maxError,\"awayMaximumError\":$awayError,\"pixelsOverBudget\":$bad,\"awayBudgetPassed\":$admitted}"
            rows += row
            println(s"scalar_lookup_probe=$row")
            if antialiasing && perspective && size == 256 && threshold && width == 4096 then
              val out = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
              for y <- 0 until size; x <- 0 until size do out.setRGB(x, y, image.getPixelReader.getArgb(x, y))
              javax.imageio.ImageIO.write(out, "png", new java.io.File("/private/tmp/scalafim-scalar-javafx.png"))
          finally backend.dispose()
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-scalar-javafx.json"), rows.result().mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
        Platform.exit()
    require(done.await(120, TimeUnit.SECONDS), "JavaFX scalar probe timed out")
    if failure != null then throw failure.nn
