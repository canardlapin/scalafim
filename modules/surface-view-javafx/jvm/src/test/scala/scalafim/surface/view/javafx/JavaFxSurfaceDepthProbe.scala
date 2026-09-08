package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing}
import javafx.scene.image.WritableImage
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Actual framebuffer checks for explicit eye-space clipping, including resize
  * and switching back to default depth limits on the retained backend.
  */
object JavaFxSurfaceDepthProbe:
  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      val stage = new Stage()
      val rows = Vector.newBuilder[String]
      try
        for approximate <- Vector(false, true); perspective <- Vector(false, true) do
          val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-2, 4),
            ScalarRamp.linear(Rgba32.unsafe(0, 0, 200), Rgba32.unsafe(200, 0, 0))))
          val scalarModel = SurfaceScalarFixture.model(mapping)
          val model = if approximate then scalarModel else
            val layer = SurfaceLayer.scalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
              SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, mapping.colorizer).toOption.get
            SurfaceViewerModel.make(scalarModel.surfaces, Vector(layer)).toOption.get
          val initial = SurfaceFaceFixture.state(model)
          val projected = if !perspective then initial else SurfaceViewer.reduce(model, initial,
            SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
          val state = SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
          val full = SurfaceCompiler.compile(model, state).toOption.get
          val middle = -full.camera.viewMatrix(11).toDouble
          val backend = (if approximate then JavaFxSurfaceBackend.createApproximate(
            JavaFxApproximationConfig.make(maxChannelError = 2).toOption.get) else JavaFxSurfaceBackend.create()).toOption.get
          try
            backend.render(full).toOption.get
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(128, 128, SceneAntialiasing.DISABLED).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(scene), 128, 128))
            stage.show()
            for
              (name, clipping) <- Vector("near" -> SurfaceClipping.NearFar(middle, 1000),
                "far" -> SurfaceClipping.NearFar(0.01, middle), "restored" -> SurfaceClipping.Disabled)
              (width, height) <- Vector((128, 128), (192, 128))
            do
              val plan = SurfaceCompiler.compile(model, state.copy(clipping = clipping)).toOption.get
              scene.setWidth(width)
              scene.setHeight(height)
              stage.setWidth(width + 40)
              backend.render(full).fold(e => throw new IllegalArgumentException(e.message), identity)
              val before = scene.snapshot(null, new WritableImage(width, height))
              backend.render(plan).fold(e => throw new IllegalArgumentException(e.message), identity)
              val actual = scene.snapshot(null, new WritableImage(width, height))
              val dimensions = RasterDimensions.unsafe(width, height)
              val expected = SurfaceRasterizer.render(plan, dimensions, SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
              val baseline = SurfaceRasterizer.render(full, dimensions, SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
              var kept = 0
              var removed = 0
              var maximumError = 0
              var maximumReferenceError = 0
              for y <- 5 until height - 5; x <- 5 until width - 5; p <- baseline.pick(x, y).toOption.flatten do
                val view = full.camera.viewMatrix
                val mesh = full.meshes.head
                val (a, b, c) = mesh.sourceFaceVertices(p.face)
                def vertexDepth(vertex: Int): Double =
                  -(view(8) * mesh.positions(vertex * 3) + view(9) * mesh.positions(vertex * 3 + 1) +
                    view(10) * mesh.positions(vertex * 3 + 2) + view(11)).toDouble
                val depth = p.barycentricA * vertexDepth(a) + p.barycentricB * vertexDepth(b) + p.barycentricC * vertexDepth(c)
                if math.min(p.barycentricA, math.min(p.barycentricB, p.barycentricC)) > 0.1 && math.abs(depth - middle) > 0.08 then
                  val color = expected.image.pixelUnsafe(x, y)
                  val pixel = actual.getPixelReader.getArgb(x, y)
                  val error = Vector(math.abs(((pixel >>> 16) & 255) - color.red),
                    math.abs(((pixel >>> 8) & 255) - color.green), math.abs((pixel & 255) - color.blue)).max
                  maximumReferenceError = math.max(maximumReferenceError, error)
                  val keep = expected.pick(x, y).toOption.flatten.nonEmpty
                  val prior = if keep then before.getPixelReader.getArgb(x, y) else -1
                  val change = Vector(16, 8, 0).map(shift => math.abs(((pixel >>> shift) & 255) - ((prior >>> shift) & 255))).max
                  maximumError = math.max(maximumError, change)
                  if keep then kept += 1 else removed += 1
              require(kept > 100 && (name == "restored" || removed > 100), s"insufficient clipping regions: $kept/$removed")
              require(maximumError <= (if approximate then 3 else 2), s"depth $name approximate=$approximate perspective=$perspective $width/$height error=$maximumError")
              // Legacy UV color interpolation has its own known approximation;
              // depth changes must preserve its actual pre-clip pixels. The
              // bounded scalar path additionally has a reference-color budget.
              require(!approximate || maximumReferenceError <= 3, s"bounded depth reference error=$maximumReferenceError")
              val row = s"{\"mode\":\"$name\",\"approximate\":$approximate,\"perspective\":$perspective,\"width\":$width,\"height\":$height,\"kept\":$kept,\"removed\":$removed,\"maximumError\":$maximumError,\"maximumReferenceError\":$maximumReferenceError}"
              rows += row
              println(s"depth_probe=$row")
          finally backend.dispose()
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-javafx-depth.json"), rows.result().mkString("[", ",", "]"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(180, TimeUnit.SECONDS), "JavaFX depth probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()
