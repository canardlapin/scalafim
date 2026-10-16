package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Native framebuffer gate: a shared edge must not blend opposing face colors. */
object JavaFxSurfaceFaceProbe:
  def main(args: Array[String]): Unit =
    val model = SurfaceFaceFixture.model
    val state = SurfaceFaceFixture.state(model)
    val plan = SurfaceCompiler.compile(model, state).toOption.get
    val size = 256
    val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(size, size),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      val stage = new Stage()
      var releaseBackend: () => Unit = () => ()
      try
        val backend = JavaFxSurfaceBackend.create().toOption.get
        releaseBackend = () =>
          backend.dispose()
          ()
        backend.render(plan).toOption.get
        val scene = backend.newSubScene(JavaFxSnapshotConfig.make(size, size).toOption.get).toOption.get
        stage.setScene(new Scene(new Group(scene), size, size))
        stage.show()
        val image = new WritableImage(size, size)
        scene.snapshot(null, image)
        val reader = image.getPixelReader
        var checked = 0
        var maximumError = 0
        for
          y <- 0 until size
          x <- 0 until size
          pick <- reference.pick(x, y).toOption.flatten
          if math.min(pick.barycentricA, math.min(pick.barycentricB, pick.barycentricC)) > 0.04
        do
          val expected = reference.image.pixelUnsafe(x, y)
          val actual = reader.getArgb(x, y)
          val error = math.max(math.abs(((actual >>> 16) & 255) - expected.red),
            math.max(math.abs(((actual >>> 8) & 255) - expected.green), math.abs((actual & 255) - expected.blue)))
          maximumError = math.max(maximumError, error)
          checked += 1
        require(checked > 10000, s"insufficient interior coverage: $checked")
        require(maximumError <= 1, s"face colors bled: maximum channel error $maximumError")
        val controller = JavaFxSurfaceController.attach(model, state, backend, scene).toOption.get
        try
          val chunk = backend.chunks.head
          val picked = controller.pick(new _root_.javafx.scene.input.PickResult(chunk.view,
            new _root_.javafx.geometry.Point3D(-0.8, 0.8, 0.0), 4.0, 1, _root_.javafx.geometry.Point2D.ZERO)).toOption.get
          require(picked.face.index == 1 && picked.vertex.index == 3,
            s"render-corner pick lost source identity: $picked")
          require(math.abs(picked.barycentricA - 0.1) < 1e-6 && math.abs(picked.barycentricC - 0.8) < 1e-6)
        finally controller.dispose()
        val next = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetTimepoint(1)).toOption.get
        val nextPlan = SurfaceCompiler.compile(model, next).toOption.get
        val program = JavaFxSurfaceProgram.compile(Some(plan), nextPlan)
        require(!program.dirty.geometry, "timepoint changed render topology")
        backend.render(nextPlan).toOption.get
        val out = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
        for y <- 0 until size; x <- 0 until size do out.setRGB(x, y, reader.getArgb(x, y))
        javax.imageio.ImageIO.write(out, "png", new java.io.File("/private/tmp/scalafim-facewise-javafx.png"))
        println(s"facewise_javafx=pass checked=$checked maximum_channel_error=$maximumError source_vertices=4 render_vertices=6")
      catch case error: Throwable => failure = error
      finally
        releaseBackend()
        stage.close()
        done.countDown()
    val completed = done.await(60, TimeUnit.SECONDS)
    Platform.exit()
    require(completed, "JavaFX facewise probe timed out")
    if failure != null then throw failure.nn
