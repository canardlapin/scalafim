package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javafx.application.Platform
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.scene.SnapshotParameters
import javafx.stage.Stage

import scalafim.surface.VertexId
import scalafim.surface.view.*

/** Writes Canvas snapshots for direct visual comparison with the browser
  * FlatMapView reference. It owns the JavaFX toolkit it starts. */
object JavaFxFlatMapProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(String.valueOf(error)), identity)

  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-flatmap-javafx"))
    try
      Platform.startup: () =>
        try run(output)
        catch case error: Throwable => failure = error
        finally done.countDown()
      if !done.await(30, TimeUnit.SECONDS) then
        throw new IllegalStateException("JavaFX flat-map probe timed out after 30 seconds")
    finally Platform.exit()
    if failure != null then throw failure.nn

  private def run(output: Path): Unit =
    Files.createDirectories(output)
    val surface = SurfaceFaceFixture.Surface
    val map = ok(SurfaceFlatMap.make(
      surface,
      Vector(SurfaceFlatMapPoint(0, 0), SurfaceFlatMapPoint(2, 0), SurfaceFlatMapPoint(2, 1), SurfaceFlatMapPoint(0, 1)),
      Vector((0, 1, 2), (0, 2, 3))
    ))
    val first = ok(SurfaceFlatMapViewport.make(100, 100, 10, SurfaceFlatMapFit.Contain))
    val resized = ok(SurfaceFlatMapViewport.make(200, 100, 10, SurfaceFlatMapFit.Contain))
    val style = JavaFxSurfaceFlatMapStyle()
    val host = ok(JavaFxSurfaceFlatMapHost.create())
    val stage = new Stage()
    stage.setScene(new Scene(new Group(host.canvas), 200, 100))
    stage.show()
    def capture(stageName: String): Unit =
      val width = math.round(host.canvas.getWidth).toInt
      val height = math.round(host.canvas.getHeight).toInt
      val parameters = new SnapshotParameters()
      parameters.setFill(Color.TRANSPARENT)
      val image = host.canvas.snapshot(parameters, new WritableImage(width, height))
      val file = output.resolve(s"$stageName.png").toFile
      require(ImageIO.write(toBufferedImage(image), "png", file), s"no PNG writer for $stageName")
      println(s"flatmap_stage=$stageName width=$width height=$height png=${file.getAbsolutePath}")
    try
      ok(host.render(map, first, style))
      capture("plain")
      val hover = ok(host.hoverAt(10, 70))
      require(hover == SurfaceFlatMapInteraction.Hover(surface, Some(VertexId(0))), s"unexpected hover: $hover")
      capture("hover")
      val click = ok(host.selectAt(10, 70)).getOrElse(throw new IllegalStateException("flat-map click missed vertex"))
      require(click == SurfaceFlatMapInteraction.Select(SurfaceViewerAction.Select(surface, VertexId(0))), s"unexpected click: $click")
      capture("selected")
      require(host.selectAt(Double.NaN, 70).left.toOption.contains(
        JavaFxSurfaceFlatMapHostError.Pointer("coordinates and radius must be finite; radius must be positive")))
      ok(host.beginRoi(SurfaceFlatMapRoiMode.Polygon, 8, 72))
      ok(host.appendRoiPoint(92, 72))
      ok(host.appendRoiPoint(92, 28))
      ok(host.appendRoiPoint(8, 28))
      val polygon = ok(host.finishRoi("resize-stable")) match
        case SurfaceFlatMapInteraction.RoiCreated(value) => value
        case other => throw new IllegalStateException(s"unexpected polygon event: $other")
      require(polygon.vertices.map(_.index) == Vector(0, 1, 2, 3), s"unexpected polygon vertices: ${polygon.vertices}")
      capture("polygon-roi")
      ok(host.resize(resized))
      ok(host.updateRoi(polygon))
      capture("resized")
      val model = SurfaceFaceFixture.model
      val state0 = SurfaceFaceFixture.state(model)
      val state1 = ok(SurfaceViewer.reduce(model, state0, SurfaceViewerAction.SetTimepoint(1)))
      val selectedState = ok(SurfaceViewer.reduce(model, state1, SurfaceViewerAction.Select(surface, VertexId(2))))
      ok(host.sync(model, selectedState))
      capture("synced-time-selection")
      ok(host.beginRoi(SurfaceFlatMapRoiMode.Lasso, 16, 94))
      ok(host.appendRoiPoint(104, 94))
      ok(host.appendRoiPoint(104, 6))
      ok(host.appendRoiPoint(16, 6))
      val lasso = ok(host.finishRoi("lasso-resize-stable")) match
        case SurfaceFlatMapInteraction.RoiCreated(value) => value
        case other => throw new IllegalStateException(s"unexpected lasso event: $other")
      require(lasso.vertices.map(_.index) == Vector(0, 3), s"unexpected lasso vertices: ${lasso.vertices}")
      capture("lasso-roi")
      val wrongThread = new AtomicReference[Either[JavaFxSurfaceFlatMapHostError, Option[SurfaceFlatMapInteraction]]]()
      val worker = new Thread(() => wrongThread.set(host.selectAt(10, 70)), "javafx-flatmap-wrong-thread-probe")
      worker.start()
      worker.join()
      require(wrongThread.get == Left(JavaFxSurfaceFlatMapHostError.WrongThread), s"unexpected wrong-thread result: ${wrongThread.get}")
      ok(host.dispose())
      require(host.selectAt(10, 70) == Left(JavaFxSurfaceFlatMapHostError.Disposed))
      println(s"flatmap_events hover=$hover click=$click polygon=${polygon.vertices.map(_.index).mkString(",")} lasso=${lasso.vertices.map(_.index).mkString(",")} timepoint=${selectedState.timepoint} selection=${selectedState.selection.map(_.vertex.index)} style=$style")
    finally
      host.dispose()
      stage.close()

  private def toBufferedImage(image: WritableImage): BufferedImage =
    val width = math.round(image.getWidth).toInt
    val height = math.round(image.getHeight).toInt
    val output = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    val reader = image.getPixelReader
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        output.setRGB(x, y, reader.getArgb(x, y))
        x += 1
      y += 1
    output
