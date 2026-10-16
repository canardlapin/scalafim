package scalafim.surface.view

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javax.imageio.ImageIO
import scala.concurrent.Await
import scala.concurrent.duration.*
import _root_.javafx.application.Platform
import _root_.javafx.scene.SnapshotParameters
import _root_.javafx.scene.image.WritableImage
import _root_.javafx.scene.paint.Color
import intaglio.RasterDimensions
import scalafim.surface.VertexId
import scalafim.surface.view.javafx.*
import scalafim.surface.view.raster.*

/** JVM C11 receipt producer. It drives JavaFX's production Canvas host and,
  * for linked-marker, its production raster display host. */
object SurfaceFlatMapQualificationJvmProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(String.valueOf(error)), identity)
  private def fx[A](body: => A): A =
    if Platform.isFxApplicationThread then body
    else
      val latch = new CountDownLatch(1)
      var result: Either[Throwable, A] = Left(new IllegalStateException("missing FX result"))
      Platform.runLater: () =>
        try result = Right(body)
        catch case error: Throwable => result = Left(error)
        finally latch.countDown()
      require(latch.await(60, TimeUnit.SECONDS), "JavaFX action timed out")
      result.fold(throw _, identity)

  def main(args: Array[String]): Unit =
    require(args.length == 1, "usage: output-directory")
    val output = Path.of(args(0))
    Files.createDirectories(output)
    Platform.startup(() => ())
    try run(output)
    finally Platform.exit()

  private def run(output: Path): Unit =
    val fixture = ok(SurfaceFlatMapQualificationFixture.decode)
    val style = JavaFxSurfaceFlatMapStyle(fixture.style.background, fixture.style.fill, fixture.style.stroke,
      fixture.style.hover, fixture.style.selected, fixture.style.roi, fixture.style.pointRadius)
    val host = fx(ok(JavaFxSurfaceFlatMapHost.create()))
    val rows = Vector.newBuilder[String]
    def point(vertex: Int) = ok(fixture.map.project(VertexId(vertex), fixture.viewport))
    def saveCanvas(name: String): Unit = fx:
      val image = host.canvas.snapshot(new SnapshotParameters(), new WritableImage(fixture.source.width, fixture.source.height))
      save(image, output.resolve(s"$name.png"))
    def roi(mode: SurfaceFlatMapRoiMode, points: Vector[SurfaceFlatMapPoint]): SurfaceFlatMapRoi =
      val screen = points.map(fixture.map.projectMapPoint(_, fixture.viewport))
      fx(ok(host.beginRoi(mode, screen.head.x, screen.head.y)))
      screen.tail.foreach(p => fx(ok(host.appendRoiPoint(p.x, p.y))))
      fx(ok(host.finishRoi(SurfaceFlatMapQualificationFixture.RoiName))) match
        case SurfaceFlatMapInteraction.RoiCreated(value) => value
        case other => throw new IllegalStateException(s"unexpected C11 ROI event: $other")
    def companionMarker(): Unit =
      val decorations = ok(fixture.linkedMarker)
      val packets = ok(SurfaceDecorationCompiler.compile(decorations, fixture.source.plan))
      val geometry = ok(SurfaceDecorationGeometry.lower(packets, fixture.source.plan, fixture.source.width, fixture.source.height))
      val raster = fx(JavaFxRasterSurfaceHost.create())
      try
        val size = RasterDimensions.unsafe(fixture.source.width, fixture.source.height)
        ok(Await.result(raster.submit(fixture.source.plan, size, Some(geometry), fixture.source.style), 60.seconds))
        fx:
          val image = raster.root.snapshot(new SnapshotParameters(), new WritableImage(size.width, size.height))
          save(image, output.resolve("linked-marker-display.png"))
      finally raster.dispose()
    def endpoint(name: String, mode: SurfaceFlatMapRoiMode, points: Vector[SurfaceFlatMapPoint], time: Boolean, marker: Boolean): Unit =
      fx(ok(host.render(fixture.map, fixture.viewport, style)))
      val hoverPoint = point(3)
      val hovered = fx(ok(host.hoverAt(hoverPoint.x, hoverPoint.y)))
      val selectedPoint = point(0)
      val event = fx(ok(host.selectAt(selectedPoint.x, selectedPoint.y))).getOrElse(throw new IllegalStateException("C11 select missed vertex 0"))
      val state0 = event match
        case SurfaceFlatMapInteraction.Select(action) => ok(SurfaceViewer.reduce(fixture.model, fixture.initial, action))
        case other => throw new IllegalStateException(s"unexpected C11 selection event: $other")
      val state = if time then ok(SurfaceViewer.reduce(fixture.model, state0, SurfaceViewerAction.SetTimepoint(1))) else state0
      fx(ok(host.sync(fixture.model, state)))
      val created = roi(mode, points)
      saveCanvas(name)
      if marker then companionMarker()
      rows += s"""{"checkpoint":"$name","image":"$name.png","linkedDisplay":${if marker then "\"linked-marker-display.png\"" else "null"},"hover":${quote(hovered.toString)},"selection":${state.selection.map(_.vertex.index).getOrElse(-1)},"timepoint":${state.timepoint},"roiVertices":[${created.vertices.map(_.index).mkString(",")}],"route":"actual JavaFxSurfaceFlatMapHost actions"}"""
    try
      endpoint("hover", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = false)
      endpoint("pick", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = false)
      endpoint("polygon", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = false)
      endpoint("lasso", SurfaceFlatMapRoiMode.Lasso, SurfaceFlatMapQualificationFixture.Lasso, time = false, marker = false)
      endpoint("linked-marker", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = false, marker = true)
      endpoint("linked-time", SurfaceFlatMapRoiMode.Polygon, SurfaceFlatMapQualificationFixture.Polygon, time = true, marker = false)
      Files.writeString(output.resolve("candidate.json"),
        s"""{"schema":"surface-flatmap-qualification.v1","sourceJson":${quote(SurfaceFlatMapQualificationFixture.SourceJson)},"width":600,"height":450,"padding":20,"fit":"contain","captures":[${rows.result().mkString(",")}]}\n""")
    finally fx(ok(host.dispose()))

  private def quote(value: String): String = SceneJson.Str(value).render

  private def save(image: WritableImage, path: Path): Unit =
    val width = math.round(image.getWidth).toInt
    val height = math.round(image.getHeight).toInt
    val data = Array.tabulate(width * height)(index => image.getPixelReader.getArgb(index % width, index / width))
    val buffered = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    buffered.setRGB(0, 0, width, height, data, 0, width)
    require(ImageIO.write(buffered, "png", path.toFile), "PNG writer unavailable")
