package scalafim.surface.view.javafx

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javax.imageio.ImageIO
import scala.concurrent.Await
import scala.concurrent.duration.*

import image4s.geometry.{D3, Frame}
import javafx.application.Platform
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.stage.Stage

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Replays the linked temporal/morph specimen through actual JavaFX hosts.
  * JavaFX snapshots are compared with the published ImageView pixels; no
  * synthetic image substitutes for the hosted result.
  */
object JavaFxLinkedViewProbe:
  private def ok[A](value: Either[?, A]): A =
    value.fold(error => throw new IllegalStateException(s"$error"), identity)

  private def fx[A](body: => A): A =
    val latch = new CountDownLatch(1)
    var result: Either[Throwable, A] = Left(new IllegalStateException("FX callback missing"))
    Platform.runLater: () =>
      try result = Right(body)
      catch case error: Throwable => result = Left(error)
      finally latch.countDown()
    require(latch.await(30, TimeUnit.SECONDS), "JavaFX callback timed out")
    result.fold(throw _, identity)

  def main(args: Array[String]): Unit =
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-linked-view"))
    Files.createDirectories(output)
    Platform.startup(() => ())
    var ownedHost = Option.empty[JavaFxRasterSurfaceHost]
    var ownedFlat = Option.empty[JavaFxSurfaceFlatMapHost]
    var ownedStage = Option.empty[Stage]
    try
      fx(Platform.setImplicitExit(false))
      val host = fx(JavaFxRasterSurfaceHost.create())
      ownedHost = Some(host)
      val flat = fx(ok(JavaFxSurfaceFlatMapHost.create()))
      ownedFlat = Some(flat)
      val stage = fx(new Stage())
      ownedStage = Some(stage)
      fx:
        flat.canvas.setLayoutX(530)
        stage.setScene(new Scene(new Group(host.imageView, flat.canvas), 760, 512, Color.TRANSPARENT))
        stage.show()
      run(output, host, flat)
    finally
      try fx:
        try ownedStage.foreach(_.close())
        finally
          try ownedFlat.foreach(_.dispose())
          finally ownedHost.foreach(_.dispose())
      finally Platform.exit()

  private def run(output: Path, host: JavaFxRasterSurfaceHost, flat: JavaFxSurfaceFlatMapHost): Unit =
    val dimensions = RasterDimensions.unsafe(512, 512)
    val flatViewport = ok(SurfaceFlatMapViewport.make(220, 220, 12, SurfaceFlatMapFit.Contain))
    var playback = SurfaceLinkedFixture.playback(0)
    var model = SurfaceLinkedFixture.model(playback)
    var state = SurfaceLinkedFixture.state(model, selected = 0, morph = 0)
    val initialNavigation = SurfaceLinkedFixture.navigation(model, state)
    var navigation = ok(SurfaceCompositionState.make(initialNavigation.views,
      Vector(Set(initialNavigation.views(1).id, initialNavigation.views(2).id))))

    fx(ok(flat.render(SurfaceLinkedFixture.FlatMap, flatViewport)))
    fx(ok(flat.sync(model, state)))
    val (_, initialPixels) = render(host, model, state, navigation, dimensions, output.resolve("initial-javafx.png"))
    captureFlat(flat, output.resolve("initial-flat-javafx.png"))

    val vertexTwo = SurfaceLinkedFixture.FlatMap.project(VertexId(2), flatViewport).getOrElse(
      throw new IllegalStateException("linked fixture did not project vertex 2"))
    fx(ok(flat.selectAt(vertexTwo.x, vertexTwo.y)).getOrElse(
      throw new IllegalStateException("flat-map missed vertex 2"))) match
      case SurfaceFlatMapInteraction.Select(action) => state = ok(SurfaceViewer.reduce(model, state, action))
      case other => throw new IllegalStateException(s"unexpected flat-map selection event: $other")
    fx(ok(flat.sync(model, state)))
    require(state.selection.exists(_.vertex == VertexId(2)), s"flat-map selection was not reduced: ${state.selection}")
    val (selectedPlan, _) = render(host, model, state, navigation, dimensions, output.resolve("flat-selected-javafx.png"))
    require(selectedPlan.views.length == 4 && selectedPlan.views.map(_.camera).distinct.length == 4,
      "linked views lost independent cameras")
    val selectedPackets = ok(SurfaceDecorationCompiler.compile(SurfaceLinkedFixture.marker(state), selectedPlan))
    require(selectedPackets.markers.length == 4 && selectedPackets.markers.forall(hasVertex(_, VertexId(2))),
      "flat selection did not produce four markers at vertex 2")
    captureFlat(flat, output.resolve("flat-selected-flat-javafx.png"))

    playback = ok(SurfacePlayback.reduce(SurfaceLinkedFixture.Axis, playback, SurfacePlaybackAction.Play))
    playback = ok(SurfacePlayback.reduce(SurfaceLinkedFixture.Axis, playback,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(.5))))
    model = SurfaceLinkedFixture.model(playback)
    val (fractionalPlan, _) = render(host, model, state, navigation, dimensions, output.resolve("fractional-javafx.png"))
    val expectedHalf = SurfaceLinkedFixture.Values(2)(0) * .5 + SurfaceLinkedFixture.Values(2)(1) * .5
    require(playback.playhead.value == .5 && playback.status == SurfacePlaybackStatus.Playing &&
      fractionalPlan.views.forall(view => math.abs(view.readouts.head.layerValues.head._2.toDouble - expectedHalf) < 1e-10),
      "fractional playback readouts differ from the independent interpolation")

    playback = ok(SurfacePlayback.reduce(SurfaceLinkedFixture.Axis, playback, SurfacePlaybackAction.Pause))
    require(ok(SurfacePlayback.reduce(SurfaceLinkedFixture.Axis, playback,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(1)))) == playback, "paused playback advanced")
    state = ok(SurfaceViewer.reduce(model, state,
      SurfaceViewerAction.SetGeometryMorphFraction(SurfaceLinkedFixture.Surface, SurfaceMorphFraction.unsafe(.5))))
    render(host, model, state, navigation, dimensions, output.resolve("paused-morph-midpoint-javafx.png"))

    val picked = pickScientific(host)
    require(picked.vertex == 0, "known lower-left pick did not resolve scientific vertex 0")
    state = ok(SurfaceViewer.reduce(model, state, SurfaceViewerAction.Select(picked.surface, VertexId(picked.vertex))))
    fx(ok(flat.sync(model, state)))
    require(state.selection.exists(selection => selection.surface == picked.surface && selection.vertex == VertexId(picked.vertex)),
      "3D ImageView pick did not become the flat-map scientific selection")
    captureFlat(flat, output.resolve("three-picked-flat-javafx.png"))
    val (pickedPlan, _) = render(host, model, state, navigation, dimensions, output.resolve("three-picked-javafx.png"))
    val pickedPackets = ok(SurfaceDecorationCompiler.compile(SurfaceLinkedFixture.marker(state), pickedPlan))
    require(pickedPackets.markers.length == 4 && pickedPackets.markers.forall(hasVertex(_, VertexId(picked.vertex))),
      "3D pick marker identity differs across the four views")

    state = ok(SurfaceViewer.reduce(model, state,
      SurfaceViewerAction.SetGeometryMorphFraction(SurfaceLinkedFixture.Surface, SurfaceMorphFraction.unsafe(1))))
    require(state.geometryPresentations(SurfaceLinkedFixture.Surface) == SurfaceGeometryPresentation.Fixed(SurfaceKind.Inflated),
      "morph endpoint was not canonicalized to inflated geometry")
    navigation = ok(navigation.navigate(navigation.views.head.id, SurfaceCompositionNavigation.PanBy(.15, .05)))
    require(navigation.views.head.pose.target.coordinates == Vector(.15, .05, 0.0),
      "nondefault linked-view pan was not retained")
    val (endpointPlan, endpointPixels) = render(host, model, state, navigation, dimensions, output.resolve("morph-endpoint-javafx.png"))

    val resized = RasterDimensions.unsafe(384, 384)
    ok(Await.result(host.resize(resized), 30.seconds))
    require(host.presentedFrame.exists(_.dimensions == resized), "decorated host resize did not republish a frame")
    snapshot(host, endpointPlan, resized, output.resolve("resized-javafx.png"))
    ok(Await.result(host.resize(dimensions), 30.seconds))
    val resizePixels = snapshot(host, endpointPlan, dimensions, output.resolve("morph-endpoint-after-resize-javafx.png"))
    require(endpointPixels.sameElements(resizePixels), "resize/re-lower changed endpoint pixels")

    val playbackJson = SurfacePlaybackDocument.encode(SurfaceLinkedFixture.document(playback))
    val restoredPlayback = ok(SurfacePlaybackDocument.decode(playbackJson))
    require(restoredPlayback.state == playback, "paused playback did not round-trip")
    val compositionJson = SurfaceCompositionCodec.encode(ok(SurfaceCompositionDocument.capture(model, state, navigation,
      SurfaceLinkedFixture.Bindings, SurfaceLinkedFixture.Provenance)))
    val composition = ok(SurfaceCompositionCodec.decode(compositionJson))
    val restoredLayer = ok(restoredPlayback.prepare(SurfaceLinkedFixture.Field, SurfaceLinkedFixture.Reference,
      SurfaceLinkedFixture.Mapping, SurfaceFrameCache.Disabled)).layer
    model = ok(SurfaceViewerModel.make(Vector(SurfaceLinkedFixture.Asset), Vector(restoredLayer)))
    val display = navigation.views.head.instances.head.display
    val restored = ok(composition.restore(model, SurfaceLinkedFixture.Bindings, Map(SurfaceLinkedFixture.Surface -> display)))
    require(restored.navigation.cameraLinks == navigation.cameraLinks &&
      restored.navigation.views.map(view => (view.id, view.viewport, view.pose.eye.coordinates, view.pose.target.coordinates)) ==
        navigation.views.map(view => (view.id, view.viewport, view.pose.eye.coordinates, view.pose.target.coordinates)),
      "composition restore changed linked navigation")
    val restoredState = ok(composition.scene.restore(model, SurfaceLinkedFixture.Bindings)).copy(
      geometryPresentations = restored.presentation.geometryPresentations)
    val (_, restoredPixels) = render(host, model, restoredState, restored.navigation, dimensions,
      output.resolve("restored-javafx.png"))
    require(endpointPixels.sameElements(restoredPixels), "restored linked composition pixels differ")

    playback = ok(SurfacePlayback.reduce(SurfaceLinkedFixture.Axis, restoredPlayback.state, SurfacePlaybackAction.Play))
    playback = ok(SurfacePlayback.reduce(SurfaceLinkedFixture.Axis, playback,
      SurfacePlaybackAction.Tick(SurfaceSeconds.unsafe(.25))))
    require(playback.playhead.value == .75, s"resumed playhead was ${playback.playhead.value}, expected .75")
    model = SurfaceLinkedFixture.model(playback)
    render(host, model, restoredState, restored.navigation, dimensions, output.resolve("resumed-javafx.png"))
    Files.writeString(output.resolve("linked-view.json"),
      s"{\"passed\":true,\"views\":4,\"selectedVertex\":${restoredState.selection.get.vertex.index},\"playhead\":0.75,\"morph\":1.0,\"initialPixels\":${initialPixels.length}}\n")

  private def render[F <: Frame[D3]](host: JavaFxRasterSurfaceHost, model: SurfaceViewerModel, state: SurfaceViewerState,
      navigation: SurfaceCompositionState[F], dimensions: RasterDimensions, path: Path): (SurfaceCompositionPlan, Array[Int]) =
    val plan = ok(SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), navigation.views))
    val receipt = ok(Await.result(host.submitDecorated(plan, dimensions, (current, size) =>
      SurfaceDecorationCompiler.compile(SurfaceLinkedFixture.marker(state, size.width.toDouble / dimensions.width), current)
        .left.map(_.toString).flatMap(value => SurfaceDecorationGeometry.lower(value, current, size.width, size.height).left.map(_.toString))), 30.seconds))
    require(host.presentedFrame.exists(_.generation == receipt.generation))
    plan -> snapshot(host, plan, dimensions, path)

  private def snapshot(host: JavaFxRasterSurfaceHost, plan: SurfaceCompositionPlan,
      dimensions: RasterDimensions, path: Path): Array[Int] = fx:
      val frame = host.presentedFrame.getOrElse(throw new IllegalStateException("raster host did not publish a frame"))
      require(frame.plan.receipt == plan.receipt && frame.dimensions == dimensions,
        "JavaFX host published a mismatched render receipt")
      val image = host.imageView.snapshot(null, new WritableImage(dimensions.width, dimensions.height))
      val actual = pixelsOf(image)
      val expected = pixelsOf(frame.decorations.fold(frame.scientific.image)(_.image))
      require(actual.sameElements(expected), "JavaFX ImageView snapshot differs from its published raster")
      write(image, path)
      actual

  private def pickScientific(host: JavaFxRasterSurfaceHost): SurfacePick = fx:
    host.pickDisplay(64.5, 192.5) match
      case Right(Some(SurfaceDisplayHit.Scientific(value))) => value
      case other => throw new IllegalStateException(s"known lower-left scientific pick failed: $other")

  private def hasVertex(marker: SurfaceMarkerPacket, vertex: VertexId): Boolean = marker.anchor match
    case SurfaceDecorationAnchor.Vertex(anchor) => anchor.surface == SurfaceLinkedFixture.Surface && anchor.vertex == vertex
    case _ => false

  private def captureFlat(host: JavaFxSurfaceFlatMapHost, path: Path): Unit = fx:
    val width = math.round(host.canvas.getWidth).toInt
    val height = math.round(host.canvas.getHeight).toInt
    write(host.canvas.snapshot(null, new WritableImage(width, height)), path)

  private def pixelsOf(image: WritableImage): Array[Int] =
    val width = math.round(image.getWidth).toInt
    Array.tabulate(width * math.round(image.getHeight).toInt)(index => image.getPixelReader.getArgb(index % width, index / width))

  private def pixelsOf(image: RasterImage): Array[Int] = Array.tabulate(image.width * image.height): index =>
    val pixel = image.pixelUnsafe(index % image.width, index / image.width)
    (pixel.alpha << 24) | (pixel.red << 16) | (pixel.green << 8) | pixel.blue

  private def write(image: WritableImage, path: Path): Unit =
    val width = math.round(image.getWidth).toInt
    val height = math.round(image.getHeight).toInt
    val output = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    output.setRGB(0, 0, width, height, pixelsOf(image), 0, width)
    require(ImageIO.write(output, "png", path.toFile), s"no PNG writer for $path")
