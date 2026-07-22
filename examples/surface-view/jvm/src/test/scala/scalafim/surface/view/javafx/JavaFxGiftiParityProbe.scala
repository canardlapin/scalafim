package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.application.Platform
import javafx.geometry.{Point2D, Point3D}
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.scene.input.PickResult
import javafx.stage.Stage

import scalafim.examples.surfaceview.*
import scalafim.graphics.*
import scalafim.surface.view.raster.*

/** Live direct-parity gate for the exact scene used by the WebGL example. */
object JavaFxGiftiParityProbe:
  def main(args: Array[String]): Unit =
    val example = SurfaceViewerExample.fromGifti(JavaFxSurfaceViewerExample.readGifti())
      .fold(error => throw new IllegalStateException(error.message), identity)
    val dimensions = SurfaceViewerVisualQaFixture.Dimensions
    val reference = SurfaceViewerVisualQaFixture.reference(example)
    val referencePick = SurfaceViewerVisualQaFixture.referencePick(reference)
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      var stage: Stage | Null = null
      var controller: JavaFxSurfaceController | Null = null
      var backend: JavaFxSurfaceBackend | Null = null
      try
        backend = JavaFxSurfaceBackend.create().toOption.get
        val interpreted = backend.nn.render(example.plan).toOption.get
        val config = JavaFxSnapshotConfig.make(dimensions.width, dimensions.height).toOption.get
        val subScene = backend.nn.newSubScene(config).toOption.get
        stage = new Stage()
        stage.nn.setScene(new Scene(new Group(subScene), dimensions.width.toDouble, dimensions.height.toDouble))
        stage.nn.show()
        val snapshot = new WritableImage(dimensions.width, dimensions.height)
        subScene.snapshot(null, snapshot)
        val observed = rasterImage(snapshot, dimensions)
        val visualQa = SurfaceVisualQa.compare(reference.image, observed).toOption.get
        val violations = visualQa.violations(SurfaceViewerVisualQaFixture.Policy)
        val expectedBounds = foregroundBounds(reference.image)
        val observedBounds = foregroundBounds(observed)

        controller = JavaFxSurfaceController.attach(
          example.model,
          example.state,
          backend.nn,
          subScene
        ).toOption.get
        val nativePick = controller.nn.pick(pickResult(example, backend.nn, referencePick)).toOption.get
        require(nativePick.surface == referencePick.surface,
          s"JavaFX picked ${nativePick.surface.value}, expected ${referencePick.surface.value}")
        require(nativePick.face.index == referencePick.face,
          s"JavaFX face ${nativePick.face.index}, expected ${referencePick.face}")
        require(nativePick.vertex.index == referencePick.vertex,
          s"JavaFX vertex ${nativePick.vertex.index}, expected ${referencePick.vertex}")
        println(
          f"fixture=bilateral-gifti size=${dimensions.width}x${dimensions.height} " +
            s"expected_foreground=${visualQa.expectedForegroundPixels} " +
            s"observed_foreground=${visualQa.observedForegroundPixels} " +
            s"expected_halves=${visualQa.expectedLeftForegroundPixels},${visualQa.expectedRightForegroundPixels} " +
            s"observed_halves=${visualQa.observedLeftForegroundPixels},${visualQa.observedRightForegroundPixels} " +
            s"expected_bounds=${expectedBounds.mkString(",")} observed_bounds=${observedBounds.mkString(",")} " +
            f"mask_iou=${visualQa.maskIntersectionOverUnion}%.6f " +
            f"centroid_distance_px=${visualQa.centroidDistancePixels}%.6f " +
            f"mean_interior_channel_error=${visualQa.meanInteriorChannelError}%.6f " +
            s"pick_surface=${nativePick.surface.value} pick_face=${nativePick.face.index} " +
            s"pick_vertex=${nativePick.vertex.index} resources=${interpreted.resourceCount}"
        )
        require(violations.isEmpty, s"JavaFX GIFTI parity failed: ${violations.mkString("; ")}")
      catch case error: Throwable => failure = error
      finally
        if controller != null then controller.nn.dispose()
        if backend != null then backend.nn.dispose()
        if stage != null then stage.nn.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def rasterImage(actual: WritableImage, dimensions: RasterDimensions): RasterImage =
    val reader = actual.getPixelReader
    RasterImage.tabulate(dimensions): (x, y) =>
      val argb = reader.getArgb(x, y)
      Rgba32.unsafe(
        (argb >>> 16) & 0xff,
        (argb >>> 8) & 0xff,
        argb & 0xff,
        (argb >>> 24) & 0xff
      )

  private def foregroundBounds(image: RasterImage): Vector[Int] =
    var minimumX = image.width
    var minimumY = image.height
    var maximumX = -1
    var maximumY = -1
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        val pixel = image.pixelUnsafe(x, y)
        if pixel.alpha > 0 && (pixel.red < 250 || pixel.green < 250 || pixel.blue < 250) then
          minimumX = math.min(minimumX, x)
          minimumY = math.min(minimumY, y)
          maximumX = math.max(maximumX, x)
          maximumY = math.max(maximumY, y)
        x += 1
      y += 1
    Vector(minimumX, minimumY, maximumX, maximumY)

  private def pickResult(
    example: SurfaceViewerExample,
    backend: JavaFxSurfaceBackend,
    reference: SurfacePick
  ): PickResult =
    val packet = example.plan.meshes.find(_.surface == reference.surface).get
    val offset = reference.face * 3
    val a = packet.indices(offset)
    val b = packet.indices(offset + 1)
    val c = packet.indices(offset + 2)
    def coordinate(axis: Int): Double =
      packet.positions(a * 3 + axis) * reference.barycentricA +
        packet.positions(b * 3 + axis) * reference.barycentricB +
        packet.positions(c * 3 + axis) * reference.barycentricC
    val chunk = backend.chunks.find: candidate =>
      candidate.surface == reference.surface &&
        reference.face >= candidate.faceStart &&
        reference.face < candidate.faceStart + candidate.faceCount
    .get
    new PickResult(
      chunk.view,
      new Point3D(coordinate(0), coordinate(1), coordinate(2)),
      reference.depth,
      reference.face - chunk.faceStart,
      Point2D.ZERO
    )
