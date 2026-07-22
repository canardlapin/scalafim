package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.application.Platform
import javafx.scene.Group
import javafx.scene.Scene
import javafx.scene.image.WritableImage
import javafx.stage.Stage

import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

object JavaFxSurfaceCorrectnessProbe:
  def main(args: Array[String]): Unit =
    val plan = fixturePlan()
    val publication = SurfacePublicationPreset.ManuscriptSingleColumn
    val dimensions = RasterDimensions.unsafe(publication.width, publication.height)
    val reference = SurfaceRasterizer.render(plan, dimensions).toOption.get.image
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      try
        val backend = JavaFxSurfaceBackend.create().toOption.get
        val interpreted = backend.render(plan).toOption.get
        require(interpreted.resourceCount == 2, s"expected mesh plus layer resources; got ${interpreted.resourceCount}")
        val snapshotConfig = JavaFxSnapshotConfig.publication(publication)
        val subScene = backend.newSubScene(snapshotConfig).toOption.get
        val stage = new Stage()
        stage.setScene(new Scene(new Group(subScene), publication.width.toDouble, publication.height.toDouble))
        stage.show()
        val actual = new WritableImage(publication.width, publication.height)
        subScene.snapshot(null, actual)
        val observed = rasterImage(actual, dimensions)
        val visualQa = SurfaceVisualQa.compare(reference, observed).toOption.get
        val violations = visualQa.violations(SurfaceVisualQaPolicy.Browser)
        val landmarkMaximumError = landmarkMaximumChannelError(reference, actual)
        println(
          f"publication_size=${publication.width}x${publication.height} " +
            f"semantic_mask_iou=${visualQa.maskIntersectionOverUnion}%.6f " +
            f"centroid_distance_px=${visualQa.centroidDistancePixels}%.6f " +
            f"mean_interior_channel_error=${visualQa.meanInteriorChannelError}%.6f " +
            f"landmark_max_channel_error=$landmarkMaximumError"
        )
        require(violations.isEmpty, s"JavaFX/reference visual QA failed: ${violations.mkString("; ")}")
        require(landmarkMaximumError <= 96, s"JavaFX/reference landmark channel error $landmarkMaximumError exceeds 96")
        backend.dispose().toOption.get
        require(backend.resourceKeys.isEmpty, "disposed JavaFX backend retained resources")
        stage.close()
      catch case error: Throwable => failure = error
      finally done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def fixturePlan(): SurfaceRenderPlan =
    val surfaceId = SurfaceId.unsafe("asymmetric-left")
    val geometry = SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-0.6, 0.8, 0.0)),
        Seq((0, 1, 2))
      ),
      Hemisphere.Left,
      SurfaceKind.Inflated
    )
    val folded = SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(-0.8, -1.0, -0.1), Seq(0.8, -1.0, 0.1), Seq(-0.5, 0.8, -0.2)),
        Seq((0, 1, 2))
      ),
      Hemisphere.Left,
      SurfaceKind.Pial
    )
    val curvature = SurfaceField.full(folded, Seq(-1.0, 0.0, 1.0), "folded curvature")
    val layer = SurfaceLayer.curvatureUnderlay(
      SurfaceLayerId.unsafe("curvature"),
      surfaceId,
      curvature,
      geometry
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, geometry).toOption.get),
      Vector(layer)
    ).toOption.get
    val dorsal = SurfaceViewer.reduce(
      model,
      SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)
    ).toOption.get
    val unlit = SurfaceViewer.reduce(model, dorsal, SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit)).toOption.get
    SurfaceCompiler.compile(model, unlit).toOption.get

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

  private def landmarkMaximumChannelError(reference: RasterImage, actual: WritableImage): Int =
    val reader = actual.getPixelReader
    val landmarks = Vector((0.14, 0.84), (0.84, 0.84), (0.27, 0.22)).map: (xFraction, yFraction) =>
      (
        math.min(reference.width - 1, math.round(xFraction * reference.width).toInt),
        math.min(reference.height - 1, math.round(yFraction * reference.height).toInt)
      )
    var maxError = 0
    landmarks.foreach: (x, y) =>
      val expected = reference.pixelUnsafe(x, y)
      val observed = reader.getArgb(x, y)
      maxError = math.max(maxError, math.abs(expected.red - ((observed >>> 16) & 0xff)))
      maxError = math.max(maxError, math.abs(expected.green - ((observed >>> 8) & 0xff)))
      maxError = math.max(maxError, math.abs(expected.blue - (observed & 0xff)))
    maxError
