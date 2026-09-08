package scalafim.surface.view.javafx

import java.util.concurrent.CountDownLatch
import javafx.application.Platform
import javafx.geometry.{Point2D, Point3D}
import javafx.scene.{Group, ParallelCamera, PerspectiveCamera, Scene}
import javafx.scene.input.PickResult
import javafx.stage.Stage

import javafx.scene.image.WritableImage

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

object JavaFxSurfaceInteractionProbe:
  def main(args: Array[String]): Unit =
    val (model, initial) = fixture()
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      try
        val initialPlan = SurfaceCompiler.compile(model, initial).toOption.get
        val reference = SurfaceRasterizer.render(initialPlan, RasterDimensions.unsafe(128, 128)).toOption.get
        val backend = JavaFxSurfaceBackend.create().toOption.get
        backend.render(initialPlan).toOption.get
        val scene = backend.newSubScene(JavaFxSnapshotConfig.make(128, 128).toOption.get).toOption.get
        val stage = new Stage()
        stage.setScene(new Scene(new Group(scene), 128.0, 128.0))
        stage.show()
        val controller = JavaFxSurfaceController.attach(model, initial, backend, scene).toOption.get
        val chunk = backend.chunks.head
        stage.getScene.getRoot.applyCss()
        stage.getScene.getRoot.layout()
        // Resolve nested camera peers through a real render before querying screen coordinates.
        scene.snapshot(null, new WritableImage(128, 128))
        // An interior barycentric point (0.8,0.1,0.1), projected by JavaFX itself.
        val interior = new Point3D(-0.76, -0.82, 0.0)
        val projected = chunk.view.localToScreen(interior)
        val origin = scene.localToScreen(0.0, 0.0)
        require(projected != null && origin != null)
        println(s"interaction_projection projected=$projected origin=$origin")
        val referencePick = reference.pick(math.floor(projected.getX-origin.getX).toInt,
          math.floor(projected.getY-origin.getY).toInt).toOption.flatten.get
        val javafxPick = new PickResult(
          chunk.view,
          interior,
          4.0,
          0,
          Point2D.ZERO
        )
        val picked = controller.pick(javafxPick).toOption.get
        require(picked.face.index == referencePick.face, s"face mismatch ${picked.face.index} != ${referencePick.face}")
        require(picked.vertex.index == referencePick.vertex, s"vertex mismatch ${picked.vertex.index} != ${referencePick.vertex}")
        require(backend.viewportCameras.nonEmpty && backend.viewportCameras.forall(_.isInstanceOf[PerspectiveCamera]), "initial perspective plan did not use PerspectiveCamera")
        controller.dispatch(SurfaceViewerAction.SetProjection(
          CameraProjection.Orthographic(OrthographicScale.unsafe(0.75))
        )).toOption.get
        require(backend.viewportCameras.nonEmpty && backend.viewportCameras.forall(_.isInstanceOf[ParallelCamera]), "mounted SubScene did not switch to ParallelCamera")
        controller.dispatch(SurfaceViewerAction.SetProjection(
          CameraProjection.Perspective(FieldOfViewDegrees.Default)
        )).toOption.get
        require(backend.viewportCameras.nonEmpty && backend.viewportCameras.forall(_.isInstanceOf[PerspectiveCamera]), "mounted SubScene did not return to PerspectiveCamera")
        var index = 0
        while index < 60 do
          controller.dispatch(SurfaceViewerAction.OrbitBy(0.25, 0.1)).toOption.get
          index += 1
        controller.dispatch(SurfaceViewerAction.ResetCamera).toOption.get
        val beforeDispose = controller.receipt
        require(beforeDispose.meshUploads == 1, s"camera interaction uploaded ${beforeDispose.meshUploads} meshes")
        require(beforeDispose.atlasUploads == 1, s"camera interaction uploaded ${beforeDispose.atlasUploads} atlases")
        controller.dispose()
        require(controller.receipt.disposed, "controller did not report disposal")
        println(f"pick_face=${picked.face.index} pick_vertex=${picked.vertex.index} navigation_p50_ms=${beforeDispose.navigationP50Millis}%.6f navigation_p95_ms=${beforeDispose.navigationP95Millis}%.6f pick_p50_ms=${beforeDispose.pickP50Millis}%.6f pick_p95_ms=${beforeDispose.pickP95Millis}%.6f mesh_uploads=${beforeDispose.meshUploads} atlas_uploads=${beforeDispose.atlasUploads}")
        stage.close()
      catch case error: Throwable => failure = error
      finally done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def fixture(): (SurfaceViewerModel, SurfaceViewerState) =
    val surfaceId = SurfaceId.unsafe("left")
    val geometry = SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-0.6, 0.8, 0.0)),
        Seq((0, 1, 2))
      ),
      Hemisphere.Left,
      SurfaceKind.Inflated
    )
    val layer = SurfaceLayer.packedRgba(
      SurfaceLayerId.unsafe("landmarks"),
      surfaceId,
      geometry,
      Vector(Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(0, 255, 0), Rgba32.unsafe(0, 0, 255))
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(surfaceId, geometry).toOption.get),
      Vector(layer)
    ).toOption.get
    val initial = SurfaceViewer.reduce(
      model,
      SurfaceViewerState.initial(model),
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)
    ).toOption.get
    (model, initial)
