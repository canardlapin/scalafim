package scalafim.examples.surfaceview

import javafx.application.Application
import javafx.scene.{Group, Scene}
import javafx.stage.Stage

import scalafim.surface.Hemisphere
import scalafim.surface.SurfaceKind
import scalafim.surface.io.{GiftiReader, GiftiSurfaceReader}
import scalafim.surface.view.javafx.*

final class JavaFxSurfaceViewerExampleApp extends Application:
  override def start(stage: Stage): Unit =
    val example = SurfaceViewerExample.fromGifti(JavaFxSurfaceViewerExample.readGifti())
      .fold(error => throw new IllegalStateException(error.message), identity)
    val backend = JavaFxSurfaceBackend.create()
      .fold(error => throw new IllegalStateException(error.message), identity)
    val render = backend.render(example.plan)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val snapshotConfig = JavaFxSnapshotConfig.make(960, 540)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val surfaceScene = backend.newSubScene(snapshotConfig)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val controller = JavaFxSurfaceController.attach(example.model, example.state, backend, surfaceScene)
      .fold(error => throw new IllegalStateException(error.message), identity)
    stage.setTitle("ScalaFIM surface viewer — JavaFX")
    stage.setScene(new Scene(new Group(surfaceScene), 960.0, 540.0))
    stage.setOnHidden(_ =>
      controller.dispose()
      backend.dispose()
    )
    stage.show()
    val snapshotStarted = System.nanoTime()
    backend.snapshot(snapshotConfig).fold(error => throw new IllegalStateException(error.message), identity)
    val snapshotMillis = (System.nanoTime() - snapshotStarted).toDouble / 1e6
    val semantic = SurfaceViewerExample.semanticReceipt(example)
    println(
      s"schema=${semantic.schema} backend=javafx vertices=${semantic.vertices} faces=${semantic.faces} " +
        s"meshResources=${example.plan.receipt.meshKeys.length} layerResources=${example.plan.receipt.layerKeys.length} " +
        f"renderMillis=${render.elapsedNanos.toDouble / 1e6}%.3f snapshotMillis=$snapshotMillis%.3f"
    )

object JavaFxSurfaceViewerExample:
  def readGifti(): scalafim.surface.SurfaceGeometry =
    val stream = Option(getClass.getResourceAsStream("/surface/tetra_lh_midthickness.surf.gii"))
      .getOrElse(throw new IllegalStateException("missing GIFTI example resource"))
    val xml =
      try new String(stream.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8)
      finally stream.close()
    val document = GiftiReader.parseString(xml)
      .fold(error => throw new IllegalStateException(error.message), identity)
    GiftiSurfaceReader.geometry(document, Hemisphere.Left, SurfaceKind.Midthickness)
      .fold(error => throw new IllegalStateException(error.message), identity)

  def main(args: Array[String]): Unit =
    Application.launch(classOf[JavaFxSurfaceViewerExampleApp], args*)
