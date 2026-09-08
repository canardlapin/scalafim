package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing, SubScene}
import javafx.scene.image.WritableImage
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.{SurfaceGeometry, SurfaceKind, SurfaceSet, TriangleMesh, SurfaceFaceField}
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Native ray intersections compared with independently rasterized original faces. */
object JavaFxNativePickProbe:
  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      val rows = Vector.newBuilder[String]
      try
        for mode <- Vector("face", "large-face", "nearest", "scalar"); perspective <- Vector(false, true); size <- Vector(96, 192) do
          val model = mode match
            case "face" => SurfaceFaceFixture.model
            case "large-face" =>
              val original = SurfaceFaceFixture.geometry
              val large = SurfaceGeometry(TriangleMesh.fromArrays(original.mesh.coordinates.map(_ * 100), original.mesh.faceIndices),
                original.hemisphere, original.kind, original.surfaceToWorld)
              val layer = SurfaceLayer.facePackedRgba(SurfaceFaceFixture.Layer, SurfaceFaceFixture.Surface,
                SurfaceFaceField.make(large, Array(SurfaceFaceFixture.Red, SurfaceFaceFixture.Blue)).toOption.get)
              SurfaceViewerModel.make(Vector(SurfaceAsset.make(SurfaceFaceFixture.Surface, large).toOption.get), Vector(layer)).toOption.get
            case "nearest" => SurfaceNearestFixture.model
            case _ => SurfaceScalarFixture.model(SurfaceScalarFixture.thresholded)
          val initial = SurfaceFaceFixture.state(model)
          val viewed = if !perspective then initial else
            val projected = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
              CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
            SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
          val state = if mode == "large-face" then SurfaceViewer.reduce(model, viewed, SurfaceViewerAction.FitCamera).toOption.get else viewed
          val plan = SurfaceCompiler.compile(model, state).toOption.get
          val backend = (if mode == "scalar" then
            JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make().toOption.get,
              JavaFxAtlasConfig.make(maxTextureSize = 64).toOption.get)
          else JavaFxSurfaceBackend.create()).toOption.get
          try
            backend.render(plan).fold(e => throw new IllegalArgumentException(e.message), identity)
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(size, size,
              SceneAntialiasing.DISABLED).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(scene), size, size))
            stage.show()
            scene.snapshot(null, new WritableImage(size, size))
            val controller = JavaFxSurfaceController.attach(model, state, backend, scene).toOption.get
            try
              val metrics = verify(model, backend, controller, scene, size, size)
              require(metrics.faces == 2)
              if mode == "scalar" then require(metrics.chunks > 1, "scalar picks did not span atlas chunks")
              val row = s"{\"mode\":\"$mode\",\"perspective\":$perspective,\"size\":$size,${metrics.json}}"
              rows += row
              println(s"native_pick=$row")
            finally controller.dispose()
          finally backend.dispose()
        rows ++= verifyUpdates(stage)
        val path = java.nio.file.Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-native-picks.json"))
        java.nio.file.Files.writeString(path, rows.result().mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(180, TimeUnit.SECONDS), "native picking probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()


  private case class Metrics(checked: Int, misses: Int, clippedMisses: Int, faces: Int, chunks: Int, maximumError: Double):
    def json: String = s"\"checked\":$checked,\"misses\":$misses,\"clippedMisses\":$clippedMisses,\"faces\":$faces,\"chunks\":$chunks,\"maximumBarycentricError\":$maximumError"

  private def verify(model: SurfaceViewerModel, backend: JavaFxSurfaceBackend, controller: JavaFxSurfaceController,
      scene: SubScene, width: Int, height: Int): Metrics =
    scene.snapshot(null, new WritableImage(width, height))
    val reference = SurfaceRasterizer.render(controller.plan, RasterDimensions.unsafe(width, height),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    val unclipped = if controller.state.clipping == SurfaceClipping.Disabled then None else
      Some(SurfaceRasterizer.render(SurfaceCompiler.compile(model,
        controller.state.copy(clipping = SurfaceClipping.Disabled)).toOption.get,
        RasterDimensions.unsafe(width, height), SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get)
    var clippedMisses = 0
    var checked = 0
    var misses = 0
    var maximumError = 0.0
    val faces = scala.collection.mutable.Set.empty[Int]
    val chunks = scala.collection.mutable.Set.empty[Int]
    for y <- 2 until height - 2 by 3; x <- 2 until width - 2 by 3 do
      val neighborhood = for dy <- -1 to 1; dx <- -1 to 1 yield reference.pick(x + dx, y + dy).toOption.flatten
      reference.pick(x, y).toOption.flatten match
        case None if neighborhood.forall(_.isEmpty) =>
          require(JavaFxNativePick.at(scene, x + 0.5, y + 0.5).isEmpty, s"native hit outside reference at $x,$y")
          misses += 1
          if unclipped.exists(_.pick(x, y).toOption.flatten.exists(p =>
              math.min(p.barycentricA, math.min(p.barycentricB, p.barycentricC)) > 0.03)) then clippedMisses += 1
        case Some(expected) =>
          val weights = Vector(expected.barycentricA, expected.barycentricB, expected.barycentricC)
          val sorted = weights.sorted
          if weights.min > 0.03 && sorted(2) - sorted(1) > 0.02 && neighborhood.forall(_.exists(_.face == expected.face)) then
            val result = JavaFxNativePick.at(scene, x + 0.5, y + 0.5)
              .getOrElse(throw new AssertionError(s"native miss at $x,$y"))
            val actual = controller.pick(result).fold(e => throw new AssertionError(e.message), identity)
            require(actual.surface == expected.surface && actual.face.index == expected.face && actual.vertex.index == expected.vertex,
              s"native ID mismatch at $x,$y: $actual versus $expected")
            val error = Vector(math.abs(actual.barycentricA - expected.barycentricA),
              math.abs(actual.barycentricB - expected.barycentricB), math.abs(actual.barycentricC - expected.barycentricC)).max
            maximumError = math.max(maximumError, error)
            require(error <= 1e-5, s"native barycentric error $error at $x,$y")
            faces += actual.face.index
            chunks += backend.chunks.indexWhere(_.view eq result.getIntersectedNode)
            checked += 1
        case _ => ()
    require(checked > 50 && faces.nonEmpty && misses > 20, s"insufficient pick coverage: $checked/$faces/$misses")
    require(JavaFxNativePick.at(scene, -1, -1).isEmpty)
    require(unclipped.isEmpty || clippedMisses > 50, s"insufficient clipped native misses: $clippedMisses")
    Metrics(checked, misses, clippedMisses, faces.size, chunks.size, maximumError)

  private def verifyUpdates(stage: Stage): Vector[String] =
    val geometry = SurfaceFaceFixture.geometry
    val white = SurfaceGeometry(geometry.mesh, geometry.hemisphere, SurfaceKind.White, geometry.surfaceToWorld)
    val pial = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.1), Seq(1.0, -1.0, 0.4), Seq(1.0, 1.0, 0.2), Seq(-1.0, 1.0, 0.1)),
      Seq((0, 1, 2), (0, 2, 3))), white.hemisphere, SurfaceKind.Pial)
    val asset = SurfaceAsset.make(SurfaceScalarFixture.Surface,
      SurfaceSet.of(SurfaceKind.White, white, SurfaceKind.Pial -> pial)).toOption.get
    val field = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface, white,
      Array(-2.0, 4.0, 4.0, -2.0, -1.0, 3.0, 3.0, -1.0), SurfaceScalarFixture.thresholded, frameCount = 2).toOption.get
    val model = SurfaceViewerModel.make(Vector(asset), Vector(field)).toOption.get
    val rows = Vector.newBuilder[String]
    for perspective <- Vector(false, true) do
      val initial = SurfaceFaceFixture.state(model)
      val projected = if !perspective then initial else SurfaceViewer.reduce(model, initial,
        SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
      val state = SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
      val backend = JavaFxSurfaceBackend.createApproximate(JavaFxApproximationConfig.make(maxChannelError = 4).toOption.get,
        JavaFxAtlasConfig.make(maxTextureSize = 64).toOption.get).toOption.get
      try
        backend.render(SurfaceCompiler.compile(model, state).toOption.get).toOption.get
        val scene = backend.newSubScene(JavaFxSnapshotConfig.make(192, 192, SceneAntialiasing.DISABLED).toOption.get).toOption.get
        stage.setScene(new Scene(new Group(scene), 192, 192))
        stage.show()
        val controller = JavaFxSurfaceController.attach(model, state, backend, scene).toOption.get
        try
          val middle = -controller.plan.camera.viewMatrix(11).toDouble
          val updates = Vector(
            "timepoint" -> Vector(SurfaceViewerAction.SetTimepoint(1)),
            "threshold" -> Vector(SurfaceViewerAction.SetLayerThreshold(SurfaceScalarFixture.Layer,
              DisplayThreshold.transparentBand(-0.5, 0.5).toOption.get)),
            "morph" -> Vector(SurfaceViewerAction.BeginGeometryMorph(SurfaceScalarFixture.Surface, SurfaceKind.Pial),
              SurfaceViewerAction.SetGeometryMorphFraction(SurfaceScalarFixture.Surface, SurfaceMorphFraction.unsafe(0.5))),
            "near" -> Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.NearFar(middle, 1000))),
            "far" -> Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.NearFar(0.01, middle))),
            "restored" -> Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.Disabled)))
          for (name, actions) <- updates do
            actions.foreach(action => controller.dispatch(action).fold(e => throw new AssertionError(e.message), identity))
            val width = if name == "restored" then 256 else 192
            scene.setWidth(width)
            stage.setWidth(width + 40)
            val metrics = verify(model, backend, controller, scene, width, 192)
            require(metrics.chunks > 1)
            val row = s"{\"mode\":\"scalar-update\",\"update\":\"$name\",\"perspective\":$perspective,\"width\":$width,\"height\":192,${metrics.json}}"
            rows += row
            println(s"native_pick=$row")
        finally controller.dispose()
      finally backend.dispose()
    rows.result()
