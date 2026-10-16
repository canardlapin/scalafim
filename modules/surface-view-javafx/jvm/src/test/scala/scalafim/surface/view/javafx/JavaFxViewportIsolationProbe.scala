package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing, SnapshotParameters}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Production viewport clipping, depth, transparency, and native traversal acceptance. */
object JavaFxViewportIsolationProbe:
  def main(args: Array[String]): Unit =
    require(args.length <= 1)
    val output = java.nio.file.Path.of(args.headOption.getOrElse("/private/tmp/scalafim-viewport-production"))
    java.nio.file.Files.createDirectories(output)
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      try
        for bilateral <- Vector(false, true); perspective <- Vector(false, true); transparent <- Vector(false, true) do
          val width = 384
          val height = 288
          val ids = Vector(SurfaceId.unsafe("clip-left"), SurfaceId.unsafe("clip-right")).take(if bilateral then 2 else 1)
          def modelFor(rebuilt: Boolean): SurfaceViewerModel =
            val geometries = ids.indices.map: index =>
              val vertices = for z <- Vector(0.2, -0.2); (x, y) <- Vector((-3.0,-3.0),(3.0,-3.0),(3.0,3.0),(-3.0,3.0)) yield Seq(x, y, z)
              SurfaceGeometry(TriangleMesh.fromRows(vertices, Seq((0,1,2),(0,2,3),(4,5,6),(4,6,7)) ++ (if rebuilt then Seq((4,6,7)) else Seq.empty)),
                if index == 0 then Hemisphere.Left else Hemisphere.Right, SurfaceKind.Pial)
            val assets = ids.zip(geometries).map((id, g) => SurfaceAsset.make(id, g).toOption.get)
            val layers = ids.zip(geometries).zipWithIndex.map: (entry, index) =>
              val (id, g) = entry
              val front = if index == 0 then Rgba32.unsafe(220,30,50) else Rgba32.unsafe(20,190,80)
              SurfaceLayer.facePackedRgba(SurfaceLayerId.unsafe(s"color-$index"), id,
                SurfaceFaceField.make(g, Array(front, front) ++ Array.fill(if rebuilt then 3 else 2)(Rgba32.unsafe(30,50,220))).toOption.get)
            SurfaceViewerModel.make(assets, layers).toOption.get
          var model = modelFor(false)
          var state = SurfaceFaceFixture.state(model)
          if bilateral then state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(ids(0), ids(1)))).toOption.get
          if perspective then
            state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.Default))).toOption.get
            state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetZoom(CameraZoom.unsafe(4.0))).toOption.get
          var plan = SurfaceCompiler.compile(model, state).toOption.get
          val backend = JavaFxSurfaceBackend.create().toOption.get
          try
            backend.render(plan).toOption.get
            val config = JavaFxSnapshotConfig.make(width, height, SceneAntialiasing.DISABLED, transparent).toOption.get
            val unmountedSnapshot = backend.snapshot(config).toOption.get
            require(unmountedSnapshot.getWidth == width && unmountedSnapshot.getHeight == height)
            val outer = backend.newSubScene(config).toOption.get
            require(backend.newSubScene(config).isLeft, "multiple mounts must be rejected explicitly")
            val parameters = new SnapshotParameters()
            parameters.setFill(if transparent then Color.TRANSPARENT else Color.WHITE)
            stage.setScene(new Scene(new Group(outer), width, height))
            stage.show()
            def verify(step: String): Unit =
              val width = outer.getWidth.toInt
              val height = outer.getHeight.toInt
              val image = outer.snapshot(parameters, new WritableImage(width, height))
              val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(width, height),
                SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
              if step == "initial" || step == "layout-resized" then
                val actualPng = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_ARGB)
                val expectedPng = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_ARGB)
                for py <- 0 until height; px <- 0 until width do
                  actualPng.setRGB(px, py, image.getPixelReader.getArgb(px, py))
                  val c = reference.image.pixelUnsafe(px, py)
                  val argb = if transparent && reference.pick(px, py).toOption.flatten.isEmpty then 0
                    else (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue
                  expectedPng.setRGB(px, py, argb)
                val name = s"$step-bilateral-$bilateral-perspective-$perspective-transparent-$transparent"
                javax.imageio.ImageIO.write(actualPng, "png", output.resolve(s"$name-javafx.png").toFile)
                javax.imageio.ImageIO.write(expectedPng, "png", output.resolve(s"$name-reference.png").toFile)
              var hits = 0
              var misses = 0
              for y <- 4 until height - 4 by 7; x <- 4 until width - 4 by 7 do
                val expected = reference.pick(x, y).toOption.flatten
                val native = JavaFxNativePick.at(outer, x + 0.5, y + 0.5)
                val pixel = image.getPixelReader.getArgb(x, y)
                expected match
                  case None =>
                    require(native.isEmpty, s"native pick leaked outside slot at $x,$y")
                    require(if transparent then pixel == 0 else pixel == -1, s"pixel leaked outside slot at $x,$y: $pixel")
                    misses += 1
                  case Some(p) =>
                    require(native.nonEmpty, s"missing nested native pick at $x,$y")
                    val node = native.get.getIntersectedNode
                    val chunk = backend.chunks.find(_.view == node).get
                    require(chunk.surface == p.surface, "pick crossed viewport ownership")
                    val nativeFace = chunk.faceStart + native.get.getIntersectedFace
                    require(nativeFace < 2, "native pick selected the farther surface")
                    if math.min(p.barycentricA, math.min(p.barycentricB, p.barycentricC)) > 1e-4 then
                      require(nativeFace == p.face, s"native interior face mismatch at $x,$y: $nativeFace vs ${p.face}")
                    val c = reference.image.pixelUnsafe(x, y)
                    val error = Vector(math.abs(((pixel >>> 16) & 255) - c.red), math.abs(((pixel >>> 8) & 255) - c.green), math.abs((pixel & 255) - c.blue)).max
                    require(error <= 1, s"depth/color mismatch at $x,$y: $error")
                    require(p.face < 2, "fixture did not expose near faces")
                    hits += 1
              require(hits > 100)
              if step == "initial" then require(misses > 100)
              println(s"viewport_production step=$step bilateral=$bilateral perspective=$perspective transparent=$transparent hits=$hits misses=$misses")

            verify("initial")
            val sameSizeSnapshot = backend.snapshot(config).toOption.get
            require(sameSizeSnapshot.getWidth == width && sameSizeSnapshot.getHeight == height)
            verify("snapshot-restored")
            val alternate = JavaFxSnapshotConfig.make(507, 211, SceneAntialiasing.BALANCED, !transparent).toOption.get
            val resizedSnapshot = backend.snapshot(alternate).toOption.get
            require(resizedSnapshot.getWidth == 507 && resizedSnapshot.getHeight == 211)
            val snapshotReference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(507, 211),
              SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
            val slot = plan.viewportFit.resolve(plan.slots, 507, 211).head.viewport
            val sx = ((slot.x + slot.width * 0.5) * 507).toInt
            val sy = ((slot.y + slot.height * 0.5) * 211).toInt
            val expected = snapshotReference.image.pixelUnsafe(sx, sy)
            val actual = resizedSnapshot.getPixelReader.getArgb(sx, sy)
            require(math.abs(((actual >>> 16) & 255) - expected.red) <= 1 &&
              math.abs(((actual >>> 8) & 255) - expected.green) <= 1 && math.abs((actual & 255) - expected.blue) <= 1)
            require(resizedSnapshot.getPixelReader.getArgb(0, 0) == (if transparent then -1 else 0))
            verify("resampled-snapshot-restored")
            val retainedMeshes = backend.chunks.map(_.mesh)
            val retainedAtlases = backend.chunks.map(_.atlas)
            outer.setWidth(511)
            outer.setHeight(257)
            verify("resized")
            val changedProjection = if perspective then CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))
              else CameraProjection.Perspective(FieldOfViewDegrees.Default)
            state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetProjection(changedProjection)).toOption.get
            state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetZoom(CameraZoom.unsafe(4.0))).toOption.get
            plan = SurfaceCompiler.compile(model, state).toOption.get
            val cameraReceipt = backend.render(plan).toOption.get
            require(!cameraReceipt.dirty.geometry && cameraReceipt.atlasUpdates == 0 && cameraReceipt.geometryBytesUpdated == 0)
            verify("projection")
            state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetPan(0.1, -0.1)).toOption.get
            plan = SurfaceCompiler.compile(model, state).toOption.get
            backend.render(plan).toOption.get
            verify("pan")
            require(backend.chunks.map(_.mesh) == retainedMeshes && backend.chunks.map(_.atlas) == retainedAtlases)
            model = modelFor(true)
            plan = SurfaceCompiler.compile(model, state).toOption.get
            val rebuild = backend.render(plan).toOption.get
            require(rebuild.dirty.geometry && backend.chunks.map(_.mesh) != retainedMeshes)
            verify("rebuilt")
            outer.setWidth(300)
            outer.setHeight(420)
            if bilateral then
              state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetLayout(SurfaceLayout.Single(ids(1)))).toOption.get
              plan = SurfaceCompiler.compile(model, state).toOption.get
              backend.render(plan).toOption.get
            verify("layout-resized")
            if bilateral then
              state = SurfaceViewer.reduce(model, state, SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(ids(0), ids(1)))).toOption.get
              plan = SurfaceCompiler.compile(model, state).toOption.get
              backend.render(plan).toOption.get
              verify("bilateral-restored")
            backend.dispose().toOption.get
            require(backend.viewportCameras.isEmpty && backend.chunks.isEmpty && outer.getCamera == null)
            outer.setWidth(17)
            outer.setHeight(23)
          finally backend.dispose()
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(120, TimeUnit.SECONDS), "viewport isolation probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()
