package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Scene, SceneAntialiasing}
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.stage.Stage
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Actual native depth, panel isolation, scientific picks, and retained resources. */
object JavaFxSurfaceCompositionProbe:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => throw new IllegalStateException(s"$e"), identity)
  private val red = Rgba32.unsafe(255, 0, 0)
  private val blue = Rgba32.unsafe(0, 0, 255)
  private def quad(name: String, slope: Double, color: Rgba32, perspective: Boolean = false): SurfaceRenderPlan =
    val id = SurfaceId.unsafe(name)
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, -slope), Seq(1.0, -1.0, slope), Seq(-1.0, 1.0, -slope), Seq(1.0, 1.0, slope)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val layer = ok(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(s"$name-color"), id, geometry, Vector.fill(4)(color)))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, geometry))), Vector(layer)))
    val state = SurfaceFaceFixture.state(model)
    ok(SurfaceCompiler.compile(model, if perspective then state.copy(camera = state.camera.copy(
      projection = CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35)))) else state))

  def main(args: Array[String]): Unit =
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-composition"))
    Files.createDirectories(output)
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      var backend: Option[JavaFxSurfaceComposition] = None
      try
        val native = ok(JavaFxSurfaceComposition.create())
        backend = Some(native)
        val a = quad("red", .4, red)
        val b = quad("blue", -.4, blue)
        val resources = SurfaceCompositionResources((a.meshes ++ b.meshes).map(m => m.resourceKey -> m).toMap,
          (a.layers ++ b.layers).map(l => l.resourceKey -> l).toMap)
        def instance(id: String, p: SurfaceRenderPlan) = SurfaceCompositionInstance(ok(SurfaceInstanceId.make(id)),
          p.meshes.head.surface, p.meshes.head.resourceKey, p.layers.map(_.resourceKey))
        val one = SurfaceCompositionView(ok(SurfaceViewId.make("crossing")), SurfaceViewport(0, 0, 1, 1), a.camera,
          Vector(instance("red-copy", a), instance("blue-copy", b)), SurfaceLighting.Unlit, SurfaceClipping.Disabled)
        var plan = ok(SurfaceCompositionPlan.make(Vector(one), resources))
        var dimensions = ok(JavaFxSnapshotConfig.make(320, 240, SceneAntialiasing.DISABLED))
        ok(native.render(plan, dimensions))
        stage.setScene(new Scene(native.root, dimensions.width, dimensions.height, Color.WHITE))
        stage.show()
        var checks = Vector.empty[String]
        var totalPicks = 0
        def verify(name: String): Array[Int] =
          val width = dimensions.width
          val height = dimensions.height
          val snapshot = ok(native.snapshot())
          val raster = ok(SurfaceRasterizer.render(plan, RasterDimensions.unsafe(width, height),
            SurfaceRasterStyle(culling = TriangleCulling.None)))
          val actual = Array.tabulate(width * height)(i => snapshot.getPixelReader.getArgb(i % width, i / width))
          val expected = Array.tabulate(width * height): i =>
            val c = raster.image.pixelUnsafe(i % width, i / width)
            (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue
          write(actual, width, height, output.resolve(s"$name-javafx.png"))
          write(expected, width, height, output.resolve(s"$name-raster.png"))
          val boxes = plan.fittedViewports(width, height).map(_._2)
          var picks = 0
          var empty = 0
          for y <- 5 until height - 5 by 11; x <- 5 until width - 5 by 11 do
            val point = ok(raster.pick(x, y))
            val viewIndex = boxes.indexWhere(box => x + .5 >= box.x * width && x + .5 < (box.x + box.width) * width &&
              y + .5 >= box.y * height && y + .5 < (box.y + box.height) * height)
            val hit = if viewIndex < 0 then None else
              val box = boxes(viewIndex)
              JavaFxNativePick.at(native.subScenes(plan.views(viewIndex).id), x + .5 - box.x * width, y + .5 - box.y * height)
                .filter(_.getIntersectedFace >= 0).map(p => ok(native.pick(p)))
            point match
              case Some(p) if math.min(p.barycentricA, math.min(p.barycentricB, p.barycentricC)) > .02 =>
                val h = hit.getOrElse(throw new IllegalStateException(s"$name missing pick at $x,$y"))
                require(h.origin == p.origin.get && h.scientific.surface == p.surface && h.scientific.face.index == p.face,
                  s"$name native identity mismatch at $x,$y: $h vs $p")
                val mesh = if p.surface == a.meshes.head.surface then a.meshes.head else b.meshes.head
                val weights = Vector(p.barycentricA, p.barycentricB, p.barycentricC)
                val xyz = Vector.tabulate(3)(axis => (0 until 3).map(corner =>
                  weights(corner) * mesh.positions(mesh.indices(p.face * 3 + corner) * 3 + axis)).sum)
                require(Vector(h.worldX, h.worldY, h.worldZ).zip(xyz).forall((left, right) => math.abs(left - right) < 1e-5),
                  s"$name scientific coordinates changed by display translation")
                val error = Vector(0, 8, 16).map(shift => math.abs(((actual(y * width + x) >>> shift) & 255) -
                  ((expected(y * width + x) >>> shift) & 255))).max
                require(error <= 1, s"$name native depth/color mismatch at $x,$y: $error")
                picks += 1
              case None =>
                require(hit.isEmpty, s"$name pick outside surface at $x,$y")
                require(actual(y * width + x) == -1, s"$name pixels outside surface at $x,$y")
                empty += 1
              case _ => ()
          require(picks > 30, s"$name too few interior picks: $picks")
          totalPicks += picks
          checks :+= s"{\"case\":\"$name\",\"interiorPicks\":$picks,\"emptySamples\":$empty}"
          actual
        val crossing = verify("crossing")
        require(crossing(120 * 320 + 80) == 0xff0000ff && crossing(120 * 320 + 240) == 0xffff0000,
          "independent crossing-depth oracle failed")
        plan = ok(SurfaceCompositionPlan.make(Vector(one.copy(instances = one.instances.reverse)), resources))
        ok(native.render(plan, dimensions))
        require(verify("reversed").sameElements(crossing), "submission order changed shared-depth image")
        val panels = Vector(a, b, a, b).zipWithIndex.map: (source, i) =>
          one.copy(id = ok(SurfaceViewId.make(s"view-$i")), viewport = SurfaceViewport((i % 2) * .5, (i / 2) * .5, .5, .5),
            instances = Vector(instance(s"panel-$i", source)))
        plan = ok(SurfaceCompositionPlan.make(panels, resources))
        val firstPanels = ok(native.render(plan, dimensions))
        require(firstPanels.meshViews == 4 && native.resourceKeys == (resources.meshes.keySet ++ resources.layers.keySet))
        val before = verify("panels")
        val oldMeshes = native.subScenes.values.toVector.flatMap(scene =>
          scene.getRoot.lookupAll("MeshView").toArray.toVector.collect { case node: _root_.javafx.scene.shape.MeshView => node.getMesh })
        val matrix = Array.tabulate(16)(a.camera.viewMatrix(_))
        matrix(3) += .5f
        plan = ok(SurfaceCompositionPlan.make(panels.updated(3, panels(3).copy(camera = a.camera.copy(viewMatrix = new FloatBufferView(matrix)))), resources))
        val moved = ok(native.render(plan, dimensions))
        require(moved.native.atlasUpdates == 0 && moved.native.geometryUpdates == 0 && moved.native.geometryBytesUpdated == 0)
        val after = verify("independent-camera")
        var changes = 0
        for y <- 0 until dimensions.height; x <- 0 until dimensions.width do
          if x < dimensions.width / 2 || y < dimensions.height / 2 then
            require(after(y * dimensions.width + x) == before(y * dimensions.width + x), "camera update changed another panel")
          else if after(y * dimensions.width + x) != before(y * dimensions.width + x) then changes += 1
        require(changes > 0)
        val newMeshes = native.subScenes.values.toVector.flatMap(scene =>
          scene.getRoot.lookupAll("MeshView").toArray.toVector.collect { case node: _root_.javafx.scene.shape.MeshView => node.getMesh })
        require(oldMeshes.size == 4 && oldMeshes.toSet == newMeshes.toSet && newMeshes.toSet.size == 2, "native meshes were not shared/retained")
        val packed = panels.take(2).map(_.copy(viewportFit = SurfaceViewportFit.Pack(1.0)))
        plan = ok(SurfaceCompositionPlan.make(packed, resources))
        dimensions = ok(JavaFxSnapshotConfig.make(320, 640, SceneAntialiasing.DISABLED))
        val packedBoxes = plan.fittedViewports(dimensions.width, dimensions.height).map(_._2)
        require(packedBoxes.map(box => (box.x, box.y, box.width, box.height)) == Vector((0.0, 0.0, 1.0, 0.5), (0.0, 0.5, 1.0, 0.5)),
          "portrait composition did not pack views vertically")
        ok(native.render(plan, dimensions))
        verify("portrait-packed")
        dimensions = ok(JavaFxSnapshotConfig.make(507, 211, SceneAntialiasing.DISABLED))
        ok(native.resize(dimensions))
        verify("resized")
        val translated = one.instances.map(_.copy(translationX = .2, translationY = -.1))
        plan = ok(SurfaceCompositionPlan.make(Vector(one.copy(instances = translated)), resources))
        ok(native.render(plan, dimensions))
        verify("translated")
        val perspectiveCamera = quad("red", .4, red, perspective = true).camera
        plan = ok(SurfaceCompositionPlan.make(Vector(one.copy(camera = perspectiveCamera, instances = translated)), resources))
        ok(native.render(plan, dimensions))
        val perspective = verify("perspective")
        val layer = resources.layers.values.head
        val corrupt = layer.copy(colors = new IntBufferView(Array.fill(layer.colors.length)(0x000000ff)))
        val rejected = ok(SurfaceCompositionPlan.make(plan.views,
          resources.copy(layers = resources.layers.updated(layer.resourceKey, corrupt))))
        require(native.render(rejected, dimensions).isLeft, "conflicting immutable resource was silently accepted")
        require(verify("after-rejected-resource").sameElements(perspective), "rejected update changed native image")
        Files.writeString(output.resolve("composition-metrics.json"),
          s"{\"passed\":true,\"nativePicks\":$totalPicks,\"cameraOnlyUploads\":0,\"checks\":[${checks.mkString(",")}]}\n")
        println(s"PASS native composition: $totalPicks original-identity picks; shared depth, panels, camera isolation, resize, translation, retained meshes")
      catch case error: Throwable => failure = error
      finally
        backend.foreach(b => ok(b.dispose()))
        stage.close()
        done.countDown()
    require(done.await(90, TimeUnit.SECONDS), "native composition probe timed out")
    Platform.exit()
    if failure != null then throw failure.nn

  private def write(pixels: Array[Int], width: Int, height: Int, path: Path): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, width, height, pixels, 0, width)
    require(ImageIO.write(image, "png", path.toFile))
