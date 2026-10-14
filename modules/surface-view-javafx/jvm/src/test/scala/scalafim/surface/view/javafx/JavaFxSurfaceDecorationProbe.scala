package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Scene, SceneAntialiasing}
import javafx.scene.paint.Color
import javafx.stage.Stage
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

object JavaFxSurfaceDecorationProbe:
  private def ok[A](value: Either[?, A]): A = value.fold(e => throw new IllegalStateException(s"$e"), identity)
  private val red = Rgba32.unsafe(255, 0, 0)
  private val blue = Rgba32.unsafe(0, 0, 255)
  private val surface = SurfaceId.unsafe("anchor")
  private val anchor = SurfaceDecorationAnchor.Face(ok(SurfaceFaceAnchor.make(surface, FaceId(0), 0, .5, .5)))
  private def source(id: SurfaceId, x0: Double, z: Double): SurfaceRenderPlan =
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(x0, -1.0, z), Seq(1.0, -1.0, z), Seq(x0, 1.0, z), Seq(1.0, 1.0, z)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val layer = ok(SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(id.value + "-color"), id, geometry, Vector.fill(4)(blue)))
    val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, geometry))), Vector(layer)))
    val state = SurfaceViewerState.initial(model).copy(camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal,
      CameraProjection.Orthographic(OrthographicScale.unsafe(1.25))), lighting = SurfaceLighting.Unlit)
    ok(SurfaceCompiler.compile(model, state))


  private def networkGeometry(plan: SurfaceCompositionPlan, width: Int, height: Int,
      shape: SurfaceNetworkShape): SurfaceDecorationGeometry =
    val scientific = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(-1.0, 1.0, 0.0), Seq(1.0, 1.0, 0.0)),
      Seq((0, 1, 2), (1, 3, 2))), Hemisphere.Left, SurfaceKind.Pial)
    val nodes = Vector.tabulate(4)(i => ok(SurfaceNetworkNode.onSurface(SurfaceNetworkNodeId.unsafe(s"node-$i"),
      surface, VertexId(i), scientific)))
    val network = ok(SurfaceNetwork.make(nodes, Vector(
      SurfaceNetworkEdge.unsafe(nodes(0).id, nodes(3).id, .5),
      SurfaceNetworkEdge.unsafe(nodes(2).id, nodes(1).id, -1))))
    val style = shape match
      case SurfaceNetworkShape.Line => SurfaceNetworkStyle.line(SurfaceNetworkRadius.unsafe(.035))
      case SurfaceNetworkShape.Tube => ok(SurfaceNetworkStyle.tube(SurfaceNetworkRadius.unsafe(.035), 12))
    val display = ok(SurfaceNetworkDisplay.compile(network, SurfaceNetworkFilter.All, style,
      SurfaceNetworkPalette(red, Rgba32.unsafe(255, 255, 0))))
    val geometries = plan.views.map: view =>
      ok(SurfaceNetworkDecorations.lower(display, plan,
        SurfaceNetworkDecorationMapping(view.id, Map(surface -> view.instances.find(_.surface == surface).get.id)),
        SurfaceMarkerShape.Sphere, ok(SurfaceDecorationSize.world(.12)), width, height,
        Some(ok(SurfaceNetworkDecorationRadius.byAbsoluteWeight(SurfaceNetworkRadius.unsafe(.07),
          SurfaceNetworkRadius.unsafe(.01), SurfaceNetworkRadius.unsafe(.1)))), red))
    geometries.head.copy(meshes = geometries.flatMap(_.meshes))

  def main(args: Array[String]): Unit =
    val out = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-decorations"))
    Files.createDirectories(out)
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
        val base = source(surface, -1, 0)
        val cover = source(SurfaceId.unsafe("cover"), 0, .4)
        val resources = SurfaceCompositionResources((base.meshes ++ cover.meshes).map(m => m.resourceKey -> m).toMap,
          (base.layers ++ cover.layers).map(l => l.resourceKey -> l).toMap)
        def instance(name: String, p: SurfaceRenderPlan) = SurfaceCompositionInstance(ok(SurfaceInstanceId.make(name)),
          p.meshes.head.surface, p.meshes.head.resourceKey, p.layers.map(_.resourceKey))
        val view = SurfaceCompositionView(ok(SurfaceViewId.make("annotations")), SurfaceViewport(0, 0, 1, 1), base.camera,
          Vector(instance("anchor", base), instance("cover", cover)), SurfaceLighting.Unlit, SurfaceClipping.Disabled)
        var plan = ok(SurfaceCompositionPlan.make(Vector(view), resources))
        var size = ok(JavaFxSnapshotConfig.make(320, 240, SceneAntialiasing.DISABLED))
        ok(native.render(plan, size))
        stage.setScene(new Scene(native.root, size.width, size.height, Color.WHITE))
        stage.show()
        val labelRasterizer = new JavaFxSurfaceLabelRasterizer("Arial")
        def marker(name: String, shape: SurfaceMarkerShape, measure: SurfaceDecorationSize, policy: SurfaceDecorationDepthPolicy) =
          SurfaceMarkerDecoration(ok(SurfaceDecorationId.make(name)), anchor, shape, measure, red, policy)
        val cases: Vector[(String, SurfaceDecoration)] = Vector(
          "disk" -> marker("disk", SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(18)), SurfaceDecorationDepthPolicy.DepthTest),
          "sphere" -> marker("sphere", SurfaceMarkerShape.Sphere, ok(SurfaceDecorationSize.world(.25)), SurfaceDecorationDepthPolicy.DepthTest),
          "crosshair" -> marker("crosshair", SurfaceMarkerShape.Crosshair, ok(SurfaceDecorationSize.pixels(18)), SurfaceDecorationDepthPolicy.Overlay),
          "contour" -> SurfaceContourDecoration(ok(SurfaceDecorationId.make("contour")), surface, Vector(1, 2, 2, 1), SurfaceContourPolicy.Transitions,
            SurfaceContourStyle(ok(SurfaceDecorationSize.pixels(4)), red, Some(SurfaceContourHalo(Rgba32.unsafe(0, 255, 0), ok(SurfaceDecorationSize.pixels(2)))))),
          "label" -> ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("label")), anchor, "ROI", ok(SurfaceDecorationSize.pixels(28)), red)))
        var metrics = Vector.empty[String]
        var totalPicks = 0
        var failures = Vector.empty[String]
        for scale <- Vector(1, 2) do
          size = ok(JavaFxSnapshotConfig.make(320 * scale, 240 * scale, SceneAntialiasing.DISABLED))
          val allCases = cases ++ Vector("network-line" -> cases.head._2, "network-tube" -> cases.head._2)
          for (name, decoration) <- allCases do
            val geometry = if name.startsWith("network-") then
              networkGeometry(plan, size.width, size.height, if name == "network-line" then SurfaceNetworkShape.Line else SurfaceNetworkShape.Tube)
            else ok(SurfaceLabelLayout.resolve(ok(SurfaceDecorationGeometry.lower(
              ok(SurfaceDecorationCompiler.compile(Vector(decoration), plan)), plan, size.width, size.height)), plan, labelRasterizer))
            val receipt = ok(native.renderDecorated(plan, geometry, size))
            require(receipt.native.geometryUpdates == 0 && receipt.native.atlasUpdates == 0, "annotation update reuploaded scientific buffers")
            val image = ok(native.snapshot())
            val raster = ok(SurfaceDecorationRaster.render(plan, geometry, RasterDimensions.unsafe(size.width, size.height),
              SurfaceRasterStyle(culling = TriangleCulling.None)))
            val actual = Array.tabulate(size.width * size.height)(i => image.getPixelReader.getArgb(i % size.width, i / size.width))
            val expected = Array.tabulate(actual.length): i =>
              val c = raster.image.pixelUnsafe(i % size.width, i / size.width)
              (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue
            write(actual, size.width, size.height, out.resolve(s"$name-$scale-javafx.png"))
            write(expected, size.width, size.height, out.resolve(s"$name-$scale-raster.png"))
            def isRed(pixel: Int): Boolean = ((pixel >>> 16) & 255) > 180 && ((pixel >>> 8) & 255) < 80 && (pixel & 255) < 80
            val expectedMask = expected.indices.filter(i => isRed(expected(i))).toSet
            val actualMask = actual.indices.filter(i => isRed(actual(i))).toSet
            val iou = expectedMask.intersect(actualMask).size.toDouble / expectedMask.union(actualMask).size
            require(expectedMask.size > 20 && actualMask.size > 20, s"$name missing glyph")
            if iou < .85 then failures :+= s"$name-$scale native/raster red mask mismatch: $iou"
            var picks = 0
            for y <- 2 until size.height - 2 by 3; x <- 2 until size.width - 2 by 3 do
              val radius = if name == "label" then 0 else 1
              val interior = (-radius to radius).forall(dy => (-radius to radius).forall(dx => expectedMask((y + dy) * size.width + x + dx) && actualMask((y + dy) * size.width + x + dx)))
              if interior then
                val result = JavaFxNativePick.at(native.subScenes(view.id), x + .5, y + .5).get
                val hit = ok(native.pickDisplay(result))
                if name == "label" then require(hit.isInstanceOf[SurfaceDisplayHit.Scientific[?]], "label intercepted scientific pick")
                else hit match
                  case SurfaceDisplayHit.Decoration(value) =>
                    require(value.origin.view == view.id, s"$name incorrect annotation view")
                    if name.startsWith("network-") then
                      require(value.id.decoration.value.startsWith("network-"), s"$name lost network identity")
                    else require(value.id.decoration == decoration.id, s"$name incorrect annotation identity")
                  case _ => throw new IllegalStateException(s"$name lost annotation pick at $x,$y")
                picks += 1
            require(picks > 0, s"$name no interior native pick samples")
            totalPicks += picks
            metrics :+= s"{\"case\":\"$name-$scale\",\"redMaskIoU\":$iou,\"nativePicks\":$picks}"
            require(native.resize(ok(JavaFxSnapshotConfig.make(size.width + 1, size.height))).isLeft, "stale decoration resize accepted")
        // Independent RGBA oracle: transparent hole, half-alpha red over blue,
        // and an occluder over the green half of the same label quad.
        size = ok(JavaFxSnapshotConfig.make(320, 240, SceneAntialiasing.DISABLED))
        val oracleLabel = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("alpha-oracle")), anchor,
          "oracle", ok(SurfaceDecorationSize.pixels(20)), red))
        val oracleRasterizer = new SurfaceLabelRasterizer:
          def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
            val pixels = Array.tabulate(12): i =>
              if i % 6 < 2 then 0
              else if i % 6 < 4 then Rgba32.unsafe(255, 0, 0, 128).toPackedInt
              else Rgba32.unsafe(0, 255, 0).toPackedInt
            Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(6, 2), pixels), 1, 3, 1))
        def alphaCapture(lighting: SurfaceLighting): Array[Int] =
          plan = ok(SurfaceCompositionPlan.make(Vector(view.copy(lighting = lighting)), resources))
          val lowered = ok(SurfaceLabelLayout.resolve(ok(SurfaceDecorationGeometry.lower(
            ok(SurfaceDecorationCompiler.compile(Vector(oracleLabel), plan)), plan, 320, 240)), plan, oracleRasterizer))
          ok(native.renderDecorated(plan, lowered, size))
          val image = ok(native.snapshot())
          val pixels = Array.tabulate(320 * 240)(i => image.getPixelReader.getArgb(i % 320, i / 320))
          val expected = ok(SurfaceDecorationRaster.render(plan, lowered, RasterDimensions.unsafe(320, 240), SurfaceRasterStyle(culling = TriangleCulling.None)))
          val rasterPixels = Array.tabulate(pixels.length): i =>
            val c = expected.image.pixelUnsafe(i % 320, i / 320)
            (c.alpha << 24) | (c.red << 16) | (c.green << 8) | c.blue
          write(pixels, 320, 240, out.resolve("alpha-oracle-javafx.png"))
          write(rasterPixels, 320, 240, out.resolve("alpha-oracle-raster.png"))
          pixels
        val rgba = alphaCapture(SurfaceLighting.Unlit)
        require(rgba(120 * 320 + 110) == 0xff0000ff, "transparent texels wrote a quad background")
        val half = rgba(120 * 320 + 150)
        require(math.abs(((half >>> 16) & 255) - 128) <= 1 && ((half >>> 8) & 255) == 0 &&
          math.abs((half & 255) - 127) <= 1, s"half-alpha texel composite incorrect: $half")
        require(rgba(120 * 320 + 190) == 0xff0000ff, "label quad ignored per-fragment occlusion")
        val bright = alphaCapture(ok(SurfaceLighting.directional(1, 1, 0, 0, 1)))
        require(bright(120 * 320 + 150) == half, "label pixels changed under lighting")
        // POT striped texture distinguishes integer and fractional mip levels.
        // The level-1 stripe is exactly 255/0; level 2 is uniformly 128.
        plan = ok(SurfaceCompositionPlan.make(Vector(view.copy(instances = Vector(view.instances.head))), resources))
        for lod <- Vector(0.0, 1.0, 2.0, 1.5) do
          val em = math.pow(2, -lod)
          val pixels = Array.tabulate(32 * 32): i =>
            val value = if (i % 32 / 2) % 2 == 0 then 255 else 0
            Rgba32.unsafe(value, value, value).toPackedInt
          val provider = new SurfaceLabelRasterizer:
            def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
              Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(32, 32), pixels), 1, 16, 16))
          val label = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make(s"mip-$lod")), anchor, "mip", ok(SurfaceDecorationSize.pixels(em)), red))
          val lowered = ok(SurfaceLabelLayout.resolve(ok(SurfaceDecorationGeometry.lower(
            ok(SurfaceDecorationCompiler.compile(Vector(label), plan)), plan, 320, 240)), plan, provider))
          ok(native.renderDecorated(plan, lowered, size))
          val image = ok(native.snapshot())
          val extent = 32 * em
          val left = 160 - extent / 2
          var maximum = 0
          var samples = 0
          for x <- math.ceil(left + .5).toInt until math.floor(left + extent - .5).toInt do
            val u = (x + .5 - left) / extent
            def bilinearStripe(width: Int, block: Int): Double =
              val tx = u * width - .5
              val i = math.floor(tx).toInt
              val f = tx - i
              def at(index: Int) = if (((index % width + width) % width) / block) % 2 == 0 then 255.0 else 0.0
              at(i) * (1 - f) + at(i + 1) * f
            val expected = lod match
              case 0.0 => bilinearStripe(32, 2)
              case 1.0 => bilinearStripe(16, 1)
              case 2.0 => 128.0
              case _ => .5 * bilinearStripe(16, 1) + 64.0
            val argb = image.getPixelReader.getArgb(x, 120)
            for shift <- Vector(0, 8, 16) do maximum = math.max(maximum, math.abs(((argb >>> shift) & 255) - math.round(expected).toInt))
            samples += 1
          require(samples > 2 && maximum <= 3, s"POT mip oracle LOD $lod differs by $maximum")
          metrics :+= s"{\"case\":\"mip-lod-$lod\",\"maximumChannelError\":$maximum,\"samples\":$samples}"
        // Two partially transparent label planes must compose regardless of
        // API submission order. Their positions are explicit display geometry.
        val alphaPixels = RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(2, 2), Array.fill(4)(Rgba32.unsafe(255, 0, 0, 128).toPackedInt))
        val provider = new SurfaceLabelRasterizer:
          def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
            Right(SurfaceLabelAsset(alphaPixels, 1, 1, 1))
        val label = ok(SurfaceLabelDecoration.make(ok(SurfaceDecorationId.make("overlap-label")), anchor, "alpha", ok(SurfaceDecorationSize.pixels(20)), red))
        val baseGeometry = ok(SurfaceLabelLayout.resolve(ok(SurfaceDecorationGeometry.lower(
          ok(SurfaceDecorationCompiler.compile(Vector(label), plan)), plan, 320, 240)), plan, provider))
        def translated(mesh: SurfaceDecorationMesh, dz: Float): SurfaceDecorationMesh =
          val positions = Array.tabulate(mesh.positions.length)(i => mesh.positions(i) + (if i % 3 == 2 then dz else 0f))
          mesh.copy(positions = new FloatBufferView(positions))
        val near = translated(baseGeometry.meshes.head, .2f)
        val greenImage = RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(2, 2), Array.fill(4)(Rgba32.unsafe(0, 255, 0, 128).toPackedInt))
        val far = translated(near, -.1f).copy(id = SurfaceDecorationElementId(ok(SurfaceDecorationId.make("far-label")), "text"),
          texture = near.texture.map(_.copy(image = greenImage)))
        for (name, meshes) <- Vector("near-first" -> Vector(near, far), "far-first" -> Vector(far, near)) do
          val geometry = baseGeometry.copy(meshes = meshes)
          ok(native.renderDecorated(plan, geometry, size))
          val image = ok(native.snapshot())
          val c = image.getPixelReader.getArgb(160, 120)
          val expected = Vector(128, 64, 63)
          val actual = Vector((c >>> 16) & 255, (c >>> 8) & 255, c & 255)
          val pass = actual.zip(expected).forall((a, b) => math.abs(a - b) <= 1)
          metrics :+= s"{\"case\":\"label-overlap-$name\",\"rgb\":[${actual.mkString(",")}],\"passed\":$pass}"
          write(Array.tabulate(320 * 240)(i => image.getPixelReader.getArgb(i % 320, i / 320)), 320, 240,
            out.resolve(s"label-overlap-$name-javafx.png"))
          if !pass then failures :+= s"label overlap $name expected $expected, got $actual"
        val opaque = translated(near, -.1f).copy(id = SurfaceDecorationElementId(ok(SurfaceDecorationId.make("opaque-glyph")), "quad"),
          texture = None, color = Rgba32.unsafe(0, 255, 0), pickable = true)
        for meshes <- Vector(Vector(near, opaque), Vector(opaque, near)) do
          ok(native.renderDecorated(plan, baseGeometry.copy(meshes = meshes), size))
          val image = ok(native.snapshot())
          val c = image.getPixelReader.getArgb(160, 120)
          require(math.abs(((c >>> 16) & 255) - 128) <= 1 && math.abs(((c >>> 8) & 255) - 127) <= 1 &&
            (c & 255) == 0, "opaque glyph behind translucent label was lost")
        val tilted = near.copy(positions = new FloatBufferView(Array.tabulate(near.positions.length)(i =>
          near.positions(i) + (if i == 2 then .1f else 0f))))
        require(native.renderDecorated(plan, baseGeometry.copy(meshes = Vector(tilted)), size).isLeft,
          "nonplanar transparent geometry passed native label admission")
        Files.writeString(out.resolve("decoration-metrics.json"), s"{\"passed\":${failures.isEmpty},\"nativePicks\":$totalPicks,\"cases\":[${metrics.mkString(",")}]}\n")
        require(failures.isEmpty, failures.mkString("; "))
        Files.writeString(out.resolve("decoration-metrics.json"), s"{\"passed\":true,\"nativePicks\":$totalPicks,\"cases\":[${metrics.mkString(",")}]}\n")
        println(s"PASS native decorations: $totalPicks typed/pass-through picks; pixel/world size, depth, contour halo, text, retained scientific resources")
      catch case error: Throwable => failure = error
      finally
        backend.foreach(b => ok(b.dispose()))
        stage.close()
        done.countDown()
    require(done.await(90, TimeUnit.SECONDS), "native annotation probe timed out")
    Platform.exit()
    if failure != null then throw failure.nn

  private def write(pixels: Array[Int], width: Int, height: Int, path: Path): Unit =
    val image = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, width, height, pixels, 0, width)
    require(ImageIO.write(image, "png", path.toFile))
