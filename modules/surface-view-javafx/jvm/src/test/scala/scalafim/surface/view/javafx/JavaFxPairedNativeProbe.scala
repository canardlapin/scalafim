package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing}
import javafx.scene.image.WritableImage
import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Native GPU snapshots and JavaFX scene traversal/ray intersections. No Stage or OS events. */
object JavaFxPairedNativeProbe:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")
  private val leftLayer = SurfaceLayerId.unsafe("left-colors")
  private val rightLayer = SurfaceLayerId.unsafe("right-colors")
  private val lateral = Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left), right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right))
  private val medial = Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left), right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right))
  private def asset(id: SurfaceId, hemisphere: Hemisphere, sign: Double): SurfaceAsset =
    val positions = Vector(3.0, 1.0).flatMap(x =>
      Vector(Vector(sign * x, -2.0, -1.0), Vector(sign * x, 3.0, -1.0), Vector(sign * x, -1.0, 4.0)))
    val faces = if sign < 0 then Vector((0, 2, 1), (3, 4, 5)) else Vector((0, 1, 2), (3, 5, 4))
    val inflated = SurfaceGeometry(TriangleMesh.fromRows(positions, faces), hemisphere, SurfaceKind.Inflated)
    val white = SurfaceGeometry(TriangleMesh.fromRows(positions.map(p => Vector(p(0), p(1) * 0.85 + 0.4, p(2) * 0.9 - 0.2)), faces), hemisphere, SurfaceKind.White)
    SurfaceAsset.make(id, SurfaceSet.of(SurfaceKind.Inflated, inflated, SurfaceKind.White -> white)).toOption.get
  private val assets = Vector(asset(left, Hemisphere.Left, -1), asset(right, Hemisphere.Right, 1))
  private val layers = assets.zipWithIndex.map: (asset, index) =>
    val outer = if index == 0 then Rgba32.unsafe(230, 60, 40) else Rgba32.unsafe(40, 190, 90)
    val inner = if index == 0 then Rgba32.unsafe(40, 80, 230) else Rgba32.unsafe(220, 180, 40)
    val colors = Vector.fill(3)(outer) ++ Vector.fill(3)(inner)
    val inverted = colors.map(c => Rgba32.unsafe(255 - c.red, 255 - c.green, 255 - c.blue))
    SurfaceLayer.packedRgba(if index == 0 then leftLayer else rightLayer, asset.id, asset.geometry,
      colors ++ inverted, frameCount = 2).toOption.get
  private val model = SurfaceViewerModel.make(assets, layers).toOption.get
  private def initial(perspective: Boolean): SurfaceViewerState =
    val value = SurfaceViewerState.initial(model)
    value.copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = lateral, camera = value.camera.copy(projection = if perspective then
        CameraProjection.Perspective(FieldOfViewDegrees.unsafe(45)) else CameraProjection.Orthographic(OrthographicScale.unsafe(4.5))))
  private val stages: Vector[(String, Int, Int, Vector[SurfaceViewerAction])] = Vector(
    ("lateral", 600, 320, Vector.empty),
    ("medial", 600, 320, Vector(SurfaceViewerAction.SetSurfaceViewpoints(medial))),
    ("orbit", 600, 320, Vector(SurfaceViewerAction.OrbitBy(17, 11))),
    ("tall", 320, 600, Vector.empty),
    ("reorder", 320, 600, Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left, right, BilateralOrder.values.last)))),
    ("white", 600, 320, Vector(SurfaceViewerAction.SetGeometryState(left, SurfaceKind.White), SurfaceViewerAction.SetGeometryState(right, SurfaceKind.White))),
    ("morph", 600, 320, Vector(SurfaceViewerAction.BeginGeometryMorph(left, SurfaceKind.Inflated), SurfaceViewerAction.BeginGeometryMorph(right, SurfaceKind.Inflated),
      SurfaceViewerAction.SetGeometryMorphFraction(left, SurfaceMorphFraction.unsafe(0.5)), SurfaceViewerAction.SetGeometryMorphFraction(right, SurfaceMorphFraction.unsafe(0.5)))),
    ("time", 600, 320, Vector(SurfaceViewerAction.SetTimepoint(1))),
    ("opacity", 600, 320, Vector(SurfaceViewerAction.SetLayerOpacity(leftLayer, DisplayOpacity.unsafe(0.5)), SurfaceViewerAction.SetLayerOpacity(rightLayer, DisplayOpacity.unsafe(0.7)))),
    ("fit", 800, 400, Vector(SurfaceViewerAction.FitCamera)),
    ("focus", 400, 400, Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Single(right)))),
    ("paired", 800, 400, Vector(SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left, right)))),
    ("reset", 800, 400, Vector(SurfaceViewerAction.ResetCamera)),
    ("global", 800, 400, Vector(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Lateral(CorticalHemisphere.Left)))),
    ("restored", 800, 400, Vector(SurfaceViewerAction.SetSurfaceViewpoints(lateral))),
    ("clipped", 800, 400, Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.NearFar(0.1, 2.0)))),
    ("unclipped", 800, 400, Vector(SurfaceViewerAction.SetClipping(SurfaceClipping.Disabled)))
  )

  def main(args: Array[String]): Unit =
    val output = Path.of(args(0))
    Files.createDirectories(output)
    val done = new CountDownLatch(1)
    @volatile var failure: Option[Throwable] = None
    Platform.startup(() => ())
    Platform.runLater: () =>
      try
        val rows = Vector.newBuilder[String]
        for perspective <- Vector(false, true); aa <- Vector(SceneAntialiasing.DISABLED, SceneAntialiasing.BALANCED) do
          val state = initial(perspective)
          val backend = JavaFxSurfaceBackend.create(JavaFxAtlasConfig.make(maxTextureSize = 64,
            encoding = JavaFxAtlasEncoding.AdaptiveAffineOpaque).toOption.get).toOption.get
          try
            backend.render(SurfaceCompiler.compile(model, state).toOption.get).toOption.get
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(600, 320, aa).toOption.get).toOption.get
            val host = new Scene(new Group(scene), 800, 600)
            host.getRoot.applyCss()
            val controller = JavaFxSurfaceController.attach(model, state, backend, scene).toOption.get
            try
              for (name, width, height, actions) <- stages do
                actions.foreach(action => controller.dispatch(action).fold(error => throw new AssertionError(error.message), identity))
                scene.setWidth(width)
                scene.setHeight(height)
                val frameName = s"${if perspective then "perspective" else "orthographic"}-$aa-$name"
                val image = scene.snapshot(null, new WritableImage(width, height))
                val reference = SurfaceRasterizer.render(controller.plan, RasterDimensions.unsafe(width, height),
                  SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
                val nativeBitmap = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                val referenceBitmap = new BufferedImage(width, height, BufferedImage.TYPE_INT_ARGB)
                for y <- 0 until height; x <- 0 until width do
                  nativeBitmap.setRGB(x, y, image.getPixelReader.getArgb(x, y))
                  val color = reference.image.pixelUnsafe(x, y)
                  referenceBitmap.setRGB(x, y, (color.alpha << 24) | (color.red << 16) | (color.green << 8) | color.blue)
                ImageIO.write(nativeBitmap, "png", output.resolve(frameName + ".png").toFile)
                ImageIO.write(referenceBitmap, "png", output.resolve(frameName + "-reference.png").toFile)
                val points = Vector.newBuilder[String]
                var hits = 0
                var misses = 0
                var maxColor = 0
                var maxBary = 0.0
                val sourceFaces = scala.collection.mutable.Map.empty[String, Set[Int]]
                for y <- 4 until height - 4 by 7; x <- 4 until width - 4 by 7 do
                  val nearby = for dy <- -2 to 2; dx <- -2 to 2 yield reference.pick(x + dx, y + dy).toOption.flatten
                  val expected = reference.pick(x, y).toOption.flatten
                  val suitable = expected match
                    case None => nearby.forall(_.isEmpty)
                    case Some(p) =>
                      val weights = Vector(p.barycentricA, p.barycentricB, p.barycentricC).sorted
                      weights.head > 0.05 && weights(2) - weights(1) > 0.02 && nearby.forall(_.exists(q => q.surface == p.surface && q.face == p.face))
                  if suitable then
                    val native = JavaFxNativePick.at(scene, x + 0.5, y + 0.5)
                    expected match
                      case None =>
                        require(native.isEmpty, s"$frameName unexpected native pick at $x,$y")
                        misses += 1
                        points += s"""{"x":${x + 0.5},"y":${y + 0.5},"surface":null}"""
                      case Some(p) =>
                        val result = native.getOrElse(throw new AssertionError(s"$frameName native miss at $x,$y"))
                        val actual = controller.pick(result).fold(error => throw new AssertionError(error.message), identity)
                        require(actual.surface == p.surface && actual.face.index == p.face && actual.vertex.index == p.vertex,
                          s"$frameName native pick IDs at $x,$y: $actual versus $p")
                        val error = Vector(math.abs(actual.barycentricA - p.barycentricA), math.abs(actual.barycentricB - p.barycentricB), math.abs(actual.barycentricC - p.barycentricC)).max
                        maxBary = math.max(maxBary, error)
                        require(error <= 1e-4, s"$frameName barycentric error $error")
                        val color = reference.image.pixelUnsafe(x, y)
                        val argb = image.getPixelReader.getArgb(x, y)
                        val colorError = Vector(math.abs(((argb >>> 16) & 255) - color.red), math.abs(((argb >>> 8) & 255) - color.green), math.abs((argb & 255) - color.blue)).max
                        maxColor = math.max(maxColor, colorError)
                        require(colorError <= 3, s"$frameName color error $colorError at $x,$y")
                        sourceFaces.update(p.surface.value, sourceFaces.getOrElse(p.surface.value, Set.empty) + p.face)
                        hits += 1
                        points += s"""{"x":${x + 0.5},"y":${y + 0.5},"surface":"${p.surface.value}","face":${p.face},"vertex":${p.vertex},"weights":[${p.barycentricA},${p.barycentricB},${p.barycentricC}],"rgb":[${color.red},${color.green},${color.blue}]}"""
                if name == "lateral" || name == "medial" then
                  val face = if name == "lateral" then 0 else 1
                  require(sourceFaces.toMap == Map("left" -> Set(face), "right" -> Set(face)), s"$frameName did not expose both intended anatomical faces: $sourceFaces")
                require((name == "clipped" && hits == 0) || hits > 15, s"$frameName insufficient hits: $hits")
                require(misses > 10, s"$frameName insufficient miss coverage")
                val json = s"""{"name":"$frameName","stage":"$name","perspective":$perspective,"aa":"$aa","width":$width,"height":$height,"hits":$hits,"misses":$misses,"maxColorError":$maxColor,"maxBarycentricError":$maxBary,"points":${points.result().mkString("[", ",", "]")}}"""
                Files.writeString(output.resolve(frameName + ".json"), json + "\n")
                rows += json
                println(s"PASS $frameName: $hits native hits, $misses misses, color $maxColor, barycentric $maxBary")
            finally controller.dispose()
          finally backend.dispose()
        Files.writeString(output.resolve("results.json"), rows.result().mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = Some(error)
      finally done.countDown()
    try
      require(done.await(180, TimeUnit.SECONDS), "paired native probe timed out")
      failure.foreach(throw _)
    finally Platform.exit()
