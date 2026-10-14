package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing}
import javafx.scene.image.WritableImage
import javafx.scene.shape.{MeshView, TriangleMesh, VertexFormat}
import javafx.scene.paint.{Color, PhongMaterial}
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** General layer/lighting experiment using separately padded face textures.
  * Sampling and native pixel errors are measured against the shared evaluator.
  * This probe does not admit a production backend capability.
  */
object JavaFxSurfaceLayeredPartitionProbe:
  private val base = Rgba32.unsafe(184, 184, 184)

  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      val rows = Vector.newBuilder[String]
      try
        for aa <- Vector(false, true); perspective <- Vector(false, true); size <- Vector(64, 128, 256); mode <- Vector("layered", "lit"); width <- Vector(16, 32, 64) do
          val mapping = SurfaceScalarFixture.thresholded
          val scalarModel = SurfaceScalarFixture.model(mapping)
          val under = SurfaceLayer.interpolatedScalar(SurfaceLayerId.unsafe("curvature"), SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, Array(0.0, 0.0, 1.0, 1.0), ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(0, 1),
              ScalarRamp.linear(Rgba32.unsafe(40, 40, 40), Rgba32.unsafe(180, 180, 180))))).toOption.get
          val over = SurfaceLayer.interpolatedScalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, mapping, opacity = DisplayOpacity.unsafe(0.7)).toOption.get
          val model = SurfaceViewerModel.make(scalarModel.surfaces, Vector(under, over)).toOption.get
          val initial = SurfaceFaceFixture.state(model)
          val state = if !perspective then initial else
            val projected = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
              CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
            SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
          val compiled = SurfaceCompiler.compile(model, state).toOption.get
          val plan = if mode != "lit" then compiled else compiled.copy(
            meshes = compiled.meshes.map(_.copy(normals = new FloatBufferView(Array[Float](
              -0.6f, 0, 0.8f, 0.6f, 0, 0.8f, 0, 0.6f, 0.8f, 0, -0.6f, 0.8f)))),
            lighting = SurfaceLighting.directional(0.2, 0.8, 0.4, 0.3, 0.85).toOption.get)
          val reference = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(size, size),
            SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
          val packet = plan.meshes.head
          val triangles = SurfaceMappingPartition.build(packet, plan.layers, SurfacePartitionBudget.make(10000, 64).toOption.get).toOption.get
          val layer = SurfaceLayer.scalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, mapping.colorizer).toOption.get
          val legacyModel = SurfaceViewerModel.make(model.surfaces, Vector(layer)).toOption.get
          val legacy = SurfaceCompiler.compile(legacyModel, state).toOption.get
          val backend = JavaFxSurfaceBackend.create().toOption.get
          var derivedViews = Vector.empty[MeshView]
          try
            backend.render(legacy).toOption.get
            require(backend.chunks.length == 1)
            val sourceView = backend.chunks.head.view
            val parent = sourceView.getParent.asInstanceOf[Group]
            var textureBytes = 0L
            derivedViews = triangles.map: triangle =>
              val (mesh, texture) = lower(plan, triangle, width)
              textureBytes += texture.getWidth.toLong * texture.getHeight.toLong * 4
              val material = new PhongMaterial()
              material.setSpecularColor(Color.TRANSPARENT)
              JavaFxSurfaceProbe.configureMaterial(material, texture, JavaFxMaterialMode.Unlit)
              val view = new MeshView(mesh)
              view.setMaterial(material)
              view.setCullFace(sourceView.getCullFace)
              view
            parent.getChildren.setAll(derivedViews*)
            val scene = backend.newSubScene(JavaFxSnapshotConfig.make(size, size,
              if aa then SceneAntialiasing.BALANCED else SceneAntialiasing.DISABLED).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(scene), size, size))
            stage.show()
            val image = new WritableImage(size, size)
            scene.snapshot(null, image)
            var checked = 0
            var away = 0
            var maxError = 0
            var awayError = 0
            var bad = 0
            val boundaries = SurfaceMappingPartition.boundaries(mapping)
            def sample(x: Int, y: Int): Option[Double] =
              if x < 0 || y < 0 || x >= size || y >= size then None
              else reference.pick(x, y).toOption.flatten.map: pick =>
                val (a, b, c) = packet.sourceFaceVertices(pick.face)
                pick.barycentricA * SurfaceScalarFixture.Values(a) + pick.barycentricB * SurfaceScalarFixture.Values(b) + pick.barycentricC * SurfaceScalarFixture.Values(c)
            for y <- 0 until size; x <- 0 until size; pick <- reference.pick(x, y).toOption.flatten do
              if math.min(pick.barycentricA, math.min(pick.barycentricB, pick.barycentricC)) > 0.04 then
                val value = sample(x, y).get
                val actual = image.getPixelReader.getArgb(x, y)
                val expected = reference.image.pixelUnsafe(x, y)
                val error = Vector(math.abs(((actual >>> 16) & 255) - expected.red),
                  math.abs(((actual >>> 8) & 255) - expected.green), math.abs((actual & 255) - expected.blue)).max
                checked += 1
                maxError = math.max(maxError, error)
                if error > 2 then bad += 1
                val dx = sample(x + 1, y).fold(0.0)(v => math.abs(v - value))
                val dy = sample(x, y + 1).fold(0.0)(v => math.abs(v - value))
                val exclusion = 2.0 * math.sqrt(dx * dx + dy * dy)
                if boundaries.forall(b => math.abs(value - b) > exclusion) then
                  away += 1
                  awayError = math.max(awayError, error)
            require(checked > 100 && away > 100, s"insufficient coverage: $checked/$away")
            val row = s"{\"mode\":\"$mode\",\"antialiasing\":$aa,\"perspective\":$perspective,\"size\":$size,\"stripWidth\":$width,\"triangles\":${triangles.length},\"textureBytes\":${textureBytes},\"checked\":$checked,\"awayChecked\":$away,\"maximumError\":$maxError,\"awayMaximumError\":$awayError,\"pixelsOverBudget\":$bad,\"awayBudgetPassed\":${awayError <= 2}}"
            rows += row
            println(s"layered_partition_probe=$row")
            if aa && perspective && size == 256 && mode == "lit" && width == 64 then
              val out = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
              for y <- 0 until size; x <- 0 until size do out.setRGB(x, y, image.getPixelReader.getArgb(x, y))
              javax.imageio.ImageIO.write(out, "png", new java.io.File("/private/tmp/scalafim-layered-partition-javafx.png"))
          finally
            backend.dispose()
            derivedViews.foreach: view =>
              view.setMesh(null)
              view.setMaterial(null)
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-layered-partition-javafx.json"), rows.result().mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
        Platform.exit()
    require(done.await(180, TimeUnit.SECONDS), "JavaFX partition probe timed out")
    if failure != null then throw failure.nn

  private def lower(plan: SurfaceRenderPlan, triangle: SurfaceMappingTriangle, width: Int): (TriangleMesh, WritableImage) =
    val packet = plan.meshes.head
    val evaluator = new SurfaceFragmentEvaluator(packet, plan.layers, plan.lighting, base).within(triangle)
    val mesh = new TriangleMesh(VertexFormat.POINT_NORMAL_TEXCOORD)
    val points = new Array[Float](9)
    val uv = new Array[Float](6)
    val faces = new Array[Int](9)
    val (a, b, c) = triangle.sourceVertices
    val corners = Vector(triangle.a, triangle.b, triangle.c)
    corners.zipWithIndex.foreach: (w, corner) =>
      for axis <- 0 until 3 do
        points(corner * 3 + axis) = (w.a * packet.positions(a * 3 + axis) + w.b * packet.positions(b * 3 + axis) + w.c * packet.positions(c * 3 + axis)).toFloat
    def delta(i: Int, j: Int): Vector[Double] = Vector.tabulate(3)(k => points(i * 3 + k).toDouble - points(j * 3 + k))
    def dot(x: Vector[Double], y: Vector[Double]): Double = x.zip(y).map(_ * _).sum
    val (i, j, k) = Vector((0, 1, 2), (1, 2, 0), (2, 0, 1)).maxBy((i, j, _) => dot(delta(j, i), delta(j, i)))
    val edge = delta(j, i)
    val length = math.sqrt(dot(edge, edge))
    val third = delta(k, i)
    val cx = dot(third, edge) / length
    val height = math.sqrt(math.max(0, dot(third, third) - cx * cx))
    require(height > 0, "degenerate derived triangle")
    // Equal texel density along orthogonal world-space axes avoids turning a
    // skinny scientific triangle into a square texture with extreme anisotropy.
    val density = (width - 3) / length
    val textureHeight = math.max(4, math.ceil(height * density).toInt + 3)
    val texture = new WritableImage(width, textureHeight)
    for y <- 0 until textureHeight; x <- 0 until width do
      val py = (y - 1.0) / density
      val px = (x - 1.0) / density
      val wk = py / height
      val wj = (px - cx * wk) / length
      val weights = new Array[Double](3)
      weights(i) = 1 - wj - wk
      weights(j) = wj
      weights(k) = wk
      val wa = weights(0) * triangle.a.a + weights(1) * triangle.b.a + weights(2) * triangle.c.a
      val wb = weights(0) * triangle.a.b + weights(1) * triangle.b.b + weights(2) * triangle.c.b
      val wc = weights(0) * triangle.a.c + weights(1) * triangle.b.c + weights(2) * triangle.c.c
      val color = evaluator.extrapolatedColor(triangle.sourceFace, a, b, c, wa, wb, wc)
      val argb = (255 << 24) | (color.red << 16) | (color.green << 8) | color.blue
      texture.getPixelWriter.setArgb(x, y, argb)
    uv(i * 2) = (1.5 / width).toFloat
    uv(i * 2 + 1) = (1.5 / textureHeight).toFloat
    uv(j * 2) = ((1.5 + length * density) / width).toFloat
    uv(j * 2 + 1) = (1.5 / textureHeight).toFloat
    uv(k * 2) = ((1.5 + cx * density) / width).toFloat
    uv(k * 2 + 1) = ((1.5 + height * density) / textureHeight).toFloat
    for corner <- 0 until 3 do
      faces(corner * 3) = corner
      faces(corner * 3 + 1) = 0
      faces(corner * 3 + 2) = corner
    mesh.getPoints.setAll(points, 0, points.length)
    mesh.getNormals.setAll(0f, 0f, 1f)
    mesh.getTexCoords.setAll(uv, 0, uv.length)
    mesh.getFaces.setAll(faces, 0, faces.length)
    (mesh, texture)
