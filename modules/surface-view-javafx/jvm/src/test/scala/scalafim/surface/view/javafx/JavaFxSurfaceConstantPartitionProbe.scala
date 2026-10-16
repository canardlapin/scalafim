package scalafim.surface.view.javafx

import java.util.concurrent.{CountDownLatch, TimeUnit}
import javafx.application.Platform
import javafx.scene.{Group, Scene, SceneAntialiasing}
import javafx.scene.image.WritableImage
import javafx.scene.shape.{TriangleMesh, VertexFormat}
import javafx.stage.Stage
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Native check of certified constant-color subdivision with palette-center UVs. */
object JavaFxSurfaceConstantPartitionProbe:
  private val base = Rgba32.unsafe(184, 184, 184)

  def main(args: Array[String]): Unit =
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup(() => ())
    Platform.runLater: () =>
      Platform.setImplicitExit(false)
      val stage = new Stage()
      val rows = Vector.newBuilder[String]
      val cache = scala.collection.mutable.Map.empty[String, SurfaceFragmentApproximation]
      try
        for aa <- Vector(false, true); perspective <- Vector(false, true); size <- Vector(64, 128, 256); mode <- Vector("layered", "lit"); width <- Vector(1) do
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
          val approximation = cache.getOrElseUpdate(mode, SurfaceFragmentApproximation.build(packet, plan.layers, plan.lighting,
            SurfaceFragmentApproximationConfig.make(width, 1000000).toOption.get).fold(e => throw new IllegalArgumentException(e.message), identity))
          val triangles = approximation.cells.map(_.triangle)
          val layer = SurfaceLayer.scalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
            SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, mapping.colorizer).toOption.get
          val legacyModel = SurfaceViewerModel.make(model.surfaces, Vector(layer)).toOption.get
          val legacy = SurfaceCompiler.compile(legacyModel, state).toOption.get
          val backend = JavaFxSurfaceBackend.create().toOption.get
          try
            backend.render(legacy).toOption.get
            require(backend.chunks.length == 1)
            val sourceView = backend.chunks.head.view
            val (mesh, texture) = lower(plan, approximation)
            val textureBytes = texture.getWidth.toLong * texture.getHeight.toLong * 4
            sourceView.setMesh(mesh)
            JavaFxSurfaceProbe.configureMaterial(backend.chunks.head.material, texture, JavaFxMaterialMode.Unlit)
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
            val row = s"{\"mode\":\"$mode\",\"antialiasing\":$aa,\"perspective\":$perspective,\"size\":$size,\"certifiedChannelError\":${approximation.maximumChannelError},\"triangles\":${triangles.length},\"textureBytes\":${textureBytes},\"checked\":$checked,\"awayChecked\":$away,\"maximumError\":$maxError,\"awayMaximumError\":$awayError,\"pixelsOverBudget\":$bad,\"awayBudgetPassed\":${awayError <= 2}}"
            rows += row
            println(s"constant_partition_probe=$row")
            if aa && perspective && size == 256 && mode == "lit" && width == 1 then
              val out = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
              for y <- 0 until size; x <- 0 until size do out.setRGB(x, y, image.getPixelReader.getArgb(x, y))
              javax.imageio.ImageIO.write(out, "png", new java.io.File("/private/tmp/scalafim-constant-partition-javafx.png"))
          finally
            backend.dispose()
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-constant-partition-javafx.json"), rows.result().mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
    try
      require(done.await(180, TimeUnit.SECONDS), "JavaFX partition probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()

  private def lower(plan: SurfaceRenderPlan, approximation: SurfaceFragmentApproximation): (TriangleMesh, WritableImage) =
    val packet = plan.meshes.head
    val palette = scala.collection.mutable.LinkedHashMap.empty[Int, Int]
    approximation.cells.foreach(cell => palette.getOrElseUpdate(cell.color.toPackedInt, palette.size))
    val columns = math.min(256, palette.size)
    val rows = (palette.size + columns - 1) / columns
    require(rows <= 1024, "prototype palette texture budget exceeded")
    val texture = new WritableImage(columns * 4, rows * 4)
    palette.foreach: (packed, index) =>
      val color = Rgba32.fromPackedInt(packed)
      val argb = (255 << 24) | (color.red << 16) | (color.green << 8) | color.blue
      for y <- 0 until 4; x <- 0 until 4 do
        texture.getPixelWriter.setArgb((index % columns) * 4 + x, (index / columns) * 4 + y, argb)
    val mesh = new TriangleMesh(VertexFormat.POINT_NORMAL_TEXCOORD)
    val points = new Array[Float](approximation.cells.length * 9)
    val uv = new Array[Float](approximation.cells.length * 2)
    val faces = new Array[Int](approximation.cells.length * 9)
    approximation.cells.zipWithIndex.foreach: (cell, face) =>
      val t = cell.triangle
      val (a, b, c) = t.sourceVertices
      Vector(t.a, t.b, t.c).zipWithIndex.foreach: (w, corner) =>
        val vertex = face * 3 + corner
        for axis <- 0 until 3 do
          points(vertex * 3 + axis) = (w.a * packet.positions(a * 3 + axis) + w.b * packet.positions(b * 3 + axis) + w.c * packet.positions(c * 3 + axis)).toFloat
        faces(vertex * 3) = vertex
        faces(vertex * 3 + 1) = 0
        faces(vertex * 3 + 2) = face
      val swatch = palette(cell.color.toPackedInt)
      uv(face * 2) = (((swatch % columns) * 4 + 2.0) / texture.getWidth).toFloat
      uv(face * 2 + 1) = (((swatch / columns) * 4 + 2.0) / texture.getHeight).toFloat
    mesh.getPoints.setAll(points, 0, points.length)
    mesh.getNormals.setAll(0f, 0f, 1f)
    mesh.getTexCoords.setAll(uv, 0, uv.length)
    mesh.getFaces.setAll(faces, 0, faces.length)
    (mesh, texture)
