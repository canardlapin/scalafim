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

/** Boundary-partition experiment. Each triangle uses a separate padded strip.
  * Pixel acceptance is <=2 byte values beyond a two-pixel boundary exclusion;
  * full interior errors and geometry/texture costs are also retained.
  */
object JavaFxSurfacePartitionProbe:
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
        for aa <- Vector(false, true); perspective <- Vector(false, true); size <- Vector(64, 128, 256); mode <- Vector("affine", "piecewise", "threshold"); width <- Vector(32, 128, 512) do
          val mapping = if mode == "affine" then ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-2, 4),
            ScalarRamp.linear(Rgba32.unsafe(0, 0, 0), Rgba32.unsafe(240, 240, 240))))
            else if mode == "threshold" then SurfaceScalarFixture.thresholded else SurfaceScalarFixture.mapping
          val model = SurfaceScalarFixture.model(mapping)
          val initial = SurfaceFaceFixture.state(model)
          val state = if !perspective then initial else
            val projected = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(
              CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50)))).toOption.get
            SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30, 20)).toOption.get
          val plan = SurfaceCompiler.compile(model, state).toOption.get
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
            derivedViews = triangles.map: triangle =>
              val (mesh, texture) = lower(packet, Vector(triangle), mapping, width)
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
            val row = s"{\"mode\":\"$mode\",\"antialiasing\":$aa,\"perspective\":$perspective,\"size\":$size,\"stripWidth\":$width,\"triangles\":${triangles.length},\"textureBytes\":${width.toLong * triangles.length * 4 * 4},\"checked\":$checked,\"awayChecked\":$away,\"maximumError\":$maxError,\"awayMaximumError\":$awayError,\"pixelsOverBudget\":$bad,\"awayBudgetPassed\":${awayError <= 2}}"
            rows += row
            println(s"partition_probe=$row")
            if aa && perspective && size == 256 && mode == "threshold" && width == 512 then
              val out = new java.awt.image.BufferedImage(size, size, java.awt.image.BufferedImage.TYPE_INT_ARGB)
              for y <- 0 until size; x <- 0 until size do out.setRGB(x, y, image.getPixelReader.getArgb(x, y))
              javax.imageio.ImageIO.write(out, "png", new java.io.File("/private/tmp/scalafim-partition-javafx.png"))
          finally
            backend.dispose()
            derivedViews.foreach: view =>
              view.setMesh(null)
              view.setMaterial(null)
        java.nio.file.Files.writeString(java.nio.file.Path.of("/private/tmp/scalafim-partition-javafx.json"), rows.result().mkString("[\n", ",\n", "\n]\n"))
      catch case error: Throwable => failure = error
      finally
        stage.close()
        done.countDown()
        Platform.exit()
    require(done.await(180, TimeUnit.SECONDS), "JavaFX partition probe timed out")
    if failure != null then throw failure.nn

  private def lower(packet: SurfaceMeshPacket, triangles: Vector[SurfaceMappingTriangle], mapping: ScalarMapping,
      width: Int): (TriangleMesh, WritableImage) =
    val mesh = new TriangleMesh(VertexFormat.POINT_NORMAL_TEXCOORD)
    val points = new Array[Float](triangles.length * 9)
    val uv = new Array[Float](triangles.length * 6)
    val faces = new Array[Int](triangles.length * 9)
    val texture = new WritableImage(width, triangles.length * 4)
    triangles.zipWithIndex.foreach: (triangle, index) =>
      val (a, b, c) = triangle.sourceVertices
      def scalar(w: SurfaceFaceWeights): Double =
        w.a * SurfaceScalarFixture.Values(a) + w.b * SurfaceScalarFixture.Values(b) + w.c * SurfaceScalarFixture.Values(c)
      val corners = Vector(triangle.a, triangle.b, triangle.c)
      val samples = corners.map(scalar)
      val lo = samples.min
      val hi = samples.max
      val center = mapping.evaluate(scalar(triangle.centroid))
      for x <- 0 until width do
        val value = lo + (hi - lo) * x / (width - 1)
        // Select the cell's side of the boundary. Do not smear an endpoint's
        // inclusion rule across an adjacent open region through texture filtering.
        val over = center.coordinate match
          case Some(coordinate) =>
            val segment = mapping.scale.segments(coordinate.segment)
            segment.ramp.colorAt(segment.window.normalize(value))
          case None => center.color
        val color = DisplayBlendMode.Normal.composite(base, over, DisplayOpacity.Opaque)
        val argb = (255 << 24) | (color.red << 16) | (color.green << 8) | color.blue
        for y <- 0 until 4 do texture.getPixelWriter.setArgb(x, index * 4 + y, argb)
      corners.zipWithIndex.foreach: (w, corner) =>
        val vertex = index * 3 + corner
        for axis <- 0 until 3 do
          points(vertex * 3 + axis) = (w.a * packet.positions(a * 3 + axis) + w.b * packet.positions(b * 3 + axis) + w.c * packet.positions(c * 3 + axis)).toFloat
        val fraction = if hi == lo then 0.5 else (samples(corner) - lo) / (hi - lo)
        uv(vertex * 2) = ((0.5 + fraction * (width - 1)) / width).toFloat
        uv(vertex * 2 + 1) = ((index * 4 + 2.0) / texture.getHeight).toFloat
        faces(vertex * 3) = vertex
        faces(vertex * 3 + 1) = 0
        faces(vertex * 3 + 2) = vertex
    mesh.getPoints.setAll(points, 0, points.length)
    mesh.getNormals.setAll(0f, 0f, 1f)
    mesh.getTexCoords.setAll(uv, 0, uv.length)
    mesh.getFaces.setAll(faces, 0, faces.length)
    (mesh, texture)
