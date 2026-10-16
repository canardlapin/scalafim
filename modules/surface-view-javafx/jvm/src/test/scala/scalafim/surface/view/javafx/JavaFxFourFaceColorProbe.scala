package scalafim.surface.view.javafx

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import java.util.concurrent.CountDownLatch
import javafx.animation.AnimationTimer
import javafx.application.Platform
import javafx.scene.{Group, Scene}
import javafx.scene.image.PixelFormat
import javafx.stage.Stage

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Native regression for four LegacyTriangle tiles.
  *
  * It exercises initial publication, a retained atlas update, a camera-only
  * update, and the NativePhong material transition. Atlas pixels alone cannot
  * establish that the JavaFX sampler rendered the self-illumination map.
  */
object JavaFxFourFaceColorProbe:
  private final case class Capture(width: Int, height: Int, pixels: Array[Int]):
    def nonBackgroundPixels: Int = pixels.count(_ != 0xffffffff)

    def chromaticPixels: Int =
      pixels.count: pixel =>
        val red = (pixel >>> 16) & 0xff
        val green = (pixel >>> 8) & 0xff
        val blue = pixel & 0xff
        ((pixel >>> 24) & 0xff) == 0xff && math.max(red, math.max(green, blue)) - math.min(red, math.min(green, blue)) > 20

  def main(args: Array[String]): Unit =
    require(args.length == 1 || args.length == 2,
      "Supply a fresh output directory and optional tetra, tetra-dorsal, or planar case")
    val output = Path.of(args(0)).toAbsolutePath
    require(!Files.exists(output), s"Refusing to overwrite $output")
    val requested = args.drop(1).headOption
    require(requested.forall(value => value == "tetra" || value == "tetra-dorsal" || value == "planar"),
      "Optional case must be tetra, tetra-dorsal, or planar")
    Files.createDirectories(output)
    Files.writeString(output.resolve("runtime.txt"),
      s"affineAtlas=${System.getProperty("scalafim.javafx.affineAtlas", "false")}\n" +
        ManagementFactory.getRuntimeMXBean.getInputArguments.toArray.mkString("arguments=", " ", "\n"))

    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      val stage = new Stage()
      var backend: Option[JavaFxSurfaceBackend] = None
      def fail(error: Throwable, renderer: JavaFxSurfaceBackend): Unit =
        failure = error
        try renderer.dispose().toOption.get
        finally
          stage.close()
          done.countDown()
      try
        val config = JavaFxAtlasConfig.Default
        require(config.encoding == JavaFxAtlasEncoding.LegacyTriangle)
        val allCases = Vector(
          ("tetra", tetrahedron(), false),
          ("tetra-dorsal", tetrahedron(), true),
          ("planar", planarFan(), true)
        )
        val cases = requested.fold(allCases)(value => allCases.filter(_._1 == value))
        val initialColors = Vector(
          Rgba32.unsafe(236, 67, 67),
          Rgba32.unsafe(54, 162, 235),
          Rgba32.unsafe(70, 196, 110),
          Rgba32.unsafe(247, 192, 62),
          Rgba32.unsafe(177, 83, 214)
        )
        val updatedColors = Vector(
          Rgba32.unsafe(31, 119, 180),
          Rgba32.unsafe(255, 127, 14),
          Rgba32.unsafe(44, 160, 44),
          Rgba32.unsafe(214, 39, 40),
          Rgba32.unsafe(148, 103, 189)
        )

        def runCase(index: Int): Unit =
          if index == cases.length then
            println(s"PASS: ${cases.length} four-face LegacyTriangle native sampler cases, initial/update/camera/material restoration")
            backend.foreach(_.dispose().toOption.get)
            stage.close()
            done.countDown()
          else
            val (label, geometry, dorsal) = cases(index)
            val first = plan(label, geometry, initialColors.take(geometry.vertexCount), dorsal)
            val second = recolor(first, updatedColors.take(geometry.vertexCount))
            val renderer = JavaFxSurfaceBackend.create(config).toOption.get
            backend = Some(renderer)
            val firstReceipt = renderer.render(first).toOption.get
            require(firstReceipt.dirty.geometry && firstReceipt.dirty.layerData)
            inspectNative(label, "initial", renderer, first, output)
            val scene = renderer.newSubScene(JavaFxSnapshotConfig.make(640, 480).toOption.get).toOption.get
            stage.setScene(new Scene(new Group(scene), 640, 480))
            stage.show()
            afterPulses(3): () =>
              try
                val initial = capture(scene, output.resolve(s"$label-initial.png"))
                require(initial.nonBackgroundPixels > 100,
                  s"$label initial four-face snapshot has only ${initial.nonBackgroundPixels} nonbackground pixels")
                require(initial.chromaticPixels > 100,
                  s"$label initial four-face snapshot has only ${initial.chromaticPixels} chromatic pixels")
                val updateReceipt = renderer.render(second).toOption.get
                require(!updateReceipt.dirty.geometry && updateReceipt.atlasUpdates == 1,
                  s"$label retained update did not update exactly one atlas: $updateReceipt")
                inspectNative(label, "updated", renderer, second, output)
                afterPulses(3): () =>
                  try
                    val updated = capture(scene, output.resolve(s"$label-updated.png"))
                    require(updated.nonBackgroundPixels > 100,
                      s"$label retained-update snapshot has only ${updated.nonBackgroundPixels} nonbackground pixels")
                    require(updated.chromaticPixels > 100,
                      s"$label retained-update snapshot has only ${updated.chromaticPixels} chromatic pixels")
                    require(!java.util.Arrays.equals(initial.pixels, updated.pixels),
                      s"$label retained-update snapshot did not change pixels")
                    val cameraPlan = cameraShift(second)
                    val cameraReceipt = renderer.render(cameraPlan).toOption.get
                    require(cameraReceipt.dirty.camera && !cameraReceipt.dirty.layerData && cameraReceipt.atlasUpdates == 0,
                      s"$label camera-only update altered atlas state: $cameraReceipt")
                    afterPulses(3): () =>
                      try
                        val camera = capture(scene, output.resolve(s"$label-camera.png"))
                        require(camera.nonBackgroundPixels > 100,
                          s"$label camera-only snapshot has only ${camera.nonBackgroundPixels} nonbackground pixels")
                        require(camera.chromaticPixels > 100,
                          s"$label camera-only snapshot lost chromatic atlas pixels")
                        val original = renderer.chunks match
                          case Vector(value) => value
                          case other => throw new IllegalStateException(s"$label/material expected one native chunk, got ${other.size}")
                        val mesh = original.mesh
                        val atlas = original.atlas
                        val points = mesh.getPoints.toArray(null)
                        val normals = mesh.getNormals.toArray(null)
                        val faces = mesh.getFaces.toArray(null)
                        val texCoords = mesh.getTexCoords.toArray(null)
                        val litReceipt = renderer.render(cameraPlan.copy(lighting = SurfaceLighting.Default)).toOption.get
                        require(!litReceipt.dirty.geometry && litReceipt.geometryUpdates == 0,
                          s"$label NativePhong transition rebuilt geometry: $litReceipt")
                        afterPulses(3): () =>
                          try
                            val restoredReceipt = renderer.render(cameraPlan).toOption.get
                            require(!restoredReceipt.dirty.geometry && restoredReceipt.geometryUpdates == 0,
                              s"$label Unlit restoration rebuilt geometry: $restoredReceipt")
                            afterPulses(3): () =>
                              try
                                val restored = capture(scene, output.resolve(s"$label-material-restored.png"))
                                require(restored.nonBackgroundPixels > 100,
                                  s"$label restored Unlit snapshot has only ${restored.nonBackgroundPixels} nonbackground pixels")
                                require(restored.chromaticPixels > 100,
                                  s"$label restored Unlit snapshot has only ${restored.chromaticPixels} chromatic pixels")
                                val restoredChunk = renderer.chunks match
                                  case Vector(value) => value
                                  case other => throw new IllegalStateException(s"$label/material restoration expected one native chunk, got ${other.size}")
                                require(restoredChunk.mesh eq mesh, s"$label Unlit restoration replaced the mesh")
                                require(restoredChunk.atlas eq atlas, s"$label Unlit restoration replaced the atlas")
                                require(java.util.Arrays.equals(mesh.getPoints.toArray(null), points), s"$label Unlit restoration changed points")
                                require(java.util.Arrays.equals(mesh.getNormals.toArray(null), normals), s"$label Unlit restoration changed normals")
                                require(java.util.Arrays.equals(mesh.getFaces.toArray(null), faces), s"$label Unlit restoration changed faces")
                                require(java.util.Arrays.equals(mesh.getTexCoords.toArray(null), texCoords), s"$label Unlit restoration changed texture coordinates")
                                inspectNative(label, "material-restored", renderer, cameraPlan, output)
                                renderer.dispose().toOption.get
                                backend = None
                                stage.setScene(null)
                                runCase(index + 1)
                              catch case error: Throwable => fail(error, renderer)
                          catch case error: Throwable => fail(error, renderer)
                      catch case error: Throwable => fail(error, renderer)
                  catch case error: Throwable => fail(error, renderer)
              catch case error: Throwable => fail(error, renderer)
        runCase(0)
      catch case error: Throwable =>
        failure = error
        stage.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def afterPulses(count: Int)(body: () => Unit): Unit =
    val timer = new AnimationTimer:
      private var pulses = 0
      override def handle(now: Long): Unit =
        pulses += 1
        if pulses == count then
          stop()
          body()
    timer.start()

  private def capture(scene: _root_.javafx.scene.SubScene, path: Path): Capture =
    val image = scene.snapshot(null, null)
    val width = image.getWidth.toInt
    val height = image.getHeight.toInt
    val pixels = new Array[Int](width * height)
    image.getPixelReader.getPixels(0, 0, width, height, PixelFormat.getIntArgbInstance(), pixels, 0, width)
    val png = new java.awt.image.BufferedImage(width, height, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    png.setRGB(0, 0, width, height, pixels, 0, width)
    require(javax.imageio.ImageIO.write(png, "png", path.toFile))
    Capture(width, height, pixels)

  private def inspectNative(label: String, phase: String, backend: JavaFxSurfaceBackend,
      render: SurfaceRenderPlan, output: Path): Unit =
    val chunk = backend.chunks match
      case Vector(value) => value
      case other => throw new IllegalStateException(s"$label/$phase expected one native chunk, got ${other.size}")
    require(chunk.faceCount == 4 && chunk.atlas.width == 16 && chunk.atlas.height == 4,
      s"$label/$phase expected a four-tile 16x4 LegacyTriangle atlas")
    require(chunk.material.getDiffuseMap == null && (chunk.material.getSelfIlluminationMap eq chunk.atlas.image),
      s"$label/$phase unlit material does not retain its atlas self-illumination map")
    require(chunk.view.isVisible && chunk.view.getOpacity == 1.0)
    requireSourceBuffers(label, phase, chunk.mesh, render)
    val coordinates = chunk.mesh.getTexCoords.toArray(null)
    require(coordinates.length == 24 && coordinates.forall(value => java.lang.Float.isFinite(value) && value >= 0f && value <= 1f),
      s"$label/$phase texture coordinates are not finite unit coordinates")
    val points = chunk.mesh.getPoints.toArray(null)
    val normals = chunk.mesh.getNormals.toArray(null)
    val faces = chunk.mesh.getFaces.toArray(null)
    require(faces.length == 36, s"$label/$phase did not retain four POINT_NORMAL_TEXCOORD faces")
    var corner = 0
    while corner < 12 do
      require(faces(corner * 3 + 2) == corner,
        s"$label/$phase face texture index ${faces(corner * 3 + 2)} does not identify corner $corner")
      corner += 1
    val image = chunk.atlas.image
    val coloredAtlasPixels = (0 until image.getWidth.toInt).flatMap(x =>
      0 until image.getHeight.toInt map (y => image.getPixelReader.getArgb(x, y)))
      .count: value =>
        val red = (value >>> 16) & 0xff
        val green = (value >>> 8) & 0xff
        val blue = value & 0xff
        ((value >>> 24) & 0xff) == 0xff && math.max(red, math.max(green, blue)) - math.min(red, math.min(green, blue)) > 20
    require(coloredAtlasPixels > 20, s"$label/$phase native atlas has only $coloredAtlasPixels chromatic pixels")
    Files.writeString(output.resolve(s"$label-$phase.txt"),
      s"atlas=${image.getWidth}x${image.getHeight} chromaticAtlasPixels=$coloredAtlasPixels " +
        s"sourceIndices=${render.meshes.head.indices.unsafeArray.mkString(",")} faces=${faces.mkString(",")} " +
        s"points=${points.mkString(",")} normals=${normals.mkString(",")} texCoords=${coordinates.mkString(",")} " +
        s"viewMatrix=${render.camera.viewMatrix.unsafeArray.mkString(",")} " +
        s"projectionMatrix=${render.camera.projectionMatrix.unsafeArray.mkString(",")}\n")

  private def requireSourceBuffers(label: String, phase: String,
      mesh: _root_.javafx.scene.shape.TriangleMesh, render: SurfaceRenderPlan): Unit =
    val packet = render.meshes match
      case Vector(value) => value
      case other => throw new IllegalStateException(s"$label/$phase expected one source mesh, got ${other.size}")
    val points = mesh.getPoints.toArray(null)
    val normals = mesh.getNormals.toArray(null)
    require(java.util.Arrays.equals(points, packet.positions.unsafeArray),
      s"$label/$phase native points differ from source buffer")
    require(java.util.Arrays.equals(normals, packet.normals.unsafeArray),
      s"$label/$phase native normals differ from source buffer")
    require(normals.grouped(3).forall(normal => normal.map(value => value.toDouble * value).sum > 1e-12),
      s"$label/$phase has a zero normal")

  private def plan(label: String, geometry: SurfaceGeometry, colors: Vector[Rgba32], dorsal: Boolean): SurfaceRenderPlan =
    val id = SurfaceId.unsafe(s"four-face-$label")
    val layer = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe(s"$label-colors"), id, geometry, colors).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, geometry).toOption.get), Vector(layer)).toOption.get
    val actions = Vector(SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(1.25)))) ++
      Option.when(dorsal)(SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)).toVector ++
      Vector(SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit), SurfaceViewerAction.FitCamera)
    val state = actions.foldLeft(SurfaceViewerState.initial(model)): (current, action) =>
      SurfaceViewer.reduce(model, current, action).toOption.get
    SurfaceCompiler.compile(model, state).toOption.get

  private def recolor(plan: SurfaceRenderPlan, colors: Vector[Rgba32]): SurfaceRenderPlan =
    val packed = new IntBufferView(colors.map(_.toPackedInt).toArray)
    plan.copy(layers = plan.layers.map(_.copy(colors = packed)))

  private def cameraShift(plan: SurfaceRenderPlan): SurfaceRenderPlan =
    val view = plan.camera.viewMatrix.unsafeArray.clone()
    view(3) += 0.03f
    plan.copy(camera = plan.camera.copy(viewMatrix = new FloatBufferView(view)))

  private def tetrahedron(): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(0.0, 0.0, 0.0), Seq(1.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0), Seq(0.0, 0.0, 1.0)),
      Seq((0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2))
    ), Hemisphere.Left, SurfaceKind.Pial)

  private def planarFan(): SurfaceGeometry =
    SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(0.0, 0.0, 0.0), Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0),
        Seq(1.0, 1.0, 0.0), Seq(-1.0, 1.0, 0.0)),
      Seq((0, 1, 2), (0, 2, 3), (0, 3, 4), (0, 4, 1))
    ), Hemisphere.Left, SurfaceKind.Pial)
