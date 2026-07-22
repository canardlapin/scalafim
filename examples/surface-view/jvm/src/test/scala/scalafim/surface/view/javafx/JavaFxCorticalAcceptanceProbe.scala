package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.CountDownLatch
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import javafx.application.Platform
import javafx.geometry.{Point2D, Point3D}
import javafx.scene.{Group, Scene}
import javafx.scene.image.WritableImage
import javafx.scene.input.PickResult
import javafx.stage.Stage

import scalafim.examples.surfaceview.*
import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Live, opt-in production-scale acceptance gate. The fsaverage5 corpus stays
  * external until its data-specific redistribution terms are documented.
  */
object JavaFxCorticalAcceptanceProbe:
  def main(args: Array[String]): Unit =
    val corpusRoot = args.headOption.map(Path.of(_))
      .orElse(sys.env.get("SCALAFIM_SURFACE_CORPUS").map(Path.of(_)))
      .getOrElse(Path.of(
        System.getProperty("user.home"),
        "code",
        "jscode",
        "surfviewjs",
        "tests",
        "data"
      ))
    val loadStarted = System.nanoTime()
    val left = read(corpusRoot, CorticalSurfaceAcceptance.LeftCorpus)
    val right = read(corpusRoot, CorticalSurfaceAcceptance.RightCorpus)
    val loadNanos = System.nanoTime() - loadStarted
    val compileStarted = System.nanoTime()
    val example = CorticalSurfaceAcceptance.build(left, right)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val compileNanos = System.nanoTime() - compileStarted
    val referenceStarted = System.nanoTime()
    val reference = CorticalSurfaceAcceptance.reference(example)
    val referenceNanos = System.nanoTime() - referenceStarted
    val referenceQa = SurfaceVisualQa.compare(reference.image, reference.image).toOption.get
    require(
      referenceQa.violations(CorticalSurfaceAcceptance.Policy).isEmpty,
      s"invalid cortical reference coverage: foreground=${referenceQa.expectedForegroundPixels} " +
        s"halves=${referenceQa.expectedLeftForegroundPixels},${referenceQa.expectedRightForegroundPixels}"
    )
    val (pickX, pickY, referencePick) = CorticalSurfaceAcceptance.landmark(reference)
    requireFlipSentinels(reference.image)

    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      var stage: Stage | Null = null
      var backend: JavaFxSurfaceBackend | Null = null
      var controller: JavaFxSurfaceController | Null = null
      try
        backend = JavaFxSurfaceBackend.create().toOption.get
        val cold = backend.nn.renderObserved(example.plan, SurfaceAdmissionPath.ColdLoad).toOption.get
        val config = JavaFxSnapshotConfig.make(
          CorticalSurfaceAcceptance.Dimensions.width,
          CorticalSurfaceAcceptance.Dimensions.height
        ).toOption.get
        val subScene = backend.nn.newSubScene(config).toOption.get
        stage = new Stage()
        stage.nn.setScene(new Scene(
          new Group(subScene),
          config.width.toDouble,
          config.height.toDouble
        ))
        stage.nn.show()
        val snapshotStarted = System.nanoTime()
        val snapshot = new WritableImage(config.width, config.height)
        subScene.snapshot(null, snapshot)
        val snapshotNanos = System.nanoTime() - snapshotStarted
        val observed = rasterImage(snapshot, CorticalSurfaceAcceptance.Dimensions)
        val visualQa = SurfaceVisualQa.compare(reference.image, observed).toOption.get
        val visualViolations = visualQa.violations(CorticalSurfaceAcceptance.Policy)
        if visualViolations.nonEmpty then writeDebug(reference.image, observed)

        controller = JavaFxSurfaceController.attach(
          example.model,
          example.state,
          backend.nn,
          subScene
        ).toOption.get
        val nativePick = controller.nn.pick(pickResult(example, backend.nn, referencePick)).toOption.get
        require(nativePick.surface == referencePick.surface,
          s"JavaFX picked ${nativePick.surface.value}, expected ${referencePick.surface.value}")
        require(nativePick.face.index == referencePick.face,
          s"JavaFX face ${nativePick.face.index}, expected ${referencePick.face}")
        require(nativePick.vertex.index == referencePick.vertex,
          s"JavaFX vertex ${nativePick.vertex.index}, expected ${referencePick.vertex}")

        val cameraPlan = CorticalSurfaceAcceptance.cameraOnlyPlan(example).toOption.get
        val camera = backend.nn.renderObserved(cameraPlan, SurfaceAdmissionPath.CameraOnly).toOption.get
        val cameraViolations = SurfaceBackendAdmission.validate(camera.observation)
        require(cameraViolations.isEmpty, cameraViolations.map(_.problem).mkString("; "))
        require(
          visualViolations.isEmpty,
          s"JavaFX cortical visual parity failed: ${visualViolations.mkString("; ")}; " +
            s"expectedForeground=${visualQa.expectedForegroundPixels} " +
            s"observedForeground=${visualQa.observedForegroundPixels} " +
            s"expectedHalves=${visualQa.expectedLeftForegroundPixels},${visualQa.expectedRightForegroundPixels} " +
            s"observedHalves=${visualQa.observedLeftForegroundPixels},${visualQa.observedRightForegroundPixels}"
        )

        println(canonicalJson(
          loadNanos,
          compileNanos,
          referenceNanos,
          snapshotNanos,
          cold,
          camera,
          visualQa,
          pickX,
          pickY,
          nativePick
        ))
      catch case error: Throwable => failure = error
      finally
        if controller != null then controller.nn.dispose()
        if backend != null then backend.nn.dispose()
        if stage != null then stage.nn.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def read(root: Path, entry: CorticalSurfaceCorpusEntry): SurfaceGeometry =
    val path = root.resolve(entry.fileName)
    require(Files.isRegularFile(path), s"missing cortical acceptance corpus file: $path")
    val actual = sha256(Files.readAllBytes(path))
    require(actual == entry.sha256,
      s"SHA-256 mismatch for $path: expected ${entry.sha256}, got $actual")
    val geometry = GiftiSurfaceReader.read(path, entry.hemisphere, SurfaceKind.Pial)
    require(geometry.vertexCount == entry.vertices,
      s"${entry.fileName} has ${geometry.vertexCount} vertices, expected ${entry.vertices}")
    require(geometry.faceCount == entry.faces,
      s"${entry.fileName} has ${geometry.faceCount} faces, expected ${entry.faces}")
    geometry

  private def sha256(bytes: Array[Byte]): String =
    MessageDigest.getInstance("SHA-256").digest(bytes).map(byte => f"${byte & 0xff}%02x").mkString

  private def rasterImage(actual: WritableImage, dimensions: RasterDimensions): RasterImage =
    val reader = actual.getPixelReader
    RasterImage.tabulate(dimensions): (x, y) =>
      val argb = reader.getArgb(x, y)
      Rgba32.unsafe(
        (argb >>> 16) & 0xff,
        (argb >>> 8) & 0xff,
        argb & 0xff,
        (argb >>> 24) & 0xff
      )

  private def requireFlipSentinels(reference: RasterImage): Unit =
    val dimensions = reference.dimensions
    val horizontal = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(dimensions.width - 1 - x, y)
    val vertical = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(x, dimensions.height - 1 - y)
    val horizontalQa = SurfaceVisualQa.compare(reference, horizontal).toOption.get
    val verticalQa = SurfaceVisualQa.compare(reference, vertical).toOption.get
    if verticalQa.violations(CorticalSurfaceAcceptance.Policy).isEmpty then
      writeDebug(reference, reference)
    require(horizontalQa.violations(CorticalSurfaceAcceptance.Policy).nonEmpty,
      "production cortical fixture cannot detect a horizontal flip")
    require(
      verticalQa.violations(CorticalSurfaceAcceptance.Policy).nonEmpty,
      f"production cortical fixture cannot detect a vertical flip: " +
        f"iou=${verticalQa.maskIntersectionOverUnion}%.6f " +
        f"centroid=${verticalQa.centroidDistancePixels}%.6f " +
        f"meanChannel=${verticalQa.meanInteriorChannelError}%.6f"
    )

  private def writeDebug(reference: RasterImage, observed: RasterImage): Unit =
    val root = Path.of(System.getProperty("java.io.tmpdir"), "scalafim-surface-cortical-debug")
    Files.createDirectories(root)
    writePng(reference, root.resolve("reference.png"))
    writePng(observed, root.resolve("javafx.png"))

  private def writePng(image: RasterImage, path: Path): Unit =
    val buffered = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_ARGB)
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        val pixel = image.pixelUnsafe(x, y)
        val argb = (pixel.alpha << 24) | (pixel.red << 16) | (pixel.green << 8) | pixel.blue
        buffered.setRGB(x, y, argb)
        x += 1
      y += 1
    ImageIO.write(buffered, "png", path.toFile)

  private def pickResult(
    example: SurfaceViewerExample,
    backend: JavaFxSurfaceBackend,
    reference: SurfacePick
  ): PickResult =
    val packet = example.plan.meshes.find(_.surface == reference.surface).get
    val offset = reference.face * 3
    val a = packet.indices(offset)
    val b = packet.indices(offset + 1)
    val c = packet.indices(offset + 2)
    def coordinate(axis: Int): Double =
      packet.positions(a * 3 + axis) * reference.barycentricA +
        packet.positions(b * 3 + axis) * reference.barycentricB +
        packet.positions(c * 3 + axis) * reference.barycentricC
    val chunk = backend.chunks.find: candidate =>
      candidate.surface == reference.surface &&
        reference.face >= candidate.faceStart &&
        reference.face < candidate.faceStart + candidate.faceCount
    .get
    new PickResult(
      chunk.view,
      new Point3D(coordinate(0), coordinate(1), coordinate(2)),
      reference.depth,
      reference.face - chunk.faceStart,
      Point2D.ZERO
    )

  private def canonicalJson(
    loadNanos: Long,
    compileNanos: Long,
    referenceNanos: Long,
    snapshotNanos: Long,
    cold: JavaFxObservedReceipt,
    camera: JavaFxObservedReceipt,
    visual: SurfaceVisualQaReceipt,
    pickX: Int,
    pickY: Int,
    pick: JavaFxSurfacePick
  ): String =
    s"{" +
      s"\"schema\":\"scalafim.surface-cortical-acceptance.v1\"," +
      s"\"backend\":\"javafx-scene3d\"," +
      s"\"leftSha256\":\"${CorticalSurfaceAcceptance.LeftCorpus.sha256}\"," +
      s"\"rightSha256\":\"${CorticalSurfaceAcceptance.RightCorpus.sha256}\"," +
      s"\"verticesPerHemisphere\":${CorticalSurfaceAcceptance.LeftCorpus.vertices}," +
      s"\"facesPerHemisphere\":${CorticalSurfaceAcceptance.LeftCorpus.faces}," +
      f"\"loadMillis\":${loadNanos.toDouble / 1e6}%.6f," +
      f"\"compileMillis\":${compileNanos.toDouble / 1e6}%.6f," +
      f"\"referenceRasterMillis\":${referenceNanos.toDouble / 1e6}%.6f," +
      f"\"coldRenderMillis\":${cold.native.elapsedNanos.toDouble / 1e6}%.6f," +
      f"\"snapshotMillis\":${snapshotNanos.toDouble / 1e6}%.6f," +
      f"\"cameraRenderMillis\":${camera.native.elapsedNanos.toDouble / 1e6}%.6f," +
      s"\"geometryUploads\":${cold.observation.geometryUploads}," +
      s"\"layerUploads\":${cold.observation.layerUploads}," +
      s"\"cameraGeometryUploads\":${camera.observation.geometryUploads}," +
      s"\"cameraLayerUploads\":${camera.observation.layerUploads}," +
      s"\"expectedForegroundPixels\":${visual.expectedForegroundPixels}," +
      s"\"observedForegroundPixels\":${visual.observedForegroundPixels}," +
      f"\"maskIntersectionOverUnion\":${visual.maskIntersectionOverUnion}%.9f," +
      f"\"centroidDistancePixels\":${visual.centroidDistancePixels}%.9f," +
      f"\"meanInteriorChannelError\":${visual.meanInteriorChannelError}%.9f," +
      s"\"landmarkX\":$pickX,\"landmarkY\":$pickY," +
      s"\"pickedSurface\":\"${pick.surface.value}\"," +
      s"\"pickedFace\":${pick.face.index},\"pickedVertex\":${pick.vertex.index}" +
      s"}"
