package scalafim.surface.view.javafx

import java.awt.{Color, Font, RenderingHints}
import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.util.concurrent.CountDownLatch
import javax.imageio.ImageIO
import javafx.application.Platform
import javafx.scene.{Group, Scene, SubScene}
import javafx.scene.image.WritableImage
import javafx.stage.Stage

import scalafim.examples.surfaceview.*
import intaglio.*
import scalafim.surface.*
import scalafim.surface.io.GiftiSurfaceReader
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Produces the native half of the four-view visual QA plate. This remains an
  * opt-in probe because JavaFX Scene3D requires a live display.
  */
object JavaFxSurfaceThresholdParityProbe:
  def main(args: Array[String]): Unit =
    val output = args.headOption.map(Path.of(_)).getOrElse(
      Path.of("/private/tmp/scalafim-surface-threshold-javafx.png")
    )
    val corpusRoot = args.drop(1).headOption.map(Path.of(_))
      .orElse(sys.env.get("SCALAFIM_SURFACE_CORPUS").map(Path.of(_)))
      .getOrElse(Path.of(
        System.getProperty("user.home"), "code", "jscode", "surfviewjs", "tests", "data"
      ))
    val geometry = read(corpusRoot, CorticalSurfaceAcceptance.LeftCorpus)
    val fixture = SurfaceThresholdParityFixture.build(geometry)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val dimensions = SurfaceThresholdParityFixture.Dimensions
    val config = JavaFxSnapshotConfig.make(dimensions.width, dimensions.height).toOption.get

    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      var stage: Stage | Null = null
      var subScene: SubScene | Null = null
      var backend: JavaFxSurfaceBackend | Null = null
      try
        backend = JavaFxSurfaceBackend.create().toOption.get
        val panels = Vector.newBuilder[(String, BufferedImage)]
        val rows = Vector.newBuilder[String]
        var index = 0
        while index < fixture.views.length do
          val view = fixture.views(index)
          val admission = if index == 0 then SurfaceAdmissionPath.ColdLoad else SurfaceAdmissionPath.CameraOnly
          val rendered = backend.nn.renderObserved(view.plan, admission).toOption.get
          val admissionViolations = SurfaceBackendAdmission.validate(rendered.observation).map(_.problem)
          require(admissionViolations.isEmpty, s"${view.label}: ${admissionViolations.mkString("; ")}")
          if index > 0 then
            require(rendered.observation.geometryUploads == 0,
              s"${view.label}: camera-only render uploaded geometry")
            require(rendered.observation.layerUploads == 0,
              s"${view.label}: camera-only render uploaded layers")
          if stage == null then
            subScene = backend.nn.newSubScene(config).toOption.get
            stage = new Stage()
            stage.nn.setScene(new Scene(new Group(subScene.nn), config.width.toDouble, config.height.toDouble))
            stage.nn.show()
          val snapshot = new WritableImage(config.width, config.height)
          subScene.nn.snapshot(null, snapshot)
          val observed = rasterImage(snapshot, dimensions)
          val reference = SurfaceThresholdParityFixture.reference(view)
          val visual = SurfaceVisualQa.compare(reference.image, observed).toOption.get
          val orientation = SurfaceThresholdParityFixture.orientationQa(reference.image, observed)
          val visualViolations = singleSurfaceViolations(visual, orientation)
          require(visualViolations.isEmpty, s"${view.label}: ${visualViolations.mkString("; ")}")
          panels += view.label -> buffered(observed)
          rows += row(view, rendered, visual, orientation)
          index += 1
        writePlate(output, panels.result(), fixture)
        println(s"{" +
          s"\"schema\":\"scalafim.surface-threshold-parity.v1\"," +
          s"\"backend\":\"javafx-scene3d\"," +
          s"\"source\":\"${CorticalSurfaceAcceptance.LeftCorpus.fileName}\"," +
          s"\"sourceSha256\":\"${CorticalSurfaceAcceptance.LeftCorpus.sha256}\"," +
          s"\"seed\":${SurfaceThresholdParityFixture.Seed}," +
          s"\"smoothingIterations\":${SurfaceThresholdParityFixture.SmoothingIterations}," +
          f"\"thresholdLower\":${-SurfaceThresholdParityFixture.Threshold}%.2f," +
          f"\"thresholdUpper\":${SurfaceThresholdParityFixture.Threshold}%.2f," +
          s"\"visibleNegativeVertices\":${fixture.visibleNegativeVertices}," +
          s"\"visiblePositiveVertices\":${fixture.visiblePositiveVertices}," +
          s"\"output\":\"${json(output.toString)}\"," +
          s"\"cases\":[${rows.result().mkString(",")}]}")
      catch case error: Throwable => failure = error
      finally
        if backend != null then backend.nn.dispose()
        if stage != null then stage.nn.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def read(root: Path, entry: CorticalSurfaceCorpusEntry): SurfaceGeometry =
    val path = root.resolve(entry.fileName)
    require(Files.isRegularFile(path), s"missing cortical corpus file: $path")
    val digest = MessageDigest.getInstance("SHA-256").digest(Files.readAllBytes(path))
      .map(byte => f"${byte & 0xff}%02x").mkString
    require(digest == entry.sha256, s"SHA-256 mismatch for $path: expected ${entry.sha256}, got $digest")
    GiftiSurfaceReader.read(path, entry.hemisphere, SurfaceKind.Pial)

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

  private def buffered(image: RasterImage): BufferedImage =
    val result = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_ARGB)
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        val pixel = image.pixelUnsafe(x, y)
        result.setRGB(x, y, (pixel.alpha << 24) | (pixel.red << 16) | (pixel.green << 8) | pixel.blue)
        x += 1
      y += 1
    result

  private def writePlate(
    output: Path,
    panels: Vector[(String, BufferedImage)],
    fixture: SurfaceThresholdParity
  ): Unit =
    val panelWidth = SurfaceThresholdParityFixture.Dimensions.width
    val panelHeight = SurfaceThresholdParityFixture.Dimensions.height
    val gap = 14
    val header = 76
    val caption = 32
    val plate = new BufferedImage(
      panelWidth * 2 + gap,
      header + (panelHeight + caption) * 2 + gap,
      BufferedImage.TYPE_INT_ARGB
    )
    val graphics = plate.createGraphics()
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    graphics.setColor(new Color(12, 17, 24))
    graphics.fillRect(0, 0, plate.getWidth, plate.getHeight)
    graphics.setColor(new Color(237, 242, 247))
    graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 24))
    graphics.drawString("Thresholded cortical overlay — JavaFX Scene3D", 10, 29)
    graphics.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 14))
    graphics.setColor(new Color(169, 182, 200))
    graphics.drawString(
      s"fixed seed ${SurfaceThresholdParityFixture.Seed} · z ≤ −2.33: ${fixture.visibleNegativeVertices} vertices · " +
        s"z ≥ 2.33: ${fixture.visiblePositiveVertices} vertices · gray = fold underlay",
      10,
      56
    )
    panels.zipWithIndex.foreach: entry =>
      val ((label, image), index) = entry
      val column = index % 2
      val row = index / 2
      val x = column * (panelWidth + gap)
      val y = header + row * (panelHeight + caption + gap)
      graphics.setColor(new Color(21, 29, 40))
      graphics.fillRect(x, y, panelWidth, caption)
      graphics.setColor(new Color(237, 242, 247))
      graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 14))
      val captionLabel = if label == "Lateral" then "LATERAL · LEFT" else if label == "Top" then "TOP · DORSAL" else label.toUpperCase
      graphics.drawString(captionLabel, x + 11, y + 21)
      graphics.drawImage(image, x, y + caption, null)
    graphics.dispose()
    Option(output.getParent).foreach(Files.createDirectories(_))
    ImageIO.write(plate, "png", output.toFile)

  private def singleSurfaceViolations(
    receipt: SurfaceVisualQaReceipt,
    orientation: SurfaceOrientationQaReceipt
  ): Vector[String] =
    val failures = Vector.newBuilder[String]
    if receipt.expectedForegroundPixels < 8000 then failures += "reference fixture does not cover enough pixels"
    if receipt.observedForegroundPixels < 8000 then failures += "native rendering does not cover enough pixels"
    if receipt.maskIntersectionOverUnion < 0.90 then failures += f"mask IoU ${receipt.maskIntersectionOverUnion}%.6f < 0.900000"
    if receipt.centroidDistancePixels > 3.0 then failures += f"centroid distance ${receipt.centroidDistancePixels}%.6f > 3.000000"
    if receipt.interiorPixelsCompared == 0 then failures += "no interior pixels available for color comparison"
    else if receipt.meanInteriorChannelError > 40.0 then
      failures += f"mean interior channel error ${receipt.meanInteriorChannelError}%.6f > 40.000000"
    if orientation.margin <= 3.0 then
      failures += f"direct orientation beats its closest flip by only ${orientation.margin}%.6f channels"
    failures.result()

  private def row(
    view: SurfaceThresholdView,
    render: JavaFxObservedReceipt,
    visual: SurfaceVisualQaReceipt,
    orientation: SurfaceOrientationQaReceipt
  ): String =
    s"{" +
      s"\"label\":\"${view.label}\"," +
      s"\"cameraDirection\":[${view.plan.camera.directionX},${view.plan.camera.directionY},${view.plan.camera.directionZ}]," +
      s"\"geometryUploads\":${render.observation.geometryUploads}," +
      s"\"layerUploads\":${render.observation.layerUploads}," +
      f"\"elapsedMillis\":${render.native.elapsedNanos.toDouble / 1e6}%.6f," +
      s"\"expectedForegroundPixels\":${visual.expectedForegroundPixels}," +
      s"\"observedForegroundPixels\":${visual.observedForegroundPixels}," +
      f"\"maskIntersectionOverUnion\":${visual.maskIntersectionOverUnion}%.9f," +
      f"\"centroidDistancePixels\":${visual.centroidDistancePixels}%.9f," +
      f"\"meanInteriorChannelError\":${visual.meanInteriorChannelError}%.9f," +
      f"\"horizontalFlipMeanChannelError\":${orientation.horizontalFlipMeanChannelError}%.9f," +
      f"\"verticalFlipMeanChannelError\":${orientation.verticalFlipMeanChannelError}%.9f," +
      f"\"rotation180MeanChannelError\":${orientation.rotation180MeanChannelError}%.9f," +
      f"\"orientationMargin\":${orientation.margin}%.9f}"

  private def json(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")
