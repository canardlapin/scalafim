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

/** Native differential gate and presentation plate for the geodesic cortical
  * reveal lens. It is opt-in because JavaFX Scene3D requires a live display.
  */
object JavaFxCorticalLensProbe:
  def main(args: Array[String]): Unit =
    val output = args.headOption.map(Path.of(_)).getOrElse(
      Path.of("/private/tmp/scalafim-surface-lens-javafx.png")
    )
    val corpusRoot = args.drop(1).headOption.map(Path.of(_))
      .orElse(sys.env.get("SCALAFIM_SURFACE_CORPUS").map(Path.of(_)))
      .getOrElse(Path.of(
        System.getProperty("user.home"),
        "code",
        "jscode",
        "FROIAtlas",
        "app",
        "public",
        "data",
        "surfaces"
      ))
    val pialEntry = corpusEntry(SurfaceKind.Pial)
    val inflatedEntry = corpusEntry(SurfaceKind.Inflated)
    val pial = read(corpusRoot, pialEntry)
    val inflated = read(corpusRoot, inflatedEntry)
    val buildStarted = System.nanoTime()
    val example = CorticalSurfaceLensAcceptance.build(pial, inflated)
      .fold(error => throw new IllegalStateException(error.message), identity)
    require(example.quality.invertedTriangles == 0,
      s"natural lens inverted ${example.quality.invertedTriangles} triangles")
    require(example.quality.minimumAreaRatio > 0.0,
      s"natural lens collapsed a triangle: minimum area ratio ${example.quality.minimumAreaRatio}")
    val buildNanos = System.nanoTime() - buildStarted
    val dimensions = CorticalSurfaceLensAcceptance.Dimensions
    val config = JavaFxSnapshotConfig.make(dimensions.width, dimensions.height).toOption.get

    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    Platform.startup: () =>
      var stage: Stage | Null = null
      var subScene: SubScene | Null = null
      var backend: JavaFxSurfaceBackend | Null = null
      try
        backend = JavaFxSurfaceBackend.create().toOption.get
        val panels = Vector.newBuilder[(CorticalLensCase, BufferedImage)]
        val rows = Vector.newBuilder[String]
        val first = example.cases.head.plan
        val stableMeshKeys = first.receipt.meshKeys
        val stableLayerKeys = first.receipt.layerKeys
        val stableCameraKey = first.receipt.cameraKey
        var index = 0
        while index < example.cases.length do
          val current = example.cases(index)
          require(current.plan.receipt.meshKeys == stableMeshKeys,
            s"${current.label}: topology/resource identity drifted")
          require(current.plan.receipt.layerKeys == stableLayerKeys,
            s"${current.label}: overlay identity drifted")
          require(current.plan.receipt.cameraKey == stableCameraKey,
            s"${current.label}: camera identity drifted")

          val rendered = backend.nn.render(current.plan).toOption.get
          require(rendered.atlasUpdates == 0,
            s"${current.label}: lens animation unexpectedly updated ${rendered.atlasUpdates} atlases")
          if index == 0 then
            require(rendered.dirty.geometry && rendered.geometryUpdates == 0,
              s"${current.label}: cold load did not rebuild geometry exactly once")
          else
            require(rendered.dirty.geometry && rendered.geometryUpdates == 1,
              s"${current.label}: expected one in-place geometry update")
            require(!rendered.dirty.layerData,
              s"${current.label}: local geometry animation dirtied layer data")
            require(rendered.geometryBytesUpdated == current.plan.profile.verticesPacked.toLong * 6L * 4L,
              s"${current.label}: geometry byte accounting drifted")

          if stage == null then
            subScene = backend.nn.newSubScene(config).toOption.get
            stage = new Stage()
            stage.nn.setTitle("ScalaFIM geodesic cortical reveal lens")
            stage.nn.setScene(new Scene(
              new Group(subScene.nn),
              config.width.toDouble,
              config.height.toDouble
            ))
            stage.nn.show()
          val snapshot = new WritableImage(config.width, config.height)
          subScene.nn.snapshot(null, snapshot)
          val observed = rasterImage(snapshot, dimensions)
          val reference = CorticalSurfaceLensAcceptance.reference(current)
          val visual = SurfaceVisualQa.compare(reference.image, observed).toOption.get
          val orientation = SurfaceThresholdParityFixture.orientationQa(reference.image, observed)
          val violations = singleSurfaceViolations(visual, orientation)
          if violations.nonEmpty then writeDebug(current.label, reference.image, observed)
          require(violations.isEmpty, s"${current.label}: ${violations.mkString("; ")}")
          panels += current -> buffered(observed)
          rows += row(current, rendered, visual, orientation)
          index += 1

        writePlate(output, panels.result(), example)
        println(s"{" +
          s"\"schema\":\"scalafim.surface-cortical-lens.v1\"," +
          s"\"backend\":\"javafx-scene3d\"," +
          s"\"pialSha256\":\"${pialEntry.sha256}\"," +
          s"\"inflatedSha256\":\"${inflatedEntry.sha256}\"," +
          s"\"centerVertex\":${example.center.index}," +
          s"\"activeVertices\":${example.activeVertices}," +
          s"\"invertedTriangles\":${example.quality.invertedTriangles}," +
          f"\"minimumAreaRatio\":${example.quality.minimumAreaRatio}%.9f," +
          f"\"maximumAreaRatio\":${example.quality.maximumAreaRatio}%.9f," +
          f"\"maximumEdgeStrain\":${example.quality.maximumEdgeStrain}%.9f," +
          f"\"p95EdgeStrain\":${example.quality.p95EdgeStrain}%.9f," +
          f"\"innerRadiusMm\":${CorticalSurfaceLensAcceptance.InnerRadius.value}%.3f," +
          f"\"outerRadiusMm\":${CorticalSurfaceLensAcceptance.OuterRadius.value}%.3f," +
          s"\"visibleNegativeVertices\":${example.visibleNegativeVertices}," +
          s"\"visiblePositiveVertices\":${example.visiblePositiveVertices}," +
          f"\"lensBuildMillis\":${buildNanos.toDouble / 1e6}%.6f," +
          s"\"output\":\"${json(output.toString)}\"," +
          s"\"cases\":[${rows.result().mkString(",")}]}" )
      catch case error: Throwable => failure = error
      finally
        if backend != null then backend.nn.dispose()
        if stage != null then stage.nn.close()
        done.countDown()
    done.await()
    Platform.exit()
    if failure != null then throw failure.nn

  private def corpusEntry(kind: SurfaceKind): CorticalMorphCorpusEntry =
    CorticalSurfaceMorphAcceptance.Corpus.find(entry =>
      entry.hemisphere == Hemisphere.Left && entry.kind == kind
    ).getOrElse(throw new IllegalStateException(s"missing left ${kind.label} corpus entry"))

  private def read(root: Path, entry: CorticalMorphCorpusEntry): SurfaceGeometry =
    val path = root.resolve(entry.fileName)
    require(Files.isRegularFile(path), s"missing cortical corpus file: $path")
    val actual = sha256(Files.readAllBytes(path))
    require(actual == entry.sha256,
      s"SHA-256 mismatch for $path: expected ${entry.sha256}, got $actual")
    GiftiSurfaceReader.read(path, entry.hemisphere, entry.kind)

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

  private def buffered(image: RasterImage): BufferedImage =
    val result = new BufferedImage(image.width, image.height, BufferedImage.TYPE_INT_ARGB)
    var y = 0
    while y < image.height do
      var x = 0
      while x < image.width do
        val pixel = image.pixelUnsafe(x, y)
        result.setRGB(x, y,
          (pixel.alpha << 24) | (pixel.red << 16) | (pixel.green << 8) | pixel.blue)
        x += 1
      y += 1
    result

  private def writeDebug(label: String, reference: RasterImage, observed: RasterImage): Unit =
    val stem = label.toLowerCase.replace(' ', '-')
    ImageIO.write(buffered(reference), "png", Path.of(s"/private/tmp/lens-$stem-reference.png").toFile)
    ImageIO.write(buffered(observed), "png", Path.of(s"/private/tmp/lens-$stem-javafx.png").toFile)

  private def writePlate(
    output: Path,
    panels: Vector[(CorticalLensCase, BufferedImage)],
    example: CorticalSurfaceLensExample
  ): Unit =
    val panelWidth = CorticalSurfaceLensAcceptance.Dimensions.width
    val panelHeight = CorticalSurfaceLensAcceptance.Dimensions.height
    val gap = 14
    val header = 90
    val caption = 38
    val footer = 52
    val plate = new BufferedImage(
      panelWidth * 2 + gap,
      header + (panelHeight + caption) * 2 + gap + footer,
      BufferedImage.TYPE_INT_ARGB
    )
    val graphics = plate.createGraphics()
    graphics.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    graphics.setColor(new Color(8, 13, 20))
    graphics.fillRect(0, 0, plate.getWidth, plate.getHeight)
    graphics.setColor(new Color(116, 185, 255))
    graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 11))
    graphics.drawString("GEODESIC CORTICAL REVEAL LENS", 12, 20)
    graphics.setColor(new Color(238, 244, 251))
    graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 25))
    graphics.drawString("The fold opens; the scientific identity stays fixed.", 11, 50)
    graphics.setColor(new Color(170, 184, 202))
    graphics.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 14))
    graphics.drawString(
      s"pinned vertex ${example.center.index} · 12–48 mm geodesic falloff · ${example.activeVertices} moving vertices · threshold ±2.33",
      12,
      75
    )
    panels.zipWithIndex.foreach: entry =>
      val ((current, image), index) = entry
      val column = index % 2
      val row = index / 2
      val x = column * (panelWidth + gap)
      val y = header + row * (panelHeight + caption + gap)
      graphics.setColor(new Color(17, 26, 38))
      graphics.fillRect(x, y, panelWidth, caption)
      graphics.setColor(new Color(238, 244, 251))
      graphics.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 14))
      graphics.drawString(current.label.toUpperCase, x + 12, y + 24)
      graphics.setColor(new Color(127, 145, 168))
      graphics.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 13))
      graphics.drawString(f"local pial → inflated · ${current.fraction.value * 100.0}%.0f%%", x + 385, y + 24)
      graphics.drawImage(image, x, y + caption, null)
      drawGuide(graphics, x, y + caption, current.guide)
    val footerY = plate.getHeight - footer
    graphics.setColor(new Color(14, 23, 34))
    graphics.fillRect(0, footerY, plate.getWidth, footer)
    graphics.setColor(new Color(183, 197, 215))
    graphics.setFont(new Font(Font.SANS_SERIF, Font.PLAIN, 13))
    graphics.drawString(
      "Warm core · cool fixed boundary · pinned center. Overlay, threshold, topology, camera, and identity remain anchored.",
      13,
      footerY + 31
    )
    graphics.dispose()
    Option(output.getParent).foreach(Files.createDirectories(_))
    ImageIO.write(plate, "png", output.toFile)

  private def drawGuide(
    graphics: java.awt.Graphics2D,
    panelX: Int,
    panelY: Int,
    guide: CorticalLensGuide
  ): Unit =
    val centerX = panelX + guide.centerX
    val centerY = panelY + guide.centerY
    def ring(radius: Double, color: Color, width: Float): Unit =
      graphics.setColor(color)
      graphics.setStroke(new java.awt.BasicStroke(width, java.awt.BasicStroke.CAP_ROUND, java.awt.BasicStroke.JOIN_ROUND))
      graphics.drawOval(
        math.round(centerX - radius).toInt,
        math.round(centerY - radius).toInt,
        math.round(radius * 2.0).toInt,
        math.round(radius * 2.0).toInt
      )
    ring(guide.outerRadiusPixels, new Color(125, 197, 255, 105), 1.5f)
    ring(guide.innerRadiusPixels, new Color(255, 224, 137, 150), 1.5f)
    graphics.setColor(new Color(255, 239, 184, 230))
    graphics.fillOval(math.round(centerX - 3.5).toInt, math.round(centerY - 3.5).toInt, 7, 7)
    graphics.setColor(new Color(7, 14, 22, 220))
    graphics.fillOval(math.round(centerX - 1.5).toInt, math.round(centerY - 1.5).toInt, 3, 3)

  private def singleSurfaceViolations(
    receipt: SurfaceVisualQaReceipt,
    orientation: SurfaceOrientationQaReceipt
  ): Vector[String] =
    val failures = Vector.newBuilder[String]
    if receipt.expectedForegroundPixels < 8000 then failures += "reference fixture does not cover enough pixels"
    if receipt.observedForegroundPixels < 8000 then failures += "native rendering does not cover enough pixels"
    if receipt.maskIntersectionOverUnion < 0.90 then
      failures += f"mask IoU ${receipt.maskIntersectionOverUnion}%.6f < 0.900000"
    if receipt.centroidDistancePixels > 3.0 then
      failures += f"centroid distance ${receipt.centroidDistancePixels}%.6f > 3.000000"
    if receipt.interiorPixelsCompared == 0 then failures += "no interior pixels available for color comparison"
    else if receipt.meanInteriorChannelError > 40.0 then
      failures += f"mean interior channel error ${receipt.meanInteriorChannelError}%.6f > 40.000000"
    if orientation.margin <= 3.0 then
      failures += f"direct orientation beats its closest flip by only ${orientation.margin}%.6f channels"
    failures.result()

  private def row(
    current: CorticalLensCase,
    render: JavaFxInterpretReceipt,
    visual: SurfaceVisualQaReceipt,
    orientation: SurfaceOrientationQaReceipt
  ): String =
    s"{" +
      s"\"label\":\"${current.label}\"," +
      f"\"fraction\":${current.fraction.value}%.3f," +
      s"\"geometryUpdates\":${render.geometryUpdates}," +
      s"\"geometryBytesUpdated\":${render.geometryBytesUpdated}," +
      s"\"atlasUpdates\":${render.atlasUpdates}," +
      f"\"elapsedMillis\":${render.elapsedNanos.toDouble / 1e6}%.6f," +
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
