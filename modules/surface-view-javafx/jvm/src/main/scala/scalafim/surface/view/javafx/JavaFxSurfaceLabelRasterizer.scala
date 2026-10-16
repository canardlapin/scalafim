package scalafim.surface.view.javafx

import intaglio.*
import javafx.application.Platform
import javafx.geometry.Rectangle2D
import javafx.scene.SnapshotParameters
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import javafx.scene.text.{Font, Text}
import javafx.scene.transform.Scale
import scalafim.surface.view.*

/** The font family is explicit. Share the resulting assets with other backends
  * when identical glyph pixels are required across platforms.
  */
final class JavaFxSurfaceLabelRasterizer(fontFamily: String, fontEm: Double = 32, rasterScale: Double = 2)
    extends SurfaceLabelRasterizer:
  require(fontFamily.trim.nonEmpty && fontEm.isFinite && fontEm > 0 && rasterScale.isFinite && rasterScale > 0)

  def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
    if !Platform.isFxApplicationThread then Left("label rasterization requires the JavaFX Application Thread")
    else if text.trim.isEmpty then Left("label text must be non-empty")
    else
      try
        val node = new Text(text)
        node.setFont(Font.font(fontFamily, fontEm))
        node.setFill(Color.rgb(color.red, color.green, color.blue, color.alpha / 255.0))
        val bounds = node.getBoundsInLocal
        val pad = 2.0
        val width = math.ceil((bounds.getWidth + pad * 2) * rasterScale).toInt
        val height = math.ceil((bounds.getHeight + pad * 2) * rasterScale).toInt
        val parameters = new SnapshotParameters()
        parameters.setFill(Color.TRANSPARENT)
        parameters.setTransform(new Scale(rasterScale, rasterScale))
        parameters.setViewport(new Rectangle2D((bounds.getMinX - pad) * rasterScale,
          (bounds.getMinY - pad) * rasterScale, width, height))
        val image = node.snapshot(parameters, new WritableImage(width, height))
        val pixels = Array.tabulate(width * height): index =>
          val argb = image.getPixelReader.getArgb(index % width, index / width)
          ((argb & 0xffffff) << 8) | ((argb >>> 24) & 255)
        Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(width, height), pixels),
          fontEm, width / rasterScale / 2, height / rasterScale / 2, rasterScale))
      catch case scala.util.control.NonFatal(error) => Left(s"label rasterization failed: ${error.getMessage}")
