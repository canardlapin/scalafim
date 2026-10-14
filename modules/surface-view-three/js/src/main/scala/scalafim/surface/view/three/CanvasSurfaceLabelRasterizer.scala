package scalafim.surface.view.three

import scala.scalajs.js

import intaglio.*
import scalafim.surface.view.*

/** Browser canvas implementation of the shared label-asset contract. The
  * returned pixels are measured once and can therefore be used by both the
  * native texture path and an export renderer without reflowing the label. */
final class CanvasSurfaceLabelRasterizer(fontFamily: String, fontEm: Double = 32.0,
    rasterScale: Double = 2.0) extends SurfaceLabelRasterizer:
  require(fontFamily.trim.nonEmpty, "font family must not be blank")
  require(fontEm.isFinite && fontEm > 0 && rasterScale.isFinite && rasterScale > 0,
    "font size and raster scale must be positive finite values")

  def rasterize(text: String, color: Rgba32): Either[String, SurfaceLabelAsset] =
    if text.trim.isEmpty then Left("label text must not be blank")
    else
      try
        val document = js.Dynamic.global.selectDynamic("document")
        if js.isUndefined(document) || document == null then Left("browser document is unavailable")
        else
          val canvas = document.applyDynamic("createElement")("canvas")
          val context = canvas.applyDynamic("getContext")("2d")
          if context == null || js.isUndefined(context) then Left("2D canvas context is unavailable")
          else
            context.updateDynamic("font")(s"${fontEm * rasterScale}px $fontFamily")
            context.updateDynamic("textBaseline")("alphabetic")
            val metrics = context.applyDynamic("measureText")(text)
            val ascent = metric(metrics, "actualBoundingBoxAscent", fontEm * rasterScale)
            val descent = metric(metrics, "actualBoundingBoxDescent", fontEm * rasterScale * 0.25)
            val pad = 2.0 * rasterScale
            val left = metric(metrics, "actualBoundingBoxLeft", 0.0)
            val right = metric(metrics, "actualBoundingBoxRight", metric(metrics, "width", fontEm * rasterScale))
            val width = math.max(1, math.ceil(left + right + pad * 2).toInt)
            val height = math.max(1, math.ceil(ascent + descent + pad * 2).toInt)
            canvas.updateDynamic("width")(width)
            canvas.updateDynamic("height")(height)
            val draw = canvas.applyDynamic("getContext")("2d")
            draw.updateDynamic("font")(s"${fontEm * rasterScale}px $fontFamily")
            draw.updateDynamic("textBaseline")("alphabetic")
            draw.updateDynamic("fillStyle")(s"rgba(${color.red}, ${color.green}, ${color.blue}, ${color.alpha.toDouble / 255.0})")
            draw.applyDynamic("fillText")(text, left + pad, ascent + pad)
            val rgba = draw.applyDynamic("getImageData")(0, 0, width, height).selectDynamic("data").asInstanceOf[js.typedarray.Uint8ClampedArray]
            val pixels = new Array[Int](width * height)
            var index = 0
            while index < pixels.length do
              val offset = index * 4
              pixels(index) = Rgba32.unsafe(rgba(offset).asInstanceOf[Int], rgba(offset + 1).asInstanceOf[Int],
                rgba(offset + 2).asInstanceOf[Int], rgba(offset + 3).asInstanceOf[Int]).toPackedInt
              index += 1
            Right(SurfaceLabelAsset(RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(width, height), pixels),
              fontEm, width / rasterScale / 2, height / rasterScale / 2, rasterScale))
      catch case error: Throwable => Left(Option(error.getMessage).getOrElse(error.toString))

  private def metric(metrics: js.Dynamic, name: String, fallback: Double): Double =
    val value = metrics.selectDynamic(name)
    if js.isUndefined(value) || value == null then fallback
    else
      val number = value.asInstanceOf[Double]
      if number.isFinite && number >= 0 then number else fallback
