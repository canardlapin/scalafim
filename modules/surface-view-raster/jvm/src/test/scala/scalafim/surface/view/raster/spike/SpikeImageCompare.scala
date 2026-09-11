package scalafim.surface.view.raster.spike

import java.nio.file.{Files, Path}

import intaglio.*
import scalafim.surface.view.raster.*

/** Compares a reference-raster PNG with a JavaFX oracle PNG of the same scene.
  *
  * Usage: `<rasterPng> <javafxPng> <outPrefix>`. Writes `<outPrefix>-diff.png`
  * (per-pixel maximum channel error, amplified x4, with mask disagreement in
  * red) and `<outPrefix>-compare.json`.
  *
  * Interior pixels are foreground pixels of the raster image whose four
  * neighbours are also foreground (the `SurfaceVisualQa` convention); the edge
  * band is the remaining foreground. Foreground follows `SurfaceVisualQa`:
  * any channel below 250.
  */
object SpikeImageCompare:
  private val ForegroundLimit = 250

  def main(args: Array[String]): Unit =
    require(args.length == 3, "usage: <rasterPng> <javafxPng> <outPrefix>")
    val (width, height, raster) = SpikeImages.readArgb(Path.of(args(0)))
    val (otherWidth, otherHeight, javafx) = SpikeImages.readArgb(Path.of(args(1)))
    require(width == otherWidth && height == otherHeight, s"size mismatch ${width}x$height vs ${otherWidth}x$otherHeight")
    val prefix = args(2)
    inline def foreground(value: Int): Boolean =
      ((value >>> 16) & 0xff) < ForegroundLimit || ((value >>> 8) & 0xff) < ForegroundLimit || (value & 0xff) < ForegroundLimit
    inline def maxChannelError(a: Int, b: Int): Int =
      math.max(
        math.abs(((a >>> 16) & 0xff) - ((b >>> 16) & 0xff)),
        math.max(math.abs(((a >>> 8) & 0xff) - ((b >>> 8) & 0xff)), math.abs((a & 0xff) - (b & 0xff)))
      )
    val diff = new Array[Int](width * height)
    val interiorErrors = new Array[Int](256)
    val edgeErrors = new Array[Int](256)
    var interior = 0
    var edge = 0
    var interiorSum = 0L
    var interiorSquares = 0L
    var maskOnlyRaster = 0
    var maskOnlyJavafx = 0
    var y = 0
    while y < height do
      var x = 0
      while x < width do
        val index = y * width + x
        val a = raster(index)
        val b = javafx(index)
        val fa = foreground(a)
        val fb = foreground(b)
        val error = maxChannelError(a, b)
        val amplified = math.min(255, error * 4)
        diff(index) =
          if fa != fb then 0xffff0000 | (if fa then 0x0000 else 0x00aa) // red: raster-only, pink-ish: javafx-only
          else 0xff000000 | (amplified << 16) | (amplified << 8) | amplified
        if fa && !fb then maskOnlyRaster += 1
        if fb && !fa then maskOnlyJavafx += 1
        if fa then
          val isInterior = x > 0 && y > 0 && x + 1 < width && y + 1 < height &&
            foreground(raster(index - 1)) && foreground(raster(index + 1)) &&
            foreground(raster(index - width)) && foreground(raster(index + width))
          if isInterior then
            interior += 1
            interiorErrors(error) += 1
            interiorSum += error
            interiorSquares += error.toLong * error.toLong
          else
            edge += 1
            edgeErrors(error) += 1
        x += 1
      y += 1
    def quantile(histogram: Array[Int], count: Int, q: Double): Int =
      if count == 0 then 0
      else
        val target = math.max(1, math.ceil(q * count).toLong)
        var running = 0L
        var level = 0
        while level < 255 && running + histogram(level) < target do
          running += histogram(level)
          level += 1
        level
    def above(histogram: Array[Int], limit: Int): Int =
      var total = 0
      var level = limit + 1
      while level < 256 do
        total += histogram(level)
        level += 1
      total
    val visualQa = SurfaceVisualQa.compare(
      SpikeImages.rasterFromArgb(width, height, raster),
      SpikeImages.rasterFromArgb(width, height, javafx)
    ).fold(error => throw new IllegalStateException(error.message), identity)
    val violations = visualQa.violations(SurfaceVisualQaPolicy.NativeBackend)
    val diffPath = Path.of(prefix + "-diff.png")
    Files.write(diffPath, SpikeImages.pngBytes(width, height, diff))
    val meanInterior = if interior == 0 then 0.0 else interiorSum.toDouble / interior
    val rmsInterior = if interior == 0 then 0.0 else math.sqrt(interiorSquares.toDouble / interior)
    val json =
      s"""{"raster":"${args(0)}","javafx":"${args(1)}","width":$width,"height":$height,"metric":"per-pixel maximum absolute RGB channel error",""" +
        s""""interiorPixels":$interior,"interiorMean":$meanInterior,"interiorRms":$rmsInterior,""" +
        s""""interiorMedian":${quantile(interiorErrors, interior, 0.5)},"interiorP95":${quantile(interiorErrors, interior, 0.95)},"interiorP99":${quantile(interiorErrors, interior, 0.99)},""" +
        s""""interiorMax":${(255 to 0 by -1).find(interiorErrors(_) > 0).getOrElse(0)},""" +
        s""""interiorWithinTwoLevels":${1.0 - above(interiorErrors, 2).toDouble / math.max(1, interior)},"interiorAbove10":${above(interiorErrors, 10)},"interiorAbove40":${above(interiorErrors, 40)},""" +
        s""""edgePixels":$edge,"edgeMedian":${quantile(edgeErrors, edge, 0.5)},"edgeP95":${quantile(edgeErrors, edge, 0.95)},"edgeAbove40":${above(edgeErrors, 40)},""" +
        s""""maskOnlyRaster":$maskOnlyRaster,"maskOnlyJavafx":$maskOnlyJavafx,""" +
        s""""visualQa":{"maskIoU":${visualQa.maskIntersectionOverUnion},"centroidDistancePx":${visualQa.centroidDistancePixels},""" +
        s""""meanInteriorChannelError":${visualQa.meanInteriorChannelError},"maxInteriorChannelError":${visualQa.maximumInteriorChannelError},""" +
        s""""expectedForeground":${visualQa.expectedForegroundPixels},"observedForeground":${visualQa.observedForegroundPixels},""" +
        s""""nativeBackendPolicyPass":${violations.isEmpty},"violations":[${violations.map(v => "\"" + v.replace("\"", "'") + "\"").mkString(",")}]},""" +
        s""""diff":"$diffPath"}"""
    Files.writeString(Path.of(prefix + "-compare.json"), json + "\n")
    println(f"compare ${width}x$height interior=$interior mean=$meanInterior%.3f median=${quantile(interiorErrors, interior, 0.5)} p95=${quantile(interiorErrors, interior, 0.95)} p99=${quantile(interiorErrors, interior, 0.99)} " +
      f"within2=${1.0 - above(interiorErrors, 2).toDouble / math.max(1, interior)}%.4f IoU=${visualQa.maskIntersectionOverUnion}%.4f centroid=${visualQa.centroidDistancePixels}%.2f qaPass=${violations.isEmpty}")
