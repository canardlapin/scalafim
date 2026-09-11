package scalafim.surface.view.raster.spike

import java.lang.management.{ManagementFactory, MemoryType}
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

/** Supersampled figure path for the reference raster interpreter.
  *
  * Usage: `<plan> <outDir> <label> <WxH> <k,...> <threads> <repetitions>`.
  * For each factor k the plan renders at kW x kH and is box-filtered to W x H;
  * k = 1 renders directly. Slot viewports are normalized and the exported
  * Contain fit depends only on aspect ratio, so the fitted slots of any
  * exported size with exactly the same aspect apply; other aspects are refused.
  *
  * A timed run is the whole figure path after scene load: render, box filter
  * and PNG encoding. Pixel and PNG digests are recorded for every run, and the
  * receipt carries the fields proposed for an application export receipt.
  */
object SurfaceRasterSupersampleSpike:
  private final case class Run(
    renderNanos: Long,
    filterNanos: Long,
    encodeNanos: Long,
    pixelSha256: String,
    pngSha256: String,
    png: Array[Byte]
  ):
    def totalNanos: Long = renderNanos + filterNanos + encodeNanos

  def main(args: Array[String]): Unit =
    require(args.length == 7, "usage: <plan> <outDir> <label> <WxH> <k,...> <threads> <repetitions>")
    val planPath = Path.of(args(0))
    val out = Path.of(args(1))
    Files.createDirectories(out)
    val label = args(2)
    val size = args(3).split("x").map(_.toInt)
    val width = size(0)
    val height = size(1)
    val factors = args(4).split(",").toVector.map(_.toInt)
    require(factors.forall(_ >= 1), "supersample factors must be positive")
    val threads = args(5).toInt.max(1)
    val repetitions = args(6).toInt.max(1)
    require(SurfaceRasterizer.capabilities.id == SurfaceBackendId.unsafe("reference-raster"), "unexpected interpreter id")
    val interpreter = if threads == 1 then "reference-raster" else "reference-raster+banded-parallel"
    val sceneSha256 = SpikeImages.sha256(Files.readAllBytes(planPath))
    val loadStarted = System.nanoTime()
    val scene = SpikePlanCodec.read(planPath)
    val loadNanos = System.nanoTime() - loadStarted
    val fit = scene.fitted.find(f => f.width.toLong * height == f.height.toLong * width)
      .getOrElse(throw new IllegalArgumentException(s"no exported fit has the aspect of ${width}x$height"))
    val plan = scene.plan.copy(slots = fit.slots)
    val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == MemoryType.HEAP)

    val records = factors.map: factor =>
      val dimensions = RasterDimensions.unsafe(width * factor, height * factor)
      def once(): Run =
        val started = System.nanoTime()
        val image =
          if threads == 1 then
            SurfaceRasterizer.render(plan, dimensions).fold(error => throw new IllegalStateException(error.message), _.image)
          else ParallelSurfaceRasterizer.render(plan, dimensions, threads).image
        val rendered = System.nanoTime()
        val argb = boxFilter(image, factor)
        val filtered = System.nanoTime()
        val png = SpikeImages.pngBytes(width, height, argb)
        val encoded = System.nanoTime()
        Run(rendered - started, filtered - rendered, encoded - filtered,
          SpikeImages.pixelSha256(SpikeImages.rasterFromArgb(width, height, argb)), SpikeImages.sha256(png), png)
      System.gc()
      pools.foreach(_.resetPeakUsage())
      val cold = once()
      val runs = Vector.fill(repetitions):
        System.gc()
        once()
      val peakHeap = pools.map(_.getPeakUsage.getUsed).sum
      val all = cold +: runs
      val deterministic = all.map(_.pixelSha256).distinct.size == 1 && all.map(_.pngSha256).distinct.size == 1
      val pngPath = out.resolve(s"raster-$label-${width}x$height-ss$factor.png")
      Files.write(pngPath, runs.last.png)
      val totals = runs.map(_.totalNanos).sorted
      val median = totals(totals.length / 2)
      println(f"$label ${width}x$height ss=$factor render=${width * factor}x${height * factor} threads=$threads " +
        f"cold=${cold.totalNanos / 1e6}%.1f ms runs=${runs.map(r => f"${r.totalNanos / 1e6}%.1f").mkString("/")} ms " +
        f"(render ${runs.last.renderNanos / 1e6}%.1f, filter ${runs.last.filterNanos / 1e6}%.1f, encode ${runs.last.encodeNanos / 1e6}%.1f) " +
        f"deterministic=$deterministic peakHeap=${peakHeap / 1048576.0}%.0f MiB")
      s"""{"interpreter":"$interpreter","planRevision":"${SurfaceRasterizer.capabilities.acceptedRevision}","threads":$threads,""" +
        s""""output":{"width":$width,"height":$height,"supersample":$factor,"renderWidth":${width * factor},"renderHeight":${height * factor},""" +
        s""""png":"$pngPath","pngBytes":${runs.last.png.length}},""" +
        s""""timing":{"coldTotalNanos":${cold.totalNanos},"coldRenderNanos":${cold.renderNanos},"runs":[""" +
        runs.map(r => s"""{"render":${r.renderNanos},"filter":${r.filterNanos},"encode":${r.encodeNanos},"total":${r.totalNanos}}""").mkString(",") +
        s"""],"medianTotalMillis":${median / 1e6},"maxTotalMillis":${totals.last / 1e6}},""" +
        s""""digests":{"deterministic":$deterministic,"coldPixelSha256":"${cold.pixelSha256}",""" +
        s""""pixelSha256":[${runs.map(r => "\"" + r.pixelSha256 + "\"").mkString(",")}],""" +
        s""""pngSha256":[${runs.map(r => "\"" + r.pngSha256 + "\"").mkString(",")}]},""" +
        s""""memory":{"peakHeapUsedBytes":$peakHeap}}"""

    val receipt =
      s"""{"receiptKind":"spike-raster-figure","label":"$label",""" +
        s""""scene":{"file":"$planPath","sha256":"$sceneSha256","cameraKey":"${plan.receipt.cameraKey}",""" +
        s""""meshKeys":[${plan.receipt.meshKeys.map(key => "\"" + key.value + "\"").mkString(",")}],""" +
        s""""layerKeys":[${plan.receipt.layerKeys.map(key => "\"" + key.value + "\"").mkString(",")}],""" +
        s""""fitSource":"${fit.width}x${fit.height}","metadata":${scene.metadata}},""" +
        s""""loadNanos":$loadNanos,"runtime":{"javaVersion":"${System.getProperty("java.runtime.version")}",""" +
        s""""osArch":"${System.getProperty("os.arch")}","availableProcessors":${Runtime.getRuntime.availableProcessors()},""" +
        s""""maxHeapBytes":${Runtime.getRuntime.maxMemory()}},"figures":[${records.mkString(",")}]}"""
    val receiptPath = out.resolve(s"raster-$label-${width}x$height-threads$threads-receipt.json")
    Files.writeString(receiptPath, receipt + "\n")
    println(s"receipt: $receiptPath")

  /** Box-filters a k-times supersampled image to ARGB at 1/k size with
    * round-half-up channel means; k = 1 copies the pixels.
    */
  private def boxFilter(image: RasterImage, factor: Int): Array[Int] =
    if factor == 1 then SpikeImages.toArgb(image)
    else
      val width = image.width / factor
      val height = image.height / factor
      val samples = factor * factor
      val output = new Array[Int](width * height)
      var y = 0
      while y < height do
        var x = 0
        while x < width do
          var red = 0
          var green = 0
          var blue = 0
          var dy = 0
          while dy < factor do
            var dx = 0
            while dx < factor do
              val pixel = image.pixelUnsafe(x * factor + dx, y * factor + dy)
              red += pixel.red
              green += pixel.green
              blue += pixel.blue
              dx += 1
            dy += 1
          output(y * width + x) = 0xff000000 | (((red + samples / 2) / samples) << 16) |
            (((green + samples / 2) / samples) << 8) | ((blue + samples / 2) / samples)
          x += 1
        y += 1
      output
