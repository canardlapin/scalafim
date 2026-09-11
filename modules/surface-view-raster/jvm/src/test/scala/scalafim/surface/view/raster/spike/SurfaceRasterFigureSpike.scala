package scalafim.surface.view.raster.spike

import java.lang.management.ManagementFactory
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

import intaglio.*
import scalafim.surface.view.raster.*

/** Measures the reference raster interpreter as a figure renderer on a
  * serialised application scene.
  *
  * Usage: `<plan> <outDir> <label> <sizes> <repetitions> <threads>` where
  * sizes is a comma-separated `WxH` list and threads `1` selects
  * `SurfaceRasterizer` itself while `N > 1` selects the test-scope banded
  * `ParallelSurfaceRasterizer` with `N` worker threads.
  *
  * Every timed run renders the full frame; the PNG written is the last run's,
  * and each run's pixel and PNG SHA-256 digests are recorded so determinism
  * within one JVM and across JVMs can be checked from the receipt alone.
  */
object SurfaceRasterFigureSpike:
  def main(args: Array[String]): Unit =
    require(args.length == 6, "usage: <plan> <outDir> <label> <WxH,...> <repetitions> <threads>")
    val planPath = Path.of(args(0))
    val out = Path.of(args(1))
    Files.createDirectories(out)
    val label = args(2)
    val sizes = args(3).split(",").toVector.map: token =>
      val parts = token.split("x")
      (parts(0).toInt, parts(1).toInt)
    val repetitions = args(4).toInt.max(1)
    val threads = args(5).toInt.max(1)
    val loadStarted = System.nanoTime()
    val scene = SpikePlanCodec.read(planPath)
    val loadNanos = System.nanoTime() - loadStarted
    val pools = ManagementFactory.getMemoryPoolMXBeans.asScala.filter(_.getType == java.lang.management.MemoryType.HEAP)
    val records = Vector.newBuilder[String]
    sizes.foreach: (width, height) =>
      val plan = scene.planFor(width, height)
      val dimensions = RasterDimensions.unsafe(width, height)
      pools.foreach(_.resetPeakUsage())
      def renderOnce(): (RasterImage, SurfaceRasterReceipt) =
        if threads == 1 then
          val result = SurfaceRasterizer.render(plan, dimensions).fold(error => throw new IllegalStateException(error.message), identity)
          (result.image, result.receipt)
        else
          val result = ParallelSurfaceRasterizer.render(plan, dimensions, threads)
          (result.image, result.receipt)
      val coldStarted = System.nanoTime()
      val (coldImage, coldReceipt) = renderOnce()
      val coldNanos = System.nanoTime() - coldStarted
      val coldPixelHash = SpikeImages.pixelSha256(coldImage)
      val runs = (1 to repetitions).map: _ =>
        System.gc()
        val started = System.nanoTime()
        val (image, receipt) = renderOnce()
        val elapsed = System.nanoTime() - started
        val heapAfter = Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()
        val pixelHash = SpikeImages.pixelSha256(image)
        val png = SpikeImages.pngBytes(image)
        (image, receipt, elapsed, heapAfter, pixelHash, SpikeImages.sha256(png), png)
      val last = runs.last
      val pngPath = out.resolve(s"raster-$label-${width}x$height.png")
      Files.write(pngPath, last._7)
      val peakHeap = pools.map(_.getPeakUsage.getUsed).sum
      val receipt = last._2
      val times = runs.map(_._3)
      records += s"""{"label":"$label","width":$width,"height":$height,"threads":$threads,"interpreter":"${if threads == 1 then "reference-raster" else "reference-raster-banded-parallel"}",""" +
        s""""coldNanos":$coldNanos,"coldPixelSha256":"$coldPixelHash","runNanos":[${times.mkString(",")}],""" +
        s""""medianMillis":${times.sorted.apply(times.length / 2) / 1e6},"minMillis":${times.min / 1e6},"maxMillis":${times.max / 1e6},""" +
        s""""pixelSha256":[${runs.map(r => "\"" + r._5 + "\"").mkString(",")}],"pngSha256":[${runs.map(r => "\"" + r._6 + "\"").mkString(",")}],""" +
        s""""pngBytes":${last._7.length},"png":"$pngPath","heapUsedAfterRenderBytes":[${runs.map(_._4).mkString(",")}],"peakHeapUsedBytes":$peakHeap,""" +
        s""""trianglesInput":${receipt.trianglesInput},"trianglesAfterClipping":${receipt.trianglesAfterClipping},"trianglesCulled":${receipt.trianglesCulled},""" +
        s""""shadedPixels":${receipt.shadedPixels},"depthRejectedPixels":${receipt.depthRejectedPixels},"setupNanos":${receipt.setupNanos},"renderNanos":${receipt.renderNanos},""" +
        s""""coldReceiptShadedPixels":${coldReceipt.shadedPixels}}"""
      println(f"$label ${width}x$height threads=$threads cold=${coldNanos / 1e6}%.1f ms runs=${times.map(t => f"${t / 1e6}%.1f").mkString("/")} ms " +
        f"deterministic=${runs.map(_._5).distinct.size == 1 && runs.head._5 == coldPixelHash} png=${runs.map(_._6).distinct.size == 1} peakHeap=${peakHeap / 1048576.0}%.0f MiB")
    val meta = s"""{"plan":"$planPath","planSha256":"${SpikeImages.sha256(Files.readAllBytes(planPath))}","loadNanos":$loadNanos,"label":"$label","threads":$threads,""" +
      s""""availableProcessors":${Runtime.getRuntime.availableProcessors()},"maxHeapBytes":${Runtime.getRuntime.maxMemory()},""" +
      s""""javaVersion":"${System.getProperty("java.runtime.version")}","osArch":"${System.getProperty("os.arch")}",""" +
      s""""sceneMetadata":${scene.metadata},"sizes":[${records.result().mkString(",")}]}"""
    val receiptPath = out.resolve(s"raster-$label-threads$threads-receipt.json")
    Files.writeString(receiptPath, meta + "\n")
    println(s"receipt: $receiptPath")
