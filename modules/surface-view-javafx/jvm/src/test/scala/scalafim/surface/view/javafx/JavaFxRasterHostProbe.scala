package scalafim.surface.view.javafx

import java.nio.file.{Files, Path}
import java.util.concurrent.{AbstractExecutorService, CountDownLatch, LinkedBlockingQueue, TimeUnit}
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.geometry.Rectangle2D
import javafx.scene.image.Image
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import intaglio.*
import scalafim.surface.view.*

/** Real JavaFX publication/lifecycle checks, run outside headless MUnit. */
object JavaFxRasterHostProbe:
  private final class QueuedExecutor extends AbstractExecutorService:
    private val jobs = new LinkedBlockingQueue[Runnable]()
    @volatile private var stopped = false
    def execute(job: Runnable): Unit =
      require(!stopped)
      jobs.add(job)
      ()
    def runNext(): Unit =
      val next = jobs.poll(10, TimeUnit.SECONDS)
      require(next != null, "render task was never scheduled")
      next.run()
    def shutdown(): Unit = stopped = true
    def shutdownNow(): java.util.List[Runnable] =
      stopped = true
      val output = new java.util.ArrayList[Runnable]()
      jobs.drainTo(output)
      output
    def isShutdown: Boolean = stopped
    def isTerminated: Boolean = stopped && jobs.isEmpty
    def awaitTermination(timeout: Long, unit: TimeUnit): Boolean = isTerminated

  private def await[A](future: Future[A]): A = Await.result(future, 10.seconds)
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new AssertionError(s"$error"), identity)
  private def fx[A](body: => A): A =
    val latch = new CountDownLatch(1)
    var result: Either[Throwable, A] = Left(new AssertionError("FX callback did not run"))
    Platform.runLater: () =>
      try result = Right(body)
      catch case error: Throwable => result = Left(error)
      finally latch.countDown()
    require(latch.await(10, TimeUnit.SECONDS), "FX callback timed out")
    result.fold(throw _, identity)

  def main(args: Array[String]): Unit =
    val out = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-raster-host"))
    Files.createDirectories(out)
    Platform.startup(() => ())
    fx(Platform.setImplicitExit(false))
    val executor = new QueuedExecutor
    var hosts = Vector.empty[JavaFxRasterSurfaceHost]
    def newHost(): JavaFxRasterSurfaceHost =
      val host = fx(JavaFxRasterSurfaceHost.create(executor))
      hosts :+= host
      host
    val plan = ok(SurfaceCompositionPlan.fromLegacy(SurfaceFaceFixture.plan))
    val size = RasterDimensions.unsafe(96, 96)
    var checks = Vector.empty[String]
    try
      val host = newHost()
      val a = host.submit(plan, size)
      // Latest desired plan, not displayed plan, must survive a resize.
      val changed = ok(SurfaceCompositionPlan.make(plan.views.map(_.copy(surfaceBase = Rgba32.unsafe(17, 19, 23))), plan.resources))
      val b = host.submit(changed, RasterDimensions.unsafe(80, 80))
      val resized = host.resize(RasterDimensions.unsafe(64, 64))
      assert(await(a) == Left(JavaFxRasterHostError.Superseded))
      assert(await(b) == Left(JavaFxRasterHostError.Superseded))
      executor.runNext()
      fx(()) // First publication releases the worker and schedules latest request.
      executor.runNext()
      assert(ok(await(resized)).provider == "JavaFX-hosted raster")
      checks :+= "replacement futures and pending-plan resize"
      fx:
        val frame = host.presentedFrame.get
        assert(frame.dimensions == RasterDimensions.unsafe(64, 64))
        assert(frame.plan eq changed)
        host.imageView.setFitWidth(128)
        host.imageView.setFitHeight(128)
        val expected = frame.scientific.pick(32, 32).toOption.flatten.get
        val actual = ok(host.pickDisplay(64, 64)).get
        assert(actual == SurfaceDisplayHit.Scientific(expected))
        host.imageView.setViewport(new Rectangle2D(16, 16, 32, 32))
        assert(ok(host.pickDisplay(64, 64)).get == SurfaceDisplayHit.Scientific(expected))
        val pixel = frame.image.getPixelReader.getArgb(32, 32)
        val source = frame.scientific.image.pixelUnsafe(32, 32)
        assert(pixel == ((source.alpha << 24) | (source.red << 16) | (source.green << 8) | source.blue))
      assert(host.pickDisplay(32, 32) == Left(JavaFxRasterHostError.WrongThread))
      checks :+= "ARGB publication and FX-confined scaled/cropped picking"

      val queued = host.submit(plan, size)
      host.dispose()
      assert(await(queued) == Left(JavaFxRasterHostError.Disposed))
      executor.runNext()
      fx(assert(host.presentedFrame.isEmpty))
      assert(!executor.isShutdown)
      checks :+= "dispose settles active future and prevents delayed publication"

      val reentrant = newHost()
      var sawCoherentFrame = false
      fx:
        reentrant.imageView.imageProperty.addListener(new ChangeListener[Image]:
          def changed(observable: _root_.javafx.beans.value.ObservableValue[? <: Image], before: Image, after: Image): Unit =
            if after != null then
              val current = reentrant.presentedFrame.get
              assert(current.image eq after)
              sawCoherentFrame = ok(reentrant.pickDisplay(48, 48)).nonEmpty
              reentrant.dispose()
        )
      val reentrantFuture = reentrant.submit(plan, size)
      executor.runNext()
      assert(await(reentrantFuture) == Left(JavaFxRasterHostError.Disposed))
      fx:
        assert(sawCoherentFrame)
        assert(reentrant.presentedFrame.isEmpty)
        assert(reentrant.imageView.getImage == null)
      checks :+= "synchronous image listener sees coherent picks and cannot resurrect disposal"

      val decorated = newHost()
      def lower(p: SurfaceCompositionPlan, dimensions: RasterDimensions): Either[String, SurfaceDecorationGeometry] =
        SurfaceDecorationCompiler.compile(Vector.empty, p).flatMap(packets =>
          SurfaceDecorationGeometry.lower(packets, p, dimensions.width, dimensions.height)).left.map(_.toString)
      val fixed = decorated.submit(plan, size, Some(ok(lower(plan, size))))
      executor.runNext()
      ok(await(fixed))
      assert(await(decorated.resize(RasterDimensions.unsafe(64, 64))) == Left(JavaFxRasterHostError.DecorationReloweringRequired))
      val rebuild = decorated.submitDecorated(plan, size, lower)
      executor.runNext()
      ok(await(rebuild))
      val smaller = decorated.resize(RasterDimensions.unsafe(64, 64))
      executor.runNext()
      ok(await(smaller))
      assert(decorated.presentedFrame.get.decorations.nonEmpty)
      checks :+= "decorated resize refuses fixed geometry and rebuilds through retained lowering"
      Files.writeString(out.resolve("host-lifecycle.json"),
        s"""{"provider":"JavaFX-hosted raster","passed":true,"checks":[${checks.map(value => "\"" + value + "\"").mkString(",")}]}\n""")
      println(s"JavaFX raster host: ${checks.length} lifecycle/interaction checks passed; $out")
    finally
      fx(hosts.foreach(_.dispose()))
      executor.shutdown()
      Platform.exit()
