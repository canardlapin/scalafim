package scalafim.image.view.canvas

import scala.scalajs.js
import intaglio.*
import intaglio.canvas.*
import scalafim.image.*
import scalafim.image.SampleSpaces.*
import scalafim.image.view.*

class CanvasViewerHostSuite extends munit.FunSuite:

  private val sampleSpace =
    SampleSpaces.requireVolumeD3(SampleSpaces(Vector(3, 3, 3))).toOption.get
  private val space = sampleSpace.grid
  private val volume = SomeScalarVolume.unsafeCopyFromCanonicalArray(
    PrimitiveBuffers.tabulate[Double](space.nVoxels)(_.toDouble),
    sampleSpace,
    "canvas"
  )
  private val layer = SliceLayer(
    LayerId.unsafe("anatomy"),
    volume,
    SliceSampling.Linear(),
    ScalarColorizer(DisplayWindow.unsafe(0.0, space.nVoxels.toDouble - 1.0))
  )
  private val model = ViewerModel.unsafe(space, Vector(layer))
  private val session = ViewerSession(
    ViewerState.centered(space),
    DeviceContext.unsafe(640.0, 480.0)
  )

  test("Canvas host compiles viewer scenes through the existing backend") {
    val compiled = CanvasViewerHost.compile(model, session).toOption.get

    assertEquals(CanvasProgram.validate(compiled.program), None)
    assertEquals(compiled.frame.device, session.device)
  }

  test("Canvas-relative pointer and wheel input become pure viewer actions") {
    val compiled = CanvasViewerHost.compile(model, session).toOption.get
    val panel = compiled.frame.panels.axial
    val rootX = panel.rect.left + panel.rect.width / 2.0
    val rootY = panel.rect.bottom + panel.rect.height / 2.0
    val deviceX = rootX * session.device.width
    val deviceY = (1.0 - rootY) * session.device.height

    val pick = CanvasViewerHost.pickAction(compiled, deviceX, deviceY).toOption.get
    val scroll = CanvasViewerHost.scrollAction(compiled, deviceX, deviceY, -2).toOption.get
    assert(pick.isInstanceOf[ViewerAction.Pick])
    assertEquals(scroll, ViewerAction.Scroll(AnatomicalPlane.Axial, -2))
    assert(CanvasViewerHost.pickAction(compiled, 639.0, 479.0).isLeft)
  }

  test("application controller owns state snapshots bindings and lifecycle") {
    val controller = CanvasViewerHost.controller(
      model,
      session,
      viewerCacheCapacity = 8,
      rasterCacheCapacity = 4
    ).toOption.get
    val initial = controller.snapshot().toOption.get
    val threshold = DisplayThreshold.transparentBand(4.0, 12.0).toOption.get
    val zoom = PanelView.unsafe(ZoomLevel.unsafe(2.0), centerX = 0.55, centerY = 0.45)

    controller.dispatch(ViewerAction.SetThreshold(layer.id, threshold)).toOption.get
    controller.dispatch(ViewerAction.SetPanelView(AnatomicalPlane.Axial, zoom)).toOption.get
    val compiled = controller.compile().toOption.get
    val panel = compiled.frame.panels.axial
    val deviceX = (panel.rect.left + panel.rect.width / 2.0) * session.device.width
    val deviceY = (1.0 - panel.rect.bottom - panel.rect.height / 2.0) * session.device.height
    controller.pick(deviceX, deviceY).toOption.get
    controller.scroll(deviceX, deviceY, 1).toOption.get

    val changed = controller.compile().toOption.get
    assertEquals(changed.frame.state.panelViews.axial, zoom)
    assertEquals(changed.frame.state.presentation(layer.id).threshold, Some(threshold))
    assertEquals(changed.frame.readouts.axial.layers.map(_.layer), Vector(layer.id))

    controller.restore(initial).toOption.get
    assertEquals(controller.session.toOption.get, session)
    controller.close()
    assert(controller.isClosed)
    assertEquals(controller.compile(), Left(CanvasViewerError.ControllerClosed))
    assertEquals(controller.dispatch(ViewerAction.SetTimepoint(0)), Left(CanvasViewerError.ControllerClosed))
  }

  test("scroll bursts coalesce and adjacent prefetch removes visible sampling work") {
    final class ManualScheduler extends CanvasTaskScheduler:
      private var tasks = Vector.empty[() => Unit]

      def schedule(task: () => Unit): Unit =
        tasks :+= task

      def pendingCount: Int =
        tasks.length

      def run(): Unit =
        val current = tasks
        tasks = Vector.empty
        current.foreach(_())

    val controller = CanvasViewerHost.controller(
      model,
      session,
      viewerCacheCapacity = 12,
      rasterCacheCapacity = 6
    ).toOption.get
    val scheduler = new ManualScheduler
    var flushed = Option.empty[(Either[CanvasViewerError, ViewerSession], CanvasScrollBatch)]
    val coordinator = controller.scrollCoordinator(scheduler) { (result, batch) =>
      flushed = Some(result -> batch)
    }.toOption.get
    coordinator.enqueue(AnatomicalPlane.Axial, 1)
    coordinator.enqueue(AnatomicalPlane.Axial, 2)
    coordinator.enqueue(AnatomicalPlane.Axial, -2)

    assertEquals(scheduler.pendingCount, 1)
    scheduler.run()
    val (result, batch) = flushed.get
    assert(result.isRight)
    assertEquals(batch.submittedEvents, 3)
    assertEquals(batch.executedActions, 1)
    assertEquals(batch.netSteps, Map(AnatomicalPlane.Axial -> 1))
    assertEquals(
      controller.session.toOption.get.state.cursor,
      session.state.cursor + AnatomicalPlane.Axial.positiveNormal.unit.scaled(session.state.sliceStep.millimeters)
    )

    val prefetchController = CanvasViewerHost.controller(
      model,
      session,
      viewerCacheCapacity = 12,
      rasterCacheCapacity = 6
    ).toOption.get
    val prefetched = prefetchController.prefetchSlices(
      AnatomicalPlane.Axial,
      Vector(1)
    ).toOption.get
    assertEquals(prefetched.requestedSlices, 1)
    assert(prefetched.viewerProfile.sampledPixels > 0L)
    prefetchController.dispatch(ViewerAction.Scroll(AnatomicalPlane.Axial, 1)).toOption.get
    val warm = prefetchController.compile().toOption.get
    assertEquals(warm.viewerProfile.cacheHits, 3)
    assertEquals(warm.viewerProfile.sampledPixels, 0L)
    assertEquals(warm.viewerProfile.colorizedPixels, 0L)
    assert(prefetchController.prefetchSlices(AnatomicalPlane.Axial, Vector(2)).isLeft)
  }

  test("runtime preserves viewer rasters and Canvas uploads across redraws") {
    var uploads = 0
    given CanvasRasterFactory with
      def create(image: RasterImage, target: CanvasRenderingContext2D): CanvasImageSource =
        uploads += 1
        js.Dynamic.literal().asInstanceOf[CanvasImageSource]

    def noArgs: js.Function0[Unit] =
      () => ()
    val context = js.Dynamic
      .literal(
        save = noArgs,
        restore = noArgs,
        beginPath = noArgs,
        closePath = noArgs,
        fill = noArgs,
        stroke = noArgs,
        clip = noArgs,
        moveTo = ((_: Double, _: Double) => ()): js.Function2[Double, Double, Unit],
        lineTo = ((_: Double, _: Double) => ()): js.Function2[Double, Double, Unit],
        rect = ((_: Double, _: Double, _: Double, _: Double) => ()): js.Function4[Double, Double, Double, Double, Unit],
        arc = ((_: Double, _: Double, _: Double, _: Double, _: Double, _: Boolean) => ()): js.Function6[Double, Double, Double, Double, Double, Boolean, Unit],
        translate = ((_: Double, _: Double) => ()): js.Function2[Double, Double, Unit],
        rotate = ((_: Double) => ()): js.Function1[Double, Unit],
        setLineDash = ((_: js.Array[Double]) => ()): js.Function1[js.Array[Double], Unit],
        fillText = ((_: String, _: Double, _: Double) => ()): js.Function3[String, Double, Double, Unit],
        drawImage = ((_: CanvasImageSource, _: Double, _: Double, _: Double, _: Double) => ()): js.Function5[CanvasImageSource, Double, Double, Double, Double, Unit],
        strokeStyle = "",
        fillStyle = "",
        globalAlpha = 1.0,
        lineWidth = 1.0,
        lineCap = "",
        lineJoin = "",
        font = "",
        textAlign = "start",
        textBaseline = "alphabetic",
        imageSmoothingEnabled = true
      )
      .asInstanceOf[CanvasRenderingContext2D]
    val runtime = CanvasViewerHost.runtime(viewerCacheCapacity = 8, rasterCacheCapacity = 4).toOption.get

    val cold = runtime.render(model, session, context).toOption.get
    val warm = runtime.render(model, session, context).toOption.get

    assertEquals(cold.compiled.viewerProfile.cacheMisses, 3)
    assertEquals(cold.compiled.viewerProfile.sampleCacheMisses, 3)
    assertEquals(cold.canvasProfile.cacheMisses, 3)
    assertEquals(cold.canvasProfile.uploadedBytes, cold.compiled.viewerProfile.colorizedPixels * 4L)
    assertEquals(warm.compiled.viewerProfile.cacheHits, 3)
    assertEquals(warm.compiled.viewerProfile.sampledPixels, 0L)
    assertEquals(warm.canvasProfile, CanvasDrawProfile(3, 3, 0, 0L))
    assertEquals(uploads, 3)
    assertEquals(runtime.sampledSliceCount, 3)
    assertEquals(runtime.rasterCount, 3)

    val resized = runtime.render(
      model,
      session.copy(device = DeviceContext.unsafe(800.0, 500.0)),
      context
    ).toOption.get
    assertEquals(resized.compiled.viewerProfile.cacheHits, 3)
    assertEquals(resized.canvasProfile, CanvasDrawProfile(3, 3, 0, 0L))

    val movedSession = session.copy(
      state = session.state.copy(cursor = session.state.cursor + AnatomicalDirection.Superior.unit.scaled(1.0))
    )
    val moved = runtime.render(model, movedSession, context).toOption.get
    assertEquals(moved.compiled.viewerProfile.cacheHits, 2)
    assertEquals(moved.compiled.viewerProfile.cacheMisses, 1)
    assertEquals(moved.canvasProfile.cacheHits, 2)
    assertEquals(moved.canvasProfile.cacheMisses, 1)
    assertEquals(uploads, 4)
  }
