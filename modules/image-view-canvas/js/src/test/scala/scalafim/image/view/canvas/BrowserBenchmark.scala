package scalafim.image.view.canvas

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.Uint8ClampedArray
import intaglio.*
import intaglio.canvas.*
import ravel.NDArray as RavelArray
import scalafim.image.*
import scalafim.image.view.*

/** Opt-in real-browser performance receipt. Timings are diagnostic and are
  * never asserted by the ordinary Scala.js test suite.
  */
object BrowserBenchmark:
  private final case class Workload(
    model: ViewerModel,
    session: ViewerSession,
    sourceReads: () => Int
  )

  private final case class InteractiveContracts(
    applicationWorkflow: Boolean,
    orthogonalOrientationAndHandedness: Boolean,
    axialSliceDirection: Boolean
  ):
    def allPass: Boolean =
      applicationWorkflow && orthogonalOrientationAndHandedness && axialSliceDirection

    def toJs: js.Dynamic =
      js.Dynamic.literal(
        applicationWorkflow = applicationWorkflow,
        orthogonalOrientationAndHandedness = orthogonalOrientationAndHandedness,
        axialSliceDirection = axialSliceDirection,
        allPass = allPass
      )

  @JSExportTopLevel("runScalafimImageViewBenchmark")
  def run(): Unit =
    given CanvasRasterFactory = CanvasRasterFactory.browser
    val document = js.Dynamic.global.document
    val canvas = document.getElementById("viewer-benchmark-canvas").asInstanceOf[CanvasElement]
    val context = canvas.getContext("2d")
    context.asInstanceOf[js.Dynamic].clearRect(0.0, 0.0, canvas.width.toDouble, canvas.height.toDouble)
    val affine = affineWorkload()
    val nonlinear = nonlinearWorkload()

    renderOnce(affine.model, affine.session, context)
    renderOnce(nonlinear.model, nonlinear.session, context)
    val interactive = interactiveWorkflow(affine.model, affine.session, context)

    val coldReadsBefore = affine.sourceReads()
    val cold = measure("cold-render", 8) { _ =>
      val runtime = makeRuntime()
      render(runtime, affine.model, affine.session, context)
    }
    val coldReads = affine.sourceReads() - coldReadsBefore

    val warmRuntime = makeRuntime()
    render(warmRuntime, affine.model, affine.session, context)
    val warm = measure("warm-redraw", 80) { _ =>
      render(warmRuntime, affine.model, affine.session, context)
    }
    val phaseRuntime = makeRuntime()
    val phaseProgram = render(phaseRuntime, affine.model, affine.session, context).compiled
    val warmCompileOnly = measureBatch("warm-compile-only", 1000) {
      phaseRuntime.compile(affine.model, affine.session)
        .fold(error => throw new IllegalArgumentException(error.message), identity)
    }
    val warmDrawOnly = measureBatch("warm-draw-only", 1000) {
      CanvasRenderer.drawCached(phaseProgram.program, context, phaseRuntime.rasterCache)
    }

    val scrollRuntime = makeRuntime()
    render(scrollRuntime, affine.model, affine.session, context)
    val scroll = measure("axial-scroll", 24) { index =>
      val distance = (index + 1).toDouble
      val next = affine.session.copy(
        state = affine.session.state.copy(
          cursor = affine.session.state.cursor + AnatomicalDirection.Superior.unit.scaled(distance)
        )
      )
      render(scrollRuntime, affine.model, next, context)
    }
    val prefetchedScroll = measurePrefetchedScroll(
      affine.model,
      affine.session,
      context,
      repetitions = 24
    )

    val windowRuntime = makeRuntime()
    render(windowRuntime, affine.model, affine.session, context)
    val overlayId = LayerId.unsafe("overlay")
    val windowed = measure("window-recolor", 24) { index =>
      val upper = 1300.0 + index.toDouble
      val next = affine.session.copy(
        state = affine.session.state.copy(
          layerPresentation = Map(
            overlayId -> LayerPresentation(window = Some(DisplayWindow.unsafe(100.0, upper)))
          )
        )
      )
      render(windowRuntime, affine.model, next, context)
    }

    val nonlinearRuntime = makeRuntime()
    render(nonlinearRuntime, nonlinear.model, nonlinear.session, context)
    val nonlinearScroll = measure("nonlinear-scroll", 16) { index =>
      val distance = (index + 1).toDouble
      val next = nonlinear.session.copy(
        state = nonlinear.session.state.copy(
          cursor = nonlinear.session.state.cursor + AnatomicalDirection.Superior.unit.scaled(distance)
        )
      )
      render(nonlinearRuntime, nonlinear.model, next, context)
    }

    val checksum = canvasChecksum(context, canvas.width, canvas.height)
    val coldReadContract = coldReads == 16
    val warmContract =
      warm.viewer.sampledPixels.asInstanceOf[Double] == 0.0 &&
        warm.viewer.colorizedPixels.asInstanceOf[Double] == 0.0 &&
        warm.canvas.uploadedBytes.asInstanceOf[Double] == 0.0
    val scrollContract =
      scroll.viewer.rasterHits.asInstanceOf[Int] == 4 &&
        scroll.viewer.rasterMisses.asInstanceOf[Int] == 2
    val prefetchContract =
      prefetchedScroll.viewer.rasterHits.asInstanceOf[Int] == 6 &&
        prefetchedScroll.viewer.sampledPixels.asInstanceOf[Double] == 0.0 &&
        prefetchedScroll.viewer.colorizedPixels.asInstanceOf[Double] == 0.0
    val windowContract =
      windowed.viewer.sourceReads.asInstanceOf[Int] == 0 &&
        windowed.viewer.sampledPixels.asInstanceOf[Double] == 0.0
    val nonlinearContract =
      nonlinearScroll.viewer.rasterHits.asInstanceOf[Int] == 2 &&
        nonlinearScroll.viewer.rasterMisses.asInstanceOf[Int] == 1
    val contracts = js.Dynamic.literal(
      coldSourceReadCoalescing = coldReadContract,
      warmZeroSamplingColorizationAndUpload = warmContract,
      axialScrollReusesTwoPlanesPerLayer = scrollContract,
      prefetchedAxialScrollHasNoVisibleSampling = prefetchContract,
      windowChangeReusesSamples = windowContract,
      nonlinearScrollReusesTwoPlanes = nonlinearContract,
      interactiveViewerWorkflow = interactive.applicationWorkflow,
      orthogonalOrientationAndHandedness = interactive.orthogonalOrientationAndHandedness,
      axialSliceDirection = interactive.axialSliceDirection,
      checksumPresent = checksum.nonEmpty
    )
    val receipt = js.Dynamic.literal(
      schema = "scalafim-image-view-browser-benchmark-v1",
      userAgent = js.Dynamic.global.navigator.userAgent,
      canvasWidth = canvas.width,
      canvasHeight = canvas.height,
      affineShape = js.Array(160, 192, 128),
      nonlinearShape = js.Array(80, 80, 64),
      coldSourceReads = coldReads,
      cold = cold,
      warm = warm,
      warmCompileOnly = warmCompileOnly,
      warmDrawOnly = warmDrawOnly,
      scroll = scroll,
      prefetchedScroll = prefetchedScroll,
      window = windowed,
      nonlinear = nonlinearScroll,
      interactive = interactive.toJs,
      contracts = contracts,
      allContractsPass = coldReadContract && warmContract && scrollContract && prefetchContract && windowContract && nonlinearContract && interactive.allPass && checksum.nonEmpty,
      canvasChecksum = checksum
    )
    js.Dynamic.global.window.scalafimImageViewBenchmark = receipt
    document.getElementById("benchmark-output").textContent = js.JSON.stringify(receipt, space = 2)
    document.body.dataset.benchmarkState = "ready"

  private def affineWorkload(): Workload =
    val space = VolumeSpace(SampleSpaces(Vector(160, 192, 128)))
    val volume = syntheticVolume(space, "browser-affine")
    var reads = 0
    val source = VolumeSource.lazyFrames(space, 1) { _ =>
      reads += 1
      Right(volume)
    }.fold(error => throw new IllegalArgumentException(error.message), identity)
    val anatomy = SliceLayer.fromSource(
      LayerId.unsafe("anatomy"),
      source,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 1400.0))
    )
    val overlay = SliceLayer.fromSource(
      LayerId.unsafe("overlay"),
      source,
      SliceSampling.Nearest(0.0),
      ScalarColorizer(DisplayWindow.unsafe(100.0, 1300.0), ColorRamp.Heat),
      opacity = LayerOpacity.unsafe(0.45)
    )
    val model = ViewerModel.unsafe(space, Vector(anatomy, overlay))
    Workload(
      model,
      ViewerSession(ViewerState.centered(space), DeviceContext.unsafe(960.0, 720.0)),
      () => reads
    )

  private def nonlinearWorkload(): Workload =
    val space = VolumeSpace(SampleSpaces(Vector(80, 80, 64)))
    val volume = syntheticVolume(space, "browser-nonlinear")
    val grid = GridSpec.fromVolumeSpace(space)
    val field =
      RavelArray.tabulate[Double](
        grid.shape.x,
        grid.shape.y,
        grid.shape.z,
        3
      ) { (x, y, _, component) =>
        component match
          case 0 => 0.35 * math.sin(y.toDouble / 9.0)
          case 1 => 0.25 * math.cos(x.toDouble / 11.0)
          case _ => 0.0
      }
    val morphism = DenseFieldMorphism.displacement(
      SpatialDomainId("source"),
      SpatialDomainId("reference"),
      grid,
      field,
      Resample.Method.Linear
    ).fold(error => throw new IllegalArgumentException(error.message), identity)
    val layer = SliceLayer(
      LayerId.unsafe("warped"),
      volume,
      SliceSampling.Linear(),
      ScalarColorizer(DisplayWindow.unsafe(0.0, 700.0)),
      mapping = LayerMapping.Pullback(morphism)
    )
    Workload(
      ViewerModel.unsafe(space, Vector(layer)),
      ViewerSession(ViewerState.centered(space), DeviceContext.unsafe(960.0, 720.0)),
      () => 0
    )

  private def syntheticVolume(space: VolumeSpace, label: String): SomeScalarVolume[Double] =
    val shape = space.shape
    val data = PrimitiveBuffers.tabulate[Double](shape.product) { index =>
      val x = index % shape.x
      val y = (index / shape.x) % shape.y
      val z = index / (shape.x * shape.y)
      x.toDouble * 3.0 + y.toDouble * 2.0 + z.toDouble * 5.0
    }
    SomeScalarVolume.unsafeCopyFromCanonicalArray(data, space.toSampleSpace, label)

  private def makeRuntime(): CanvasViewerRuntime =
    CanvasViewerHost.runtime(viewerCacheCapacity = 96, rasterCacheCapacity = 48)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def renderOnce(
    model: ViewerModel,
    session: ViewerSession,
    context: CanvasRenderingContext2D
  )(using CanvasRasterFactory): CanvasViewerRender =
    render(makeRuntime(), model, session, context)

  private def interactiveWorkflow(
    model: ViewerModel,
    session: ViewerSession,
    context: CanvasRenderingContext2D
  )(using CanvasRasterFactory): InteractiveContracts =
    val controller = CanvasViewerHost.controller(
      model,
      session,
      viewerCacheCapacity = 24,
      rasterCacheCapacity = 12
    ).fold(error => throw new IllegalArgumentException(error.message), identity)
    val initial = controller.snapshot()
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val overlay = LayerId.unsafe("overlay")
    val threshold = DisplayThreshold.transparentBand(450.0, 900.0)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val view = PanelView.unsafe(ZoomLevel.unsafe(2.0), centerX = 0.55, centerY = 0.45)
    controller.dispatch(ViewerAction.SetVisibility(overlay, visible = true))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    controller.dispatch(ViewerAction.SetThreshold(overlay, threshold))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    controller.dispatch(ViewerAction.SetPanelView(AnatomicalPlane.Axial, view))
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val compiled = controller.compile()
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val orientationAndHandedness = compiled.frame.panels.all.forall { panel =>
      val actual = panel.grid.plane
      val expected = SlicePlane.canonical(
        panel.anatomicalPlane,
        actual.through,
        compiled.frame.state.convention
      )
      actual.screenRight == expected.screenRight &&
        actual.screenUp == expected.screenUp &&
        actual.normal == expected.normal
    }
    val panel = compiled.frame.panels.axial
    val deviceX = (panel.rect.left + panel.rect.width / 2.0) * session.device.width
    val deviceY = (1.0 - panel.rect.bottom - panel.rect.height / 2.0) * session.device.height
    controller.pick(deviceX, deviceY)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val picked = controller.session
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    controller.scroll(deviceX, deviceY, 1)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val rendered = controller.render(context)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val current = controller.session
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val expectedCursor = picked.state.cursor +
      AnatomicalPlane.Axial.positiveNormal.unit.scaled(picked.state.sliceStep.millimeters)
    val axialSliceDirection = current.state.cursor == expectedCursor
    val exercised =
      model.layers.length == 2 &&
        current.state.cursor != initial.session.state.cursor &&
        current.state.panelViews.axial == view &&
        current.state.presentation(overlay).visible &&
        current.state.presentation(overlay).threshold.contains(threshold) &&
        rendered.compiled.frame.readouts.axial.layers.map(_.layer) == model.layers.map(_.id)
    controller.restore(initial)
      .fold(error => throw new IllegalArgumentException(error.message), identity)
    val restored = controller.session.contains(initial.session)
    controller.close()
    val lifecycleClosed = controller.compile() == Left(CanvasViewerError.ControllerClosed)
    InteractiveContracts(
      applicationWorkflow = exercised && restored && lifecycleClosed,
      orthogonalOrientationAndHandedness = orientationAndHandedness,
      axialSliceDirection = axialSliceDirection
    )

  private def render(
    runtime: CanvasViewerRuntime,
    model: ViewerModel,
    session: ViewerSession,
    context: CanvasRenderingContext2D
  )(using CanvasRasterFactory): CanvasViewerRender =
    runtime.render(model, session, context)
      .fold(error => throw new IllegalArgumentException(error.message), identity)

  private def measure(
    name: String,
    repetitions: Int
  )(run: Int => CanvasViewerRender): js.Dynamic =
    val samples = new Array[Double](repetitions)
    var last = Option.empty[CanvasViewerRender]
    var index = 0
    while index < repetitions do
      val started = js.Dynamic.global.performance.now().asInstanceOf[Double]
      last = Some(run(index))
      samples(index) = js.Dynamic.global.performance.now().asInstanceOf[Double] - started
      index += 1
    val sorted = samples.sorted
    val middle = sorted(sorted.length / 2)
    val p95 = sorted(math.min(sorted.length - 1, math.ceil(sorted.length * 0.95).toInt - 1))
    val result = last.get
    js.Dynamic.literal(
      scenario = name,
      repetitions = repetitions,
      medianMs = middle,
      p95Ms = p95,
      minMs = sorted.head,
      maxMs = sorted.last,
      viewer = viewerProfile(result.compiled.viewerProfile),
      canvas = canvasProfile(result.canvasProfile)
    )

  private def measureBatch(name: String, repetitions: Int)(run: => Any): js.Dynamic =
    val started = js.Dynamic.global.performance.now().asInstanceOf[Double]
    var index = 0
    while index < repetitions do
      run
      index += 1
    val elapsed = js.Dynamic.global.performance.now().asInstanceOf[Double] - started
    js.Dynamic.literal(
      scenario = name,
      repetitions = repetitions,
      elapsedMs = elapsed,
      meanMs = elapsed / repetitions.toDouble
    )

  private def measurePrefetchedScroll(
    model: ViewerModel,
    initial: ViewerSession,
    context: CanvasRenderingContext2D,
    repetitions: Int
  )(using CanvasRasterFactory): js.Dynamic =
    val runtime = makeRuntime()
    render(runtime, model, initial, context)
    val samples = new Array[Double](repetitions)
    var current = initial
    var last = Option.empty[CanvasViewerRender]
    var index = 0
    while index < repetitions do
      runtime.prefetchSlices(model, current, AnatomicalPlane.Axial, Vector(1))
        .fold(error => throw new IllegalArgumentException(error.message), identity)
      val next = current.copy(
        state = current.state.copy(
          cursor = current.state.cursor + AnatomicalDirection.Superior.unit.scaled(current.state.sliceStep.millimeters)
        )
      )
      val started = js.Dynamic.global.performance.now().asInstanceOf[Double]
      last = Some(render(runtime, model, next, context))
      samples(index) = js.Dynamic.global.performance.now().asInstanceOf[Double] - started
      current = next
      index += 1
    val sorted = samples.sorted
    val result = last.get
    js.Dynamic.literal(
      scenario = "prefetched-axial-scroll",
      repetitions = repetitions,
      medianMs = sorted(sorted.length / 2),
      p95Ms = sorted(math.min(sorted.length - 1, math.ceil(sorted.length * 0.95).toInt - 1)),
      minMs = sorted.head,
      maxMs = sorted.last,
      viewer = viewerProfile(result.compiled.viewerProfile),
      canvas = canvasProfile(result.canvasProfile)
    )

  private def viewerProfile(profile: ViewerProfile): js.Dynamic =
    js.Dynamic.literal(
      layerRequests = profile.layerRequests,
      rasterHits = profile.cacheHits,
      rasterMisses = profile.cacheMisses,
      sampleHits = profile.sampleCacheHits,
      sampleMisses = profile.sampleCacheMisses,
      sampledPixels = profile.sampledPixels.toDouble,
      colorizedPixels = profile.colorizedPixels.toDouble,
      sourceReads = profile.sourceReads
    )

  private def canvasProfile(profile: CanvasDrawProfile): js.Dynamic =
    js.Dynamic.literal(
      imageRequests = profile.imageRequests,
      cacheHits = profile.cacheHits,
      cacheMisses = profile.cacheMisses,
      uploadedBytes = profile.uploadedBytes.toDouble
    )

  private def canvasChecksum(
    context: CanvasRenderingContext2D,
    width: Int,
    height: Int
  ): String =
    val bytes = context.asInstanceOf[js.Dynamic]
      .getImageData(0, 0, width, height)
      .data
      .asInstanceOf[Uint8ClampedArray]
    var hash = 0x811c9dc5
    var index = 0
    val length = bytes.length
    while index < length do
      hash = (hash ^ bytes(index).toInt) * 16777619
      index += 16
    java.lang.Integer.toUnsignedString(hash, 16)
