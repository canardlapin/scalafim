package scalafim.examples.surfaceview

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSExportTopLevel
import intaglio.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import scalafim.surface.view.three.*

/** Actual production native and fallback hosts consuming the same source input. */
object SurfaceQualificationAdapter:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalArgumentException(s"$error"), identity)

  @JSExportTopLevel("captureScalafimQualification")
  def capture(three: js.Dynamic, canvas: js.Dynamic, rasterCanvas: js.Dynamic, input: String,
      depthPolicy: String = "conditioned"): js.Dynamic =
    val prepared = ok(SurfaceQualificationFixture.decode(input))
    val labels = new CanvasSurfaceLabelRasterizer("Arial")
    val geometry = ok(SurfaceLabelLayout.resolve(prepared.decorations, prepared.plan, labels))
    val precision = depthPolicy match
      case "conditioned" => ThreeDepthPrecision.ConditionOccupiedRange
      case "preserved" => ThreeDepthPrecision.PreserveDepthProjection
      case other => throw new IllegalArgumentException(s"unknown depth policy: $other")
    val runtime = ok(ThreeJsRuntime.create(three, canvas, ThreeJsRuntimeOptions.Default.copy(depthPrecision = precision)))
    val backend = ok(ThreeSurfaceBackend.create(runtime))
    val host = ok(CanvasRasterSurfaceHost.attach(rasterCanvas))
    val size = ThreeCanvasSize.unsafe(prepared.width, prepared.height)
    val dimensions = RasterDimensions.unsafe(prepared.width, prepared.height)
    val disposal = js.Dynamic.literal()
    def drawNative() =
      if geometry.meshes.isEmpty then backend.render(prepared.plan, size, forceDraw = true)
      else backend.renderDecorated(prepared.plan, geometry, size, forceDraw = true)
    try
      val nativeStarted = System.nanoTime()
      val native = drawNative()
      val nativeReceipt = native match
        case Left(error) => js.Dynamic.literal(status = "refused", reason = error.toString)
        case Right(first) =>
          canvas.applyDynamic("toDataURL")("image/png")
          val coldSnapshotNanos = System.nanoTime() - nativeStarted
          val warmSamples = Vector.fill(5):
            val started = System.nanoTime()
            val receipt = ok(drawNative())
            val image = canvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
            (receipt, System.nanoTime() - started, image)
          val (warm, warmSnapshotNanos, image) = warmSamples.last
          js.Dynamic.literal(status = "captured", image = image,
            coldNanos = first.elapsedNanos.toDouble, warmNanos = warm.elapsedNanos.toDouble,
            coldSnapshotNanos = coldSnapshotNanos.toDouble, warmSnapshotNanos = warmSnapshotNanos.toDouble,
            timingBoundary = "render and synchronous PNG readback; submission times reported separately",
            warmSnapshotSamplesNanos = warmSamples.map(_._2.toDouble).toJSArray,
            warmImagesIdentical = warmSamples.forall(_._3 == image),
            warmResourceCounts = warmSamples.map(_._1.resourceCount).toJSArray,
            warmUploadBytes = warmSamples.map(_._1.uploadedBytes.toDouble).toJSArray,
            coldGeometryUploads = first.geometryUploads, warmGeometryUploads = warm.geometryUploads,
            warmColorUploads = warm.colorUploads, drawCalls = warm.drawCalls,
            depthPolicy = depthPolicy, projections = runtime.projectionReceipts.map(p => js.Dynamic.literal(
              view = p.view.map(_.value).orNull, columnMajorProjection = p.columnMajorProjection.toJSArray,
              viewport = js.Array(p.viewport._1, p.viewport._2, p.viewport._3, p.viewport._4))).toJSArray)
      val first = ok(host.submitGeometry(prepared.plan, dimensions, geometry,
        prepared.style))
      val rasterWarmSamples = Vector.fill(5):
        ok(host.submitGeometry(prepared.plan, dimensions, geometry, prepared.style))
      val warm = rasterWarmSamples.last
      val picks = new js.Array[js.Any]()
      for row <- 1 to 5; column <- 1 to 5 do
        val x = prepared.width * column / 6.0
        val y = prepared.height * row / 6.0
        val picked = ok(host.pickDisplay(x, y))
        val value = picked match
          case Some(SurfaceDisplayHit.Scientific(hit)) => js.Dynamic.literal(surface = hit.surface.value, vertex = hit.vertex, face = hit.face,
            view = hit.origin.map(_.view.value).orNull)
          case Some(SurfaceDisplayHit.Decoration(hit)) => js.Dynamic.literal(decoration = hit.id.decoration.value, element = hit.id.suffix)
          case None => null
        val nativePick = if native.isLeft then null else ok(backend.pickDisplay(x, y)).map:
          case SurfaceDisplayHit.Scientific(hit) => js.Dynamic.literal(surface = hit.surface.value, vertex = hit.vertex, face = hit.face,
            world = js.Array(hit.worldX, hit.worldY, hit.worldZ), view = hit.origin.map(_.view.value).orNull)
          case SurfaceDisplayHit.Decoration(hit) => js.Dynamic.literal(decoration = hit.id.decoration.value, element = hit.id.suffix)
        .orNull
        picks.push(js.Dynamic.literal(x = x, y = y, raster = value, native = nativePick))
      val camera = prepared.plan.views.head.camera
      val rasterImage = rasterCanvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      val lower: CanvasRasterSurfaceHost.DecorationLowering = (plan, size) =>
        prepared.lowerDecorations(plan, size.width, size.height)
          .flatMap(geometry => SurfaceLabelLayout.resolve(geometry, plan, labels))
      ok(host.submitDecorated(prepared.plan, dimensions, lower, prepared.style))
      ok(host.resize(RasterDimensions.unsafe(math.max(1, prepared.width / 2), math.max(1, prepared.height / 2))))
      ok(host.resize(dimensions))
      val resizedImage = rasterCanvas.applyDynamic("toDataURL")("image/png").asInstanceOf[String]
      js.Dynamic.literal(native = nativeReceipt,
        raster = js.Dynamic.literal(status = "captured", provider = "Canvas2D/reference-raster",
          image = rasterImage, coldNanos = first.renderNanos.toDouble, resizeBackExact = resizedImage == rasterImage,
          warmNanos = warm.renderNanos.toDouble,
          warmSamplesNanos = rasterWarmSamples.map(_.renderNanos.toDouble).toJSArray),
        viewMatrix = Vector.tabulate(16)(camera.viewMatrix.apply).toJSArray,
        projectionMatrix = Vector.tabulate(16)(camera.projectionMatrix.apply).toJSArray,
        viewCameras = prepared.plan.views.map(view => js.Dynamic.literal(id = view.id.value,
          viewMatrix = Vector.tabulate(16)(view.camera.viewMatrix.apply).toJSArray,
          projectionMatrix = Vector.tabulate(16)(view.camera.projectionMatrix.apply).toJSArray)).toJSArray,
        matrixOrder = "row-major", picks = picks, disposal = disposal,
        numericReadouts = prepared.numericReadouts.map((surface, layer, values) => js.Dynamic.literal(
          surface = surface.value, layer = layer.value, values = values.toJSArray)).toJSArray,
        markerCenters = prepared.markerCenters.map((id, surface, vertex, center) => js.Dynamic.literal(
          id = id.value, surface = surface.value, vertex = vertex.index, world = center.toJSArray)).toJSArray,
        sourceSummaries = prepared.sourceSummaries.map(s => js.Dynamic.literal(surface = s.surface.value, vertices = s.vertices, faces = s.faces)).toJSArray)
    finally
      host.dispose()
      ok(backend.dispose())
      disposal.canvasPresentedFrameReleased = host.presentedFrame.isEmpty
      disposal.backendResourceCount = backend.resourceKeys.size
