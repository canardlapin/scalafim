package scalafim.surface.view.three

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

/** Explicit real-browser diagnostic used by the backend acceptance harness.
  * It is inert unless a host calls the exported function.
  */
object ThreeBrowserProbe:
  /** Full live admission matrix. The returned array contains plain JSON
    * objects matching `scalafim.surface-benchmark.v1`.
    */
  @JSExportTopLevel("runScalafimThreeAdmissionMatrix")
  def runAdmissionMatrix(
    three: js.Dynamic,
    canvas: js.Dynamic,
    repetitions: Int
  ): js.Array[js.Any] =
    val samples = repetitions.max(1)
    val output = new js.Array[js.Any]()
    val baseSize = ThreeCanvasSize.unsafe(720, 540, 1.0)
    val metadata = browserMetadata(canvas)
    SurfaceBenchmarkMatrix.VertexCounts.foreach: vertices =>
      SurfaceBenchmarkMatrix.LayerCounts.foreach: layers =>
        val runtime = ThreeJsRuntime.create(three, canvas)
          .fold(error => throw new IllegalStateException(error.message), identity)
        val backend = ThreeSurfaceBackend.create(runtime)
          .fold(error => throw new IllegalStateException(error.message), identity)
        SurfaceBenchmarkMatrix.Paths.foreach: path =>
          val benchmark = SurfaceBenchmarkCase.unsafe(vertices, layers, path)
          val observations = Vector.newBuilder[SurfaceBackendObservation]
          val timings = Vector.newBuilder[Long]
          val count = if path == SurfaceAdmissionPath.ColdLoad then 1 else samples
          var repetition = 0
          while repetition < count do
            val base = SurfaceBenchmarkFixture.plan(vertices, layers)
            if path != SurfaceAdmissionPath.ColdLoad then
              backend.render(base, baseSize).fold(error => throw new IllegalStateException(error.message), identity)
            val target = SurfaceBenchmarkFixture.planFor(benchmark)
            val size =
              if path == SurfaceAdmissionPath.Resize then ThreeCanvasSize.unsafe(800, 600, 1.0)
              else baseSize
            val observed = backend.renderObserved(
              target,
              size,
              path,
              forceDraw = path == SurfaceAdmissionPath.Snapshot || path == SurfaceAdmissionPath.Pick
            ).fold(error => throw new IllegalStateException(error.message), identity)
            val enriched = enrichBrowserObservation(backend, canvas, size, target, path, observed.observation)
            val violations = SurfaceBackendAdmission.validate(enriched)
            require(violations.isEmpty, violations.map(_.problem).mkString("; "))
            observations += enriched
            timings += enriched.timings.iterator.map(_.elapsedNanos).sum
            repetition += 1
          val summary = SurfaceTimingSummary.from(SurfaceRenderPhase.RenderSubmission, timings.result())
            .fold(error => throw new IllegalStateException(error), identity)
          val receipt = SurfaceBenchmarkReceipt(
            benchmark,
            metadata,
            observations.result(),
            Vector(summary)
          )
          output.push(js.JSON.parse(SurfaceBenchmarkJson.encode(receipt)))
        backend.dispose().fold(error => throw new IllegalStateException(error.message), identity)
    output

  @JSExportTopLevel("runScalafimThreeProbe")
  def run(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val runtime = ThreeJsRuntime.create(three, canvas).fold(error => throw new IllegalStateException(error.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime).fold(error => throw new IllegalStateException(error.message), identity)
    val size = ThreeCanvasSize.unsafe(720, 540, 1.0)
    val small = profile(backend, size, 32768, 12)
    val large = profile(backend, size, 163842, 12)
    val gpuProjection = projectionParity(three)
    val stats = backend.stats
    js.Dynamic.literal(
      status = "pass",
      backend = "scalafim-three",
      context = runtime.contextState.toString,
      small = small,
      large = large,
      gpuProjection = gpuProjection,
      totalDrawCalls = stats.drawCalls.toDouble,
      totalGeometryUploads = stats.geometryUploads.toDouble,
      totalGeometryUpdates = stats.geometryUpdates.toDouble,
      totalColorUploads = stats.colorUploads.toDouble,
      totalUploadedBytes = stats.uploadedBytes.toDouble
    )

  private def projectionParity(three: js.Dynamic): js.Dynamic =
    val fixture = SurfaceFeatureFixture.projectionCase
    val cpu = SurfaceVolumeProjection.materialize(fixture.morphism, fixture.volume, fixture.policy)
    val document = js.Dynamic.global.document
    val canvas = document.applyDynamic("createElement")("canvas")
    val gpu = ThreeVolumeProjector.project(
      three,
      canvas,
      fixture.morphism,
      fixture.volume,
      fixture.policy
    ).fold(error => throw new IllegalStateException(error.message), identity)
    var maximumAbsoluteError = 0.0
    var countMismatches = 0
    var qualityMismatches = 0
    val cpuValues = new js.Array[Double]()
    val gpuValues = new js.Array[Double]()
    var vertex = 0
    while vertex < cpu.values.size do
      val id = VertexId.unsafe(vertex)
      val cpuValue = cpu.values.valueAt(id).get
      val gpuValue = gpu.projection.values.valueAt(id).get
      cpuValues.push(cpuValue)
      gpuValues.push(gpuValue)
      maximumAbsoluteError = math.max(maximumAbsoluteError, math.abs(cpuValue - gpuValue))
      if cpu.sampleCounts.valueAt(id) != gpu.projection.sampleCounts.valueAt(id) then countMismatches += 1
      if cpu.quality.valueAt(id) != gpu.projection.quality.valueAt(id) then qualityMismatches += 1
      vertex += 1
    val passed = maximumAbsoluteError <= 1e-5 && countMismatches == 0 && qualityMismatches == 0
    js.Dynamic.literal(
      status = if passed then "pass" else "fail",
      vertices = gpu.receipt.vertices,
      textureWidth = gpu.receipt.width,
      textureHeight = gpu.receipt.height,
      renderedPixels = gpu.receipt.renderedPixels,
      uploadedBytes = gpu.receipt.uploadedBytes.toDouble,
      elapsedMillis = gpu.receipt.elapsedNanos.toDouble / 1e6,
      maximumAbsoluteError = maximumAbsoluteError,
      countMismatches = countMismatches,
      qualityMismatches = qualityMismatches,
      cpuValues = cpuValues,
      gpuValues = gpuValues
    )

  private def profile(
    backend: ThreeSurfaceBackend,
    size: ThreeCanvasSize,
    vertexCount: Int,
    repetitions: Int
  ): js.Dynamic =
    val compileStarted = now()
    val fixture = fixtureFor(vertexCount)
    val plan = SurfaceCompiler.compile(fixture.model, fixture.initial)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val compileMillis = now() - compileStarted
    val uploadStarted = now()
    val initial = backend.render(plan, size).fold(error => throw new IllegalStateException(error.message), identity)
    val uploadAndFirstFrameMillis = now() - uploadStarted
    val frames = new Array[Double](repetitions)
    var repetition = 0
    while repetition < repetitions do
      val started = now()
      backend.render(plan, size, forceDraw = true).fold(error => throw new IllegalStateException(error.message), identity)
      frames(repetition) = now() - started
      repetition += 1
    val startedMorph = SurfaceViewer.reduce(
      fixture.model,
      fixture.initial,
      SurfaceViewerAction.BeginGeometryMorph(fixture.surface, SurfaceKind.Pial)
    ).fold(error => throw new IllegalStateException(error.message), identity)
    val morphFrames = new Array[Double](repetitions)
    var morphGeometryUploads = 0
    var morphGeometryUpdates = 0
    var morphColorUploads = 0
    var morphBytes = 0L
    repetition = 0
    while repetition < repetitions do
      val fraction = SurfaceMorphFraction.unsafe((repetition + 1).toDouble / repetitions.toDouble)
      val state = SurfaceViewer.reduce(
        fixture.model,
        startedMorph,
        SurfaceViewerAction.SetGeometryMorphFraction(fixture.surface, fraction)
      ).fold(error => throw new IllegalStateException(error.message), identity)
      val started = now()
      val morphPlan = SurfaceCompiler.compile(fixture.model, state)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val receipt = backend.render(morphPlan, size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      morphFrames(repetition) = now() - started
      morphGeometryUploads += receipt.geometryUploads
      morphGeometryUpdates += receipt.geometryUpdates
      morphColorUploads += receipt.colorUploads
      morphBytes += receipt.uploadedBytes
      repetition += 1
    val pickStarted = now()
    val pick = backend.pick(size.width / 2.0, size.height / 2.0).fold(error => throw new IllegalStateException(error.message), identity)
    val pickMillis = now() - pickStarted
    js.Dynamic.literal(
      vertices = vertexCount,
      faces = plan.profile.facesPacked,
      compileMillis = compileMillis,
      uploadAndFirstFrameMillis = uploadAndFirstFrameMillis,
      frameMedianMillis = percentile(frames, 0.5),
      frameP95Millis = percentile(frames, 0.95),
      morphMedianMillis = percentile(morphFrames, 0.5),
      morphP95Millis = percentile(morphFrames, 0.95),
      morphGeometryUploads = morphGeometryUploads,
      morphGeometryUpdates = morphGeometryUpdates,
      morphColorUploads = morphColorUploads,
      morphUploadedBytes = morphBytes.toDouble,
      pickMillis = pickMillis,
      pickedVertex = pick.map(_.vertex).getOrElse(-1),
      geometryUploads = initial.geometryUploads,
      colorUploads = initial.colorUploads,
      uploadedBytes = initial.uploadedBytes.toDouble,
      drawCalls = initial.drawCalls + repetitions * 2
    )

  private final case class ProfileFixture(
    surface: SurfaceId,
    model: SurfaceViewerModel,
    initial: SurfaceViewerState
  )

  private def fixtureFor(vertexCount: Int): ProfileFixture =
    val coordinates = new Array[Double](vertexCount * 3)
    val columns = if vertexCount <= 32768 then 256 else 512
    val rows = (vertexCount + columns - 1) / columns
    var vertex = 0
    while vertex < vertexCount do
      val row = vertex / columns
      val column = vertex % columns
      val latitude = -math.Pi * 0.5 + math.Pi * row.toDouble / math.max(1, rows - 1).toDouble
      val longitude = 2.0 * math.Pi * column.toDouble / math.max(1, columns - 1).toDouble
      val radius = math.cos(latitude)
      val offset = vertex * 3
      coordinates(offset) = 0.75 * radius * math.cos(longitude)
      coordinates(offset + 1) = radius * math.sin(longitude)
      coordinates(offset + 2) = 0.82 * math.sin(latitude)
      vertex += 1
    var cellCount = 0
    var row = 0
    while row + 1 < rows do
      var column = 0
      while column + 1 < columns do
        val lowerRight = (row + 1) * columns + column + 1
        if lowerRight < vertexCount then cellCount += 1
        column += 1
      row += 1
    val indices = new Array[Int](cellCount * 6)
    var index = 0
    row = 0
    while row + 1 < rows do
      var column = 0
      while column + 1 < columns do
        val a = row * columns + column
        val b = a + 1
        val c = (row + 1) * columns + column
        val d = c + 1
        if d < vertexCount then
          indices(index) = a
          indices(index + 1) = b
          indices(index + 2) = c
          indices(index + 3) = b
          indices(index + 4) = d
          indices(index + 5) = c
          index += 6
        column += 1
      row += 1
    val geometry = SurfaceGeometry(
      TriangleMesh.fromArrays(coordinates, indices),
      Hemisphere.Left,
      SurfaceKind.Inflated
    )
    val foldedCoordinates = coordinates.clone()
    vertex = 0
    while vertex < vertexCount do
      val offset = vertex * 3
      foldedCoordinates(offset) *= 0.82
      foldedCoordinates(offset + 2) += 0.08 * math.sin(vertex.toDouble * 0.031)
      vertex += 1
    val folded = SurfaceGeometry(
      TriangleMesh.fromArrays(foldedCoordinates, indices.clone()),
      Hemisphere.Left,
      SurfaceKind.Pial
    )
    val surfaceId = SurfaceId.unsafe(s"profile-$vertexCount")
    val layerId = SurfaceLayerId.unsafe("curvature")
    val values = Array.tabulate(vertexCount)(index => math.sin(index.toDouble * 0.013))
    val curvature = SurfaceField.full(folded, values.toIndexedSeq, "folded curvature")
    val layer = SurfaceLayer.curvatureUnderlay(
      layerId,
      surfaceId,
      curvature,
      geometry
    ).toOption.get
    val model = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(
        surfaceId,
        SurfaceSet.of(SurfaceKind.Inflated, geometry, SurfaceKind.Pial -> folded)
      ).toOption.get),
      Vector(layer)
    ).toOption.get
    ProfileFixture(surfaceId, model, SurfaceViewerState.initial(model))

  private def percentile(values: Array[Double], fraction: Double): Double =
    val sorted = values.sorted
    sorted(math.min(sorted.length - 1, math.floor((sorted.length - 1) * fraction).toInt))

  private def now(): Double =
    js.Dynamic.global.performance.applyDynamic("now")().asInstanceOf[Double]

  private def enrichBrowserObservation(
    backend: ThreeSurfaceBackend,
    canvas: js.Dynamic,
    size: ThreeCanvasSize,
    plan: SurfaceRenderPlan,
    path: SurfaceAdmissionPath,
    observation: SurfaceBackendObservation
  ): SurfaceBackendObservation =
    path match
      case SurfaceAdmissionPath.Snapshot =>
        val started = now()
        canvas.applyDynamic("toDataURL")("image/png")
        observation.copy(timings = observation.timings :+ SurfacePhaseTiming.unsafe(
          SurfaceRenderPhase.Snapshot,
          ((now() - started) * 1e6).toLong
        ))
      case SurfaceAdmissionPath.Pick =>
        val started = now()
        val picked = backend.pick(size.width / 2.0, size.height / 2.0)
          .fold(error => throw new IllegalStateException(error.message), identity)
        val elapsed = ((now() - started) * 1e6).toLong
        val event = picked.map(value => SurfaceResourceEvent.Picked(value.surface, value.face, value.vertex)).toVector
        observation.copy(
          events = observation.events ++ event,
          timings = observation.timings :+ SurfacePhaseTiming.unsafe(SurfaceRenderPhase.Pick, elapsed)
        )
      case _ => observation

  private def browserMetadata(canvas: js.Dynamic): SurfaceRuntimeMetadata =
    val navigator = js.Dynamic.global.navigator
    val userAgent =
      if js.isUndefined(navigator) then "unknown"
      else navigator.userAgent.asInstanceOf[String]
    val renderer =
      try
        val context = canvas.applyDynamic("getContext")("webgl2")
        if context == null then "unknown"
        else context.applyDynamic("getParameter")(context.RENDERER).toString
      catch case _: Throwable => "unknown"
    SurfaceRuntimeMetadata(
      SurfaceRuntimePlatform.ScalaJs,
      "browser",
      userAgent,
      userAgent,
      "browser",
      "WebGL2",
      renderer
    )
