package scalafim.examples.surfaceview

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel
import scala.scalajs.js.typedarray.{Float32Array, Uint8Array, Uint32Array}

import intaglio.*
import scalafim.image.DMat
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*
import scalafim.surface.view.three.*

object ThreeSurfaceViewerExample:
  private final case class NativeCanvasComparison(
    visualQa: SurfaceVisualQaReceipt,
    pick: SurfacePick
  )

  @JSExportTopLevel("mountScalafimSurfaceViewerExample")
  def mount(three: js.Dynamic, canvas: js.Dynamic): js.Dynamic =
    val example = SurfaceViewerExample.portable
    val runtime = ThreeJsRuntime.create(three, canvas)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val dimensions = SurfaceViewerVisualQaFixture.Dimensions
    val size = ThreeCanvasSize.unsafe(dimensions.width, dimensions.height, 1.0)
    val render = backend.render(example.plan, size)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val comparison = compareNativeCanvas(example, canvas, size)
    val visualQa = comparison.visualQa
    val visualViolations = visualQa.violations(SurfaceViewerVisualQaFixture.Policy)
    val pickX = SurfaceViewerVisualQaFixture.InteriorPickX.toDouble
    val pickY = SurfaceViewerVisualQaFixture.InteriorPickY.toDouble
    val pick = backend.pick(pickX, pickY)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val semantic = SurfaceViewerExample.semanticReceipt(example)
    js.Dynamic.literal(
      schema = semantic.schema,
      backend = backend.capabilities.id.value,
      source = semantic.source,
      vertices = semantic.vertices,
      faces = semantic.faces,
      layerOrder = js.Array(semantic.layerOrder*),
      slotOrder = js.Array(semantic.slotOrder*),
      selectedSurface = semantic.selectedSurface,
      selectedVertex = semantic.selectedVertex,
      referenceImageHash = semantic.imageHash,
      referencePickedSurface = semantic.pickedSurface,
      referencePickedVertex = semantic.pickedVertex,
      screenReferencePickedSurface = comparison.pick.surface.value,
      screenReferencePickedFace = comparison.pick.face,
      screenReferencePickedVertex = comparison.pick.vertex,
      webglPickedSurface = pick.map(_.surface.value).orNull,
      webglPickedFace = pick.map(_.face).getOrElse(-1),
      webglPickedVertex = pick.map(_.vertex).getOrElse(-1),
      geometryUploads = render.geometryUploads,
      colorUploads = render.colorUploads,
      uploadedBytes = render.uploadedBytes.toDouble,
      drawCalls = render.drawCalls,
      elapsedMillis = render.elapsedNanos.toDouble / 1e6,
      visualQa = js.Dynamic.literal(
        expectedForegroundPixels = visualQa.expectedForegroundPixels,
        observedForegroundPixels = visualQa.observedForegroundPixels,
        expectedLeftForegroundPixels = visualQa.expectedLeftForegroundPixels,
        expectedRightForegroundPixels = visualQa.expectedRightForegroundPixels,
        observedLeftForegroundPixels = visualQa.observedLeftForegroundPixels,
        observedRightForegroundPixels = visualQa.observedRightForegroundPixels,
        maskIntersectionOverUnion = visualQa.maskIntersectionOverUnion,
        centroidDistancePixels = visualQa.centroidDistancePixels,
        interiorPixelsCompared = visualQa.interiorPixelsCompared,
        meanInteriorChannelError = visualQa.meanInteriorChannelError,
        maximumInteriorChannelError = visualQa.maximumInteriorChannelError,
        violations = js.Array(visualViolations*)
      )
    )

  @JSExportTopLevel("mountScalafimCorticalSurfaceViewer")
  def mountCortical(
    three: js.Dynamic,
    canvas: js.Dynamic,
    leftVertices: Float32Array,
    leftFaces: Uint32Array,
    rightVertices: Float32Array,
    rightFaces: Uint32Array
  ): js.Dynamic =
    val left = geometry(leftVertices, leftFaces, Hemisphere.Left, SurfaceKind.Pial)
    val right = geometry(rightVertices, rightFaces, Hemisphere.Right, SurfaceKind.Pial)
    val example = CorticalSurfaceAcceptance.build(left, right)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val runtime = ThreeJsRuntime.create(three, canvas)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val dimensions = CorticalSurfaceAcceptance.Dimensions
    val size = ThreeCanvasSize.unsafe(dimensions.width, dimensions.height, 1.0)
    val referenceStarted = System.nanoTime()
    val reference = CorticalSurfaceAcceptance.reference(example)
    val referenceMillis = (System.nanoTime() - referenceStarted).toDouble / 1e6
    val (pickX, pickY, referencePick) = CorticalSurfaceAcceptance.landmark(reference)
    val cold = backend.renderObserved(example.plan, size, SurfaceAdmissionPath.ColdLoad)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val observed = canvasImage(canvas, dimensions)
    val visualQa = SurfaceVisualQa.compare(reference.image, observed)
      .fold(error => throw new IllegalStateException(error.message), identity)
    // Raster picks represent the center of an integer pixel. Native ray
    // coordinates are continuous canvas coordinates, so compare at the same
    // half-pixel location rather than the pixel's upper-left edge.
    val nativePick = backend.pick(pickX.toDouble + 0.5, pickY.toDouble + 0.5)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val nativeFaceCompatible = nativePick.exists: pick =>
      pick.face == referencePick.face || faceContainsVertex(example.plan, pick.surface, pick.face, referencePick.vertex)
    val cameraPlan = CorticalSurfaceAcceptance.cameraOnlyPlan(example)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val camera = backend.renderObserved(cameraPlan, size, SurfaceAdmissionPath.CameraOnly)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val cameraViolations = SurfaceBackendAdmission.validate(camera.observation).map(_.problem)
    val visualViolations = visualQa.violations(CorticalSurfaceAcceptance.Policy)
    js.Dynamic.literal(
      schema = "scalafim.surface-cortical-acceptance.v1",
      backend = backend.capabilities.id.value,
      leftVertices = left.vertexCount,
      leftFaces = left.faceCount,
      rightVertices = right.vertexCount,
      rightFaces = right.faceCount,
      drawCalls = cold.native.drawCalls,
      geometryUploads = cold.native.geometryUploads,
      colorUploads = cold.native.colorUploads,
      uploadedBytes = cold.native.uploadedBytes.toDouble,
      coldRenderMillis = cold.native.elapsedNanos.toDouble / 1e6,
      referenceRasterMillis = referenceMillis,
      cameraGeometryUploads = camera.observation.geometryUploads,
      cameraLayerUploads = camera.observation.layerUploads,
      cameraRenderMillis = camera.native.elapsedNanos.toDouble / 1e6,
      cameraViolations = js.Array(cameraViolations*),
      landmarkX = pickX,
      landmarkY = pickY,
      referencePickedSurface = referencePick.surface.value,
      referencePickedFace = referencePick.face,
      referencePickedVertex = referencePick.vertex,
      nativePickedSurface = nativePick.map(_.surface.value).orNull,
      nativePickedFace = nativePick.map(_.face).getOrElse(-1),
      nativePickedVertex = nativePick.map(_.vertex).getOrElse(-1),
      nativeFaceCompatible = nativeFaceCompatible,
      visualQa = visualQaLiteral(visualQa, visualViolations)
    )

  @JSExportTopLevel("mountScalafimCorticalMorphViewer")
  def mountCorticalMorph(
    three: js.Dynamic,
    canvas: js.Dynamic,
    leftWhiteVertices: Float32Array,
    leftWhiteFaces: Uint32Array,
    leftPialVertices: Float32Array,
    leftPialFaces: Uint32Array,
    leftInflatedVertices: Float32Array,
    leftInflatedFaces: Uint32Array,
    rightWhiteVertices: Float32Array,
    rightWhiteFaces: Uint32Array,
    rightPialVertices: Float32Array,
    rightPialFaces: Uint32Array,
    rightInflatedVertices: Float32Array,
    rightInflatedFaces: Uint32Array
  ): js.Dynamic =
    val example = CorticalSurfaceMorphAcceptance.build(
      geometry(leftWhiteVertices, leftWhiteFaces, Hemisphere.Left, SurfaceKind.White),
      geometry(leftPialVertices, leftPialFaces, Hemisphere.Left, SurfaceKind.Pial),
      geometry(leftInflatedVertices, leftInflatedFaces, Hemisphere.Left, SurfaceKind.Inflated),
      geometry(rightWhiteVertices, rightWhiteFaces, Hemisphere.Right, SurfaceKind.White),
      geometry(rightPialVertices, rightPialFaces, Hemisphere.Right, SurfaceKind.Pial),
      geometry(rightInflatedVertices, rightInflatedFaces, Hemisphere.Right, SurfaceKind.Inflated)
    ).fold(error => throw new IllegalStateException(error.message), identity)
    val cases = CorticalSurfaceMorphAcceptance.cases(example)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val runtime = ThreeJsRuntime.create(three, canvas)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val backend = ThreeSurfaceBackend.create(runtime)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val dimensions = CorticalSurfaceAcceptance.Dimensions
    val size = ThreeCanvasSize.unsafe(dimensions.width, dimensions.height, 1.0)
    val rows = new js.Array[js.Dynamic]()
    var index = 0
    while index < cases.length do
      val current = cases(index)
      val reference = SurfaceRasterizer.render(
        current.plan,
        dimensions,
        SurfaceRasterStyle(culling = TriangleCulling.None)
      ).fold(error => throw new IllegalStateException(error.message), identity)
      val render = backend.render(current.plan, size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val observed = canvasImage(canvas, dimensions)
      val visualQa = SurfaceVisualQa.compare(reference.image, observed)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val violations = visualQa.violations(CorticalSurfaceAcceptance.Policy)
      rows.push(js.Dynamic.literal(
        label = current.label,
        from = current.from.label,
        to = current.to.label,
        fraction = current.fraction.value,
        geometryUploads = render.geometryUploads,
        geometryUpdates = render.geometryUpdates,
        colorUploads = render.colorUploads,
        uploadedBytes = render.uploadedBytes.toDouble,
        elapsedMillis = render.elapsedNanos.toDouble / 1e6,
        meshBounds = js.Array(current.plan.meshes.map(meshBounds)*),
        visualQa = visualQaLiteral(visualQa, violations)
      ))
      index += 1
    js.Dynamic.literal(
      schema = "scalafim.surface-cortical-morph-acceptance.v1",
      backend = backend.capabilities.id.value,
      cases = rows
    )

  @JSExportTopLevel("mountScalafimThresholdParityPlate")
  def mountThresholdParityPlate(
    three: js.Dynamic,
    canvases: js.Array[js.Dynamic],
    leftVertices: Float32Array,
    leftFaces: Uint32Array
  ): js.Dynamic =
    require(canvases.length == SurfaceThresholdParityFixture.Viewpoints.length,
      s"expected ${SurfaceThresholdParityFixture.Viewpoints.length} canvases; got ${canvases.length}")
    val surface = geometry(leftVertices, leftFaces, Hemisphere.Left, SurfaceKind.Pial)
    val fixture = SurfaceThresholdParityFixture.build(surface)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val dimensions = SurfaceThresholdParityFixture.Dimensions
    val size = ThreeCanvasSize.unsafe(dimensions.width, dimensions.height, 1.0)
    val rows = new js.Array[js.Dynamic]()
    var index = 0
    while index < fixture.views.length do
      val view = fixture.views(index)
      val canvas = canvases(index)
      val runtime = ThreeJsRuntime.create(three, canvas)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val backend = ThreeSurfaceBackend.create(runtime)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val referenceStarted = System.nanoTime()
      val reference = SurfaceThresholdParityFixture.reference(view)
      val referenceMillis = (System.nanoTime() - referenceStarted).toDouble / 1e6
      val render = backend.render(view.plan, size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val observed = canvasImage(canvas, dimensions)
      val visual = SurfaceVisualQa.compare(reference.image, observed)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val orientation = SurfaceThresholdParityFixture.orientationQa(reference.image, observed)
      val violations = singleSurfaceViolations(visual, orientation)
      rows.push(js.Dynamic.literal(
        label = view.label,
        cameraDirection = js.Array(
          view.plan.camera.directionX,
          view.plan.camera.directionY,
          view.plan.camera.directionZ
        ),
        geometryUploads = render.geometryUploads,
        colorUploads = render.colorUploads,
        uploadedBytes = render.uploadedBytes.toDouble,
        elapsedMillis = render.elapsedNanos.toDouble / 1e6,
        referenceRasterMillis = referenceMillis,
        orientationQa = orientationQaLiteral(orientation),
        visualQa = visualQaLiteral(visual, violations)
      ))
      index += 1
    js.Dynamic.literal(
      schema = "scalafim.surface-threshold-parity.v1",
      backend = "three-webgl2",
      source = CorticalSurfaceAcceptance.LeftCorpus.fileName,
      seed = SurfaceThresholdParityFixture.Seed,
      smoothingIterations = SurfaceThresholdParityFixture.SmoothingIterations,
      thresholdLower = -SurfaceThresholdParityFixture.Threshold,
      thresholdUpper = SurfaceThresholdParityFixture.Threshold,
      visibleNegativeVertices = fixture.visibleNegativeVertices,
      visiblePositiveVertices = fixture.visiblePositiveVertices,
      vertices = surface.vertexCount,
      faces = surface.faceCount,
      cases = rows
    )

  @JSExportTopLevel("mountScalafimCorticalLensPlate")
  def mountCorticalLensPlate(
    three: js.Dynamic,
    canvases: js.Array[js.Dynamic],
    pialVertices: Float32Array,
    pialFaces: Uint32Array,
    inflatedVertices: Float32Array,
    inflatedFaces: Uint32Array
  ): js.Dynamic =
    require(canvases.length == CorticalSurfaceLensAcceptance.Fractions.length,
      s"expected ${CorticalSurfaceLensAcceptance.Fractions.length} canvases; got ${canvases.length}")
    val buildStarted = System.nanoTime()
    val pial = geometry(pialVertices, pialFaces, Hemisphere.Left, SurfaceKind.Pial)
    val inflated = geometry(inflatedVertices, inflatedFaces, Hemisphere.Left, SurfaceKind.Inflated)
    val example = CorticalSurfaceLensAcceptance.build(pial, inflated)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val buildMillis = (System.nanoTime() - buildStarted).toDouble / 1e6
    val dimensions = CorticalSurfaceLensAcceptance.Dimensions
    val size = ThreeCanvasSize.unsafe(dimensions.width, dimensions.height, 1.0)
    val rows = new js.Array[js.Dynamic]()
    var index = 0
    while index < example.cases.length do
      val current = example.cases(index)
      val canvas = canvases(index)
      val runtime = ThreeJsRuntime.create(three, canvas)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val backend = ThreeSurfaceBackend.create(runtime)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val reference = CorticalSurfaceLensAcceptance.reference(current)
      val render = backend.render(current.plan, size)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val observed = canvasImage(canvas, dimensions)
      val visual = SurfaceVisualQa.compare(reference.image, observed)
        .fold(error => throw new IllegalStateException(error.message), identity)
      val orientation = SurfaceThresholdParityFixture.orientationQa(reference.image, observed)
      val violations = singleSurfaceViolations(visual, orientation)
      rows.push(js.Dynamic.literal(
        label = current.label,
        fraction = current.fraction.value,
        geometryUploads = render.geometryUploads,
        geometryUpdates = render.geometryUpdates,
        colorUploads = render.colorUploads,
        uploadedBytes = render.uploadedBytes.toDouble,
        elapsedMillis = render.elapsedNanos.toDouble / 1e6,
        bounds = meshBounds(current.plan.meshes.head),
        guide = js.Dynamic.literal(
          centerX = current.guide.centerX,
          centerY = current.guide.centerY,
          innerRadiusPixels = current.guide.innerRadiusPixels,
          outerRadiusPixels = current.guide.outerRadiusPixels
        ),
        orientationQa = orientationQaLiteral(orientation),
        visualQa = visualQaLiteral(visual, violations)
      ))
      index += 1
    js.Dynamic.literal(
      schema = "scalafim.surface-cortical-lens.v1",
      backend = "three-webgl2",
      centerVertex = example.center.index,
      activeVertices = example.activeVertices,
      quality = js.Dynamic.literal(
        invertedTriangles = example.quality.invertedTriangles,
        minimumAreaRatio = example.quality.minimumAreaRatio,
        maximumAreaRatio = example.quality.maximumAreaRatio,
        maximumEdgeStrain = example.quality.maximumEdgeStrain,
        p95EdgeStrain = example.quality.p95EdgeStrain
      ),
      innerRadius = CorticalSurfaceLensAcceptance.InnerRadius.value,
      outerRadius = CorticalSurfaceLensAcceptance.OuterRadius.value,
      visibleNegativeVertices = example.visibleNegativeVertices,
      visiblePositiveVertices = example.visiblePositiveVertices,
      buildMillis = buildMillis,
      cases = rows
    )

  private def compareNativeCanvas(
    example: SurfaceViewerExample,
    canvas: js.Dynamic,
    size: ThreeCanvasSize
  ): NativeCanvasComparison =
    val dimensions = SurfaceViewerVisualQaFixture.Dimensions
    val reference = SurfaceViewerVisualQaFixture.reference(example)
    val observed = canvasImage(canvas, dimensions)
    val visualQa = SurfaceVisualQa.compare(reference.image, observed)
      .fold(error => throw new IllegalStateException(error.message), identity)
    val referencePick = SurfaceViewerVisualQaFixture.referencePick(reference)
    NativeCanvasComparison(visualQa, referencePick)

  private def canvasImage(canvas: js.Dynamic, dimensions: RasterDimensions): RasterImage =
    val context = canvas.applyDynamic("getContext")("webgl2")
    if context == null || js.isUndefined(context) then
      throw new IllegalStateException("WebGL2 context is unavailable for visual QA")
    context.applyDynamic("finish")()
    val bytes = new Uint8Array(dimensions.pixelCount * 4)
    context.applyDynamic("readPixels")(
      0,
      0,
      dimensions.width,
      dimensions.height,
      context.selectDynamic("RGBA"),
      context.selectDynamic("UNSIGNED_BYTE"),
      bytes
    )
    RasterImage.tabulate(dimensions): (x, y) =>
      val sourceY = dimensions.height - 1 - y
      val offset = (sourceY * dimensions.width + x) * 4
      Rgba32.unsafe(bytes(offset), bytes(offset + 1), bytes(offset + 2), bytes(offset + 3))

  private def geometry(
    vertices: Float32Array,
    faces: Uint32Array,
    hemisphere: Hemisphere,
    kind: SurfaceKind
  ): SurfaceGeometry =
    val coordinates = new Array[Double](vertices.length)
    var index = 0
    while index < coordinates.length do
      coordinates(index) = vertices(index).toDouble
      index += 1
    val indices = new Array[Int](faces.length)
    index = 0
    while index < indices.length do
      indices(index) = faces(index).toInt
      index += 1
    SurfaceGeometry(
      TriangleMesh.fromArrays(coordinates, indices),
      hemisphere,
      kind,
      DMat.eye(4)
    )

  private def visualQaLiteral(receipt: SurfaceVisualQaReceipt, violations: Vector[String]): js.Dynamic =
    js.Dynamic.literal(
      expectedForegroundPixels = receipt.expectedForegroundPixels,
      observedForegroundPixels = receipt.observedForegroundPixels,
      expectedLeftForegroundPixels = receipt.expectedLeftForegroundPixels,
      expectedRightForegroundPixels = receipt.expectedRightForegroundPixels,
      observedLeftForegroundPixels = receipt.observedLeftForegroundPixels,
      observedRightForegroundPixels = receipt.observedRightForegroundPixels,
      maskIntersectionOverUnion = receipt.maskIntersectionOverUnion,
      centroidDistancePixels = receipt.centroidDistancePixels,
      interiorPixelsCompared = receipt.interiorPixelsCompared,
      meanInteriorChannelError = receipt.meanInteriorChannelError,
      maximumInteriorChannelError = receipt.maximumInteriorChannelError,
      violations = js.Array(violations*)
    )

  private def singleSurfaceViolations(
    receipt: SurfaceVisualQaReceipt,
    orientation: SurfaceOrientationQaReceipt
  ): Vector[String] =
    val failures = Vector.newBuilder[String]
    if receipt.expectedForegroundPixels < 8000 then failures += "reference fixture does not cover enough pixels"
    if receipt.observedForegroundPixels < 8000 then failures += "native rendering does not cover enough pixels"
    if receipt.maskIntersectionOverUnion < 0.90 then
      failures += f"foreground mask IoU ${receipt.maskIntersectionOverUnion}%.6f is below 0.900000"
    if receipt.centroidDistancePixels > 3.0 then
      failures += f"foreground centroid distance ${receipt.centroidDistancePixels}%.6f exceeds 3.000000 pixels"
    if receipt.interiorPixelsCompared == 0 then failures += "no interior pixels were available for color comparison"
    else if receipt.meanInteriorChannelError > 40.0 then
      failures += f"mean interior channel error ${receipt.meanInteriorChannelError}%.6f exceeds 40.000000"
    if orientation.margin <= 3.0 then
      failures += f"direct orientation beats its closest flip by only ${orientation.margin}%.6f channels"
    failures.result()

  private def orientationQaLiteral(receipt: SurfaceOrientationQaReceipt): js.Dynamic =
    js.Dynamic.literal(
      directMeanChannelError = receipt.directMeanChannelError,
      horizontalFlipMeanChannelError = receipt.horizontalFlipMeanChannelError,
      verticalFlipMeanChannelError = receipt.verticalFlipMeanChannelError,
      rotation180MeanChannelError = receipt.rotation180MeanChannelError,
      minimumAlternativeError = receipt.minimumAlternativeError,
      margin = receipt.margin
    )

  private def meshBounds(mesh: SurfaceMeshPacket): js.Dynamic =
    var minimumX = Float.PositiveInfinity
    var minimumY = Float.PositiveInfinity
    var minimumZ = Float.PositiveInfinity
    var maximumX = Float.NegativeInfinity
    var maximumY = Float.NegativeInfinity
    var maximumZ = Float.NegativeInfinity
    var offset = 0
    while offset < mesh.positions.length do
      minimumX = math.min(minimumX, mesh.positions(offset))
      minimumY = math.min(minimumY, mesh.positions(offset + 1))
      minimumZ = math.min(minimumZ, mesh.positions(offset + 2))
      maximumX = math.max(maximumX, mesh.positions(offset))
      maximumY = math.max(maximumY, mesh.positions(offset + 1))
      maximumZ = math.max(maximumZ, mesh.positions(offset + 2))
      offset += 3
    js.Dynamic.literal(
      surface = mesh.surface.value,
      minimum = js.Array(minimumX, minimumY, minimumZ),
      maximum = js.Array(maximumX, maximumY, maximumZ)
    )

  private def faceContainsVertex(
    plan: SurfaceRenderPlan,
    surface: SurfaceId,
    face: Int,
    vertex: Int
  ): Boolean =
    plan.meshes.find(_.surface == surface).exists: mesh =>
      val offset = face * 3
      offset >= 0 && offset + 2 < mesh.indices.length &&
        (mesh.indices(offset) == vertex || mesh.indices(offset + 1) == vertex || mesh.indices(offset + 2) == vertex)
