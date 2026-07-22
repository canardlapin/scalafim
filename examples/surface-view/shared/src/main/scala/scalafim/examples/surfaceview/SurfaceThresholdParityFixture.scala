package scalafim.examples.surfaceview

import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

final case class SurfaceThresholdView(
  label: String,
  viewpoint: SurfaceViewpoint,
  plan: SurfaceRenderPlan
)

final case class SurfaceThresholdParity(
  model: SurfaceViewerModel,
  state: SurfaceViewerState,
  noise: Array[Double],
  visibleNegativeVertices: Int,
  visiblePositiveVertices: Int,
  views: Vector[SurfaceThresholdView]
)

final case class SurfaceOrientationQaReceipt(
  directMeanChannelError: Double,
  horizontalFlipMeanChannelError: Double,
  verticalFlipMeanChannelError: Double,
  rotation180MeanChannelError: Double
):
  def minimumAlternativeError: Double =
    math.min(horizontalFlipMeanChannelError, math.min(verticalFlipMeanChannelError, rotation180MeanChannelError))

  def margin: Double = minimumAlternativeError - directMeanChannelError

/** One portable scientific/display contract for native and browser visual QA.
  * The generated field is spatially smoothed, fixed-seed Gaussian noise. Both
  * native backends therefore receive identical geometry, layers, thresholds,
  * camera directions, projection, and unlit color packets.
  */
object SurfaceThresholdParityFixture:
  val Surface: SurfaceId = SurfaceId.unsafe("threshold-left")
  val Curvature: SurfaceLayerId = SurfaceLayerId.unsafe("curvature")
  val Noise: SurfaceLayerId = SurfaceLayerId.unsafe("noise-z")
  val Seed: Int = 0x13579bdf
  val SmoothingIterations: Int = 5
  val Threshold: Double = 2.33
  val Dimensions: RasterDimensions = RasterDimensions.unsafe(600, 450)
  val ProjectionScale: Double = 105.0

  val Viewpoints: Vector[(String, SurfaceViewpoint)] = Vector(
    "Ventral" -> SurfaceViewpoint.Ventral,
    "Lateral" -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left),
    "Posterior" -> SurfaceViewpoint.Posterior,
    "Top" -> SurfaceViewpoint.Dorsal
  )

  def build(geometry: SurfaceGeometry): Either[SurfaceViewError, SurfaceThresholdParity] =
    val topology = MeshTopology.from(geometry)
    val noise = deterministicNoise(topology, Seed, SmoothingIterations)
    val curvature = curvatureValues(topology)
    val curvatureField = SurfaceField.full(geometry, curvature, "umbrella-Laplacian curvature underlay")
    val threshold = DisplayThreshold.transparentBand(-Threshold, Threshold).toOption.get
    val ramp = ColorRamp(
      Rgba32.unsafe(32, 92, 224),
      Rgba32.unsafe(235, 56, 38)
    )
    for
      underlay <- SurfaceLayer.curvatureUnderlay(Curvature, Surface, curvatureField, geometry)
      overlay <- SurfaceLayer.scalar(
        Noise,
        Surface,
        geometry,
        noise,
        ScalarColorizer(DisplayWindow.unsafe(-4.0, 4.0), ramp)
      )
      asset <- SurfaceAsset.make(Surface, geometry)
      model <- SurfaceViewerModel.make(Vector(asset), Vector(underlay, overlay))
      base <- commonActions(threshold).foldLeft[Either[SurfaceViewError, SurfaceViewerState]](
        Right(SurfaceViewerState.initial(model))
      )((current, action) => current.flatMap(SurfaceViewer.reduce(model, _, action)))
      views <- compileViews(model, base)
    yield
      var negative = 0
      var positive = 0
      var vertex = 0
      while vertex < noise.length do
        if noise(vertex) <= -Threshold then negative += 1
        else if noise(vertex) >= Threshold then positive += 1
        vertex += 1
      SurfaceThresholdParity(model, base, noise, negative, positive, views)

  def reference(view: SurfaceThresholdView): SurfaceRasterResult =
    SurfaceRasterizer.render(
      view.plan,
      Dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).fold(error => throw new IllegalStateException(error.message), identity)

  def orientationQa(reference: RasterImage, observed: RasterImage): SurfaceOrientationQaReceipt =
    def error(candidate: RasterImage): Double =
      SurfaceVisualQa.compare(candidate, observed)
        .fold(failure => throw new IllegalArgumentException(failure.message), identity)
        .meanInteriorChannelError
    val dimensions = reference.dimensions
    val horizontal = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(dimensions.width - 1 - x, y)
    val vertical = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(x, dimensions.height - 1 - y)
    val rotation = RasterImage.tabulate(dimensions): (x, y) =>
      reference.pixelUnsafe(dimensions.width - 1 - x, dimensions.height - 1 - y)
    SurfaceOrientationQaReceipt(error(reference), error(horizontal), error(vertical), error(rotation))

  def deterministicNoise(
    topology: MeshTopology,
    seed: Int = Seed,
    smoothingIterations: Int = SmoothingIterations
  ): Array[Double] =
    require(smoothingIterations >= 0, "smoothing iterations must be non-negative")
    val values = gaussianNoise(topology.mesh.vertexCount, seed)
    var source = values
    var iteration = 0
    while iteration < smoothingIterations do
      val target = new Array[Double](source.length)
      var vertex = 0
      while vertex < source.length do
        val neighbors = topology.neighborsOf(VertexId(vertex))
        if neighbors.isEmpty then target(vertex) = source(vertex)
        else
          var sum = 0.0
          var neighbor = 0
          while neighbor < neighbors.length do
            sum += source(neighbors(neighbor).index)
            neighbor += 1
          target(vertex) = 0.5 * source(vertex) + 0.5 * sum / neighbors.length
        vertex += 1
      source = target
      iteration += 1
    standardize(source)

  /** Signed umbrella-Laplacian curvature normalized to the conventional
    * underlay window. Unlike a world-axis normal component, this definition
    * cannot make one anatomical camera systematically darker than another.
    */
  def curvatureValues(topology: MeshTopology): Vector[Double] =
    val normals = topology.vertexNormals
    val raw = new Array[Double](topology.mesh.vertexCount)
    var vertex = 0
    while vertex < raw.length do
      val point = topology.mesh.vertex(VertexId(vertex))
      val neighbors = topology.neighborsOf(VertexId(vertex))
      if neighbors.nonEmpty then
        var x = 0.0
        var y = 0.0
        var z = 0.0
        var neighbor = 0
        while neighbor < neighbors.length do
          val adjacent = topology.mesh.vertex(neighbors(neighbor))
          x += adjacent.x
          y += adjacent.y
          z += adjacent.z
          neighbor += 1
        val inverse = 1.0 / neighbors.length
        val delta = Point3D(x * inverse - point.x, y * inverse - point.y, z * inverse - point.z)
        raw(vertex) = delta.dot(normals(vertex))
      vertex += 1
    val normalized = standardizedOrZero(raw)
    normalized.iterator.map(value => math.max(-1.0, math.min(1.0, value / 2.5))).toVector

  private def commonActions(threshold: DisplayThreshold): Vector[SurfaceViewerAction] = Vector(
    SurfaceViewerAction.SetLayout(SurfaceLayout.Single(Surface)),
    SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(ProjectionScale))),
    SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
    SurfaceViewerAction.SetLayerThreshold(Noise, threshold)
  )

  private def compileViews(
    model: SurfaceViewerModel,
    base: SurfaceViewerState
  ): Either[SurfaceViewError, Vector[SurfaceThresholdView]] =
    val result = Vector.newBuilder[SurfaceThresholdView]
    var index = 0
    while index < Viewpoints.length do
      val (label, viewpoint) = Viewpoints(index)
      val compiled = for
        state <- SurfaceViewer.reduce(model, base, SurfaceViewerAction.SetViewpoint(viewpoint))
        plan <- SurfaceCompiler.compile(model, state)
      yield SurfaceThresholdView(label, viewpoint, plan)
      compiled match
        case Left(error) => return Left(error)
        case Right(view) => result += view
      index += 1
    Right(result.result())

  private def gaussianNoise(count: Int, initialSeed: Int): Array[Double] =
    val result = new Array[Double](count)
    var state = if initialSeed == 0 then 0x6d2b79f5 else initialSeed
    def uniform(): Double =
      state ^= state << 13
      state ^= state >>> 17
      state ^= state << 5
      ((state >>> 8).toDouble + 0.5) / 16777216.0
    var index = 0
    while index < count do
      val radius = math.sqrt(-2.0 * math.log(uniform()))
      val angle = 2.0 * math.Pi * uniform()
      result(index) = radius * math.cos(angle)
      if index + 1 < count then result(index + 1) = radius * math.sin(angle)
      index += 2
    result

  private def standardize(values: Array[Double]): Array[Double] =
    var sum = 0.0
    var index = 0
    while index < values.length do
      sum += values(index)
      index += 1
    val mean = sum / values.length
    var squared = 0.0
    index = 0
    while index < values.length do
      val centered = values(index) - mean
      squared += centered * centered
      index += 1
    val standardDeviation = math.sqrt(squared / values.length)
    require(standardDeviation > 0.0 && standardDeviation.isFinite, "noise variance must be positive and finite")
    val result = new Array[Double](values.length)
    index = 0
    while index < values.length do
      result(index) = (values(index) - mean) / standardDeviation
      index += 1
    result

  private def standardizedOrZero(values: Array[Double]): Array[Double] =
    var sum = 0.0
    var index = 0
    while index < values.length do
      sum += values(index)
      index += 1
    val mean = sum / values.length
    var squared = 0.0
    index = 0
    while index < values.length do
      val centered = values(index) - mean
      squared += centered * centered
      index += 1
    val standardDeviation = math.sqrt(squared / values.length)
    if standardDeviation <= 1e-15 || !standardDeviation.isFinite then Array.fill(values.length)(0.0)
    else
      val result = new Array[Double](values.length)
      index = 0
      while index < values.length do
        result(index) = (values(index) - mean) / standardDeviation
        index += 1
      result
