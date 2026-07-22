package scalafim.examples.surfaceview

import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

final case class CorticalLensCase(
  label: String,
  fraction: SurfaceMorphFraction,
  plan: SurfaceRenderPlan,
  guide: CorticalLensGuide
)

final case class CorticalLensGuide(
  centerX: Double,
  centerY: Double,
  innerRadiusPixels: Double,
  outerRadiusPixels: Double
):
  require(centerX.isFinite && centerY.isFinite, "lens guide center must be finite")
  require(innerRadiusPixels.isFinite && innerRadiusPixels > 0.0, "lens guide inner radius must be positive")
  require(outerRadiusPixels.isFinite && outerRadiusPixels > innerRadiusPixels, "lens guide radii must be ordered")

final case class CorticalSurfaceLensExample(
  model: SurfaceViewerModel,
  pinnedState: SurfaceViewerState,
  center: VertexId,
  activeVertices: Int,
  quality: SurfaceLensQuality,
  visibleNegativeVertices: Int,
  visiblePositiveVertices: Int,
  cases: Vector[CorticalLensCase]
)

/** A real-cortex acceptance fixture for the geodesic reveal lens. A strongly
  * displaced lateral suprathreshold vertex anchors the lens so the live plate
  * exercises both visible anatomy and an activation rather than an arbitrary
  * mesh index.
  */
object CorticalSurfaceLensAcceptance:
  val Surface: SurfaceId = SurfaceId.unsafe("lens-left")
  val Curvature: SurfaceLayerId = SurfaceLayerId.unsafe("lens-curvature")
  val Activation: SurfaceLayerId = SurfaceLayerId.unsafe("lens-activation")
  val Dimensions: RasterDimensions = RasterDimensions.unsafe(600, 450)
  val InnerRadius: SurfaceLensRadius = SurfaceLensRadius.unsafe(12.0)
  val OuterRadius: SurfaceLensRadius = SurfaceLensRadius.unsafe(48.0)
  val Fractions: Vector[(String, SurfaceMorphFraction)] = Vector(
    "Folded" -> SurfaceMorphFraction.unsafe(0.0),
    "Opening" -> SurfaceMorphFraction.unsafe(0.35),
    "Reveal" -> SurfaceMorphFraction.unsafe(0.70),
    "Full lens" -> SurfaceMorphFraction.unsafe(1.0)
  )

  def build(
    pial: SurfaceGeometry,
    inflated: SurfaceGeometry
  ): Either[SurfaceViewError, CorticalSurfaceLensExample] =
    if pial.hemisphere != Hemisphere.Left || inflated.hemisphere != Hemisphere.Left then
      Left(SurfaceViewError.IncompatibleMorph("reveal-lens acceptance requires left-hemisphere geometries"))
    else if pial.kind != SurfaceKind.Pial || inflated.kind != SurfaceKind.Inflated then
      Left(SurfaceViewError.IncompatibleMorph("reveal-lens acceptance requires pial and inflated endpoints"))
    else if !pial.hasSameMeshDomain(inflated) || pial.surfaceToWorld != inflated.surfaceToWorld then
      Left(SurfaceViewError.IncompatibleMorph("reveal-lens endpoints must share exact topology and transform"))
    else
      val topology = MeshTopology.from(pial)
      val noise = SurfaceThresholdParityFixture.deterministicNoise(topology)
      val curvature = SurfaceThresholdParityFixture.curvatureValues(topology)
      val center = revealCenter(pial, inflated, noise)
      val curvatureField = SurfaceField.full(pial, curvature, "signed umbrella-Laplacian curvature")
      val threshold = DisplayThreshold.transparentBand(
        -SurfaceThresholdParityFixture.Threshold,
        SurfaceThresholdParityFixture.Threshold
      ).toOption.get
      val ramp = ColorRamp(Rgba32.unsafe(32, 92, 224), Rgba32.unsafe(235, 56, 38))
      for
        underlay <- SurfaceLayer.curvatureUnderlay(Curvature, Surface, curvatureField, pial)
        overlay <- SurfaceLayer.scalar(
          Activation,
          Surface,
          pial,
          noise,
          ScalarColorizer(DisplayWindow.unsafe(-4.0, 4.0), ramp)
        )
        asset <- SurfaceAsset.make(
          Surface,
          SurfaceSet.of(SurfaceKind.Pial, pial, SurfaceKind.Inflated -> inflated)
        )
        model <- SurfaceViewerModel.make(Vector(asset), Vector(underlay, overlay))
        base <- commonActions(center, threshold).foldLeft[Either[SurfaceViewError, SurfaceViewerState]](
          Right(SurfaceViewerState.initial(model))
        )((current, action) => current.flatMap(SurfaceViewer.reduce(model, _, action)))
        pinned <- SurfaceViewer.reduce(
          model,
          base,
          SurfaceViewerAction.BeginGeometryLens(
            Surface,
            SurfaceKind.Inflated,
            center,
            InnerRadius,
            OuterRadius
          )
        )
        cases <- compileCases(model, pinned)
      yield
        val deformation = pinned.geometryPresentations(Surface) match
          case SurfaceGeometryPresentation.RevealLens(_, _, value, _) => value
          case _ => throw new IllegalStateException("lens action did not produce a reveal-lens presentation")
        var negative = 0
        var positive = 0
        var vertex = 0
        while vertex < noise.length do
          if noise(vertex) <= -SurfaceThresholdParityFixture.Threshold then negative += 1
          else if noise(vertex) >= SurfaceThresholdParityFixture.Threshold then positive += 1
          vertex += 1
        CorticalSurfaceLensExample(
          model,
          pinned,
          center,
          deformation.lens.activeVertexCount,
          deformation.quality,
          negative,
          positive,
          cases
        )

  def reference(current: CorticalLensCase): SurfaceRasterResult =
    SurfaceRasterizer.render(
      current.plan,
      Dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).fold(error => throw new IllegalStateException(error.message), identity)

  private def commonActions(
    center: VertexId,
    threshold: DisplayThreshold
  ): Vector[SurfaceViewerAction] = Vector(
    SurfaceViewerAction.SetLayout(SurfaceLayout.Single(Surface)),
    SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Lateral(CorticalHemisphere.Left)),
    SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(105.0))),
    SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
    SurfaceViewerAction.SetLayerThreshold(Activation, threshold),
    SurfaceViewerAction.Select(Surface, center)
  )

  private def compileCases(
    model: SurfaceViewerModel,
    pinned: SurfaceViewerState
  ): Either[SurfaceViewError, Vector[CorticalLensCase]] =
    val result = Vector.newBuilder[CorticalLensCase]
    var index = 0
    while index < Fractions.length do
      val (label, fraction) = Fractions(index)
      val compiled = for
        state <- SurfaceViewer.reduce(
          model,
          pinned,
          SurfaceViewerAction.SetGeometryMorphFraction(Surface, fraction)
        )
        plan <- SurfaceCompiler.compile(model, state)
      yield CorticalLensCase(label, fraction, plan, guide(plan, pinned.selection.get.vertex))
      compiled match
        case Left(error) => return Left(error)
        case Right(value) => result += value
      index += 1
    Right(result.result())

  private def guide(plan: SurfaceRenderPlan, center: VertexId): CorticalLensGuide =
    val positions = plan.meshes.head.positions
    val offset = center.index * 3
    val x = positions(offset).toDouble
    val y = positions(offset + 1).toDouble
    val z = positions(offset + 2).toDouble
    val view = plan.camera.viewMatrix
    val projection = plan.camera.projectionMatrix
    val vx = view(0) * x + view(1) * y + view(2) * z + view(3)
    val vy = view(4) * x + view(5) * y + view(6) * z + view(7)
    val vz = view(8) * x + view(9) * y + view(10) * z + view(11)
    val vw = view(12) * x + view(13) * y + view(14) * z + view(15)
    val clipX = projection(0) * vx + projection(1) * vy + projection(2) * vz + projection(3) * vw
    val clipY = projection(4) * vx + projection(5) * vy + projection(6) * vz + projection(7) * vw
    val clipW = projection(12) * vx + projection(13) * vy + projection(14) * vz + projection(15) * vw
    val inverseW = if clipW == 0.0 then 1.0 else 1.0 / clipW
    val centerX = (clipX * inverseW + 1.0) * 0.5 * Dimensions.width
    val centerY = (1.0 - clipY * inverseW) * 0.5 * Dimensions.height
    val pixelsPerWorldX = math.abs(projection(0).toDouble) * Dimensions.width * 0.5
    CorticalLensGuide(
      centerX,
      centerY,
      InnerRadius.value * pixelsPerWorldX,
      OuterRadius.value * pixelsPerWorldX
    )

  private def revealCenter(
    pial: SurfaceGeometry,
    inflated: SurfaceGeometry,
    noise: Array[Double]
  ): VertexId =
    var bestVertex = 0
    var bestScore = Double.NegativeInfinity
    var minimumX = Double.PositiveInfinity
    var maximumX = Double.NegativeInfinity
    var vertex = 0
    while vertex < pial.vertexCount do
      val x = pial.mesh.vertex(VertexId(vertex)).x
      minimumX = math.min(minimumX, x)
      maximumX = math.max(maximumX, x)
      vertex += 1
    val inverseRange = 1.0 / math.max(1e-12, maximumX - minimumX)
    var foundThreshold = false
    vertex = 0
    while vertex < pial.vertexCount do
      val magnitude = math.abs(noise(vertex))
      val passes = magnitude >= SurfaceThresholdParityFixture.Threshold
      val folded = pial.mesh.vertex(VertexId(vertex))
      val open = inflated.mesh.vertex(VertexId(vertex))
      val dx = open.x - folded.x
      val dy = open.y - folded.y
      val dz = open.z - folded.z
      val displacement = math.sqrt(dx * dx + dy * dy + dz * dz)
      val lateralness = (maximumX - folded.x) * inverseRange
      val score = displacement * (0.2 + 0.8 * lateralness) * (1.0 + 0.05 * magnitude)
      if passes && (!foundThreshold || score > bestScore) then
        bestVertex = vertex
        bestScore = score
        foundThreshold = true
      else if !foundThreshold && score > bestScore then
        bestVertex = vertex
        bestScore = score
      vertex += 1
    VertexId(bestVertex)
