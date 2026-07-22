package scalafim.examples.surfaceview

import scalafim.graphics.*
import scalafim.surface.*
import scalafim.surface.view.*
import scalafim.surface.view.raster.*

final case class CorticalSurfaceCorpusEntry(
  fileName: String,
  sha256: String,
  hemisphere: Hemisphere,
  vertices: Int,
  faces: Int
)

object CorticalSurfaceAcceptance:
  val LeftCorpus: CorticalSurfaceCorpusEntry = CorticalSurfaceCorpusEntry(
    "fsaverage5-lh-pial.gii",
    "b425b4914362af4aaf43bb5d022afd39a0c5f6ec4601e353631cdd737884c951",
    Hemisphere.Left,
    10242,
    20480
  )
  val RightCorpus: CorticalSurfaceCorpusEntry = CorticalSurfaceCorpusEntry(
    "fsaverage5-rh-pial.gii",
    "6142dfcec7533aaeb962b1de829e21948bb35c85645a08f2c4b7ea503d655ee9",
    Hemisphere.Right,
    10242,
    20480
  )

  val Dimensions: RasterDimensions = RasterDimensions.unsafe(960, 540)
  val Policy: SurfaceVisualQaPolicy = SurfaceVisualQaPolicy.NativeBackend

  def build(
    left: SurfaceGeometry,
    right: SurfaceGeometry
  ): Either[SurfaceViewError, SurfaceViewerExample] =
    build(
      SurfaceSet.of(left.kind, left),
      SurfaceSet.of(right.kind, right)
    )

  def build(
    leftSurfaces: SurfaceSet,
    rightSurfaces: SurfaceSet
  ): Either[SurfaceViewError, SurfaceViewerExample] =
    val left = leftSurfaces.default
    val right = rightSurfaces.default
    for
      _ <- requireGeometry(left, LeftCorpus)
      _ <- requireGeometry(right, RightCorpus)
      activationLayer <- SurfaceLayer.scalar(
        SurfaceViewerExample.Activation,
        SurfaceViewerExample.LeftSurface,
        left,
        scalarValues(left),
        ScalarColorizer(DisplayWindow.unsafe(-2.5, 2.5), ColorRamp.Heat),
        opacity = DisplayOpacity.unsafe(0.9)
      )
      parcelLayer <- SurfaceLayer.labels(
        SurfaceViewerExample.Parcels,
        SurfaceViewerExample.RightSurface,
        right,
        labelValues(right),
        LabelColorizer(Map(
          1 -> Rgba32.unsafe(38, 139, 210),
          2 -> Rgba32.unsafe(220, 80, 64),
          3 -> Rgba32.unsafe(92, 184, 92)
        ))
      )
      leftAsset <- SurfaceAsset.make(SurfaceViewerExample.LeftSurface, leftSurfaces)
      rightAsset <- SurfaceAsset.make(SurfaceViewerExample.RightSurface, rightSurfaces)
      model <- SurfaceViewerModel.make(
        Vector(leftAsset, rightAsset),
        Vector(activationLayer, parcelLayer)
      )
      state <- actions(left).foldLeft[Either[SurfaceViewError, SurfaceViewerState]](
        Right(SurfaceViewerState.initial(model))
      )((current, action) => current.flatMap(SurfaceViewer.reduce(model, _, action)))
      plan <- SurfaceCompiler.compile(model, state)
    yield SurfaceViewerExample(model, state, plan)

  def reference(example: SurfaceViewerExample): SurfaceRasterResult =
    SurfaceRasterizer.render(
      example.plan,
      Dimensions,
      SurfaceRasterStyle(culling = TriangleCulling.None)
    ).fold(error => throw new IllegalStateException(error.message), identity)

  /** Select a stable, face-interior landmark near the center of the left
    * viewport. Requiring a 3 x 3 neighborhood with one face keeps native
    * ray-picking comparisons away from rasterization edges.
    */
  def landmark(reference: SurfaceRasterResult): (Int, Int, SurfacePick) =
    val targetX = Dimensions.width / 4
    val targetY = Dimensions.height / 2
    var bestDistance = Double.PositiveInfinity
    var best: Option[(Int, Int, SurfacePick)] = None
    var y = 1
    while y + 1 < Dimensions.height do
      var x = 1
      while x < Dimensions.width / 2 - 1 do
        reference.pick(x, y).toOption.flatten.foreach: pick =>
          if sameFaceNeighborhood(reference, x, y, pick) then
            val distance = math.hypot(x - targetX, y - targetY)
            if distance < bestDistance then
              bestDistance = distance
              best = Some((x, y, pick))
        x += 1
      y += 1
    best.getOrElse(throw new IllegalStateException("cortical reference has no stable left-hemisphere landmark"))

  def cameraOnlyPlan(example: SurfaceViewerExample): Either[SurfaceViewError, SurfaceRenderPlan] =
    for
      state <- SurfaceViewer.reduce(
        example.model,
        example.state,
        SurfaceViewerAction.OrbitBy(2.0, -1.0)
      )
      plan <- SurfaceCompiler.compile(example.model, state)
    yield plan

  private def requireGeometry(
    geometry: SurfaceGeometry,
    entry: CorticalSurfaceCorpusEntry
  ): Either[SurfaceViewError, Unit] =
    if geometry.hemisphere != entry.hemisphere then
      Left(SurfaceViewError.InvalidBilateralLayout(
        s"${entry.fileName} must be ${entry.hemisphere.code}; got ${geometry.hemisphere.code}"
      ))
    else if geometry.vertexCount != entry.vertices then
      Left(SurfaceViewError.InvalidBilateralLayout(
        s"${entry.fileName} must have ${entry.vertices} vertices; got ${geometry.vertexCount}"
      ))
    else if geometry.faceCount != entry.faces then
      Left(SurfaceViewError.InvalidBilateralLayout(
        s"${entry.fileName} must have ${entry.faces} faces; got ${geometry.faceCount}"
      ))
    else Right(())

  private def scalarValues(geometry: SurfaceGeometry): Array[Double] =
    val (minimum, maximum) = yRange(geometry)
    val scale = if maximum == minimum then 0.0 else 5.0 / (maximum - minimum)
    Array.tabulate(geometry.vertexCount): vertex =>
      (geometry.mesh.vertex(VertexId(vertex)).y - minimum) * scale - 2.5

  private def labelValues(geometry: SurfaceGeometry): Array[Int] =
    val (minimum, maximum) = yRange(geometry)
    val span = maximum - minimum
    Array.tabulate(geometry.vertexCount): vertex =>
      val fraction =
        if span == 0.0 then 0.5
        else (geometry.mesh.vertex(VertexId(vertex)).y - minimum) / span
      if fraction < 1.0 / 3.0 then 1
      else if fraction < 2.0 / 3.0 then 2
      else 3

  private def yRange(geometry: SurfaceGeometry): (Double, Double) =
    var minimum = Double.PositiveInfinity
    var maximum = Double.NegativeInfinity
    var vertex = 0
    while vertex < geometry.vertexCount do
      val y = geometry.mesh.vertex(VertexId(vertex)).y
      minimum = math.min(minimum, y)
      maximum = math.max(maximum, y)
      vertex += 1
    (minimum, maximum)

  private def actions(left: SurfaceGeometry): Vector[SurfaceViewerAction] =
    var selected = 0
    var maximumZ = Double.NegativeInfinity
    var vertex = 0
    while vertex < left.vertexCount do
      val z = left.mesh.vertex(VertexId(vertex)).z
      if z > maximumZ then
        maximumZ = z
        selected = vertex
      vertex += 1
    Vector(
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(
        SurfaceViewerExample.LeftSurface,
        SurfaceViewerExample.RightSurface,
        BilateralOrder.LeftThenRight
      )),
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(105.0))),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
      SurfaceViewerAction.SetLayerThreshold(
        SurfaceViewerExample.Activation,
        DisplayThreshold.transparentBand(-0.35, 0.35).toOption.get
      ),
      SurfaceViewerAction.Select(SurfaceViewerExample.LeftSurface, VertexId(selected))
    )

  private def sameFaceNeighborhood(
    reference: SurfaceRasterResult,
    x: Int,
    y: Int,
    center: SurfacePick
  ): Boolean =
    var dy = -1
    while dy <= 1 do
      var dx = -1
      while dx <= 1 do
        reference.pick(x + dx, y + dy).toOption.flatten match
          case Some(pick) if pick.surface == center.surface && pick.face == center.face => ()
          case _ => return false
        dx += 1
      dy += 1
    true
