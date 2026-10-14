package scalafim.surface.view

import intaglio.*

/** The square's four original vertices have distinct, time-varying labels. */
object SurfaceNearestFixture:
  val Surface: SurfaceId = SurfaceFaceFixture.Surface
  val Layer: SurfaceLayerId = SurfaceLayerId.unsafe("nearest-labels")
  val Palette: Vector[Rgba32] = Vector(Rgba32.unsafe(255, 0, 0), Rgba32.unsafe(0, 255, 0),
    Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 255, 0))

  def model: SurfaceViewerModel =
    val geometry = SurfaceFaceFixture.geometry
    val colorizer = new Colorizer[Int]:
      def color(value: Int): Rgba32 = Palette(value)
    val labels = SurfaceLayer.labels(Layer, Surface, geometry, Array(0, 1, 2, 3, 3, 2, 1, 0), colorizer,
      frameCount = 2, interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(Surface, geometry).toOption.get), Vector(labels)).toOption.get

  def state(model: SurfaceViewerModel): SurfaceViewerState = SurfaceFaceFixture.state(model)

  def plan: SurfaceRenderPlan =
    val viewer = model
    SurfaceCompiler.compile(viewer, state(viewer)).toOption.get
