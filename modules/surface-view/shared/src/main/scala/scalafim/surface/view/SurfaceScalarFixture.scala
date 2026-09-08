package scalafim.surface.view

import intaglio.*

/** Coordinate field f(x,y,z) = 1 + 3*x crosses limits and an optional hidden band. */
object SurfaceScalarFixture:
  val Surface: SurfaceId = SurfaceFaceFixture.Surface
  val Layer: SurfaceLayerId = SurfaceLayerId.unsafe("scalar-fragments")
  val Values: Vector[Double] = Vector(-2.0, 4.0, 4.0, -2.0)
  val mapping: ScalarMapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.make(Vector(0.0 -> Rgba32.unsafe(0, 0, 200), 0.25 -> Rgba32.unsafe(0, 200, 0),
      1.0 -> Rgba32.unsafe(200, 0, 0))).toOption.get))
  val thresholded: ScalarMapping = mapping.resolve(threshold = Some(DisplayThreshold.transparentBand(-0.25, 0.25).toOption.get)).toOption.get

  def model(display: ScalarMapping = mapping, values: Array[Double] = Values.toArray): SurfaceViewerModel =
    val geometry = SurfaceFaceFixture.geometry
    val layer = SurfaceLayer.interpolatedScalar(Layer, Surface, geometry, values, display).toOption.get
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(Surface, geometry).toOption.get), Vector(layer)).toOption.get

  def plan(display: ScalarMapping = mapping): SurfaceRenderPlan =
    val viewer = model(display)
    SurfaceCompiler.compile(viewer, SurfaceFaceFixture.state(viewer)).toOption.get
