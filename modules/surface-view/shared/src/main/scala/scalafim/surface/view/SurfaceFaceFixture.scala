package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

/** Adjacent faces deliberately share vertices but carry opposing colors. */
object SurfaceFaceFixture:
  val Surface: SurfaceId = SurfaceId.unsafe("facewise-left")
  val Layer: SurfaceLayerId = SurfaceLayerId.unsafe("face-values")
  val Red: Rgba32 = Rgba32.unsafe(255, 0, 0)
  val Blue: Rgba32 = Rgba32.unsafe(0, 0, 255)

  def geometry: SurfaceGeometry = SurfaceGeometry(
    TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(1.0, 1.0, 0.0), Seq(-1.0, 1.0, 0.0)),
      Seq((0, 1, 2), (0, 2, 3))
    ), Hemisphere.Left, SurfaceKind.Inflated)

  def model: SurfaceViewerModel =
    val mesh = geometry
    val field = SurfaceFaceField.make(mesh, Array(Red, Blue, Blue, Red), frameCount = 2).toOption.get
    SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(Surface, mesh).toOption.get),
      Vector(SurfaceLayer.facePackedRgba(Layer, Surface, field))
    ).toOption.get

  def state(model: SurfaceViewerModel): SurfaceViewerState =
    val initial = SurfaceViewerState.initial(model)
    val actions = Vector(
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal),
      SurfaceViewerAction.SetLighting(SurfaceLighting.Unlit),
      SurfaceViewerAction.SetProjection(CameraProjection.Orthographic(OrthographicScale.unsafe(1.25)))
    )
    actions.foldLeft(initial)((state, action) => SurfaceViewer.reduce(model, state, action).toOption.get)

  def plan: SurfaceRenderPlan =
    val viewer = model
    SurfaceCompiler.compile(viewer, state(viewer)).toOption.get
