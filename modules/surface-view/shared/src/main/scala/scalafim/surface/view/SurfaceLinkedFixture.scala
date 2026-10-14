package scalafim.surface.view

import image4s.geometry.{D3, Frame}
import intaglio.*
import scalafim.image.world.Spaces
import scalafim.surface.*

/** Synthetic linked-view specimen: an asymmetric temporal field and a 1.2
  * geometry expansion. The expansion is not an anatomical inflated surface.
  */
object SurfaceLinkedFixture:
  private def ok[A](value: Either[?, A]): A = value.fold(error => throw new IllegalStateException(s"$error"), identity)
  val Surface = SurfaceId.unsafe("linked-plane")
  val Layer = SurfaceLayerId.unsafe("effect")
  val Frame = Spaces.MNI152NLin2009cAsym
  val Faces = Vector((0, 1, 2), (0, 2, 3))
  val Vertices = Vector(Vector(-1.0, -1.0, 0.0), Vector(1.0, -1.0, 0.0),
    Vector(1.0, 1.0, 0.0), Vector(-1.0, 1.0, 0.0))
  val Values = Vector(Vector(-1.0, 0.0, -.25), Vector(0.0, 1.0, -1.0),
    Vector(.25, .75, .5), Vector(-.5, .25, 1.0))
  val Geometry = SurfaceGeometry(TriangleMesh.fromRows(Vertices, Faces), Hemisphere.Left, SurfaceKind.Pial)
  val Expanded = SurfaceGeometry(TriangleMesh.fromRows(Vertices.map(_.map(_ * 1.2)), Faces),
    Hemisphere.Left, SurfaceKind.Inflated)
  val Asset = ok(SurfaceAsset.make(Surface, SurfaceSet.of(SurfaceKind.Pial, Geometry, SurfaceKind.Inflated -> Expanded)))
  val Axis = ok(SurfaceTimeAxis.uniform(3))
  val Field = ok(SurfaceTemporalField.fromMatrix(SurfaceTemporalFieldId.unsafe("linked-series"),
    SurfaceMatrix.fromRows(Geometry, Vector.tabulate(4)(VertexId(_)), Values), Axis))
  val Mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
    ScalarRamp.linear(Rgba32.unsafe(0, 32, 255), Rgba32.unsafe(255, 96, 0))))
  val Reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture://linked-series-v1"), SurfaceContentDigest.unsafe("e" * 64))
  val Bindings = SurfaceSceneBindings(Map(Surface -> Reference), Map(Layer -> Reference))
  val Provenance = ok(SurfaceProvenance.make("linked-display-probe", "1", "2026-10-14"))
  val Legend = SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(Layer), ok(LegendTitle.make("Synthetic effect")),
    showHidden = false, showInvalid = false)
  val FlatMap = ok(SurfaceFlatMap.make(Surface, Vertices.map(p => SurfaceFlatMapPoint(p(0), p(1))), Faces))

  def playback(time: Double, status: SurfacePlaybackStatus = SurfacePlaybackStatus.Paused): SurfacePlaybackState =
    SurfacePlaybackState(SurfaceSeconds.unsafe(time), status, SurfacePlaybackRate.unsafe(1), looping = false)

  def document(playback: SurfacePlaybackState): SurfacePlaybackDocument =
    ok(SurfacePlaybackDocument.capture(Field, Layer, Surface, Reference, Mapping, SurfaceInterpolation.Linear, playback))

  def model(playback: SurfacePlaybackState): SurfaceViewerModel =
    val layer = ok(document(playback).prepare(Field, Reference, Mapping, SurfaceFrameCache.Disabled)).layer
    ok(SurfaceViewerModel.make(Vector(Asset), Vector(layer)))

  def state(model: SurfaceViewerModel, selected: Int, morph: Double): SurfaceViewerState =
    val initial = SurfaceViewerState.initial(model).copy(
      camera = SurfaceCamera.unsafe(SurfaceViewpoint.Dorsal, CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35))),
      lighting = SurfaceLighting.Unlit, clipping = ok(SurfaceClipping.nearFar(.01, 1000)),
      colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)
    val actions = Vector(SurfaceViewerAction.Select(Surface, VertexId(selected)),
      SurfaceViewerAction.BeginGeometryMorph(Surface, SurfaceKind.Inflated),
      SurfaceViewerAction.SetGeometryMorphFraction(Surface, SurfaceMorphFraction.unsafe(morph)))
    actions.foldLeft(initial)((value, action) => ok(SurfaceViewer.reduce(model, value, action)))

  val Eyes = Vector(Vector(0.0, 0.0, 4.0), Vector(-1.0, .8, 4.0), Vector(1.0, .5, 4.0), Vector(0.0, -1.0, 4.0))
  def navigation(model: SurfaceViewerModel, state: SurfaceViewerState): SurfaceCompositionState[Frame.type] =
    val display = ok(SurfaceDisplayFrame.declare(model, Surface, Frame))
    val views = Eyes.zipWithIndex.map: (eye, index) =>
      val pose = ok(SurfaceCameraPose.make[Frame.type](ok(SurfaceCameraPose.pointIn[Frame.type](Frame, Vector(0, 0, 0))),
        ok(SurfaceCameraPose.pointIn[Frame.type](Frame, eye))))
      SurfaceCompositionInputView(ok(SurfaceViewId.make(s"panel-$index")),
        SurfaceViewport((index % 2) * .5, (index / 2) * .5, .5, .5), state.camera, pose,
        Vector(SurfaceCompositionInputInstance(ok(SurfaceInstanceId.make(s"plane-$index")), Surface, display)),
        state.lighting, state.clipping, state.colorPipeline)
    ok(SurfaceCompositionState.make(views))

  def composition(model: SurfaceViewerModel, state: SurfaceViewerState): SurfaceCompositionPlan =
    ok(SurfaceCompositionCompiler.compile(model, SurfaceCompositionPresentation.fromViewerState(state), navigation(model, state).views))

  def marker(state: SurfaceViewerState, pixelScale: Double = 1): Vector[SurfaceDecoration] =
    state.selection.toVector.map: selection =>
      SurfaceMarkerDecoration(ok(SurfaceDecorationId.make("selected-scientific-vertex")),
        SurfaceDecorationAnchor.Vertex(SurfaceVertexAnchor.make(selection.surface, selection.vertex)),
        SurfaceMarkerShape.Disk, ok(SurfaceDecorationSize.pixels(5 * pixelScale)),
        Rgba32.unsafe(255, 220, 80), SurfaceDecorationDepthPolicy.Overlay)
