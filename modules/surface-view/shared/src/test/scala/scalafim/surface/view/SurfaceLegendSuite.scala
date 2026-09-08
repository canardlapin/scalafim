package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceLegendSuite extends munit.FunSuite:
  private val surface = SurfaceFaceFixture.Surface
  private val geometry = SurfaceFaceFixture.geometry
  private val first = SurfaceLayerId.unsafe("first")
  private val second = SurfaceLayerId.unsafe("second")
  private val blue = Rgba32.unsafe(0, 0, 200)
  private val white = Rgba32.unsafe(200, 200, 200)
  private val red = Rgba32.unsafe(200, 0, 0)
  private val title = LegendTitle.make("Contrast", Some("percent signal change")).toOption.get
  private val sequential = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-4, 8), ScalarRamp.linear(blue, red)))
  private val split = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-4, 8), 0, -1, 2,
    ScalarRamp.linear(blue, white), ScalarRamp.linear(white, red)).toOption.get)
  private def scalar(id: SurfaceLayerId, mapping: ScalarMapping): SurfaceLayer =
    SurfaceLayer.scalar(id, surface, geometry, Array(-2.5, -1, 2, 5), mapping.colorizer).toOption.get
  private def model(layers: SurfaceLayer*): SurfaceViewerModel =
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), layers.toVector).toOption.get
  private def calibration(legend: SurfaceLegend): ScalarLegend = legend.content match
    case SurfaceLegendContent.Continuous(value) => value
    case SurfaceLegendContent.Split(value) => value
    case _ => fail("expected scalar legend")
  private def continuous(ids: SurfaceLayerId*): SurfaceLegendRequest =
    SurfaceLegendRequest.Continuous(SurfaceLegendLayers.make(ids.toVector).toOption.get, title)

  test("continuous calibration uses effective overrides and analytic full-window positions"):
    val viewer = model(scalar(first, sequential))
    val initial = SurfaceFaceFixture.state(viewer)
    val windowed = SurfaceViewer.reduce(viewer, initial,
      SurfaceViewerAction.SetLayerWindow(first, DisplayWindow.unsafe(-8, 16))).toOption.get
    val updated = SurfaceViewer.reduce(viewer, windowed, SurfaceViewerAction.SetLayerThreshold(first,
      DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-2, 4)))).toOption.get
    val legend = SurfaceLegend.bind(continuous(first), viewer, updated).toOption.get
    val scale = calibration(legend)
    assertEqualsDouble(scale.position(-2), 0.25, 0)
    assertEqualsDouble(scale.position(4), 0.5, 0)
    assertEquals(scale.colorAt(0.375), Rgba32.unsafe(0, 0, 0, 0))
    assertEquals(scale.colorAt(0), blue)
    assertEquals(scale.colorAt(1), red)
    val effective = SurfaceCompiler.compile(viewer, updated).toOption.get.layers.head.scalarMapping.get
    assertEquals(scale.mapping.canonicalKey, effective.canonicalKey)
    for cell <- scale.cells(64) do assertEquals(cell.color, effective.color(cell.sampleValue))
    assert(legend.notes.contains("Unlit mapping colors"))

  test("split bar retains asymmetric widths and its own tail calibrations"):
    val viewer = model(scalar(first, split))
    val request = SurfaceLegendRequest.Split(SurfaceLegendLayers.one(first), title)
    val legend = SurfaceLegend.bind(request, viewer, SurfaceFaceFixture.state(viewer)).toOption.get
    val scale = calibration(legend)
    assertEqualsDouble(scale.position(-1), 0.25, 0)
    assertEqualsDouble(scale.position(2), 0.5, 0)
    assertEquals(scale.mapping.color(-2.5), Rgba32.unsafe(100, 100, 200))
    assertEquals(scale.mapping.color(5), Rgba32.unsafe(200, 100, 100))
    assertEquals(scale.colorAt(0.375).alpha, 0)
    assert(SurfaceLegend.bind(continuous(first), viewer, SurfaceFaceFixture.state(viewer)).isLeft)
    val other = model(scalar(first, sequential))
    assert(SurfaceLegend.bind(request, other, SurfaceFaceFixture.state(other)).isLeft)

  test("shared legends compare effective mappings, including unsampled invalid and hidden colors"):
    val viewer = model(scalar(first, sequential), scalar(second, sequential.copy(invalid = white)))
    val state = SurfaceFaceFixture.state(viewer)
    assertEquals(SurfaceLegend.bind(continuous(first, second), viewer, state).left.toOption,
      Some(SurfaceLegendError.IncompatibleMappings(Vector(first, second))))
    val same = model(scalar(first, sequential), scalar(second, sequential))
    val sameState = SurfaceFaceFixture.state(same)
    val a = SurfaceLegend.bind(continuous(first, second), same, sameState).toOption.get
    val b = SurfaceLegend.bind(continuous(second, first), same, sameState).toOption.get
    assertEquals(a.canonicalKey, b.canonicalKey)
    val changed = SurfaceViewer.reduce(same, sameState,
      SurfaceViewerAction.SetLayerWindow(second, DisplayWindow.unsafe(-8, 8))).toOption.get
    assert(SurfaceLegend.bind(continuous(first, second), same, changed).isLeft)

  test("different base windows may share only after their effective mappings agree"):
    val other = sequential.copy(scale = ScalarScale.sequential(DisplayWindow.unsafe(-8, 16), ScalarRamp.linear(blue, red)))
    val viewer = model(scalar(first, sequential), scalar(second, other))
    val state = SurfaceViewer.reduce(viewer, SurfaceFaceFixture.state(viewer),
      SurfaceViewerAction.SetLayerWindow(second, sequential.scale.window)).toOption.get
    assert(SurfaceLegend.bind(continuous(first, second), viewer, state).isRight)

  test("window and threshold changes invalidate snapshots while camera and opacity preserve identity"):
    val viewer = model(scalar(first, sequential))
    val state = SurfaceFaceFixture.state(viewer)
    val legend = SurfaceLegend.bind(continuous(first), viewer, state).toOption.get
    for action <- Vector(SurfaceViewerAction.OrbitBy(10, 20),
        SurfaceViewerAction.SetLayerOpacity(first, DisplayOpacity.unsafe(0.25))) do
      val updated = SurfaceViewer.reduce(viewer, state, action).toOption.get
      assertEquals(legend.validateCurrent(viewer, updated), Right(()))
    for action <- Vector(SurfaceViewerAction.SetLayerWindow(first, DisplayWindow.unsafe(-8, 16)),
        SurfaceViewerAction.SetLayerThreshold(first, DisplayThreshold.TransparentBand(ThresholdBand.unsafe(-1, 1)))) do
      val updated = SurfaceViewer.reduce(viewer, state, action).toOption.get
      assertEquals(legend.validateCurrent(viewer, updated), Left(SurfaceLegendError.StaleLegend))
      assert(SurfaceLegend.bind(legend.request, viewer, updated).toOption.get.validateCurrent(viewer, updated).isRight)

  test("custom ticks are checked again after the effective window changes"):
    val viewer = model(scalar(first, sequential))
    val state = SurfaceFaceFixture.state(viewer)
    val request = SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(first), title, Vector(AxisTick.unsafe(8, "upper")))
    assert(SurfaceLegend.bind(request, viewer, state).isRight)
    val updated = SurfaceViewer.reduce(viewer, state,
      SurfaceViewerAction.SetLayerWindow(first, DisplayWindow.unsafe(-4, 4))).toOption.get
    assert(SurfaceLegend.bind(request, viewer, updated).isLeft)

  test("automatic binding rejects opaque, hidden, missing, and duplicate layer requests"):
    val layer = SurfaceLayer.scalar(first, surface, geometry, Array.fill(4)(0.0), Colorizer.constant[Double](red)).toOption.get
    val viewer = model(layer)
    val state = SurfaceFaceFixture.state(viewer)
    assertEquals(SurfaceLegend.bind(continuous(first), viewer, state).left.toOption, Some(SurfaceLegendError.OpaqueMapping(first)))
    assertEquals(SurfaceLegend.bind(continuous(second), viewer, state).left.toOption, Some(SurfaceLegendError.UnknownLayer(second)))
    val hidden = state.copy(presentations = state.presentations.updated(first, state.presentations(first).copy(visible = false)))
    assertEquals(SurfaceLegend.bind(continuous(first), viewer, hidden).left.toOption, Some(SurfaceLegendError.HiddenLayer(first)))
    assertEquals(SurfaceLegend.bind(continuous(first), viewer, state.copy(presentations = Map.empty)).left.toOption,
      Some(SurfaceLegendError.MissingPresentation(first)))
    assertEquals(SurfaceLegend.bind(continuous(first), viewer, state.copy(layerOrder = Vector.empty)).left.toOption,
      Some(SurfaceLegendError.UndisplayedLayer(first)))
    assert(SurfaceLegendLayers.make(Vector.empty).isLeft)
    assert(SurfaceLegendLayers.make(Vector(first, first)).isLeft)

  private val palette = LabelColorizer(Map(10 -> red, 20 -> blue), white)
  private val categoryNames = Map(10 -> "Motor", 20 -> "Visual")
  private def categories(id: SurfaceLayerId, colors: Colorizer[Int] = palette,
    interpolation: SurfaceVertexInterpolation = SurfaceVertexInterpolation.NearestSample): SurfaceLayer =
    SurfaceLayer.labels(id, surface, geometry, Array(10, 20, 10, 99), colors, interpolation = interpolation).toOption.get

  test("categorical vertex and face legends use palette keys, exact colors, and explicit fallback"):
    val field = SurfaceFaceField.make(geometry, Array(10, 20)).toOption.get
    val viewer = model(categories(first), SurfaceLayer.faceLabels(second, surface, field, palette))
    val request = SurfaceLegendRequest.Categorical(SurfaceLegendLayers.make(Vector(first, second)).toOption.get, title, categoryNames)
    val legend = SurfaceLegend.bind(request, viewer, SurfaceFaceFixture.state(viewer)).toOption.get
    legend.content match
      case SurfaceLegendContent.Categorical(_, entries, fallback) =>
        assertEquals(entries.map(e => (e.id, e.label, e.color)), Vector((10, "Motor", red), (20, "Visual", blue)))
        assertEquals(fallback.color, white)
        assertEquals(fallback.label, "Other / unmapped")
      case _ => fail("expected categorical legend")
    assertEquals(legend.sources.map(_.interpolation), Vector(SurfaceMapInterpolation.NearestVertex, SurfaceMapInterpolation.FaceConstant))

  test("categorical legends reject partial names, opaque callbacks, interpolated categories, and differing fallback colors"):
    val request = SurfaceLegendRequest.Categorical(SurfaceLegendLayers.one(first), title, categoryNames)
    for layer <- Vector(categories(first, Colorizer.constant[Int](red)),
        categories(first, interpolation = SurfaceVertexInterpolation.Color)) do
      val viewer = model(layer)
      assert(SurfaceLegend.bind(request, viewer, SurfaceFaceFixture.state(viewer)).isLeft)
    val viewer = model(categories(first), categories(second, palette.copy(fallback = red)))
    val state = SurfaceFaceFixture.state(viewer)
    val incomplete = SurfaceLegendRequest.Categorical(SurfaceLegendLayers.one(first), title, Map(10 -> "Motor"))
    assert(SurfaceLegend.bind(incomplete, viewer, state).isLeft)
    val shared = SurfaceLegendRequest.Categorical(SurfaceLegendLayers.make(Vector(first, second)).toOption.get, title, categoryNames)
    assert(SurfaceLegend.bind(shared, viewer, state).isLeft)

  test("manual legends are explicit and quantity, units, labels, and colors participate in identity"):
    val viewer = model(scalar(first, sequential))
    val state = SurfaceFaceFixture.state(viewer)
    val entries = Vector(SurfaceLegendItem.unsafe("Selected", red))
    val request = SurfaceLegendRequest.Manual(title, entries)
    val legend = SurfaceLegend.bind(request, viewer, state).toOption.get
    assertEquals(legend.sources, Vector.empty)
    assertEquals(legend.notes, Vector("Manual color key"))
    val changed = Vector(
      SurfaceLegendRequest.Manual(LegendTitle.make("Other", title.units).toOption.get, entries),
      SurfaceLegendRequest.Manual(LegendTitle.make(title.quantity, Some("t statistic")).toOption.get, entries),
      SurfaceLegendRequest.Manual(title, Vector(SurfaceLegendItem.unsafe("Other", red))),
      SurfaceLegendRequest.Manual(title, Vector(SurfaceLegendItem.unsafe("Selected", blue))))
    for next <- changed do assertNotEquals(SurfaceLegend.bind(next, viewer, state).toOption.get.canonicalKey, legend.canonicalKey)
    assert(SurfaceLegend.bind(SurfaceLegendRequest.Manual(title, Vector.empty), viewer, state).isLeft)
