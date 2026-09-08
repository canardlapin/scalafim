package scalafim.surface.view

import intaglio.*

class SurfaceScalarInterpolationSuite extends munit.FunSuite:
  test("interpolation precedes saturation and nonlinear mapping"):
    val sample = SurfaceScalarInterpolation.value(-2.0, 0.0, 4.0, 0.1, 0.2, 0.7)
    assertEqualsDouble(sample, 2.6, 1e-14)
    assertEqualsDouble(DisplayWindow.unsafe(-1, 1).normalize(sample), 1.0, 0.0)
    // Clamping the vertices first would produce 0.8 in normalized coordinates.
    assertEqualsDouble(0.1 * 0.0 + 0.2 * 0.5 + 0.7 * 1.0, 0.8, 1e-15)

  test("nonfinite contributors invalidate interiors but not exact opposite edges"):
    for invalid <- Vector(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      assert(SurfaceScalarInterpolation.value(invalid, 2, 4, 0.1, 0.4, 0.5).isNaN)
      assertEqualsDouble(SurfaceScalarInterpolation.value(invalid, 2, 4, 0, 0.25, 0.75), 3.5, 1e-15)

  test("extreme finite samples retain constants and cancellation without overflow"):
    val huge = Double.MaxValue
    assertEqualsDouble(SurfaceScalarInterpolation.value(huge, huge, huge, 0.2, 0.3, 0.5) / huge, 1.0, 0.0)
    assertEqualsDouble(SurfaceScalarInterpolation.value(-huge, 0, huge, 0.25, 0.5, 0.25), 0.0, 0.0)
    assertEqualsDouble(SurfaceScalarInterpolation.value(-huge, 0, huge, 0, 0.5, 0.5) / huge, 0.5, 1e-15)
    assertEqualsDouble(SurfaceScalarInterpolation.value(huge, -huge, 1e-200, 0.25, 0.25, 0.5) / 1e-200, 0.5, 1e-15)

  test("permutations and affine unit changes preserve interpolation"):
    val x = SurfaceScalarInterpolation.value(-2, 0, 4, 0.1, 0.2, 0.7)
    assertEqualsDouble(SurfaceScalarInterpolation.value(4, -2, 0, 0.7, 0.1, 0.2), x, 1e-14)
    assertEqualsDouble(SurfaceScalarInterpolation.value(6, 10, 18, 0.1, 0.2, 0.7), 10 + 2 * x, 1e-14)
    intercept[IllegalArgumentException](SurfaceScalarInterpolation.value(1, 2, 3, -0.1, 0.4, 0.7))

  test("raw sample ownership, identity, memory, and camera updates are explicit"):
    val values = SurfaceScalarFixture.Values.toArray
    val model = SurfaceScalarFixture.model(values = values)
    values(0) = 99
    val state = SurfaceFaceFixture.state(model)
    val first = SurfaceCompiler.compile(model, state).toOption.get
    assertEqualsDouble(first.layers.head.scalarField.get.samples(0), -2.0, 0.0)
    val other = SurfaceScalarFixture.model(values = Array(-3.0, 6.0, 6.0, -3.0))
    val changed = SurfaceCompiler.compile(other, SurfaceFaceFixture.state(other)).toOption.get
    assertEquals(Vector.tabulate(4)(first.layers.head.colors(_)), Vector.tabulate(4)(changed.layers.head.colors(_)))
    assertNotEquals(first.receipt.layerKeys, changed.receipt.layerKeys)
    assertEquals(first.receipt.meshKeys, changed.receipt.meshKeys)
    val legacy = SurfaceLayer.scalar(SurfaceScalarFixture.Layer, SurfaceScalarFixture.Surface,
      SurfaceFaceFixture.geometry, SurfaceScalarFixture.Values.toArray, SurfaceScalarFixture.mapping.colorizer).toOption.get
    val legacyModel = SurfaceViewerModel.make(model.surfaces, Vector(legacy)).toOption.get
    assertEquals(first.profile.primitiveBytes - SurfaceCompiler.compile(legacyModel, state).toOption.get.profile.primitiveBytes, 32L)
    val orbit = SurfaceViewer.reduce(model, state, SurfaceViewerAction.OrbitBy(10, 20)).toOption.get
    assertEquals(SurfaceCompiler.compile(model, orbit).toOption.get.receipt.layerKeys, first.receipt.layerKeys)

  test("lighting and additional layers compile with stable fragment semantics"):
    val model = SurfaceScalarFixture.model()
    val state = SurfaceFaceFixture.state(model)
    assert(SurfaceCompiler.compile(model, state.copy(lighting = SurfaceViewerState.initial(model).lighting)).isRight)
    val second = SurfaceLayer.scalar(SurfaceLayerId.unsafe("underlay"), SurfaceScalarFixture.Surface,
      SurfaceFaceFixture.geometry, Array.fill(4)(0.0), SurfaceScalarFixture.mapping.colorizer).toOption.get
    val mixed = SurfaceViewerModel.make(model.surfaces, model.layers :+ second).toOption.get
    val hidden = SurfaceViewer.reduce(mixed, SurfaceFaceFixture.state(mixed), SurfaceViewerAction.SetLayerVisible(second.id, false)).toOption.get
    assert(SurfaceCompiler.compile(mixed, hidden).toOption.get.fragmentSurfaces(SurfaceScalarFixture.Surface))

  test("scene revision 5 preserves scalar policy and infers the backend requirement"):
    val model = SurfaceScalarFixture.model()
    val ref = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:scalar"), SurfaceContentDigest.unsafe("d" * 64))
    val bindings = SurfaceSceneBindings(Map(SurfaceScalarFixture.Surface -> ref), Map(SurfaceScalarFixture.Layer -> ref))
    val doc = SurfaceSceneDocument.capture(model, SurfaceFaceFixture.state(model), bindings,
      SurfaceProvenance.make("scalar-test", "1", "2026-09-07").toOption.get).toOption.get
    val json = SurfaceSceneCodec.encode(doc)
    val decoded = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(decoded.revision, SurfaceDocumentRevision.V6)
    assert(decoded.layers.head.scalarInterpolation)
    assert(decoded.restore(model, bindings).isRight)
    val unsupported = SurfaceBackendCapabilities(SurfaceBackendId.unsafe("no-scalars"), SurfacePlanRevision.Current, Set.empty)
    assert(decoded.admit(unsupported).isLeft)
    assert(decoded.admit(unsupported.copy(features = Set(SurfaceBackendFeature.ScalarInterpolation, SurfaceBackendFeature.FragmentComposition))).isRight)
    assert(SurfaceSceneCodec.decode(json.replace(",\"legends\":[]", "").replace("\"revision\":6", "\"revision\":4")).isLeft)
