package scalafim.surface.view

import intaglio.*

class SurfaceFaceFlatPolicySuite extends munit.FunSuite:
  private val surface = SurfaceFaceFixture.Surface
  private val layerId = SurfaceLayerId.unsafe("map")
  private val mapping = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-1.0, 1.0), 0.0, -0.25, 0.25,
    ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
    ScalarRamp.linear(Rgba32.unsafe(255, 64, 0), Rgba32.unsafe(255, 255, 0))).toOption.get)
  private val values = Array(-0.9, 0.1, 0.8, 0.3)

  private def model(reduction: Option[SurfaceFaceReduction]): SurfaceViewerModel =
    val geometry = SurfaceFaceFixture.geometry
    val layer = reduction.fold(SurfaceLayer.interpolatedScalar(layerId, surface, geometry, values, mapping))(r =>
      SurfaceLayer.faceFlatScalar(layerId, surface, geometry, values, mapping, r)).toOption.get
    SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(layer)).toOption.get

  private def plan(reduction: Option[SurfaceFaceReduction]): SurfaceRenderPlan =
    val m = model(reduction)
    SurfaceCompiler.compile(m, SurfaceFaceFixture.state(m)).toOption.get

  private val ref = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:face-flat"), SurfaceContentDigest.unsafe("e" * 64))
  private val bindings = SurfaceSceneBindings(Map(surface -> ref), Map(layerId -> ref))
  private def document(reduction: Option[SurfaceFaceReduction]): SurfaceSceneDocument =
    val m = model(reduction)
    SurfaceSceneDocument.capture(m, SurfaceFaceFixture.state(m), bindings,
      SurfaceProvenance.make("face-flat-test", "1", "2026-09-11").toOption.get).toOption.get

  test("face-flat layers compile to their declared policy with raw samples and fragment semantics"):
    for reduction <- SurfaceFaceReduction.values do
      val compiled = plan(Some(reduction))
      val layer = compiled.layers.head
      assertEquals(layer.interpolation, reduction.policy)
      assertEquals(layer.interpolation.faceReduction, Some(reduction))
      assert(layer.interpolation.scalar && layer.interpolation.faceFlat)
      assertEquals(layer.scalarField.map(_.samples.length), Some(4))
      assert(compiled.fragmentSurfaces(surface))
      assert(compiled.meshes.head.sourceVertices.isEmpty, "face-flat layers keep the scientific vertices")
    val keys = Vector(None, Some(SurfaceFaceReduction.Mean), Some(SurfaceFaceReduction.MaxMagnitude)).map(r => plan(r).receipt.layerKeys)
    assertEquals(keys.distinct.length, 3)
    assert(!SurfaceMapInterpolation.VertexScalar.faceFlat && SurfaceMapInterpolation.VertexScalar.scalar)

  test("scene documents carry the face reduction and require face-flat support, not scalar interpolation"):
    val doc = document(Some(SurfaceFaceReduction.MaxMagnitude))
    val json = SurfaceSceneCodec.encode(doc)
    assert(json.contains("\"faceReduction\":\"max-magnitude\""), json)
    val decoded = SurfaceSceneCodec.decode(json).toOption.get
    assertEquals(decoded.layers.head.faceReduction, Some(SurfaceFaceReduction.MaxMagnitude))
    assert(!decoded.layers.head.scalarInterpolation)
    assert(decoded.restore(model(Some(SurfaceFaceReduction.MaxMagnitude)), bindings).isRight)
    // The declared reduction is part of the layer identity.
    assert(decoded.restore(model(Some(SurfaceFaceReduction.Mean)), bindings).isLeft)
    assert(decoded.restore(model(None), bindings).isLeft)
    val interpolating = SurfaceBackendCapabilities(SurfaceBackendId.unsafe("interpolating"), SurfacePlanRevision.Current,
      Set(SurfaceBackendFeature.ScalarInterpolation, SurfaceBackendFeature.FragmentComposition))
    assertEquals(decoded.admit(interpolating), Left(SurfaceSceneError.MissingCapabilities(Vector(SurfaceBackendFeature.FaceFlatScalar))))
    val faceFlat = interpolating.copy(features = Set(SurfaceBackendFeature.FaceFlatScalar, SurfaceBackendFeature.FragmentComposition))
    assert(decoded.admit(faceFlat).isRight)
    // An interpolated-scalar document still requires interpolation, never face-flat support.
    val scalarDoc = SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(document(None))).toOption.get
    assert(scalarDoc.admit(faceFlat).isLeft)
    assert(scalarDoc.admit(interpolating).isRight)

  test("documents without face-flat layers are unchanged and malformed reductions are refused"):
    val plainJson = SurfaceSceneCodec.encode(document(None))
    assert(!plainJson.contains("faceReduction"))
    val json = SurfaceSceneCodec.encode(document(Some(SurfaceFaceReduction.Mean)))
    assert(json.contains("\"faceReduction\":\"mean\""))
    assert(SurfaceSceneCodec.decode(json.replace("\"faceReduction\":\"mean\"", "\"faceReduction\":\"median\"")).isLeft)
    // A layer cannot both interpolate and reduce per face.
    assert(SurfaceSceneCodec.decode(json.replace("\"scalarInterpolation\":false", "\"scalarInterpolation\":true")).isLeft)
