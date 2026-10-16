package scalafim.surface.view

import intaglio.*
import scalafim.surface.{SurfaceGeometry, SurfaceMatrix, VertexId}

class SurfaceBivariateLegendSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(String.valueOf(error)), identity)
  private val surface = SurfaceFaceFixture.Surface
  private val first = SurfaceLayerId.unsafe("bivariate-first")
  private val second = SurfaceLayerId.unsafe("bivariate-second")
  private val geometry = SurfaceFaceFixture.geometry
  private val effectTitle = LegendTitle.make("Effect", Some("z")).toOption.get
  private val confidenceTitle = LegendTitle.make("Confidence", Some("probability")).toOption.get
  private val effectTicks = Vector(AxisTick.unsafe(-1, "low"), AxisTick.unsafe(1, "high"))
  private val confidenceTicks = Vector(AxisTick.unsafe(0, "none"), AxisTick.unsafe(1, "certain"))
  private val scalar = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
    ScalarRamp.linear(Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 0, 0))))
  private val mapping = BivariateEffectConfidenceMapping(scalar)
  private val request = SurfaceLegendRequest.Bivariate(SurfaceLegendLayers.one(first), effectTitle, confidenceTitle,
    effectTicks, confidenceTicks, width = 7, height = 5)
  private val reference = SurfaceExternalReference(SurfaceAssetUri.unsafe("fixture:bivariate"), SurfaceContentDigest.unsafe("e" * 64))
  private val provenance = SurfaceProvenance.make("bivariate-test", "1", "2026-10-14").toOption.get

  private def matrix(values: Vector[Vector[Double]], target: SurfaceGeometry = geometry): SurfaceMatrix[Double] =
    SurfaceMatrix.fromRows(target, Vector.tabulate(target.vertexCount)(VertexId(_)), values)

  private def layer(id: SurfaceLayerId, value: BivariateEffectConfidenceMapping = mapping): SurfaceLayer =
    ok(SurfaceLayer.bivariateEffectConfidence(id, surface,
      matrix(Vector.fill(geometry.vertexCount)(Vector(-1.0, 1.0))),
      matrix(Vector.fill(geometry.vertexCount)(Vector(0.0, 1.0))), value))

  private def model(firstMapping: BivariateEffectConfidenceMapping = mapping,
    secondMapping: Option[BivariateEffectConfidenceMapping] = None): SurfaceViewerModel =
    val layers = Vector(layer(first, firstMapping)) ++ secondMapping.map(value => layer(second, value))
    ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(surface, geometry))), layers))

  private def bindings(viewer: SurfaceViewerModel): SurfaceSceneBindings =
    SurfaceSceneBindings(Map(surface -> reference), viewer.layers.map(_.id -> reference).toMap)

  test("bivariate binding uses the paired layer mapping and preserves its key pixels"):
    val viewer = model()
    val legend = ok(SurfaceLegend.bind(request, viewer, SurfaceFaceFixture.state(viewer)))
    val key = legend.content match
      case SurfaceLegendContent.Bivariate(value) => value
      case other => fail(s"expected bivariate key, got $other")
    assertEquals(key.mapping.canonicalKey, mapping.canonicalKey)
    assertEquals(key.colorAt(0.0, 0.5), mapping.color(0.0, 0.5))
    assertEquals(legend.sources.map(_.mappingKey), Vector(mapping.canonicalKey))
    val retitled = SurfaceLegendRequest.Bivariate(SurfaceLegendLayers.one(first),
      LegendTitle.make("Effect size", Some("z")).toOption.get, confidenceTitle, effectTicks, confidenceTicks, 7, 5)
    assertNotEquals(ok(SurfaceLegend.bind(retitled, viewer, SurfaceFaceFixture.state(viewer))).canonicalKey, legend.canonicalKey)
    val reticked = SurfaceLegendRequest.Bivariate(SurfaceLegendLayers.one(first), effectTitle, confidenceTitle,
      Vector(AxisTick.unsafe(-1, "minimum"), AxisTick.unsafe(1, "maximum")), confidenceTicks, 7, 5)
    assertNotEquals(ok(SurfaceLegend.bind(reticked, viewer, SurfaceFaceFixture.state(viewer))).canonicalKey, legend.canonicalKey)
    val changed = model(mapping.copy(policy = BivariateConfidencePolicy.ConfidenceAlpha))
    assertEquals(legend.validateCurrent(changed, SurfaceFaceFixture.state(changed)).left.toOption, Some(SurfaceLegendError.StaleLegend))

  test("bivariate binding refuses hidden sources and mismatched mappings"):
    val viewer = model()
    val initial = SurfaceFaceFixture.state(viewer)
    val hidden = ok(SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.SetLayerVisible(first, false)))
    assertEquals(SurfaceLegend.bind(request, viewer, hidden).left.toOption, Some(SurfaceLegendError.HiddenLayer(first)))
    val other = mapping.copy(policy = BivariateConfidencePolicy.ConfidenceAlpha)
    val paired = model(secondMapping = Some(other))
    val mismatch = request match
      case SurfaceLegendRequest.Bivariate(_, a, b, x, y, width, height) =>
        SurfaceLegendRequest.Bivariate(ok(SurfaceLegendLayers.make(Vector(first, second))), a, b, x, y, width, height)
      case _ => fail("expected bivariate request")
    assertEquals(SurfaceLegend.bind(mismatch, paired, SurfaceFaceFixture.state(paired)).left.toOption,
      Some(SurfaceLegendError.IncompatibleMappings(Vector(first, second))))

  test("bivariate legends and mapping identity survive v8 documents and reject changed mappings"):
    val viewer = model()
    val state = SurfaceFaceFixture.state(viewer)
    val document = ok(SurfaceSceneDocument.capture(viewer, state, bindings(viewer), provenance, legendRequests = Vector(request)))
    assertEquals(document.layers.head.bivariateMappingKey, Some(mapping.canonicalKey))
    val encoded = SurfaceSceneCodec.encode(document)
    assert(encoded.contains("\"bivariateMappingKey\""))
    assert(encoded.contains("\"effectQuantity\":\"Effect\""))
    val decoded = ok(SurfaceSceneCodec.decode(encoded))
    assertEquals(SurfaceSceneCodec.encode(decoded), encoded)
    assertEquals(decoded.restore(viewer, bindings(viewer)), Right(state))
    assert(decoded.restore(model(mapping.copy(policy = BivariateConfidencePolicy.ConfidenceAlpha)), bindings(viewer)).isLeft)

  test("bivariate drawing returns a typed layout failure when long axis labels leave no key width"):
    val viewer = model()
    val longLabel = "confidence-label-" * 12
    val narrow = SurfaceLegendRequest.Bivariate(SurfaceLegendLayers.one(first), effectTitle, confidenceTitle,
      effectTicks, Vector(AxisTick.unsafe(0, longLabel), AxisTick.unsafe(1, longLabel)), 7, 5)
    val legend = ok(SurfaceLegend.bind(narrow, viewer, SurfaceFaceFixture.state(viewer)))
    val style = ScalarLegendStyle(widthPt = 60, fontPt = 10, marginPt = 8, gapPt = 5)
    assert(SurfaceLegendPublication.draw(legend, style, SwatchLegendStyle()).isLeft)

  test("threshold edge opt-in persists through v8 and refuses a model without the policy"):
    val edgeId = SurfaceLayerId.unsafe("threshold-edge")
    val edge = ok(SurfaceLayer.scalarWithThresholdEdges(edgeId, surface, geometry, Array(-1.0, -.5, .5, 1.0), scalar))
    val viewer = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(surface, geometry))), Vector(edge)))
    val state = SurfaceFaceFixture.state(viewer)
    val document = ok(SurfaceSceneDocument.capture(viewer, state, bindings(viewer), provenance))
    assert(document.layers.head.thresholdEdges)
    val decoded = ok(SurfaceSceneCodec.decode(SurfaceSceneCodec.encode(document)))
    assert(decoded.layers.head.thresholdEdges)
    assertEquals(decoded.restore(viewer, bindings(viewer)), Right(state))
    val ordinary = ok(SurfaceLayer.scalar(edgeId, surface, geometry, Array(-1.0, -.5, .5, 1.0), scalar.colorizer))
    val changed = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(surface, geometry))), Vector(ordinary)))
    assert(decoded.restore(changed, bindings(changed)).isLeft)
