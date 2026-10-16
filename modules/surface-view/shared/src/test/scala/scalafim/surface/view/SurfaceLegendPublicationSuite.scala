package scalafim.surface.view

import intaglio.*

class SurfaceLegendPublicationSuite extends munit.FunSuite:
  private val surface = SurfaceFaceFixture.Surface
  private val scalarId = SurfaceLayerId.unsafe("scalar")
  private val categoryId = SurfaceLayerId.unsafe("category")
  private val blue = Rgba32.unsafe(0, 0, 200)
  private val red = Rgba32.unsafe(200, 0, 0)
  private val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-4, 8), ScalarRamp.linear(blue, red)))
  private val geometry = SurfaceFaceFixture.geometry
  private val scalar = SurfaceLayer.scalar(scalarId, surface, geometry, Array(-4.0, -1, 2, 8), mapping.colorizer).toOption.get
  private val category = SurfaceLayer.labels(categoryId, surface, geometry, Array(1, 2, 1, 2),
    LabelColorizer(Map(1 -> red, 2 -> blue)), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
  private val viewer = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(scalar, category)).toOption.get
  private val state = SurfaceFaceFixture.state(viewer)
  private val title = LegendTitle.make("Estimated response relative to the comparison condition", Some("percent signal change")).toOption.get
  private val requests = Vector(SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(scalarId), title),
    SurfaceLegendRequest.Categorical(SurfaceLegendLayers.one(categoryId), LegendTitle.make("Functional regions").toOption.get,
      Map(1 -> "Motor and premotor association cortex", 2 -> "Visual association cortex")))
  private def spec(preset: SurfacePublicationPreset = SurfacePublicationPreset.ManuscriptSingleColumn): SurfacePublicationSpec =
    SurfacePublicationSpec(preset, Some("Surface map with calibrated legends"), SurfaceOrientationMark.LeftLateral)

  test("all presets reserve disjoint measured surface and legend regions within the page"):
    for preset <- SurfacePublicationPreset.values do
      val publication = SurfaceLegendPublication.prepare(viewer, state, spec(preset), requests).toOption.get
      val receipt = publication.receipt
      val frame = receipt.surfaceFrame
      assertEqualsDouble(frame.widthPt, publication.input.dimensions.width * 0.75, 0)
      assertEqualsDouble(frame.heightPt, publication.input.dimensions.height * 0.75, 0)
      for legend <- receipt.legends do
        assert(frame.leftPt + frame.widthPt < legend.frame.leftPt)
        assert(legend.frame.leftPt + legend.frame.widthPt <= preset.width * 0.75)
        assert(legend.frame.topPt + legend.frame.heightPt <= preset.height * 0.75)
      receipt.legends.sliding(2).foreach(pair => assert(pair(0).frame.topPt + pair(0).frame.heightPt < pair(1).frame.topPt))
      assertEquals(receipt.legends.map(_.canonicalKey), requests.map(request => SurfaceLegend.bind(request, viewer, state).toOption.get.canonicalKey))
      assertEquals(receipt.surface, publication.input.plan.receipt)

  test("render callback receives the prepared scene and exact dimensions; pixels remain in the returned image"):
    val publication = SurfaceLegendPublication.prepare(viewer, state, spec(), requests).toOption.get
    var calls = 0
    val artifact = publication.renderWith[String]: input =>
      calls += 1
      assertEquals(input.plan.receipt, publication.receipt.surface)
      assertEquals(input.background, spec().background)
      Right(RasterImage.tabulate(input.dimensions)((x, y) => if x == y then red else blue))
    .toOption.get
    assertEquals(calls, 1)
    assertEquals(artifact.receipt, publication.receipt)
    val image = artifact.scene.grobs.collectFirst { case value: Grob.Image => value.image }.get
    assertEquals(image.pixelUnsafe(0, 0), red)
    assertEquals(image.pixelUnsafe(1, 0), blue)
    val wrong = publication.renderWith[String](_ => Right(RasterImage.tabulate(RasterDimensions.unsafe(1, 1))((_, _) => red)))
    assert(wrong.isLeft)
    assertEquals(publication.renderWith[String](_ => Left("renderer failed")), Left("renderer failed"))

  test("window changes update publication identities and camera changes preserve legend calibration"):
    val initial = SurfaceLegendPublication.prepare(viewer, state, spec(), requests).toOption.get.receipt
    val changed = SurfaceViewer.reduce(viewer, state,
      SurfaceViewerAction.SetLayerWindow(scalarId, DisplayWindow.unsafe(-8, 16))).toOption.get
    val updated = SurfaceLegendPublication.prepare(viewer, changed, spec(), requests).toOption.get.receipt
    assertNotEquals(initial.legends.head.canonicalKey, updated.legends.head.canonicalKey)
    assertNotEquals(initial.surface.layerKeys, updated.surface.layerKeys)
    val camera = SurfaceViewer.reduce(viewer, state, SurfaceViewerAction.OrbitBy(20, 10)).toOption.get
    val orbited = SurfaceLegendPublication.prepare(viewer, camera, spec(), requests).toOption.get.receipt
    assertEquals(initial.legends.map(_.canonicalKey), orbited.legends.map(_.canonicalKey))
    assertNotEquals(initial.surface.cameraKey, orbited.surface.cameraKey)

  test("excessive legend stacks and invalid shared requests are rejected before rendering"):
    val overflow = SurfaceLegendPublication.prepare(viewer, state, spec(), Vector.fill(30)(requests.head))
    assert(overflow.left.toOption.exists(_.isInstanceOf[SurfaceLegendPublicationError.InsufficientSpace]))
    val shared = SurfaceLegendRequest.Continuous(SurfaceLegendLayers.make(Vector(scalarId, categoryId)).toOption.get, title)
    assert(SurfaceLegendPublication.prepare(viewer, state, spec(), Vector(shared)).isLeft)

  test("empty legends release the entire column; legacy swatches become an explicit manual card"):
    val empty = SurfaceLegendPublication.prepare(viewer, state, spec(), Vector.empty).toOption.get
    val withLegend = SurfaceLegendPublication.prepare(viewer, state, spec(), Vector(requests.head)).toOption.get
    assert(empty.input.dimensions.width > withLegend.input.dimensions.width)
    assertEquals(empty.receipt.legends, Vector.empty)
    val manual = spec().copy(legend = Vector(SurfaceLegendItem.unsafe("Selected", red)))
    val publication = SurfaceLegendPublication.prepare(viewer, state, manual, Vector.empty).toOption.get
    assertEquals(publication.receipt.legends.length, 1)
    assertEquals(publication.receipt.legends.head.sources, Vector.empty)
    assert(publication.receipt.publication.legendLabels.contains("Selected"))
