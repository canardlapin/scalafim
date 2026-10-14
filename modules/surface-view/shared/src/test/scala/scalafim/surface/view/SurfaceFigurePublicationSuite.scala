package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceFigurePublicationSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(s"$error"), identity)
  private val model = SurfaceLinkedFixture.model(SurfaceLinkedFixture.playback(.5))
  private val state = SurfaceLinkedFixture.state(model, selected = 2, morph = .5)
  private val navigation = SurfaceLinkedFixture.navigation(model, state)
  private val request = Vector(SurfaceLinkedFixture.Legend)

  test("preparation preserves independently posed composition views and binds its legend to the same state"):
    val labels = navigation.views.map(view => view.id -> s"Orientation ${view.id.value}").toMap
    val publication = ok(SurfaceFigurePublication.prepare(model, state, navigation,
      ok(SurfaceFigureSpec.make(Some("Linked surface figure"), panelLabels = labels)), request))
    assertEquals(publication.input.plan.views.map(_.camera).distinct.length, navigation.views.length)
    assertEquals(publication.receipt.composition, publication.input.plan.receipt)
    assertEquals(publication.receipt.legends.map(_.canonicalKey),
      request.map(value => ok(SurfaceLegend.bind(value, model, state)).canonicalKey))
    assertEquals(publication.input.background.alpha, 0)
    val frame = publication.receipt.surfaceFrame
    publication.input.plan.views.foreach: view =>
      assert(view.viewport.x * publication.receipt.physicalWidthPt >= frame.leftPt)
      assert((view.viewport.x + view.viewport.width) * publication.receipt.physicalWidthPt <= frame.leftPt + frame.widthPt)
      assert(view.viewport.y * publication.receipt.physicalHeightPt >= frame.topPt)
      assert((view.viewport.y + view.viewport.height) * publication.receipt.physicalHeightPt <= frame.topPt + frame.heightPt)
      val expectedAspect = navigation.views.find(_.id == view.id).get.camera.aspectRatio.value
      assertEqualsDouble(view.viewport.width * publication.receipt.physicalWidthPt /
        (view.viewport.height * publication.receipt.physicalHeightPt), expectedAspect, 1e-12)
    publication.receipt.legends.foreach(legend => assert(legend.frame.leftPt >= frame.leftPt + frame.widthPt))

  test("callbacks receive exact scaled raster dimensions and physical layout remains fixed"):
    val one = ok(SurfaceFigurePublication.prepare(model, state, navigation,
      ok(SurfaceFigureSpec.make(outputScale = SurfaceFigureOutputScale.One)), request))
    val two = ok(SurfaceFigurePublication.prepare(model, state, navigation,
      ok(SurfaceFigureSpec.make(outputScale = SurfaceFigureOutputScale.Two)), request))
    assertEquals(one.input.dimensions, RasterDimensions.unsafe(1200, 900))
    assertEquals(two.input.dimensions, RasterDimensions.unsafe(2400, 1800))
    assertEquals(one.receipt.pixelsPerInch, 96)
    assertEquals(two.receipt.pixelsPerInch, 192)
    assertEqualsDouble(one.receipt.physicalWidthPt, two.receipt.physicalWidthPt, 0)
    assertEqualsDouble(one.receipt.physicalHeightPt, two.receipt.physicalHeightPt, 0)
    assertEquals(one.receipt.legends.map(_.frame), two.receipt.legends.map(_.frame))
    var observed = false
    val artifact = two.renderWith[String]: input =>
      observed = true
      assertEquals(input.plan.receipt, two.input.plan.receipt)
      assertEquals(input.pixelScale, 2.0)
      assertEquals(input.background.alpha, 0)
      Right(RasterImage.tabulate(input.dimensions)((x, y) => if x == y then Rgba32.unsafe(255, 0, 0) else Rgba32.unsafe(0, 0, 255, 0)))
    assert(observed)
    assert(artifact.isRight)
    assert(two.renderWith[String](_ => Right(RasterImage.tabulate(RasterDimensions.unsafe(1, 1))((_, _) => Rgba32.unsafe(0, 0, 0)))).isLeft)

  test("hidden or incompatible mapping requests are refused before rasterization"):
    val hidden = state.copy(presentations = state.presentations.updated(SurfaceLinkedFixture.Layer,
      state.presentations(SurfaceLinkedFixture.Layer).copy(visible = false)))
    assert(SurfaceFigurePublication.prepare(model, hidden, navigation,
      ok(SurfaceFigureSpec.make()), request).isLeft)
    val bivariate = SurfaceLegendRequest.Bivariate(SurfaceLegendLayers.one(SurfaceLinkedFixture.Layer),
      ok(LegendTitle.make("Effect")), ok(LegendTitle.make("Confidence")), Vector.empty, Vector.empty)
    assert(SurfaceFigurePublication.prepare(model, state, navigation,
      ok(SurfaceFigureSpec.make()), Vector(bivariate)).isLeft)
    val unknown = SurfaceViewId.make("unknown").toOption.get
    assert(SurfaceFigurePublication.prepare(model, state, navigation,
      ok(SurfaceFigureSpec.make(panelLabels = Map(unknown -> "Unknown"))), request).isLeft)

  test("composition-aware legend binding follows composition instances instead of legacy layout"):
    val left = SurfaceId.unsafe("legend-left")
    val right = SurfaceId.unsafe("legend-right")
    val leftLayer = SurfaceLayerId.unsafe("legend-left-layer")
    val rightLayer = SurfaceLayerId.unsafe("legend-right-layer")
    val mapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
      ScalarRamp.linear(Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 0, 0))))
    def layer(id: SurfaceLayerId, surface: SurfaceId) = ok(SurfaceLayer.scalar(id, surface, SurfaceFaceFixture.geometry,
      Array(-1.0, -.25, .25, 1.0), mapping.colorizer))
    val viewer = ok(SurfaceViewerModel.make(Vector(
      ok(SurfaceAsset.make(left, SurfaceFaceFixture.geometry)), ok(SurfaceAsset.make(right, SurfaceFaceFixture.geometry))),
      Vector(layer(leftLayer, left), layer(rightLayer, right))))
    val leftState = SurfaceViewerState.initial(viewer).copy(layout = SurfaceLayout.Single(left))
    val rightState = leftState.copy(layout = SurfaceLayout.Single(right))
    val rightPlan = ok(SurfaceCompositionPlan.fromLegacy(ok(SurfaceCompiler.compile(viewer, rightState))))
    def request(id: SurfaceLayerId) = SurfaceLegendRequest.Continuous(SurfaceLegendLayers.one(id),
      ok(LegendTitle.make("Effect")))
    assert(SurfaceLegend.bind(request(rightLayer), viewer, leftState).isLeft)
    assert(SurfaceLegend.bind(request(rightLayer), viewer, leftState, rightPlan).isRight)
    assert(SurfaceLegend.bind(request(leftLayer), viewer, leftState).isRight)
    assert(SurfaceLegend.bind(request(leftLayer), viewer, leftState, rightPlan).isLeft)
