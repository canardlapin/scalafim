package scalafim.surface.view

import intaglio.*

class SurfaceColorPipelineSuite extends munit.FunSuite:
  private val black = Rgba32.unsafe(0, 0, 0)
  private val white = Rgba32.unsafe(255, 255, 255)
  private val mesh = SurfaceMeshPacket(SurfaceId.unsafe("analytic"), SurfaceResourceKey("triangle"),
    new FloatBufferView(Array(-1f, -1f, 0f, 1f, -1f, 0f, 0f, 1f, 0f)),
    new FloatBufferView(Array(0f, 0f, 1f, 0f, 0f, 1f, 0f, 0f, 1f)), new IntBufferView(Array(0, 1, 2)))
  private def layer(colors: Vector[Rgba32], opacity: Double = 1.0): SurfaceLayerPacket =
    val buffer = new IntBufferView(colors.map(_.toPackedInt).toArray)
    SurfaceLayerPacket(SurfaceLayerId.unsafe("colors"), mesh.surface, SurfaceResourceKey("colors"),
      buffer, DisplayOpacity.unsafe(opacity), DisplayBlendMode.Normal, sampleColors = Some(buffer))
  private def evaluate(layers: Vector[SurfaceLayerPacket], pipeline: SurfaceColorPipeline,
      lighting: SurfaceLighting = SurfaceLighting.Unlit, base: Rgba32 = black): Rgba32 =
    new SurfaceFragmentEvaluator(mesh, layers, lighting, base, colorPipeline = pipeline)
      .color(0, 0, 1, 2, .5, .5, 0)

  test("black-white midpoint decodes vertices before interpolation"):
    val colors = Vector(layer(Vector(black, white, black)))
    assertEquals(evaluate(colors, SurfaceColorPipeline.LegacyDisplayBytes), Rgba32.unsafe(128, 128, 128))
    assertEquals(evaluate(colors, SurfaceColorPipeline.DisplaySrgbLinearLight), Rgba32.unsafe(188, 188, 188))

  test("half-light gray is 92 and layer opacity composes encoded bytes before interpolation"):
    val gray = Rgba32.unsafe(128, 128, 128)
    val light = SurfaceLighting.directional(.5, 0, 0, 0, 1).toOption.get
    assertEquals(evaluate(Vector(layer(Vector.fill(3)(gray))), SurfaceColorPipeline.DisplaySrgbLinearLight, light),
      Rgba32.unsafe(92, 92, 92))
    assertEquals(evaluate(Vector(layer(Vector.fill(3)(white), .5)), SurfaceColorPipeline.DisplaySrgbLinearLight), gray)
    // Composing this layer after interpolation gives 94 instead of 92.
    assertEquals(evaluate(Vector(layer(Vector(black, white, black), .5)), SurfaceColorPipeline.DisplaySrgbLinearLight).red, 92)

  test("alpha uses coverage interpolation without a color transfer"):
    val transparentWhite = Rgba32.unsafe(255, 255, 255, 0)
    val value = evaluate(Vector(layer(Vector(white, transparentWhite, white))),
      SurfaceColorPipeline.DisplaySrgbLinearLight, base = Rgba32.unsafe(0, 0, 0, 0))
    assertEquals(value.alpha, 128)

  test("linear composition refuses missing source colors before evaluator access"):
    val legacy = SurfaceFaceFixture.plan
    val resources = SurfaceCompositionPlan.fromLegacy(legacy).toOption.get
    val view = resources.views.head.copy(colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)
    assert(SurfaceCompositionPlan.make(Vector(view), resources.resources).isLeft)
