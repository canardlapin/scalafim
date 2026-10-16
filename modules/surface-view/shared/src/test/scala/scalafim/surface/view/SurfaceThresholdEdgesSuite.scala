package scalafim.surface.view

import intaglio.*
import scalafim.surface.*

class SurfaceThresholdEdgesSuite extends munit.FunSuite:
  private val mapping = SurfaceVisibilityFixture.ThresholdMapping
  test("visibility signs hide threshold equality and invalid values rather than encoding scalar distances"):
    val edge = SurfaceThresholdEdges.packet(Array(-.50001, -.5, 0, .5, .50001, Double.NaN), mapping).get
    assertEquals((0 until 6).map(edge.signs(_)).toVector, Vector(1f, -1f, -1f, -1f, 1f, -1f))
    assertEquals(edge.colors(2), Rgba32.unsafe(255, 0, 0).toPackedInt)
    assertEquals(edge.colors(5), 0)

  test("finite extreme windows preserve visible overlay colors"):
    val red = Rgba32.unsafe(255, 0, 0)
    val extreme = mapping.copy(scale = ScalarScale.sequential(DisplayWindow.unsafe(-1e308, 1e308), ScalarRamp.linear(red, red)))
    val packet = SurfaceThresholdEdges.packet(Array(-2.0, 2.0), extreme).get
    assertEquals(Vector(packet.colors(0), packet.colors(1)), Vector(red.toPackedInt, red.toPackedInt))

  test("edge policy overlays in linear light and retains the underlay alpha"):
    val plan = SurfaceVisibilityFixture.plan("threshold-edge")
    val evaluator = new SurfaceFragmentEvaluator(plan.meshes.head, plan.layers, SurfaceLighting.Unlit,
      Rgba32.unsafe(0, 0, 255, 128), colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)
    // At the visibility boundary, reference coverage is one half. Vertex
    // scalar interpolation would put the cutoff elsewhere for these samples.
    assertEqualsDouble(evaluator.edgeSign(0, 1, 2, .5, .5, 0), 0, 0)
    val boundary = evaluator.color(0, 0, 1, 2, .5, .5, 0, .1)
    assertEquals(boundary, Rgba32.unsafe(188, 0, 188, 128))
    assertEquals(evaluator.color(0, 0, 1, 2, .75, .25, 0), Rgba32.unsafe(0, 0, 255, 128))
    assertEquals(evaluator.color(0, 0, 1, 2, .25, .75, 0), Rgba32.unsafe(255, 0, 0, 128))

  test("helper-lane overlay alpha is clamped before mixing linear colors"):
    val plan = SurfaceVisibilityFixture.plan("threshold-edge")
    val colors = Vector(Rgba32.unsafe(128, 0, 0, 0), Rgba32.unsafe(128, 0, 0),
      Rgba32.unsafe(128, 0, 0), Rgba32.unsafe(128, 0, 0))
    val layer = plan.layers.head.copy(thresholdEdge = Some(SurfaceThresholdEdgePacket(
      new FloatBufferView(Array.fill(4)(1f)), new IntBufferView(colors.map(_.toPackedInt).toArray))))
    val evaluator = new SurfaceFragmentEvaluator(plan.meshes.head, Vector(layer), SurfaceLighting.Unlit,
      Rgba32.unsafe(0, 0, 128), colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)
    assertEquals(evaluator.color(0, 0, 1, 2, -1, 2, 0), Rgba32.unsafe(128, 0, 0))
    assertEquals(evaluator.color(0, 0, 1, 2, 2, -1, 0), Rgba32.unsafe(0, 0, 128))

  test("only the eligible topmost visible layer carries a threshold edge packet"):
    val geometry = SurfaceFaceFixture.geometry
    val surface = SurfaceFaceFixture.Surface
    val edge = SurfaceLayer.scalarWithThresholdEdges(SurfaceLayerId.unsafe("edge"), surface, geometry,
      Array(-.25, .75, .75, -.25), mapping).toOption.get
    val top = SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("top"), surface, geometry,
      Vector.fill(4)(Rgba32.unsafe(0, 255, 0))).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), Vector(edge, top)).toOption.get
    val state = SurfaceViewerState.initial(model).copy(colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)
    assert(SurfaceCompiler.compile(model, state).toOption.get.layers.forall(_.thresholdEdge.isEmpty))
    val reordered = state.copy(layerOrder = state.layerOrder.reverse)
    val packet = SurfaceCompiler.compile(model, reordered).toOption.get.layers.last
    assert(packet.thresholdEdge.nonEmpty)
    assert(SurfaceCompiler.compile(model, reordered.copy(colorPipeline = SurfaceColorPipeline.LegacyDisplayBytes)).isLeft)
    val invalid = SurfaceCompositionPlan.fromLegacy(SurfaceCompiler.compile(model, reordered).toOption.get).toOption.get
    assert(SurfaceCompositionPlan.make(invalid.views.map(v => v.copy(instances = v.instances.map(i =>
      i.copy(layerOrder = i.layerOrder.reverse)))), invalid.resources).isLeft)

  test("direct threshold-edge packets must retain the compiler's fragment stack invariants"):
    val plan = SurfaceVisibilityFixture.plan("threshold-edge")
    val layer = plan.layers.head
    val edge = layer.thresholdEdge.get
    val short = SurfaceThresholdEdgePacket(new FloatBufferView(Array(1f)), edge.colors)
    val nonfinite = SurfaceThresholdEdgePacket(new FloatBufferView(Array(Float.NaN, 1f, 1f, 1f)), edge.colors)
    val malformed = plan.copy(layers = Vector(layer.copy(thresholdEdge = Some(short))))
    assert(SurfaceThresholdEdgeValidation.validate(plan).isRight)
    assert(SurfaceThresholdEdgeValidation.validate(malformed).isLeft)
    assert(SurfaceCompositionPlan.fromLegacy(malformed).isLeft)
    assert(SurfaceThresholdEdgeValidation.validate(plan.copy(layers = Vector(layer.copy(thresholdEdge = Some(nonfinite))))).isLeft)
    assert(SurfaceThresholdEdgeValidation.validate(plan.copy(colorPipeline = SurfaceColorPipeline.LegacyDisplayBytes)).isLeft)
    assert(SurfaceThresholdEdgeValidation.validate(plan.copy(fragmentSurfaces = Set.empty)).isLeft)
    val underlay = layer.copy(layer = SurfaceLayerId.unsafe("underlay"), thresholdEdge = None)
    assert(SurfaceThresholdEdgeValidation.validate(plan.copy(layers = Vector(layer, underlay))).isLeft)
    assert(SurfaceThresholdEdgeValidation.validate(plan.copy(layers = Vector(layer.copy(blendMode = DisplayBlendMode.Screen)))).isLeft)
    assert(SurfaceThresholdEdgeValidation.validate(plan.copy(layers = Vector(layer.copy(
      interpolation = SurfaceMapInterpolation.NearestVertex)))).isLeft)
