package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.view.*

class SurfaceThresholdDerivativeSuite extends munit.FunSuite:
  test("perspective visibility derivatives pair rows from the bottom at even and odd heights"):
    val plan = SurfaceVisibilityFixture.plan("threshold-edge")
    val layer = plan.layers.head.copy(thresholdEdge = Some(SurfaceThresholdEdgePacket(
      new FloatBufferView(Array(-1f, -1f, 1f, 1f)),
      new IntBufferView(Array.fill(4)(Rgba32.unsafe(255, 0, 0).toPackedInt)))))
    val evaluator = new SurfaceFragmentEvaluator(plan.meshes.head, Vector(layer), SurfaceLighting.Unlit,
      Rgba32.unsafe(0, 0, 0), colorPipeline = SurfaceColorPipeline.DisplaySrgbLinearLight)
    // Triangle screen coordinates (0,0), (200,0), (0,200), with W=(1,1,2).
    // The projective sign field is (3y/400 - 1) / (1 - y/400).
    def vertex(x: Double, y: Double, inverseW: Double, a: Double, b: Double, c: Double) =
      SurfaceRasterizer.ScreenVertex(x, y, 0, inverseW, 0, 0, 0, 0, a, b, c)
    val a = vertex(0, 0, 1, 1, 0, 0)
    val b = vertex(200, 0, 1, 0, 1, 0)
    val c = vertex(0, 200, .5, 0, 0, .5)
    def sign(y: Double): Double = (3 * y / 400 - 1) / (1 - y / 400)
    val even = math.abs(sign(101.5) - sign(100.5))
    val odd = math.abs(sign(100.5) - sign(99.5))
    assert(math.abs(even - odd) > 1e-5)
    for (height, expected) <- Vector(256 -> even, 257 -> odd) do
      val actual = SurfaceRasterizer.thresholdWidth(evaluator, a, b, c, -40000, 0, 1, 2, 50, 100, height)
      assertEqualsDouble(actual, expected, 1e-14)
