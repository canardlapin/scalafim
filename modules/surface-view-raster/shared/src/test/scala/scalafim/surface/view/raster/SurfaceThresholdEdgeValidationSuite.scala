package scalafim.surface.view.raster

import intaglio.RasterDimensions

import scalafim.surface.view.*

class SurfaceThresholdEdgeValidationSuite extends munit.FunSuite:
  test("raster entry rejects direct threshold-edge packets before rendering"):
    val plan = SurfaceVisibilityFixture.plan("threshold-edge")
    val layer = plan.layers.head
    val malformed = SurfaceThresholdEdgePacket(new FloatBufferView(Array(1f)), layer.thresholdEdge.get.colors)
    val result = SurfaceRasterizer.render(plan.copy(layers = Vector(layer.copy(thresholdEdge = Some(malformed))),
      fragmentSurfaces = Set.empty), RasterDimensions.unsafe(32, 32))
    assert(result.isLeft)
