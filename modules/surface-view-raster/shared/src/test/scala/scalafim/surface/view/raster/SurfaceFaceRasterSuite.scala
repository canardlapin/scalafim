package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.view.*

class SurfaceFaceRasterSuite extends munit.FunSuite:
  test("adjacent faces retain constant color and original picks across their shared edge"):
    val plan = SurfaceFaceFixture.plan
    val result = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(128, 128),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    val counts = Array(0, 0)
    var y = 0
    while y < 128 do
      var x = 0
      while x < 128 do
        result.pick(x, y).toOption.flatten.foreach: pick =>
          counts(pick.face) += 1
          val expected = if pick.face == 0 then SurfaceFaceFixture.Red else SurfaceFaceFixture.Blue
          assertEquals(result.image.pixelUnsafe(x, y), expected)
          val corners = if pick.face == 0 then Vector(0, 1, 2) else Vector(0, 2, 3)
          assert(corners.contains(pick.vertex))
          val weights = Vector(pick.barycentricA, pick.barycentricB, pick.barycentricC)
          val chosen = weights.indices.maxBy(weights(_))
          assertEquals(pick.vertex, corners(chosen))
          assertEqualsDouble(weights.sum, 1.0, 1e-6)
        x += 1
      y += 1
    assert(counts.forall(_ > 1000), counts.toVector.toString)

  test("a face overlay blends with the surface base without leaking into its neighbor"):
    val plan = SurfaceFaceFixture.plan
    val layer = plan.layers.head.copy(opacity = DisplayOpacity.unsafe(0.5))
    val result = SurfaceRasterizer.render(plan.copy(layers = Vector(layer)), RasterDimensions.unsafe(128, 128),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    for (x, y) <- Vector((90, 80), (30, 40)) do
      val pick = result.pick(x, y).toOption.flatten.get
      val sample = if pick.face == 0 then SurfaceFaceFixture.Red else SurfaceFaceFixture.Blue
      val expected = DisplayBlendMode.Normal.composite(Rgba32.unsafe(184, 184, 184), sample, layer.opacity)
      assertEquals(result.image.pixelUnsafe(x, y), expected)
