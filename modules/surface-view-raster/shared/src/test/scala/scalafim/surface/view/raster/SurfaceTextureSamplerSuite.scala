package scalafim.surface.view.raster

import intaglio.*

class SurfaceTextureSamplerSuite extends munit.FunSuite:
  private def image(width: Int, height: Int)(pixel: (Int, Int) => Rgba32): RasterImage =
    RasterImage.unsafeFromOwnedPackedArray(RasterDimensions.unsafe(width, height),
      Array.tabulate(width * height)(i => pixel(i % width, i / width).toPackedInt))

  test("POT mips and fractional LOD have independently specified color averages"):
    val texture = image(4, 4)((x, _) => if x < 2 then Rgba32.unsafe(0, 0, 0) else Rgba32.unsafe(255, 255, 255))
    val sampler = new SurfaceTextureSampler(texture)
    assertEquals(sampler.sample(.25, .25, 0, 0, 0, 0).red, 0)
    assertEquals(sampler.sample(.25, .25, .5, 0, 0, .5).red, 0)
    assertEquals(sampler.sample(.25, .25, 1, 0, 0, 1).red, 128)
    assertEquals(sampler.sample(.25, .25, math.sqrt(.5), 0, 0, math.sqrt(.5)).red, 64)
    assertEquals(sampler.sample(0, .25, 0, 0, 0, 0).red, 128)

  test("mip reduction preserves premultiplied color through transparent checker texels"):
    val texture = image(8, 8)((x, y) => if (x + y) % 2 == 0 then Rgba32.unsafe(255, 0, 0) else Rgba32.unsafe(0, 0, 0, 0))
    val sampler = new SurfaceTextureSampler(texture)
    for footprint <- Vector(2.0, 4.0, 8.0) do
      val color = sampler.sample(.31, .63, footprint / 8, 0, 0, footprint / 8)
      assertEquals(color.red, 255)
      assertEquals(color.green, 0)
      assertEquals(color.blue, 0)
      assertEquals(color.alpha, 128)

  test("odd dimensions use explicit area averaging without losing the final row"):
    val texture = image(3, 3)((_, y) => Rgba32.unsafe(y * 120, 0, 0))
    assertEquals(new SurfaceTextureSampler(texture).sample(.5, .5, 1, 0, 0, 1).red, 120)
