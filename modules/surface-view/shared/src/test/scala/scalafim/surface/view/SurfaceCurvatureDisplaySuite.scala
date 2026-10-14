package scalafim.surface.view

import intaglio.*

class SurfaceCurvatureDisplaySuite extends munit.FunSuite:
  test("reference curvature gray levels and polarity are explicit"):
    val mapping = SurfaceCurvatureDisplay.Default.mapping
    assertEquals(Vector(-1.0, 0.0, 1.0).map(mapping.color(_).red), Vector(191, 128, 64))
    val reversed = SurfaceCurvatureDisplay.make(polarity = SurfaceLayer.SulcalPolarity.NegativeIsSulcal).toOption.get.mapping
    assertEquals(Vector(-1.0, 0.0, 1.0).map(reversed.color(_).red), Vector(64, 128, 191))

  test("contrast saturation follows the independent formula across the domain"):
    for brightness <- Vector(.2, .5, .8); contrast <- Vector(0.0, .5, 2.0); smoothness <- Vector(.25, 1.0, 3.0) do
      val style = SurfaceCurvatureDisplay.make(brightness, contrast, smoothness).toOption.get
      for i <- -100 to 100 do
        val x = i / 50.0
        val expected = math.round((brightness - (x / smoothness).max(-.5).min(.5) * contrast).max(0).min(1) * 255).toInt
        assert(math.abs(style.mapping.color(x).red - expected) <= 1)
    assertEquals(SurfaceCurvatureDisplay.Default.mapping.color(Double.NaN).alpha, 0)

  test("invalid controls fail at construction"):
    assert(SurfaceCurvatureDisplay.make(brightness = 1.1).isLeft)
    assert(SurfaceCurvatureDisplay.make(contrast = -1).isLeft)
    assert(SurfaceCurvatureDisplay.make(smoothness = 0).isLeft)
    assert(SurfaceCurvatureDisplay.make(smoothness = Double.PositiveInfinity).isLeft)
