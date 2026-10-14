package scalafim.surface.view

import intaglio.*
import scalafim.surface.{SurfaceGeometry, SurfaceMatrix, VertexId}

class SurfaceBivariateLayerSuite extends munit.FunSuite:
  private def ok[A](value: Either[?, A]): A = value.fold(error => fail(String.valueOf(error)), identity)
  private val geometry = SurfaceFaceFixture.geometry
  private val id = SurfaceLayerId.unsafe("bivariate")
  private val surface = SurfaceFaceFixture.Surface
  private val effectMapping = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1, 1),
    ScalarRamp.linear(Rgba32.unsafe(0, 0, 255), Rgba32.unsafe(255, 0, 0))))
  private val hsv = BivariateEffectConfidenceMapping(effectMapping)

  private def matrix(values: Vector[Vector[Double]], target: SurfaceGeometry = geometry): SurfaceMatrix[Double] =
    SurfaceMatrix.fromRows(target, Vector.tabulate(target.vertexCount)(VertexId(_)), values)

  test("paired full vertex fields retain frame selection and expose their mapping"):
    val effect = matrix(Vector(Vector(-1, 0), Vector(-.5, .5), Vector(0, 1), Vector(.25, .75)))
    val confidence = matrix(Vector(Vector(0, 1), Vector(.25, .5), Vector(.5, .75), Vector(1, 0)))
    val layer = ok(SurfaceLayer.bivariateEffectConfidence(id, surface, effect, confidence, hsv,
      DisplayOpacity.unsafe(.5), DisplayBlendMode.Screen))
    assertEquals(layer.frameCount, 2)
    assertEquals(layer.interpolation, SurfaceMapInterpolation.VertexColor)
    assertEquals(layer.bivariateMapping, Some(hsv))
    val colors = new Array[Int](geometry.vertexCount)
    layer.writeColors(1, SurfaceLayerPresentation(id, true, DisplayOpacity.unsafe(.5), None, None), colors)
    assertEquals(colors(0), hsv.color(0, 1).toPackedInt)
    assertEquals(colors(2), hsv.color(1, .75).toPackedInt)

  test("reference HSV confidence is opaque while alpha confidence remains explicit"):
    assertEquals(hsv.color(.2, .2).alpha, 255)
    val alpha = BivariateEffectConfidenceMapping(effectMapping, BivariateConfidencePolicy.ConfidenceAlpha)
    assert(alpha.color(.2, .2).alpha < 255)

  test("mismatched source identities and frame counts are refused"):
    val one = matrix(Vector.fill(4)(Vector(0.0)))
    val two = matrix(Vector.fill(4)(Vector(0.0, 1.0)))
    assert(SurfaceLayer.bivariateEffectConfidence(id, surface, one, two, hsv).isLeft)
    val reordered = SurfaceMatrix.fromRows(geometry, Vector(VertexId(1), VertexId(0), VertexId(2), VertexId(3)), Vector.fill(4)(Vector(0.0)))
    assert(SurfaceLayer.bivariateEffectConfidence(id, surface, one, reordered, hsv).isLeft)
