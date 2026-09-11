package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceFaceFlatRasterSuite extends munit.FunSuite:
  private val surface = SurfaceId.unsafe("face-flat-raster")
  private val onset = ScalarMapping(ScalarScale.split(DisplayWindow.unsafe(-1.0, 1.0), 0.0, -0.25, 0.25,
    ScalarRamp.linear(Rgba32.unsafe(0, 255, 255), Rgba32.unsafe(0, 64, 255)),
    ScalarRamp.linear(Rgba32.unsafe(255, 64, 0), Rgba32.unsafe(255, 255, 0))).toOption.get)
  private val curvature = ScalarMapping(ScalarScale.sequential(DisplayWindow.unsafe(-1.0, 1.0),
    ScalarRamp.linear(Rgba32.unsafe(48, 48, 48), Rgba32.unsafe(208, 208, 208))))

  test("the reference raster draws face-flat layers as one colour per face, equal to the face rule"):
    assert(SurfaceRasterizer.capabilities.supports(SurfaceBackendFeature.FaceFlatScalar))
    val n = 6
    val coordinates = new Array[Double]((n + 1) * (n + 1) * 3)
    for j <- 0 to n; i <- 0 to n do
      coordinates((j * (n + 1) + i) * 3) = 2.0 * i / n - 1.0
      coordinates((j * (n + 1) + i) * 3 + 1) = 2.0 * j / n - 1.0
    val faces = new Array[Int](n * n * 6)
    for cell <- 0 until n * n do
      val a = (cell / n) * (n + 1) + cell % n
      faces(cell * 6) = a; faces(cell * 6 + 1) = a + 1; faces(cell * 6 + 2) = a + n + 2
      faces(cell * 6 + 3) = a; faces(cell * 6 + 4) = a + n + 2; faces(cell * 6 + 5) = a + n + 1
    val geometry = SurfaceGeometry(TriangleMesh.fromArrays(coordinates, faces), Hemisphere.Left, SurfaceKind.Inflated)
    val random = new scala.util.Random(5)
    val values = Array.tabulate((n + 1) * (n + 1))(i => if i % 13 == 1 then Double.NaN else random.nextDouble() * 2 - 1)
    val curve = Array.tabulate((n + 1) * (n + 1))(_ => random.nextDouble() * 2 - 1)
    for reduction <- SurfaceFaceReduction.values do
      val layers = Vector(
        SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("curv"), surface, geometry, curve, curvature, SurfaceFaceReduction.Mean).toOption.get,
        SurfaceLayer.faceFlatScalar(SurfaceLayerId.unsafe("map"), surface, geometry, values, onset, reduction).toOption.get)
      val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(surface, geometry).toOption.get), layers).toOption.get
      val plan = SurfaceCompiler.compile(model, SurfaceFaceFixture.state(model)).toOption.get
      val colors = SurfaceFaceTexels.colors(plan, plan.meshes.head).toOption.get
      val result = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(96, 96), SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
      var checked = 0
      for y <- 0 until 96; x <- 0 until 96 do
        result.pick(x, y).toOption.flatten.foreach: pick =>
          val expected = Rgba32.fromPackedInt(colors(pick.face))
          val actual = result.image.pixelUnsafe(x, y)
          assertEquals((actual.red, actual.green, actual.blue), (expected.red, expected.green, expected.blue), s"$reduction pixel ($x, $y) face ${pick.face}")
          checked += 1
      assert(checked > 3000, s"only $checked covered pixels")
