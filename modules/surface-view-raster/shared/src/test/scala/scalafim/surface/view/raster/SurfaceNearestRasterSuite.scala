package scalafim.surface.view.raster

import intaglio.*
import scalafim.surface.*
import scalafim.surface.view.*

class SurfaceNearestRasterSuite extends munit.FunSuite:
  test("nearest regions match original triangle barycentrics under orthographic and perspective views"):
    val model = SurfaceNearestFixture.model
    val plain = SurfaceLayer.packedRgba(SurfaceNearestFixture.Layer, SurfaceNearestFixture.Surface,
      SurfaceFaceFixture.geometry, Vector.fill(4)(Rgba32.unsafe(255, 255, 255))).toOption.get
    val referenceModel = SurfaceViewerModel.make(model.surfaces, Vector(plain)).toOption.get
    for perspective <- Vector(false, true) do
      val initial = SurfaceNearestFixture.state(model)
      val projected = if !perspective then initial else SurfaceViewer.reduce(model, initial,
        SurfaceViewerAction.SetProjection(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50.0)))).toOption.get
      val state = if !perspective then projected else
        val oblique = SurfaceViewer.reduce(model, projected, SurfaceViewerAction.OrbitBy(30.0, 20.0)).toOption.get
        val clipping = SurfaceClipping.worldPlanes(Vector(WorldClipPlane.unsafe(1.0, 0.0, 0.0, 0.4, ClipKeepSide.Positive))).toOption.get
        SurfaceViewer.reduce(model, oblique, SurfaceViewerAction.SetClipping(clipping)).toOption.get
      val plan = SurfaceCompiler.compile(model, state).toOption.get
      val referencePlan = SurfaceCompiler.compile(referenceModel, state).toOption.get
      val dimensions = RasterDimensions.unsafe(192, 192)
      val style = SurfaceRasterStyle(culling = TriangleCulling.None)
      val actual = SurfaceRasterizer.render(plan, dimensions, style).toOption.get
      val reference = SurfaceRasterizer.render(referencePlan, dimensions, style).toOption.get
      var checked = 0
      for y <- 0 until 192; x <- 0 until 192 do
        reference.pick(x, y).toOption.flatten.foreach: original =>
          val weights = Vector(original.barycentricA, original.barycentricB, original.barycentricC)
          val ranked = weights.sorted.reverse
          if weights.min > 0.02 && ranked(0) - ranked(1) > 0.02 then
            val vertices = if original.face == 0 then Vector(0, 1, 2) else Vector(0, 2, 3)
            val vertex = vertices(weights.indices.maxBy(weights(_)))
            assertEquals(actual.image.pixelUnsafe(x, y), SurfaceNearestFixture.Palette(vertex))
            val pick = actual.pick(x, y).toOption.flatten.get
            assertEquals(pick.face, original.face)
            assertEquals(pick.vertex, vertex)
            assertEqualsDouble(pick.barycentricA, original.barycentricA, 1e-6)
            assertEqualsDouble(pick.barycentricB, original.barycentricB, 1e-6)
            assertEqualsDouble(pick.barycentricC, original.barycentricC, 1e-6)
            checked += 1
      assert(checked > 1000, s"insufficient perspective=$perspective coverage: $checked")

  test("face and nearest layers blend as constant samples within each region"):
    val model = SurfaceNearestFixture.model
    val underlay = SurfaceFaceFixture.model.layers.head
    val mixed = SurfaceViewerModel.make(model.surfaces, underlay +: model.layers).toOption.get
    val state = SurfaceViewer.reduce(mixed, SurfaceNearestFixture.state(mixed),
      SurfaceViewerAction.SetLayerOpacity(SurfaceNearestFixture.Layer, DisplayOpacity.unsafe(0.5))).toOption.get
    val plan = SurfaceCompiler.compile(mixed, state).toOption.get
    val result = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(128, 128),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    var checked = 0
    for y <- 0 until 128; x <- 0 until 128 do
      result.pick(x, y).toOption.flatten.foreach: pick =>
        val weights = Vector(pick.barycentricA, pick.barycentricB, pick.barycentricC).sorted.reverse
        if weights(0) - weights(1) > 0.03 then
          val base = if pick.face == 0 then SurfaceFaceFixture.Red else SurfaceFaceFixture.Blue
          val expected = DisplayBlendMode.Normal.composite(base, SurfaceNearestFixture.Palette(pick.vertex), DisplayOpacity.unsafe(0.5))
          assertEquals(result.image.pixelUnsafe(x, y), expected)
          checked += 1
    assert(checked > 1000)

  test("the exact centroid tie uses the smallest scientific id even with permuted face order"):
    val id = SurfaceNearestFixture.Surface
    val geometry = SurfaceGeometry(TriangleMesh.fromRows(
      Seq(Seq(-1.0, -1.0, 0.0), Seq(1.0, -1.0, 0.0), Seq(0.0, 2.0, 0.0)), Seq((2, 0, 1))),
      Hemisphere.Left, SurfaceKind.Inflated)
    val layer = SurfaceLayer.packedRgba(SurfaceNearestFixture.Layer, id, geometry,
      SurfaceNearestFixture.Palette.take(3), interpolation = SurfaceVertexInterpolation.NearestSample).toOption.get
    val model = SurfaceViewerModel.make(Vector(SurfaceAsset.make(id, geometry).toOption.get), Vector(layer)).toOption.get
    val original = SurfaceCompiler.compile(model, SurfaceNearestFixture.state(model)).toOption.get
    // Put the scientific centroid exactly on the center pixel, independent of camera fitting.
    val identity = new FloatBufferView(Array[Float](1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))
    val plan = original.copy(camera = SurfaceCameraPacket(identity, identity, 0.0, 0.0, 1.0))
    val result = SurfaceRasterizer.render(plan, RasterDimensions.unsafe(129, 129),
      SurfaceRasterStyle(culling = TriangleCulling.None)).toOption.get
    assertEquals(result.pick(64, 64).toOption.flatten.get.vertex, 0)
    assertEquals(result.image.pixelUnsafe(64, 64), SurfaceNearestFixture.Palette(0))
