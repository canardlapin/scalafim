package scalafim.surface.view

class SurfaceClipRangeSuite extends munit.FunSuite:
  test("explicit depth limits map eye-space endpoints to clipping planes in both projections"):
    val model = SurfaceScalarFixture.model()
    val initial = SurfaceFaceFixture.state(model)
    for projection <- Vector(initial.camera.projection, CameraProjection.Perspective(FieldOfViewDegrees.unsafe(50))) do
      val state = SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetProjection(projection)).toOption.get
      val before = SurfaceCompiler.compile(model, state).toOption.get
      val changed = SurfaceViewer.reduce(model, state,
        SurfaceViewerAction.SetClipping(SurfaceClipping.nearFar(2, 6).toOption.get)).toOption.get
      val after = SurfaceCompiler.compile(model, changed).toOption.get
      val p = after.camera.projectionMatrix
      def depth(distance: Double): Double = (-distance * p(10) + p(11)) / (-distance * p(14) + p(15))
      assertEqualsDouble(depth(2), -1, 1e-6)
      assertEqualsDouble(depth(6), 1, 1e-6)
      assert(depth(1) < -1 && depth(7) > 1)
      for index <- Vector(0, 5, 14, 15) do
        assertEqualsDouble(p(index).toDouble, before.camera.projectionMatrix(index).toDouble, 0)
      assertEquals(after.receipt.meshKeys, before.receipt.meshKeys)
      assertEquals(after.receipt.layerKeys, before.receipt.layerKeys)
      assertNotEquals(after.receipt.cameraKey, before.receipt.cameraKey)
      val restored = SurfaceCompiler.compile(model, changed.copy(clipping = SurfaceClipping.Disabled)).toOption.get
      assertEquals(restored.receipt.cameraKey, before.receipt.cameraKey)

  test("direct and checked clip ranges reject invalid endpoints"):
    for (near, far) <- Vector((0.0, 2.0), (2.0, 2.0), (3.0, 2.0), (1.0, Double.PositiveInfinity), (Double.NaN, 2.0)) do
      assert(SurfaceClipping.nearFar(near, far).isLeft)
      intercept[IllegalArgumentException](SurfaceClipping.NearFar(near, far))
