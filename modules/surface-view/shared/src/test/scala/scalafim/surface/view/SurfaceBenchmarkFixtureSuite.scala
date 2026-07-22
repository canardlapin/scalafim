package scalafim.surface.view

class SurfaceBenchmarkFixtureSuite extends munit.FunSuite:
  test("pinned fixtures have exact vertex and layer counts"):
    SurfaceBenchmarkMatrix.VertexCounts.foreach: vertices =>
      SurfaceBenchmarkMatrix.LayerCounts.foreach: layers =>
        val plan = SurfaceBenchmarkFixture.plan(vertices, layers)
        assertEquals(plan.profile.verticesPacked, vertices)
        assertEquals(plan.layers.length, layers)
        assert(plan.profile.facesPacked > 0)
        assertEquals(plan.meshes.head.positions.length, vertices * 3)
        assertEquals(plan.layers.head.colors.length, vertices)

  test("admission-path fixtures encode only their expected dirty resource"):
    val base = SurfaceBenchmarkFixture.plan(32768, 4)
    val camera = SurfaceBenchmarkFixture.planFor(SurfaceBenchmarkCase.unsafe(32768, 4, SurfaceAdmissionPath.CameraOnly))
    val style = SurfaceBenchmarkFixture.planFor(SurfaceBenchmarkCase.unsafe(32768, 4, SurfaceAdmissionPath.StyleUpdate))
    val data = SurfaceBenchmarkFixture.planFor(SurfaceBenchmarkCase.unsafe(32768, 4, SurfaceAdmissionPath.LayerDataUpdate))
    val timepoint = SurfaceBenchmarkFixture.planFor(SurfaceBenchmarkCase.unsafe(32768, 4, SurfaceAdmissionPath.TimepointUpdate))
    assertEquals(camera.receipt.meshKeys, base.receipt.meshKeys)
    assertEquals(camera.receipt.layerKeys, base.receipt.layerKeys)
    assertNotEquals(camera.receipt.cameraKey, base.receipt.cameraKey)
    assertEquals(style.receipt.meshKeys, base.receipt.meshKeys)
    assertEquals(style.receipt.layerKeys, base.receipt.layerKeys)
    assertNotEquals(style.layers.head.opacity, base.layers.head.opacity)
    assertEquals(data.receipt.meshKeys, base.receipt.meshKeys)
    assertNotEquals(data.receipt.layerKeys, base.receipt.layerKeys)
    assertEquals(timepoint.receipt.meshKeys, base.receipt.meshKeys)
    assertNotEquals(timepoint.receipt.layerKeys, base.receipt.layerKeys)
    assertEquals(timepoint.receipt.timepoint, 1)
