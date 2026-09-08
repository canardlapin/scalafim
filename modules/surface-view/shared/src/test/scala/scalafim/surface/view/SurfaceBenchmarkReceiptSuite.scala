package scalafim.surface.view

class SurfaceBenchmarkReceiptSuite extends munit.FunSuite:
  test("pinned matrix spans both mesh scales, every layer count, and every path"):
    assertEquals(SurfaceBenchmarkMatrix.Cases.length, 48)
    assertEquals(SurfaceBenchmarkMatrix.Cases.map(_.vertices).toSet, Set(32768, 163842))
    assertEquals(SurfaceBenchmarkMatrix.Cases.map(_.layers).toSet, Set(1, 4, 8))
    assertEquals(SurfaceBenchmarkMatrix.Cases.map(_.path).toSet, SurfaceAdmissionPath.values.toSet - SurfaceAdmissionPath.DerivedGeometryUpdate)

  test("timing summaries use deterministic nearest-rank percentiles"):
    val summary = SurfaceTimingSummary.from(SurfaceRenderPhase.Pick, Vector(9L, 1L, 5L, 3L, 7L)).toOption.get
    assertEquals(summary.minimumNanos, 1L)
    assertEquals(summary.p50Nanos, 5L)
    assertEquals(summary.p95Nanos, 9L)
    assertEquals(summary.maximumNanos, 9L)
    assert(SurfaceTimingSummary.from(SurfaceRenderPhase.Pick, Vector.empty).isLeft)

  test("benchmark JSON is deterministic, versioned, escaped, and includes structural counters"):
    val capabilities = SurfaceBackendCapabilities(
      SurfaceBackendId.unsafe("test-backend"),
      SurfacePlanRevision.Current,
      Set(SurfaceBackendFeature.DepthBuffer),
      Vector("device \"unknown\"")
    )
    val observation = SurfaceBackendObservation(
      capabilities,
      SurfacePlanRevision.Current,
      SurfaceAdmissionPath.CameraOnly,
      Vector(SurfaceResourceEvent.CacheHit(SurfaceResourceKey("mesh")), SurfaceResourceEvent.DrawSubmitted(1, 4)),
      Vector(SurfacePhaseTiming.unsafe(SurfaceRenderPhase.RenderSubmission, 20L))
    )
    val receipt = SurfaceBenchmarkReceipt(
      SurfaceBenchmarkCase.unsafe(32768, 1, SurfaceAdmissionPath.CameraOnly),
      SurfaceRuntimeMetadata(SurfaceRuntimePlatform.Jvm, "OpenJDK", "25", "macOS", "aarch64", "JavaFX", "unknown"),
      Vector(observation),
      Vector(SurfaceTimingSummary.from(SurfaceRenderPhase.RenderSubmission, Vector(10L, 20L)).toOption.get)
    )
    val first = SurfaceBenchmarkJson.encode(receipt)
    assertEquals(first, SurfaceBenchmarkJson.encode(receipt))
    assert(first.contains("\"schema\":\"scalafim.surface-benchmark.v1\""))
    assert(first.contains("\"geometryUploads\":0"))
    assert(first.contains("device \\\"unknown\\\""))
