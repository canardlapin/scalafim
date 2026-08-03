package scalafim.surface.view

class SurfaceBackendConformanceSuite extends munit.FunSuite:
  private val backend = SurfaceBackendCapabilities(
    SurfaceBackendId.unsafe("recording"),
    SurfacePlanRevision.Current,
    Set(SurfaceBackendFeature.DepthBuffer, SurfaceBackendFeature.BilateralViewports)
  )
  private val meshKey = SurfaceResourceKey("mesh")
  private val layerKey = SurfaceResourceKey("layer")

  test("semantic matrix covers every conformance family"):
    assertEquals(
      SurfaceBackendAdmission.requiredSemanticCases.map(_.family).toSet,
      SurfaceConformanceFamily.values.toSet
    )

  test("cold-load observations expose stable structural counters"):
    val observation = SurfaceBackendObservation(
      backend,
      SurfacePlanRevision.Current,
      SurfaceAdmissionPath.ColdLoad,
      Vector(
        SurfaceResourceEvent.MeshUploaded(meshKey, 4, 4, 144L),
        SurfaceResourceEvent.LayerUploaded(layerKey, 4, 4, 1, 16L),
        SurfaceResourceEvent.DrawSubmitted(1, 4)
      ),
      Vector(SurfacePhaseTiming.unsafe(SurfaceRenderPhase.Compile, 12L))
    )
    assertEquals(observation.geometryUploads, 1)
    assertEquals(observation.layerUploads, 1)
    assertEquals(observation.uploadedBytes, 160L)
    assertEquals(observation.drawCalls, 1)
    assertEquals(SurfaceBackendAdmission.validate(observation), Vector.empty)

  test("camera-only and data-only admission paths reject geometry churn"):
    val upload = SurfaceResourceEvent.MeshUploaded(meshKey, 4, 4, 144L)
    val camera = SurfaceBackendObservation(
      backend,
      SurfacePlanRevision.Current,
      SurfaceAdmissionPath.CameraOnly,
      Vector(upload, SurfaceResourceEvent.DrawSubmitted(1, 4)),
      Vector.empty
    )
    val data = camera.copy(path = SurfaceAdmissionPath.LayerDataUpdate)
    assert(SurfaceBackendAdmission.validate(camera).exists(_.problem.contains("camera-only")))
    assert(SurfaceBackendAdmission.validate(data).exists(_.problem.contains("layer-data")))

  test("invalid resource and revision receipts fail admission explicitly"):
    val obsolete = SurfaceBackendObservation(
      backend.copy(acceptedRevision = SurfacePlanRevision.Current),
      SurfacePlanRevision.unsafe(2),
      SurfaceAdmissionPath.Resize,
      Vector(
        SurfaceResourceEvent.Resized(0, 100),
        SurfaceResourceEvent.Picked(SurfaceId.unsafe("left"), -1, -1)
      ),
      Vector.empty
    )
    val problems = SurfaceBackendAdmission.validate(obsolete).map(_.problem)
    assert(problems.exists(_.contains("expected")))
    assert(problems.exists(_.contains("revisions differ")))
    assert(problems.exists(_.contains("resize dimensions")))
    assert(problems.exists(_.contains("picked face")))
