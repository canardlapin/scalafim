package scalafim.surface.view

import scalafim.surface.*

/** V1 receipts were generated independently from main at fedfca31 by
  * CameraFramingBaselineFixture. They protect restore compatibility rather
  * than treating the current bounds-fitting implementation as its oracle.
  */
class CameraFramingMigrationSuite extends munit.FunSuite:
  private final case class Expected(view: Vector[Double], landmark: Vector[Double])

  private val squareProjection = Vector(
    3.1715948581695557, 0.0, 0.0, 0.0, 0.0, 3.1715948581695557, 0.0, 0.0,
    0.0, 0.0, -1.0000200271606445, -0.02000020071864128, 0.0, 0.0, -1.0, 0.0
  )
  private val single = Expected(
    Vector(-1,0,0,-8, 0,0,1,-1.5, 0,1,0,-12.892854690551758, 0,0,0,1),
    Vector(0.8518433635645855,-0.5324021022278659,0.9986770878025287)
  )
  private val bilateral = Expected(
    Vector(-1,0,0,2, 0,0,1,-1.5, 0,1,0,-25.540578842163086, 0,0,0,1),
    Vector(0.46064316604907685,-0.28790197878067303,0.9992938187235139)
  )
  private val orbit = Expected(
    Vector(-0.9563047289848328,-0.2923716902732849,0,2.2049813270568848,
      0.05578714981675148,-0.18247154355049133,0.9816271662712097,-1.4015434980392456,
      -0.28700000047683716,0.938734769821167,0.1908089965581894,-25.191526412963867,0,0,0,1),
    Vector(0.5588701142749944,-0.253125910746341,0.9992705575172346)
  )
  private val reset = Expected(
    Vector(-1,0,0,2, 0,0,1,-1.5, 0,1,0,-31.675724029541016, 0,0,0,1),
    Vector(0.37672180178069753,-0.23545112611293595,0.9994261215715294)
  )
  // Emitted by main@fedfca31 CameraFramingBaselineFixture, not manufactured by
  // the V7 encoder under test.
  private val singleV1 = """{"schema":"scalafim.surface-scene","revision":1,"assets":[{"id":"left","uri":"asset:///left","sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","hemisphere":"left","kind":"pial","vertexCount":4,"faceCount":4,"topologyIdentity":"bc77d0d0b9bcda3b"},{"id":"right","uri":"asset:///right","sha256":"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa","hemisphere":"right","kind":"pial","vertexCount":4,"faceCount":4,"topologyIdentity":"bc77d0d0b9bcda3b"}],"layers":[],"layout":{"kind":"single","surface":"left"},"camera":{"viewpoint":"anterior","projection":{"kind":"perspective","value":35.0},"zoom":1.25,"pan":[0.0,0.0],"orbit":[0.0,0.0]},"lighting":{"kind":"directional","ambient":0.35,"diffuse":0.65,"direction":[-0.4,-0.5,0.7681145747868608]},"clipping":{"kind":"disabled"},"timepoint":0,"selection":null,"layerStates":[],"requiredFeatures":[],"provenance":{"producer":"camera-fixture","softwareVersion":"1","createdAt":"2026-10-14T00:00:00Z","entries":{}}}"""
  private val bilateralV1 = singleV1.replace("\"layout\":{\"kind\":\"single\",\"surface\":\"left\"}",
    "\"layout\":{\"kind\":\"bilateral\",\"left\":\"left\",\"right\":\"right\",\"order\":\"left-then-right\"}")

  private def ok[A](result: Either[?, A]): A = result.toOption.get

  private def asset(id: String, hemisphere: Hemisphere, x: Double): SurfaceAsset =
    val mesh = TriangleMesh.fromRows(
      Vector(
        Vector(x - 3.0, -2.0, -1.0), Vector(x + 5.0, -1.0, 2.0),
        Vector(x - 1.0, 4.0, 7.0), Vector(x + 2.0, 3.0, -4.0)
      ),
      Vector((0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2))
    )
    ok(SurfaceAsset.make(SurfaceId.unsafe(id), SurfaceGeometry(mesh, hemisphere, SurfaceKind.Pial)))

  private def fixture(): (SurfaceViewerModel, SurfaceViewerState, SurfaceId, SurfaceId) =
    val left = asset("left", Hemisphere.Left, -9.0)
    val right = asset("right", Hemisphere.Right, 11.0)
    val model = ok(SurfaceViewerModel.make(Vector(left, right), Vector.empty))
    val camera = SurfaceCamera.unsafe(SurfaceViewpoint.Anterior,
      CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35.0)), CameraZoom.unsafe(1.25))
    (model, SurfaceViewerState.initial(model).copy(camera = camera, framing = CameraFraming.LegacyV1), left.id, right.id)

  private def bindings(model: SurfaceViewerModel): SurfaceSceneBindings = SurfaceSceneBindings(model.surfaces.map: surface =>
    surface.id -> SurfaceExternalReference(SurfaceAssetUri.unsafe(s"asset:///${surface.id.value}"), SurfaceContentDigest.unsafe("a" * 64))
  .toMap, Map.empty)

  private def landmark(plan: SurfaceRenderPlan): Vector[Double] =
    val slot = plan.slots.head
    val mesh = plan.meshes.head
    val world = Array(mesh.positions(0).toDouble + slot.worldOffsetX, mesh.positions(1).toDouble + slot.worldOffsetY,
      mesh.positions(2).toDouble + slot.worldOffsetZ, 1.0)
    def product(matrix: FloatBufferView, vector: Array[Double], row: Int): Double =
      (0 until 4).map(column => matrix(row * 4 + column).toDouble * vector(column)).sum
    val view = Array.tabulate(4)(product(plan.camera.viewMatrix, world, _))
    val clip = Array.tabulate(4)(product(plan.camera.projectionMatrix, view, _))
    Vector(clip(0) / clip(3), clip(1) / clip(3), clip(2) / clip(3))

  private def assertLegacy(plan: SurfaceRenderPlan, expected: Expected): Unit =
    assertEquals(plan.viewportFit, SurfaceViewportFit.Fill)
    Vector.tabulate(16)(plan.camera.viewMatrix.apply(_).toDouble).zip(expected.view).foreach { case (actual, wanted) =>
      assertEqualsDouble(actual, wanted, 1e-6)
    }
    Vector.tabulate(16)(plan.camera.projectionMatrix.apply(_).toDouble).zip(squareProjection).foreach { case (actual, wanted) =>
      assertEqualsDouble(actual, wanted, 1e-7)
    }
    landmark(plan).zip(expected.landmark).foreach { case (actual, wanted) => assertEqualsDouble(actual, wanted, 1e-6) }

  test("baseline V1 documents decode, restore, and reproduce independently recorded single and bilateral matrices"):
    val (model, initial, left, right) = fixture()
    val singleState = ok(SurfaceSceneCodec.decode(singleV1)).restore(model, bindings(model)).toOption.get
    assertEquals(singleState.framing, CameraFraming.LegacyV1)
    assertLegacy(ok(SurfaceCompiler.compile(model, singleState)), single)
    val bilateralState = ok(SurfaceSceneCodec.decode(bilateralV1)).restore(model, bindings(model)).toOption.get
    assertEquals(bilateralState.framing, CameraFraming.LegacyV1)
    assertLegacy(ok(SurfaceCompiler.compile(model, bilateralState)), bilateral)
    val wide = ok(SurfaceViewer.reduce(model, bilateralState, SurfaceViewerAction.SetAspectRatio(CameraAspectRatio.unsafe(2.0))))
    assertLegacy(ok(SurfaceCompiler.compile(model, wide)), bilateral)

  test("legacy framing survives orbit and reset, and framing participates in the camera receipt"):
    val (model, _, _, _) = fixture()
    val bilateralState = ok(SurfaceSceneCodec.decode(bilateralV1)).restore(model, bindings(model)).toOption.get
    val moved = ok(SurfaceViewer.reduce(model, bilateralState, SurfaceViewerAction.OrbitBy(17.0, -11.0)))
    assertLegacy(ok(SurfaceCompiler.compile(model, moved)), orbit)
    val restored = ok(SurfaceViewer.reduce(model, moved, SurfaceViewerAction.ResetCamera))
    assertEquals(restored.framing, CameraFraming.LegacyV1)
    assertLegacy(ok(SurfaceCompiler.compile(model, restored)), reset)
    val framingOnly = ok(SurfaceViewer.reduce(model, restored, SurfaceViewerAction.SetCameraFraming(CameraFraming.BoundsFit)))
    assertEquals(framingOnly.camera, restored.camera)
    assert(ok(SurfaceCompiler.compile(model, restored)).receipt.cameraKey != ok(SurfaceCompiler.compile(model, framingOnly)).receipt.cameraKey)
    assertEquals(ok(SurfaceViewer.reduce(model, restored, SurfaceViewerAction.FitCamera)).framing, CameraFraming.BoundsFit)

  test("V1 saved near/far remains historically inactive until explicit framing migration"):
    val (model, _, _, _) = fixture()
    val clippedV1 = singleV1.replace("\"clipping\":{\"kind\":\"disabled\"}",
      "\"clipping\":{\"kind\":\"near-far\",\"near\":1.0,\"far\":2.0}")
    val restored = ok(ok(SurfaceSceneCodec.decode(clippedV1)).restore(model, bindings(model)))
    assertEquals(restored.clipping, SurfaceClipping.NearFar(1.0, 2.0))
    val legacy = ok(SurfaceCompiler.compile(model, restored))
    assertLegacy(legacy, single)
    assert(landmark(legacy)(2) < 1.0)
    val migrated = ok(SurfaceViewer.reduce(model, restored, SurfaceViewerAction.SetCameraFraming(CameraFraming.BoundsFit)))
    val corrected = ok(SurfaceCompiler.compile(model, migrated))
    assertEqualsDouble(corrected.camera.projectionMatrix(10).toDouble, -3.0, 1e-7)
    assertEqualsDouble(corrected.camera.projectionMatrix(11).toDouble, -4.0, 1e-7)
    assert(landmark(corrected)(2) > 1.0)

  test("revision policy decodes V1 as legacy, V2 through V6 as bounds-fit, and records V7 framing"):
    val (model, initial, _, _) = fixture()
    val referenceBindings = bindings(model)
    val provenance = ok(SurfaceProvenance.make("migration-test", "1", "2026-10-14T00:00:00Z"))
    val v7 = ok(SurfaceSceneDocument.capture(model, initial, referenceBindings, provenance))
    val encoded = SurfaceSceneCodec.encode(v7)
    assert(encoded.contains("\"framing\":\"legacy-v1\""))
    assertEquals(ok(SurfaceSceneCodec.decode(encoded)).framing, CameraFraming.LegacyV1)
    val old = singleV1
    assertEquals(ok(SurfaceSceneCodec.decode(old)).framing, CameraFraming.LegacyV1)
    for revision <- 2 to 6 do
      val historical = old.replace("\"revision\":1", s"\"revision\":$revision")
      val withV6Legends = if revision == 6 then historical.replace("\"provenance\":", "\"legends\":[],\"provenance\":") else historical
      assertEquals(ok(SurfaceSceneCodec.decode(withV6Legends)).framing, CameraFraming.BoundsFit)
