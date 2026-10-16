package scalafim.surface.view

import scalafim.surface.*

/** Frozen-output generator run only against main at fedfca31.
  *
  * It deliberately writes a compact, copyable receipt for the V1 camera
  * implementation. The candidate migration suite stores the recorded numbers;
  * this source is not part of production and is never compiled by consumers.
  */
object CameraFramingBaselineFixture:
  private def ok[A](value: Either[?, A]): A = value.toOption.get

  private def asset(id: String, hemisphere: Hemisphere, x: Double): SurfaceAsset =
    val mesh = TriangleMesh.fromRows(
      Vector(
        Vector(x - 3.0, -2.0, -1.0), Vector(x + 5.0, -1.0, 2.0),
        Vector(x - 1.0, 4.0, 7.0), Vector(x + 2.0, 3.0, -4.0)
      ),
      Vector((0, 1, 2), (0, 2, 3), (0, 3, 1), (1, 3, 2))
    )
    ok(SurfaceAsset.make(SurfaceId.unsafe(id), SurfaceGeometry(mesh, hemisphere, SurfaceKind.Pial)))

  private def values(plan: SurfaceRenderPlan): String =
    val view = plan.camera.viewMatrix.unsafeArray.map(_.toDouble).mkString(",")
    val projection = plan.camera.projectionMatrix.unsafeArray.map(_.toDouble).mkString(",")
    val slot = plan.slots.head
    val mesh = plan.meshes.head
    val world = Array(mesh.positions(0).toDouble + slot.worldOffsetX, mesh.positions(1).toDouble + slot.worldOffsetY,
      mesh.positions(2).toDouble + slot.worldOffsetZ, 1.0)
    def product(matrix: Array[Float], vector: Array[Double], row: Int): Double =
      (0 until 4).map(column => matrix(row * 4 + column).toDouble * vector(column)).sum
    val v = Array.tabulate(4)(product(plan.camera.viewMatrix.unsafeArray, world, _))
    val c = Array.tabulate(4)(product(plan.camera.projectionMatrix.unsafeArray, v, _))
    s"Fill|$view|$projection|${c(0) / c(3)},${c(1) / c(3)},${c(2) / c(3)}"

  private def record(name: String, model: SurfaceViewerModel, state: SurfaceViewerState): Unit =
    println(s"$name=${values(ok(SurfaceCompiler.compile(model, state)))}")

  private def recordDocument(name: String, model: SurfaceViewerModel, state: SurfaceViewerState): Unit =
    val bindings = SurfaceSceneBindings(model.surfaces.map: surface =>
      surface.id -> SurfaceExternalReference(SurfaceAssetUri.unsafe(s"asset:///${surface.id.value}"), SurfaceContentDigest.unsafe("a" * 64))
    .toMap, Map.empty)
    val provenance = ok(SurfaceProvenance.make("camera-fixture", "1", "2026-10-14T00:00:00Z"))
    println(s"$name=${SurfaceSceneCodec.encode(ok(SurfaceSceneDocument.capture(model, state, bindings, provenance)))}")

  def main(args: Array[String]): Unit =
    val left = asset("left", Hemisphere.Left, -9.0)
    val right = asset("right", Hemisphere.Right, 11.0)
    val model = ok(SurfaceViewerModel.make(Vector(left, right), Vector.empty))
    val initial = SurfaceViewerState.initial(model).copy(camera = SurfaceCamera.unsafe(
      SurfaceViewpoint.Anterior,
      CameraProjection.Perspective(FieldOfViewDegrees.unsafe(35.0)),
      CameraZoom.unsafe(1.25)
    ))
    val bilateral = ok(SurfaceViewer.reduce(model, initial,
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(left.id, right.id))))
    record("single-square", model, initial)
    val nearFar = ok(SurfaceViewer.reduce(model, initial, SurfaceViewerAction.SetClipping(SurfaceClipping.NearFar(1.0, 2.0))))
    record("single-near-far-1-2", model, nearFar)
    recordDocument("single-near-far-v1-json", model, nearFar)
    record("bilateral-square", model, bilateral)
    recordDocument("single-v1-json", model, initial)
    recordDocument("bilateral-v1-json", model, bilateral)
    val orbit = ok(SurfaceViewer.reduce(model, bilateral, SurfaceViewerAction.OrbitBy(17.0, -11.0)))
    record("bilateral-orbit", model, orbit)
    val reset = ok(SurfaceViewer.reduce(model, orbit, SurfaceViewerAction.ResetCamera))
    record("bilateral-reset", model, reset)
