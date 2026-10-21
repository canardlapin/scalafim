package scalafim.surface.view.javafx

import javafx.scene.PerspectiveCamera
import javafx.scene.transform.Affine
import scalafim.surface.*
import scalafim.surface.view.*

class JavaFxPairedCameraSuite extends munit.FunSuite:
  private val left = SurfaceId.unsafe("left")
  private val right = SurfaceId.unsafe("right")

  private def asset(id: SurfaceId, hemisphere: Hemisphere, sign: Double): SurfaceAsset =
    val positions = Vector(3.0, 1.0).flatMap(x =>
      Vector(Vector(sign * x, -2.0, -1.0), Vector(sign * x, 3.0, -1.0), Vector(sign * x, -1.0, 4.0)))
    val faces = if sign < 0 then Vector((0, 2, 1), (3, 4, 5)) else Vector((0, 1, 2), (3, 5, 4))
    SurfaceAsset.make(id, SurfaceGeometry(TriangleMesh.fromRows(positions, faces), hemisphere, SurfaceKind.Inflated)).toOption.get

  private val model = SurfaceViewerModel.make(Vector(asset(left, Hemisphere.Left, -1),
    asset(right, Hemisphere.Right, 1)), Vector.empty).toOption.get

  private def plan(medial: Boolean, projection: CameraProjection, orbit: SurfaceOrbit = SurfaceOrbit.Zero): SurfaceRenderPlan =
    val initial = SurfaceViewerState.initial(model)
    val viewpoints = if medial then Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left),
      right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right)) else Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left),
      right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right))
    val state = initial.copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = viewpoints, camera = initial.camera.copy(projection = projection, orbit = orbit))
    SurfaceCompiler.compile(model, state).toOption.get

  test("native transforms project each effective camera into its slot"):
    for
      medial <- Vector(false, true)
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default), CameraProjection.Orthographic(OrthographicScale.unsafe(5)))
      (width, height) <- Vector(901.0 -> 301.0, 301.0 -> 901.0)
    do
      val compiled = plan(medial, projection, SurfaceOrbit.unsafe(23, -14))
      val global = new Affine()
      JavaFxSurfaceProbe.configureCamera(new PerspectiveCamera(true), global, compiled)
      val transforms = Map(left -> new Affine(), right -> new Affine())
      JavaFxSurfaceProbe.configureLayout(compiled, transforms, width, height)
      compiled.viewportFit.resolve(compiled.slots, width, height).foreach: slot =>
        val mesh = compiled.meshes.find(_.surface == slot.surface).get
        val packet = compiled.cameraFor(slot.surface)
        for index <- 0 until mesh.positions.length by 3 do
          val x = mesh.positions(index).toDouble
          val y = mesh.positions(index + 1).toDouble
          val z = mesh.positions(index + 2).toDouble
          val native = global.transform(transforms(slot.surface).transform(x, y, z))
          val wx = x + slot.worldOffsetX
          val wy = y + slot.worldOffsetY
          val wz = z + slot.worldOffsetZ
          val eye = Vector.tabulate(4)(row => packet.viewMatrix(row * 4) * wx + packet.viewMatrix(row * 4 + 1) * wy + packet.viewMatrix(row * 4 + 2) * wz + packet.viewMatrix(row * 4 + 3))
          val clip = Vector.tabulate(4)(row => (0 until 4).map(k => packet.projectionMatrix(row * 4 + k) * eye(k)).sum)
          val expectedX = width * (slot.viewport.x + slot.viewport.width * (1 + clip(0) / clip(3)) * 0.5)
          val expectedY = height * (slot.viewport.y + slot.viewport.height * (1 - clip(1) / clip(3)) * 0.5)
          val actualX = if packet.projectionMatrix(15) == 1 then native.getX else width * 0.5 + height * 0.5 * packet.projectionMatrix(5) * native.getX / native.getZ
          val actualY = if packet.projectionMatrix(15) == 1 then native.getY else height * 0.5 + height * 0.5 * packet.projectionMatrix(5) * native.getY / native.getZ
          assertEqualsDouble(actualX, expectedX, 0.001)
          assertEqualsDouble(actualY, expectedY, 0.001)

  test("camera matrix changes dirty native camera without resource uploads and invalid packets reject"):
    val before = plan(medial = false, CameraProjection.Perspective(FieldOfViewDegrees.Default))
    val after = plan(medial = true, CameraProjection.Perspective(FieldOfViewDegrees.Default))
    val changed = JavaFxSurfaceProgram.compile(Some(before), after)
    assert(changed.dirty.camera)
    assert(!changed.dirty.geometry && !changed.dirty.layerData && !changed.dirty.material)
    assert(JavaFxSurfaceProgram.compile(Some(after), plan(medial = true, CameraProjection.Perspective(FieldOfViewDegrees.Default))).dirty.isClean)
    val sameKey = after.copy(receipt = before.receipt)
    assert(JavaFxSurfaceProgram.compile(Some(before), sameKey).dirty.camera)
    val invalid = after.copy(surfaceCameras = Map(SurfaceId.unsafe("absent") -> after.camera))
    assert(JavaFxSurfaceBackend.validateCapabilities(invalid).isLeft)
    assert(JavaFxSurfaceProbe.compile(invalid).isLeft)
    val nonRigid = after.camera.viewMatrix.unsafeArray.clone()
    nonRigid(0) = 2.0f
    assert(JavaFxSurfaceBackend.validateCapabilities(after.copy(camera = after.camera.copy(
      viewMatrix = new FloatBufferView(nonRigid)))).isLeft)
    val mismatchedProjection = after.camera.projectionMatrix.unsafeArray.clone()
    mismatchedProjection(0) = mismatchedProjection(0) * 0.5f
    assert(JavaFxSurfaceBackend.validateCapabilities(after.copy(surfaceCameras = Map(left -> after.camera.copy(
      projectionMatrix = new FloatBufferView(mismatchedProjection))))).isLeft)

  test("public camera updates refuse malformed packets before native transforms change"):
    val before = plan(medial = false, CameraProjection.Perspective(FieldOfViewDegrees.Default))
    val compiled = JavaFxSurfaceProbe.compile(before).toOption.get
    def transforms = (Vector(compiled.root) ++ compiled.surfaceGroups.toVector.sortBy(_._1.value).map(_._2))
      .flatMap: node =>
        val t = node.getLocalToParentTransform
        Vector(t.getMxx,t.getMxy,t.getMxz,t.getTx,t.getMyx,t.getMyy,t.getMyz,t.getTy,t.getMzx,t.getMzy,t.getMzz,t.getTz)
    val original = transforms
    def unchanged(): Unit = transforms.zip(original).foreach((actual,expected) => assertEqualsDouble(actual,expected,0))
    val malformed = before.copy(surfaceCameras = Map(left -> before.camera.copy(viewMatrix = new FloatBufferView(Array(1f)))))
    val nonRigid = before.camera.viewMatrix.unsafeArray.clone()
    nonRigid(0) = 2f
    val scaled = before.copy(surfaceCameras = Map(left -> before.camera.copy(viewMatrix = new FloatBufferView(nonRigid))))
    for invalid <- Vector(malformed,scaled,before.copy(surfaceCameras=Map(SurfaceId.unsafe("missing") -> before.camera))) do
      assert(compiled.applyCamera(invalid).isLeft)
      unchanged()
    assert(compiled.applyCamera(before).isRight)
    unchanged()

/** Actual JavaFX ray traversal for asymmetric bilateral meshes. This checks the
  * native pick result is decoded through the same current per-surface camera
  * plan used for rendering, rather than only checking affine equations. */
object JavaFxPairedNativePickProbe:
  import java.nio.file.{Files, Path}
  import java.util.concurrent.{CountDownLatch, TimeUnit}
  import _root_.javafx.application.Platform
  import _root_.javafx.geometry.Point3D
  import _root_.javafx.scene.{Group, Scene, SceneAntialiasing}
  import _root_.javafx.scene.image.WritableImage
  import _root_.javafx.stage.Stage
  import intaglio.*

  private val left = SurfaceId.unsafe("pick-left")
  private val right = SurfaceId.unsafe("pick-right")

  def main(args: Array[String]): Unit =
    val output = Path.of(args.headOption.getOrElse("/private/tmp/scalafim-javafx-paired-native-picks.json"))
    val done = new CountDownLatch(1)
    @volatile var failure: Throwable | Null = null
    def record(error: Throwable): Unit =
      if failure == null then failure = error
      else if failure.nn ne error then failure.nn.addSuppressed(error)
    def cleanup(action: => Unit): Unit =
      try action
      catch case error: Throwable => record(error)
    try
      Platform.startup(() => ())
      Platform.runLater: () =>
        var stage: Option[Stage] = None
        var backend: Option[JavaFxSurfaceBackend] = None
        var controller: Option[JavaFxSurfaceController] = None
        try
          Platform.setImplicitExit(false)
          val mounted = new Stage()
          stage = Some(mounted)
          val (model, state) = fixture()
          val plan = SurfaceCompiler.compile(model, state).toOption.get
          val native = JavaFxSurfaceBackend.create().toOption.get
          backend = Some(native)
          native.render(plan).toOption.get
          val width = 320
          val height = 180
          val scene = native.newSubScene(JavaFxSnapshotConfig.make(width, height, SceneAntialiasing.DISABLED).toOption.get).toOption.get
          mounted.setScene(new Scene(new Group(scene), width, height))
          mounted.show()
          mounted.getScene.getRoot.applyCss()
          mounted.getScene.getRoot.layout()
          scene.snapshot(null, new WritableImage(width, height))
          val active = JavaFxSurfaceController.attach(model, state, native, scene).toOption.get
          controller = Some(active)
          var hits = 0
          val origin = scene.localToScreen(0.0, 0.0)
          require(origin != null, "outer paired SubScene has no screen origin")
          val interiors = Map(left -> new Point3D(-3.0, -0.3, 0.33), right -> new Point3D(3.0, -0.3, 0.33))
          interiors.foreach: (surface, interior) =>
            val chunk = native.chunks.find(_.surface == surface).get
            val projected = chunk.view.localToScreen(interior)
            require(projected != null, s"$surface did not project through its paired camera")
            val picked = JavaFxNativePick.at(scene, projected.getX - origin.getX, projected.getY - origin.getY)
              .getOrElse(throw new AssertionError(s"native miss for $surface at $projected"))
            val actual = active.pick(picked).fold(error => throw new AssertionError(error.message), identity)
            require(actual.surface == surface && actual.face.index == 0,
              s"native paired identity mismatch for $surface: $actual")
            hits += 1
          require(hits == 2, s"native paired pick count changed: $hits")
          Files.writeString(output, s"{\"passed\":true,\"hits\":$hits,\"surfaces\":[\"${left.value}\",\"${right.value}\"]}\n")
          println(s"PASS paired-native-picks hits=$hits receipt=$output")
        catch case error: Throwable => record(error)
        finally
          try cleanup(controller.foreach(_.dispose()))
          finally
            try cleanup(backend.foreach(value => value.dispose().fold(error => throw IllegalStateException(error.message), identity)))
            finally
              try cleanup(stage.foreach(_.close()))
              finally done.countDown()
      require(done.await(90, TimeUnit.SECONDS), "paired native pick probe timed out")
      if failure != null then throw failure.nn
    finally Platform.exit()

  private def fixture(): (SurfaceViewerModel, SurfaceViewerState) =
    def asset(id: SurfaceId, hemisphere: Hemisphere, x: Double): SurfaceAsset =
      val mesh = TriangleMesh.fromRows(Vector(
        Vector(x, -3.0, -2.0), Vector(x, 2.5, -1.0), Vector(x, -0.5, 4.0)
      ), Vector((0, 1, 2)))
      SurfaceAsset.make(id, SurfaceGeometry(mesh, hemisphere, SurfaceKind.Inflated)).toOption.get
    val leftAsset = asset(left, Hemisphere.Left, -3.0)
    val rightAsset = asset(right, Hemisphere.Right, 3.0)
    val layers = Vector(
      SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("pick-left-colors"), left, leftAsset.geometry,
        Vector(Rgba32.unsafe(230, 40, 30), Rgba32.unsafe(40, 220, 70), Rgba32.unsafe(40, 80, 230))).toOption.get,
      SurfaceLayer.packedRgba(SurfaceLayerId.unsafe("pick-right-colors"), right, rightAsset.geometry,
        Vector(Rgba32.unsafe(40, 220, 70), Rgba32.unsafe(230, 40, 30), Rgba32.unsafe(240, 190, 30))).toOption.get
    )
    val model = SurfaceViewerModel.make(Vector(leftAsset, rightAsset), layers).toOption.get
    val initial = SurfaceViewerState.initial(model).copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left),
        right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right)))
    (model, initial)
