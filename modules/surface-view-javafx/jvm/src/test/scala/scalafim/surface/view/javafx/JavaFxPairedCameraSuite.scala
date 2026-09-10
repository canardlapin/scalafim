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
  private val model = SurfaceViewerModel.make(Vector(asset(left, Hemisphere.Left, -1), asset(right, Hemisphere.Right, 1)), Vector.empty).toOption.get
  private def state(medial: Boolean = false): SurfaceViewerState =
    val initial = SurfaceViewerState.initial(model)
    initial.copy(layout = SurfaceLayout.Bilateral(left, right), lighting = SurfaceLighting.Unlit,
      surfaceViewpoints = if medial then Map(left -> SurfaceViewpoint.Medial(CorticalHemisphere.Left), right -> SurfaceViewpoint.Medial(CorticalHemisphere.Right))
        else Map(left -> SurfaceViewpoint.Lateral(CorticalHemisphere.Left), right -> SurfaceViewpoint.Lateral(CorticalHemisphere.Right)))
  private def plan(value: SurfaceViewerState): SurfaceRenderPlan = SurfaceCompiler.compile(model, value).toOption.get

  test("native display transforms project each anatomical camera into its fitted slot"):
    for
      medial <- Vector(false, true)
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default), CameraProjection.Orthographic(OrthographicScale.unsafe(5)))
      orbit <- Vector(SurfaceOrbit.Zero, SurfaceOrbit.unsafe(23, -14))
      (width, height) <- Vector(901.0 -> 301.0, 301.0 -> 901.0, 1802.0 -> 602.0)
    do
      val initial = state(medial)
      val compiled = plan(initial.copy(camera = initial.camera.copy(projection = projection, orbit = orbit)))
      val global = new Affine()
      val camera = new PerspectiveCamera(true)
      JavaFxSurfaceProbe.configureCamera(camera, global, compiled)
      val transforms = Map(left -> new Affine(), right -> new Affine())
      JavaFxSurfaceProbe.configureLayout(compiled, transforms, width, height)
      compiled.viewportFit.resolve(compiled.slots, width, height).foreach: slot =>
        val mesh = compiled.meshes.find(_.surface == slot.surface).get
        val packet = compiled.cameraFor(slot.surface)
        val v = packet.viewMatrix
        val p = packet.projectionMatrix
        for index <- 0 until mesh.positions.length by 3 do
          val x = mesh.positions(index).toDouble
          val y = mesh.positions(index + 1).toDouble
          val z = mesh.positions(index + 2).toDouble
          val native = global.transform(transforms(slot.surface).transform(x, y, z))
          val wx = x + slot.worldOffsetX
          val wy = y + slot.worldOffsetY
          val wz = z + slot.worldOffsetZ
          val eye = Vector.tabulate(4)(row => v(row * 4) * wx + v(row * 4 + 1) * wy + v(row * 4 + 2) * wz + v(row * 4 + 3))
          val clip = Vector.tabulate(4)(row => (0 until 4).map(k => p(row * 4 + k) * eye(k)).sum)
          val expectedX = width * (slot.viewport.x + slot.viewport.width * (1 + clip(0) / clip(3)) * 0.5)
          val expectedY = height * (slot.viewport.y + slot.viewport.height * (1 - clip(1) / clip(3)) * 0.5)
          val actualX = if p(15) == 1 then native.getX else width * 0.5 + height * 0.5 * p(5) * native.getX / native.getZ
          val actualY = if p(15) == 1 then native.getY else height * 0.5 + height * 0.5 * p(5) * native.getY / native.getZ
          assertEqualsDouble(actualX, expectedX, 0.001)
          assertEqualsDouble(actualY, expectedY, 0.001)

  test("paired camera changes reuse mesh and material resources; repeated plans are clean"):
    val before = plan(state())
    val after = plan(state(true))
    val changed = JavaFxSurfaceProgram.compile(Some(before), after)
    assert(changed.dirty.camera)
    assert(!changed.dirty.geometry && !changed.dirty.layerData && !changed.dirty.material)
    assert(JavaFxSurfaceProgram.compile(Some(after), plan(state(true))).dirty.isClean)

  test("invalid surface cameras reject before native creation"):
    val valid = plan(state())
    val invalid = valid.copy(surfaceCameras = Map(SurfaceId.unsafe("absent") -> valid.camera))
    assert(JavaFxSurfaceBackend.validateCapabilities(invalid).isLeft)
    assert(JavaFxSurfaceProbe.compile(invalid).isLeft)
