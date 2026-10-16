package scalafim.surface.view

import scalafim.surface.*

class SurfaceFitCameraSuite extends munit.FunSuite:
  private def fixture(bilateral: Boolean, translation: Double = 0.0): (SurfaceViewerModel, SurfaceViewerState) =
    val ids = Vector(SurfaceId.unsafe("left"), SurfaceId.unsafe("right"))
    val assets = ids.zipWithIndex.take(if bilateral then 2 else 1).map: (id, index) =>
      val cx = (if index == 0 then -45.0 else 45.0) + translation
      val vertices = for x <- Vector(cx-30.0,cx+30.0); y <- Vector(-100.0+translation,60.0+translation); z <- Vector(-25.0+translation,95.0+translation) yield Seq(x,y,z)
      val geometry = SurfaceGeometry(TriangleMesh.fromRows(vertices,
        Seq((0,1,3),(0,3,2),(4,6,7),(4,7,5),(0,4,5),(0,5,1),(2,3,7),(2,7,6),(0,2,6),(0,6,4),(1,5,7),(1,7,3))),
        if index == 0 then Hemisphere.Left else Hemisphere.Right, SurfaceKind.Pial)
      SurfaceAsset.make(id, geometry).toOption.get
    val model = SurfaceViewerModel.make(assets, Vector.empty).toOption.get
    val initial = SurfaceViewerState.initial(model)
    val state = if bilateral then SurfaceViewer.reduce(model, initial,
      SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(ids(0),ids(1)))).toOption.get else initial
    (model, state)

  private def projected(plan: SurfaceRenderPlan): Vector[(Double, Double, Double)] =
    plan.slots.zip(plan.meshes).flatMap: (slot, mesh) =>
      (0 until mesh.positions.length by 3).toVector.map: index =>
        val world = Vector(mesh.positions(index).toDouble + slot.worldOffsetX,
          mesh.positions(index+1).toDouble + slot.worldOffsetY, mesh.positions(index+2).toDouble + slot.worldOffsetZ, 1.0)
        val view = (0 until 4).map(row => (0 until 4).map(col => plan.camera.viewMatrix(row*4+col).toDouble * world(col)).sum)
        val clip = (0 until 4).map(row => (0 until 4).map(col => plan.camera.projectionMatrix(row*4+col).toDouble * view(col)).sum)
        assert(clip(3) > 0.0)
        (clip(0)/clip(3),clip(1)/clip(3),clip(2)/clip(3))

  test("explicit fits contain canonical corners tightly across anatomical views, orbits and projection types"):
    for
      bilateral <- Vector(false,true)
      viewpoint <- Vector(SurfaceViewpoint.Lateral(CorticalHemisphere.Left), SurfaceViewpoint.Anterior, SurfaceViewpoint.Dorsal)
      orbit <- Vector(SurfaceOrbit.Zero,SurfaceOrbit.unsafe(17.0,-11.0))
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.unsafe(10.0)), CameraProjection.Perspective(FieldOfViewDegrees.Default),
        CameraProjection.Perspective(FieldOfViewDegrees.unsafe(90.0)), CameraProjection.Orthographic(OrthographicScale.unsafe(20.0)))
    do
      val (model, initial) = fixture(bilateral)
      val before = initial.copy(camera = SurfaceCamera.unsafe(viewpoint,projection,CameraZoom.unsafe(2.0),3.0,-2.0,orbit))
      val fitted = SurfaceViewer.reduce(model,before,SurfaceViewerAction.FitCamera).toOption.get
      assertEquals(fitted.copy(camera = before.camera),before)
      assertEquals(fitted.camera.viewpoint,viewpoint)
      assertEquals(fitted.camera.orbit,orbit)
      assertEqualsDouble(fitted.camera.panX,0.0,0.0)
      assertEqualsDouble(fitted.camera.panY,0.0,0.0)
      val points = projected(SurfaceCompiler.compile(model,fitted).toOption.get)
      points.foreach: (x,y,z) =>
        assert(math.abs(x) <= 0.90001 && math.abs(y) <= 0.90001 && math.abs(z) < 1.0,s"outside fitted frustum: $x $y $z")
      val coverage = points.map((x,y,_) => math.max(math.abs(x),math.abs(y))).max
      assert(coverage >= 0.89999,s"fit wastes the reference viewport: $coverage")
      val again = SurfaceViewer.reduce(model,fitted,SurfaceViewerAction.FitCamera).toOption.get
      assertEqualsDouble(again.camera.zoom.value,fitted.camera.zoom.value,1e-10)
      assertEqualsDouble(again.camera.aspectRatio.value,fitted.camera.aspectRatio.value,1e-10)

  test("fit aspect follows projected anatomy and persists through pan, reset and orbit"):
    val (model, initial) = fixture(false)
    val fitted = SurfaceViewer.reduce(model,initial,SurfaceViewerAction.FitCamera).toOption.get
    assertEqualsDouble(fitted.camera.aspectRatio.value,4.0/3.0,1e-12)
    val moved = SurfaceViewer.reduce(model,fitted,SurfaceViewerAction.SetPan(1.0,2.0)).toOption.get
    val reset = SurfaceViewer.reduce(model,moved,SurfaceViewerAction.ResetCamera).toOption.get
    assertEquals(moved.camera.aspectRatio,fitted.camera.aspectRatio)
    assertEquals(reset.camera.aspectRatio,fitted.camera.aspectRatio)
    val orbit = SurfaceViewer.reduce(model,fitted,SurfaceViewerAction.OrbitBy(15.0,10.0)).toOption.get
    assertEquals(orbit.camera.zoom,fitted.camera.zoom)
    assertEquals(orbit.camera.aspectRatio,fitted.camera.aspectRatio)
    val compiled = SurfaceCompiler.compile(model,fitted).toOption.get
    assertEquals(compiled.viewportFit,SurfaceViewportFit.Contain(4.0/3.0))
    assert(compiled.receipt.cameraKey != SurfaceCompiler.compile(model,
      fitted.copy(camera = fitted.camera.copy(aspectRatio = CameraAspectRatio.Default))).toOption.get.receipt.cameraKey)

  test("fitting is invariant to common world translation and bilateral source separation"):
    val (a,sa) = fixture(true)
    val (b,sb) = fixture(true,20.0)
    val ca = SurfaceViewer.reduce(a,sa,SurfaceViewerAction.FitCamera).toOption.get.camera
    val cb = SurfaceViewer.reduce(b,sb,SurfaceViewerAction.FitCamera).toOption.get.camera
    assertEqualsDouble(ca.zoom.value,cb.zoom.value,1e-12)
    assertEqualsDouble(ca.aspectRatio.value,cb.aspectRatio.value,1e-12)
    val (single,ss) = fixture(false)
    val cs = SurfaceViewer.reduce(single,ss,SurfaceViewerAction.FitCamera).toOption.get.camera
    assertEqualsDouble(ca.zoom.value,cs.zoom.value,1e-12)

  test("camera aspect and unknown fit surfaces are rejected at public boundaries"):
    for value <- Vector(0.0,-1.0,Double.NaN,Double.PositiveInfinity) do assert(CameraAspectRatio.make(value).isLeft)
    val (model,state) = fixture(false)
    assert(SurfaceViewer.reduce(model,state.copy(layout = SurfaceLayout.Single(SurfaceId.unsafe("missing"))),SurfaceViewerAction.FitCamera).isLeft)

  test("explicit fit contains expanded translated morphs without automatic camera movement"):
    val (original, _) = fixture(false)
    val source = original.surfaces.head
    val indices = source.geometry.mesh.faceIndices
    val faces = (0 until indices.length by 3).map(i => (indices(i), indices(i + 1), indices(i + 2)))
    val inflated = SurfaceGeometry(TriangleMesh.fromRows(
      source.geometry.mesh.vertices.map(p => Seq(p.x * 1.8 + 35.0, p.y * 1.7 - 40.0, p.z * 1.6 + 25.0)), faces),
      Hemisphere.Left, SurfaceKind.Inflated)
    val asset = SurfaceAsset.make(source.id, SurfaceSet.of(source.geometry.kind, source.geometry,
      inflated.kind -> inflated)).toOption.get
    val model = SurfaceViewerModel.make(Vector(asset), Vector.empty).toOption.get
    for
      projection <- Vector(CameraProjection.Perspective(FieldOfViewDegrees.Default),
        CameraProjection.Orthographic(OrthographicScale.unsafe(20.0)))
      fraction <- Vector(0.5, 1.0)
    do
      val initial = SurfaceViewerState.initial(model)
      val cameraState = initial.copy(camera = initial.camera.copy(projection = projection,
        orbit = SurfaceOrbit.unsafe(17.0, -11.0)))
      val canonical = SurfaceViewer.reduce(model, cameraState, SurfaceViewerAction.FitCamera).toOption.get
      val started = SurfaceViewer.reduce(model, canonical,
        SurfaceViewerAction.BeginGeometryMorph(source.id, SurfaceKind.Inflated)).toOption.get
      val morphed = SurfaceViewer.reduce(model, started,
        SurfaceViewerAction.SetGeometryMorphFraction(source.id, SurfaceMorphFraction.unsafe(fraction))).toOption.get
      assertEquals(morphed.camera, canonical.camera)
      val before = SurfaceCompiler.compile(model, morphed).toOption.get
      assert(projected(before).exists((x, y, _) => math.max(math.abs(x), math.abs(y)) > 0.9))
      val fitted = SurfaceViewer.reduce(model, morphed, SurfaceViewerAction.FitCamera).toOption.get
      val after = SurfaceCompiler.compile(model, fitted).toOption.get
      assertEquals(after.receipt.meshKeys, before.receipt.meshKeys)
      assertEquals(after.receipt.layerKeys, before.receipt.layerKeys)
      assertEquals(after.meshes.head.geometryKey, before.meshes.head.geometryKey)
      assertEquals(fitted.camera.orbit, canonical.camera.orbit)
      projected(after).foreach: (x, y, z) =>
        assert(math.abs(x) <= 0.90001 && math.abs(y) <= 0.90001 && math.abs(z) < 1.0,
          s"morph $fraction outside fitted frustum: $x $y $z")
