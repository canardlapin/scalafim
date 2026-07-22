package scalafim.surface.view

import scalafim.graphics.*
import scalafim.image.DMat
import scalafim.surface.*

class SurfaceViewerSuite extends munit.FunSuite:

  private def mesh(offset: Double = 0.0): TriangleMesh =
    TriangleMesh.fromRows(
      Seq(
        Seq(offset + 0.0, 0.0, 0.0),
        Seq(offset + 1.0, 0.0, 0.0),
        Seq(offset + 0.0, 1.0, 0.0),
        Seq(offset + 0.0, 0.0, 1.0)
      ),
      Seq((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
    )

  private def geometry(
    hemisphere: Hemisphere = Hemisphere.Left,
    offset: Double = 0.0,
    transform: DMat = DMat.eye(4),
    kind: SurfaceKind = SurfaceKind.Inflated
  ): SurfaceGeometry =
    SurfaceGeometry(mesh(offset), hemisphere, kind, transform)

  private val leftId = SurfaceId.unsafe("left")
  private val rightId = SurfaceId.unsafe("right")
  private val scalarId = SurfaceLayerId.unsafe("activation")
  private val labelId = SurfaceLayerId.unsafe("atlas")

  private def scalarLayer(
    geometry: SurfaceGeometry,
    surfaceId: SurfaceId = leftId,
    frames: Int = 1
  ): SurfaceLayer =
    val values = Array.tabulate(geometry.vertexCount * frames)(index => index.toDouble - 1.0)
    SurfaceLayer.scalar(
      scalarId,
      surfaceId,
      geometry,
      values,
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 6.0)),
      frameCount = frames
    ).toOption.get

  private def labelLayer(geometry: SurfaceGeometry): SurfaceLayer =
    SurfaceLayer.labels(
      labelId,
      leftId,
      geometry,
      Array(1, 1, 2, 2),
      LabelColorizer(Map(1 -> Rgba32.unsafe(255, 0, 0), 2 -> Rgba32.unsafe(0, 0, 255)))
    ).toOption.get

  private def model(frames: Int = 1): SurfaceViewerModel =
    val left = geometry()
    SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, left).toOption.get),
      Vector(scalarLayer(left, frames = frames), labelLayer(left))
    ).toOption.get

  test("opaque ids and camera scalars reject invalid states"):
    assert(SurfaceId.make(" ").isLeft)
    assert(SurfaceLayerId.make("").isLeft)
    assert(CameraZoom.make(0.0).isLeft)
    assert(FieldOfViewDegrees.make(180.0).isLeft)
    assert(OrthographicScale.make(Double.NaN).isLeft)
    assert(SurfaceClipping.nearFar(2.0, 1.0).isLeft)
    assert(SurfaceLighting.directional(0.2, 0.8, 0.0, 0.0, 0.0).isLeft)
    assert(SurfaceOrbit.make(0.0, 90.0).isLeft)
    assertEqualsDouble(SurfaceOrbit.unsafe(370.0, 20.0).yawDegrees, 10.0, 0.0)

  test("canonical anatomical viewpoints encode RAS directions explicitly for both hemispheres"):
    assertEquals(SurfaceViewpoint.Lateral(CorticalHemisphere.Left).cameraDirection, (-1.0, 0.0, 0.0))
    assertEquals(SurfaceViewpoint.Medial(CorticalHemisphere.Left).cameraDirection, (1.0, 0.0, 0.0))
    assertEquals(SurfaceViewpoint.Lateral(CorticalHemisphere.Right).cameraDirection, (1.0, 0.0, 0.0))
    assertEquals(SurfaceViewpoint.Medial(CorticalHemisphere.Right).cameraDirection, (-1.0, 0.0, 0.0))
    assertEquals(SurfaceViewpoint.Anterior.cameraDirection, (0.0, 1.0, 0.0))
    assertEquals(SurfaceViewpoint.Posterior.cameraDirection, (0.0, -1.0, 0.0))
    assertEquals(SurfaceViewpoint.Dorsal.cameraDirection, (0.0, 0.0, 1.0))
    assertEquals(SurfaceViewpoint.Ventral.cameraDirection, (0.0, 0.0, -1.0))

  test("layers own input buffers and validate frame-major lengths"):
    val left = geometry()
    val values = Array(-1.0, 0.0, 1.0, 2.0)
    val layer = SurfaceLayer.scalar(
      scalarId,
      leftId,
      left,
      values,
      ScalarColorizer(DisplayWindow.unsafe(-1.0, 2.0))
    ).toOption.get
    values(0) = 99.0
    val asset = SurfaceAsset.make(leftId, left).toOption.get
    val viewer = SurfaceViewerModel.make(Vector(asset), Vector(layer)).toOption.get
    val plan = SurfaceCompiler.compile(viewer, SurfaceViewerState.initial(viewer)).toOption.get
    assertEquals(plan.readouts, Vector.empty)
    assertEquals(plan.layers.head.colors(0), Rgba32.unsafe(0, 0, 0).packedInt)
    assert(SurfaceLayer.scalar(scalarId, leftId, left, Array(1.0), ScalarColorizer(DisplayWindow.unsafe(0.0, 1.0))).isLeft)

  test("model rejects duplicate ids, unknown surfaces, topology mismatch, and dynamic frame disagreement"):
    val left = geometry()
    val asset = SurfaceAsset.make(leftId, left).toOption.get
    assert(SurfaceViewerModel.make(Vector(asset, asset), Vector.empty).isLeft)

    val unknown = scalarLayer(left, SurfaceId.unsafe("missing"))
    assertEquals(
      SurfaceViewerModel.make(Vector(asset), Vector(unknown)).left.toOption,
      Some(SurfaceViewError.UnknownSurface(SurfaceId.unsafe("missing")))
    )

    val rewound = TriangleMesh.fromRows(left.mesh.vertices.map(p => Seq(p.x, p.y, p.z)), Seq((0, 2, 1), (0, 1, 3), (0, 2, 3), (1, 2, 3)))
    val wrongGeometry = SurfaceGeometry(rewound, Hemisphere.Left, SurfaceKind.Pial)
    val wrongLayer = scalarLayer(wrongGeometry)
    assertEquals(
      SurfaceViewerModel.make(Vector(asset), Vector(wrongLayer)).left.toOption,
      Some(SurfaceViewError.IncompatibleLayerDomain(scalarId, leftId))
    )

    val two = scalarLayer(left, frames = 2)
    val three = SurfaceLayer.scalar(
      SurfaceLayerId.unsafe("three"), leftId, left,
      Array.tabulate(12)(_.toDouble), ScalarColorizer(DisplayWindow.unsafe(0.0, 12.0)), frameCount = 3
    ).toOption.get
    assert(SurfaceViewerModel.make(Vector(asset), Vector(two, three)).isLeft)

  test("reducer is total over timepoint, layer capability, ordering, and selection actions"):
    val viewer = model(frames = 2)
    val initial = SurfaceViewerState.initial(viewer)
    val atOne = SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.SetTimepoint(1)).toOption.get
    assertEquals(atOne.timepoint, 1)
    assert(SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.SetTimepoint(2)).isLeft)
    assert(SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetLayerWindow(labelId, DisplayWindow.unsafe(0.0, 2.0))
    ).isLeft)

    val reordered = SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.MoveLayer(labelId, 0)).toOption.get
    assertEquals(reordered.layerOrder, Vector(labelId, scalarId))
    assert(SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.MoveLayer(labelId, 2)).isLeft)

    val selected = SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.Select(leftId, VertexId(3))).toOption.get
    assertEquals(selected.selection.map(_.vertex.index), Some(3))
    assert(SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.Select(leftId, VertexId(4))).isLeft)
    assertEquals(
      SurfaceViewer.reduce(viewer, selected, SurfaceViewerAction.ClearSelection).toOption.get.selection,
      None
    )

  test("bilateral layout validates hemispheres and preserves explicit order"):
    val left = geometry(Hemisphere.Left)
    val right = geometry(Hemisphere.Right, offset = 2.0)
    val viewer = SurfaceViewerModel.make(
      Vector(
        SurfaceAsset.make(leftId, left).toOption.get,
        SurfaceAsset.make(rightId, right).toOption.get
      ),
      Vector.empty
    ).toOption.get
    val initial = SurfaceViewerState.initial(viewer)
    val layout = SurfaceLayout.Bilateral(leftId, rightId, BilateralOrder.RightThenLeft)
    val state = SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.SetLayout(layout)).toOption.get
    val plan = SurfaceCompiler.compile(viewer, state).toOption.get
    assertEquals(plan.slots.map(_.surface), Vector(rightId, leftId))
    assertEqualsDouble(plan.slots.head.viewport.width, 0.5, 0.0)

    val reversed = SurfaceLayout.Bilateral(rightId, leftId)
    assert(SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.SetLayout(reversed)).isLeft)

  test("compiler applies surfaceToWorld before packing positions and readouts"):
    val transform = DMat.fromRows(Vector(
      Vector(1.0, 0.0, 0.0, 10.0),
      Vector(0.0, 2.0, 0.0, 20.0),
      Vector(0.0, 0.0, 3.0, 30.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))
    val left = geometry(transform = transform)
    val viewer = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, left).toOption.get),
      Vector(scalarLayer(left))
    ).toOption.get
    val selected = SurfaceViewer.reduce(
      viewer,
      SurfaceViewerState.initial(viewer),
      SurfaceViewerAction.Select(leftId, VertexId(2))
    ).toOption.get
    val plan = SurfaceCompiler.compile(viewer, selected).toOption.get
    assertEquals(plan.meshes.head.positions(0), 10.0f)
    assertEquals(plan.meshes.head.positions(1), 20.0f)
    assertEquals(plan.meshes.head.positions(2), 30.0f)
    val readout = plan.readouts.head
    assertEqualsDouble(readout.worldX, 10.0, 0.0)
    assertEqualsDouble(readout.worldY, 22.0, 0.0)
    assertEqualsDouble(readout.worldZ, 30.0, 0.0)
    assertEquals(readout.layerValues.head._1, scalarId)
    assertEquals(plan.chrome.size, 1)

  test("compiler centers the camera on transformed world bounds"):
    val transform = DMat.fromRows(Vector(
      Vector(1.0, 0.0, 0.0, 10.0),
      Vector(0.0, 2.0, 0.0, 20.0),
      Vector(0.0, 0.0, 3.0, 30.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))
    val left = geometry(transform = transform)
    val viewer = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, left).toOption.get),
      Vector.empty
    ).toOption.get
    val plan = SurfaceCompiler.compile(viewer, SurfaceViewerState.initial(viewer)).toOption.get
    val view = plan.camera.viewMatrix
    val centerX = 10.5
    val centerY = 21.0
    val centerZ = 31.5
    def transformed(row: Int): Double =
      view(row * 4).toDouble * centerX +
        view(row * 4 + 1).toDouble * centerY +
        view(row * 4 + 2).toDouble * centerZ +
        view(row * 4 + 3).toDouble
    assertEqualsDouble(transformed(0), 0.0, 1e-6)
    assertEqualsDouble(transformed(1), 0.0, 1e-6)
    assertEqualsDouble(transformed(2), -4.0, 1e-6)

    val untranslated = geometry()
    val untranslatedViewer = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, untranslated).toOption.get),
      Vector.empty
    ).toOption.get
    val untranslatedPlan = SurfaceCompiler.compile(
      untranslatedViewer,
      SurfaceViewerState.initial(untranslatedViewer)
    ).toOption.get
    assertEquals(plan.receipt.meshKeys, untranslatedPlan.receipt.meshKeys)
    assertNotEquals(plan.meshes.head.geometryKey, untranslatedPlan.meshes.head.geometryKey)
    assertNotEquals(plan.receipt.cameraKey, untranslatedPlan.receipt.cameraKey)

  test("anatomical camera distance scales beyond millimetre-space geometry"):
    val millimetres = DMat.fromRows(Vector(
      Vector(100.0, 0.0, 0.0, 0.0),
      Vector(0.0, 100.0, 0.0, 0.0),
      Vector(0.0, 0.0, 100.0, 0.0),
      Vector(0.0, 0.0, 0.0, 1.0)
    ))
    val left = geometry(transform = millimetres)
    val viewer = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, left).toOption.get),
      Vector.empty
    ).toOption.get
    val plan = SurfaceCompiler.compile(viewer, SurfaceViewerState.initial(viewer)).toOption.get
    val view = plan.camera.viewMatrix
    val positions = plan.meshes.head.positions
    var offset = 0
    while offset < positions.length do
      val cameraZ =
        view(8) * positions(offset) +
          view(9) * positions(offset + 1) +
          view(10) * positions(offset + 2) +
          view(11)
      assert(cameraZ < 0.0f, s"vertex ${offset / 3} remained behind the anatomical camera")
      offset += 3

  test("threshold boundaries, layer order, receipts, and profile are deterministic"):
    val viewer = model()
    val initial = SurfaceViewerState.initial(viewer)
    val thresholded = SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetLayerThreshold(scalarId, DisplayThreshold.transparentBand(-0.5, 0.5).toOption.get)
    ).toOption.get
    val reordered = SurfaceViewer.reduce(viewer, thresholded, SurfaceViewerAction.MoveLayer(labelId, 0)).toOption.get
    val first = SurfaceCompiler.compile(viewer, reordered).toOption.get
    val second = SurfaceCompiler.compile(viewer, reordered).toOption.get
    assertEquals(first.receipt, second.receipt)
    assertEquals(first.layers.map(_.layer), Vector(labelId, scalarId))
    assertEquals(first.drawPasses.length, 2)
    assertEquals(first.profile.verticesPacked, 4)
    assertEquals(first.profile.facesPacked, 4)
    assertEquals(first.profile.colorValuesWritten, 8)

  test("camera-only actions preserve every mesh and layer resource key"):
    val viewer = model()
    val initial = SurfaceViewerState.initial(viewer)
    val before = SurfaceCompiler.compile(viewer, initial).toOption.get
    val moved = SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetViewpoint(SurfaceViewpoint.Dorsal)
    ).toOption.get
    val after = SurfaceCompiler.compile(viewer, moved).toOption.get
    assertEquals(after.receipt.meshKeys, before.receipt.meshKeys)
    assertEquals(after.receipt.layerKeys, before.receipt.layerKeys)
    assertNotEquals(after.receipt.cameraKey, before.receipt.cameraKey)
    assertEquals(after.camera.directionZ, 1.0)

  test("geometry families morph without changing topology, layers, selection, or camera framing"):
    val white = geometry(offset = 0.0, kind = SurfaceKind.White)
    val pial = geometry(offset = 2.0, kind = SurfaceKind.Pial)
    val family = SurfaceSet.of(
      SurfaceKind.White,
      white,
      SurfaceKind.Pial -> pial
    )
    val asset = SurfaceAsset.make(leftId, family).toOption.get
    val viewer = SurfaceViewerModel.make(
      Vector(asset),
      Vector(scalarLayer(white))
    ).toOption.get
    val selected = SurfaceViewer.reduce(
      viewer,
      SurfaceViewerState.initial(viewer),
      SurfaceViewerAction.Select(leftId, VertexId(0))
    ).toOption.get
    val before = SurfaceCompiler.compile(viewer, selected).toOption.get
    val canonicalViewer = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, white).toOption.get),
      Vector.empty
    ).toOption.get
    val canonical = SurfaceCompiler.compile(
      canonicalViewer,
      SurfaceViewerState.initial(canonicalViewer)
    ).toOption.get
    assertEquals(
      before.camera.viewMatrix.unsafeArray.toSeq,
      canonical.camera.viewMatrix.unsafeArray.toSeq
    )
    val started = SurfaceViewer.reduce(
      viewer,
      selected,
      SurfaceViewerAction.BeginGeometryMorph(leftId, SurfaceKind.Pial)
    ).toOption.get
    val halfway = SurfaceViewer.reduce(
      viewer,
      started,
      SurfaceViewerAction.SetGeometryMorphFraction(leftId, SurfaceMorphFraction.unsafe(0.5))
    ).toOption.get
    val during = SurfaceCompiler.compile(viewer, halfway).toOption.get

    assertEquals(halfway.geometryPresentations(leftId), SurfaceGeometryPresentation.Morphing(
      SurfaceKind.White,
      SurfaceKind.Pial,
      SurfaceMorphFraction.unsafe(0.5)
    ))
    assertEquals(during.meshes.head.positions(0), 1.0f)
    assertEquals(during.receipt.meshKeys, before.receipt.meshKeys)
    assertNotEquals(during.meshes.head.geometryKey, before.meshes.head.geometryKey)
    assertEquals(during.receipt.layerKeys, before.receipt.layerKeys)
    assertEquals(during.receipt.cameraKey, before.receipt.cameraKey)
    assertEqualsDouble(during.readouts.head.worldX, 1.0, 0.0)
    assertEquals(during.readouts.head.vertex, 0)

    val reversed = SurfaceViewer.reduce(
      viewer,
      halfway,
      SurfaceViewerAction.BeginGeometryMorph(leftId, SurfaceKind.White)
    ).toOption.get
    assertEquals(reversed.geometryPresentations(leftId), SurfaceGeometryPresentation.Morphing(
      SurfaceKind.Pial,
      SurfaceKind.White,
      SurfaceMorphFraction.unsafe(0.5)
    ))
    val completed = SurfaceViewer.reduce(
      viewer,
      reversed,
      SurfaceViewerAction.SetGeometryMorphFraction(leftId, SurfaceMorphFraction.unsafe(1.0))
    ).toOption.get
    assertEquals(completed.geometryPresentations(leftId), SurfaceGeometryPresentation.Fixed(SurfaceKind.White))

  test("geodesic reveal weights have analytic smootherstep values and reject invalid domains"):
    val white = geometry(offset = 0.0, kind = SurfaceKind.White)
    val pial = geometry(offset = 2.0, kind = SurfaceKind.Pial)
    val lens = SurfaceGeodesicLens.make(
      white,
      VertexId(0),
      SurfaceLensRadius.unsafe(0.0),
      SurfaceLensRadius.unsafe(2.0)
    ).toOption.get
    assertEqualsDouble(lens.weightAt(VertexId(0)).get, 1.0, 0.0)
    assertEqualsDouble(lens.weightAt(VertexId(1)).get, 0.5, 1e-12)
    assertEqualsDouble(lens.weightAt(VertexId(2)).get, 0.5, 1e-12)
    assertEqualsDouble(lens.weightAt(VertexId(3)).get, 0.5, 1e-12)
    assertEquals(lens.activeVertexCount, 4)

    val revealed = SurfaceMorph.reveal(
      white,
      pial,
      lens,
      SurfaceMorphFraction.unsafe(1.0)
    ).toOption.get
    assertEqualsDouble(revealed.mesh.vertex(VertexId(0)).x, 2.0, 0.0)
    assertEqualsDouble(revealed.mesh.vertex(VertexId(1)).x, 2.0, 1e-12)
    assertEquals(revealed.mesh.faceIndices.toSeq, white.mesh.faceIndices.toSeq)
    assertEquals(revealed.mesh.topologyIdentity, white.mesh.topologyIdentity)

    assert(SurfaceLensRadius.make(-1.0).isLeft)
    assert(SurfaceLensRadius.make(Double.NaN).isLeft)
    assert(SurfaceGeodesicLens.make(
      white,
      VertexId(0),
      SurfaceLensRadius.unsafe(1.0),
      SurfaceLensRadius.unsafe(1.0)
    ).isLeft)
    assert(SurfaceGeodesicLens.make(
      white,
      VertexId(white.vertexCount),
      SurfaceLensRadius.unsafe(0.0),
      SurfaceLensRadius.unsafe(1.0)
    ).isLeft)
    assert(SurfaceMorph.reveal(
      geometry(Hemisphere.Right, kind = SurfaceKind.White),
      geometry(Hemisphere.Right, offset = 2.0, kind = SurfaceKind.Pial),
      lens,
      SurfaceMorphFraction.unsafe(0.5)
    ).isLeft)

  test("direct reveal lens remains an explicit diagnostic correspondence mode"):
    val white = geometry(offset = 0.0, kind = SurfaceKind.White)
    val pial = geometry(offset = 2.0, kind = SurfaceKind.Pial)
    val family = SurfaceSet.of(SurfaceKind.White, white, SurfaceKind.Pial -> pial)
    val viewer = SurfaceViewerModel.make(
      Vector(SurfaceAsset.make(leftId, family).toOption.get),
      Vector(scalarLayer(white))
    ).toOption.get
    val selected = SurfaceViewer.reduce(
      viewer,
      SurfaceViewerState.initial(viewer),
      SurfaceViewerAction.Select(leftId, VertexId(0))
    ).toOption.get
    val before = SurfaceCompiler.compile(viewer, selected).toOption.get
    val pinned = SurfaceViewer.reduce(
      viewer,
      selected,
      SurfaceViewerAction.BeginGeometryLens(
        leftId,
        SurfaceKind.Pial,
        VertexId(0),
        SurfaceLensRadius.unsafe(0.25),
        SurfaceLensRadius.unsafe(0.75),
        SurfaceLensDeformationPolicy.DirectCorrespondence
      )
    ).toOption.get
    val halfway = SurfaceViewer.reduce(
      viewer,
      pinned,
      SurfaceViewerAction.SetGeometryMorphFraction(leftId, SurfaceMorphFraction.unsafe(0.5))
    ).toOption.get
    val during = SurfaceCompiler.compile(viewer, halfway).toOption.get

    assertEquals(during.meshes.head.positions(0), 1.0f)
    assertEquals(during.meshes.head.positions(3), 1.0f)
    assertEquals(during.meshes.head.positions(6), 0.0f)
    assertEquals(during.receipt.meshKeys, before.receipt.meshKeys)
    assertNotEquals(during.meshes.head.geometryKey, before.meshes.head.geometryKey)
    assertEquals(during.receipt.layerKeys, before.receipt.layerKeys)
    assertEquals(during.receipt.cameraKey, before.receipt.cameraKey)
    assertEqualsDouble(during.readouts.head.worldX, 1.0, 0.0)
    assertEquals(during.readouts.head.vertex, 0)
    assert(SurfaceViewer.reduce(
      viewer,
      halfway,
      SurfaceViewerAction.BeginGeometryMorph(leftId, SurfaceKind.Pial)
    ).isLeft)

    val fullyOpen = SurfaceViewer.reduce(
      viewer,
      halfway,
      SurfaceViewerAction.SetGeometryMorphFraction(leftId, SurfaceMorphFraction.unsafe(1.0))
    ).toOption.get
    assert(fullyOpen.geometryPresentations(leftId).isInstanceOf[SurfaceGeometryPresentation.RevealLens])
    val retargeted = SurfaceViewer.reduce(
      viewer,
      fullyOpen,
      SurfaceViewerAction.BeginGeometryLens(
        leftId,
        SurfaceKind.Pial,
        VertexId(1),
        SurfaceLensRadius.unsafe(0.25),
        SurfaceLensRadius.unsafe(0.75),
        SurfaceLensDeformationPolicy.DirectCorrespondence
      )
    ).toOption.get
    val moved = SurfaceCompiler.compile(viewer, retargeted).toOption.get
    assertEquals(moved.meshes.head.positions(0), 0.0f)
    assertEquals(moved.meshes.head.positions(3), 3.0f)
    val cleared = SurfaceViewer.reduce(
      viewer,
      retargeted,
      SurfaceViewerAction.ClearGeometryLens(leftId)
    ).toOption.get
    assertEquals(cleared.geometryPresentations(leftId), SurfaceGeometryPresentation.Fixed(SurfaceKind.White))

  test("natural reveal is translation-invariant, pin-fixed, harmonic, and inversion-free"):
    def grid(kind: SurfaceKind, translationX: Double, warped: Boolean): SurfaceGeometry =
      val vertices = for
        y <- 0 until 5
        x <- 0 until 5
      yield
        val z = if warped then 0.35 * (x - 2).toDouble else 0.0
        Seq(x.toDouble + translationX, y.toDouble, z)
      val faces = for
        y <- 0 until 4
        x <- 0 until 4
        a = y * 5 + x
        triangle <- Seq((a, a + 1, a + 5), (a + 1, a + 6, a + 5))
      yield triangle
      SurfaceGeometry(TriangleMesh.fromRows(vertices, faces), Hemisphere.Left, kind)

    val source = grid(SurfaceKind.Pial, translationX = 0.0, warped = false)
    val translatedTarget = grid(SurfaceKind.Inflated, translationX = 12.0, warped = true)
    val alignedTarget = grid(SurfaceKind.Inflated, translationX = 0.0, warped = true)
    val center = VertexId(12)
    val lens = SurfaceGeodesicLens.make(
      source,
      center,
      SurfaceLensRadius.unsafe(1.1),
      SurfaceLensRadius.unsafe(3.1)
    ).toOption.get
    val translated = SurfaceLensDeformation.make(source, translatedTarget, lens).toOption.get
    val aligned = SurfaceLensDeformation.make(source, alignedTarget, lens).toOption.get

    assertEquals(translated.policy, SurfaceLensDeformationPolicy.Natural)
    assertEquals(translated.displacementAt(center), Some(Point3D.Zero))
    assertEquals(translated.displacementAt(VertexId(0)), Some(Point3D.Zero))
    var vertex = 0
    while vertex < source.vertexCount do
      val a = translated.displacementAt(VertexId(vertex)).get
      val b = aligned.displacementAt(VertexId(vertex)).get
      assertEqualsDouble(a.x, b.x, 1e-12)
      assertEqualsDouble(a.y, b.y, 1e-12)
      assertEqualsDouble(a.z, b.z, 1e-12)
      vertex += 1

    val collar = translated.displacementAt(VertexId(7)).get
    val direct = SurfaceLensDeformation.make(
      source,
      translatedTarget,
      lens,
      SurfaceLensDeformationPolicy.DirectCorrespondence
    ).toOption.get.displacementAt(VertexId(7)).get
    assertNotEquals(collar, direct)
    assertEquals(translated.quality.invertedTriangles, 0)
    assert(translated.quality.minimumAreaRatio > 0.0)
    assert(translated.quality.maximumEdgeStrain < 1.0)
    assert(translated.quality.p95EdgeStrain <= translated.quality.maximumEdgeStrain)

    val opened = SurfaceMorph.reveal(
      source,
      translatedTarget,
      translated,
      SurfaceMorphFraction.unsafe(1.0)
    ).toOption.get
    assertEquals(opened.mesh.vertex(center), source.mesh.vertex(center))
    assertEquals(opened.mesh.faceIndices.toSeq, source.mesh.faceIndices.toSeq)
    assertEquals(opened.mesh.topologyIdentity, source.mesh.topologyIdentity)
    assert(SurfaceLensRelaxationSteps.make(0).isLeft)
    assert(SurfaceLensRelaxationSteps.make(1001).isLeft)

  test("lens quality detects an intermediate collapse even when endpoint winding agrees"):
    val source = SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(0.0, 0.0, 0.0), Seq(1.0, 0.0, 0.0), Seq(0.0, 1.0, 0.0)),
        Seq((0, 1, 2))
      ),
      Hemisphere.Left,
      SurfaceKind.Pial
    )
    val rotated = SurfaceGeometry(
      TriangleMesh.fromRows(
        Seq(Seq(0.0, 0.0, 0.0), Seq(-1.0, 0.0, 0.0), Seq(0.0, -1.0, 0.0)),
        Seq((0, 1, 2))
      ),
      Hemisphere.Left,
      SurfaceKind.Inflated
    )
    val lens = SurfaceGeodesicLens.make(
      source,
      VertexId(0),
      SurfaceLensRadius.unsafe(10.0),
      SurfaceLensRadius.unsafe(20.0)
    ).toOption.get
    val deformation = SurfaceLensDeformation.make(
      source,
      rotated,
      lens,
      SurfaceLensDeformationPolicy.DirectCorrespondence
    ).toOption.get
    assertEquals(deformation.quality.invertedTriangles, 1)
    assertEqualsDouble(deformation.quality.minimumAreaRatio, 1.0, 1e-12)
    assertEquals(
      SurfaceLensDeformation.make(source, rotated, lens).left.toOption,
      Some(SurfaceViewError.UnsafeLensDeformation(1))
    )

  test("geometry family actions reject absent variants and invalid transition updates"):
    val viewer = model()
    val initial = SurfaceViewerState.initial(viewer)
    assert(SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetGeometryState(leftId, SurfaceKind.Pial)
    ).isLeft)
    assert(SurfaceViewer.reduce(
      viewer,
      initial,
      SurfaceViewerAction.SetGeometryMorphFraction(leftId, SurfaceMorphFraction.unsafe(0.5))
    ).isLeft)

  test("orbit and reset preserve canonical anatomical bases without resource uploads"):
    val viewer = model()
    val initial = SurfaceViewerState.initial(viewer)
    val before = SurfaceCompiler.compile(viewer, initial).toOption.get
    val orbited = SurfaceViewer.reduce(viewer, initial, SurfaceViewerAction.OrbitBy(20.0, 15.0)).toOption.get
    val during = SurfaceCompiler.compile(viewer, orbited).toOption.get
    assertEquals(during.receipt.meshKeys, before.receipt.meshKeys)
    assertEquals(during.receipt.layerKeys, before.receipt.layerKeys)
    assertNotEquals(during.receipt.cameraKey, before.receipt.cameraKey)
    assertNotEquals(during.camera.directionX, before.camera.directionX)
    val reset = SurfaceViewer.reduce(viewer, orbited, SurfaceViewerAction.ResetCamera).toOption.get
    val after = SurfaceCompiler.compile(viewer, reset).toOption.get
    assertEqualsDouble(after.camera.directionX, -1.0, 0.0)
    assertEqualsDouble(after.camera.directionY, 0.0, 0.0)
    assertEqualsDouble(after.camera.directionZ, 0.0, 0.0)
