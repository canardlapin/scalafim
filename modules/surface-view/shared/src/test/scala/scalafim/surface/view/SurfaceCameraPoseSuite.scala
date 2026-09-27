package scalafim.surface.view

import image4s.geometry.{Affine, D3, Point}
import reframe4s.lie.FramedAffine
import scalafim.image.world.*
import scalafim.surface.*

/** STP P8.02: surface camera targets and positions are typed points in the displayed surface's frame. */
class SurfaceCameraPoseSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def affine(values: Double*): Affine[D3] = ok(Affine.fromRowMajor[D3](values.toVector))

  private val tkRas =
    val namespace = ok(DatasetNamespace("ds-camera"))
    val reference = ok(ReferenceAcquisition(Map("acq" -> "mprage"), ok(GeometryDigest(Vector(8, 8, 8), Vector.fill(12)(1.0), 1, 1))))
    FrameCatalog.frame(WorldSpace.SubjectTkRas(namespace, ok(SubjectId("01")), reference))
  private val mni = Spaces.MNI152NLin2009cAsym

  private val mesh = TriangleMesh.fromRows(
    Vector(Vector(0.0, 0.0, 0.0), Vector(10.0, 0.0, 0.0), Vector(0.0, 10.0, 0.0), Vector(0.0, 0.0, 10.0)),
    Vector((0, 1, 2), (0, 1, 3), (0, 2, 3), (1, 2, 3))
  )
  /** Stored coordinates placed by a translation, so display coordinates differ from stored ones. */
  private val placedGeometry = SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White, affine(1, 0, 0, -20, 0, 1, 0, 4, 0, 0, 1, 7, 0, 0, 0, 1))
  private val id = SurfaceId.unsafe("lh.white")
  private val model = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, placedGeometry))), Vector.empty))
  private val state = SurfaceViewerState.initial(model)
  private val display = ok(SurfaceDisplayFrame.declare(model, id, tkRas))

  /** Row-major 4x4 view matrix applied to a point. */
  private def toView(plan: SurfaceRenderPlan, p: Vector[Double]): Vector[Double] =
    val m = plan.camera.viewMatrix.unsafeArray
    Vector.tabulate(3)(row => m(4 * row) * p(0) + m(4 * row + 1) * p(1) + m(4 * row + 2) * p(2) + m(4 * row + 3))

  test("the viewpoint camera's pose is typed in the display frame and compiles to the same view"):
    val pose = ok(SurfaceCompiler.cameraPose(model, state, display))
    assert(pose.target.belongsTo(tkRas))
    assert(pose.eye.belongsTo(tkRas))
    // the family frame is the centre of the placed geometry's bounds: (-15, 9, 12)
    assertEquals(pose.target.coordinates, Vector(-15.0, 9.0, 12.0))
    val direction = state.camera.viewpoint.cameraDirection
    assertEqualsDouble(pose.direction.coordinates(0), direction._1, 1e-12)
    assertEqualsDouble(pose.direction.coordinates(1), direction._2, 1e-12)
    assertEqualsDouble(pose.direction.coordinates(2), direction._3, 1e-12)
    val plain = ok(SurfaceCompiler.compile(model, state))
    val posed = ok(SurfaceCompiler.compile(model, state, display, pose))
    plain.camera.viewMatrix.unsafeArray.zip(posed.camera.viewMatrix.unsafeArray).foreach((a, b) => assertEqualsDouble(a.toDouble, b.toDouble, 0.0))
    assertEqualsDouble(posed.camera.directionX, plain.camera.directionX, 1e-12)
    assert(posed.receipt.cameraKey.startsWith("pose:"))
    assertEquals(posed.meshes.map(_.geometryKey), plain.meshes.map(_.geometryKey))

  test("a volume point linked to the surface focuses the camera on its vertex"):
    val white = ok(FramedSurface.in(tkRas)(SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.White), SurfacePlacement.StoredCoordinates))
    val shown = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, white.geometry))), Vector.empty))
    val shownState = SurfaceViewerState.initial(shown)
    val bound: SurfaceDisplayFrame[tkRas.type] = ok(SurfaceDisplayFrame.bind(shown, id, white))
    val pull = FramedAffine.betweenFrames[mni.type, tkRas.type, D3](mni, tkRas)(affine(1, 0, 0, 5, 0, 1, 0, -3, 0, 0, 1, 1, 0, 0, 0, 1))
    val link = ok(WorldLink.pullback[tkRas.type, mni.type](pull, None, "composite without inverse"))
    val cursor = ok(SurfaceVolumeCursor.forDisplay(bound, link))
    val hit = ok(cursor.toSurface(ok(Point.in(mni)(-5.0, 3.0, 8.5)), SurfaceLinkRadius.unsafe(1.0)))
    assertEquals(hit.selection.vertex, VertexId(3))
    val initial = ok(SurfaceCompiler.cameraPose(shown, shownState, bound))
    val focused = ok(initial.focusedOn(hit.vertex))
    assertEquals(focused.target.coordinates, Vector(0.0, 0.0, 10.0))
    assertEqualsDouble(focused.distance, initial.distance, 1e-9)
    val plan = ok(SurfaceCompiler.compile(shown, shownState, bound, focused))
    val centred = toView(plan, hit.vertex.coordinates)
    assertEqualsDouble(centred(0), 0.0, 1e-4)
    assertEqualsDouble(centred(1), 0.0, 1e-4)
    assertEqualsDouble(centred(2), -focused.distance, 1e-4)

  test("typed poses need the single-surface layout of their display surface"):
    val rightId = SurfaceId.unsafe("rh.white")
    val right = SurfaceGeometry(mesh, Hemisphere.Right, SurfaceKind.White)
    val both = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, placedGeometry)), ok(SurfaceAsset.make(rightId, right))), Vector.empty))
    val left = ok(SurfaceDisplayFrame.declare(both, id, tkRas))
    val bilateral = ok(SurfaceViewer.reduce(both, SurfaceViewerState.initial(both), SurfaceViewerAction.SetLayout(SurfaceLayout.Bilateral(id, rightId))))
    assertEquals(
      SurfaceCompiler.cameraPose(both, bilateral, left).left.map(_.isInstanceOf[SurfaceCameraError.LayoutMismatch]),
      Left(true)
    )
    val single = SurfaceViewerState.initial(both)
    val pose = ok(SurfaceCompiler.cameraPose(both, single, left))
    val showingRight = ok(SurfaceViewer.reduce(both, single, SurfaceViewerAction.SetLayout(SurfaceLayout.Single(rightId))))
    assertEquals(
      SurfaceCompiler.compile(both, showingRight, left, pose),
      Left(SurfaceCameraError.LayoutMismatch(id, SurfaceLayout.Single(rightId)))
    )

  test("display frames are checked against what the viewer shows"):
    assertEquals(SurfaceDisplayFrame.declare(model, SurfaceId.unsafe("missing"), tkRas).map(_.surfaceId), Left(SurfaceCameraError.UnknownSurface(SurfaceId.unsafe("missing"))))
    val stored = ok(FramedSurface.in(tkRas)(placedGeometry, SurfacePlacement.StoredCoordinates))
    SurfaceDisplayFrame.bind(model, id, stored) match
      case Left(SurfaceCameraError.DisplayMismatch(`id`, reason)) => assert(reason.contains("mm"), clue = reason)
      case other                                                  => fail(s"expected the stored-coordinate surface to be refused, got $other")
    val placed = ok(FramedSurface.in(tkRas)(placedGeometry, SurfacePlacement.SurfaceToWorld))
    assert(SurfaceDisplayFrame.bind(model, id, placed).isRight)
    val replaced = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, SurfaceGeometry(mesh, Hemisphere.Left, SurfaceKind.Pial)))), Vector.empty))
    val pose = ok(SurfaceCompiler.cameraPose(model, state, display))
    SurfaceCompiler.compile(replaced, SurfaceViewerState.initial(replaced), display, pose) match
      case Left(SurfaceCameraError.DisplayMismatch(`id`, _)) => ()
      case other                                             => fail(s"expected a replaced geometry to be refused, got $other")
    val rebuilt = ok(SurfaceViewerModel.make(Vector(ok(SurfaceAsset.make(id, placedGeometry))), Vector.empty))
    assert(
      SurfaceCompiler.compile(rebuilt, state, display, pose).left.exists(_.isInstanceOf[SurfaceCameraError.DisplayMismatch]),
      clue = "a display frame belongs to the asset instance it was declared for"
    )
    val redeclared = ok(SurfaceDisplayFrame.declare(rebuilt, id, tkRas))
    assert(SurfaceCompiler.compile(rebuilt, state, redeclared, ok(SurfaceCompiler.cameraPose(rebuilt, state, redeclared))).isRight)

  test("a pose needs distinct eye and target points"):
    val p = ok(SurfaceCameraPose.pointIn(tkRas, Vector(1.0, 2.0, 3.0)))
    assertEquals(SurfaceCameraPose.make(p, p).map(_.distance), Left(SurfaceCameraError.DegeneratePose(0.0)))
