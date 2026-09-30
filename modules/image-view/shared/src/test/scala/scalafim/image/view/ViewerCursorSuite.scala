package scalafim.image.view

import image4s.geometry.{Affine, D3, Point}
import reframe4s.lie.FramedAffine
import scalafim.image.*
import scalafim.image.world.{FrameCatalog, Spaces, TemplateName, WorldLink, WorldSpace}

/** STP P8.02: the viewer cursor crosses into typed frames only through the reference frame's alignment. */
class ViewerCursorSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def volumeIn(template: String): SomeScalarVolume[Double] =
    val space = ok(SampleSpaces.inWorld(SampleSpaces(Vector(5, 5, 5)), WorldSpace.Template(TemplateName.unsafe(template))))
    val typed = ok(SampleSpaces.requireVolumeD3(space))
    SomeScalarVolume.unsafeCopyFromCanonicalArray(PrimitiveBuffers.tabulate[Double](typed.grid.shape.product)(_.toDouble), typed, template)

  private val reference = volumeIn("MNI152NLin2009cAsym")
  private val model = ok(
    ViewerModel.make(
      reference.grid,
      Vector(SliceLayer(LayerId.unsafe("t1"), reference, SliceSampling.Linear(), ScalarColorizer(DisplayWindow.unsafe(0.0, 10.0))))
    )
  )
  private val state = ViewerState.centered(reference.grid)
  private val mni = Spaces.MNI152NLin2009cAsym

  test("the cursor reads as a point of any owner of the reference frame's world"):
    assert(!mni.sameRuntimeOwnerAs(reference.grid.frame), clue = "the static template frame is another owner of the key")
    val cursor = ok(ViewerCursor.point(model, state, mni))
    assert(cursor.belongsTo(mni))
    assertEquals(cursor.coordinates, Vector(state.cursor.x, state.cursor.y, state.cursor.z))

  test("a typed point sets the cursor only when its frame is the reference frame"):
    val action = ok(ViewerCursor.set(model, ok(Point.in(mni)(1.0, -2.0, 3.0))))
    action match
      case ViewerAction.SetCursor(cursor) =>
        assert(cursor.belongsTo(reference.grid.frame))
        assertEquals(cursor.toWorldPoint, WorldPoint(1.0, -2.0, 3.0))
      case other => fail(s"expected SetCursor, got $other")
    val fsaverage = FrameCatalog.frame(WorldSpace.Template(TemplateName.unsafe("fsaverage")))
    assert(ViewerCursor.set(model, ok(Point.in(fsaverage)(1.0, -2.0, 3.0))).left.exists(_.isInstanceOf[ViewerCursorError.FrameMismatch]))
    assert(ViewerCursor.point(model, state, fsaverage).left.exists(_.isInstanceOf[ViewerCursorError.FrameMismatch]))

  test("a point in another world reaches the cursor through a typed link"):
    val subject = FrameCatalog.frame(ok(WorldSpace.declare("sub-01 T1w")))
    // pullback MNI -> subject: subject = MNI + (2, 0, 0); the forward map is its exact inverse
    val pull = FramedAffine.betweenFrames[mni.type, subject.type, D3](mni, subject)(ok(Affine.fromRowMajor[D3](Vector(1, 0, 0, 2, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))))
    val link = ok(WorldLink.pullback[subject.type, mni.type](pull, Some(pull.inverse), "affine"))
    assert(ViewerCursor.set(model, ok(Point.in(subject)(5.0, 5.0, 5.0))).isLeft, clue = "subject coordinates are not MNI coordinates")
    val inMni = ok(link.toRight(ok(Point.in(subject)(5.0, 5.0, 5.0))))
    ok(ViewerCursor.set(model, inMni)) match
      case ViewerAction.SetCursor(cursor) => assertEquals(cursor.toWorldPoint, WorldPoint(3.0, 5.0, 5.0))
      case other => fail(s"expected SetCursor, got $other")

  test("cursor actions and states cannot replay into an equally shaped different world"):
    val other = volumeIn("fsaverage")
    val otherModel = ok(
      ViewerModel.make(
        other.grid,
        Vector(SliceLayer(LayerId.unsafe("other"), other, SliceSampling.Linear(), ScalarColorizer(DisplayWindow.unsafe(0.0, 10.0))))
      )
    )
    val otherSession = ViewerSession(ViewerState.centered(other.grid), intaglio.DeviceContext.unsafe(320.0, 240.0))
    val action = ok(ViewerCursor.set(model, ok(Point.in(mni)(1.0, 2.0, 3.0))))

    assert(ViewerReducer.reduce(otherModel, otherSession, action).left.exists(_.isInstanceOf[ImageViewError.CursorFrameMismatch]))
    assert(ViewerSession(state, otherSession.device).validateModel(otherModel).left.exists(_.isInstanceOf[ImageViewError.CursorFrameMismatch]))
    assert(ViewerCompiler.compile(otherModel, state, otherSession.device).left.exists(_.isInstanceOf[ImageViewError.CursorFrameMismatch]))

  test("equivalent persistent frame owners remain valid and pick and scroll retain the reference owner"):
    val session = ViewerSession(state, intaglio.DeviceContext.unsafe(900.0, 700.0))
    val equivalent = ok(ViewerCursor.set(model, ok(Point.in(mni)(1.0, 2.0, 3.0))))
    val adopted = ok(ViewerReducer.reduce(model, session, equivalent))
    assert(adopted.state.cursor.belongsTo(reference.grid.frame))

    val scrolled = ok(ViewerReducer.reduce(model, session, ViewerAction.Scroll(AnatomicalPlane.Axial, 1)))
    assert(scrolled.state.cursor.belongsTo(reference.grid.frame))
    val panel = ViewerCompiler.panels(reference.grid, session.state, session.device).axial
    val picked = ok(ViewerReducer.reduce(
      model,
      session,
      ViewerAction.Pick(AnatomicalPlane.Axial, ViewerPointer.unsafe(panel.rect.left + panel.rect.width / 2.0, panel.rect.bottom + panel.rect.height / 2.0))
    ))
    assert(picked.state.cursor.belongsTo(reference.grid.frame))
