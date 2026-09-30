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
    assertEquals(ViewerCursor.set(model, ok(Point.in(mni)(1.0, -2.0, 3.0))), Right(ViewerAction.SetCursor(WorldPoint(1.0, -2.0, 3.0))))
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
    assertEquals(ViewerCursor.set(model, inMni), Right(ViewerAction.SetCursor(WorldPoint(3.0, 5.0, 5.0))))
