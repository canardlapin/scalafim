package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Point}
import reframe4s.core.{MapError, SpatialMap}
import reframe4s.lie.FramedAffine
import scalafim.image.world.{FrameCatalog, LinkDirection, Spaces, WorldLinkError, WorldSpace}

/** STP P8.02: a `WorldTransform` is the typed map behind a linked cursor between two worlds. */
class WorldTransformLinkSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val native: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("sub-01 T1w")))
  private val mni: Frame[D3] = Spaces.MNI152NLin2009cAsym

  private val mniToNative =
    ok(Affine.fromRowMajor[D3](Vector(1.1, 0.05, -0.02, 3.0, -0.03, 0.95, 0.1, -12.5, 0.02, -0.08, 1.2, 7.25, 0, 0, 0, 1)))

  private val registration =
    WorldTransform.Linear(FramedAffine.betweenFrames[mni.type, native.type, D3](mni, native)(mniToNative), TransformProvenance.constructed("T1w to MNI affine"))

  /** A dense-style MNI -> T1w pullback read without its inverse, as from an fMRIPrep composite's warp stage. */
  private val warp =
    val shift = new SpatialMap[mni.type, mni.type, D3]:
      val source: mni.type = mni
      val target: mni.type = mni
      def apply(point: Point[mni.type, D3]): Either[MapError, Point[mni.type, D3]] =
        val c = point.coordinates
        Point.fromVector(mni, Vector(c(0) + math.sin(c(1) / 10.0), c(1), c(2))).left.map(MapError.Geometry(_))
    WorldTransform.Mapped(shift, PushAvailability.Unavailable[mni.type, mni.type](), TransformProvenance.constructed("synthetic warp"))

  private def close(a: Point[?, D3], b: Point[?, D3])(using munit.Location): Unit =
    a.coordinates.zip(b.coordinates).foreach((x, y) => assertEqualsDouble(x, y, 1e-9))

  test("an affine transform links both ways, agreeing with pullPoint and mapPoint"):
    val link = ok(registration.link)
    assert(LinkDirection.values.forall(link.supports))
    val peak = ok(Point.in(mni)(-42.0, 18.0, 24.0))
    val inSubject = ok(link.toLeft(peak))
    close(inSubject, ok(registration.pullPoint(peak)))
    close(ok(link.toRight(inSubject)), peak)
    close(ok(link.swap.toLeft(inSubject)), ok(registration.mapPoint(inSubject)))

  test("a composite whose warp has no inverse links target to source only; the forward direction is a typed error"):
    val composite: WorldTransform[native.type, mni.type] = registration.andThen(warp)
    val link = ok(composite.link)
    val peak = ok(Point.in(mni)(-42.0, 18.0, 24.0))
    close(ok(link.toLeft(peak)), ok(composite.pullPoint(peak)))
    assert(!link.supports(LinkDirection.LeftToRight))
    link.toRight(ok(Point.in(native)(0.0, 0.0, 0.0))) match
      case Left(WorldLinkError.DirectionUnavailable(LinkDirection.LeftToRight, description)) =>
        assertEquals(description, composite.provenance.describe)
      case other => fail(s"expected the forward direction to be unavailable, got $other")
