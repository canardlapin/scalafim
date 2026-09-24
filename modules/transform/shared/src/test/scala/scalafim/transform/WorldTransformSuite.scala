package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Point}
import reframe4s.core.{MapError, SpatialMap}
import reframe4s.lie.FramedAffine
import scalafim.image.world.{FrameCatalog, Spaces, WorldSpace}

class WorldTransformSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val native: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("native T1w")))
  private val mni: Frame[D3] = Spaces.MNI152NLin2009cAsym
  private val mni6: Frame[D3] = Spaces.MNI152NLin6Asym

  private def affine(values: Double*): Affine[D3] = ok(Affine.fromRowMajor[D3](values.toVector))

  // pullbacks: MNI -> native and MNI6 -> MNI (sheared, oblique, translated)
  private val mniToNative = affine(1.1, 0.05, -0.02, 3.0, -0.03, 0.95, 0.1, -12.5, 0.02, -0.08, 1.2, 7.25, 0, 0, 0, 1)
  private val mni6ToMni = affine(0.99, 0.01, 0.0, -0.5, 0.0, 1.01, 0.02, 1.25, 0.01, 0.0, 0.98, -2.0, 0, 0, 0, 1)

  private val nativeToMni =
    WorldTransform.Linear(FramedAffine.betweenFrames[mni.type, native.type, D3](mni, native)(mniToNative), TransformProvenance.constructed("test registration"))
  private val mniToMni6 =
    WorldTransform.Linear(FramedAffine.betweenFrames[mni6.type, mni.type, D3](mni6, mni)(mni6ToMni), TransformProvenance.constructed("template bridge"))

  private def close(a: Point[?, D3], b: Point[?, D3], tol: Double = 1e-9)(using munit.Location): Unit =
    a.coordinates.zip(b.coordinates).foreach((x, y) => assertEqualsDouble(x, y, tol))

  test("pullPoint and mapPoint are mutually inverse for affines"):
    val p = ok(Point.in(mni)(10.0, -20.0, 30.0))
    val back = ok(nativeToMni.mapPoint(ok(nativeToMni.pullPoint(p))))
    close(back, p)

  test("the affine inverse swaps directions exactly"):
    val inverse = nativeToMni.inverse
    val q = ok(Point.in(native)(-4.0, 5.5, 12.0))
    close(ok(inverse.pullPoint(q)), ok(nativeToMni.mapPoint(q)))

  test("two affines fuse into one affine equal to their lazy composition"):
    val fused = ok(nativeToMni.andThen(mniToMni6))
    val lazyChain: WorldTransform[native.type, mni6.type] = (nativeToMni: WorldTransform[native.type, mni.type]).andThen(mniToMni6)
    val p = ok(Point.in(mni6)(1.0, 2.0, 3.0))
    close(ok(fused.pullPoint(p)), ok(lazyChain.pullPoint(p)))
    val q = ok(Point.in(native)(7.0, -8.0, 9.0))
    close(ok(fused.mapPoint(q)), ok(lazyChain.mapPoint(q)))
    assert(fused.provenance.describe.contains("fused affine composition"))

  test("a dense-style map without an inverse has no forward direction"):
    val shift = new SpatialMap[mni.type, native.type, D3]:
      val source: mni.type = mni
      val target: native.type = native
      def apply(point: Point[mni.type, D3]): Either[MapError, Point[native.type, D3]] =
        val c = point.coordinates
        Point.fromVector(native, Vector(c(0) + math.sin(c(1) / 10.0), c(1), c(2))).left.map(MapError.Geometry(_)).map(_.asInstanceOf[Point[native.type, D3]])
    val warp = WorldTransform.Mapped(shift, PushAvailability.Unavailable(), TransformProvenance.constructed("synthetic warp"))
    val p = ok(Point.in(mni)(1.0, 5.0, 2.0))
    assertEqualsDouble(ok(warp.pullPoint(p)).coordinates(0), 1.0 + math.sin(0.5), 1e-12)
    assert(warp.mapPoint(ok(Point.in(native)(0.0, 0.0, 0.0))).left.exists(_.isInstanceOf[TransformError.NoForwardMap]))
    assert(warp.invert.isLeft)
    // composing with an affine keeps the missing forward direction honest
    val chained = warp.andThen(mniToMni6)
    assert(chained.push.isEmpty)
    assert(chained.pullPoint(ok(Point.in(mni6)(0.0, 1.0, 2.0))).isRight)

  test("transforms between mismatched spaces do not compose"):
    val errors = compileErrors("mniToMni6.andThen(nativeToMni)")
    assert(errors.nonEmpty, "MNI6-target transform must not compose with a native-source transform")
