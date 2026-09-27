package scalafim.transform.field

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** AFNI 3dQwarp `_WARP` fields against nitransforms' AFNI reader (cross-implementation; see oracle/afni_qwarp).
  *
  * The fields carry a qform that disagrees with an sform of code 2. AFNI's default and nitransforms place the lattice
  * with the sform, ITK with the qform, so these rows pin both the LPS displacement sign and AFNI's affine choice.
  * A native AFNI check (3dNwarpXYZ) is still pending.
  */
class AfniQwarpCrosscheckSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("qwarp source")))
  private val base: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("qwarp base")))
  private val context = DenseContext(Frames[source.type, base.type](source, base))
  private val table = OracleTable.load("afni_qwarp/points.tsv")

  private def bytes(name: String) = TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(s"afni_qwarp/$name")))

  private def pull(transform: WorldTransform[source.type, base.type], p: Vector[Double]): Vector[Double] =
    ok(transform.pullPoint(ok(Point.fromVector(base, p)).asInstanceOf[Point[base.type, D3]])).coordinates

  /** Off-grid rows: nitransforms' cubic B-spline differs from the analytic affine field by up to 1.2e-5 mm. */
  private val tolerance = Map("affine_WARP.nii" -> 5e-5, "smooth_WARP.nii" -> 1e-5)

  test("_WARP fields are detected as 3dQwarp and pull points as nitransforms' AFNI reader does"):
    assertEquals(table.keys.distinct.sorted, tolerance.keys.toVector.sorted)
    assert(table.keys.count(_ == "affine_WARP.nii") >= 12 && table.keys.count(_ == "smooth_WARP.nii") >= 8)
    tolerance.foreach: (name, tol) =>
      assertEquals(TransformDetection.detect(bytes(name), Some(name)), Right(TransformFormat.AfniQwarp), name)
      val warp = ok(Transforms.decode(bytes(name), TransformFormat.AfniQwarp)) match
        case NativeTransform.AfniQwarp(field) => ok(LpsDisplacementInterpretation.AfniQwarp.interpret(field, context))
        case other                           => fail(s"expected a 3dQwarp field, got $other")
      table.keyed.filter(_._1 == name).foreach: (_, row) =>
        pull(warp, row.take(3)).zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, tol, s"$name at ${row.take(3)}"))

  test("ITK's reading of the same file differs: the affine choice, not the displacement sign, separates ANTs and AFNI"):
    val field = ok(VectorFieldNiftiCodec.decode(bytes("smooth_WARP.nii")))
    assertEquals((field.raw.qformCode, field.raw.sformCode), (1, 2))
    val afni = ok(LatticeAffine.of(field.raw, LatticeAffine.SformFirst)).rowMajor
    val itk = ok(LatticeAffine.of(field.raw, LatticeAffine.Itk)).rowMajor
    afni.zip(field.raw.sformRowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
    itk.zip(field.raw.qformRowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
    val ants = ok(LpsDisplacementInterpretation.Ants.interpret(field, context))
    val (_, row) = table.keyed.filter(_._1 == "smooth_WARP.nii").head
    val moved = ants.pullPoint(ok(Point.fromVector(base, row.take(3))).asInstanceOf[Point[base.type, D3]])
    assert(moved.fold(_ => true, p => p.coordinates.zip(row.slice(3, 6)).exists((a, e) => math.abs(a - e) > 1e-3)), s"ITK placement unexpectedly agrees: $moved")
