package scalafim.transform.field

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** AFNI 3dQwarp `_WARP` fields against nitransforms' AFNI reader (cross-implementation; see oracle/afni_qwarp).
  *
  * The read fields have a cardinal sform (code 2) and an oblique qform (code 1). nitransforms and AFNI place the lattice
  * with the sform (AFNI through its cardinalised grid, which equals a cardinal sform), ITK with the qform, so these rows
  * pin both the LPS displacement sign and AFNI's affine choice. An oblique field is refused: AFNI's warp code places it
  * on cardinalised axes, which no oracle pins yet. A native AFNI check (3dNwarpXYZ) is still pending.
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

  /** Off-grid rows: nitransforms' cubic B-spline differs from the analytic affine field by up to 1.1e-5 mm. */
  private val tolerance = Map("affine_WARP.nii" -> 2e-5, "smooth_WARP.nii" -> 1e-5)

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

  test("ITK would place the same file with its qform: the affine choice, not the displacement sign, separates ANTs and AFNI"):
    val field = ok(VectorFieldNiftiCodec.decode(bytes("smooth_WARP.nii")))
    assertEquals((field.raw.qformCode, field.raw.sformCode), (1, 2))
    val afni = ok(LatticeAffine.of(field.raw, LatticeAffine.AfniCardinal)).rowMajor
    val itk = ok(LatticeAffine.of(field.raw, LatticeAffine.Itk)).rowMajor
    afni.zip(field.raw.sformRowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
    itk.zip(field.raw.qformRowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))
    assert(afni.zip(itk).exists((a, b) => math.abs(a - b) > 1.0), "the two placements must differ materially")

  test("an oblique _WARP field is refused as an unqualified convention, never placed on its oblique sform"):
    val field = ok(VectorFieldNiftiCodec.decode(bytes("oblique_WARP.nii")))
    LpsDisplacementInterpretation.AfniQwarp.interpret(field, context) match
      case Left(TransformError.UnqualifiedConvention(TransformFormat.AfniQwarp, reason)) => assert(reason.contains("cardinal"), reason)
      case other                                                                      => fail(s"expected an unqualified-convention refusal, got $other")
    assert(LpsDisplacementInterpretation.Ants.interpret(field, context).isRight, "the same bytes remain readable as an ITK field")
