package scalafim.transform

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.afni.{Aff12Codec, Aff12Interpretation}
import scalafim.transform.fsl.{FlirtCodec, FlirtInterpretation, FslHeaderGeometry}
import scalafim.transform.itk.{ItkLinearInterpretation, ItkTextCodec}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** P4.05: ScalaFIM and nitransforms (cross-implementation) agree on ITK, FLIRT and AFNI files they both read.
  * nitransforms reads only plain ITK AffineTransform files; the other ITK parameterisations are covered by the native
  * SimpleITK oracle instead (see the manifest).
  */
class NitransformsCrosscheckSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("moving")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("reference")))
  private val frames = Frames[moving.type, reference.type](moving, reference)
  private val table = OracleTable.load("nitransforms_crosscheck/points.tsv")

  private def fsl(index: Int) =
    ok(FslHeaderGeometry(ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"conventions/fsl_case$index.nii"))))))

  private def transformFor(key: String): WorldTransform[moving.type, reference.type] =
    val text = TransformSource.Text(OracleFixtures.text(key))
    if key.startsWith("itk_linear/") then ok(ItkLinearInterpretation.interpret(ok(ItkTextCodec.decode(text)), frames)).composed
    else if key.startsWith("conventions/flirt_") then
      val Array(src, ref) = key.stripPrefix("conventions/flirt_").stripSuffix(".mat").split("_to_").map(_.toInt)
      ok(FlirtInterpretation.interpret(ok(FlirtCodec.decode(text)), FslGrids(moving, fsl(src), reference, fsl(ref))))
    else ok(Aff12Interpretation.interpret(ok(Aff12Codec.decode(text)), AfniContext(frames, CardinalCorrection.Off))).transforms.head

  /** fsl_case4 has neither qform nor sform. FSL (fslpy) and image4s place such a volume with pure pixdim scaling at
    * the origin, and ScalaFIM follows them (see ConventionOracleSuite); nibabel, and so nitransforms, centres it
    * instead. The world of that volume therefore differs by definition, not by a FLIRT convention error.
    */
  private val worldUndefinedInNibabel = Set("conventions/flirt_4_to_2.mat")

  test("every file nitransforms can read maps points as ScalaFIM does"):
    val keys = table.keys.distinct.filterNot(worldUndefinedInNibabel)
    assert(keys.exists(_.startsWith("itk_linear/")) && keys.exists(_.startsWith("conventions/flirt_")) && keys.exists(_.contains("afni")))
    keys.foreach: key =>
      val transform = transformFor(key)
      table.keyed.filter(_._1 == key).foreach: (_, row) =>
        val pulled = ok(transform.pullPoint(ok(Point.fromVector(reference, row.take(3))))).coordinates
        pulled.zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$key at ${row.take(3)}"))
