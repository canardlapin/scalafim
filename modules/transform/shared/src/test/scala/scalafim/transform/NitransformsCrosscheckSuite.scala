package scalafim.transform

import image4s.geometry.{Affine, D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.afni.{Aff12Codec, Aff12Interpretation, AfniCardinal}
import scalafim.transform.freesurfer.{LtaCodec, LtaInterpretation}
import scalafim.transform.fsl.{FlirtCodec, FlirtInterpretation, FslHeaderGeometry}
import scalafim.transform.itk.{ItkLinearInterpretation, ItkTextCodec}
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** P4.05, P3.12: ScalaFIM and nitransforms (cross-implementation) agree on ITK, FLIRT, AFNI and FreeSurfer LTA files
  * they both read. nitransforms reads only plain ITK AffineTransform files; the other ITK parameterisations are covered
  * by the native SimpleITK oracle instead (see the manifest). A `#oblique` key reads the generic AFNI matrix against
  * oblique base and source images, exercising AFNI's cardinal/real correction as nitransforms implements it.
  */
class NitransformsCrosscheckSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("moving")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("reference")))
  private val frames = Frames[moving.type, reference.type](moving, reference)
  private val table = OracleTable.load("nitransforms_crosscheck/points.tsv")

  private def raw(path: String) = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))))
  private def fsl(name: String) = ok(FslHeaderGeometry(raw(s"conventions/$name.nii")))

  private def obliquity(name: String) =
    AfniCardinal.obliquity(ok(Affine.fromRowMajor[D3](raw(s"nitransforms_crosscheck/$name").sformRowMajor)))

  private def transformFor(key: String): WorldTransform[moving.type, reference.type] =
    val path = key.takeWhile(_ != '#')
    val text = TransformSource.Text(OracleFixtures.text(path))
    if key.startsWith("itk_linear/") then ok(ItkLinearInterpretation.interpret(ok(ItkTextCodec.decode(text)), frames)).composed
    else if key.startsWith("conventions/flirt_") then
      val Array(src, ref) = key.stripPrefix("conventions/flirt_").stripSuffix(".mat") match
        case "fresh" => Array("flirt_fresh_src", "flirt_fresh_ref")
        case pair    => pair.split("_to_").map(i => s"fsl_case$i")
      ok(FlirtInterpretation.interpret(ok(FlirtCodec.decode(text)), FslGrids(moving, fsl(src), reference, fsl(ref))))
    else if key.startsWith("freesurfer_linear/") then ok(LtaInterpretation.interpret(ok(LtaCodec.decode(text)), frames))
    else
      val correction =
        if key.endsWith("#oblique") then
          val (source, base) = (obliquity("afni_oblique_source.nii"), obliquity("afni_oblique_base.nii"))
          assert(source.nonEmpty && base.nonEmpty, "the #oblique images must both be oblique")
          CardinalCorrection.On(source, base)
        else CardinalCorrection.Off
      ok(Aff12Interpretation.interpret(ok(Aff12Codec.decode(text)), AfniContext(frames, correction))).transforms.head

  /** fsl_case4 has neither qform nor sform. FSL (fslpy) and image4s place such a volume with pure pixdim scaling at
    * the origin, and ScalaFIM follows them (see ConventionOracleSuite); nibabel, and so nitransforms, centres it
    * instead. The world of that volume therefore differs by definition, not by a FLIRT convention error.
    */
  private val worldUndefinedInNibabel = Set("conventions/flirt_4_to_2.mat")

  test("every file nitransforms can read maps points as ScalaFIM does"):
    val keys = table.keys.distinct.filterNot(worldUndefinedInNibabel)
    val expected = Vector(
      "itk_linear/affine.tfm",
      "conventions/flirt_fresh.mat",
      "neurotransform/afni_oracle/oracle.aff12.1D",
      "nitransforms_crosscheck/afni_generic.aff12.1D",
      "nitransforms_crosscheck/afni_generic.aff12.1D#oblique",
      "freesurfer_linear/ras2ras.lta",
      "freesurfer_linear/vox2vox.lta"
    )
    expected.foreach(key => assert(keys.contains(key), s"missing cross-check rows for $key"))
    keys.foreach(key => assert(table.keys.count(_ == key) >= 8, s"$key needs at least eight points"))
    keys.foreach: key =>
      val transform = transformFor(key)
      table.keyed.filter(_._1 == key).foreach: (_, row) =>
        val pulled = ok(transform.pullPoint(ok(Point.fromVector(reference, row.take(3))))).coordinates
        pulled.zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$key at ${row.take(3)}"))

  test("oblique base and source images change the generic AFNI pullback, so the cardinal correction is exercised"):
    val plain = table.keyed.filter(_._1 == "nitransforms_crosscheck/afni_generic.aff12.1D").map(_._2)
    val oblique = table.keyed.filter(_._1 == "nitransforms_crosscheck/afni_generic.aff12.1D#oblique").map(_._2)
    assert(plain.size >= 8, "the generic AFNI matrix needs at least eight rows")
    assertEquals(plain.map(_.take(3)), oblique.map(_.take(3)))
    assert(plain.zip(oblique).forall((a, b) => a.slice(3, 6).zip(b.slice(3, 6)).exists((x, y) => math.abs(x - y) > 1e-2)))
