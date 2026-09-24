package scalafim.transform.freesurfer

import image4s.geometry.{Affine, D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, FreeSurferVolumeGeometry, WorldSpace}
import scalafim.transform.*
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** Every FreeSurfer linear format encoding one transform must yield the same pullback (see oracle manifest). */
class FreeSurferLinearOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val root = "freesurfer_linear"
  private def text(name: String) = TransformSource.Text(OracleFixtures.text(s"$root/$name"))

  private val movable: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("movable")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("reference")))
  private val frames = Frames[movable.type, reference.type](movable, reference)

  private def geometry(name: String): FreeSurferVolumeGeometry =
    val raw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"$root/$name"))))
    FreeSurferVolumeGeometry(raw.spatialShape, ok(Affine.fromRowMajor[D3](raw.sformRowMajor)))

  private val points = OracleTable.load(s"$root/points.tsv")

  private def assertPullback(transform: WorldTransform[movable.type, reference.type], tol: Double)(using munit.Location): Unit =
    points.rows.foreach: row =>
      val p = row.take(3)
      val expected = row.slice(3, 6)
      val pulled = ok(transform.pullPoint(ok(Point.fromVector(reference, p)).asInstanceOf[Point[reference.type, D3]])).coordinates
      pulled.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, tol, s"at $p"))

  test("LTA RAS_TO_RAS, VOX_TO_VOX, REGISTER_DAT and FSLREG files all encode the same pullback"):
    Vector("ras2ras.lta" -> 1e-9, "vox2vox.lta" -> 1e-5, "registerdat.lta" -> 1e-5, "fslreg.lta" -> 1e-5).foreach: (name, tol) =>
      val lta = ok(LtaCodec.decode(text(name)))
      assertEquals(lta.source.dims, Vector(40, 48, 36), name)
      assertEquals(lta.destination.dims, Vector(64, 56, 50), name)
      assertPullback(ok(LtaInterpretation.interpret(lta, frames)), tol)

  test("LTA volume geometry reproduces the NIfTI voxel-to-world of each volume"):
    val lta = ok(LtaCodec.decode(text("ras2ras.lta")))
    ok(lta.source.voxelToRas).rowMajor.zip(geometry("movable.nii").norig.rowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-5))
    ok(lta.destination.voxelToRas).rowMajor.zip(geometry("reference.nii").norig.rowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-5))

  test("register.dat and talairach.xfm give the same pullback"):
    val grids = TkRegGrids[movable.type, reference.type](movable, geometry("movable.nii"), reference, geometry("reference.nii"))
    assertPullback(ok(RegisterDatInterpretation.interpret(ok(RegisterDatCodec.decode(text("register.dat"))), grids)), 1e-5)
    assertPullback(ok(MniXfmInterpretation.interpret(ok(MniXfmCodec.decode(text("talairach.xfm"))), frames)), 1e-9)

  test("CORONAL_RAS_TO_CORONAL_RAS and unknown LTA types are refused, never guessed"):
    val coronal = ok(LtaCodec.decode(text("coronal.lta")))
    assertEquals(coronal.kind, LtaKind.CoronalRasToCoronalRas)
    LtaInterpretation.interpret(coronal, frames) match
      case Left(TransformError.Io(TransformIoError.UnsupportedLtaType(21, _))) => ()
      case other                                                               => fail(s"expected a typed refusal, got $other")
    assertEquals(LtaKind.fromCode(99), LtaKind.Other(99))

  test("expression writes RAS_TO_RAS LTAs that read back to the same pullback, and every codec round-trips"):
    val lta = ok(LtaCodec.decode(text("vox2vox.lta")))
    val transform = ok(LtaInterpretation.interpret(lta, frames))
    val written = ok(LtaExpression.express(transform, LtaGeometry(movable, lta.source, reference, lta.destination)))
    assertEquals(written.kind, LtaKind.RasToRas)
    assertPullback(ok(LtaInterpretation.interpret(ok(LtaCodec.decode(ok(LtaCodec.encode(written)))), frames)), 1e-5)
    Vector("ras2ras.lta", "vox2vox.lta", "registerdat.lta", "fslreg.lta", "coronal.lta").foreach: name =>
      val file = ok(LtaCodec.decode(text(name)))
      assertEquals(ok(LtaCodec.decode(ok(LtaCodec.encode(file)))), file, name)
    val xfm = ok(MniXfmCodec.decode(text("talairach.xfm")))
    assertEquals(ok(MniXfmCodec.decode(ok(MniXfmCodec.encode(xfm)))), xfm)
    val dat = ok(RegisterDatCodec.decode(text("register.dat")))
    assertEquals(ok(RegisterDatCodec.decode(ok(RegisterDatCodec.encode(dat)))), dat)
    assertEquals(dat.flags, Vector("round"))

  test("malformed FreeSurfer files are typed failures"):
    assert(LtaCodec.decode(TransformSource.Text("type = 1\nnxforms = 2\n")).isLeft)
    assert(MniXfmCodec.decode(TransformSource.Text("MNI Transform File\nTransform_Type = Grid_Transform;\n")).isLeft)
    assert(RegisterDatCodec.decode(TransformSource.Text("bert\n1\n1\n")).isLeft)
