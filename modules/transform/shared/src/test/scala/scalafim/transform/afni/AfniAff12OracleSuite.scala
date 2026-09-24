package scalafim.transform.afni

import image4s.geometry.{Affine, D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.oracle.OracleFixtures

/** AFNI affines against files AFNI 26.1.04 produced itself (neurotransform afni_oracle). */
class AfniAff12OracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("afni source")))
  private val base: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("afni base")))
  private val context = AfniContext(Frames[source.type, base.type](source, base), CardinalCorrection.Off)

  private def series(name: String): Aff12Series =
    ok(Aff12Codec.decode(TransformSource.Text(OracleFixtures.text(s"neurotransform/afni_oracle/$name"))))

  private def rows(name: String): Vector[Vector[Double]] =
    OracleFixtures.text(s"neurotransform/afni_oracle/$name").linesIterator.map(_.trim).filter(_.nonEmpty).map(_.split("\\s+").toVector.map(_.toDouble)).toVector

  private def ras(dicom: Vector[Double]): Vector[Double] = Vector(-dicom(0), -dicom(1), dicom(2))

  private val transform = ok(Aff12Interpretation.interpret(series("oracle.aff12.1D"), context)).transforms.head

  test("the pullback sends each base landmark to where AFNI placed it in the source (image oracle)"):
    val sourceByValue = rows("landmarks_source_dicom.txt").map(r => r(3) -> r.take(3)).toMap
    rows("landmarks_base_dicom.txt").foreach: r =>
      val baseXyz = r.slice(3, 6)
      val pulled = ok(transform.pullPoint(ok(Point.fromVector(base, ras(baseXyz))).asInstanceOf[Point[base.type, D3]])).coordinates
      ras(sourceByValue(r(6))).zip(pulled).foreach((e, a) => assertEqualsDouble(a, e, 1e-12, s"landmark value ${r(6)}"))

  test("the pullback agrees with AFNI Vecwarp point mapping"):
    rows("landmarks_base_dicom.txt").map(_.slice(3, 6)).zip(rows("vecwarp_source_dicom.1D")).foreach: (baseXyz, expected) =>
      val pulled = ok(transform.pullPoint(ok(Point.fromVector(base, ras(baseXyz))).asInstanceOf[Point[base.type, D3]])).coordinates
      ras(expected).zip(pulled).foreach((e, a) => assertEqualsDouble(a, e, 1e-9))

  test("our exact inverse equals AFNI's own cat_matvec -I"):
    val afniInverse = ok(Aff12Interpretation.interpret(series("oracle_inverse.aff12.1D"), AfniContext(Frames[base.type, source.type](base, source), CardinalCorrection.Off))).transforms.head
    transform.inverse.framed.operator.rowMajor.zip(afniInverse.framed.operator.rowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-12))

  test("a 3dvolreg series decodes to one transform per volume and round-trips value-exactly, negative zeros included"):
    val volreg = series("volreg_series.aff12.1D")
    assertEquals(volreg.rows.size, 4)
    assert(volreg.comments.exists(_.contains("3dvolreg")))
    assert(volreg.rows.head.exists(v => v == 0.0 && 1.0 / v < 0.0), "fixture has negative zeros")
    val again = ok(Aff12Codec.decode(ok(Aff12Codec.encode(volreg))))
    assertEquals(again.rows.map(_.map(java.lang.Double.doubleToRawLongBits)), volreg.rows.map(_.map(java.lang.Double.doubleToRawLongBits)))
    assertEquals(ok(Aff12Interpretation.interpret(volreg, context)).transforms.size, 4)

  test("expression inverts interpretation, with and without cardinal correction"):
    val oblique = ok(Affine.fromRowMajor[D3](Vector(0.95, -0.2, 0.05, -80.0, 0.21, 0.97, -0.03, -110.0, -0.04, 0.02, 2.5, -40.0, 0, 0, 0, 1)))
    val obliquity = AfniCardinal.obliquity(oblique)
    assert(obliquity.nonEmpty, "rotated grid must be oblique")
    assert(AfniCardinal.obliquity(ok(Affine.fromRowMajor[D3](Vector(-2.0, 0, 0, 90.0, 0, 2.0, 0, -126.0, 0, 0, 2.0, -72.0, 0, 0, 0, 1)))).isEmpty)
    Vector(CardinalCorrection.Off, CardinalCorrection.On(obliquity, obliquity)).foreach: correction =>
      val ctx = AfniContext(Frames[source.type, base.type](source, base), correction)
      val read = ok(Aff12Interpretation.interpret(series("oracle.aff12.1D"), ctx))
      val written = ok(Aff12Expression.express(read, ctx))
      written.rows.head.zip(series("oracle.aff12.1D").rows.head).foreach((a, e) => assertEqualsDouble(a, e, 1e-9, s"$correction"))

  test("malformed aff12 content is a typed failure"):
    assert(Aff12Codec.decode(TransformSource.Text("1 0 0 0 1 0\n")).isLeft)
    assert(Aff12Codec.decode(TransformSource.Text("# only a comment\n")).isLeft)
