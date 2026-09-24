package scalafim.transform.itk

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** ITK linear transforms against ITK's own TransformPoint (SimpleITK 2.5.6), on both platforms. */
class ItkLinearOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val moving: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("moving")))
  private val fixed: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("fixed")))
  private val frames = Frames[moving.type, fixed.type](moving, fixed)

  private val points = OracleTable.load("itk_linear/points.tsv")
  private val cases = points.keys.distinct

  private def text(name: String) = TransformSource.Text(OracleFixtures.text(s"itk_linear/$name.tfm"))
  private def binary(name: String) = TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.bytes(s"itk_linear/$name.mat")))

  private def lps(v: Vector[Double]): Vector[Double] = Vector(-v(0), -v(1), v(2))

  private def assertMatchesItk(name: String, transform: WorldTransform[moving.type, fixed.type])(using munit.Location): Unit =
    points.keyed.filter(_._1 == name).foreach: (_, row) =>
      val input = Vector(row(points.index("x")), row(points.index("y")), row(points.index("z")))
      val expected = Vector(row(points.index("tx")), row(points.index("ty")), row(points.index("tz")))
      // ITK maps fixed-space LPS points to moving-space LPS points: our pullback in RAS.
      val pulled = ok(transform.pullPoint(ok(Point.fromVector(fixed, lps(input))).asInstanceOf[Point[fixed.type, D3]]))
      lps(pulled.coordinates).zip(expected).zipWithIndex.foreach { case ((a, e), axis) => assertEqualsDouble(a, e, 1e-9, s"$name axis $axis at $input") }

  test("the oracle covers every linear parameterisation"):
    assertEquals(
      cases.toSet,
      Set("affine", "euler", "euler_zyx", "versor_rigid", "similarity", "scale_skew_versor", "translation", "composite_euler_affine")
    )
    assert(points.rows.size >= cases.size * 8)

  test("text files reproduce ITK TransformPoint for every parameterisation and the composite"):
    cases.foreach: name =>
      val file = ok(ItkTextCodec.decode(text(name)))
      assertMatchesItk(name, ok(ItkLinearInterpretation.interpret(file, frames)).composed)

  test("MATLAB v4 files decode to the same transforms as their text twins"):
    cases.filterNot(_.startsWith("composite")).foreach: name =>
      val fromMat = ok(ItkMatlabCodec.decode(binary(name)))
      val fromText = ok(ItkTextCodec.decode(text(name)))
      assertEquals(fromMat.entries.map(_.typeName), fromText.entries.map(_.typeName), name)
      fromMat.entries.zip(fromText.entries).foreach: (m, t) =>
        (m.parameters ++ m.fixedParameters).zip(t.parameters ++ t.fixedParameters).foreach((a, b) => assertEqualsDouble(a, b, 1e-15, name))
      assertMatchesItk(name, ok(ItkLinearInterpretation.interpret(fromMat, frames)).composed)

  test("encode then decode is value-exact for both codecs"):
    cases.foreach: name =>
      val file = ok(ItkTextCodec.decode(text(name)))
      assertEquals(ok(ItkTextCodec.decode(ok(ItkTextCodec.encode(file)))), file, name)
      if !file.isComposite then assertEquals(ok(ItkMatlabCodec.decode(ok(ItkMatlabCodec.encode(file)))), file, name)

  test("a linear world transform written as an ITK affine reads back to the same mapping"):
    cases.foreach: name =>
      val original = ok(ItkLinearInterpretation.linear(ok(ItkTextCodec.decode(text(name))), frames, AssetRef(name, None), TransformFormat.ItkText))
      val written = ok(ItkTextCodec.encode(ok(ItkLinearExpression.express(original, frames))))
      assertMatchesItk(name, ok(ItkLinearInterpretation.interpret(ok(ItkTextCodec.decode(written)), frames)).composed)

  test("the imported neurotransform ITK affine decodes identically from .tfm and .mat"):
    val tfm = ok(ItkTextCodec.decode(TransformSource.Text(OracleFixtures.text("neurotransform/itk_oracle/affine.tfm"))))
    val mat = ok(ItkMatlabCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.bytes("neurotransform/itk_oracle/affine.mat")))))
    assertEquals(mat.entries.map(_.kind), Vector("AffineTransform"))
    tfm.entries.head.parameters.zip(mat.entries.head.parameters).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))

  test("unsupported and malformed ITK content is a typed failure"):
    val bspline = "#Insight Transform File V1.0\n#Transform 0\nTransform: BSplineTransform_double_3_3\nParameters: 0 0\nFixedParameters: 0\n"
    val file = ok(ItkTextCodec.decode(TransformSource.Text(bspline)))
    ItkLinearInterpretation.interpret(file, frames) match
      case Left(TransformError.Io(TransformIoError.Unsupported(UnsupportedFormat.ItkBSpline, _))) => ()
      case other => fail(s"expected an explicit BSpline refusal, got $other")
    val short = "#Insight Transform File V1.0\nTransform: AffineTransform_double_3_3\nParameters: 1 0 0\nFixedParameters: 0 0 0\n"
    assert(ItkLinearInterpretation.interpret(ok(ItkTextCodec.decode(TransformSource.Text(short))), frames).isLeft)
    assert(ItkTextCodec.decode(TransformSource.Text("not an itk file")).isLeft)
    assert(ItkMatlabCodec.decode(TransformSource.Binary(IArray.fill[Byte](8)(0))).isLeft)
