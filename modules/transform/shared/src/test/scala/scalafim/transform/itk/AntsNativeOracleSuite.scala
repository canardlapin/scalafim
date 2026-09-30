package scalafim.transform.itk

import image4s.geometry.{D3, Frame, Point}
import reframe4s.field.CoordinateBoundaryPolicy
import scalafim.image.world.{FrameCatalog, WorldSpace}
import scalafim.transform.*
import scalafim.transform.field.{DenseContext, LpsDisplacementInterpretation, VectorFieldNiftiCodec}
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** ANTs 2.6 CLI output on asymmetric points, including native 5D SyN fields and inverse composites. */
class AntsNativeOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A])(using munit.Location): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("ANTs oracle source")))
  private val target: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("ANTs oracle target")))
  private val frames = Frames[source.type, target.type](source, target)
  private val context = DenseContext(frames, CoordinateBoundaryPolicy.PreserveSource)
  private lazy val points = OracleTable.load("ants_native/points.tsv")
  private val linear = Vector("affine", "euler", "euler_zyx", "versor_rigid", "similarity", "scale_skew_versor")
  private val fields = Vector("syn_0Warp", "syn_0InverseWarp")
  private val composites = Vector("syn_Composite", "syn_InverseComposite")

  private def lps(point: Vector[Double]): Vector[Double] = Vector(-point(0), -point(1), point(2))

  private def matches(key: String, transform: WorldTransform[source.type, target.type])(using munit.Location): Unit =
    val rows = points.keyed.filter(_._1 == key)
    assertEquals(rows.size, 12, key)
    rows.foreach: (_, row) =>
      val input = lps(row.take(3))
      val actual = ok(transform.pullPoint(ok(Point.in(target)(input(0), input(1), input(2)))))
      // ANTs serializes CSV output with six significant digits. Bound that rounding per coordinate,
      // plus 5e-6 mm for its float32 point input and NIfTI storage; never treat CSV as full precision.
      lps(actual.coordinates).zip(row.drop(3)).foreach: (a, e) =>
        val csvRounding = if e == 0.0 then 0.0 else 0.5 * math.pow(10.0, math.floor(math.log10(math.abs(e))) - 5.0)
        assertEqualsDouble(a, e, csvRounding + 5e-6, s"$key at $input")

  test("the native manifest pins ANTs 2.6 and every fixture byte"):
    val manifest = OracleFixtures.text("ants_native/manifest.json")
    assert(manifest.contains("ANTs Version: 2.6."), manifest)
    assert(manifest.contains("\"kind\": \"native-oracle\""), manifest)
    assert(manifest.contains("antsApplyTransformsToPoints"), manifest)
    val section = manifest.substring(manifest.indexOf("\"sha256\""))
    val hashes = "\"([^\"]+)\":\\s*\"([0-9a-f]{64})\"".r.findAllMatchIn(section).map(m => m.group(1) -> m.group(2)).toVector
    assertEquals(hashes.size, 17, "every native fixture file must be recorded")
    hashes.foreach((name, digest) => assertEquals(OracleFixtures.sha256Hex(s"ants_native/$name"), digest, name))
    assertEquals(points.keys.distinct.toSet, (linear ++ fields ++ composites).toSet)

  test("every MATLAB parameterisation reproduces antsApplyTransformsToPoints"):
    linear.foreach: key =>
      val file = ok(ItkMatlabCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.bytes(s"ants_native/$key.mat")))))
      matches(key, ok(ItkLinearInterpretation.interpret(file, frames)).composed)

  test("ANTs-written 5D forward and inverse vector fields reproduce native point output"):
    fields.foreach: key =>
      val field = ok(VectorFieldNiftiCodec.decode(TransformSource.Binary(IArray.unsafeFromArray(OracleFixtures.decoded(s"ants_native/$key.nii.gz")))))
      assertEquals(field.raw.shape, Vector(13, 15, 17, 1, 3), key)
      assertEquals(field.raw.intentCode, 1007, key)
      matches(key, ok(LpsDisplacementInterpretation.Ants.interpret(field, context)))

  test("decoded Composite and InverseComposite reproduce native ANTs point output on both platforms"):
    composites.foreach: key =>
      val decoded = ItkHdf5Dumps.parse(OracleFixtures.text(s"ants_native/$key.components.txt"))
      assert(decoded.stages.exists(_.isDisplacementField), s"$key must exercise a nonlinear component")
      matches(key, ok(ItkHdf5Interpretation.interpret(decoded, context)).composed)
