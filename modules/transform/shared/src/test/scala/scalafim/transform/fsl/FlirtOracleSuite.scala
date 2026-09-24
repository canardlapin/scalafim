package scalafim.transform.fsl

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, FslVolumeGeometry, WorldSpace}
import scalafim.transform.*
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** FLIRT matrices against fslpy's world<->world conversion, on volumes whose qform and sform disagree. */
class FlirtOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def geometry(index: Int): FslVolumeGeometry =
    ok(FslHeaderGeometry(ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"conventions/fsl_case$index.nii"))))))

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("flirt input")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("flirt reference")))

  private val pairs = OracleTable.load("conventions/flirt_pairs.tsv")

  test("FLIRT matrices read from disk map points exactly as fslpy's world->world conversion"):
    pairs.rows.foreach: row =>
      val (src, ref) = (row(0).toInt, row(1).toInt)
      val grids = FslGrids[source.type, reference.type](source, geometry(src), reference, geometry(ref))
      val matrix = ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"conventions/flirt_${src}_to_$ref.mat"))))
      val transform = ok(FlirtInterpretation.interpret(matrix, grids))
      val world = pairs.block(row, "world00", 16) // fslpy: source world -> reference world
      Vector(Vector(10.0, -20.0, 30.0), Vector(-5.5, 3.25, 0.0), Vector(0.0, 0.0, 0.0), Vector(40.0, 12.0, -8.0)).foreach: p =>
        val expected = Vector.tabulate(3)(r => world(4 * r) * p(0) + world(4 * r + 1) * p(1) + world(4 * r + 2) * p(2) + world(4 * r + 3))
        val mapped = ok(transform.mapPoint(ok(Point.fromVector(source, p)).asInstanceOf[Point[source.type, D3]])).coordinates
        // FLIRT files are written with 10 decimals, so agreement is at the 1e-6 mm level.
        mapped.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, 1e-6, s"$src->$ref at $p"))

  test("expressing a FLIRT transform reproduces the stored matrix, and encoding round-trips"):
    pairs.rows.foreach: row =>
      val (src, ref) = (row(0).toInt, row(1).toInt)
      val grids = FslGrids[source.type, reference.type](source, geometry(src), reference, geometry(ref))
      val matrix = ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"conventions/flirt_${src}_to_$ref.mat"))))
      val again = ok(FlirtExpression.express(ok(FlirtInterpretation.interpret(matrix, grids)), grids))
      again.rowMajor.zip(matrix.rowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-9))
      assertEquals(ok(FlirtCodec.decode(ok(FlirtCodec.encode(matrix)))), matrix)

  test("malformed FLIRT text is a typed failure"):
    assert(FlirtCodec.decode(TransformSource.Text("1 0 0\n0 1 0\n0 0 1\n")).isLeft)
    assert(FlirtCodec.decode(TransformSource.Text("1 0 0 0\n0 1 0 0\n0 0 1 0\n0 0 2 1\n")).isLeft)
