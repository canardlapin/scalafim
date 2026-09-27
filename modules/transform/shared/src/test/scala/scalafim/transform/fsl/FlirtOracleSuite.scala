package scalafim.transform.fsl

import image4s.geometry.{D3, Frame, Point}
import scalafim.image.world.{FrameCatalog, FslAffineSource, FslVolumeGeometry, WorldSpace}
import scalafim.transform.*
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** FLIRT matrices against fslpy's world<->world conversion (reference implementation), on volumes whose qform and
  * sform disagree, including a fresh case whose source and reference grids differ in shape, spacing and handedness.
  */
class FlirtOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def geometry(index: Int): FslVolumeGeometry =
    ok(FslHeaderGeometry(ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"conventions/fsl_case$index.nii"))))))

  private val source: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("flirt input")))
  private val reference: Frame[D3] = FrameCatalog.frame(ok(WorldSpace.declare("flirt reference")))

  private val pairs = OracleTable.load("conventions/flirt_pairs.tsv")

  private val points = OracleTable.load("conventions/flirt_points.tsv")

  private def raw(name: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"conventions/$name"))))

  /** key -> (matrix file, source geometry, reference geometry) for the five shared-grid pairs and the fresh case. */
  private def files(key: String): (String, FslVolumeGeometry, FslVolumeGeometry) =
    if key == "fresh" then ("flirt_fresh.mat", ok(FslHeaderGeometry(raw("flirt_fresh_src.nii"))), ok(FslHeaderGeometry(raw("flirt_fresh_ref.nii"))))
    else
      val Array(src, ref) = key.split("_to_").map(_.toInt)
      (s"flirt_${src}_to_$ref.mat", geometry(src), geometry(ref))

  test("FLIRT matrices read from disk map ten asymmetric points per case exactly as fslpy's world->world conversion"):
    val keys = points.keys.distinct
    assertEquals(keys.toSet, pairs.rows.map(row => s"${row(0).toInt}_to_${row(1).toInt}").toSet + "fresh")
    keys.foreach: key =>
      val (mat, src, ref) = files(key)
      val grids = FslGrids[source.type, reference.type](source, src, reference, ref)
      val transform = ok(FlirtInterpretation.interpret(ok(FlirtCodec.decode(TransformSource.Text(OracleFixtures.text(s"conventions/$mat")))), grids))
      val rows = points.keyed.filter(_._1 == key)
      assert(rows.size >= 8 && rows.forall(_._2.take(3).exists(_ != 0.0)), s"$key needs at least eight points off the origin")
      rows.foreach: (_, row) =>
        val mapped = ok(transform.mapPoint(ok(Point.fromVector(source, row.take(3))).asInstanceOf[Point[source.type, D3]])).coordinates
        // FLIRT files are written with 10 (fresh: 12) decimals, so agreement is at the 1e-6 mm level.
        mapped.zip(row.slice(3, 6)).foreach((a, e) => assertEqualsDouble(a, e, 1e-6, s"$key at ${row.take(3)}"))

  test("the fresh case selects the sform of a neurological source and the qform of a radiological reference, as fslpy does"):
    val fresh = OracleTable.load("conventions/flirt_fresh.tsv")
    fresh.keyed.foreach: (key, row) =>
      val g = ok(FslHeaderGeometry(raw(s"flirt_fresh_$key.nii")))
      assertEquals(g.selected, if row(fresh.index("selected")) == 2.0 then FslAffineSource.Sform else FslAffineSource.Qform, key)
      assertEquals(g.neurological, row(fresh.index("neurological")) == 1.0, key)
      g.voxelToWorld.rowMajor.zip(fresh.block(row, "v2w00", 16)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$key voxel->world"))
      g.voxelToFsl.rowMajor.zip(fresh.block(row, "v2f00", 16)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$key voxel->fsl"))
    assertEquals(fresh.keys, Vector("src", "ref"))

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
