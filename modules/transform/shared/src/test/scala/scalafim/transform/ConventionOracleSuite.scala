package scalafim.transform

import image4s.geometry.{Affine, D3}
import scalafim.image.world.{FreeSurferVolumeGeometry, FslAffineSource, FslVolumeGeometry, ToolCoordinates}
import scalafim.transform.oracle.OracleTable

/** Convention kernel against reference implementations (fslpy, nibabel); see oracle/conventions/manifest.json. */
class ConventionOracleSuite extends munit.FunSuite:
  private def m16(row: Vector[Double], table: OracleTable, prefix: String): Vector[Double] =
    table.block(row, s"${prefix}00", 16)

  private def affine(values: Vector[Double]): Affine[D3] =
    Affine.fromRowMajor[D3](values).fold(e => fail(e.message), identity)

  private def assertMatrix(actual: Affine[D3], expected: Vector[Double], tol: Double)(using munit.Location): Unit =
    actual.rowMajor.zip(expected).zipWithIndex.foreach { case ((a, e), i) => assertEqualsDouble(a, e, tol, s"element $i") }

  private val fslCases = OracleTable.load("conventions/fsl_cases.tsv")

  private def geometry(row: Vector[Double]): FslVolumeGeometry =
    def col(name: String) = row(fslCases.header.indexOf(name))
    val q = Option.when(col("qcode") != 0)(affine(m16(row, fslCases, "qform")))
    val s = Option.when(col("scode") != 0)(affine(m16(row, fslCases, "sform")))
    FslVolumeGeometry
      .fromHeader(Vector(col("nx"), col("ny"), col("nz")).map(_.toInt), Vector(col("px"), col("py"), col("pz")), col("qcode").toInt, q, col("scode").toInt, s)
      .fold(e => fail(e.message), identity)

  test("FSL affine selection and handedness match fslpy when qform and sform disagree"):
    fslCases.rows.foreach: row =>
      val g = geometry(row)
      val expectedSource = row(fslCases.header.indexOf("selected")).toInt match
        case 2 => FslAffineSource.Sform
        case 1 => FslAffineSource.Qform
        case _ => FslAffineSource.Scaling
      assertEquals(g.selected, expectedSource, s"case ${row.head}")
      assertEquals(g.neurological, row(fslCases.header.indexOf("neurological")) == 1.0, s"case ${row.head}")
      assertMatrix(g.voxelToWorld, m16(row, fslCases, "v2w"), 1e-5)
      assertMatrix(g.voxelToFsl, m16(row, fslCases, "v2f"), 1e-5)

  test("FLIRT matrices convert to world->world exactly as fslpy.fromFlirt does"):
    val pairs = OracleTable.load("conventions/flirt_pairs.tsv")
    val byCase = fslCases.rows.map(row => row.head.toInt -> geometry(row)).toMap
    pairs.rows.foreach: row =>
      val (src, ref) = (byCase(row(0).toInt), byCase(row(1).toInt))
      val flirt = affine(m16(row, pairs, "flirt"))
      // world(src) -> fsl(src) -> FLIRT -> fsl(ref) -> world(ref)
      val world = Vector(ToolCoordinates.fromRas(ToolCoordinates.FslScaledVoxel(src)), flirt, ToolCoordinates.toRas(ToolCoordinates.FslScaledVoxel(ref)))
        .reduce((a, b) => a.andThen(b).fold(e => fail(e.message), identity))
      assertMatrix(world, m16(row, pairs, "world"), 1e-5)

  test("FreeSurfer Norig/Torig of conformed volumes match nibabel, and tkRAS maps to scanner by c_ras"):
    val table = OracleTable.load("conventions/freesurfer_conformed.tsv")
    table.rows.foreach: row =>
      def col(name: String) = row(table.header.indexOf(name))
      val fs = FreeSurferVolumeGeometry(Vector(col("nx"), col("ny"), col("nz")).map(_.toInt), affine(m16(row, table, "norig")))
      assertMatrix(fs.torig, m16(row, table, "torig"), 1e-4) // nibabel's Torig is float32
      val shift = fs.tkrToScanner.rowMajor
      Vector(3, 7, 11).zip(Vector("cr", "ca", "cs")).foreach((i, c) => assertEqualsDouble(shift(i), col(c), 1e-5)) // MGH stores Pxyz_c as float32
