package scalafim.transform.nifti

import scalafim.transform.TransformIoError
import scalafim.transform.oracle.{OracleFixtures, OracleTable}

/** The raw NIfTI-1 transform-container reader against nibabel, for every NIfTI oracle file, on both platforms. */
class NiftiRawSuite extends munit.FunSuite:
  private val table = OracleTable.load("nifti_headers/headers.tsv")

  private def parse(path: String): NiftiRaw =
    NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(path))).fold(e => fail(s"$path: ${e.message}"), identity)

  test("the oracle covers every vendored NIfTI transform container"):
    assert(table.keys.size > 150, s"only ${table.keys.size} files")
    assert(table.keys.exists(_.endsWith("coef.nii.gz")))
    assert(table.keys.exists(_.endsWith("warp.nii.gz")))

  test("header fields, affines and scaled data agree with nibabel"):
    table.keyed.foreach: (path, row) =>
      val raw = parse(path)
      def col(name: String) = row(table.index(name))
      val clue = s"$path"
      assertEquals(raw.dim, Vector.tabulate(8)(i => col(s"dim$i").toInt), clue)
      assertEquals(raw.intentCode, col("intent").toInt, clue)
      assertEquals(raw.datatype, col("datatype").toInt, clue)
      assertEquals(raw.qformCode, col("qcode").toInt, clue)
      assertEquals(raw.sformCode, col("scode").toInt, clue)
      Vector("pixdim0", "pixdim1", "pixdim2", "pixdim3").zipWithIndex.foreach((name, i) => assertEquals(raw.pixdim(i), col(name), clue))
      assertEquals(raw.intentP, Vector(col("p1"), col("p2"), col("p3")), clue)
      if raw.qformCode > 0 then
        raw.qformRowMajor.zip(table.block(row, "qform00", 16)).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, clue))
      if raw.sformCode > 0 then
        raw.sformRowMajor.zip(table.block(row, "sform00", 16)).foreach((a, e) => assertEqualsDouble(a, e, 0.0, clue))
      val n = math.min(4L, raw.voxelCount).toInt
      (0 until n).foreach(i => assertEqualsDouble(raw.value(i.toLong), col(s"v$i"), 0.0, clue))
      var sum = 0.0
      var i = 0L
      while i < raw.voxelCount do
        sum += raw.value(i)
        i += 1
      assertEqualsDouble(sum, col("sum"), 1e-9 * math.max(1.0, math.abs(col("sum"))) + 1e-6, clue)

  test("non-NIfTI and truncated input is a typed failure"):
    assert(NiftiRaw.parse(IArray.fill[Byte](10)(0)).isLeft)
    val truncated = OracleFixtures.decoded("neurotransform/itk_oracle/warp.nii.gz").take(400)
    NiftiRaw.parse(IArray.unsafeFromArray(truncated)) match
      case Left(TransformIoError.UnsupportedNifti(_)) => ()
      case other                                      => fail(s"expected a typed refusal, got $other")
