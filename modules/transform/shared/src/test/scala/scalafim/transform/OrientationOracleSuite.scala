package scalafim.transform

import image4s.Continuous
import image4s.geometry.{Affine, D3}
import ravel.NDArray
import scalafim.image.{AxisCodes, Reorientation, SampleSpaces, SomeNeuroVolume}
import scalafim.image.SomeNeuroVolume.*
import scalafim.transform.nifti.NiftiRaw
import scalafim.transform.oracle.OracleFixtures

/** Axis codes and data-plus-affine reorientation against nibabel (aff2axcodes, as_reoriented). */
class OrientationOracleSuite extends munit.FunSuite:
  private def ok[E, A](result: Either[E, A]): A =
    result.fold(error => fail(s"unexpected failure: $error"), identity)

  private def raw(name: String): NiftiRaw = ok(NiftiRaw.parse(IArray.unsafeFromArray(OracleFixtures.decoded(s"orientation/$name"))))

  test("axis codes agree with nibabel aff2axcodes for permuted, flipped and oblique affines"):
    val lines = OracleFixtures.text("orientation/axcodes.tsv").linesIterator.drop(1).filter(_.nonEmpty).toVector
    assert(lines.size >= 9)
    lines.foreach: line =>
      val cells = line.split("\t")
      val affine = ok(Affine.fromRowMajor[D3](cells.slice(1, 17).map(_.toDouble).toVector))
      assertEquals(ok(AxisCodes.of(affine)).toString, cells(17), cells(0))

  private def volume(file: NiftiRaw) =
    val dims = file.spatialShape
    val space = ok(SampleSpaces.make(dims, affine = Some(ok(Affine.fromRowMajor[D3](file.sformRowMajor)))))
    SomeNeuroVolume.unsafeFromRavel[Double, Continuous](NDArray.tabulate[Double](dims(0), dims(1), dims(2))((x, y, z) => file.value(x, y, z)), space, "oracle")

  test("reorienting data and affine together reproduces nibabel as_reoriented"):
    val source = volume(raw("source.nii"))
    Vector("RAS", "LPI", "PIR", "SAL", "LAS").foreach: codes =>
      val expected = raw(s"reoriented_$codes.nii")
      val result = ok(Reorientation.volume(source, ok(AxisCodes.parse(codes))))
      assertEquals(result.space.grid.shape, expected.spatialShape, codes)
      result.space.grid.indexToFrame.rowMajor.zip(expected.sformRowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-5, s"$codes affine"))
      assertEquals(ok(AxisCodes.ofSpace(result.space)).toString, codes)
      val d = expected.spatialShape
      for x <- 0 until d(0); y <- 0 until d(1); z <- 0 until d(2) do
        assertEqualsDouble(result.values(x, y, z), expected.value(x, y, z), 0.0, s"$codes voxel ($x,$y,$z)")

  test("reorientation keeps world identity and returns to the start"):
    val source = volume(raw("source.nii"))
    val original = ok(AxisCodes.ofSpace(source.space))
    val there = ok(Reorientation.volume(source, AxisCodes.RAS))
    val back = ok(Reorientation.volume(there, original))
    assertEquals(SampleSpaces.worldOf(there.space), SampleSpaces.worldOf(source.space))
    back.space.grid.indexToFrame.rowMajor.zip(source.space.grid.indexToFrame.rowMajor).foreach((a, e) => assertEqualsDouble(a, e, 1e-9))
    assert(AxisCodes.parse("RRS").isLeft)
    assert(AxisCodes.parse("RA").isLeft)
