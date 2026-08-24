package scalafim.image

import SampleSpaces.*

import gale.linalg.DMat
import image4s.geometry.Affine
import image4s.geometry.D3
import scalafim.image.NeuroAffineSyntax.*

class SpaceUtilsSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows, clue = "")
    assertEquals(actual.cols, expected.cols, clue = "")
    var row = 0
    while row < actual.rows do
      var column = 0
      while column < actual.cols do
        assertClose(actual(row, column), expected(row, column), tol)
        column += 1
      row += 1

  test("outputAlignedSpace returns identity-aligned space for identity affine") {
    val identity = Affine.identity[D3]
    val out = SpaceUtils.outputAlignedSpace(Vector(5, 6, 7), identity)
    assertEquals(out.shape, Vector(5, 6, 7), clue = "")
    assertEquals(out.affine, identity, clue = "")
    assertClose(out.bounds.min, Vector(0.0, 0.0, 0.0), 1e-10)
    assertClose(out.bounds.max, Vector(4.0, 5.0, 6.0), 1e-10)
  }

  test("outputAlignedSpace supports SomeSampleSpace and custom voxel sizes") {
    val sp =
      SampleSpaces(
        Vector(5, 6, 7),
        spacing = Some(Vector(2.0, 3.0, 4.0)),
        origin = Some(Vector(10.0, 20.0, 30.0))
      )
    val out = SpaceUtils.outputAlignedSpace(sp, Vector(2.0, 3.0, 4.0))
    assertEquals(out.shape, Vector(5, 6, 7), clue = "")
    assertClose(Vector(out.affine.matrix(0, 0), out.affine.matrix(1, 1), out.affine.matrix(2, 2)), Vector(2.0, 3.0, 4.0), 1e-10)
    assertClose(Vector(out.affine.matrix(0, 3), out.affine.matrix(1, 3), out.affine.matrix(2, 3)), Vector(10.0, 20.0, 30.0), 1e-10)
  }

  test("vox2outVox is an alias for outputAlignedSpace") {
    val a = SpaceUtils.outputAlignedSpace(Vector(4, 4, 4), Affine.identity[D3])
    val b = SpaceUtils.vox2outVox(SampleSpaces(Vector(4, 4, 4)))
    assertEquals(a.shape, b.shape, clue = "")
    assertEquals(a.affine, b.affine, clue = "")
  }

  test("sliceToVolumeAffine embeds a 2D slice in a 3D volume") {
    val aff =
      SpaceUtils.sliceToVolumeAffine(
        index = 3,
        axis = 3,
        shape = Some(Vector(10, 10, 10)),
        indexBase = SpaceUtils.IndexBase.R,
        axisBase = SpaceUtils.IndexBase.R
      )
    val expected =
      DMat.dense(
        4,
        3,
        Vector(
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 2.0),
          Vector(0.0, 0.0, 1.0)
        ).flatten
      )
    assertClose(aff, expected, 0.0)
  }

  test("deoblique builds axis-aligned target and supports gridset") {
    val tx =
      ProviderSpaces.affine(
        Vector(
          Vector(2.0, 0.25, 0.0, 0.0),
          Vector(-0.1, 3.0, 0.0, 0.0),
          Vector(0.0, 0.0, 4.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val sp = SampleSpaces(Vector(18, 12, 10), spacing = Some(Vector(2.0, 3.0, 4.0)), affine = Some(tx))
    assert(tx.neuroObliquity.max > 0.0, clue = "")

    val deob = Deoblique.target(sp)
    val canonicalMinSpacing = tx.neuroVoxelSizes.min
    val deobAffine = deob.affineD3.fold(error => fail(error.message), identity)
    assertClose(
      Vector(deobAffine.matrix(0, 0), deobAffine.matrix(1, 1), deobAffine.matrix(2, 2)),
      Vector.fill(3)(canonicalMinSpacing),
      1e-10
    )
    assertClose(deobAffine.neuroObliquity, Vector(0.0, 0.0, 0.0), 1e-10)

    val grid = SampleSpaces(Vector(20, 20, 20), spacing = Some(Vector(1.0, 1.0, 1.0)), origin = Some(Vector(-5.0, -6.0, -7.0)))
    assertEquals(Deoblique.target(sp, grid), grid, clue = "")
  }

  test("deoblique resamples SomeNeuroVolume to target grid") {
    val tx =
      ProviderSpaces.affine(
        Vector(
          Vector(2.0, 0.3, 0.0, 0.0),
          Vector(0.0, 3.0, 0.0, 0.0),
          Vector(0.0, 0.0, 4.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val sp = SampleSpaces(Vector(8, 6, 4), spacing = Some(Vector(2.0, 3.0, 4.0)), affine = Some(tx))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](sp.spatialDims.product)(_.toDouble), sp)
    val out = Deoblique(vol, newgrid = 2.0, method = Resample.Method.Linear)

    assertEquals(out.space.spacing, Vector(2.0, 2.0, 2.0), clue = "")
    val outAffine = out.space.affineD3.fold(error => fail(error.message), identity)
    assertClose(outAffine.neuroObliquity, Vector(0.0, 0.0, 0.0), 1e-10)
  }
