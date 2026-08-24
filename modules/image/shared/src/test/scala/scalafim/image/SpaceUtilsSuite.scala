package scalafim.image

class SpaceUtilsSuite extends munit.FunSuite:

  private def assertClose(actual: Double, expected: Double, tol: Double = 1e-10): Unit =
    assert(math.abs(actual - expected) <= tol, clue = s"actual=$actual expected=$expected")

  private def assertClose(actual: Vector[Double], expected: Vector[Double], tol: Double): Unit =
    assertEquals(actual.length, expected.length, clue = "")
    actual.zip(expected).foreach { case (a, e) => assertClose(a, e, tol) }

  private def assertClose(actual: DMat, expected: DMat, tol: Double): Unit =
    assertEquals(actual.rows, expected.rows, clue = "")
    assertEquals(actual.cols, expected.cols, clue = "")
    var i = 0
    while i < actual.data.length do
      assertClose(actual.data(i), expected.data(i), tol)
      i += 1

  test("outputAlignedSpace returns identity-aligned space for identity affine") {
    val out = SpaceUtils.outputAlignedSpace(Vector(5, 6, 7), DMat.eye(4))
    assertEquals(out.shape, Vector(5, 6, 7), clue = "")
    assertEquals(out.affine, DMat.eye(4), clue = "")
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
    assertClose(Vector(out.affine(0, 0), out.affine(1, 1), out.affine(2, 2)), Vector(2.0, 3.0, 4.0), 1e-10)
    assertClose(Vector(out.affine(0, 3), out.affine(1, 3), out.affine(2, 3)), Vector(10.0, 20.0, 30.0), 1e-10)
  }

  test("vox2outVox is an alias for outputAlignedSpace") {
    val a = SpaceUtils.outputAlignedSpace(Vector(4, 4, 4), DMat.eye(4))
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
      DMat.fromRows(
        Vector(
          Vector(1.0, 0.0, 0.0),
          Vector(0.0, 1.0, 0.0),
          Vector(0.0, 0.0, 2.0),
          Vector(0.0, 0.0, 1.0)
        )
      )
    assertEquals(aff, expected, clue = "")
  }

  test("deoblique builds axis-aligned target and supports gridset") {
    val tx =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.25, 0.0, 0.0),
          Vector(-0.1, 3.0, 0.0, 0.0),
          Vector(0.0, 0.0, 4.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val sp = SampleSpaces(Vector(18, 12, 10), spacing = Some(Vector(2.0, 3.0, 4.0)), trans = Some(tx))
    assert(Affine.obliquity(sp.trans).max > 0.0, clue = "")

    val deob = Deoblique.target(sp)
    val canonicalMinSpacing = Affine.voxelSizes(tx).min
    assertClose(
      Vector(deob.trans(0, 0), deob.trans(1, 1), deob.trans(2, 2)),
      Vector.fill(3)(canonicalMinSpacing),
      1e-10
    )
    assertClose(Affine.obliquity(deob.trans), Vector(0.0, 0.0, 0.0), 1e-10)

    val grid = SampleSpaces(Vector(20, 20, 20), spacing = Some(Vector(1.0, 1.0, 1.0)), origin = Some(Vector(-5.0, -6.0, -7.0)))
    assertEquals(Deoblique.target(sp, grid), grid, clue = "")
  }

  test("deoblique resamples SomeNeuroVolume to target grid") {
    val tx =
      DMat.fromRows(
        Vector(
          Vector(2.0, 0.3, 0.0, 0.0),
          Vector(0.0, 3.0, 0.0, 0.0),
          Vector(0.0, 0.0, 4.0, 0.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val sp = SampleSpaces(Vector(8, 6, 4), spacing = Some(Vector(2.0, 3.0, 4.0)), trans = Some(tx))
    val vol = SomeScalarVolume.unsafeCopyFromCanonicalArray[Double](PrimitiveBuffers.tabulate[Double](sp.spatialDims.product)(_.toDouble), sp)
    val out = Deoblique(vol, newgrid = 2.0, method = Resample.Method.Linear)

    assertEquals(out.space.spacing, Vector(2.0, 2.0, 2.0), clue = "")
    assertClose(Affine.obliquity(out.space.trans), Vector(0.0, 0.0, 0.0), 1e-10)
  }
