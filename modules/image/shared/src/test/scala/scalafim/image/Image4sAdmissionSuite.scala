package scalafim.image

import image4s.apply
import munit.FunSuite
import ravel.NDArray

final class Image4sAdmissionSuite extends FunSuite:
  test("native scalar volume is already an image4s Sampled value"):
    val shape = Vector(2, 3, 4)
    val affine =
      ProviderSpaces.affine(
        Vector(
          Vector(0.0, -2.0, 0.0, 11.0),
          Vector(3.0, 0.0, 0.0, -7.0),
          Vector(0.0, 0.0, 4.0, 5.0),
          Vector(0.0, 0.0, 0.0, 1.0)
        )
      )
    val space = SampleSpaces(shape, affine = Some(affine))
    val values =
      NDArray.tabulate[Double](2, 3, 4): (x, y, z) =>
        100.0 * x.toDouble + 10.0 * y.toDouble + z.toDouble
    val volume =
      SomeScalarVolume.unsafeFromRavel(values, space, "admission-volume")

    assert(
      volume.asInstanceOf[AnyRef].eq(volume.sampled.asInstanceOf[AnyRef])
    )
    assert(volume.values.eq(values))
    assertEquals(volume.sampled.sampleSpace.spatialRank, 3, clue = "")
    val ranked =
      volume.sampled
        .requireDataRank[3]
        .fold(error => fail(error.message), identity)
    assertEquals(ranked.logicalShape, shape, clue = "")
    assertEquals(
      ranked.grid.indexToFrame.rowMajor,
      affine.rowMajor,
      clue = ""
    )
    assertEqualsDouble(ranked(1, 2, 3), 123.0, 0.0, clue = "")
    assertEqualsDouble(ranked(0, 1, 2), 12.0, 0.0, clue = "")
