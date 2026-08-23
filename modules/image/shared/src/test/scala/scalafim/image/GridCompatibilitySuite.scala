package scalafim.image

import Ops.*
import spire.std.double.given

class GridCompatibilitySuite extends munit.FunSuite:

  private val translatedAffine =
    DMat.fromRows(
      Vector(
        Vector(1.0, 0.0, 0.0, 10.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  private val reflectedAffine =
    DMat.fromRows(
      Vector(
        Vector(-1.0, 0.0, 0.0, 1.0),
        Vector(0.0, 1.0, 0.0, 0.0),
        Vector(0.0, 0.0, 1.0, 0.0),
        Vector(0.0, 0.0, 0.0, 1.0)
      )
    )

  test("exact compatibility rejects equal-shaped spaces with different affines") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val translated = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))

    assert(GridCompatibility.exact(expected, translated).isLeft)
    assert(GridCompatibility.spatial(expected, translated).isLeft)
  }

  test("spatial compatibility ignores only non-spatial series extent") {
    val first = NeuroSpace(Vector(2, 2, 1, 3))
    val second = NeuroSpace(Vector(2, 2, 1, 5))

    assert(GridCompatibility.exact(first, second).isLeft)
    assertEquals(GridCompatibility.spatial(first, second), Right(()), clue = "")
  }

  test("pointwise volume arithmetic rejects a different physical grid") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val reflected = NeuroSpace(Vector(2, 2, 1), trans = Some(reflectedAffine))
    val left = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 1.0), expected)
    val right = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 2.0), reflected)

    intercept[IllegalArgumentException] {
      left + right
    }
  }

  test("volume concatenation rejects a different physical grid") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val translated = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))
    val left = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 1.0), expected)
    val right = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 2.0), translated)

    intercept[IllegalArgumentException] {
      left.concat(right)
    }
  }

  test("mask-based sparse conversion rejects a different physical grid") {
    val expected = NeuroSpace(Vector(2, 2, 1))
    val translated = NeuroSpace(Vector(2, 2, 1), trans = Some(translatedAffine))
    val volume = NeuroVol.copyFromCanonicalArray(PrimitiveBuffers.fillConst[Double](4, 1.0), expected)
    val mask = NeuroVol.copyFromCanonicalArray(Array[Boolean](true, true, false, false), translated)

    intercept[IllegalArgumentException] {
      volume.asSparse(mask)
    }
  }
