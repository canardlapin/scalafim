package scalafim.image

import munit.FunSuite

class DMatSuite extends FunSuite:
  test("fromRowMajorOwned adopts its primitive buffer without copying") {
    val values = NArrayUtil.fromArray(Array(1.0, 2.0, 3.0, 4.0))
    val matrix = DMat.fromRowMajorOwned(2, 2, values)

    values(0) = 9.0

    assertEquals(matrix.rows, 2)
    assertEquals(matrix.cols, 2)
    assertEqualsDouble(matrix(0, 0), 9.0, 0.0)
    assertEqualsDouble(matrix(1, 1), 4.0, 0.0)
  }

  test("fromRowMajorOwned rejects invalid dimensions and buffer lengths") {
    intercept[IllegalArgumentException] {
      DMat.fromRowMajorOwned(0, 2, NArrayUtil.ofSize[Double](0))
    }
    intercept[IllegalArgumentException] {
      DMat.fromRowMajorOwned(2, 2, NArrayUtil.ofSize[Double](3))
    }
    intercept[IllegalArgumentException] {
      DMat.fromRowMajorOwned(Int.MaxValue, 2, NArrayUtil.ofSize[Double](1))
    }
  }
