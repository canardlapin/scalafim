package scalafim.image

import ravel.NDArray
import ravel.Rank

class SliceImageOrdinalSuite extends munit.FunSuite:
  private def slice[A](values: NDArray[A, Rank[2]]): SliceImage[A] =
    val width = values.shape(1)
    val height = values.shape(0)
    val space = SampleSpaces.requireVolumeD3(SampleSpaces(Vector(width, height, 1)))
      .fold(error => fail(error.message), identity)
    val grid = SliceGrid.covering(space.grid,
      SlicePlane.canonical(AnatomicalPlane.Axial, WorldPoint(0, 0, 0)), PixelSpacing(1, 1))
    assertEquals((grid.dimensions.width, grid.dimensions.height), (width, height))
    SliceImage.unsafe(grid, values)

  test("canonical non-square integer slices retain top-to-bottom row order") {
    val image = slice(NDArray.tabulate[Int](3, 5)((row, column) => row * 10 + column))
    val expected = Vector(0, 1, 2, 3, 4, 10, 11, 12, 13, 14, 20, 21, 22, 23, 24)
    assertEquals(expected.indices.map(image.valueAtCanonicalOrdinal).toVector, expected)
  }

  test("transposed slices use logical order rather than physical storage order") {
    val values = NDArray.tabulate[Int](5, 3)((row, column) => row * 10 + column).transpose
    assert(!values.isCanonicalLayout)
    val image = slice(values)
    val expected = Vector(0, 10, 20, 30, 40, 1, 11, 21, 31, 41, 2, 12, 22, 32, 42)
    assertEquals(expected.indices.map(image.valueAtCanonicalOrdinal).toVector, expected)
  }

  test("canonical offset views retain their source offset") {
    val values = NDArray.tabulate[Int](5, 5)((row, column) => row * 10 + column).slice(0, 1 until 4)
    assert(values.isCanonicalLayout)
    assert(!values.isWholeBuffer)
    val image = slice(values)
    val expected = Vector(10, 11, 12, 13, 14, 20, 21, 22, 23, 24, 30, 31, 32, 33, 34)
    assertEquals(expected.indices.map(image.valueAtCanonicalOrdinal).toVector, expected)
  }

  test("ordinal access preserves signed zero, non-finite values and subnormal values") {
    val expected = Vector(-0.0, 0.0, Double.NaN, Double.NegativeInfinity, java.lang.Double.MIN_VALUE)
    val image = slice(NDArray.tabulate[Double](1, 5)((_, column) => expected(column)))
    expected.indices.foreach { index =>
      assertEquals(java.lang.Double.doubleToRawLongBits(image.valueAtCanonicalOrdinal(index)),
        java.lang.Double.doubleToRawLongBits(expected(index)))
    }
  }

  test("canonical and noncanonical slices retain the existing ordinal bounds refusal") {
    val canonical = NDArray.tabulate[Int](2, 3)((row, column) => row * 10 + column)
    Vector(canonical, canonical.transpose).foreach { values =>
      val image = slice(values)
      Vector(-1, values.size).foreach { index =>
        val failure = intercept[IllegalArgumentException](image.valueAtCanonicalOrdinal(index))
        assertEquals(failure.getMessage, "requirement failed: slice ordinal out of bounds")
      }
    }
  }
