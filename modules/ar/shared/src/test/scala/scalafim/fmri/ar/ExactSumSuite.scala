package scalafim.fmri.ar

class ExactSumSuite extends munit.FunSuite:

  // ExactSum promises bit-exact, correctly rounded totals, so these assertions compare doubles exactly.

  test("cancellation is exact where naive summation loses the small terms") {
    assertEquals(sumOf(1e16, 1.0, -1e16), 1.0)
    assertEquals(sumOf(1e308, 1e308, -1e308, -1e308, 3.5), 3.5)
    assertEquals(sumOf(Double.MinPositiveValue, -Double.MinPositiveValue), 0.0)
    assertEquals(sumOf(), 0.0)
  }

  test("totals round once to nearest, ties to even") {
    val ulpAtOne = math.ulp(1.0)
    assertEquals(sumOf(1.0, ulpAtOne / 2.0), 1.0)
    assertEquals(sumOf(1.0, ulpAtOne / 2.0, Double.MinPositiveValue), 1.0 + ulpAtOne)
    assertEquals(sumOf(1.0 + ulpAtOne, ulpAtOne / 2.0), 1.0 + 2.0 * ulpAtOne)
    assertEquals(sumOf(-1.0, -ulpAtOne / 2.0, -Double.MinPositiveValue), -(1.0 + ulpAtOne))
    assertEquals(sumOf(Double.MinPositiveValue, Double.MinPositiveValue), 2.0 * Double.MinPositiveValue)
    assertEquals(sumOf(Double.MaxValue, Double.MaxValue), Double.PositiveInfinity)
  }

  test("totals are independent of order and of how partial sums are merged") {
    val values = Vector.tabulate(257) { index =>
      val raw = math.sin(index.toDouble * 12.9898) * 43758.5453
      (raw - math.floor(raw) - 0.5) * math.pow(10.0, (index % 9 - 4).toDouble)
    }
    val reference = sumOf(values*)
    assertEquals(sumOf(values.reverse*), reference)
    (1 to 17).foreach { width =>
      val parts = values.grouped(width).map { group =>
        val part = ExactSum.zero()
        group.foreach(part.add)
        part
      }.toVector
      val merged = ExactSum.zero()
      parts.reverse.foreach(merged.addAll)
      assertEquals(merged.value, reference, s"width=$width")
    }
  }

  test("non-finite inputs follow IEEE addition") {
    assertEquals(sumOf(1.0, Double.PositiveInfinity), Double.PositiveInfinity)
    assert(sumOf(Double.PositiveInfinity, Double.NegativeInfinity).isNaN)
    assert(sumOf(1.0, Double.NaN).isNaN)
  }

  private def sumOf(values: Double*): Double =
    val sum = ExactSum.zero()
    values.foreach(sum.add)
    sum.value
