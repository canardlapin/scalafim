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

  test("boundary totals: subnormals, large ties, window edges, overflow threshold, and signed zero") {
    val minNormal = java.lang.Double.MIN_NORMAL
    val tiny = Double.MinPositiveValue
    // Subnormal totals are exact; the first rounding happens at 2^-1021, where one ulp is two units.
    assertEquals(sumOf(minNormal, -tiny), minNormal - tiny)
    assertEquals(sumOf(tiny, tiny, tiny), 3.0 * tiny)
    assertEquals(sumOf(2.0 * minNormal, tiny), 2.0 * minNormal)
    assertEquals(sumOf(2.0 * minNormal, 3.0 * tiny), 2.0 * minNormal + 4.0 * tiny)
    // Ties at large magnitudes.
    assertEquals(sumOf(math.pow(2.0, 1000), math.pow(2.0, 947)), math.pow(2.0, 1000))
    assertEquals(
      sumOf(math.pow(2.0, 1000) + math.pow(2.0, 948), math.pow(2.0, 947)),
      math.pow(2.0, 1000) + math.pow(2.0, 949)
    )
    // Overflow happens exactly at (2^1024 - 2^970), the midpoint above MaxValue.
    val halfTop = math.ulp(Double.MaxValue) / 2.0
    assertEquals(sumOf(Double.MaxValue, halfTop), Double.PositiveInfinity)
    assertEquals(sumOf(-Double.MaxValue, -halfTop), Double.NegativeInfinity)
    assertEquals(sumOf(Double.MaxValue, halfTop, -tiny), Double.MaxValue)
    assertEquals(sumOf(-Double.MaxValue, -halfTop, tiny), -Double.MaxValue)
    // Exact zero is always +0.0, including a sum of negative zeros.
    Vector(Vector(-0.0), Vector(-0.0, -0.0), Vector(5.0, -5.0), Vector(-tiny, tiny)).foreach { values =>
      assertEquals(java.lang.Double.doubleToRawLongBits(sumOf(values*)), 0L, values.toString)
    }
    // The integer fast path ends at 2^62 units; probe ties and near-ties on both sides of each window edge.
    Vector(53, 61, 62, 63, 64).foreach { exponent =>
      val edge = powerOfTwo(exponent - 1074)
      val ulp = math.ulp(edge)
      Vector(
        Vector(edge, ulp / 2.0),
        Vector(edge, ulp / 2.0, tiny),
        Vector(edge, ulp / 2.0, -tiny),
        Vector(edge, ulp, ulp / 2.0),
        Vector(edge, -ulp / 4.0),
        Vector(edge, -ulp / 4.0, -tiny),
        Vector(edge, -edge / 2.0, ulp / 8.0),
        Vector(-edge, -ulp / 2.0, -tiny)
      ).foreach(values => assertMatchesReference(values, s"edge 2^$exponent units"))
    }
  }

  test("randomized totals match an independent BigInteger reference rounded to nearest, ties to even") {
    val random = new scala.util.Random(0x5ca1af1L)
    val tiny = Double.MinPositiveValue
    def mantissa(): Long = (random.nextLong() >>> 11) | (1L << 52)
    def clustered(base: Int): Double =
      val exponent = math.max(-1074, math.min(971, base - random.nextInt(70)))
      val magnitude = mantissa().toDouble * powerOfTwo(exponent)
      if random.nextBoolean() then magnitude else -magnitude
    def anyFinite(): Double =
      var candidate = java.lang.Double.longBitsToDouble(random.nextLong())
      while !candidate.isFinite do candidate = java.lang.Double.longBitsToDouble(random.nextLong())
      candidate
    (0 until 600).foreach { trial =>
      val values: Vector[Double] = trial % 6 match
        case 0 => Vector.fill(1 + random.nextInt(12))(anyFinite())
        case 1 =>
          val base = random.nextInt(2040) - 1070
          Vector.fill(2 + random.nextInt(20))(clustered(base))
        case 2 =>
          val x = clustered(random.nextInt(1900) - 1000)
          val half = math.ulp(x) / 2.0
          Vector(x, if random.nextBoolean() then half else -half) ++
            Vector.fill(random.nextInt(3))(if random.nextBoolean() then tiny else -tiny)
        case 3 => Vector.fill(1 + random.nextInt(30))(clustered(-1000 - random.nextInt(74)))
        case 4 =>
          val top = Double.MaxValue - math.ulp(Double.MaxValue) * random.nextInt(4).toDouble
          Vector(top, math.ulp(Double.MaxValue) / 2.0, clustered(970 - random.nextInt(60)))
        case _ =>
          val x = clustered(random.nextInt(1900) - 1000)
          Vector(x, -x, clustered(random.nextInt(1900) - 1000), clustered(random.nextInt(1900) - 1000))
      assertMatchesReference(values, s"trial $trial")
    }
  }

  private def sumOf(values: Double*): Double =
    val sum = ExactSum.zero()
    values.foreach(sum.add)
    sum.value

  // Independent reference: the exact total in units of 2^-1074 as a BigInteger, built from the IEEE bit fields.
  // java.math.BigDecimal is deliberately avoided; its double conversions are not exact on Scala.js.
  private val OverflowThreshold =
    java.math.BigInteger.ONE.shiftLeft(2098).subtract(java.math.BigInteger.ONE.shiftLeft(2044))

  private def units(value: Double): java.math.BigInteger =
    val bits = java.lang.Double.doubleToRawLongBits(value)
    val exponent = ((bits >>> 52) & 0x7ffL).toInt
    val fraction = bits & 0xfffffffffffffL
    val magnitude =
      if exponent == 0 then java.math.BigInteger.valueOf(fraction)
      else java.math.BigInteger.valueOf(fraction | (1L << 52)).shiftLeft(exponent - 1)
    if bits < 0L then magnitude.negate() else magnitude

  /** Exact 2^exponent for exponent in [-1074, 1023], built from bits so it is portable to Scala.js. */
  private def powerOfTwo(exponent: Int): Double =
    if exponent >= -1022 then java.lang.Double.longBitsToDouble((exponent + 1023).toLong << 52)
    else java.lang.Double.longBitsToDouble(1L << (exponent + 1074))

  private def nextUp(value: Double): Double =
    if value == 0.0 then Double.MinPositiveValue
    else
      val bits = java.lang.Double.doubleToRawLongBits(value)
      java.lang.Double.longBitsToDouble(if value > 0.0 then bits + 1L else bits - 1L)

  private def nextDown(value: Double): Double = -nextUp(-value)

  private def assertMatchesReference(values: Vector[Double], clue: String): Unit =
    val exact = values.foldLeft(java.math.BigInteger.ZERO)((total, value) => total.add(units(value)))
    val actual = sumOf(values*)
    val context = s"$clue values=$values actual=$actual"
    if exact.abs().compareTo(OverflowThreshold) >= 0 then
      assertEquals(actual, if exact.signum() > 0 then Double.PositiveInfinity else Double.NegativeInfinity, context)
    else if exact.signum() == 0 then
      assertEquals(java.lang.Double.doubleToRawLongBits(actual), 0L, context)
    else
      assert(actual.isFinite, context)
      assertEquals(math.signum(actual), exact.signum().toDouble, context)
      val distance = exact.subtract(units(actual)).abs()
      Vector(nextUp(actual), nextDown(actual)).filter(_.isFinite).foreach { neighbour =>
        val other = exact.subtract(units(neighbour)).abs()
        val order = distance.compareTo(other)
        assert(order <= 0, s"$context: $neighbour is nearer")
        if order == 0 then
          assertEquals(java.lang.Double.doubleToRawLongBits(actual) & 1L, 0L, s"$context: tie not rounded to even")
      }
