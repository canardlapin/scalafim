package scalafim.fmri.design

class PortableNumberSuite extends munit.FunSuite:
  test("integral, fractional and exponent forms are identical on every platform"):
    assertEquals(PortableNumber.format(1.0), "1")
    assertEquals(PortableNumber.format(-4.0), "-4")
    assertEquals(PortableNumber.format(0.25), "0.25")
    assertEquals(PortableNumber.format(-0.0), "0")
    assertEquals(PortableNumber.format(1.0e-7), "1e-7")
    assertEquals(PortableNumber.format(-1.25e-9), "-1.25e-9")
    assertEquals(PortableNumber.format(1.0e21), "1e21")
    assertEquals(PortableNumber.format(123456.789), "123456.789")
    assertEquals(PortableNumber.format(0.1 + 0.2), "0.30000000000000004")
    assertEquals(PortableNumber.format(Double.MinPositiveValue), "5e-324")
    assertEquals(PortableNumber.format(-3.0 * Double.MinPositiveValue), "-1.5e-323")
    assertEquals(PortableNumber.format(Double.MaxValue), "1.7976931348623157e308")

  test("formatted text round-trips to the same double"):
    val values = Vector(1.0, 0.1, 1.0 / 3.0, 6.02214076e23, -2.5e-300, Double.MaxValue, Double.MinPositiveValue, 3.0 * Double.MinPositiveValue, 2.2250738585072014e-308, 1.0e-310)
    values.foreach(value => assertEquals(PortableNumber.format(value).toDouble, value))

  test("non-finite values are rejected"):
    intercept[IllegalArgumentException](PortableNumber.format(Double.NaN))
