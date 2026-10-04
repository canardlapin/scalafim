package scalafim.fmri.design

/** Platform-independent decimal text for finite doubles.
  *
  * `Double.toString` differs between the JVM (`1.0`, `1.0E-7`) and Scala.js
  * (`1`, `1e-7`). Identifiers, formula text, JSON and CSV that must be
  * byte-identical across platforms use this formatter instead. The digits are the
  * fewest significant digits (rounded half-even) that read back as the same
  * double; this is chosen here rather than trusted to the platform, because the
  * JVM prints at least two digits (`4.9E-324` where JS prints `5e-324`).
  * Integral values print without a fraction, decimal exponents in `[-6, 21)`
  * print plainly, and other values use `d.ddde±x`. Negative zero prints as `0`. */
object PortableNumber:
  def format(value: Double): String =
    require(value.isFinite, s"portable number text requires a finite value, got $value")
    if value == 0.0 then "0"
    else
      val decimal = shortest(new java.math.BigDecimal(java.lang.Double.toString(value)), value)
      val exponent = decimal.precision - decimal.scale - 1
      if exponent >= -6 && exponent < 21 then decimal.toPlainString
      else
        val digits = decimal.unscaledValue.abs.toString
        val sign = if decimal.signum < 0 then "-" else ""
        val mantissa = if digits.length == 1 then digits else s"${digits.head}.${digits.tail}"
        s"$sign${mantissa}e$exponent"

  @annotation.tailrec
  private def shortest(exact: java.math.BigDecimal, value: Double, digits: Int = 1): java.math.BigDecimal =
    if digits >= exact.precision then exact.stripTrailingZeros
    else
      val candidate = exact.round(new java.math.MathContext(digits, java.math.RoundingMode.HALF_EVEN))
      if candidate.doubleValue == value then candidate.stripTrailingZeros
      else shortest(exact, value, digits + 1)
