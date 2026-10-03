package scalafim.fmri.design

/** Platform-independent decimal text for finite doubles.
  *
  * `Double.toString` differs between the JVM (`1.0`, `1.0E-7`) and Scala.js
  * (`1`, `1e-7`). Identifiers, formula text, JSON and CSV that must be
  * byte-identical across platforms use this formatter instead. Both platforms
  * produce the shortest round-tripping digits, so only the layout is normalized:
  * integral values print without a fraction, decimal exponents in `[-6, 21)` print
  * plainly, and other values use `d.ddde±x`. Negative zero prints as `0`. */
object PortableNumber:
  def format(value: Double): String =
    require(value.isFinite, s"portable number text requires a finite value, got $value")
    if value == 0.0 then "0"
    else
      val decimal = new java.math.BigDecimal(java.lang.Double.toString(value)).stripTrailingZeros
      val exponent = decimal.precision - decimal.scale - 1
      if exponent >= -6 && exponent < 21 then decimal.toPlainString
      else
        val digits = decimal.unscaledValue.abs.toString
        val sign = if decimal.signum < 0 then "-" else ""
        val mantissa = if digits.length == 1 then digits else s"${digits.head}.${digits.tail}"
        s"$sign${mantissa}e$exponent"
