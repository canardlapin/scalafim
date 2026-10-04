package scalafim.fmri.ar

/** Exact, order-independent accumulation of IEEE doubles.
  *
  * Every finite double is an integer multiple of `2^-1074`, so the running total is held exactly as a fixed-point
  * integer in carry-save base-`2^32` digits. Addition and [[addAll]] are therefore associative and commutative,
  * and [[value]] rounds the exact total to nearest (ties to even) once. This is what lets pooled lag products
  * reduced over arbitrary spatial blocks agree bit-for-bit with the single whole-volume reduction.
  *
  * Non-finite inputs follow IEEE addition in a separate channel, so a total that saw `NaN` or an infinity reports
  * it exactly as a naive sum would. Instances are mutable builders; callers that publish one must stop mutating it.
  */
private[ar] final class ExactSum private (
    private val digits: Array[Long],
    private var pending: Int,
    private var special: Double,
    private var hasSpecial: Boolean
):
  import ExactSum.*

  def add(value: Double): Unit =
    if value != 0.0 then
      if !value.isFinite then
        special = if hasSpecial then special + value else value
        hasSpecial = true
      else
        val bits = java.lang.Double.doubleToRawLongBits(value)
        val exponent = ((bits >>> 52) & 0x7ffL).toInt
        val fraction = bits & 0xfffffffffffffL
        val mantissa = if exponent == 0 then fraction else fraction | (1L << 52)
        val position = if exponent == 0 then 0 else exponent - 1
        val index = position >>> 5
        val shift = position & 31
        val low = (mantissa << shift) & DigitMask
        val middle = (mantissa >>> (32 - shift)) & DigitMask
        val high = if shift == 0 then 0L else mantissa >>> (64 - shift)
        if bits < 0L then
          digits(index) -= low
          digits(index + 1) -= middle
          digits(index + 2) -= high
        else
          digits(index) += low
          digits(index + 1) += middle
          digits(index + 2) += high
        pending += 1
        if pending >= NormalizeEvery then normalize()

  def addAll(other: ExactSum): Unit =
    normalize()
    // This side is now below 2^32 per digit and the other side, whatever its pending count, below 2^62 + 2^32,
    // so adding its carry-save digits directly cannot overflow and leaves `other` untouched.
    var index = 0
    while index < DigitCount do
      digits(index) += other.digits(index)
      index += 1
    pending = 1
    normalize()
    if other.hasSpecial then
      special = if hasSpecial then special + other.special else other.special
      hasSpecial = true

  def copy(): ExactSum =
    new ExactSum(digits.clone(), pending, special, hasSpecial)

  /** The exact total rounded once to the nearest double. */
  def value: Double =
    if hasSpecial then special
    else
      val magnitude = digits.clone()
      normalizeDigits(magnitude)
      val negative = magnitude(DigitCount - 1) < 0L
      if negative then
        var index = 0
        while index < DigitCount do
          magnitude(index) = -magnitude(index)
          index += 1
        normalizeDigits(magnitude)
      val rounded = roundMagnitude(magnitude)
      if negative then -rounded else rounded

  private def normalize(): Unit =
    if pending > 0 then
      normalizeDigits(digits)
      pending = 0

private[ar] object ExactSum:
  // Every finite double has magnitude below 2^1024 = 2^2098 units, so bits 0 through 2097 (digits 0 through 65)
  // cover one term; the remaining digits absorb carries from up to 2^60 terms.
  private val DigitCount = 70
  private val DigitMask = 0xffffffffL
  // Each addition moves a digit by less than 2^32, so 2^30 additions keep every carry-save digit below 2^63.
  private val NormalizeEvery = 1 << 30
  private val SignificandWindow = 62

  def zero(): ExactSum =
    new ExactSum(new Array[Long](DigitCount), 0, 0.0, false)

  private def normalizeDigits(digits: Array[Long]): Unit =
    var index = 0
    while index < DigitCount - 1 do
      val carry = digits(index) >> 32
      digits(index) -= carry << 32
      digits(index + 1) += carry
      index += 1

  /** Round a normalized non-negative fixed-point integer, in units of `2^-1074`, to the nearest double. */
  private def roundMagnitude(digits: Array[Long]): Double =
    var top = DigitCount - 1
    while top >= 0 && digits(top) == 0L do top -= 1
    if top < 0 then 0.0
    else
      val bitLength = 32 * top + (64 - java.lang.Long.numberOfLeadingZeros(digits(top)))
      if bitLength <= SignificandWindow then
        var integer = 0L
        var index = top
        while index >= 0 do
          integer = (integer << 32) | digits(index)
          index -= 1
        // Either the integer is below 2^53 and exact, or the scaled result is normal; one rounding either way.
        scale(integer.toDouble, -1074)
      else
        val dropped = bitLength - SignificandWindow
        var window = 0L
        var bit = bitLength - 1
        while bit >= dropped do
          window = (window << 1) | bitAt(digits, bit)
          bit -= 1
        // Bit zero of the 62-bit window sits below the rounding position, so it can carry the sticky bit.
        val sticky = if anyBitBelow(digits, dropped) then 1L else 0L
        scale((window | sticky).toDouble, dropped - 1074)

  private def bitAt(digits: Array[Long], bit: Int): Long =
    val index = math.min(bit >>> 5, DigitCount - 1)
    (digits(index) >>> (bit - 32 * index)) & 1L

  private def anyBitBelow(digits: Array[Long], bitCount: Int): Boolean =
    val whole = bitCount >>> 5
    var index = 0
    while index < whole do
      if digits(index) != 0L then return true
      index += 1
    val remainder = bitCount & 31
    remainder > 0 && (digits(whole) & ((1L << remainder) - 1L)) != 0L

  /** `value * 2^exponent` for `value` an integer below 2^62. An exponent above 1023 only arises from a window of at
    * least 2^61 units scaled past 2^1084, which overflows; it is answered directly because `powerOfTwo` would
    * otherwise encode an out-of-range exponent field.
    */
  private def scale(value: Double, exponent: Int): Double =
    if exponent > 1023 then Double.PositiveInfinity
    else value * powerOfTwo(exponent)

  private def powerOfTwo(exponent: Int): Double =
    if exponent >= -1022 then java.lang.Double.longBitsToDouble((exponent + 1023).toLong << 52)
    else java.lang.Double.longBitsToDouble(1L << (exponent + 1074))
