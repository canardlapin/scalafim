package scalafim.response

sealed trait DecodeConsistency:
  def agrees(left: Double, right: Double): Boolean

object DecodeConsistency:
  case object ExactBits extends DecodeConsistency:
    def agrees(left: Double, right: Double): Boolean =
      java.lang.Double.doubleToRawLongBits(left) ==
        java.lang.Double.doubleToRawLongBits(right)

  final class UlpBounded private[response] (
      val maxUlps: Int
  ) extends DecodeConsistency:
    def agrees(left: Double, right: Double): Boolean =
      if ExactBits.agrees(left, right) then true
      else if left.isNaN || right.isNaN then false
      else
        val leftKey = orderedKey(java.lang.Double.doubleToRawLongBits(left))
        val rightKey = orderedKey(java.lang.Double.doubleToRawLongBits(right))
        val distance =
          if java.lang.Long.compareUnsigned(leftKey, rightKey) >= 0 then
            leftKey - rightKey
          else
            rightKey - leftKey
        java.lang.Long.compareUnsigned(distance, maxUlps.toLong) <= 0

    override def equals(other: Any): Boolean =
      other match
        case that: UlpBounded =>
          maxUlps == that.maxUlps
        case _ =>
          false

    override def hashCode(): Int =
      maxUlps

  final class AbsoluteRelative private[response] (
      val absolute: Double,
      val relative: Double
  ) extends DecodeConsistency:
    def agrees(left: Double, right: Double): Boolean =
      if ExactBits.agrees(left, right) then true
      else if !left.isFinite || !right.isFinite then false
      else
        val difference = math.abs(left - right)
        difference <= absolute + relative * math.max(math.abs(left), math.abs(right))

    override def equals(other: Any): Boolean =
      other match
        case that: AbsoluteRelative =>
          java.lang.Double.doubleToRawLongBits(absolute) ==
            java.lang.Double.doubleToRawLongBits(that.absolute) &&
            java.lang.Double.doubleToRawLongBits(relative) ==
              java.lang.Double.doubleToRawLongBits(that.relative)
        case _ =>
          false

    override def hashCode(): Int =
      31 * java.lang.Double.hashCode(absolute) + java.lang.Double.hashCode(relative)

  def ulpBounded(maxUlps: Int): Either[ConsistencyError, DecodeConsistency] =
    if maxUlps < 0 then Left(ConsistencyError.NegativeUlps(maxUlps))
    else Right(new UlpBounded(maxUlps))

  def absoluteRelative(
      absolute: Double,
      relative: Double
  ): Either[ConsistencyError, DecodeConsistency] =
    if !absolute.isFinite || absolute < 0.0 then
      Left(ConsistencyError.InvalidTolerance("absolute", absolute))
    else if !relative.isFinite || relative < 0.0 then
      Left(ConsistencyError.InvalidTolerance("relative", relative))
    else
      Right(new AbsoluteRelative(absolute, relative))

  private def orderedKey(bits: Long): Long =
    if bits < 0L then ~bits
    else bits | Long.MinValue

