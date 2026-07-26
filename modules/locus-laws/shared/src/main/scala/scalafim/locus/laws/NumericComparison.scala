package scalafim.locus.laws

trait NumericComparison[-A]:
  def equivalent(left: A, right: A): Boolean

object NumericComparison:
  def exact[A]: NumericComparison[A] =
    new NumericComparison[A]:
      def equivalent(left: A, right: A): Boolean =
        left == right

  def double(
      absoluteTolerance: Double,
      relativeTolerance: Double
  ): NumericComparison[Double] =
    require(absoluteTolerance >= 0.0, "absolute tolerance must be non-negative")
    require(relativeTolerance >= 0.0, "relative tolerance must be non-negative")
    new NumericComparison[Double]:
      def equivalent(left: Double, right: Double): Boolean =
        if left.isNaN || right.isNaN then false
        else
          val scale = math.max(math.abs(left), math.abs(right))
          math.abs(left - right) <= absoluteTolerance + relativeTolerance * scale
