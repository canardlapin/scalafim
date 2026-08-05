package scalafim.fmri.laws

/** The numerical work whose accumulated round-off a law is bounding. */
enum NumericalOperation(val amplification: Double):
  case DirectEvaluation extends NumericalOperation(4.0)
  case LinearCombination extends NumericalOperation(12.0)
  case Quadrature extends NumericalOperation(32.0)
  case BasisTransport extends NumericalOperation(24.0)
  case QrFactorization extends NumericalOperation(96.0)
  case WeightedTransformation extends NumericalOperation(48.0)
  case CovarianceCombination extends NumericalOperation(128.0)

/** Evidence used to derive a comparison-local floating-point bound.
  *
  * `conditionEstimate` is deliberately supplied by the property that knows the relevant geometry. It is never inferred
  * from a repository-wide global tolerance, and hostile cases cannot loosen unrelated comparisons.
  */
final case class NumericalEvidence(
    operation: NumericalOperation,
    scale: Double,
    conditionEstimate: Double,
    primitiveOperations: Int
):
  require(scale >= 0.0 && scale.isFinite, "numerical scale must be finite and non-negative")
  require(
    conditionEstimate >= 1.0 && conditionEstimate.isFinite,
    "condition evidence must be finite and at least one"
  )
  require(primitiveOperations > 0, "operation evidence must be positive")

  val absolute: Double =
    val accumulated =
      math.ulp(1.0) *
        operation.amplification *
        math.max(1.0, scale) *
        conditionEstimate *
        primitiveOperations.toDouble
    math.max(1e-14, accumulated)

  val relative: Double =
    math.max(1e-14, absolute / math.max(1.0, scale))

  def bound(expected: Double): Double =
    absolute + relative * math.abs(expected)

  def close(actual: Double, expected: Double): Boolean =
    actual.isFinite && expected.isFinite && math.abs(actual - expected) <= bound(expected)

  def describe(actual: Double, expected: Double): String =
    s"operation=$operation actual=$actual expected=$expected error=${math.abs(actual - expected)} " +
      s"bound=${bound(expected)} scale=$scale condition=$conditionEstimate operations=$primitiveOperations"

object NumericalEvidence:
  def direct(values: IterableOnce[Double], operations: Int): NumericalEvidence =
    val scale = values.iterator.map(math.abs).maxOption.getOrElse(0.0)
    NumericalEvidence(NumericalOperation.DirectEvaluation, scale, 1.0, math.max(1, operations))
