package scalafim.fmri.mvpa.dataset.predictive

import scalafim.fmri.mvpa.{AxisDescriptor, AxisDigest, AxisRef}

/** A fixed target block with total assessment mass, shared equally by its
  * coordinates. It does not rescale predictors or provider ridge penalties.
  */
final class RidgeResponseBlock private (
    val name: String, val targets: Vector[String], val mass: Double
)
object RidgeResponseBlock:
  def apply(name: String, targets: Vector[String], mass: Double): Either[AlderRidgeRegressionError, RidgeResponseBlock] =
    if name.trim.isEmpty then Left(AlderRidgeRegressionError.InvalidTargetGeometry("block name is blank"))
    else if targets.isEmpty || targets.exists(_.trim.isEmpty) || targets.distinct.length != targets.length then
      Left(AlderRidgeRegressionError.InvalidTargetGeometry("block targets must be non-empty, non-blank and distinct"))
    else if !mass.isFinite || mass <= 0.0 then Left(AlderRidgeRegressionError.InvalidTargetGeometry("block mass must be finite and positive"))
    else Right(new RidgeResponseBlock(name, targets, mass))

enum RidgeTargetMetricOrigin:
  case UniformCoordinates
  case FixedDeclared(reason: String)

/** Fixed diagonal target utility for shared-penalty selection and assessment.
  * Blocks partition the exact target axis. Coordinate j in block b has weight
  * mass(b)/size(b); normalized weights sum to one. An assessment appearance
  * contributes sum_j normalizedWeight(j) * residual(j)^2. The mean divides by
  * assessment appearances, including repeats. Per-output metrics and prediction
  * values retain their original units.
  *
  * Every fixed-lambda provider fit is unchanged. This is also equivalent to
  * weighting BOTH each output's residual and ridge penalty by its positive
  * weight; it is not residual-only weighted regression, GLS or a target prior.
  * FixedDeclared records the caller's declaration, not proof that weights were
  * chosen without assessment data. Learned weights/scales are not fitted here.
  */
final class RidgeTargetGeometry private (
    val responseAxis: AxisDescriptor,
    val targets: Vector[String],
    val blocks: Vector[RidgeResponseBlock],
    val coordinateWeights: Vector[Double],
    val normalizedWeights: Vector[Double],
    val totalMass: Double,
    val origin: RidgeTargetMetricOrigin,
    val identity: String
):
  val normalizedMass: Double = normalizedWeights.sum

  override def equals(other: Any): Boolean = other match
    case that: RidgeTargetGeometry => identity == that.identity && responseAxis == that.responseAxis
    case _ => false

  override def hashCode(): Int = 31 * identity.hashCode + responseAxis.hashCode

  private[predictive] def squaredError(observed: Vector[Double], predicted: Vector[Double]): Double =
    require(observed.length == targets.length && predicted.length == targets.length, "target metric arity differs")
    var sum = 0.0
    var column = 0
    while column < targets.length do
      val residual = observed(column) - predicted(column)
      // Multiplying the residual by sqrt(weight) first avoids avoidable overflow
      // in residual^2 when the weighted contribution is representable.
      val scaled = math.sqrt(normalizedWeights(column)) * residual
      sum += scaled * scaled
      column += 1
    sum

object RidgeTargetGeometry:
  def uniform[R](axis: AxisRef[R]): Either[AlderRidgeRegressionError, RidgeTargetGeometry] =
    for
      keys <- keysOf(axis)
      block <- RidgeResponseBlock("all-targets", keys, keys.length.toDouble)
      geometry <- create(axis, keys, Vector(block), RidgeTargetMetricOrigin.UniformCoordinates)
    yield geometry

  def declared[R](axis: AxisRef[R], blocks: Vector[RidgeResponseBlock], reason: String): Either[AlderRidgeRegressionError, RidgeTargetGeometry] =
    if reason.trim.isEmpty then Left(AlderRidgeRegressionError.InvalidTargetGeometry("fixed metric declaration is blank"))
    else keysOf(axis).flatMap(keys => create(axis, keys, blocks, RidgeTargetMetricOrigin.FixedDeclared(reason)))

  private def keysOf[R](axis: AxisRef[R]): Either[AlderRidgeRegressionError, Vector[String]] =
    Right(axis.toRecord.stableKeys)

  private def create[R](axis: AxisRef[R], keys: Vector[String], blocks: Vector[RidgeResponseBlock], origin: RidgeTargetMetricOrigin): Either[AlderRidgeRegressionError, RidgeTargetGeometry] =
    val assigned = blocks.flatMap(_.targets)
    if blocks.isEmpty || blocks.map(_.name).distinct.length != blocks.length then
      Left(AlderRidgeRegressionError.InvalidTargetGeometry("blocks must be non-empty with distinct names"))
    else if assigned.distinct.length != assigned.length || assigned.toSet != keys.toSet then
      Left(AlderRidgeRegressionError.InvalidTargetGeometry("blocks must partition every target key exactly once without foreign keys"))
    else
      val canonical = blocks.sortBy(_.name)
      val weightOf = canonical.flatMap(block => block.targets.map(_ -> (block.mass / block.targets.length.toDouble))).toMap
      val weights = keys.map(weightOf)
      val total = weights.sum
      val normalized = weights.map(_ / total)
      if !total.isFinite || total <= 0.0 || weights.exists(weight => !weight.isFinite || weight <= 0.0) || normalized.exists(weight => !weight.isFinite || weight <= 0.0) then
        Left(AlderRidgeRegressionError.InvalidTargetGeometry("expanded and normalized target weights must remain finite and positive"))
      else
        val fingerprint = AxisDigest.sha256Hex: writer =>
          writer.string("scalafim.ridge-target-assessment.v1")
          writer.string(axis.descriptor.stableKey)
          origin match
            case RidgeTargetMetricOrigin.UniformCoordinates => writer.string("uniform-coordinates")
            case RidgeTargetMetricOrigin.FixedDeclared(reason) =>
              writer.string("fixed-declared")
              writer.string(reason)
          writer.intLE(canonical.length)
          canonical.foreach: block =>
            writer.string(block.name)
            writer.string(java.lang.Long.toHexString(java.lang.Double.doubleToLongBits(block.mass)))
            val members = keys.filter(block.targets.toSet.contains)
            writer.intLE(members.length)
            members.foreach(writer.string)
        Right(new RidgeTargetGeometry(axis.descriptor, keys, canonical, weights, normalized, total, origin, fingerprint))
