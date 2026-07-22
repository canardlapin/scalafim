package scalafim.multivar

import gale.linalg.{DMat, DVec, DoubleLinearOperator, MutableDVec}

enum QuadraticFamily extends PenaltyFunctionalWitness:
  case Ridge
  case Tikhonov
  case GraphSmoothness
  case DerivativeSmoothness
  case SplineSmoothness
  case BlockSmoothness
  case MultisetDisagreement

  def functionalIdentity: PenaltyFunctionalIdentity = PenaltyFunctionalIdentity.SquaredNorm

/** Where the same PSD pullback enters a program. These placements are
  * scientifically and algebraically distinct even when they share `T* G T`.
  */
enum QuadraticPlacement:
  case ObjectiveRidge
  case DenominatorLoading

  def coefficient(weight: PenaltyWeight): Double =
    this match
      case ObjectiveRidge => -weight.value
      case DenominatorLoading => weight.value

enum QuadraticLoweringError:
  case NonlinearTarget(capability: TargetCapability)
  case FunctionalMismatch(actual: FunctionalKind)
  case GeometryMismatch(expected: ValueIdentity, actual: ValueIdentity)
  case TargetOperatorMismatch(expected: ValueIdentity, actual: Vector[ValueIdentity])
  case Semantic(error: SemanticError)

  def message: String =
    this match
      case NonlinearTarget(capability) => s"quadratic pullback requires a linear target, got $capability"
      case FunctionalMismatch(actual) => s"quadratic pullback requires a squared-norm functional, got $actual"
      case GeometryMismatch(expected, actual) =>
        s"squared-norm geometry ${expected.stableKey} does not match supplied geometry ${actual.stableKey}"
      case TargetOperatorMismatch(expected, actual) =>
        s"target expression does not bind supplied operator ${expected.stableKey}; bound ${actual.map(_.stableKey).mkString(", ")}"
      case Semantic(error) => error.message

final case class QuadraticEquivalenceProof(
    rule: String,
    exact: Boolean,
    targetIdentity: ValueIdentity,
    geometryIdentity: ValueIdentity,
    pulledBackIdentity: ValueIdentity,
    inputEvidence: EvidenceStatus,
    outputEvidence: EvidenceStatus,
    tolerance: CertificateTolerance
)

final case class QuadraticLowering[Feature <: SemanticSpace](
    original: PenaltyTerm,
    family: QuadraticFamily,
    placement: QuadraticPlacement,
    pulledBack: Op[Dual[Feature], Primal[Feature], PenaltyOperatorRole, CertifiedPsd],
    proof: QuadraticEquivalenceProof
):
  def effectiveCoefficient: Double = placement.coefficient(original.weight)

/** Exact compiler rewrite for a linear target and certified PSD geometry.
  * Composition stays structural: a sparse or matrix-free target is never
  * materialized merely to form the pullback.
  */
object QuadraticPullback:
  def lower[
      Feature <: SemanticSpace,
      Target <: SemanticSpace,
      RT <: OperatorRoleTag,
      ET <: OperatorEvidence,
      RG <: OperatorRoleTag,
      EG <: CertifiedPsd
  ](
      original: PenaltyTerm,
      target: Op[Dual[Feature], Primal[Target], RT, ET],
      geometry: Op[Primal[Target], Dual[Target], RG, EG],
      family: QuadraticFamily,
      placement: QuadraticPlacement
  ): Either[QuadraticLoweringError, QuadraticLowering[Feature]] =
    for
      _ <- requireLinear(original)
      _ <- requireGeometry(original, geometry.valueIdentity)
      _ <- requireTarget(original, target.valueIdentity)
      raw = target
        .andThen(geometry)
        .andThen(target.dual)
        .retag(OperatorRoleWitness.penalty, "quadratic-pullback")
      context <- CertificateContext
        .from(
          CertificateTolerance.strict,
          CertificateNorm.Frobenius,
          "exact-linear-quadratic-pullback",
          "operator-algebra",
          NumericalPrecision.Float64
        )
        .left
        .map(QuadraticLoweringError.Semantic.apply)
      certificate = Certificate.unsafe[PsdProperty](
        raw.valueIdentity,
        CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
        context
      )
      certified <- Op.certifiedPsd(raw, certificate).left.map(QuadraticLoweringError.Semantic.apply)
    yield
      QuadraticLowering(
        original,
        family,
        placement,
        certified,
        QuadraticEquivalenceProof(
          "linear-quadratic-pullback",
          exact = true,
          target.valueIdentity,
          geometry.valueIdentity,
          certified.valueIdentity,
          geometry.certificate.status,
          certified.certificate.status,
          CertificateTolerance.strict
        )
      )

  /** Forms either `S - lambda L` or `S + lambda L` without materializing either
    * input. The output is intentionally unchecked: subtraction may destroy PSD,
    * while an SPD-preserving denominator proof is a separate rewrite.
    */
  def effective[
      Feature <: SemanticSpace,
      R <: OperatorRoleTag,
      E <: OperatorEvidence
  ](
      base: Op[Dual[Feature], Primal[Feature], R, E],
      lowering: QuadraticLowering[Feature]
  ): Op[Dual[Feature], Primal[Feature], R, UncheckedEvidence] =
    val identity = ValueIdentity.derived(
      lowering.placement.toString.toLowerCase,
      base.valueIdentity,
      lowering.pulledBack.valueIdentity
    )
    new Op(
      WeightedSumKernel(base.kernel, lowering.pulledBack.kernel, 1.0, lowering.effectiveCoefficient),
      base.domain,
      base.codomain,
      base.role,
      OperatorCertificate.unchecked(identity),
      identity,
      (base.provenance ++ lowering.pulledBack.provenance).append(
        SemanticProvenanceEvent.Derived(
          lowering.placement.toString,
          Vector(base.valueIdentity, lowering.pulledBack.valueIdentity)
        )
      )
    )

  private def requireLinear(original: PenaltyTerm): Either[QuadraticLoweringError, Unit] =
    if original.target.capability == TargetCapability.Linear then Right(())
    else Left(QuadraticLoweringError.NonlinearTarget(original.target.capability))

  private def requireGeometry(original: PenaltyTerm, actual: ValueIdentity): Either[QuadraticLoweringError, Unit] =
    original.functional match
      case FunctionalKind.SquaredNorm(expected) if expected == actual => Right(())
      case FunctionalKind.SquaredNorm(expected) => Left(QuadraticLoweringError.GeometryMismatch(expected, actual))
      case other => Left(QuadraticLoweringError.FunctionalMismatch(other))

  private def requireTarget(original: PenaltyTerm, expected: ValueIdentity): Either[QuadraticLoweringError, Unit] =
    if original.target.operators.contains(expected) then Right(())
    else Left(QuadraticLoweringError.TargetOperatorMismatch(expected, original.target.operators))

private[multivar] final case class WeightedSumKernel(
    left: SemanticKernel,
    right: SemanticKernel,
    leftWeight: Double,
    rightWeight: Double
) extends SemanticKernel:
  require(left.rows == right.rows && left.cols == right.cols, "weighted operator sum requires equal shapes")

  private val operator = WeightedSumLinearOperator(left.linearMap, right.linearMap, leftWeight, rightWeight)

  val rows: Int = left.rows
  val cols: Int = left.cols
  val representation: OperatorRepresentation = OperatorRepresentation.MatrixFree
  val linearMap: DoubleLinearOperator = operator

  def forward(input: DMat): Either[SemanticError, DMat] =
    operator.applyTo(input).left.map(SemanticError.LinearMapFailure.apply)

  def adjoint: SemanticKernel =
    WeightedSumKernel(left.adjoint, right.adjoint, leftWeight, rightWeight)

private final case class WeightedSumLinearOperator(
    left: DoubleLinearOperator,
    right: DoubleLinearOperator,
    leftWeight: Double,
    rightWeight: Double
) extends DoubleLinearOperator:
  require(left.rows == right.rows && left.cols == right.cols, "weighted operator sum requires equal shapes")

  val rows: Int = left.rows
  val cols: Int = left.cols

  def applyTo(input: DVec, output: MutableDVec): Unit =
    combine(input, output, transpose = false)

  override def transposeApplyTo(input: DVec, output: MutableDVec): Unit =
    combine(input, output, transpose = true)

  private def combine(input: DVec, output: MutableDVec, transpose: Boolean): Unit =
    val length = if transpose then cols else rows
    val leftOutput = MutableDVec.zeros(length)
    val rightOutput = MutableDVec.zeros(length)
    if transpose then
      left.transposeApplyTo(input, leftOutput)
      right.transposeApplyTo(input, rightOutput)
    else
      left.applyTo(input, leftOutput)
      right.applyTo(input, rightOutput)
    var index = 0
    while index < length do
      output(index) = leftWeight * leftOutput(index) + rightWeight * rightOutput(index)
      index += 1
