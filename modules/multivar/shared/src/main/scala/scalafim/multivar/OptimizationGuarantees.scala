package scalafim.multivar

sealed trait PositiveProofConstantRole
sealed trait SmoothnessConstantRole extends PositiveProofConstantRole
sealed trait StrongConvexityConstantRole extends PositiveProofConstantRole
sealed trait CoercivityConstantRole extends PositiveProofConstantRole

/** A positive mathematical constant whose phantom role prevents, for example,
  * a Lipschitz bound from being supplied as a strong-convexity modulus.
  */
opaque type PositiveProofConstant[Role <: PositiveProofConstantRole] = Double

type SmoothnessConstant = PositiveProofConstant[SmoothnessConstantRole]
type StrongConvexityModulus = PositiveProofConstant[StrongConvexityConstantRole]
type NullspaceCoercivityModulus = PositiveProofConstant[CoercivityConstantRole]

object PositiveProofConstant:
  def smoothness(value: Double): Either[OptimizationGuaranteeError, SmoothnessConstant] =
    positive("smoothness constant", value)

  def strongConvexity(value: Double): Either[OptimizationGuaranteeError, StrongConvexityModulus] =
    positive("strong-convexity modulus", value)

  def nullspaceCoercivity(value: Double): Either[OptimizationGuaranteeError, NullspaceCoercivityModulus] =
    positive("nullspace-coercivity modulus", value)

  def operatorNorm(value: Double): Either[OptimizationGuaranteeError, CertifiedOperatorNormBound] =
    NonNegativeProofBound.operatorNorm(value)

  private[multivar] def unsafeSmoothness(value: Double): SmoothnessConstant =
    unsafe(smoothness(value))

  private[multivar] def unsafeStrongConvexity(value: Double): StrongConvexityModulus =
    unsafe(strongConvexity(value))

  private[multivar] def unsafeOperatorNorm(value: Double): CertifiedOperatorNormBound =
    operatorNorm(value).fold(error => throw new IllegalArgumentException(error.message), identity)

  extension [Role <: PositiveProofConstantRole](value: PositiveProofConstant[Role])
    inline def doubleValue: Double = value

  private def positive[Role <: PositiveProofConstantRole](
      label: String,
      value: Double
  ): Either[OptimizationGuaranteeError, PositiveProofConstant[Role]] =
    if value.isFinite && value > 0.0 then Right(value)
    else Left(OptimizationGuaranteeError.InvalidBound(label, value, "must be finite and positive"))

  private def unsafe[Role <: PositiveProofConstantRole](
      result: Either[OptimizationGuaranteeError, PositiveProofConstant[Role]]
  ): PositiveProofConstant[Role] =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

sealed trait NonNegativeProofBoundRole
sealed trait ResidualBoundRole extends NonNegativeProofBoundRole
sealed trait ObjectiveGapBoundRole extends NonNegativeProofBoundRole
sealed trait DistanceBoundRole extends NonNegativeProofBoundRole
sealed trait InexactnessBoundRole extends NonNegativeProofBoundRole
sealed trait OperatorNormBoundRole extends NonNegativeProofBoundRole

opaque type NonNegativeProofBound[Role <: NonNegativeProofBoundRole] = Double

type CertifiedResidualBound = NonNegativeProofBound[ResidualBoundRole]
type CertifiedObjectiveGap = NonNegativeProofBound[ObjectiveGapBoundRole]
type CertifiedDistanceBound = NonNegativeProofBound[DistanceBoundRole]
type ControlledInexactnessBound = NonNegativeProofBound[InexactnessBoundRole]
type CertifiedOperatorNormBound = NonNegativeProofBound[OperatorNormBoundRole]

object NonNegativeProofBound:
  def residual(value: Double): Either[OptimizationGuaranteeError, CertifiedResidualBound] =
    nonNegative("semantic residual", value)

  def objectiveGap(value: Double): Either[OptimizationGuaranteeError, CertifiedObjectiveGap] =
    nonNegative("objective-gap bound", value)

  def distance(value: Double): Either[OptimizationGuaranteeError, CertifiedDistanceBound] =
    nonNegative("distance-to-minimizer bound", value)

  def inexactness(value: Double): Either[OptimizationGuaranteeError, ControlledInexactnessBound] =
    nonNegative("controlled-inexactness bound", value)

  def operatorNorm(value: Double): Either[OptimizationGuaranteeError, CertifiedOperatorNormBound] =
    nonNegative("operator-norm upper bound", value)

  private[multivar] def unsafeResidual(value: Double): CertifiedResidualBound =
    unsafe(residual(value))

  private[multivar] def unsafeObjectiveGap(value: Double): CertifiedObjectiveGap =
    unsafe(objectiveGap(value))

  private[multivar] def unsafeDistance(value: Double): CertifiedDistanceBound =
    unsafe(distance(value))

  extension [Role <: NonNegativeProofBoundRole](value: NonNegativeProofBound[Role])
    inline def doubleValue: Double = value

  private def nonNegative[Role <: NonNegativeProofBoundRole](
      label: String,
      value: Double
  ): Either[OptimizationGuaranteeError, NonNegativeProofBound[Role]] =
    if value.isFinite && value >= 0.0 then Right(value)
    else Left(OptimizationGuaranteeError.InvalidBound(label, value, "must be finite and non-negative"))

  private def unsafe[Role <: NonNegativeProofBoundRole](
      result: Either[OptimizationGuaranteeError, NonNegativeProofBound[Role]]
  ): NonNegativeProofBound[Role] =
    result.fold(error => throw new IllegalArgumentException(error.message), identity)

enum ObservationMaskIdentity:
  case Complete
  case Observed(mask: ValueIdentity)

/** Complete identity boundary for one optimization claim.
  *
  * The result is included because numerical certificates concern one returned
  * value, not merely a model class. `Complete` is an explicit mask state; an
  * absent mask is never silently interpreted as an unknown missingness policy.
  */
final class OptimizationIdentityBindings private (
    val contract: ContractReference[ModelContractReference],
    val program: ValueIdentity,
    val data: ValueIdentity,
    val mask: ObservationMaskIdentity,
    val operators: Vector[ValueIdentity],
    val parameters: Vector[ParameterId],
    val result: ValueIdentity
):
  private[multivar] def containsValue(identity: ValueIdentity): Boolean =
    identity == program || identity == data || identity == result ||
      operators.contains(identity) || mask == ObservationMaskIdentity.Observed(identity)

object OptimizationIdentityBindings:
  def from(
      contract: ContractReference[ModelContractReference],
      program: ValueIdentity,
      data: ValueIdentity,
      mask: ObservationMaskIdentity,
      operators: Vector[ValueIdentity],
      parameters: Vector[ParameterId],
      result: ValueIdentity
  ): Either[OptimizationGuaranteeError, OptimizationIdentityBindings] =
    if operators.isEmpty then Left(OptimizationGuaranteeError.EmptyIdentitySet("operators"))
    else if parameters.isEmpty then Left(OptimizationGuaranteeError.EmptyIdentitySet("parameters"))
    else if operators.distinct.length != operators.length then
      Left(OptimizationGuaranteeError.DuplicateIdentity("operator"))
    else if parameters.distinct.length != parameters.length then
      Left(OptimizationGuaranteeError.DuplicateIdentity("parameter"))
    else Right(new OptimizationIdentityBindings(contract, program, data, mask, operators, parameters, result))

final class ProperClosedConvexWitness private (
    val program: ValueIdentity,
    val functional: ValueIdentity,
    val assumption: ContractReference[AssumptionReference]
)

object ProperClosedConvexWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      functional: ValueIdentity,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, ProperClosedConvexWitness] =
    requireBoundValue(bindings, functional, "convex functional").map: _ =>
      new ProperClosedConvexWitness(bindings.program, functional, assumption)

final class SmoothnessWitness private (
    val program: ValueIdentity,
    val functional: ValueIdentity,
    val lipschitz: SmoothnessConstant,
    val assumption: ContractReference[AssumptionReference]
)

object SmoothnessWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      functional: ValueIdentity,
      lipschitz: SmoothnessConstant,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, SmoothnessWitness] =
    requireBoundValue(bindings, functional, "smooth functional").map: _ =>
      new SmoothnessWitness(bindings.program, functional, lipschitz, assumption)

final class StrongConvexityWitness private (
    val program: ValueIdentity,
    val functional: ValueIdentity,
    val modulus: StrongConvexityModulus,
    val assumption: ContractReference[AssumptionReference]
)

object StrongConvexityWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      functional: ValueIdentity,
      modulus: StrongConvexityModulus,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, StrongConvexityWitness] =
    requireBoundValue(bindings, functional, "strongly convex functional").map: _ =>
      new StrongConvexityWitness(bindings.program, functional, modulus, assumption)

/** Runtime companion to the extant static `PsdEvidence` hierarchy. */
final class PsdOperatorWitness private (
    val program: ValueIdentity,
    val operator: ValueIdentity,
    val certificate: NumericalCertificate,
    val assumption: ContractReference[AssumptionReference]
)

object PsdOperatorWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      operator: ValueIdentity,
      certificate: NumericalCertificate,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, PsdOperatorWitness] =
    if !bindings.operators.contains(operator) then
      Left(OptimizationGuaranteeError.ForeignValueIdentity("PSD operator", operator))
    else if certificate.valueIdentity != operator then
      Left(OptimizationGuaranteeError.CertificateIdentityMismatch(operator, certificate.valueIdentity))
    else
      certificate.claim match
        case CertificateClaim.PositiveSemidefinite(_, _, _) | CertificateClaim.PositiveDefinite(_, _, _) =>
          Right(new PsdOperatorWitness(bindings.program, operator, certificate, assumption))
        case _ => Left(OptimizationGuaranteeError.CertificatePropertyMismatch("psd", certificate.claim.property))

final class NullspaceCoercivityWitness private (
    val program: ValueIdentity,
    val functional: ValueIdentity,
    val nullspace: ValueIdentity,
    val modulus: NullspaceCoercivityModulus,
    val assumption: ContractReference[AssumptionReference]
)

object NullspaceCoercivityWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      functional: ValueIdentity,
      nullspace: ValueIdentity,
      modulus: NullspaceCoercivityModulus,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, NullspaceCoercivityWitness] =
    for
      _ <- requireBoundValue(bindings, functional, "coercive functional")
      _ <- requireBoundValue(bindings, nullspace, "nullspace")
    yield new NullspaceCoercivityWitness(bindings.program, functional, nullspace, modulus, assumption)

final class OperatorNormWitness private (
    val program: ValueIdentity,
    val operator: ValueIdentity,
    val upperBound: CertifiedOperatorNormBound,
    val assumption: ContractReference[AssumptionReference]
)

object OperatorNormWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      operator: ValueIdentity,
      upperBound: CertifiedOperatorNormBound,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, OperatorNormWitness] =
    if bindings.operators.contains(operator) then
      Right(new OperatorNormWitness(bindings.program, operator, upperBound, assumption))
    else Left(OptimizationGuaranteeError.ForeignValueIdentity("norm-bounded operator", operator))

enum ExactOracleKind:
  case Proximal
  case Projection

final class ExactOracleLawWitness private (
    val program: ValueIdentity,
    val target: ValueIdentity,
    val kind: ExactOracleKind,
    val assumption: ContractReference[AssumptionReference]
)

object ExactOracleLawWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      target: ValueIdentity,
      kind: ExactOracleKind,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, ExactOracleLawWitness] =
    requireBoundValue(bindings, target, s"exact $kind oracle").map: _ =>
      new ExactOracleLawWitness(bindings.program, target, kind, assumption)

final class ControlledInexactnessWitness private (
    val program: ValueIdentity,
    val target: ValueIdentity,
    val errorBound: ControlledInexactnessBound,
    val assumption: ContractReference[AssumptionReference]
)

object ControlledInexactnessWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      target: ValueIdentity,
      errorBound: ControlledInexactnessBound,
      assumption: ContractReference[AssumptionReference]
  ): Either[OptimizationGuaranteeError, ControlledInexactnessWitness] =
    requireBoundValue(bindings, target, "controlled inexact oracle").map: _ =>
      new ControlledInexactnessWitness(bindings.program, target, errorBound, assumption)

enum TheoremAssumptionEvidence:
  case StaticType
  case Numerical(certificate: NumericalCertificate)
  case AlgorithmicReduction(name: String)

final class TheoremAssumptionWitness private (
    val program: ValueIdentity,
    val assumption: ContractReference[AssumptionReference],
    val boundValues: Vector[ValueIdentity],
    val evidence: TheoremAssumptionEvidence
)

object TheoremAssumptionWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      assumption: ContractReference[AssumptionReference],
      boundValues: Vector[ValueIdentity],
      evidence: TheoremAssumptionEvidence
  ): Either[OptimizationGuaranteeError, TheoremAssumptionWitness] =
    if boundValues.isEmpty then Left(OptimizationGuaranteeError.EmptyIdentitySet("theorem-assumption value"))
    else
      boundValues.find(identity => !bindings.containsValue(identity)) match
        case Some(identity) => Left(OptimizationGuaranteeError.ForeignValueIdentity("theorem assumption", identity))
        case None =>
          evidence match
            case TheoremAssumptionEvidence.Numerical(certificate)
                if !boundValues.contains(certificate.valueIdentity) =>
              Left(OptimizationGuaranteeError.ForeignCertificateIdentity(certificate.valueIdentity))
            case TheoremAssumptionEvidence.AlgorithmicReduction(name) if name.trim.isEmpty =>
              Left(OptimizationGuaranteeError.EmptyAlgorithmicReduction)
            case _ => Right(new TheoremAssumptionWitness(bindings.program, assumption, boundValues.distinct, evidence))

enum OptimizationProofObligation:
  case ProperClosedConvex(functional: ValueIdentity)
  case Smooth(functional: ValueIdentity)
  case StronglyConvex(functional: ValueIdentity)
  case PositiveSemidefinite(operator: ValueIdentity)
  case CoerciveOnNullspace(functional: ValueIdentity, nullspace: ValueIdentity)
  case NormBounded(operator: ValueIdentity)
  case ExactProximal(target: ValueIdentity)
  case ExactProjection(target: ValueIdentity)
  case ControlledInexactness(target: ValueIdentity)

final class OptimizationAssumptions private (
    val bindings: OptimizationIdentityBindings,
    val properClosedConvex: Vector[ProperClosedConvexWitness],
    val smoothness: Vector[SmoothnessWitness],
    val strongConvexity: Vector[StrongConvexityWitness],
    val positiveSemidefinite: Vector[PsdOperatorWitness],
    val nullspaceCoercivity: Vector[NullspaceCoercivityWitness],
    val normBounds: Vector[OperatorNormWitness],
    val exactOracleLaws: Vector[ExactOracleLawWitness],
    val controlledInexactness: Vector[ControlledInexactnessWitness],
    val theoremAssumptions: Vector[TheoremAssumptionWitness]
):
  def assumptionReferences: Set[ContractReference[AssumptionReference]] =
    properClosedConvex.map(_.assumption).toSet ++
      smoothness.map(_.assumption) ++
      strongConvexity.map(_.assumption) ++
      positiveSemidefinite.map(_.assumption) ++
      nullspaceCoercivity.map(_.assumption) ++
      normBounds.map(_.assumption) ++
      exactOracleLaws.map(_.assumption) ++
      controlledInexactness.map(_.assumption) ++
      theoremAssumptions.map(_.assumption)

  def unsatisfied(obligations: Set[OptimizationProofObligation]): Set[OptimizationProofObligation] =
    obligations.filterNot:
      case OptimizationProofObligation.ProperClosedConvex(functional) =>
        properClosedConvex.exists(_.functional == functional)
      case OptimizationProofObligation.Smooth(functional) =>
        smoothness.exists(_.functional == functional)
      case OptimizationProofObligation.StronglyConvex(functional) =>
        strongConvexity.exists(_.functional == functional)
      case OptimizationProofObligation.PositiveSemidefinite(operator) =>
        positiveSemidefinite.exists(_.operator == operator)
      case OptimizationProofObligation.CoerciveOnNullspace(functional, nullspace) =>
        nullspaceCoercivity.exists(witness => witness.functional == functional && witness.nullspace == nullspace)
      case OptimizationProofObligation.NormBounded(operator) =>
        normBounds.exists(_.operator == operator)
      case OptimizationProofObligation.ExactProximal(target) =>
        exactOracleLaws.exists(witness => witness.target == target && witness.kind == ExactOracleKind.Proximal)
      case OptimizationProofObligation.ExactProjection(target) =>
        exactOracleLaws.exists(witness => witness.target == target && witness.kind == ExactOracleKind.Projection)
      case OptimizationProofObligation.ControlledInexactness(target) =>
        controlledInexactness.exists(_.target == target)

object OptimizationAssumptions:
  def from(
      bindings: OptimizationIdentityBindings,
      properClosedConvex: Vector[ProperClosedConvexWitness] = Vector.empty,
      smoothness: Vector[SmoothnessWitness] = Vector.empty,
      strongConvexity: Vector[StrongConvexityWitness] = Vector.empty,
      positiveSemidefinite: Vector[PsdOperatorWitness] = Vector.empty,
      nullspaceCoercivity: Vector[NullspaceCoercivityWitness] = Vector.empty,
      normBounds: Vector[OperatorNormWitness] = Vector.empty,
      exactOracleLaws: Vector[ExactOracleLawWitness] = Vector.empty,
      controlledInexactness: Vector[ControlledInexactnessWitness] = Vector.empty,
      theoremAssumptions: Vector[TheoremAssumptionWitness] = Vector.empty
  ): Either[OptimizationGuaranteeError, OptimizationAssumptions] =
    val programs =
      properClosedConvex.map(_.program) ++ smoothness.map(_.program) ++ strongConvexity.map(_.program) ++
        positiveSemidefinite.map(_.program) ++ nullspaceCoercivity.map(_.program) ++ normBounds.map(_.program) ++
        exactOracleLaws.map(_.program) ++ controlledInexactness.map(_.program) ++ theoremAssumptions.map(_.program)
    if programs.exists(_ != bindings.program) then Left(OptimizationGuaranteeError.AssumptionProgramMismatch)
    else
      Right(
        new OptimizationAssumptions(
          bindings,
          properClosedConvex,
          smoothness,
          strongConvexity,
          positiveSemidefinite,
          nullspaceCoercivity,
          normBounds,
          exactOracleLaws,
          controlledInexactness,
          theoremAssumptions
        )
      )

  def empty(bindings: OptimizationIdentityBindings): OptimizationAssumptions =
    new OptimizationAssumptions(
      bindings,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

enum NumericalTermination:
  case Converged
  case IterationLimit
  case Infeasible
  case NumericalFailure

final class SemanticOptimizationEvidence private (
    val bindings: OptimizationIdentityBindings,
    val termination: NumericalTermination,
    val stationarity: Option[CertifiedResidualBound],
    val blockStationarity: Vector[(ParameterId, CertifiedResidualBound)],
    val feasibility: Option[CertifiedResidualBound],
    val objectiveGap: Option[CertifiedObjectiveGap],
    val distanceToMinimizer: Option[CertifiedDistanceBound],
    val numericalCertificates: Vector[NumericalCertificate]
)

object SemanticOptimizationEvidence:
  def from(
      bindings: OptimizationIdentityBindings,
      termination: NumericalTermination,
      stationarity: Option[CertifiedResidualBound] = None,
      blockStationarity: Vector[(ParameterId, CertifiedResidualBound)] = Vector.empty,
      feasibility: Option[CertifiedResidualBound] = None,
      objectiveGap: Option[CertifiedObjectiveGap] = None,
      distanceToMinimizer: Option[CertifiedDistanceBound] = None,
      numericalCertificates: Vector[NumericalCertificate] = Vector.empty
  ): Either[OptimizationGuaranteeError, SemanticOptimizationEvidence] =
    val blockParameters = blockStationarity.map(_._1)
    if blockParameters.distinct.length != blockParameters.length then
      Left(OptimizationGuaranteeError.DuplicateIdentity("block-stationarity parameter"))
    else
      blockParameters.find(parameter => !bindings.parameters.contains(parameter)) match
        case Some(parameter) => Left(OptimizationGuaranteeError.ForeignParameterIdentity(parameter))
        case None =>
          numericalCertificates.find(certificate => !bindings.containsValue(certificate.valueIdentity)) match
            case Some(certificate) =>
              Left(OptimizationGuaranteeError.ForeignCertificateIdentity(certificate.valueIdentity))
            case None =>
              Right(
                new SemanticOptimizationEvidence(
                  bindings,
                  termination,
                  stationarity,
                  blockStationarity,
                  feasibility,
                  objectiveGap,
                  distanceToMinimizer,
                  numericalCertificates
                )
              )

final class GlobalOptimalityWitness private (
    val bindings: OptimizationIdentityBindings,
    val theorem: ContractReference[TheoremReference],
    val assumptions: Set[ContractReference[AssumptionReference]],
    val oracle: OracleFamily
)

object GlobalOptimalityWitness:
  def from(
      bindings: OptimizationIdentityBindings,
      theorem: ContractReference[TheoremReference],
      assumptions: Set[ContractReference[AssumptionReference]],
      oracle: OracleFamily
  ): Either[OptimizationGuaranteeError, GlobalOptimalityWitness] =
    if assumptions.isEmpty then Left(OptimizationGuaranteeError.EmptyGlobalWitnessAssumptions)
    else Right(new GlobalOptimalityWitness(bindings, theorem, assumptions, oracle))

enum AchievedOptimizationGuarantee:
  case ExactGlobal(witness: GlobalOptimalityWitness, evidence: SemanticOptimizationEvidence)
  case EpsilonGlobal(gap: CertifiedObjectiveGap, evidence: SemanticOptimizationEvidence)
  case UniqueMinimizerWithinBound(distance: CertifiedDistanceBound, evidence: SemanticOptimizationEvidence)
  case Stationary(residual: CertifiedResidualBound, evidence: SemanticOptimizationEvidence)
  case CoordinatewiseStationary(
      residuals: Vector[(ParameterId, CertifiedResidualBound)],
      evidence: SemanticOptimizationEvidence
  )
  case FeasibleOnly(residual: CertifiedResidualBound, evidence: SemanticOptimizationEvidence)
  case Unresolved(evidence: SemanticOptimizationEvidence, reason: String)

  def claimClass: OptimizationClaimClass =
    this match
      case ExactGlobal(_, _) => OptimizationClaimClass.ExactGlobal
      case EpsilonGlobal(_, _) => OptimizationClaimClass.EpsilonGlobal
      case UniqueMinimizerWithinBound(_, _) => OptimizationClaimClass.UniqueMinimizerWithinBound
      case Stationary(_, _) => OptimizationClaimClass.Stationary
      case CoordinatewiseStationary(_, _) => OptimizationClaimClass.CoordinatewiseStationary
      case FeasibleOnly(_, _) => OptimizationClaimClass.Feasible
      case Unresolved(_, _) => OptimizationClaimClass.Unresolved

  def semanticEvidence: SemanticOptimizationEvidence =
    this match
      case ExactGlobal(_, value) => value
      case EpsilonGlobal(_, value) => value
      case UniqueMinimizerWithinBound(_, value) => value
      case Stationary(_, value) => value
      case CoordinatewiseStationary(_, value) => value
      case FeasibleOnly(_, value) => value
      case Unresolved(value, _) => value

object OptimizationGuaranteeAdmission:
  def admit(
      contract: MathematicalModelContract,
      requested: OptimizationClaimClass,
      assumptions: OptimizationAssumptions,
      obligations: Set[OptimizationProofObligation],
      evidence: SemanticOptimizationEvidence,
      globalWitness: Option[GlobalOptimalityWitness] = None
  ): Either[OptimizationGuaranteeError, AchievedOptimizationGuarantee] =
    if evidence.bindings.contract != contract.id then
      Left(OptimizationGuaranteeError.ContractIdentityMismatch(contract.id, evidence.bindings.contract))
    else if assumptions.bindings ne evidence.bindings then
      Left(OptimizationGuaranteeError.EvidenceBindingMismatch)
    else if !contract.admissibleClaims.contains(requested) then
      Left(OptimizationGuaranteeError.ClaimNotAdmissible(contract.family, requested))
    else
      val required = obligations ++ automaticObligations(requested, evidence.bindings)
      val missing = assumptions.unsatisfied(required)
      if missing.nonEmpty then Left(OptimizationGuaranteeError.MissingProofObligations(missing))
      else
        requested match
          case OptimizationClaimClass.ExactGlobal =>
            validateGlobalWitness(contract, requested, assumptions, evidence, globalWitness).map: witness =>
              AchievedOptimizationGuarantee.ExactGlobal(witness, evidence)
          case OptimizationClaimClass.EpsilonGlobal =>
            for
              _ <- validateGlobalWitness(contract, requested, assumptions, evidence, globalWitness)
              gap <- evidence.objectiveGap.toRight(OptimizationGuaranteeError.MissingSemanticEvidence(requested, "objective gap"))
            yield AchievedOptimizationGuarantee.EpsilonGlobal(gap, evidence)
          case OptimizationClaimClass.UniqueMinimizerWithinBound =>
            for
              _ <- validateGlobalWitness(contract, requested, assumptions, evidence, globalWitness)
              strong <- assumptions.strongConvexity.headOption.toRight(
                OptimizationGuaranteeError.MissingSemanticEvidence(requested, "strong-convexity modulus")
              )
              distance <- distanceBound(evidence, strong.modulus)
            yield AchievedOptimizationGuarantee.UniqueMinimizerWithinBound(distance, evidence)
          case OptimizationClaimClass.Stationary =>
            evidence.stationarity
              .toRight(OptimizationGuaranteeError.MissingSemanticEvidence(requested, "stationarity residual"))
              .map(AchievedOptimizationGuarantee.Stationary(_, evidence))
          case OptimizationClaimClass.CoordinatewiseStationary =>
            val covered = evidence.blockStationarity.map(_._1).toSet
            if covered != evidence.bindings.parameters.toSet then
              Left(OptimizationGuaranteeError.IncompleteBlockStationarity(evidence.bindings.parameters.toSet, covered))
            else Right(AchievedOptimizationGuarantee.CoordinatewiseStationary(evidence.blockStationarity, evidence))
          case OptimizationClaimClass.Feasible =>
            evidence.feasibility
              .toRight(OptimizationGuaranteeError.MissingSemanticEvidence(requested, "feasibility residual"))
              .map(AchievedOptimizationGuarantee.FeasibleOnly(_, evidence))
          case OptimizationClaimClass.Unresolved =>
            Right(AchievedOptimizationGuarantee.Unresolved(evidence, unresolvedReason(evidence.termination)))

  private def automaticObligations(
      requested: OptimizationClaimClass,
      bindings: OptimizationIdentityBindings
  ): Set[OptimizationProofObligation] =
    requested match
      case OptimizationClaimClass.EpsilonGlobal =>
        Set(OptimizationProofObligation.ProperClosedConvex(bindings.program))
      case OptimizationClaimClass.UniqueMinimizerWithinBound =>
        Set(
          OptimizationProofObligation.ProperClosedConvex(bindings.program),
          OptimizationProofObligation.StronglyConvex(bindings.program)
        )
      case _ => Set.empty

  private def validateGlobalWitness(
      contract: MathematicalModelContract,
      requested: OptimizationClaimClass,
      assumptions: OptimizationAssumptions,
      evidence: SemanticOptimizationEvidence,
      witness: Option[GlobalOptimalityWitness]
  ): Either[OptimizationGuaranteeError, GlobalOptimalityWitness] =
    witness.toRight(OptimizationGuaranteeError.MissingGlobalOptimalityWitness(requested)).flatMap: value =>
      if value.bindings ne evidence.bindings then Left(OptimizationGuaranteeError.EvidenceBindingMismatch)
      else
        contract.theorems.find(_.id == value.theorem) match
          case None => Left(OptimizationGuaranteeError.UnknownTheorem(value.theorem))
          case Some(theorem) if !theorem.supportedClaims.contains(requested) =>
            Left(OptimizationGuaranteeError.TheoremDoesNotSupport(value.theorem, requested))
          case Some(theorem) if !theorem.assumptions.toSet.subsetOf(value.assumptions) =>
            Left(OptimizationGuaranteeError.IncompleteTheoremAssumptions(value.theorem))
          case Some(_) if !value.assumptions.subsetOf(assumptions.assumptionReferences) =>
            Left(OptimizationGuaranteeError.UnwitnessedTheoremAssumptions(value.theorem))
          case Some(_) if !contract.oracles.contains(value.oracle) =>
            Left(OptimizationGuaranteeError.OracleNotAdmissible(value.oracle))
          case Some(_) => Right(value)

  private def distanceBound(
      evidence: SemanticOptimizationEvidence,
      modulus: StrongConvexityModulus
  ): Either[OptimizationGuaranteeError, CertifiedDistanceBound] =
    evidence.distanceToMinimizer match
      case Some(distance) => Right(distance)
      case None =>
        evidence.objectiveGap match
          case Some(gap) =>
            NonNegativeProofBound.distance(Math.sqrt(2.0 * gap.doubleValue / modulus.doubleValue))
          case None =>
            Left(
              OptimizationGuaranteeError.MissingSemanticEvidence(
                OptimizationClaimClass.UniqueMinimizerWithinBound,
                "a direct distance-to-minimizer or objective-gap bound"
              )
            )

  private def unresolvedReason(termination: NumericalTermination): String =
    termination match
      case NumericalTermination.Converged => "numerical stopping criterion met without a semantic proof obligation"
      case NumericalTermination.IterationLimit => "iteration budget exhausted"
      case NumericalTermination.Infeasible => "solver reported infeasibility"
      case NumericalTermination.NumericalFailure => "solver reported numerical failure"

enum OptimizationGuaranteeError:
  case InvalidBound(label: String, value: Double, requirement: String)
  case EmptyIdentitySet(kind: String)
  case DuplicateIdentity(kind: String)
  case ForeignValueIdentity(kind: String, actual: ValueIdentity)
  case ForeignParameterIdentity(actual: ParameterId)
  case ForeignCertificateIdentity(actual: ValueIdentity)
  case CertificateIdentityMismatch(expected: ValueIdentity, actual: ValueIdentity)
  case CertificatePropertyMismatch(expected: String, actual: String)
  case AssumptionProgramMismatch
  case EmptyGlobalWitnessAssumptions
  case EmptyAlgorithmicReduction
  case ContractIdentityMismatch(
      expected: ContractReference[ModelContractReference],
      actual: ContractReference[ModelContractReference]
  )
  case EvidenceBindingMismatch
  case ClaimNotAdmissible(family: MathematicalModelFamily, claim: OptimizationClaimClass)
  case MissingProofObligations(obligations: Set[OptimizationProofObligation])
  case MissingSemanticEvidence(claim: OptimizationClaimClass, evidence: String)
  case MissingGlobalOptimalityWitness(claim: OptimizationClaimClass)
  case UnknownTheorem(theorem: ContractReference[TheoremReference])
  case TheoremDoesNotSupport(theorem: ContractReference[TheoremReference], claim: OptimizationClaimClass)
  case IncompleteTheoremAssumptions(theorem: ContractReference[TheoremReference])
  case UnwitnessedTheoremAssumptions(theorem: ContractReference[TheoremReference])
  case OracleNotAdmissible(oracle: OracleFamily)
  case IncompleteBlockStationarity(expected: Set[ParameterId], actual: Set[ParameterId])

  def message: String =
    this match
      case InvalidBound(label, value, requirement) => s"$label $value $requirement"
      case EmptyIdentitySet(kind) => s"optimization bindings require at least one $kind identity"
      case DuplicateIdentity(kind) => s"optimization bindings contain a duplicate $kind identity"
      case ForeignValueIdentity(kind, actual) => s"$kind ${actual.stableKey} is not bound to this optimization problem"
      case ForeignParameterIdentity(actual) => s"parameter ${actual.value} is not bound to this optimization problem"
      case ForeignCertificateIdentity(actual) =>
        s"numerical certificate ${actual.stableKey} is not bound to this optimization problem"
      case CertificateIdentityMismatch(expected, actual) =>
        s"certificate identity ${actual.stableKey} does not match ${expected.stableKey}"
      case CertificatePropertyMismatch(expected, actual) => s"expected a $expected certificate, got $actual"
      case AssumptionProgramMismatch => "all assumptions must bind the same program as the optimization evidence"
      case EmptyGlobalWitnessAssumptions => "a global-optimality witness must name its theorem assumptions"
      case EmptyAlgorithmicReduction => "an algorithmic-reduction witness must name its reduction"
      case ContractIdentityMismatch(expected, actual) =>
        s"evidence contract ${actual.value} does not match ${expected.value}"
      case EvidenceBindingMismatch => "assumptions, witness, and evidence must share one exact identity binding"
      case ClaimNotAdmissible(family, claim) => s"$claim is not admissible for $family"
      case MissingProofObligations(obligations) =>
        s"missing proof obligations: ${obligations.toVector.sortBy(_.toString).mkString(", ")}"
      case MissingSemanticEvidence(claim, evidence) => s"$claim requires $evidence evidence"
      case MissingGlobalOptimalityWitness(claim) => s"$claim requires a theorem-bound global-optimality witness"
      case UnknownTheorem(theorem) => s"theorem ${theorem.value} is not part of the model contract"
      case TheoremDoesNotSupport(theorem, claim) => s"theorem ${theorem.value} does not support $claim"
      case IncompleteTheoremAssumptions(theorem) => s"theorem ${theorem.value} is missing declared assumptions"
      case UnwitnessedTheoremAssumptions(theorem) => s"theorem ${theorem.value} names assumptions without witnesses"
      case OracleNotAdmissible(oracle) => s"oracle $oracle is not admitted by the model contract"
      case IncompleteBlockStationarity(expected, actual) =>
        s"coordinatewise stationarity covers ${actual.size} of ${expected.size} parameter blocks"

private def requireBoundValue(
    bindings: OptimizationIdentityBindings,
    identity: ValueIdentity,
    kind: String
): Either[OptimizationGuaranteeError, Unit] =
  if bindings.containsValue(identity) then Right(())
  else Left(OptimizationGuaranteeError.ForeignValueIdentity(kind, identity))
