package scalafim.multivar.ir

import scalafim.multivar.*

enum ProgramOperatorRoleIr:
  case Table
  case Metric
  case Cometric
  case Covariance
  case Scatter
  case Penalty
  case Kernel
  case RowLink
  case Frame
  case Cross
  case Component
  case Score
  case Axis
  case Coefficient
  case ConstraintMap
  case Composed(first: ProgramOperatorRoleIr, second: ProgramOperatorRoleIr)
  case Dual(of: ProgramOperatorRoleIr)
  case MetricAdjoint(of: ProgramOperatorRoleIr)

enum ProgramRepresentationIr:
  case Dense
  case Sparse
  case Diagonal
  case Block
  case LowRank
  case Kronecker
  case LazyAffine
  case MatrixFree

enum ProgramGaugeIr:
  case Ungauged
  case Shape(id: String)
  case Orthonormal(metricIdentity: String)

enum ProgramOperatorDerivationIr:
  case Source
  case SecondOrder(sourceTable: String, relationship: String, targetTable: String)
  case Compress(sourceFrame: String, secondOrder: String, targetFrame: String)
  case Scores(frame: String, table: String)
  case Axes(frame: String, cometric: String)
  case Lowered(rule: String, inputs: Vector[String])

final case class ProgramOperatorEvidenceIr(
    status: EvidenceStatusIr,
    certificates: Vector[CertificateIr]
)

final case class ProgramOpIr(
    id: String,
    domain: CoordinateIr,
    codomain: CoordinateIr,
    role: ProgramOperatorRoleIr,
    evidence: ProgramOperatorEvidenceIr,
    representation: ProgramRepresentationIr,
    gauge: ProgramGaugeIr,
    derivation: ProgramOperatorDerivationIr,
    valueIdentity: String,
    provenance: Vector[ProvenanceEventIr]
)

enum ProgramParameterizationIr:
  case Identity
  case KnownSupport(embeddingIdentity: String, injective: Boolean)
  case SharedBasis(basisIdentity: String, injective: Boolean)
  case FixedRank(rank: Int, gauge: String)
  case BlockDiagonal(blocks: Vector[String])
  case NullSpace(basisIdentity: String, tolerance: ToleranceIr)

final case class ProgramFrameParameterIr(
    id: String,
    featureSpaceId: String,
    componentSpaceId: String,
    parameterization: ProgramParameterizationIr
)

enum ProgramObjectiveIr:
  case MaximizeTrace(parameterId: String, operatorIdentity: String)
  case MaximizeCrossTrace(sourceParameterId: String, targetParameterId: String, operatorIdentity: String)
  case GeneralizedRayleigh(parameterId: String, numeratorIdentity: String, denominatorIdentity: String)
  case TraceRatio(parameterId: String, numeratorIdentity: String, denominatorIdentity: String)
  case RatioTrace(parameterId: String, numeratorIdentity: String, denominatorIdentity: String)
  case MinimizeDisagreement(parameterId: String, operatorIdentity: String)
  case SequentialCrossRegression(
      sourceParameterId: String,
      targetParameterId: String,
      crossIdentity: String,
      predictorIdentity: String
  )

final case class ProgramNormalizationV2Ir(parameterId: String, operatorIdentity: String)

enum ProgramTargetCapabilityIr:
  case Linear
  case Affine
  case Smooth
  case General

final case class ProgramTargetIr(
    parameterId: String,
    capability: ProgramTargetCapabilityIr,
    operation: String,
    operatorIdentity: Option[String],
    additionalParameterIds: Vector[String] = Vector.empty,
    additionalOperatorIdentities: Vector[String] = Vector.empty,
    equivariance: ProgramFrameSymmetryIr = ProgramFrameSymmetryIr.Orthogonal
):
  def parameterIds: Vector[String] = parameterId +: additionalParameterIds
  def operatorIdentities: Vector[String] = operatorIdentity.toVector ++ additionalOperatorIdentities

enum ProgramFunctionalIr:
  case SquaredNorm(geometryIdentity: String)
  case L1
  case GroupL21
  case GroupL2(groupsIdentity: String)
  case SparseGroup(l1Fraction: Double, groupsIdentity: String)
  case ElasticNet(l1Fraction: Double)
  case Huber(delta: Double)
  case TotalVariation
  case NuclearNorm
  case NegativeLogDet

enum ProgramFeasibleSetIr:
  case ZeroSubspace
  case NonnegativeOrthant
  case Simplex
  case Monotone(orderIdentity: String)
  case Box(lower: Double, upper: Double)
  case NormBall(radius: Double)
  case PsdCone
  case Stiefel
  case FixedSupport(indices: Vector[Int])
  case RankBounded(rank: Int)

enum ProgramFrameSymmetryIr:
  case Orthogonal
  case SignedPermutation
  case Permutation
  case Identity

final case class ProgramPenaltyV2Ir(
    target: ProgramTargetIr,
    functional: ProgramFunctionalIr,
    weight: Double,
    symmetry: ProgramFrameSymmetryIr
)

final case class ProgramConstraintV2Ir(
    target: ProgramTargetIr,
    feasibleSet: ProgramFeasibleSetIr,
    symmetry: ProgramFrameSymmetryIr
)

enum ProgramPredictionMetricIr:
  case SquaredError
  case Correlation
  case Mahalanobis(metricIdentity: String)

enum ProgramEquivalenceIr:
  case Value(tolerance: ToleranceIr)
  case Operator(domain: CoordinateIr, codomain: CoordinateIr, tolerance: ToleranceIr)
  case Subspace(projectorTolerance: ToleranceIr, principalAngleTolerance: ToleranceIr)
  case Frame(symmetry: ProgramFrameSymmetryIr, tolerance: ToleranceIr)
  case Prediction(metric: ProgramPredictionMetricIr, tolerance: ToleranceIr)
  case Objective(tolerance: ToleranceIr)

enum ProgramRepresentativeIr:
  case DeterministicSign
  case OrderedSpectrumThenSign
  case ProcrustesToReference(referenceIdentity: String)
  case PredictionMap
  case ObjectiveValueOnly

enum ProgramSolverGuaranteeIr:
  case GlobalSpectralOptimum
  case GlobalConvexOptimum
  case StationaryPoint
  case FeasiblePoint

final case class ProgramResultContractIr(
    equivalence: ProgramEquivalenceIr,
    representative: ProgramRepresentativeIr,
    guarantee: ProgramSolverGuaranteeIr,
    redundantCoordinates: Boolean = false,
    parameterGauges: Vector[String] = Vector.empty
)

final case class OperatorProgramV2Ir(
    id: String,
    parameters: Vector[ProgramFrameParameterIr],
    objective: ProgramObjectiveIr,
    normalizations: Vector[ProgramNormalizationV2Ir],
    penalties: Vector[ProgramPenaltyV2Ir],
    constraints: Vector[ProgramConstraintV2Ir],
    result: ProgramResultContractIr,
    provenance: Vector[ProvenanceEventIr]
)

final case class FunctionalFrameIr(
    parameterId: String,
    weightsIdentity: String,
    cometricIdentity: Option[String],
    scoreIdentities: Vector[String],
    axisIdentity: Option[String]
)

enum ProgramRewriteRuleIr:
  case ExactLinearReduction
  case SupportRestriction
  case QuadraticPullback
  case GeneralizedToStandardEigen
  case Whitening

final case class ProgramRewriteIr(
    id: String,
    originalProgramId: String,
    loweredProgramId: String,
    rule: ProgramRewriteRuleIr,
    inputOperators: Vector[String],
    outputOperators: Vector[String],
    proof: CertificateIr,
    remainingEquivalence: ProgramEquivalenceIr,
    provenance: Vector[ProvenanceEventIr]
)

final case class ProgramFitIr(
    programId: String,
    frames: Vector[FunctionalFrameIr],
    objectiveValue: Double,
    retainedRank: Int,
    spectralClusters: Vector[Vector[Int]],
    residualCertificates: Vector[CertificateIr],
    solverGuarantee: ProgramSolverGuaranteeIr,
    remainingEquivalence: ProgramEquivalenceIr,
    provenance: Vector[ProvenanceEventIr]
)

enum ProgramOperatorPolicyKindIr:
  case LinearShrinkage
  case LdaWithinScatterShrinkage
  case PsdRepair
  case SupportRestriction
  case GaugeFixing
  case JointBlockShrinkage
  case BlockwiseShrinkage
  case Custom(name: String)

enum ProgramPolicySelectionIr:
  case Fixed(strength: Double)
  case FoldSelected(selectorId: String, candidates: Vector[Double])

enum ProgramScaleMatchingIr:
  case None
  case MatchTrace
  case MatchDiagonalMean
  case Fixed(value: Double)

enum ProgramPolicyScopeIr:
  case SingleOperator
  case JointSystem
  case BlockwiseUnsafe

enum ProgramPreservationClaimIr:
  case PsdPreserved
  case SpdPreserved
  case BlockAdjointsPreserved
  case SharedGaugePreserved
  case SupportRestricted
  case GaugeFixed
  case EvidenceDowngraded(reason: String)

final case class ProgramOperatorPolicyIr(
    id: String,
    kind: ProgramOperatorPolicyKindIr,
    inputOperators: Vector[String],
    outputOperators: Vector[String],
    selection: ProgramPolicySelectionIr,
    scaleMatching: ProgramScaleMatchingIr,
    scope: ProgramPolicyScopeIr,
    preservation: Vector[ProgramPreservationClaimIr],
    provenance: Vector[ProvenanceEventIr]
)

enum ProgramSplitMethodIr:
  case PrimalDual
  case Admm
  case AugmentedLagrangian
  case Conic

enum ProgramLoweredTermIr:
  case Penalty(index: Int)
  case Constraint(index: Int)

enum ProgramAuxiliaryEquationIr:
  case TargetCopy
  case LatentGroupSum(groupsIdentity: String)

final case class ProgramAuxiliaryConstraintIr(
    variableId: String,
    target: ProgramTargetIr,
    equation: ProgramAuxiliaryEquationIr
)

final case class ProgramCompositeLoweringIr(
    id: String,
    programId: String,
    term: ProgramLoweredTermIr,
    targetOperator: String,
    method: ProgramSplitMethodIr,
    availableCapabilities: Vector[ProgramSplitMethodIr],
    auxiliary: ProgramAuxiliaryConstraintIr,
    provenance: Vector[ProvenanceEventIr]
)

final case class OperatorProgramDocumentIr(
    schema: String,
    spaces: Vector[SpaceIr],
    operators: Vector[ProgramOpIr],
    programs: Vector[OperatorProgramV2Ir],
    rewrites: Vector[ProgramRewriteIr],
    fits: Vector[ProgramFitIr],
    operatorPolicies: Vector[ProgramOperatorPolicyIr] = Vector.empty,
    compositeLowerings: Vector[ProgramCompositeLoweringIr] = Vector.empty
)

object OperatorProgramDocumentIr:
  val schemaV02: String = "scalafim-operator-program-ir/0.2"

  val empty: OperatorProgramDocumentIr =
    OperatorProgramDocumentIr(
      schemaV02,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

object ProgramSemanticIr:
  def operator[From <: Coordinate, To <: Coordinate, R <: OperatorRoleTag, E <: OperatorEvidence](
      id: String,
      value: Op[From, To, R, E],
      derivation: ProgramOperatorDerivationIr,
      gauge: ProgramGaugeIr = ProgramGaugeIr.Ungauged
  ): ProgramOpIr =
    ProgramOpIr(
      id,
      SemanticIr.coordinate(value.domain.descriptor),
      SemanticIr.coordinate(value.codomain.descriptor),
      role(value.role.value),
      ProgramOperatorEvidenceIr(
        evidenceStatus(value.certificate.status),
        value.certificate.claims.map(certificate)
      ),
      representation(value.representation),
      gauge,
      derivation,
      value.valueIdentity.stableKey,
      SemanticIr.provenance(value.provenance)
    )

  def program(id: String, value: OperatorProgram): OperatorProgramV2Ir =
    OperatorProgramV2Ir(
      id,
      value.parameters.map: parameter =>
        ProgramFrameParameterIr(
          parameter.variable.id.value,
          parameter.variable.featureSpace.descriptor.id.value,
          parameter.variable.componentSpace.descriptor.id.value,
          parameterization(parameter.kind)
        )
      ,
      objective(value.objective),
      value.normalizations.map: normalization =>
        ProgramNormalizationV2Ir(normalization.parameter.id.value, normalization.geometry.valueIdentity.stableKey)
      ,
      value.penalties.map(penalty),
      value.constraints.map(constraint),
      result(value.resultSemantics),
      SemanticIr.provenance(value.provenance)
    )

  def operatorPolicy(value: OperatorPolicyRecord): ProgramOperatorPolicyIr =
    ProgramOperatorPolicyIr(
      value.id.stringValue,
      operatorPolicyKind(value.kind),
      value.inputIdentities.map(_.stableKey),
      value.outputIdentities.map(_.stableKey),
      value.selection match
        case PolicySelection.Fixed(strength) => ProgramPolicySelectionIr.Fixed(strength.value)
        case PolicySelection.FoldSelected(hook) =>
          ProgramPolicySelectionIr.FoldSelected(hook.id.stringValue, hook.candidates.map(_.value)),
      value.scaleMatching match
        case ScaleMatching.None => ProgramScaleMatchingIr.None
        case ScaleMatching.MatchTrace => ProgramScaleMatchingIr.MatchTrace
        case ScaleMatching.MatchDiagonalMean => ProgramScaleMatchingIr.MatchDiagonalMean
        case ScaleMatching.Fixed(current) => ProgramScaleMatchingIr.Fixed(current),
      value.scope match
        case PolicyScope.SingleOperator => ProgramPolicyScopeIr.SingleOperator
        case PolicyScope.JointSystem => ProgramPolicyScopeIr.JointSystem
        case PolicyScope.BlockwiseUnsafe => ProgramPolicyScopeIr.BlockwiseUnsafe,
      value.preservation.map:
        case PreservationClaim.PsdPreserved => ProgramPreservationClaimIr.PsdPreserved
        case PreservationClaim.SpdPreserved => ProgramPreservationClaimIr.SpdPreserved
        case PreservationClaim.BlockAdjointsPreserved => ProgramPreservationClaimIr.BlockAdjointsPreserved
        case PreservationClaim.SharedGaugePreserved => ProgramPreservationClaimIr.SharedGaugePreserved
        case PreservationClaim.SupportRestricted => ProgramPreservationClaimIr.SupportRestricted
        case PreservationClaim.GaugeFixed => ProgramPreservationClaimIr.GaugeFixed
        case PreservationClaim.EvidenceDowngraded(reason) =>
          ProgramPreservationClaimIr.EvidenceDowngraded(reason)
      ,
      SemanticIr.provenance(value.provenance)
    )

  def compositePenaltyLowering(
      id: String,
      programId: String,
      penaltyIndex: Int,
      value: CompositePenaltyPlan[?, ?],
      availableCapabilities: Vector[SplitMethod]
  ): ProgramCompositeLoweringIr =
    ProgramCompositeLoweringIr(
      id,
      programId,
      ProgramLoweredTermIr.Penalty(penaltyIndex),
      value.targetOperator.valueIdentity.stableKey,
      splitMethod(value.method),
      availableCapabilities.map(splitMethod),
      ProgramAuxiliaryConstraintIr(
        value.auxiliary.variable.stringValue,
        target(value.auxiliary.target),
        value.auxiliary.equation match
          case AuxiliaryEquation.TargetCopy => ProgramAuxiliaryEquationIr.TargetCopy
          case AuxiliaryEquation.LatentGroupSum(groups) =>
            ProgramAuxiliaryEquationIr.LatentGroupSum(groups.stableKey)
      ),
      SemanticIr.provenance(value.provenance)
    )

  def compositeConstraintLowering(
      id: String,
      programId: String,
      constraintIndex: Int,
      value: CompositeConstraintPlan[?, ?],
      availableCapabilities: Vector[SplitMethod],
      provenance: SemanticProvenance
  ): ProgramCompositeLoweringIr =
    ProgramCompositeLoweringIr(
      id,
      programId,
      ProgramLoweredTermIr.Constraint(constraintIndex),
      value.targetOperator.valueIdentity.stableKey,
      splitMethod(value.method),
      availableCapabilities.map(splitMethod),
      ProgramAuxiliaryConstraintIr(
        value.auxiliary.variable.stringValue,
        target(value.auxiliary.target),
        ProgramAuxiliaryEquationIr.TargetCopy
      ),
      SemanticIr.provenance(provenance)
    )

  private def splitMethod(value: SplitMethod): ProgramSplitMethodIr =
    value match
      case SplitMethod.PrimalDual => ProgramSplitMethodIr.PrimalDual
      case SplitMethod.Admm => ProgramSplitMethodIr.Admm
      case SplitMethod.AugmentedLagrangian => ProgramSplitMethodIr.AugmentedLagrangian
      case SplitMethod.Conic => ProgramSplitMethodIr.Conic

  private def operatorPolicyKind(value: String): ProgramOperatorPolicyKindIr =
    value match
      case "linear-shrinkage" => ProgramOperatorPolicyKindIr.LinearShrinkage
      case "lda-within-scatter-shrinkage" => ProgramOperatorPolicyKindIr.LdaWithinScatterShrinkage
      case "nearest-psd-repair" => ProgramOperatorPolicyKindIr.PsdRepair
      case "support-restriction" => ProgramOperatorPolicyKindIr.SupportRestriction
      case "trace-one-gauge" => ProgramOperatorPolicyKindIr.GaugeFixing
      case "joint-block-shrinkage" => ProgramOperatorPolicyKindIr.JointBlockShrinkage
      case "blockwise-shrinkage" => ProgramOperatorPolicyKindIr.BlockwiseShrinkage
      case other => ProgramOperatorPolicyKindIr.Custom(other)

  private def role(value: OperatorRole): ProgramOperatorRoleIr =
    value match
      case OperatorRole.Table => ProgramOperatorRoleIr.Table
      case OperatorRole.Metric => ProgramOperatorRoleIr.Metric
      case OperatorRole.Cometric => ProgramOperatorRoleIr.Cometric
      case OperatorRole.Covariance => ProgramOperatorRoleIr.Covariance
      case OperatorRole.Scatter => ProgramOperatorRoleIr.Scatter
      case OperatorRole.Penalty => ProgramOperatorRoleIr.Penalty
      case OperatorRole.Kernel => ProgramOperatorRoleIr.Kernel
      case OperatorRole.RowLink => ProgramOperatorRoleIr.RowLink
      case OperatorRole.Frame => ProgramOperatorRoleIr.Frame
      case OperatorRole.Cross => ProgramOperatorRoleIr.Cross
      case OperatorRole.Component => ProgramOperatorRoleIr.Component
      case OperatorRole.Score => ProgramOperatorRoleIr.Score
      case OperatorRole.Axis => ProgramOperatorRoleIr.Axis
      case OperatorRole.Coefficient => ProgramOperatorRoleIr.Coefficient
      case OperatorRole.ConstraintMap => ProgramOperatorRoleIr.ConstraintMap
      case OperatorRole.Composed(first, second) => ProgramOperatorRoleIr.Composed(role(first), role(second))
      case OperatorRole.Dual(of) => ProgramOperatorRoleIr.Dual(role(of))
      case OperatorRole.MetricAdjoint(of) => ProgramOperatorRoleIr.MetricAdjoint(role(of))

  private def representation(value: OperatorRepresentation): ProgramRepresentationIr =
    value match
      case OperatorRepresentation.Dense => ProgramRepresentationIr.Dense
      case OperatorRepresentation.Sparse => ProgramRepresentationIr.Sparse
      case OperatorRepresentation.Diagonal => ProgramRepresentationIr.Diagonal
      case OperatorRepresentation.Block => ProgramRepresentationIr.Block
      case OperatorRepresentation.LowRank => ProgramRepresentationIr.LowRank
      case OperatorRepresentation.Kronecker => ProgramRepresentationIr.Kronecker
      case OperatorRepresentation.LazyAffine => ProgramRepresentationIr.LazyAffine
      case OperatorRepresentation.MatrixFree => ProgramRepresentationIr.MatrixFree

  private def evidenceStatus(value: EvidenceStatus): EvidenceStatusIr =
    value match
      case EvidenceStatus.Unchecked => EvidenceStatusIr.Unchecked
      case EvidenceStatus.Certified => EvidenceStatusIr.Certified
      case EvidenceStatus.Assumed => EvidenceStatusIr.Assumed

  private def certificate(value: NumericalCertificate): CertificateIr =
    val residual =
      value.claim match
        case CertificateClaim.Symmetric(current, _) => Some(current)
        case CertificateClaim.PositiveSemidefinite(_, current, _) => Some(current)
        case CertificateClaim.PositiveDefinite(_, current, _) => Some(current)
        case CertificateClaim.Indefinite(_, _, current, _) => Some(current)
        case CertificateClaim.Rank(_, _, current, _) => Some(current)
        case CertificateClaim.Orthogonal(current, _) => Some(current)
        case CertificateClaim.Converged(_, current, _) => Some(current)
    CertificateIr(
      value.claim.property,
      value.valueIdentity.stableKey,
      tolerance(value.context.tolerance),
      value.context.norm.toString.toLowerCase,
      value.context.method,
      value.context.precision match
        case NumericalPrecision.Float32 => "float32"
        case NumericalPrecision.Float64 => "float64"
        case NumericalPrecision.Extended(label) => s"extended:$label",
      value.context.backend,
      value.context.regularization,
      residual
    )

  private def parameterization(value: ParameterizationKind): ProgramParameterizationIr =
    value match
      case ParameterizationKind.Identity => ProgramParameterizationIr.Identity
      case ParameterizationKind.KnownSupport(embedding, injective) =>
        ProgramParameterizationIr.KnownSupport(embedding.stableKey, injective)
      case ParameterizationKind.SharedBasis(basis, injective) =>
        ProgramParameterizationIr.SharedBasis(basis.stableKey, injective)
      case ParameterizationKind.FixedRank(rank, gauge) =>
        ProgramParameterizationIr.FixedRank(rank.value, gauge.toString)
      case ParameterizationKind.BlockDiagonal(blocks) =>
        ProgramParameterizationIr.BlockDiagonal(blocks.map(_.value))
      case ParameterizationKind.NullSpace(basis, current) =>
        ProgramParameterizationIr.NullSpace(basis.stableKey, tolerance(current))

  private def objective(value: BaseObjective): ProgramObjectiveIr =
    value match
      case BaseObjective.MaximizeTrace(expression) =>
        ProgramObjectiveIr.MaximizeTrace(expression.parameter.id.value, expression.secondOrder.valueIdentity.stableKey)
      case BaseObjective.MaximizeCrossTrace(expression) =>
        ProgramObjectiveIr.MaximizeCrossTrace(
          expression.source.id.value,
          expression.target.id.value,
          expression.secondOrder.valueIdentity.stableKey
        )
      case BaseObjective.GeneralizedRayleigh(numerator, denominator) =>
        ProgramObjectiveIr.GeneralizedRayleigh(
          numerator.parameter.id.value,
          numerator.secondOrder.valueIdentity.stableKey,
          denominator.secondOrder.valueIdentity.stableKey
        )
      case BaseObjective.TraceRatio(numerator, denominator) =>
        ProgramObjectiveIr.TraceRatio(
          numerator.parameter.id.value,
          numerator.secondOrder.valueIdentity.stableKey,
          denominator.secondOrder.valueIdentity.stableKey
        )
      case BaseObjective.RatioTrace(numerator, denominator) =>
        ProgramObjectiveIr.RatioTrace(
          numerator.parameter.id.value,
          numerator.secondOrder.valueIdentity.stableKey,
          denominator.secondOrder.valueIdentity.stableKey
        )
      case BaseObjective.MinimizeDisagreement(expression) =>
        ProgramObjectiveIr.MinimizeDisagreement(expression.parameter.id.value, expression.secondOrder.valueIdentity.stableKey)
      case BaseObjective.SequentialCrossRegression(cross, predictor) =>
        ProgramObjectiveIr.SequentialCrossRegression(
          cross.source.id.value,
          cross.target.id.value,
          cross.secondOrder.valueIdentity.stableKey,
          predictor.secondOrder.valueIdentity.stableKey
        )

  private def target(value: TargetExpression): ProgramTargetIr =
    ProgramTargetIr(
      value.parameter.value,
      value.capability match
        case TargetCapability.Linear => ProgramTargetCapabilityIr.Linear
        case TargetCapability.Affine => ProgramTargetCapabilityIr.Affine
        case TargetCapability.Smooth => ProgramTargetCapabilityIr.Smooth
        case TargetCapability.General => ProgramTargetCapabilityIr.General,
      value.operation,
      value.operator.map(_.stableKey),
      value.parameters.drop(1).map(_.value),
      value.operators.drop(1).map(_.stableKey),
      symmetry(value.equivariance)
    )

  private def penalty(value: PenaltyTerm): ProgramPenaltyV2Ir =
    ProgramPenaltyV2Ir(target(value.target), functional(value.functional), value.weight.value, symmetry(value.symmetry))

  private def constraint(value: ConstraintTerm): ProgramConstraintV2Ir =
    ProgramConstraintV2Ir(target(value.target), feasibleSet(value.feasibleSet), symmetry(value.symmetry))

  private def functional(value: FunctionalKind): ProgramFunctionalIr =
    value match
      case FunctionalKind.SquaredNorm(geometry) => ProgramFunctionalIr.SquaredNorm(geometry.stableKey)
      case FunctionalKind.L1 => ProgramFunctionalIr.L1
      case FunctionalKind.GroupL21 => ProgramFunctionalIr.GroupL21
      case FunctionalKind.GroupL2(groups) => ProgramFunctionalIr.GroupL2(groups.stableKey)
      case FunctionalKind.SparseGroup(fraction, groups) =>
        ProgramFunctionalIr.SparseGroup(fraction.value, groups.stableKey)
      case FunctionalKind.ElasticNet(fraction) => ProgramFunctionalIr.ElasticNet(fraction.value)
      case FunctionalKind.Huber(delta) => ProgramFunctionalIr.Huber(delta.value)
      case FunctionalKind.TotalVariation => ProgramFunctionalIr.TotalVariation
      case FunctionalKind.NuclearNorm => ProgramFunctionalIr.NuclearNorm
      case FunctionalKind.NegativeLogDet => ProgramFunctionalIr.NegativeLogDet

  private def feasibleSet(value: FeasibleSetKind): ProgramFeasibleSetIr =
    value match
      case FeasibleSetKind.ZeroSubspace => ProgramFeasibleSetIr.ZeroSubspace
      case FeasibleSetKind.NonnegativeOrthant => ProgramFeasibleSetIr.NonnegativeOrthant
      case FeasibleSetKind.Simplex => ProgramFeasibleSetIr.Simplex
      case FeasibleSetKind.Monotone(order) => ProgramFeasibleSetIr.Monotone(order.stableKey)
      case FeasibleSetKind.Box(bounds) => ProgramFeasibleSetIr.Box(bounds.lower, bounds.upper)
      case FeasibleSetKind.NormBall(radius) => ProgramFeasibleSetIr.NormBall(radius.value)
      case FeasibleSetKind.PsdCone => ProgramFeasibleSetIr.PsdCone
      case FeasibleSetKind.Stiefel => ProgramFeasibleSetIr.Stiefel
      case FeasibleSetKind.FixedSupport(indices) => ProgramFeasibleSetIr.FixedSupport(indices.indices)
      case FeasibleSetKind.RankBounded(rank) => ProgramFeasibleSetIr.RankBounded(rank.value)

  private def result(value: ResultSemantics): ProgramResultContractIr =
    ProgramResultContractIr(
      equivalence(value.equivalence),
      value.representative match
        case RepresentativeRule.DeterministicSign => ProgramRepresentativeIr.DeterministicSign
        case RepresentativeRule.OrderedSpectrumThenSign => ProgramRepresentativeIr.OrderedSpectrumThenSign
        case RepresentativeRule.ProcrustesToReference(reference) =>
          ProgramRepresentativeIr.ProcrustesToReference(reference.stableKey)
        case RepresentativeRule.PredictionMap => ProgramRepresentativeIr.PredictionMap
        case RepresentativeRule.ObjectiveValueOnly => ProgramRepresentativeIr.ObjectiveValueOnly,
      guarantee(value.guarantee),
      value.parameterIdentifiability.redundantCoordinates,
      value.parameterIdentifiability.gauges.map(_.toString)
    )

  def equivalence(value: ResultEquivalence): ProgramEquivalenceIr =
    value match
      case ResultEquivalence.ValueEquivalent(current) => ProgramEquivalenceIr.Value(tolerance(current))
      case ResultEquivalence.OperatorEquivalent(domain, codomain, current) =>
        ProgramEquivalenceIr.Operator(SemanticIr.coordinate(domain), SemanticIr.coordinate(codomain), tolerance(current))
      case ResultEquivalence.SubspaceEquivalent(projector, angle) =>
        ProgramEquivalenceIr.Subspace(tolerance(projector), tolerance(angle))
      case ResultEquivalence.FrameEquivalent(group, current) =>
        ProgramEquivalenceIr.Frame(symmetry(group), tolerance(current))
      case ResultEquivalence.PredictionEquivalent(metric, current) =>
        ProgramEquivalenceIr.Prediction(
          metric match
            case PredictionMetric.SquaredError => ProgramPredictionMetricIr.SquaredError
            case PredictionMetric.Correlation => ProgramPredictionMetricIr.Correlation
            case PredictionMetric.Mahalanobis(identity) => ProgramPredictionMetricIr.Mahalanobis(identity.stableKey),
          tolerance(current)
        )
      case ResultEquivalence.ObjectiveEquivalent(current) => ProgramEquivalenceIr.Objective(tolerance(current))

  def guarantee(value: SolverGuarantee): ProgramSolverGuaranteeIr =
    value match
      case SolverGuarantee.GlobalSpectralOptimum => ProgramSolverGuaranteeIr.GlobalSpectralOptimum
      case SolverGuarantee.GlobalConvexOptimum => ProgramSolverGuaranteeIr.GlobalConvexOptimum
      case SolverGuarantee.StationaryPoint => ProgramSolverGuaranteeIr.StationaryPoint
      case SolverGuarantee.FeasiblePoint => ProgramSolverGuaranteeIr.FeasiblePoint

  private def symmetry(value: FrameSymmetry): ProgramFrameSymmetryIr =
    value match
      case FrameSymmetry.Orthogonal => ProgramFrameSymmetryIr.Orthogonal
      case FrameSymmetry.SignedPermutation => ProgramFrameSymmetryIr.SignedPermutation
      case FrameSymmetry.Permutation => ProgramFrameSymmetryIr.Permutation
      case FrameSymmetry.Identity => ProgramFrameSymmetryIr.Identity

  private def tolerance(value: CertificateTolerance): ToleranceIr =
    ToleranceIr(value.absolute, value.relative)
