package scalafim.multivar.ir

import gale.linalg.DMat
import scalafim.multivar.*

object SemanticIr:
  def space(value: MvSpace): SpaceIr =
    SpaceIr(value.id.value, spaceRole(value.role), value.size)

  def coordinate(value: CoordinateDescriptor): CoordinateIr =
    CoordinateIr(
      value.space.id.value,
      value.variance match
        case CoordinateVariance.Primal => VarianceIr.Primal
        case CoordinateVariance.Dual => VarianceIr.Dual
    )

  def operator[From <: Coordinate, To <: Coordinate](
      id: String,
      role: OperatorRoleIr,
      value: Lin[From, To]
  ): Either[IrError, OperatorIr] =
    value(DMat.eye(value.cols)).left.map { error =>
      IrError(RejectionCategory.Malformed, s"operators.$id.payload", error.message)
    }.flatMap { matrix =>
      PayloadIrFactory.inlineDense(matrix.rows, matrix.cols, matrix.valuesRowMajor.toVector).map { payload =>
        OperatorIr(
          id,
          role,
          coordinate(value.domain.descriptor),
          coordinate(value.codomain.descriptor),
          value.descriptor.representation.label,
          payload,
          value.valueIdentity.stableKey,
          provenance(value.provenance)
        )
      }
    }

  def operatorExternal[From <: Coordinate, To <: Coordinate](
      id: String,
      role: OperatorRoleIr,
      value: Lin[From, To],
      uri: String,
      mediaType: String,
      sha256: String
  ): Either[IrError, OperatorIr] =
    PayloadIrFactory.external(uri, mediaType, value.rows, value.cols, sha256).map { payload =>
      OperatorIr(
        id,
        role,
        coordinate(value.domain.descriptor),
        coordinate(value.codomain.descriptor),
        value.descriptor.representation.label,
        payload,
        value.valueIdentity.stableKey,
        provenance(value.provenance)
      )
    }

  def form(
      id: String,
      value: FormDescriptor,
      operatorId: String,
      scaleSemantics: ScaleSemanticsIr = ScaleSemanticsIr.AbsoluteMetric
  ): FormIr =
    FormIr(
      id,
      formRole(value.role),
      value.space.id.value,
      operatorId,
      value.structure match
        case FormStructure.General => FormStructureIr.General
        case FormStructure.Symmetric => FormStructureIr.Symmetric,
      value.positivity match
        case PositivityStatus.Unchecked => PositivityIr.Unchecked
        case PositivityStatus.Psd => PositivityIr.Psd
        case PositivityStatus.Spd => PositivityIr.Spd
        case PositivityStatus.Indefinite => PositivityIr.Indefinite,
      value.evidence match
        case EvidenceStatus.Unchecked => EvidenceStatusIr.Unchecked
        case EvidenceStatus.Certified => EvidenceStatusIr.Certified
        case EvidenceStatus.Assumed => EvidenceStatusIr.Assumed,
      scaleSemantics,
      value.certificates.map(certificate)
    )

  def measure[S <: SemanticSpace](id: String, value: RowMeasure[S]): Either[IrError, MeasureIr] =
    PayloadIrFactory.inlineDense(value.weights.length, 1, value.weights.toVector).map { payload =>
      val normalization =
        value.descriptor.normalization match
          case RowMeasureNormalization.UnitMass(originalMass) => MeasureNormalizationIr.UnitMass(originalMass)
      MeasureIr(
        id,
        value.space.id.value,
        payload,
        normalization,
        value.descriptor.valueIdentity.stableKey
      )
    }

  def relationship(
      id: String,
      value: RowRelationshipDescriptor,
      operatorId: String,
      provenanceValue: SemanticProvenance
  ): Either[IrError, RelationshipIr] =
    val marginals = value.marginals match
      case None => Right(None)
      case Some(current) =>
        for
          left <- PayloadIrFactory.inlineDense(current.left.length, 1, current.left.toVector)
          right <- PayloadIrFactory.inlineDense(current.right.length, 1, current.right.toVector)
        yield Some(MarginalsIr(left, right, current.totalMass))
    marginals.map { marginalValue =>
      RelationshipIr(
        id,
        relationshipKind(value.kind),
        operatorId,
        support(value.support),
        relationshipNormalization(value.normalization),
        marginalValue,
        value.origin match
          case AlignmentOrigin.ObservedKeys => AlignmentOriginIr.ObservedKeys
          case AlignmentOrigin.ExternallySupplied => AlignmentOriginIr.ExternallySupplied
          case AlignmentOrigin.UnsafeAssumption => AlignmentOriginIr.UnsafeAssumption,
        provenance(provenanceValue)
      )
    }

  def objective(id: String, value: ObjectiveDefinition): ObjectiveIr =
    ObjectiveIr(
      id,
      value.formula match
        case ObjectiveFormula.PairwiseAssociation => ObjectiveFormulaIr.PairwiseAssociation
        case ObjectiveFormula.QuadraticDisagreement => ObjectiveFormulaIr.QuadraticDisagreement
        case ObjectiveFormula.HardScoreAgreement => ObjectiveFormulaIr.HardScoreAgreement,
      value.renderedFormula,
      value.constraints,
      value.normalization match
        case ObjectiveNormalization.DirectSumMetricOrthonormal => ObjectiveNormalizationIr.DirectSumMetricOrthonormal
        case ObjectiveNormalization.PerViewMetricOrthonormal => ObjectiveNormalizationIr.PerViewMetricOrthonormal
        case ObjectiveNormalization.ConstraintOnly => ObjectiveNormalizationIr.ConstraintOnly,
      value.solverFormulation match
        case SolverFormulation.SymmetricFeatureEigen => SolverFormulationIr.SymmetricFeatureEigen
        case SolverFormulation.QuadraticPenalty => SolverFormulationIr.QuadraticPenalty
        case SolverFormulation.LinearEquality => SolverFormulationIr.LinearEquality,
      value.solvedToReportedRelationship match
        case SolvedToReportedRelationship.Identical => SolvedToReportedIr.Identical
        case SolvedToReportedRelationship.Scale(factor) => SolvedToReportedIr.Scale(factor)
        case SolvedToReportedRelationship.Transform(description) => SolvedToReportedIr.Transform(description)
    )

  def provenance(value: SemanticProvenance): Vector[ProvenanceEventIr] =
    value.events.map {
      case SemanticProvenanceEvent.Source(label) => ProvenanceEventIr.Source(label)
      case SemanticProvenanceEvent.Adapted(adapter) => ProvenanceEventIr.Adapted(adapter)
      case SemanticProvenanceEvent.Derived(operation, inputs) =>
        ProvenanceEventIr.Derived(operation, inputs.map(_.stableKey))
      case SemanticProvenanceEvent.Certified(property, method) =>
        ProvenanceEventIr.Certified(property, method)
      case SemanticProvenanceEvent.UnsafeAssumption(property, reason) =>
        ProvenanceEventIr.UnsafeAssumption(property, reason)
    }

  def singularity(value: SingularGeometryPolicy): SingularityPolicyIr =
    value match
      case SingularGeometryPolicy.RejectSingularGeometry => SingularityPolicyIr.Reject
      case SingularGeometryPolicy.RestrictToSupport(threshold) =>
        SingularityPolicyIr.RestrictToSupport(threshold.toDouble)
      case SingularGeometryPolicy.RegularizeWith(amount) => SingularityPolicyIr.Regularize(amount.toDouble)
      case SingularGeometryPolicy.WorkInQuotientSpace(threshold) => SingularityPolicyIr.Quotient(threshold.toDouble)

  private def certificate(value: NumericalCertificate): CertificateIr =
    val residual =
      value.claim match
        case CertificateClaim.Symmetric(residual, _) => Some(residual)
        case CertificateClaim.PositiveSemidefinite(_, residual, _) => Some(residual)
        case CertificateClaim.PositiveDefinite(_, residual, _) => Some(residual)
        case CertificateClaim.Indefinite(_, _, residual, _) => Some(residual)
        case CertificateClaim.Rank(_, _, residual, _) => Some(residual)
        case CertificateClaim.Orthogonal(residual, _) => Some(residual)
        case CertificateClaim.Converged(_, residual, _) => Some(residual)
        case CertificateClaim.SolverTrace(_, residual, _, _) => Some(residual)
    CertificateIr(
      value.claim.property,
      value.valueIdentity.stableKey,
      ToleranceIr(value.context.tolerance.absolute, value.context.tolerance.relative),
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

  private def support(value: RelationshipSupport): RelationshipSupportIr =
    RelationshipSupportIr(
      value.matchedMass,
      value.unmatchedSourceMass,
      value.unmatchedTargetMass,
      value.uncertainMass,
      value.excludedMass,
      value.structuralZeroCount,
      value.cardinality match
        case CardinalityStructure.OneToOne => CardinalityIr.OneToOne
        case CardinalityStructure.OneToMany => CardinalityIr.OneToMany
        case CardinalityStructure.ManyToOne => CardinalityIr.ManyToOne
        case CardinalityStructure.ManyToMany => CardinalityIr.ManyToMany,
      value.duplicateKeys,
      value.everySourceRepresented,
      value.everyTargetRepresented
    )

  private def relationshipKind(value: RowRelationshipKind): RelationshipKindIr =
    value match
      case RowRelationshipKind.SameEntityEvidence => RelationshipKindIr.SameEntityEvidence
      case RowRelationshipKind.ExactBijection => RelationshipKindIr.ExactBijection
      case RowRelationshipKind.PartialInjection => RelationshipKindIr.PartialInjection
      case RowRelationshipKind.IncidenceMap => RelationshipKindIr.IncidenceMap
      case RowRelationshipKind.AggregationMap => RelationshipKindIr.AggregationMap
      case RowRelationshipKind.NonnegativeCoupling => RelationshipKindIr.NonnegativeCoupling
      case RowRelationshipKind.ProbabilisticCoupling => RelationshipKindIr.ProbabilisticCoupling
      case RowRelationshipKind.SignedRowLink => RelationshipKindIr.SignedRowLink
      case RowRelationshipKind.IncidenceInducedLink => RelationshipKindIr.IncidenceInducedLink
      case RowRelationshipKind.HubInducedLink => RelationshipKindIr.HubInducedLink

  private def relationshipNormalization(value: RelationshipNormalization): RelationshipNormalizationIr =
    value match
      case RelationshipNormalization.Unnormalized => RelationshipNormalizationIr.Unnormalized
      case RelationshipNormalization.UnitMass => RelationshipNormalizationIr.UnitMass
      case RelationshipNormalization.SourceMassPreserving => RelationshipNormalizationIr.SourceMassPreserving
      case RelationshipNormalization.TargetMassPreserving => RelationshipNormalizationIr.TargetMassPreserving
      case RelationshipNormalization.DoublyStochastic => RelationshipNormalizationIr.DoublyStochastic
      case RelationshipNormalization.ConditionalOnSource => RelationshipNormalizationIr.ConditionalOnSource
      case RelationshipNormalization.ConditionalOnTarget => RelationshipNormalizationIr.ConditionalOnTarget

  private def formRole(value: FormRole): FormRoleIr =
    value match
      case FormRole.BilinearForm => FormRoleIr.Bilinear
      case FormRole.SymmetricForm => FormRoleIr.Symmetric
      case FormRole.SemiMetric => FormRoleIr.SemiMetric
      case FormRole.Metric => FormRoleIr.Metric
      case FormRole.IndefiniteForm => FormRoleIr.Indefinite
      case FormRole.Cometric => FormRoleIr.Cometric
      case FormRole.Penalty => FormRoleIr.Penalty
      case FormRole.Kernel => FormRoleIr.Kernel
      case FormRole.Covariance => FormRoleIr.Covariance
      case FormRole.Precision => FormRoleIr.Precision

  private def spaceRole(value: SpaceRole): SpaceRoleIr =
    value match
      case SpaceRole.Samples => SpaceRoleIr.Samples
      case SpaceRole.Observed => SpaceRoleIr.Observed
      case SpaceRole.Latent => SpaceRoleIr.Latent
      case SpaceRole.Kernel => SpaceRoleIr.Kernel
      case SpaceRole.Block => SpaceRoleIr.Block
