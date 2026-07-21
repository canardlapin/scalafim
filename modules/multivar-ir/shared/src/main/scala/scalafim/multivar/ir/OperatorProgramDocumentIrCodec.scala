package scalafim.multivar.ir

object OperatorProgramDocumentIrCodec:
  def encode(document: OperatorProgramDocumentIr): String =
    IrJson.render(ProgramIrEncoder.document(document))

  def decode(text: String): Either[IrError, OperatorProgramDocumentIr] =
    IrJson.parse(text).flatMap(ProgramIrDecoder.document).flatMap(OperatorProgramIrValidator.validate)

private object ProgramIrEncoder:
  import IrJson.*

  def document(value: OperatorProgramDocumentIr): IrJson =
    obj(
      "schema" -> Str(value.schema),
      "spaces" -> arr(value.spaces.map(space)),
      "operators" -> arr(value.operators.map(operator)),
      "programs" -> arr(value.programs.map(program)),
      "rewrites" -> arr(value.rewrites.map(rewrite)),
      "fits" -> arr(value.fits.map(fit)),
      "operator_policies" -> arr(value.operatorPolicies.map(operatorPolicy)),
      "composite_lowerings" -> arr(value.compositeLowerings.map(compositeLowering))
    )

  private def space(value: SpaceIr): IrJson =
    obj("id" -> Str(value.id), "role" -> Str(spaceRole(value.role)), "dimension" -> Num(value.dimension))

  private def coordinate(value: CoordinateIr): IrJson =
    obj("space_id" -> Str(value.spaceId), "variance" -> Str(variance(value.variance)))

  private def operator(value: ProgramOpIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "domain" -> coordinate(value.domain),
      "codomain" -> coordinate(value.codomain),
      "role" -> operatorRole(value.role),
      "evidence" -> evidence(value.evidence),
      "representation" -> Str(representation(value.representation)),
      "gauge" -> gauge(value.gauge),
      "derivation" -> derivation(value.derivation),
      "value_identity" -> Str(value.valueIdentity),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def operatorRole(value: ProgramOperatorRoleIr): IrJson =
    value match
      case ProgramOperatorRoleIr.Composed(first, second) =>
        obj("kind" -> Str("composed"), "first" -> operatorRole(first), "second" -> operatorRole(second))
      case ProgramOperatorRoleIr.Dual(of) => obj("kind" -> Str("dual"), "of" -> operatorRole(of))
      case ProgramOperatorRoleIr.MetricAdjoint(of) => obj("kind" -> Str("metric_adjoint"), "of" -> operatorRole(of))
      case other => obj("kind" -> Str(simpleRole(other)))

  private def evidence(value: ProgramOperatorEvidenceIr): IrJson =
    obj(
      "status" -> Str(evidenceStatus(value.status)),
      "certificates" -> arr(value.certificates.map(certificate))
    )

  private def gauge(value: ProgramGaugeIr): IrJson =
    value match
      case ProgramGaugeIr.Ungauged => obj("kind" -> Str("ungauged"))
      case ProgramGaugeIr.Shape(id) => obj("kind" -> Str("shape"), "id" -> Str(id))
      case ProgramGaugeIr.Orthonormal(metric) =>
        obj("kind" -> Str("orthonormal"), "metric_identity" -> Str(metric))

  private def derivation(value: ProgramOperatorDerivationIr): IrJson =
    value match
      case ProgramOperatorDerivationIr.Source => obj("kind" -> Str("source"))
      case ProgramOperatorDerivationIr.SecondOrder(source, relationship, target) =>
        obj(
          "kind" -> Str("second_order"),
          "source_table" -> Str(source),
          "relationship" -> Str(relationship),
          "target_table" -> Str(target)
        )
      case ProgramOperatorDerivationIr.Compress(source, secondOrder, target) =>
        obj(
          "kind" -> Str("compress"),
          "source_frame" -> Str(source),
          "second_order" -> Str(secondOrder),
          "target_frame" -> Str(target)
        )
      case ProgramOperatorDerivationIr.Scores(frame, table) =>
        obj("kind" -> Str("scores"), "frame" -> Str(frame), "table" -> Str(table))
      case ProgramOperatorDerivationIr.Axes(frame, cometric) =>
        obj("kind" -> Str("axes"), "frame" -> Str(frame), "cometric" -> Str(cometric))
      case ProgramOperatorDerivationIr.Lowered(rule, inputs) =>
        obj("kind" -> Str("lowered"), "rule" -> Str(rule), "inputs" -> arr(inputs.map(Str.apply)))

  private def program(value: OperatorProgramV2Ir): IrJson =
    obj(
      "id" -> Str(value.id),
      "parameters" -> arr(value.parameters.map(parameter)),
      "objective" -> objective(value.objective),
      "normalizations" -> arr(value.normalizations.map(normalization)),
      "penalties" -> arr(value.penalties.map(penalty)),
      "constraints" -> arr(value.constraints.map(constraint)),
      "result" -> result(value.result),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def parameter(value: ProgramFrameParameterIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "feature_space_id" -> Str(value.featureSpaceId),
      "component_space_id" -> Str(value.componentSpaceId),
      "parameterization" -> parameterization(value.parameterization)
    )

  private def parameterization(value: ProgramParameterizationIr): IrJson =
    value match
      case ProgramParameterizationIr.Identity => obj("kind" -> Str("identity"))
      case ProgramParameterizationIr.KnownSupport(embedding, injective) =>
        obj("kind" -> Str("known_support"), "embedding_identity" -> Str(embedding), "injective" -> Bool(injective))
      case ProgramParameterizationIr.SharedBasis(basis, injective) =>
        obj("kind" -> Str("shared_basis"), "basis_identity" -> Str(basis), "injective" -> Bool(injective))
      case ProgramParameterizationIr.FixedRank(rank, gauge) =>
        obj("kind" -> Str("fixed_rank"), "rank" -> Num(rank), "gauge" -> Str(gauge))
      case ProgramParameterizationIr.BlockDiagonal(blocks) =>
        obj("kind" -> Str("block_diagonal"), "blocks" -> arr(blocks.map(Str.apply)))
      case ProgramParameterizationIr.NullSpace(basis, current) =>
        obj("kind" -> Str("null_space"), "basis_identity" -> Str(basis), "tolerance" -> tolerance(current))

  private def objective(value: ProgramObjectiveIr): IrJson =
    value match
      case ProgramObjectiveIr.MaximizeTrace(parameter, operator) =>
        obj("kind" -> Str("maximize_trace"), "parameter_id" -> Str(parameter), "operator_identity" -> Str(operator))
      case ProgramObjectiveIr.MaximizeCrossTrace(source, target, operator) =>
        obj(
          "kind" -> Str("maximize_cross_trace"),
          "source_parameter_id" -> Str(source),
          "target_parameter_id" -> Str(target),
          "operator_identity" -> Str(operator)
        )
      case ProgramObjectiveIr.GeneralizedRayleigh(parameter, numerator, denominator) =>
        ratioObjective("generalized_rayleigh", parameter, numerator, denominator)
      case ProgramObjectiveIr.TraceRatio(parameter, numerator, denominator) =>
        ratioObjective("trace_ratio", parameter, numerator, denominator)
      case ProgramObjectiveIr.RatioTrace(parameter, numerator, denominator) =>
        ratioObjective("ratio_trace", parameter, numerator, denominator)
      case ProgramObjectiveIr.MinimizeDisagreement(parameter, operator) =>
        obj("kind" -> Str("minimize_disagreement"), "parameter_id" -> Str(parameter), "operator_identity" -> Str(operator))
      case ProgramObjectiveIr.SequentialCrossRegression(source, target, cross, predictor) =>
        obj(
          "kind" -> Str("sequential_cross_regression"),
          "source_parameter_id" -> Str(source),
          "target_parameter_id" -> Str(target),
          "cross_identity" -> Str(cross),
          "predictor_identity" -> Str(predictor)
        )

  private def ratioObjective(kind: String, parameter: String, numerator: String, denominator: String): IrJson =
    obj(
      "kind" -> Str(kind),
      "parameter_id" -> Str(parameter),
      "numerator_identity" -> Str(numerator),
      "denominator_identity" -> Str(denominator)
    )

  private def normalization(value: ProgramNormalizationV2Ir): IrJson =
    obj("parameter_id" -> Str(value.parameterId), "operator_identity" -> Str(value.operatorIdentity))

  private def target(value: ProgramTargetIr): IrJson =
    obj(
      "parameter_id" -> Str(value.parameterId),
      "capability" -> Str(targetCapability(value.capability)),
      "operation" -> Str(value.operation),
      "operator_identity" -> value.operatorIdentity.fold[IrJson](Null)(Str.apply),
      "additional_parameter_ids" -> arr(value.additionalParameterIds.map(Str.apply)),
      "additional_operator_identities" -> arr(value.additionalOperatorIdentities.map(Str.apply)),
      "equivariance" -> Str(symmetry(value.equivariance))
    )

  private def penalty(value: ProgramPenaltyV2Ir): IrJson =
    obj(
      "target" -> target(value.target),
      "functional" -> functional(value.functional),
      "weight" -> Num(value.weight),
      "symmetry" -> Str(symmetry(value.symmetry))
    )

  private def functional(value: ProgramFunctionalIr): IrJson =
    value match
      case ProgramFunctionalIr.SquaredNorm(geometry) =>
        obj("kind" -> Str("squared_norm"), "geometry_identity" -> Str(geometry))
      case ProgramFunctionalIr.ElasticNet(fraction) =>
        obj("kind" -> Str("elastic_net"), "l1_fraction" -> Num(fraction))
      case ProgramFunctionalIr.GroupL2(groups) =>
        obj("kind" -> Str("group_l2"), "groups_identity" -> Str(groups))
      case ProgramFunctionalIr.SparseGroup(fraction, groups) =>
        obj(
          "kind" -> Str("sparse_group"),
          "l1_fraction" -> Num(fraction),
          "groups_identity" -> Str(groups)
        )
      case ProgramFunctionalIr.Huber(delta) => obj("kind" -> Str("huber"), "delta" -> Num(delta))
      case other => obj("kind" -> Str(simpleFunctional(other)))

  private def constraint(value: ProgramConstraintV2Ir): IrJson =
    obj(
      "target" -> target(value.target),
      "feasible_set" -> feasibleSet(value.feasibleSet),
      "symmetry" -> Str(symmetry(value.symmetry))
    )

  private def feasibleSet(value: ProgramFeasibleSetIr): IrJson =
    value match
      case ProgramFeasibleSetIr.Box(lower, upper) =>
        obj("kind" -> Str("box"), "lower" -> Num(lower), "upper" -> Num(upper))
      case ProgramFeasibleSetIr.NormBall(radius) => obj("kind" -> Str("norm_ball"), "radius" -> Num(radius))
      case ProgramFeasibleSetIr.Monotone(order) =>
        obj("kind" -> Str("monotone"), "order_identity" -> Str(order))
      case ProgramFeasibleSetIr.FixedSupport(indices) =>
        obj("kind" -> Str("fixed_support"), "indices" -> arr(indices.map(value => Num(value))))
      case ProgramFeasibleSetIr.RankBounded(rank) => obj("kind" -> Str("rank_bounded"), "rank" -> Num(rank))
      case other => obj("kind" -> Str(simpleFeasibleSet(other)))

  private def result(value: ProgramResultContractIr): IrJson =
    obj(
      "equivalence" -> equivalence(value.equivalence),
      "representative" -> representative(value.representative),
      "guarantee" -> Str(guarantee(value.guarantee)),
      "redundant_coordinates" -> Bool(value.redundantCoordinates),
      "parameter_gauges" -> arr(value.parameterGauges.map(Str.apply))
    )

  private def equivalence(value: ProgramEquivalenceIr): IrJson =
    value match
      case ProgramEquivalenceIr.Value(current) =>
        obj("kind" -> Str("value"), "tolerance" -> tolerance(current))
      case ProgramEquivalenceIr.Operator(domain, codomain, current) =>
        obj(
          "kind" -> Str("operator"),
          "domain" -> coordinate(domain),
          "codomain" -> coordinate(codomain),
          "tolerance" -> tolerance(current)
        )
      case ProgramEquivalenceIr.Subspace(projector, angle) =>
        obj(
          "kind" -> Str("subspace"),
          "projector_tolerance" -> tolerance(projector),
          "principal_angle_tolerance" -> tolerance(angle)
        )
      case ProgramEquivalenceIr.Frame(group, current) =>
        obj("kind" -> Str("frame"), "symmetry" -> Str(symmetry(group)), "tolerance" -> tolerance(current))
      case ProgramEquivalenceIr.Prediction(metric, current) =>
        obj("kind" -> Str("prediction"), "metric" -> predictionMetric(metric), "tolerance" -> tolerance(current))
      case ProgramEquivalenceIr.Objective(current) =>
        obj("kind" -> Str("objective"), "tolerance" -> tolerance(current))

  private def predictionMetric(value: ProgramPredictionMetricIr): IrJson =
    value match
      case ProgramPredictionMetricIr.SquaredError => obj("kind" -> Str("squared_error"))
      case ProgramPredictionMetricIr.Correlation => obj("kind" -> Str("correlation"))
      case ProgramPredictionMetricIr.Mahalanobis(metric) =>
        obj("kind" -> Str("mahalanobis"), "metric_identity" -> Str(metric))

  private def representative(value: ProgramRepresentativeIr): IrJson =
    value match
      case ProgramRepresentativeIr.ProcrustesToReference(reference) =>
        obj("kind" -> Str("procrustes_to_reference"), "reference_identity" -> Str(reference))
      case other => obj("kind" -> Str(simpleRepresentative(other)))

  private def rewrite(value: ProgramRewriteIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "original_program_id" -> Str(value.originalProgramId),
      "lowered_program_id" -> Str(value.loweredProgramId),
      "rule" -> Str(rewriteRule(value.rule)),
      "input_operators" -> arr(value.inputOperators.map(Str.apply)),
      "output_operators" -> arr(value.outputOperators.map(Str.apply)),
      "proof" -> certificate(value.proof),
      "remaining_equivalence" -> equivalence(value.remainingEquivalence),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def frame(value: FunctionalFrameIr): IrJson =
    obj(
      "parameter_id" -> Str(value.parameterId),
      "weights_identity" -> Str(value.weightsIdentity),
      "cometric_identity" -> value.cometricIdentity.fold[IrJson](Null)(Str.apply),
      "score_identities" -> arr(value.scoreIdentities.map(Str.apply)),
      "axis_identity" -> value.axisIdentity.fold[IrJson](Null)(Str.apply)
    )

  private def fit(value: ProgramFitIr): IrJson =
    obj(
      "program_id" -> Str(value.programId),
      "frames" -> arr(value.frames.map(frame)),
      "objective_value" -> Num(value.objectiveValue),
      "retained_rank" -> Num(value.retainedRank),
      "spectral_clusters" -> arr(value.spectralClusters.map(cluster => arr(cluster.map(value => Num(value))))),
      "residual_certificates" -> arr(value.residualCertificates.map(certificate)),
      "solver_guarantee" -> Str(guarantee(value.solverGuarantee)),
      "remaining_equivalence" -> equivalence(value.remainingEquivalence),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def operatorPolicy(value: ProgramOperatorPolicyIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "kind" -> policyKind(value.kind),
      "input_operators" -> arr(value.inputOperators.map(Str.apply)),
      "output_operators" -> arr(value.outputOperators.map(Str.apply)),
      "selection" -> policySelection(value.selection),
      "scale_matching" -> scaleMatching(value.scaleMatching),
      "scope" -> Str(policyScope(value.scope)),
      "preservation" -> arr(value.preservation.map(preservation)),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def policyKind(value: ProgramOperatorPolicyKindIr): IrJson =
    value match
      case ProgramOperatorPolicyKindIr.LinearShrinkage => obj("kind" -> Str("linear_shrinkage"))
      case ProgramOperatorPolicyKindIr.LdaWithinScatterShrinkage =>
        obj("kind" -> Str("lda_within_scatter_shrinkage"))
      case ProgramOperatorPolicyKindIr.PsdRepair => obj("kind" -> Str("psd_repair"))
      case ProgramOperatorPolicyKindIr.SupportRestriction => obj("kind" -> Str("support_restriction"))
      case ProgramOperatorPolicyKindIr.GaugeFixing => obj("kind" -> Str("gauge_fixing"))
      case ProgramOperatorPolicyKindIr.JointBlockShrinkage => obj("kind" -> Str("joint_block_shrinkage"))
      case ProgramOperatorPolicyKindIr.BlockwiseShrinkage => obj("kind" -> Str("blockwise_shrinkage"))
      case ProgramOperatorPolicyKindIr.Custom(name) => obj("kind" -> Str("custom"), "name" -> Str(name))

  private def policySelection(value: ProgramPolicySelectionIr): IrJson =
    value match
      case ProgramPolicySelectionIr.Fixed(strength) =>
        obj("kind" -> Str("fixed"), "strength" -> Num(strength))
      case ProgramPolicySelectionIr.FoldSelected(selector, candidates) =>
        obj(
          "kind" -> Str("fold_selected"),
          "selector_id" -> Str(selector),
          "candidates" -> arr(candidates.map(Num.apply))
        )

  private def scaleMatching(value: ProgramScaleMatchingIr): IrJson =
    value match
      case ProgramScaleMatchingIr.None => obj("kind" -> Str("none"))
      case ProgramScaleMatchingIr.MatchTrace => obj("kind" -> Str("match_trace"))
      case ProgramScaleMatchingIr.MatchDiagonalMean => obj("kind" -> Str("match_diagonal_mean"))
      case ProgramScaleMatchingIr.Fixed(current) => obj("kind" -> Str("fixed"), "value" -> Num(current))

  private def policyScope(value: ProgramPolicyScopeIr): String =
    value match
      case ProgramPolicyScopeIr.SingleOperator => "single_operator"
      case ProgramPolicyScopeIr.JointSystem => "joint_system"
      case ProgramPolicyScopeIr.BlockwiseUnsafe => "blockwise_unsafe"

  private def preservation(value: ProgramPreservationClaimIr): IrJson =
    value match
      case ProgramPreservationClaimIr.PsdPreserved => obj("kind" -> Str("psd_preserved"))
      case ProgramPreservationClaimIr.SpdPreserved => obj("kind" -> Str("spd_preserved"))
      case ProgramPreservationClaimIr.BlockAdjointsPreserved => obj("kind" -> Str("block_adjoints_preserved"))
      case ProgramPreservationClaimIr.SharedGaugePreserved => obj("kind" -> Str("shared_gauge_preserved"))
      case ProgramPreservationClaimIr.SupportRestricted => obj("kind" -> Str("support_restricted"))
      case ProgramPreservationClaimIr.GaugeFixed => obj("kind" -> Str("gauge_fixed"))
      case ProgramPreservationClaimIr.EvidenceDowngraded(reason) =>
        obj("kind" -> Str("evidence_downgraded"), "reason" -> Str(reason))

  private def compositeLowering(value: ProgramCompositeLoweringIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "program_id" -> Str(value.programId),
      "term" -> loweredTerm(value.term),
      "target_operator" -> Str(value.targetOperator),
      "method" -> Str(splitMethod(value.method)),
      "available_capabilities" -> arr(value.availableCapabilities.map(method => Str(splitMethod(method)))),
      "auxiliary" -> auxiliaryConstraint(value.auxiliary),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def loweredTerm(value: ProgramLoweredTermIr): IrJson =
    value match
      case ProgramLoweredTermIr.Penalty(index) => obj("kind" -> Str("penalty"), "index" -> Num(index))
      case ProgramLoweredTermIr.Constraint(index) => obj("kind" -> Str("constraint"), "index" -> Num(index))

  private def auxiliaryConstraint(value: ProgramAuxiliaryConstraintIr): IrJson =
    obj(
      "variable_id" -> Str(value.variableId),
      "target" -> target(value.target),
      "equation" -> auxiliaryEquation(value.equation)
    )

  private def auxiliaryEquation(value: ProgramAuxiliaryEquationIr): IrJson =
    value match
      case ProgramAuxiliaryEquationIr.TargetCopy => obj("kind" -> Str("target_copy"))
      case ProgramAuxiliaryEquationIr.LatentGroupSum(groups) =>
        obj("kind" -> Str("latent_group_sum"), "groups_identity" -> Str(groups))

  private def splitMethod(value: ProgramSplitMethodIr): String =
    value match
      case ProgramSplitMethodIr.PrimalDual => "primal_dual"
      case ProgramSplitMethodIr.Admm => "admm"
      case ProgramSplitMethodIr.AugmentedLagrangian => "augmented_lagrangian"
      case ProgramSplitMethodIr.Conic => "conic"

  private def tolerance(value: ToleranceIr): IrJson =
    obj("absolute" -> Num(value.absolute), "relative" -> Num(value.relative))

  private def certificate(value: CertificateIr): IrJson =
    obj(
      "property" -> Str(value.property),
      "value_identity" -> Str(value.valueIdentity),
      "tolerance" -> tolerance(value.tolerance),
      "norm" -> Str(value.norm),
      "method" -> Str(value.method),
      "precision" -> Str(value.precision),
      "backend" -> Str(value.backend),
      "regularization" -> value.regularization.fold[IrJson](Null)(Str.apply),
      "residual" -> value.residual.fold[IrJson](Null)(Num.apply)
    )

  private def provenance(value: ProvenanceEventIr): IrJson =
    value match
      case ProvenanceEventIr.Source(label) => obj("kind" -> Str("source"), "label" -> Str(label))
      case ProvenanceEventIr.Adapted(adapter) => obj("kind" -> Str("adapted"), "adapter" -> Str(adapter))
      case ProvenanceEventIr.Derived(operation, inputs) =>
        obj("kind" -> Str("derived"), "operation" -> Str(operation), "inputs" -> arr(inputs.map(Str.apply)))
      case ProvenanceEventIr.Certified(property, method) =>
        obj("kind" -> Str("certified"), "property" -> Str(property), "method" -> Str(method))
      case ProvenanceEventIr.UnsafeAssumption(property, reason) =>
        obj("kind" -> Str("unsafe_assumption"), "property" -> Str(property), "reason" -> Str(reason))

  private def spaceRole(value: SpaceRoleIr): String =
    value match
      case SpaceRoleIr.Samples => "samples"
      case SpaceRoleIr.Observed => "observed"
      case SpaceRoleIr.Latent => "latent"
      case SpaceRoleIr.Kernel => "kernel"
      case SpaceRoleIr.Block => "block"

  private def variance(value: VarianceIr): String =
    value match
      case VarianceIr.Primal => "primal"
      case VarianceIr.Dual => "dual"

  private def simpleRole(value: ProgramOperatorRoleIr): String =
    value match
      case ProgramOperatorRoleIr.Table => "table"
      case ProgramOperatorRoleIr.Metric => "metric"
      case ProgramOperatorRoleIr.Cometric => "cometric"
      case ProgramOperatorRoleIr.Covariance => "covariance"
      case ProgramOperatorRoleIr.Scatter => "scatter"
      case ProgramOperatorRoleIr.Penalty => "penalty"
      case ProgramOperatorRoleIr.Kernel => "kernel"
      case ProgramOperatorRoleIr.RowLink => "row_link"
      case ProgramOperatorRoleIr.Frame => "frame"
      case ProgramOperatorRoleIr.Cross => "cross"
      case ProgramOperatorRoleIr.Component => "component"
      case ProgramOperatorRoleIr.Score => "score"
      case ProgramOperatorRoleIr.Axis => "axis"
      case ProgramOperatorRoleIr.Coefficient => "coefficient"
      case ProgramOperatorRoleIr.ConstraintMap => "constraint_map"
      case _ => throw new IllegalArgumentException("compound operator role requires structural encoding")

  private def evidenceStatus(value: EvidenceStatusIr): String =
    value match
      case EvidenceStatusIr.Unchecked => "unchecked"
      case EvidenceStatusIr.Certified => "certified"
      case EvidenceStatusIr.Assumed => "assumed"

  private def representation(value: ProgramRepresentationIr): String =
    value match
      case ProgramRepresentationIr.Dense => "dense"
      case ProgramRepresentationIr.Sparse => "sparse"
      case ProgramRepresentationIr.Diagonal => "diagonal"
      case ProgramRepresentationIr.Block => "block"
      case ProgramRepresentationIr.LowRank => "low_rank"
      case ProgramRepresentationIr.Kronecker => "kronecker"
      case ProgramRepresentationIr.LazyAffine => "lazy_affine"
      case ProgramRepresentationIr.MatrixFree => "matrix_free"

  private def targetCapability(value: ProgramTargetCapabilityIr): String =
    value match
      case ProgramTargetCapabilityIr.Linear => "linear"
      case ProgramTargetCapabilityIr.Affine => "affine"
      case ProgramTargetCapabilityIr.Smooth => "smooth"
      case ProgramTargetCapabilityIr.General => "general"

  private def simpleFunctional(value: ProgramFunctionalIr): String =
    value match
      case ProgramFunctionalIr.L1 => "l1"
      case ProgramFunctionalIr.GroupL21 => "group_l21"
      case ProgramFunctionalIr.TotalVariation => "total_variation"
      case ProgramFunctionalIr.NuclearNorm => "nuclear_norm"
      case ProgramFunctionalIr.NegativeLogDet => "negative_log_det"
      case _ => throw new IllegalArgumentException("parameterized functional requires structural encoding")

  private def simpleFeasibleSet(value: ProgramFeasibleSetIr): String =
    value match
      case ProgramFeasibleSetIr.ZeroSubspace => "zero_subspace"
      case ProgramFeasibleSetIr.NonnegativeOrthant => "nonnegative_orthant"
      case ProgramFeasibleSetIr.Simplex => "simplex"
      case ProgramFeasibleSetIr.PsdCone => "psd_cone"
      case ProgramFeasibleSetIr.Stiefel => "stiefel"
      case _ => throw new IllegalArgumentException("parameterized feasible set requires structural encoding")

  private def symmetry(value: ProgramFrameSymmetryIr): String =
    value match
      case ProgramFrameSymmetryIr.Orthogonal => "orthogonal"
      case ProgramFrameSymmetryIr.SignedPermutation => "signed_permutation"
      case ProgramFrameSymmetryIr.Permutation => "permutation"
      case ProgramFrameSymmetryIr.Identity => "identity"

  private def simpleRepresentative(value: ProgramRepresentativeIr): String =
    value match
      case ProgramRepresentativeIr.DeterministicSign => "deterministic_sign"
      case ProgramRepresentativeIr.OrderedSpectrumThenSign => "ordered_spectrum_then_sign"
      case ProgramRepresentativeIr.PredictionMap => "prediction_map"
      case ProgramRepresentativeIr.ObjectiveValueOnly => "objective_value_only"
      case _ => throw new IllegalArgumentException("parameterized representative requires structural encoding")

  private def guarantee(value: ProgramSolverGuaranteeIr): String =
    value match
      case ProgramSolverGuaranteeIr.GlobalSpectralOptimum => "global_spectral_optimum"
      case ProgramSolverGuaranteeIr.GlobalConvexOptimum => "global_convex_optimum"
      case ProgramSolverGuaranteeIr.StationaryPoint => "stationary_point"
      case ProgramSolverGuaranteeIr.FeasiblePoint => "feasible_point"
      case ProgramSolverGuaranteeIr.CoordinatewiseStationary => "coordinatewise_stationary"
      case ProgramSolverGuaranteeIr.LocallyOptimal => "locally_optimal"
      case ProgramSolverGuaranteeIr.HeuristicFeasible => "heuristic_feasible"
      case ProgramSolverGuaranteeIr.Unresolved => "unresolved"

  private def rewriteRule(value: ProgramRewriteRuleIr): String =
    value match
      case ProgramRewriteRuleIr.ExactLinearReduction => "exact_linear_reduction"
      case ProgramRewriteRuleIr.SupportRestriction => "support_restriction"
      case ProgramRewriteRuleIr.QuadraticPullback => "quadratic_pullback"
      case ProgramRewriteRuleIr.GeneralizedToStandardEigen => "generalized_to_standard_eigen"
      case ProgramRewriteRuleIr.Whitening => "whitening"

  private def obj(fields: (String, IrJson)*): IrJson = Obj(fields.toVector)
  private def arr(values: Vector[IrJson]): IrJson = Arr(values)

private object ProgramIrDecoder:
  import IrJson.*

  def document(value: IrJson): Either[IrError, OperatorProgramDocumentIr] =
    for
      current <- fields(
        value,
        "$",
        Set("schema", "spaces", "operators", "programs", "rewrites", "fits", "operator_policies", "composite_lowerings")
      )
      schema <- required(current, "schema", "$", string(_, "$.schema"))
      spaces <- required(current, "spaces", "$", vector(_, "$.spaces", space))
      operators <- required(current, "operators", "$", vector(_, "$.operators", operator))
      programs <- required(current, "programs", "$", vector(_, "$.programs", program))
      rewrites <- required(current, "rewrites", "$", vector(_, "$.rewrites", rewrite))
      fits <- required(current, "fits", "$", vector(_, "$.fits", fit))
      policies <- required(current, "operator_policies", "$", vector(_, "$.operator_policies", operatorPolicy))
      lowerings <- required(current, "composite_lowerings", "$", vector(_, "$.composite_lowerings", compositeLowering))
    yield OperatorProgramDocumentIr(schema, spaces, operators, programs, rewrites, fits, policies, lowerings)

  private def space(value: IrJson, path: String): Either[IrError, SpaceIr] =
    for
      current <- fields(value, path, Set("id", "role", "dimension"))
      id <- required(current, "id", path, string(_, s"$path.id"))
      role <- required(current, "role", path, spaceRole(_, s"$path.role"))
      dimension <- required(current, "dimension", path, integer(_, s"$path.dimension"))
    yield SpaceIr(id, role, dimension)

  private def coordinate(value: IrJson, path: String): Either[IrError, CoordinateIr] =
    for
      current <- fields(value, path, Set("space_id", "variance"))
      spaceId <- required(current, "space_id", path, string(_, s"$path.space_id"))
      currentVariance <- required(current, "variance", path, variance(_, s"$path.variance"))
    yield CoordinateIr(spaceId, currentVariance)

  private def operator(value: IrJson, path: String): Either[IrError, ProgramOpIr] =
    for
      current <- fields(
        value,
        path,
        Set("id", "domain", "codomain", "role", "evidence", "representation", "gauge", "derivation", "value_identity", "provenance")
      )
      id <- required(current, "id", path, string(_, s"$path.id"))
      domain <- required(current, "domain", path, coordinate(_, s"$path.domain"))
      codomain <- required(current, "codomain", path, coordinate(_, s"$path.codomain"))
      role <- required(current, "role", path, operatorRole(_, s"$path.role"))
      currentEvidence <- required(current, "evidence", path, evidence(_, s"$path.evidence"))
      currentRepresentation <- required(current, "representation", path, representation(_, s"$path.representation"))
      currentGauge <- required(current, "gauge", path, gauge(_, s"$path.gauge"))
      currentDerivation <- required(current, "derivation", path, derivation(_, s"$path.derivation"))
      identity <- required(current, "value_identity", path, string(_, s"$path.value_identity"))
      provenanceValue <- required(current, "provenance", path, vector(_, s"$path.provenance", provenance))
    yield ProgramOpIr(
      id,
      domain,
      codomain,
      role,
      currentEvidence,
      currentRepresentation,
      currentGauge,
      currentDerivation,
      identity,
      provenanceValue
    )

  private def operatorRole(value: IrJson, path: String): Either[IrError, ProgramOperatorRoleIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "composed" =>
          exact(current, path, Set("kind", "first", "second")).flatMap: checked =>
            for
              first <- required(checked, "first", path, operatorRole(_, s"$path.first"))
              second <- required(checked, "second", path, operatorRole(_, s"$path.second"))
            yield ProgramOperatorRoleIr.Composed(first, second)
        case "dual" | "metric_adjoint" =>
          exact(current, path, Set("kind", "of")).flatMap: checked =>
            required(checked, "of", path, operatorRole(_, s"$path.of")).map: of =>
              if kind == "dual" then ProgramOperatorRoleIr.Dual(of) else ProgramOperatorRoleIr.MetricAdjoint(of)
        case other =>
          exact(current, path, Set("kind")).flatMap(_ => simpleOperatorRole(other, path))

  private def evidence(value: IrJson, path: String): Either[IrError, ProgramOperatorEvidenceIr] =
    for
      current <- fields(value, path, Set("status", "certificates"))
      status <- required(current, "status", path, evidenceStatus(_, s"$path.status"))
      certificates <- required(current, "certificates", path, vector(_, s"$path.certificates", certificate))
    yield ProgramOperatorEvidenceIr(status, certificates)

  private def gauge(value: IrJson, path: String): Either[IrError, ProgramGaugeIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "ungauged" => exact(current, path, Set("kind")).map(_ => ProgramGaugeIr.Ungauged)
        case "shape" =>
          exact(current, path, Set("kind", "id")).flatMap(checked => required(checked, "id", path, string(_, s"$path.id"))).map(ProgramGaugeIr.Shape.apply)
        case "orthonormal" =>
          exact(current, path, Set("kind", "metric_identity"))
            .flatMap(checked => required(checked, "metric_identity", path, string(_, s"$path.metric_identity")))
            .map(ProgramGaugeIr.Orthonormal.apply)
        case _ => malformed(path, s"unknown gauge '$kind'")

  private def derivation(value: IrJson, path: String): Either[IrError, ProgramOperatorDerivationIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "source" => exact(current, path, Set("kind")).map(_ => ProgramOperatorDerivationIr.Source)
        case "second_order" =>
          for
            checked <- exact(current, path, Set("kind", "source_table", "relationship", "target_table"))
            source <- required(checked, "source_table", path, string(_, s"$path.source_table"))
            relationship <- required(checked, "relationship", path, string(_, s"$path.relationship"))
            target <- required(checked, "target_table", path, string(_, s"$path.target_table"))
          yield ProgramOperatorDerivationIr.SecondOrder(source, relationship, target)
        case "compress" =>
          for
            checked <- exact(current, path, Set("kind", "source_frame", "second_order", "target_frame"))
            source <- required(checked, "source_frame", path, string(_, s"$path.source_frame"))
            secondOrder <- required(checked, "second_order", path, string(_, s"$path.second_order"))
            target <- required(checked, "target_frame", path, string(_, s"$path.target_frame"))
          yield ProgramOperatorDerivationIr.Compress(source, secondOrder, target)
        case "scores" =>
          for
            checked <- exact(current, path, Set("kind", "frame", "table"))
            frame <- required(checked, "frame", path, string(_, s"$path.frame"))
            table <- required(checked, "table", path, string(_, s"$path.table"))
          yield ProgramOperatorDerivationIr.Scores(frame, table)
        case "axes" =>
          for
            checked <- exact(current, path, Set("kind", "frame", "cometric"))
            frame <- required(checked, "frame", path, string(_, s"$path.frame"))
            cometric <- required(checked, "cometric", path, string(_, s"$path.cometric"))
          yield ProgramOperatorDerivationIr.Axes(frame, cometric)
        case "lowered" =>
          for
            checked <- exact(current, path, Set("kind", "rule", "inputs"))
            rule <- required(checked, "rule", path, string(_, s"$path.rule"))
            inputs <- required(checked, "inputs", path, vector(_, s"$path.inputs", string))
          yield ProgramOperatorDerivationIr.Lowered(rule, inputs)
        case _ => malformed(path, s"unknown derivation '$kind'")

  private def program(value: IrJson, path: String): Either[IrError, OperatorProgramV2Ir] =
    for
      current <- fields(value, path, Set("id", "parameters", "objective", "normalizations", "penalties", "constraints", "result", "provenance"))
      id <- required(current, "id", path, string(_, s"$path.id"))
      parameters <- required(current, "parameters", path, vector(_, s"$path.parameters", parameter))
      objectiveValue <- required(current, "objective", path, objective(_, s"$path.objective"))
      normalizations <- required(current, "normalizations", path, vector(_, s"$path.normalizations", normalization))
      penalties <- required(current, "penalties", path, vector(_, s"$path.penalties", penalty))
      constraints <- required(current, "constraints", path, vector(_, s"$path.constraints", constraint))
      resultValue <- required(current, "result", path, result(_, s"$path.result"))
      provenanceValue <- required(current, "provenance", path, vector(_, s"$path.provenance", provenance))
    yield OperatorProgramV2Ir(id, parameters, objectiveValue, normalizations, penalties, constraints, resultValue, provenanceValue)

  private def parameter(value: IrJson, path: String): Either[IrError, ProgramFrameParameterIr] =
    for
      current <- fields(value, path, Set("id", "feature_space_id", "component_space_id", "parameterization"))
      id <- required(current, "id", path, string(_, s"$path.id"))
      feature <- required(current, "feature_space_id", path, string(_, s"$path.feature_space_id"))
      component <- required(current, "component_space_id", path, string(_, s"$path.component_space_id"))
      currentParameterization <- required(current, "parameterization", path, parameterization(_, s"$path.parameterization"))
    yield ProgramFrameParameterIr(id, feature, component, currentParameterization)

  private def parameterization(value: IrJson, path: String): Either[IrError, ProgramParameterizationIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "identity" => exact(current, path, Set("kind")).map(_ => ProgramParameterizationIr.Identity)
        case "known_support" =>
          for
            checked <- exact(current, path, Set("kind", "embedding_identity", "injective"))
            embedding <- required(checked, "embedding_identity", path, string(_, s"$path.embedding_identity"))
            injective <- required(checked, "injective", path, boolean(_, s"$path.injective"))
          yield ProgramParameterizationIr.KnownSupport(embedding, injective)
        case "shared_basis" =>
          for
            checked <- exact(current, path, Set("kind", "basis_identity", "injective"))
            basis <- required(checked, "basis_identity", path, string(_, s"$path.basis_identity"))
            injective <- required(checked, "injective", path, boolean(_, s"$path.injective"))
          yield ProgramParameterizationIr.SharedBasis(basis, injective)
        case "fixed_rank" =>
          for
            checked <- exact(current, path, Set("kind", "rank", "gauge"))
            rank <- required(checked, "rank", path, integer(_, s"$path.rank"))
            gauge <- required(checked, "gauge", path, string(_, s"$path.gauge"))
          yield ProgramParameterizationIr.FixedRank(rank, gauge)
        case "block_diagonal" =>
          exact(current, path, Set("kind", "blocks"))
            .flatMap(checked => required(checked, "blocks", path, vector(_, s"$path.blocks", string)))
            .map(ProgramParameterizationIr.BlockDiagonal.apply)
        case "null_space" =>
          for
            checked <- exact(current, path, Set("kind", "basis_identity", "tolerance"))
            basis <- required(checked, "basis_identity", path, string(_, s"$path.basis_identity"))
            currentTolerance <- required(checked, "tolerance", path, tolerance(_, s"$path.tolerance"))
          yield ProgramParameterizationIr.NullSpace(basis, currentTolerance)
        case _ => malformed(path, s"unknown parameterization '$kind'")

  private def objective(value: IrJson, path: String): Either[IrError, ProgramObjectiveIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "maximize_trace" | "minimize_disagreement" =>
          for
            checked <- exact(current, path, Set("kind", "parameter_id", "operator_identity"))
            parameter <- required(checked, "parameter_id", path, string(_, s"$path.parameter_id"))
            operator <- required(checked, "operator_identity", path, string(_, s"$path.operator_identity"))
          yield
            if kind == "maximize_trace" then ProgramObjectiveIr.MaximizeTrace(parameter, operator)
            else ProgramObjectiveIr.MinimizeDisagreement(parameter, operator)
        case "maximize_cross_trace" =>
          for
            checked <- exact(current, path, Set("kind", "source_parameter_id", "target_parameter_id", "operator_identity"))
            source <- required(checked, "source_parameter_id", path, string(_, s"$path.source_parameter_id"))
            target <- required(checked, "target_parameter_id", path, string(_, s"$path.target_parameter_id"))
            operator <- required(checked, "operator_identity", path, string(_, s"$path.operator_identity"))
          yield ProgramObjectiveIr.MaximizeCrossTrace(source, target, operator)
        case "generalized_rayleigh" | "trace_ratio" | "ratio_trace" =>
          for
            checked <- exact(current, path, Set("kind", "parameter_id", "numerator_identity", "denominator_identity"))
            parameter <- required(checked, "parameter_id", path, string(_, s"$path.parameter_id"))
            numerator <- required(checked, "numerator_identity", path, string(_, s"$path.numerator_identity"))
            denominator <- required(checked, "denominator_identity", path, string(_, s"$path.denominator_identity"))
          yield
            kind match
              case "generalized_rayleigh" => ProgramObjectiveIr.GeneralizedRayleigh(parameter, numerator, denominator)
              case "trace_ratio" => ProgramObjectiveIr.TraceRatio(parameter, numerator, denominator)
              case _ => ProgramObjectiveIr.RatioTrace(parameter, numerator, denominator)
        case "sequential_cross_regression" =>
          for
            checked <- exact(current, path, Set("kind", "source_parameter_id", "target_parameter_id", "cross_identity", "predictor_identity"))
            source <- required(checked, "source_parameter_id", path, string(_, s"$path.source_parameter_id"))
            target <- required(checked, "target_parameter_id", path, string(_, s"$path.target_parameter_id"))
            cross <- required(checked, "cross_identity", path, string(_, s"$path.cross_identity"))
            predictor <- required(checked, "predictor_identity", path, string(_, s"$path.predictor_identity"))
          yield ProgramObjectiveIr.SequentialCrossRegression(source, target, cross, predictor)
        case _ => malformed(path, s"unknown objective '$kind'")

  private def normalization(value: IrJson, path: String): Either[IrError, ProgramNormalizationV2Ir] =
    for
      current <- fields(value, path, Set("parameter_id", "operator_identity"))
      parameter <- required(current, "parameter_id", path, string(_, s"$path.parameter_id"))
      operator <- required(current, "operator_identity", path, string(_, s"$path.operator_identity"))
    yield ProgramNormalizationV2Ir(parameter, operator)

  private def target(value: IrJson, path: String): Either[IrError, ProgramTargetIr] =
    for
      current <- fields(
        value,
        path,
        Set(
          "parameter_id",
          "capability",
          "operation",
          "operator_identity",
          "additional_parameter_ids",
          "additional_operator_identities",
          "equivariance"
        )
      )
      parameter <- required(current, "parameter_id", path, string(_, s"$path.parameter_id"))
      capability <- required(current, "capability", path, targetCapability(_, s"$path.capability"))
      operation <- required(current, "operation", path, string(_, s"$path.operation"))
      operator <- required(current, "operator_identity", path, optionalString(_, s"$path.operator_identity"))
      additionalParameters <- required(
        current,
        "additional_parameter_ids",
        path,
        vector(_, s"$path.additional_parameter_ids", string)
      )
      additionalOperators <- required(
        current,
        "additional_operator_identities",
        path,
        vector(_, s"$path.additional_operator_identities", string)
      )
      equivariance <- required(current, "equivariance", path, symmetry(_, s"$path.equivariance"))
    yield ProgramTargetIr(
      parameter,
      capability,
      operation,
      operator,
      additionalParameters,
      additionalOperators,
      equivariance
    )

  private def penalty(value: IrJson, path: String): Either[IrError, ProgramPenaltyV2Ir] =
    for
      current <- fields(value, path, Set("target", "functional", "weight", "symmetry"))
      targetValue <- required(current, "target", path, target(_, s"$path.target"))
      functionalValue <- required(current, "functional", path, functional(_, s"$path.functional"))
      weight <- required(current, "weight", path, number(_, s"$path.weight"))
      symmetryValue <- required(current, "symmetry", path, symmetry(_, s"$path.symmetry"))
    yield ProgramPenaltyV2Ir(targetValue, functionalValue, weight, symmetryValue)

  private def functional(value: IrJson, path: String): Either[IrError, ProgramFunctionalIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "squared_norm" =>
          exact(current, path, Set("kind", "geometry_identity"))
            .flatMap(checked => required(checked, "geometry_identity", path, string(_, s"$path.geometry_identity")))
            .map(ProgramFunctionalIr.SquaredNorm.apply)
        case "elastic_net" =>
          exact(current, path, Set("kind", "l1_fraction"))
            .flatMap(checked => required(checked, "l1_fraction", path, number(_, s"$path.l1_fraction")))
            .map(ProgramFunctionalIr.ElasticNet.apply)
        case "group_l2" =>
          exact(current, path, Set("kind", "groups_identity"))
            .flatMap(checked => required(checked, "groups_identity", path, string(_, s"$path.groups_identity")))
            .map(ProgramFunctionalIr.GroupL2.apply)
        case "sparse_group" =>
          for
            checked <- exact(current, path, Set("kind", "l1_fraction", "groups_identity"))
            fraction <- required(checked, "l1_fraction", path, number(_, s"$path.l1_fraction"))
            groups <- required(checked, "groups_identity", path, string(_, s"$path.groups_identity"))
          yield ProgramFunctionalIr.SparseGroup(fraction, groups)
        case "huber" =>
          exact(current, path, Set("kind", "delta"))
            .flatMap(checked => required(checked, "delta", path, number(_, s"$path.delta")))
            .map(ProgramFunctionalIr.Huber.apply)
        case other => exact(current, path, Set("kind")).flatMap(_ => simpleFunctional(other, path))

  private def constraint(value: IrJson, path: String): Either[IrError, ProgramConstraintV2Ir] =
    for
      current <- fields(value, path, Set("target", "feasible_set", "symmetry"))
      targetValue <- required(current, "target", path, target(_, s"$path.target"))
      feasible <- required(current, "feasible_set", path, feasibleSet(_, s"$path.feasible_set"))
      symmetryValue <- required(current, "symmetry", path, symmetry(_, s"$path.symmetry"))
    yield ProgramConstraintV2Ir(targetValue, feasible, symmetryValue)

  private def feasibleSet(value: IrJson, path: String): Either[IrError, ProgramFeasibleSetIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "box" =>
          for
            checked <- exact(current, path, Set("kind", "lower", "upper"))
            lower <- required(checked, "lower", path, number(_, s"$path.lower"))
            upper <- required(checked, "upper", path, number(_, s"$path.upper"))
          yield ProgramFeasibleSetIr.Box(lower, upper)
        case "norm_ball" =>
          exact(current, path, Set("kind", "radius"))
            .flatMap(checked => required(checked, "radius", path, number(_, s"$path.radius")))
            .map(ProgramFeasibleSetIr.NormBall.apply)
        case "monotone" =>
          exact(current, path, Set("kind", "order_identity"))
            .flatMap(checked => required(checked, "order_identity", path, string(_, s"$path.order_identity")))
            .map(ProgramFeasibleSetIr.Monotone.apply)
        case "fixed_support" =>
          exact(current, path, Set("kind", "indices"))
            .flatMap(checked => required(checked, "indices", path, vector(_, s"$path.indices", integer)))
            .map(ProgramFeasibleSetIr.FixedSupport.apply)
        case "rank_bounded" =>
          exact(current, path, Set("kind", "rank"))
            .flatMap(checked => required(checked, "rank", path, integer(_, s"$path.rank")))
            .map(ProgramFeasibleSetIr.RankBounded.apply)
        case other => exact(current, path, Set("kind")).flatMap(_ => simpleFeasibleSet(other, path))

  private def result(value: IrJson, path: String): Either[IrError, ProgramResultContractIr] =
    for
      current <- fields(
        value,
        path,
        Set("equivalence", "representative", "guarantee", "redundant_coordinates", "parameter_gauges")
      )
      equivalenceValue <- required(current, "equivalence", path, equivalence(_, s"$path.equivalence"))
      representativeValue <- required(current, "representative", path, representative(_, s"$path.representative"))
      guaranteeValue <- required(current, "guarantee", path, guarantee(_, s"$path.guarantee"))
      redundant <- required(current, "redundant_coordinates", path, boolean(_, s"$path.redundant_coordinates"))
      gauges <- required(current, "parameter_gauges", path, vector(_, s"$path.parameter_gauges", string))
    yield ProgramResultContractIr(equivalenceValue, representativeValue, guaranteeValue, redundant, gauges)

  private def equivalence(value: IrJson, path: String): Either[IrError, ProgramEquivalenceIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "value" | "objective" =>
          exact(current, path, Set("kind", "tolerance"))
            .flatMap(checked => required(checked, "tolerance", path, tolerance(_, s"$path.tolerance")))
            .map(currentTolerance => if kind == "value" then ProgramEquivalenceIr.Value(currentTolerance) else ProgramEquivalenceIr.Objective(currentTolerance))
        case "operator" =>
          for
            checked <- exact(current, path, Set("kind", "domain", "codomain", "tolerance"))
            domain <- required(checked, "domain", path, coordinate(_, s"$path.domain"))
            codomain <- required(checked, "codomain", path, coordinate(_, s"$path.codomain"))
            currentTolerance <- required(checked, "tolerance", path, tolerance(_, s"$path.tolerance"))
          yield ProgramEquivalenceIr.Operator(domain, codomain, currentTolerance)
        case "subspace" =>
          for
            checked <- exact(current, path, Set("kind", "projector_tolerance", "principal_angle_tolerance"))
            projector <- required(checked, "projector_tolerance", path, tolerance(_, s"$path.projector_tolerance"))
            angle <- required(checked, "principal_angle_tolerance", path, tolerance(_, s"$path.principal_angle_tolerance"))
          yield ProgramEquivalenceIr.Subspace(projector, angle)
        case "frame" =>
          for
            checked <- exact(current, path, Set("kind", "symmetry", "tolerance"))
            group <- required(checked, "symmetry", path, symmetry(_, s"$path.symmetry"))
            currentTolerance <- required(checked, "tolerance", path, tolerance(_, s"$path.tolerance"))
          yield ProgramEquivalenceIr.Frame(group, currentTolerance)
        case "prediction" =>
          for
            checked <- exact(current, path, Set("kind", "metric", "tolerance"))
            metric <- required(checked, "metric", path, predictionMetric(_, s"$path.metric"))
            currentTolerance <- required(checked, "tolerance", path, tolerance(_, s"$path.tolerance"))
          yield ProgramEquivalenceIr.Prediction(metric, currentTolerance)
        case _ => malformed(path, s"unknown equivalence '$kind'")

  private def predictionMetric(value: IrJson, path: String): Either[IrError, ProgramPredictionMetricIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "squared_error" => exact(current, path, Set("kind")).map(_ => ProgramPredictionMetricIr.SquaredError)
        case "correlation" => exact(current, path, Set("kind")).map(_ => ProgramPredictionMetricIr.Correlation)
        case "mahalanobis" =>
          exact(current, path, Set("kind", "metric_identity"))
            .flatMap(checked => required(checked, "metric_identity", path, string(_, s"$path.metric_identity")))
            .map(ProgramPredictionMetricIr.Mahalanobis.apply)
        case _ => malformed(path, s"unknown prediction metric '$kind'")

  private def representative(value: IrJson, path: String): Either[IrError, ProgramRepresentativeIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "procrustes_to_reference" =>
          exact(current, path, Set("kind", "reference_identity"))
            .flatMap(checked => required(checked, "reference_identity", path, string(_, s"$path.reference_identity")))
            .map(ProgramRepresentativeIr.ProcrustesToReference.apply)
        case other => exact(current, path, Set("kind")).flatMap(_ => simpleRepresentative(other, path))

  private def rewrite(value: IrJson, path: String): Either[IrError, ProgramRewriteIr] =
    for
      current <- fields(value, path, Set("id", "original_program_id", "lowered_program_id", "rule", "input_operators", "output_operators", "proof", "remaining_equivalence", "provenance"))
      id <- required(current, "id", path, string(_, s"$path.id"))
      original <- required(current, "original_program_id", path, string(_, s"$path.original_program_id"))
      lowered <- required(current, "lowered_program_id", path, string(_, s"$path.lowered_program_id"))
      rule <- required(current, "rule", path, rewriteRule(_, s"$path.rule"))
      inputs <- required(current, "input_operators", path, vector(_, s"$path.input_operators", string))
      outputs <- required(current, "output_operators", path, vector(_, s"$path.output_operators", string))
      proof <- required(current, "proof", path, certificate(_, s"$path.proof"))
      remaining <- required(current, "remaining_equivalence", path, equivalence(_, s"$path.remaining_equivalence"))
      provenanceValue <- required(current, "provenance", path, vector(_, s"$path.provenance", provenance))
    yield ProgramRewriteIr(id, original, lowered, rule, inputs, outputs, proof, remaining, provenanceValue)

  private def frame(value: IrJson, path: String): Either[IrError, FunctionalFrameIr] =
    for
      current <- fields(value, path, Set("parameter_id", "weights_identity", "cometric_identity", "score_identities", "axis_identity"))
      parameter <- required(current, "parameter_id", path, string(_, s"$path.parameter_id"))
      weights <- required(current, "weights_identity", path, string(_, s"$path.weights_identity"))
      cometric <- required(current, "cometric_identity", path, optionalString(_, s"$path.cometric_identity"))
      scores <- required(current, "score_identities", path, vector(_, s"$path.score_identities", string))
      axis <- required(current, "axis_identity", path, optionalString(_, s"$path.axis_identity"))
    yield FunctionalFrameIr(parameter, weights, cometric, scores, axis)

  private def fit(value: IrJson, path: String): Either[IrError, ProgramFitIr] =
    for
      current <- fields(value, path, Set("program_id", "frames", "objective_value", "retained_rank", "spectral_clusters", "residual_certificates", "solver_guarantee", "remaining_equivalence", "provenance"))
      program <- required(current, "program_id", path, string(_, s"$path.program_id"))
      frames <- required(current, "frames", path, vector(_, s"$path.frames", frame))
      objective <- required(current, "objective_value", path, number(_, s"$path.objective_value"))
      rank <- required(current, "retained_rank", path, integer(_, s"$path.retained_rank"))
      clusters <- required(current, "spectral_clusters", path, vector(_, s"$path.spectral_clusters", (item, currentPath) => vector(item, currentPath, integer)))
      residuals <- required(current, "residual_certificates", path, vector(_, s"$path.residual_certificates", certificate))
      solver <- required(current, "solver_guarantee", path, guarantee(_, s"$path.solver_guarantee"))
      remaining <- required(current, "remaining_equivalence", path, equivalence(_, s"$path.remaining_equivalence"))
      provenanceValue <- required(current, "provenance", path, vector(_, s"$path.provenance", provenance))
    yield ProgramFitIr(program, frames, objective, rank, clusters, residuals, solver, remaining, provenanceValue)

  private def operatorPolicy(value: IrJson, path: String): Either[IrError, ProgramOperatorPolicyIr] =
    for
      current <- fields(
        value,
        path,
        Set(
          "id",
          "kind",
          "input_operators",
          "output_operators",
          "selection",
          "scale_matching",
          "scope",
          "preservation",
          "provenance"
        )
      )
      id <- required(current, "id", path, string(_, s"$path.id"))
      kind <- required(current, "kind", path, policyKind(_, s"$path.kind"))
      inputs <- required(current, "input_operators", path, vector(_, s"$path.input_operators", string))
      outputs <- required(current, "output_operators", path, vector(_, s"$path.output_operators", string))
      selection <- required(current, "selection", path, policySelection(_, s"$path.selection"))
      matching <- required(current, "scale_matching", path, scaleMatching(_, s"$path.scale_matching"))
      scope <- required(current, "scope", path, policyScope(_, s"$path.scope"))
      claims <- required(current, "preservation", path, vector(_, s"$path.preservation", preservation))
      provenanceValue <- required(current, "provenance", path, vector(_, s"$path.provenance", provenance))
    yield ProgramOperatorPolicyIr(id, kind, inputs, outputs, selection, matching, scope, claims, provenanceValue)

  private def policyKind(value: IrJson, path: String): Either[IrError, ProgramOperatorPolicyKindIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "linear_shrinkage" => exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.LinearShrinkage)
        case "lda_within_scatter_shrinkage" =>
          exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.LdaWithinScatterShrinkage)
        case "psd_repair" => exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.PsdRepair)
        case "support_restriction" =>
          exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.SupportRestriction)
        case "gauge_fixing" => exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.GaugeFixing)
        case "joint_block_shrinkage" =>
          exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.JointBlockShrinkage)
        case "blockwise_shrinkage" =>
          exact(current, path, Set("kind")).map(_ => ProgramOperatorPolicyKindIr.BlockwiseShrinkage)
        case "custom" =>
          exact(current, path, Set("kind", "name"))
            .flatMap(checked => required(checked, "name", path, string(_, s"$path.name")))
            .map(ProgramOperatorPolicyKindIr.Custom.apply)
        case _ => malformed(path, s"unknown operator policy kind '$kind'")

  private def policySelection(value: IrJson, path: String): Either[IrError, ProgramPolicySelectionIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "fixed" =>
          exact(current, path, Set("kind", "strength"))
            .flatMap(checked => required(checked, "strength", path, number(_, s"$path.strength")))
            .map(ProgramPolicySelectionIr.Fixed.apply)
        case "fold_selected" =>
          for
            checked <- exact(current, path, Set("kind", "selector_id", "candidates"))
            selector <- required(checked, "selector_id", path, string(_, s"$path.selector_id"))
            candidates <- required(checked, "candidates", path, vector(_, s"$path.candidates", number))
          yield ProgramPolicySelectionIr.FoldSelected(selector, candidates)
        case _ => malformed(path, s"unknown operator policy selection '$kind'")

  private def scaleMatching(value: IrJson, path: String): Either[IrError, ProgramScaleMatchingIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "none" => exact(current, path, Set("kind")).map(_ => ProgramScaleMatchingIr.None)
        case "match_trace" => exact(current, path, Set("kind")).map(_ => ProgramScaleMatchingIr.MatchTrace)
        case "match_diagonal_mean" =>
          exact(current, path, Set("kind")).map(_ => ProgramScaleMatchingIr.MatchDiagonalMean)
        case "fixed" =>
          exact(current, path, Set("kind", "value"))
            .flatMap(checked => required(checked, "value", path, number(_, s"$path.value")))
            .map(ProgramScaleMatchingIr.Fixed.apply)
        case _ => malformed(path, s"unknown scale matching '$kind'")

  private def policyScope(value: IrJson, path: String): Either[IrError, ProgramPolicyScopeIr] =
    string(value, path).flatMap:
      case "single_operator" => Right(ProgramPolicyScopeIr.SingleOperator)
      case "joint_system" => Right(ProgramPolicyScopeIr.JointSystem)
      case "blockwise_unsafe" => Right(ProgramPolicyScopeIr.BlockwiseUnsafe)
      case other => malformed(path, s"unknown operator policy scope '$other'")

  private def preservation(value: IrJson, path: String): Either[IrError, ProgramPreservationClaimIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "psd_preserved" => exact(current, path, Set("kind")).map(_ => ProgramPreservationClaimIr.PsdPreserved)
        case "spd_preserved" => exact(current, path, Set("kind")).map(_ => ProgramPreservationClaimIr.SpdPreserved)
        case "block_adjoints_preserved" =>
          exact(current, path, Set("kind")).map(_ => ProgramPreservationClaimIr.BlockAdjointsPreserved)
        case "shared_gauge_preserved" =>
          exact(current, path, Set("kind")).map(_ => ProgramPreservationClaimIr.SharedGaugePreserved)
        case "support_restricted" =>
          exact(current, path, Set("kind")).map(_ => ProgramPreservationClaimIr.SupportRestricted)
        case "gauge_fixed" => exact(current, path, Set("kind")).map(_ => ProgramPreservationClaimIr.GaugeFixed)
        case "evidence_downgraded" =>
          exact(current, path, Set("kind", "reason"))
            .flatMap(checked => required(checked, "reason", path, string(_, s"$path.reason")))
            .map(ProgramPreservationClaimIr.EvidenceDowngraded.apply)
        case _ => malformed(path, s"unknown preservation claim '$kind'")

  private def compositeLowering(value: IrJson, path: String): Either[IrError, ProgramCompositeLoweringIr] =
    for
      current <- fields(
        value,
        path,
        Set(
          "id",
          "program_id",
          "term",
          "target_operator",
          "method",
          "available_capabilities",
          "auxiliary",
          "provenance"
        )
      )
      id <- required(current, "id", path, string(_, s"$path.id"))
      program <- required(current, "program_id", path, string(_, s"$path.program_id"))
      term <- required(current, "term", path, loweredTerm(_, s"$path.term"))
      targetOperator <- required(current, "target_operator", path, string(_, s"$path.target_operator"))
      method <- required(current, "method", path, splitMethod(_, s"$path.method"))
      capabilities <- required(
        current,
        "available_capabilities",
        path,
        vector(_, s"$path.available_capabilities", splitMethod)
      )
      auxiliary <- required(current, "auxiliary", path, auxiliaryConstraint(_, s"$path.auxiliary"))
      provenanceValue <- required(current, "provenance", path, vector(_, s"$path.provenance", provenance))
    yield ProgramCompositeLoweringIr(
      id,
      program,
      term,
      targetOperator,
      method,
      capabilities,
      auxiliary,
      provenanceValue
    )

  private def loweredTerm(value: IrJson, path: String): Either[IrError, ProgramLoweredTermIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "penalty" | "constraint" =>
          exact(current, path, Set("kind", "index"))
            .flatMap(checked => required(checked, "index", path, integer(_, s"$path.index")))
            .map(index => if kind == "penalty" then ProgramLoweredTermIr.Penalty(index) else ProgramLoweredTermIr.Constraint(index))
        case _ => malformed(path, s"unknown lowered term '$kind'")

  private def auxiliaryConstraint(value: IrJson, path: String): Either[IrError, ProgramAuxiliaryConstraintIr] =
    for
      current <- fields(value, path, Set("variable_id", "target", "equation"))
      variable <- required(current, "variable_id", path, string(_, s"$path.variable_id"))
      targetValue <- required(current, "target", path, target(_, s"$path.target"))
      equation <- required(current, "equation", path, auxiliaryEquation(_, s"$path.equation"))
    yield ProgramAuxiliaryConstraintIr(variable, targetValue, equation)

  private def auxiliaryEquation(value: IrJson, path: String): Either[IrError, ProgramAuxiliaryEquationIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "target_copy" => exact(current, path, Set("kind")).map(_ => ProgramAuxiliaryEquationIr.TargetCopy)
        case "latent_group_sum" =>
          exact(current, path, Set("kind", "groups_identity"))
            .flatMap(checked => required(checked, "groups_identity", path, string(_, s"$path.groups_identity")))
            .map(ProgramAuxiliaryEquationIr.LatentGroupSum.apply)
        case _ => malformed(path, s"unknown auxiliary equation '$kind'")

  private def splitMethod(value: IrJson, path: String): Either[IrError, ProgramSplitMethodIr] =
    string(value, path).flatMap:
      case "primal_dual" => Right(ProgramSplitMethodIr.PrimalDual)
      case "admm" => Right(ProgramSplitMethodIr.Admm)
      case "augmented_lagrangian" => Right(ProgramSplitMethodIr.AugmentedLagrangian)
      case "conic" => Right(ProgramSplitMethodIr.Conic)
      case other => malformed(path, s"unknown split method '$other'")

  private def tolerance(value: IrJson, path: String): Either[IrError, ToleranceIr] =
    for
      current <- fields(value, path, Set("absolute", "relative"))
      absolute <- required(current, "absolute", path, number(_, s"$path.absolute"))
      relative <- required(current, "relative", path, number(_, s"$path.relative"))
    yield ToleranceIr(absolute, relative)

  private def certificate(value: IrJson, path: String): Either[IrError, CertificateIr] =
    for
      current <- fields(value, path, Set("property", "value_identity", "tolerance", "norm", "method", "precision", "backend", "regularization", "residual"))
      property <- required(current, "property", path, string(_, s"$path.property"))
      identity <- required(current, "value_identity", path, string(_, s"$path.value_identity"))
      currentTolerance <- required(current, "tolerance", path, tolerance(_, s"$path.tolerance"))
      norm <- required(current, "norm", path, string(_, s"$path.norm"))
      method <- required(current, "method", path, string(_, s"$path.method"))
      precision <- required(current, "precision", path, string(_, s"$path.precision"))
      backend <- required(current, "backend", path, string(_, s"$path.backend"))
      regularization <- required(current, "regularization", path, optionalString(_, s"$path.regularization"))
      residual <- required(current, "residual", path, optionalNumber(_, s"$path.residual"))
    yield CertificateIr(property, identity, currentTolerance, norm, method, precision, backend, regularization, residual)

  private def provenance(value: IrJson, path: String): Either[IrError, ProvenanceEventIr] =
    tagged(value, path).flatMap: (kind, current) =>
      kind match
        case "source" =>
          exact(current, path, Set("kind", "label"))
            .flatMap(checked => required(checked, "label", path, string(_, s"$path.label")))
            .map(ProvenanceEventIr.Source.apply)
        case "adapted" =>
          exact(current, path, Set("kind", "adapter"))
            .flatMap(checked => required(checked, "adapter", path, string(_, s"$path.adapter")))
            .map(ProvenanceEventIr.Adapted.apply)
        case "derived" =>
          for
            checked <- exact(current, path, Set("kind", "operation", "inputs"))
            operation <- required(checked, "operation", path, string(_, s"$path.operation"))
            inputs <- required(checked, "inputs", path, vector(_, s"$path.inputs", string))
          yield ProvenanceEventIr.Derived(operation, inputs)
        case "certified" =>
          for
            checked <- exact(current, path, Set("kind", "property", "method"))
            property <- required(checked, "property", path, string(_, s"$path.property"))
            method <- required(checked, "method", path, string(_, s"$path.method"))
          yield ProvenanceEventIr.Certified(property, method)
        case "unsafe_assumption" =>
          for
            checked <- exact(current, path, Set("kind", "property", "reason"))
            property <- required(checked, "property", path, string(_, s"$path.property"))
            reason <- required(checked, "reason", path, string(_, s"$path.reason"))
          yield ProvenanceEventIr.UnsafeAssumption(property, reason)
        case _ => malformed(path, s"unknown provenance event '$kind'")

  private def tagged(value: IrJson, path: String): Either[IrError, (String, Map[String, IrJson])] =
    fields(value, path, Set.empty, rejectUnknown = false).flatMap: current =>
      required(current, "kind", path, string(_, s"$path.kind")).map(_ -> current)

  private def exact(current: Map[String, IrJson], path: String, allowed: Set[String]): Either[IrError, Map[String, IrJson]] =
    current.keys.find(key => !allowed.contains(key)) match
      case Some(key) => Left(IrError(RejectionCategory.UnknownField, s"$path.$key", "unknown field"))
      case None => Right(current)

  private def fields(
      value: IrJson,
      path: String,
      allowed: Set[String],
      rejectUnknown: Boolean = true
  ): Either[IrError, Map[String, IrJson]] =
    value match
      case Obj(values) =>
        values.find((key, _) => rejectUnknown && !allowed.contains(key)) match
          case Some((key, _)) => Left(IrError(RejectionCategory.UnknownField, s"$path.$key", "unknown field"))
          case None => Right(values.toMap)
      case _ => malformed(path, "expected object")

  private def required[A](
      current: Map[String, IrJson],
      key: String,
      path: String,
      decode: IrJson => Either[IrError, A]
  ): Either[IrError, A] =
    current.get(key).toRight(IrError(RejectionCategory.Malformed, s"$path.$key", "missing required field")).flatMap(decode)

  private def vector[A](value: IrJson, path: String, decode: (IrJson, String) => Either[IrError, A]): Either[IrError, Vector[A]] =
    value match
      case Arr(values) =>
        values.zipWithIndex.foldLeft[Either[IrError, Vector[A]]](Right(Vector.empty)): (result, item) =>
          result.flatMap(current => decode(item._1, s"$path[${item._2}]").map(current :+ _))
      case _ => malformed(path, "expected array")

  private def string(value: IrJson, path: String): Either[IrError, String] =
    value match
      case Str(current) => Right(current)
      case _ => malformed(path, "expected string")

  private def number(value: IrJson, path: String): Either[IrError, Double] =
    value match
      case Num(current) => Right(current)
      case _ => malformed(path, "expected number")

  private def integer(value: IrJson, path: String): Either[IrError, Int] =
    number(value, path).flatMap: current =>
      if current == Math.rint(current) && current >= Int.MinValue && current <= Int.MaxValue then Right(current.toInt)
      else malformed(path, "expected integer")

  private def boolean(value: IrJson, path: String): Either[IrError, Boolean] =
    value match
      case Bool(current) => Right(current)
      case _ => malformed(path, "expected boolean")

  private def optionalString(value: IrJson, path: String): Either[IrError, Option[String]] =
    value match
      case Null => Right(None)
      case other => string(other, path).map(Some.apply)

  private def optionalNumber(value: IrJson, path: String): Either[IrError, Option[Double]] =
    value match
      case Null => Right(None)
      case other => number(other, path).map(Some.apply)

  private def spaceRole(value: IrJson, path: String): Either[IrError, SpaceRoleIr] =
    string(value, path).flatMap:
      case "samples" => Right(SpaceRoleIr.Samples)
      case "observed" => Right(SpaceRoleIr.Observed)
      case "latent" => Right(SpaceRoleIr.Latent)
      case "kernel" => Right(SpaceRoleIr.Kernel)
      case "block" => Right(SpaceRoleIr.Block)
      case other => malformed(path, s"unknown space role '$other'")

  private def variance(value: IrJson, path: String): Either[IrError, VarianceIr] =
    string(value, path).flatMap:
      case "primal" => Right(VarianceIr.Primal)
      case "dual" => Right(VarianceIr.Dual)
      case other => malformed(path, s"unknown variance '$other'")

  private def simpleOperatorRole(value: String, path: String): Either[IrError, ProgramOperatorRoleIr] =
    value match
      case "table" => Right(ProgramOperatorRoleIr.Table)
      case "metric" => Right(ProgramOperatorRoleIr.Metric)
      case "cometric" => Right(ProgramOperatorRoleIr.Cometric)
      case "covariance" => Right(ProgramOperatorRoleIr.Covariance)
      case "scatter" => Right(ProgramOperatorRoleIr.Scatter)
      case "penalty" => Right(ProgramOperatorRoleIr.Penalty)
      case "kernel" => Right(ProgramOperatorRoleIr.Kernel)
      case "row_link" => Right(ProgramOperatorRoleIr.RowLink)
      case "frame" => Right(ProgramOperatorRoleIr.Frame)
      case "cross" => Right(ProgramOperatorRoleIr.Cross)
      case "component" => Right(ProgramOperatorRoleIr.Component)
      case "score" => Right(ProgramOperatorRoleIr.Score)
      case "axis" => Right(ProgramOperatorRoleIr.Axis)
      case "coefficient" => Right(ProgramOperatorRoleIr.Coefficient)
      case "constraint_map" => Right(ProgramOperatorRoleIr.ConstraintMap)
      case other => malformed(path, s"unknown operator role '$other'")

  private def evidenceStatus(value: IrJson, path: String): Either[IrError, EvidenceStatusIr] =
    string(value, path).flatMap:
      case "unchecked" => Right(EvidenceStatusIr.Unchecked)
      case "certified" => Right(EvidenceStatusIr.Certified)
      case "assumed" => Right(EvidenceStatusIr.Assumed)
      case other => malformed(path, s"unknown evidence status '$other'")

  private def representation(value: IrJson, path: String): Either[IrError, ProgramRepresentationIr] =
    string(value, path).flatMap:
      case "dense" => Right(ProgramRepresentationIr.Dense)
      case "sparse" => Right(ProgramRepresentationIr.Sparse)
      case "diagonal" => Right(ProgramRepresentationIr.Diagonal)
      case "block" => Right(ProgramRepresentationIr.Block)
      case "low_rank" => Right(ProgramRepresentationIr.LowRank)
      case "kronecker" => Right(ProgramRepresentationIr.Kronecker)
      case "lazy_affine" => Right(ProgramRepresentationIr.LazyAffine)
      case "matrix_free" => Right(ProgramRepresentationIr.MatrixFree)
      case other => malformed(path, s"unknown representation '$other'")

  private def targetCapability(value: IrJson, path: String): Either[IrError, ProgramTargetCapabilityIr] =
    string(value, path).flatMap:
      case "linear" => Right(ProgramTargetCapabilityIr.Linear)
      case "affine" => Right(ProgramTargetCapabilityIr.Affine)
      case "smooth" => Right(ProgramTargetCapabilityIr.Smooth)
      case "general" => Right(ProgramTargetCapabilityIr.General)
      case other => malformed(path, s"unknown target capability '$other'")

  private def simpleFunctional(value: String, path: String): Either[IrError, ProgramFunctionalIr] =
    value match
      case "l1" => Right(ProgramFunctionalIr.L1)
      case "group_l21" => Right(ProgramFunctionalIr.GroupL21)
      case "total_variation" => Right(ProgramFunctionalIr.TotalVariation)
      case "nuclear_norm" => Right(ProgramFunctionalIr.NuclearNorm)
      case "negative_log_det" => Right(ProgramFunctionalIr.NegativeLogDet)
      case other => malformed(path, s"unknown functional '$other'")

  private def simpleFeasibleSet(value: String, path: String): Either[IrError, ProgramFeasibleSetIr] =
    value match
      case "zero_subspace" => Right(ProgramFeasibleSetIr.ZeroSubspace)
      case "nonnegative_orthant" => Right(ProgramFeasibleSetIr.NonnegativeOrthant)
      case "simplex" => Right(ProgramFeasibleSetIr.Simplex)
      case "psd_cone" => Right(ProgramFeasibleSetIr.PsdCone)
      case "stiefel" => Right(ProgramFeasibleSetIr.Stiefel)
      case other => malformed(path, s"unknown feasible set '$other'")

  private def symmetry(value: IrJson, path: String): Either[IrError, ProgramFrameSymmetryIr] =
    string(value, path).flatMap:
      case "orthogonal" => Right(ProgramFrameSymmetryIr.Orthogonal)
      case "signed_permutation" => Right(ProgramFrameSymmetryIr.SignedPermutation)
      case "permutation" => Right(ProgramFrameSymmetryIr.Permutation)
      case "identity" => Right(ProgramFrameSymmetryIr.Identity)
      case other => malformed(path, s"unknown frame symmetry '$other'")

  private def simpleRepresentative(value: String, path: String): Either[IrError, ProgramRepresentativeIr] =
    value match
      case "deterministic_sign" => Right(ProgramRepresentativeIr.DeterministicSign)
      case "ordered_spectrum_then_sign" => Right(ProgramRepresentativeIr.OrderedSpectrumThenSign)
      case "prediction_map" => Right(ProgramRepresentativeIr.PredictionMap)
      case "objective_value_only" => Right(ProgramRepresentativeIr.ObjectiveValueOnly)
      case other => malformed(path, s"unknown representative '$other'")

  private def guarantee(value: IrJson, path: String): Either[IrError, ProgramSolverGuaranteeIr] =
    string(value, path).flatMap:
      case "global_spectral_optimum" => Right(ProgramSolverGuaranteeIr.GlobalSpectralOptimum)
      case "global_convex_optimum" => Right(ProgramSolverGuaranteeIr.GlobalConvexOptimum)
      case "stationary_point" => Right(ProgramSolverGuaranteeIr.StationaryPoint)
      case "feasible_point" => Right(ProgramSolverGuaranteeIr.FeasiblePoint)
      case "coordinatewise_stationary" => Right(ProgramSolverGuaranteeIr.CoordinatewiseStationary)
      case "locally_optimal" => Right(ProgramSolverGuaranteeIr.LocallyOptimal)
      case "heuristic_feasible" => Right(ProgramSolverGuaranteeIr.HeuristicFeasible)
      case "unresolved" => Right(ProgramSolverGuaranteeIr.Unresolved)
      case other => malformed(path, s"unknown solver guarantee '$other'")

  private def rewriteRule(value: IrJson, path: String): Either[IrError, ProgramRewriteRuleIr] =
    string(value, path).flatMap:
      case "exact_linear_reduction" => Right(ProgramRewriteRuleIr.ExactLinearReduction)
      case "support_restriction" => Right(ProgramRewriteRuleIr.SupportRestriction)
      case "quadratic_pullback" => Right(ProgramRewriteRuleIr.QuadraticPullback)
      case "generalized_to_standard_eigen" => Right(ProgramRewriteRuleIr.GeneralizedToStandardEigen)
      case "whitening" => Right(ProgramRewriteRuleIr.Whitening)
      case other => malformed(path, s"unknown rewrite rule '$other'")

  private def malformed[A](path: String, detail: String): Either[IrError, A] =
    Left(IrError(RejectionCategory.Malformed, path, detail))
