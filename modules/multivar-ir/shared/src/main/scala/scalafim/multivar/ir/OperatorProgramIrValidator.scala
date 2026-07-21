package scalafim.multivar.ir

object OperatorProgramIrValidator:
  def validate(document: OperatorProgramDocumentIr): Either[IrError, OperatorProgramDocumentIr] =
    for
      _ <- requireValue(
        document.schema == OperatorProgramDocumentIr.schemaV02,
        RejectionCategory.SchemaVersionMismatch,
        "$.schema",
        s"expected ${OperatorProgramDocumentIr.schemaV02}, got ${document.schema}"
      )
      spaces <- unique(document.spaces, _.id, "$.spaces")
      operators <- unique(document.operators, _.valueIdentity, "$.operators")
      programs <- unique(document.programs, _.id, "$.programs")
      _ <- validateSpaces(document.spaces)
      _ <- validateOperators(document.operators, spaces, operators)
      _ <- validatePolicies(document.operatorPolicies, operators)
      _ <- validatePrograms(document.programs, spaces, operators)
      _ <- validateCompositeLowerings(document.compositeLowerings, programs, operators)
      _ <- validateRewrites(document.rewrites, programs, operators)
      _ <- validateFits(document.fits, programs, operators)
    yield document

  private def validateSpaces(spaces: Vector[SpaceIr]): Either[IrError, Unit] =
    spaces.foldLeft[Either[IrError, Unit]](Right(())): (result, space) =>
      result.flatMap: _ =>
        requireValue(space.id.nonEmpty && space.dimension > 0, RejectionCategory.Malformed, s"spaces.${space.id}", "space id and dimension must be valid")

  private def validateOperators(
      values: Vector[ProgramOpIr],
      spaces: Map[String, SpaceIr],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    values.foldLeft[Either[IrError, Unit]](Right(())): (result, operator) =>
      result.flatMap: _ =>
        for
          _ <- validateCoordinate(operator.domain, spaces, s"operators.${operator.id}.domain")
          _ <- validateCoordinate(operator.codomain, spaces, s"operators.${operator.id}.codomain")
          _ <- validatePorts(operator, spaces)
          _ <- validateEvidence(operator)
          _ <- validateDerivation(operator, operators)
        yield ()

  private def validateCoordinate(
      coordinate: CoordinateIr,
      spaces: Map[String, SpaceIr],
      path: String
  ): Either[IrError, Unit] =
    requireValue(
      spaces.contains(coordinate.spaceId),
      RejectionCategory.DomainCodomainMismatch,
      path,
      s"unknown space '${coordinate.spaceId}'"
    )

  private def validatePorts(operator: ProgramOpIr, spaces: Map[String, SpaceIr]): Either[IrError, Unit] =
    val domain = spaces(operator.domain.spaceId)
    val codomain = spaces(operator.codomain.spaceId)
    val sameSpace = domain.id == codomain.id
    val valid =
      operator.role match
        case ProgramOperatorRoleIr.Table =>
          operator.domain.variance == VarianceIr.Dual && domain.role == SpaceRoleIr.Observed &&
            operator.codomain.variance == VarianceIr.Primal && codomain.role == SpaceRoleIr.Samples
        case ProgramOperatorRoleIr.Metric =>
          sameSpace && operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Dual
        case ProgramOperatorRoleIr.Cometric | ProgramOperatorRoleIr.Covariance | ProgramOperatorRoleIr.Scatter |
            ProgramOperatorRoleIr.Penalty | ProgramOperatorRoleIr.Kernel =>
          sameSpace && operator.domain.variance == VarianceIr.Dual && operator.codomain.variance == VarianceIr.Primal
        case ProgramOperatorRoleIr.RowLink =>
          domain.role == SpaceRoleIr.Samples && codomain.role == SpaceRoleIr.Samples &&
            operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Dual
        case ProgramOperatorRoleIr.Frame =>
          domain.role == SpaceRoleIr.Latent && codomain.role == SpaceRoleIr.Observed &&
            operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Dual
        case ProgramOperatorRoleIr.Cross =>
          domain.role == SpaceRoleIr.Observed && codomain.role == SpaceRoleIr.Observed &&
            operator.domain.variance == VarianceIr.Dual && operator.codomain.variance == VarianceIr.Primal
        case ProgramOperatorRoleIr.Component =>
          domain.role == SpaceRoleIr.Latent && codomain.role == SpaceRoleIr.Latent &&
            operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Dual
        case ProgramOperatorRoleIr.Score =>
          domain.role == SpaceRoleIr.Latent && codomain.role == SpaceRoleIr.Samples &&
            operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Primal
        case ProgramOperatorRoleIr.Axis =>
          domain.role == SpaceRoleIr.Latent && codomain.role == SpaceRoleIr.Observed &&
            operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Primal
        case ProgramOperatorRoleIr.Coefficient =>
          domain.role == SpaceRoleIr.Observed && codomain.role == SpaceRoleIr.Observed &&
            operator.domain.variance == VarianceIr.Dual && operator.codomain.variance == VarianceIr.Dual
        case ProgramOperatorRoleIr.ConstraintMap =>
          operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Primal
        case ProgramOperatorRoleIr.Composed(_, _) | ProgramOperatorRoleIr.Dual(_) |
            ProgramOperatorRoleIr.MetricAdjoint(_) => true
    requireValue(
      valid,
      RejectionCategory.DomainCodomainMismatch,
      s"operators.${operator.id}",
      s"${operator.role} is incompatible with ${operator.domain} -> ${operator.codomain}"
    )

  private def validateEvidence(operator: ProgramOpIr): Either[IrError, Unit] =
    val certificatesMatch =
      operator.evidence.certificates.forall(certificate => certificate.valueIdentity == operator.valueIdentity)
    operator.evidence.status match
      case EvidenceStatusIr.Unchecked =>
        requireValue(
          operator.evidence.certificates.isEmpty,
          RejectionCategory.UncertifiedPositivity,
          s"operators.${operator.id}.evidence",
          "unchecked evidence cannot carry certificates"
        )
      case EvidenceStatusIr.Certified =>
        requireValue(
          operator.evidence.certificates.nonEmpty && certificatesMatch,
          RejectionCategory.UncertifiedPositivity,
          s"operators.${operator.id}.evidence",
          "certified evidence requires value-bound certificates"
        )
      case EvidenceStatusIr.Assumed =>
        requireValue(
          operator.evidence.certificates.isEmpty && operator.provenance.exists(_.isInstanceOf[ProvenanceEventIr.UnsafeAssumption]),
          RejectionCategory.UncertifiedPositivity,
          s"operators.${operator.id}.evidence",
          "assumed evidence requires an explicit unsafe-assumption provenance event"
        )

  private def validateDerivation(
      output: ProgramOpIr,
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    def referenced(identity: String): Either[IrError, ProgramOpIr] =
      operators.get(identity).toRight(
        IrError(RejectionCategory.DomainCodomainMismatch, s"operators.${output.id}.derivation", s"unknown operator '$identity'")
      )
    output.derivation match
      case ProgramOperatorDerivationIr.Source => Right(())
      case ProgramOperatorDerivationIr.SecondOrder(sourceId, relationshipId, targetId) =>
        for
          source <- referenced(sourceId)
          relationship <- referenced(relationshipId)
          target <- referenced(targetId)
          _ <- requireValue(
            source.role == ProgramOperatorRoleIr.Table && target.role == ProgramOperatorRoleIr.Table &&
              relationship.role == ProgramOperatorRoleIr.RowLink &&
              relationship.codomain.spaceId == source.codomain.spaceId &&
              relationship.domain.spaceId == target.codomain.spaceId &&
              output.domain == target.domain && output.codomain.spaceId == source.domain.spaceId &&
              output.codomain.variance == VarianceIr.Primal &&
              (output.role == ProgramOperatorRoleIr.Cross || output.role == ProgramOperatorRoleIr.Covariance ||
                output.role == ProgramOperatorRoleIr.Scatter),
            RejectionCategory.DomainCodomainMismatch,
            s"operators.${output.id}.derivation",
            "secondOrder orientation does not compose as X_source* L X_target"
          )
        yield ()
      case ProgramOperatorDerivationIr.Compress(sourceId, secondOrderId, targetId) =>
        for
          source <- referenced(sourceId)
          secondOrder <- referenced(secondOrderId)
          target <- referenced(targetId)
          _ <- requireValue(
            source.role == ProgramOperatorRoleIr.Frame && target.role == ProgramOperatorRoleIr.Frame &&
              (secondOrder.role == ProgramOperatorRoleIr.Cross || secondOrder.role == ProgramOperatorRoleIr.Covariance ||
                secondOrder.role == ProgramOperatorRoleIr.Scatter) &&
              secondOrder.codomain.spaceId == source.codomain.spaceId &&
              secondOrder.domain.spaceId == target.codomain.spaceId &&
              output.domain == target.domain && output.codomain.spaceId == source.domain.spaceId &&
              output.codomain.variance == VarianceIr.Dual && output.role == ProgramOperatorRoleIr.Component,
            RejectionCategory.DomainCodomainMismatch,
            s"operators.${output.id}.derivation",
            "compress orientation does not compose as W_source* S W_target"
          )
        yield ()
      case ProgramOperatorDerivationIr.Scores(frameId, tableId) =>
        for
          frame <- referenced(frameId)
          table <- referenced(tableId)
          _ <- requireValue(
            frame.role == ProgramOperatorRoleIr.Frame && table.role == ProgramOperatorRoleIr.Table &&
              frame.codomain.spaceId == table.domain.spaceId && output.domain == frame.domain &&
              output.codomain == table.codomain && output.role == ProgramOperatorRoleIr.Score,
            RejectionCategory.DomainCodomainMismatch,
            s"operators.${output.id}.derivation",
            "score derivation must be table composed with frame"
          )
        yield ()
      case ProgramOperatorDerivationIr.Axes(frameId, cometricId) =>
        for
          frame <- referenced(frameId)
          cometric <- referenced(cometricId)
          _ <- requireValue(
            frame.role == ProgramOperatorRoleIr.Frame && cometric.role == ProgramOperatorRoleIr.Cometric &&
              frame.codomain.spaceId == cometric.domain.spaceId && output.domain == frame.domain &&
              output.codomain == cometric.codomain && output.role == ProgramOperatorRoleIr.Axis,
            RejectionCategory.DomainCodomainMismatch,
            s"operators.${output.id}.derivation",
            "axis derivation must be cometric composed with frame"
          )
        yield ()
      case ProgramOperatorDerivationIr.Lowered(rule, inputs) =>
        for
          _ <- requireValue(rule.trim.nonEmpty && inputs.nonEmpty, RejectionCategory.Malformed, s"operators.${output.id}.derivation", "lowering rule and inputs must be explicit")
          _ <- inputs.foldLeft[Either[IrError, Unit]](Right(()))((result, identity) => result.flatMap(_ => referenced(identity).map(_ => ())))
        yield ()

  private def validatePrograms(
      values: Vector[OperatorProgramV2Ir],
      spaces: Map[String, SpaceIr],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    values.foldLeft[Either[IrError, Unit]](Right(())): (result, program) =>
      result.flatMap(_ => validateProgram(program, spaces, operators))

  private def validateProgram(
      program: OperatorProgramV2Ir,
      spaces: Map[String, SpaceIr],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    for
      parameters <- unique(program.parameters, _.id, s"programs.${program.id}.parameters")
      _ <- requireValue(parameters.nonEmpty, RejectionCategory.Malformed, s"programs.${program.id}", "program requires parameters")
      _ <- program.parameters.foldLeft[Either[IrError, Unit]](Right(())): (result, parameter) =>
        result.flatMap: _ =>
          requireValue(
            spaces.get(parameter.featureSpaceId).exists(_.role == SpaceRoleIr.Observed) &&
              spaces.get(parameter.componentSpaceId).exists(_.role == SpaceRoleIr.Latent),
            RejectionCategory.DomainCodomainMismatch,
            s"programs.${program.id}.parameters.${parameter.id}",
            "parameter requires observed feature and latent component spaces"
          )
      _ <- validateObjective(program, parameters, operators)
      _ <- validateNormalizations(program, parameters, operators)
      _ <- validateTerms(program, parameters)
      _ <- requireValue(
        program.result.parameterGauges.forall(_.trim.nonEmpty) &&
          program.result.parameterGauges.distinct.length == program.result.parameterGauges.length &&
          (program.result.parameterGauges.isEmpty || program.result.redundantCoordinates),
        RejectionCategory.Malformed,
        s"programs.${program.id}.result",
        "parameter gauges must be distinct, named, and imply redundant coordinates"
      )
    yield ()

  private def validatePolicies(
      policies: Vector[ProgramOperatorPolicyIr],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    unique(policies, _.id, "$.operator_policies").flatMap: _ =>
      policies.foldLeft[Either[IrError, Unit]](Right(())): (result, policy) =>
        result.flatMap: _ =>
          val selectionValid =
            policy.selection match
              case ProgramPolicySelectionIr.Fixed(strength) =>
                strength.isFinite && strength >= 0.0 && strength <= 1.0
              case ProgramPolicySelectionIr.FoldSelected(selector, candidates) =>
                selector.trim.nonEmpty && candidates.nonEmpty && candidates.distinct.length == candidates.length &&
                  candidates.forall(value => value.isFinite && value >= 0.0 && value <= 1.0)
          val scaleValid =
            policy.scaleMatching match
              case ProgramScaleMatchingIr.Fixed(value) => value.isFinite && value > 0.0
              case _ => true
          val customKindValid =
            policy.kind match
              case ProgramOperatorPolicyKindIr.Custom(name) => name.trim.nonEmpty
              case _ => true
          val hasDerivedProvenance = policy.provenance.exists:
            case ProvenanceEventIr.Derived(_, inputs) => inputs.nonEmpty && inputs.forall(policy.inputOperators.contains)
            case _ => false
          val unsafeDowngradeVisible =
            policy.scope != ProgramPolicyScopeIr.BlockwiseUnsafe || policy.preservation.exists:
              case ProgramPreservationClaimIr.EvidenceDowngraded(reason) => reason.trim.nonEmpty
              case _ => false
          val jointClaimsVisible =
            policy.scope != ProgramPolicyScopeIr.JointSystem ||
              (policy.preservation.contains(ProgramPreservationClaimIr.PsdPreserved) &&
                policy.preservation.contains(ProgramPreservationClaimIr.BlockAdjointsPreserved) &&
                policy.preservation.contains(ProgramPreservationClaimIr.SharedGaugePreserved))
          requireValue(
            policy.inputOperators.nonEmpty && policy.outputOperators.nonEmpty &&
              policy.inputOperators.forall(operators.contains) && policy.outputOperators.forall(operators.contains) &&
              selectionValid && scaleValid && customKindValid && policy.preservation.nonEmpty &&
              hasDerivedProvenance && unsafeDowngradeVisible && jointClaimsVisible,
            RejectionCategory.Malformed,
            s"operator_policies.${policy.id}",
            "operator policy requires known operators, valid selection and scale, preservation evidence, and derived provenance"
          )

  private def validateCompositeLowerings(
      lowerings: Vector[ProgramCompositeLoweringIr],
      programs: Map[String, OperatorProgramV2Ir],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    unique(lowerings, _.id, "$.composite_lowerings").flatMap: _ =>
      lowerings.foldLeft[Either[IrError, Unit]](Right(())): (result, lowering) =>
        result.flatMap: _ =>
          val termTarget = programs.get(lowering.programId).flatMap: program =>
            lowering.term match
              case ProgramLoweredTermIr.Penalty(index) => program.penalties.lift(index).map(_.target)
              case ProgramLoweredTermIr.Constraint(index) => program.constraints.lift(index).map(_.target)
          val capabilitiesValid =
            lowering.availableCapabilities.nonEmpty &&
              lowering.availableCapabilities.distinct.length == lowering.availableCapabilities.length &&
              lowering.availableCapabilities.contains(lowering.method)
          val equationValid =
            lowering.auxiliary.equation match
              case ProgramAuxiliaryEquationIr.TargetCopy => true
              case ProgramAuxiliaryEquationIr.LatentGroupSum(groups) => groups.trim.nonEmpty
          val hasDerivedProvenance = lowering.provenance.exists:
            case ProvenanceEventIr.Derived(_, inputs) => inputs.contains(lowering.targetOperator)
            case _ => false
          requireValue(
            operators.contains(lowering.targetOperator) &&
              termTarget.contains(lowering.auxiliary.target) &&
              lowering.auxiliary.target.capability == ProgramTargetCapabilityIr.Linear &&
              lowering.auxiliary.target.operatorIdentities.contains(lowering.targetOperator) &&
              lowering.auxiliary.variableId.trim.nonEmpty && capabilitiesValid && equationValid && hasDerivedProvenance,
            RejectionCategory.Malformed,
            s"composite_lowerings.${lowering.id}",
            "composite lowering requires a known linear term target, explicit auxiliary equation, selected capability, and derived provenance"
          )

  private def validateObjective(
      program: OperatorProgramV2Ir,
      parameters: Map[String, ProgramFrameParameterIr],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    def parameter(id: String): Either[IrError, ProgramFrameParameterIr] =
      parameters.get(id).toRight(IrError(RejectionCategory.Malformed, s"programs.${program.id}.objective", s"unknown parameter '$id'"))
    def operator(identity: String): Either[IrError, ProgramOpIr] =
      operators.get(identity).toRight(IrError(RejectionCategory.DomainCodomainMismatch, s"programs.${program.id}.objective", s"unknown operator '$identity'"))
    def self(parameterId: String, identity: String): Either[IrError, ProgramOpIr] =
      for
        current <- parameter(parameterId)
        value <- operator(identity)
        _ <- requireValue(
          value.domain.spaceId == current.featureSpaceId && value.codomain.spaceId == current.featureSpaceId &&
            value.domain.variance == VarianceIr.Dual && value.codomain.variance == VarianceIr.Primal,
          RejectionCategory.DomainCodomainMismatch,
          s"programs.${program.id}.objective",
          s"operator '$identity' does not act on parameter '$parameterId'"
        )
      yield value
    def spd(value: ProgramOpIr): Either[IrError, Unit] =
      val certified = value.evidence.status == EvidenceStatusIr.Certified && value.evidence.certificates.exists(_.property == "spd")
      val assumed = value.evidence.status == EvidenceStatusIr.Assumed
      requireValue(certified || assumed, RejectionCategory.UncertifiedPositivity, s"programs.${program.id}.objective", "denominator geometry must carry explicit SPD evidence")
    program.objective match
      case ProgramObjectiveIr.MaximizeTrace(id, operatorId) => self(id, operatorId).map(_ => ())
      case ProgramObjectiveIr.MinimizeDisagreement(id, operatorId) => self(id, operatorId).map(_ => ())
      case ProgramObjectiveIr.GeneralizedRayleigh(id, numerator, denominator) =>
        for _ <- self(id, numerator); value <- self(id, denominator); _ <- spd(value) yield ()
      case ProgramObjectiveIr.TraceRatio(id, numerator, denominator) =>
        for _ <- self(id, numerator); value <- self(id, denominator); _ <- spd(value) yield ()
      case ProgramObjectiveIr.RatioTrace(id, numerator, denominator) =>
        for _ <- self(id, numerator); value <- self(id, denominator); _ <- spd(value) yield ()
      case ProgramObjectiveIr.MaximizeCrossTrace(sourceId, targetId, operatorId) =>
        for
          source <- parameter(sourceId)
          target <- parameter(targetId)
          value <- operator(operatorId)
          _ <- requireValue(
            sourceId != targetId && value.codomain.spaceId == source.featureSpaceId &&
              value.domain.spaceId == target.featureSpaceId && value.role == ProgramOperatorRoleIr.Cross,
            RejectionCategory.DomainCodomainMismatch,
            s"programs.${program.id}.objective",
            "cross objective endpoints do not match its parameters"
          )
        yield ()
      case ProgramObjectiveIr.SequentialCrossRegression(sourceId, targetId, crossId, predictorId) =>
        for
          source <- parameter(sourceId)
          target <- parameter(targetId)
          cross <- operator(crossId)
          _ <- self(sourceId, predictorId)
          _ <- requireValue(
            sourceId != targetId && cross.codomain.spaceId == source.featureSpaceId &&
              cross.domain.spaceId == target.featureSpaceId && cross.role == ProgramOperatorRoleIr.Cross,
            RejectionCategory.DomainCodomainMismatch,
            s"programs.${program.id}.objective",
            "sequential regression endpoints do not match its parameters"
          )
        yield ()

  private def validateNormalizations(
      program: OperatorProgramV2Ir,
      parameters: Map[String, ProgramFrameParameterIr],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    val ids = program.normalizations.map(_.parameterId)
    for
      _ <- requireValue(ids.distinct.length == ids.length && ids.toSet == parameters.keySet, RejectionCategory.Malformed, s"programs.${program.id}.normalizations", "each parameter requires exactly one normalization")
      _ <- program.normalizations.foldLeft[Either[IrError, Unit]](Right(())): (result, normalization) =>
        result.flatMap: _ =>
          val parameter = parameters(normalization.parameterId)
          operators.get(normalization.operatorIdentity) match
            case None => Left(IrError(RejectionCategory.DomainCodomainMismatch, s"programs.${program.id}.normalizations", "unknown normalization operator"))
            case Some(operator) =>
              val certified = operator.evidence.status == EvidenceStatusIr.Certified && operator.evidence.certificates.exists(_.property == "spd")
              val assumed = operator.evidence.status == EvidenceStatusIr.Assumed
              requireValue(
                operator.domain.spaceId == parameter.featureSpaceId && operator.codomain.spaceId == parameter.featureSpaceId &&
                  (certified || assumed),
                RejectionCategory.UncertifiedPositivity,
                s"programs.${program.id}.normalizations.${normalization.parameterId}",
                "normalization must be an SPD operator on the parameter feature space"
              )
    yield ()

  private def validateTerms(
      program: OperatorProgramV2Ir,
      parameters: Map[String, ProgramFrameParameterIr]
  ): Either[IrError, Unit] =
    val targets = program.penalties.map(_.target) ++ program.constraints.map(_.target)
    for
      _ <- requireValue(
        targets.forall: target =>
          target.parameterIds.nonEmpty && target.parameterIds.distinct.length == target.parameterIds.length &&
            target.parameterIds.forall(parameters.contains) && target.operation.trim.nonEmpty,
        RejectionCategory.Malformed,
        s"programs.${program.id}.terms",
        "term target must name distinct known parameters and an operation"
      )
      _ <- requireValue(program.penalties.forall(term => term.weight.isFinite && term.weight > 0.0), RejectionCategory.Malformed, s"programs.${program.id}.penalties", "penalty weights must be finite and positive")
    yield ()

  private def validateRewrites(
      rewrites: Vector[ProgramRewriteIr],
      programs: Map[String, OperatorProgramV2Ir],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    unique(rewrites, _.id, "$.rewrites").flatMap: _ =>
      rewrites.foldLeft[Either[IrError, Unit]](Right(())): (result, rewrite) =>
        result.flatMap: _ =>
          val hasDerivedProvenance = rewrite.provenance.exists:
            case ProvenanceEventIr.Derived(_, inputs) => inputs.nonEmpty
            case _ => false
          requireValue(
            rewrite.originalProgramId != rewrite.loweredProgramId &&
              programs.contains(rewrite.originalProgramId) && programs.contains(rewrite.loweredProgramId) &&
              rewrite.inputOperators.nonEmpty && rewrite.outputOperators.nonEmpty &&
              rewrite.inputOperators.forall(operators.contains) && rewrite.outputOperators.forall(operators.contains) &&
              rewrite.proof.property == "rewrite" && rewrite.proof.valueIdentity == rewrite.id && hasDerivedProvenance,
            RejectionCategory.Malformed,
            s"rewrites.${rewrite.id}",
            "rewrite requires distinct programs, known operators, a bound proof, and derived provenance"
          )

  private def validateFits(
      fits: Vector[ProgramFitIr],
      programs: Map[String, OperatorProgramV2Ir],
      operators: Map[String, ProgramOpIr]
  ): Either[IrError, Unit] =
    fits.foldLeft[Either[IrError, Unit]](Right(())): (result, fit) =>
      result.flatMap: _ =>
        programs.get(fit.programId) match
          case None => Left(IrError(RejectionCategory.Malformed, s"fits.${fit.programId}", "unknown program"))
          case Some(program) =>
            val expected = program.parameters.map(_.id).toSet
            val actual = fit.frames.map(_.parameterId)
            val operatorsKnown = fit.frames.forall: frame =>
              operators.get(frame.weightsIdentity).exists(_.role == ProgramOperatorRoleIr.Frame) &&
                frame.cometricIdentity.forall(identity => operators.get(identity).exists(_.role == ProgramOperatorRoleIr.Cometric)) &&
                frame.scoreIdentities.forall(identity => operators.get(identity).exists(_.role == ProgramOperatorRoleIr.Score)) &&
                frame.axisIdentity.forall(identity => operators.get(identity).exists(_.role == ProgramOperatorRoleIr.Axis))
            requireValue(
              fit.objectiveValue.isFinite && fit.retainedRank >= 0 &&
                actual.distinct.length == actual.length && actual.toSet == expected && operatorsKnown &&
                fit.spectralClusters.flatten.distinct.length == fit.spectralClusters.flatten.length &&
                fit.residualCertificates.forall(certificate => certificate.property == "converged" && certificate.residual.exists(value => value.isFinite && value >= 0.0)),
              RejectionCategory.Malformed,
              s"fits.${fit.programId}",
              "fit must match its program, reference typed frames, and carry valid residual certificates"
            )

  private def unique[A](values: Vector[A], key: A => String, path: String): Either[IrError, Map[String, A]] =
    val keyed = values.map(value => key(value) -> value)
    if keyed.map(_._1).exists(_.trim.isEmpty) || keyed.map(_._1).distinct.length != keyed.length then
      Left(IrError(RejectionCategory.Malformed, path, "identities must be non-empty and unique"))
    else Right(keyed.toMap)

  private def requireValue(
      condition: Boolean,
      category: RejectionCategory,
      path: String,
      detail: String
  ): Either[IrError, Unit] =
    if condition then Right(()) else Left(IrError(category, path, detail))
