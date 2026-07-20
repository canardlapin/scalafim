package scalafim.multivar.ir

final case class IrCapabilities(singularityPolicies: Set[String])

object IrCapabilities:
  val all: IrCapabilities =
    IrCapabilities(Set("reject", "restrict_to_support", "regularize", "quotient"))

object IrValidator:
  def validate(
      document: MultivarIrDocument,
      capabilities: IrCapabilities = IrCapabilities.all
  ): Either[IrError, MultivarIrDocument] =
    for
      _ <- validateSchema(document.schema)
      spaces <- unique(document.spaces.map(space => space.id -> space), "spaces")
      operators <- unique(document.operators.map(operator => operator.id -> operator), "operators")
      forms <- unique(document.forms.map(form => form.id -> form), "forms")
      measures <- unique(document.measures.map(measure => measure.id -> measure), "measures")
      _ <- unique(document.diagrams.map(diagram => diagram.id -> diagram), "diagrams")
      _ <- unique(document.relationships.map(relationship => relationship.id -> relationship), "relationships")
      _ <- unique(document.objectives.map(objective => objective.id -> objective), "objectives")
      _ <- validateSpaces(document.spaces)
      _ <- validateOperators(document.operators, spaces)
      _ <- validateForms(document.forms, spaces, operators)
      _ <- validateMeasures(document.measures, spaces)
      _ <- validateDiagrams(document.diagrams, operators, forms, measures, capabilities)
      _ <- validateRelationships(document.relationships, operators)
      _ <- validateObjectives(document.objectives)
    yield document

  private def validateSchema(schema: SchemaHeader): Either[IrError, Unit] =
    if schema.name != "scalafim-multivar-ir" then
      Left(IrError(RejectionCategory.SchemaVersionMismatch, "schema.name", "unsupported schema family"))
    else if schema.version != SchemaVersion.v0_1 then
      Left(
        IrError(
          RejectionCategory.SchemaVersionMismatch,
          "schema.version",
          s"decoder supports 0.1, got ${schema.version.major}.${schema.version.minor}"
        )
      )
    else Right(())

  private def unique[A](entries: Vector[(String, A)], path: String): Either[IrError, Map[String, A]] =
    val ids = entries.map(_._1)
    if ids.exists(_.trim.isEmpty) then Left(IrError(RejectionCategory.Malformed, path, "ids must be non-empty"))
    else if ids.distinct.length != ids.length then
      Left(IrError(RejectionCategory.Malformed, path, "ids must be unique"))
    else Right(entries.toMap)

  private def validateSpaces(spaces: Vector[SpaceIr]): Either[IrError, Unit] =
    spaces.find(_.dimension <= 0) match
      case Some(space) =>
        Left(IrError(RejectionCategory.Malformed, s"spaces.${space.id}.dimension", "must be positive"))
      case None => Right(())

  private def validateOperators(
      operators: Vector[OperatorIr],
      spaces: Map[String, SpaceIr]
  ): Either[IrError, Unit] =
    operators.foldLeft[Either[IrError, Unit]](Right(())) { (result, operator) =>
      result.flatMap { _ =>
        for
          domain <- spaces.get(operator.domain.spaceId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"operators.${operator.id}.domain", "unknown space")
          )
          codomain <- spaces.get(operator.codomain.spaceId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"operators.${operator.id}.codomain", "unknown space")
          )
          _ <-
            if operator.payload.columns == domain.dimension && operator.payload.rows == codomain.dimension then Right(())
            else
              Left(
                IrError(
                  RejectionCategory.DomainCodomainMismatch,
                  s"operators.${operator.id}.payload",
                  s"payload is ${operator.payload.rows}x${operator.payload.columns}, expected ${codomain.dimension}x${domain.dimension}"
                )
              )
          _ <- validateOperatorOrientation(operator)
          _ <- validatePayload(operator.payload, s"operators.${operator.id}.payload")
        yield ()
      }
    }

  private def validateOperatorOrientation(operator: OperatorIr): Either[IrError, Unit] =
    val valid =
      operator.role match
        case OperatorRoleIr.Table =>
          operator.domain.variance == VarianceIr.Dual && operator.codomain.variance == VarianceIr.Primal
        case OperatorRoleIr.RowMap | OperatorRoleIr.LinearConstraint =>
          operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Primal
        case OperatorRoleIr.RowLink =>
          operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Dual
        case OperatorRoleIr.LinearMap => true
    if valid then Right(())
    else
      Left(
        IrError(
          RejectionCategory.DomainCodomainMismatch,
          s"operators.${operator.id}",
          s"${operator.role} has incompatible primal or dual orientation"
        )
      )

  private def validatePayload(payload: PayloadIr, path: String): Either[IrError, Unit] =
    payload match
      case PayloadIr.InlineDense(rows, columns, values, sha256) =>
        if rows <= 0 || columns <= 0 || values.length != rows * columns || values.exists(!_.isFinite) then
          Left(IrError(RejectionCategory.Malformed, path, "invalid inline dense payload"))
        else if IrPayloadHash.dense(rows, columns, values) != sha256 then
          Left(IrError(RejectionCategory.PayloadTampered, s"$path.sha256", "inline dense digest mismatch"))
        else Right(())
      case PayloadIr.InlineSparse(rows, columns, rowIndices, columnIndices, values, sha256) =>
        if rows <= 0 || columns <= 0 || rowIndices.length != columnIndices.length ||
            rowIndices.length != values.length || values.exists(!_.isFinite)
        then Left(IrError(RejectionCategory.Malformed, path, "invalid inline sparse payload"))
        else if rowIndices.indices.exists(index => rowIndices(index) < 0 || rowIndices(index) >= rows) ||
            columnIndices.indices.exists(index => columnIndices(index) < 0 || columnIndices(index) >= columns)
        then Left(IrError(RejectionCategory.Malformed, path, "inline sparse index is out of bounds"))
        else if IrPayloadHash.sparse(rows, columns, rowIndices, columnIndices, values) != sha256 then
          Left(IrError(RejectionCategory.PayloadTampered, s"$path.sha256", "inline sparse digest mismatch"))
        else Right(())
      case PayloadIr.External(uri, mediaType, rows, columns, sha256) =>
        if uri.trim.isEmpty || mediaType.trim.isEmpty || rows <= 0 || columns <= 0 then
          Left(IrError(RejectionCategory.Malformed, path, "invalid external payload metadata"))
        else if !IrPayloadHash.isSha256(sha256) then
          Left(IrError(RejectionCategory.Malformed, s"$path.sha256", "expected a lowercase SHA-256 digest"))
        else Right(())

  private def validateForms(
      forms: Vector[FormIr],
      spaces: Map[String, SpaceIr],
      operators: Map[String, OperatorIr]
  ): Either[IrError, Unit] =
    forms.foldLeft[Either[IrError, Unit]](Right(())) { (result, form) =>
      result.flatMap { _ =>
        for
          _ <- spaces.get(form.spaceId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"forms.${form.id}.space_id", "unknown space")
          )
          operator <- operators.get(form.operatorId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"forms.${form.id}.operator_id", "unknown operator")
          )
          _ <- validateFormOrientation(form, operator)
          _ <- validatePositivity(form, operator)
          _ <- form.scaleSemantics match
            case ScaleSemanticsIr.ShapeMetric(gaugeId) if gaugeId.trim.isEmpty =>
              Left(IrError(RejectionCategory.Malformed, s"forms.${form.id}.scale_semantics.gauge_id", "must be non-empty"))
            case _ => Right(())
        yield ()
      }
    }

  private def validateFormOrientation(form: FormIr, operator: OperatorIr): Either[IrError, Unit] =
    val sameSpace = operator.domain.spaceId == form.spaceId && operator.codomain.spaceId == form.spaceId
    val dualToPrimal = Set(FormRoleIr.Cometric, FormRoleIr.Kernel, FormRoleIr.Covariance).contains(form.role)
    val orientation =
      if dualToPrimal then
        operator.domain.variance == VarianceIr.Dual && operator.codomain.variance == VarianceIr.Primal
      else operator.domain.variance == VarianceIr.Primal && operator.codomain.variance == VarianceIr.Dual
    if sameSpace && orientation then Right(())
    else
      Left(
        IrError(
          RejectionCategory.DomainCodomainMismatch,
          s"forms.${form.id}",
          "form operator has incompatible space or primal/dual orientation"
        )
      )

  private def validatePositivity(form: FormIr, operator: OperatorIr): Either[IrError, Unit] =
    val properties = form.certificates.filter(_.valueIdentity == operator.valueIdentity).map(_.property.toLowerCase).toSet
    val certified =
      form.positivity match
        case PositivityIr.Spd => properties.contains("spd")
        case PositivityIr.Psd => properties.contains("psd") || properties.contains("spd")
        case PositivityIr.Indefinite => properties.contains("indefinite") || form.evidence == EvidenceStatusIr.Assumed
        case PositivityIr.Unchecked => true
    if form.evidence == EvidenceStatusIr.Certified && !certified then
      Left(
        IrError(
          RejectionCategory.UncertifiedPositivity,
          s"forms.${form.id}.certificates",
          s"${form.positivity} requires a certificate bound to ${operator.valueIdentity}"
        )
      )
    else if (form.positivity == PositivityIr.Spd || form.positivity == PositivityIr.Psd) &&
        form.evidence == EvidenceStatusIr.Unchecked
    then
      Left(
        IrError(
          RejectionCategory.UncertifiedPositivity,
          s"forms.${form.id}.evidence",
          "positive geometry must be certified or explicitly assumed"
        )
      )
    else Right(())

  private def validateMeasures(
      measures: Vector[MeasureIr],
      spaces: Map[String, SpaceIr]
  ): Either[IrError, Unit] =
    measures.foldLeft[Either[IrError, Unit]](Right(())) { (result, measure) =>
      result.flatMap { _ =>
        spaces.get(measure.spaceId) match
          case None => Left(IrError(RejectionCategory.DomainCodomainMismatch, s"measures.${measure.id}", "unknown space"))
          case Some(space) =>
            if measure.weights.rows != space.dimension || measure.weights.columns != 1 then
              Left(
                IrError(
                  RejectionCategory.DomainCodomainMismatch,
                  s"measures.${measure.id}.weights",
                  "measure payload must be a column vector over its space"
                )
              )
            else validatePayload(measure.weights, s"measures.${measure.id}.weights")
      }
    }

  private def validateDiagrams(
      diagrams: Vector[DiagramIr],
      operators: Map[String, OperatorIr],
      forms: Map[String, FormIr],
      measures: Map[String, MeasureIr],
      capabilities: IrCapabilities
  ): Either[IrError, Unit] =
    diagrams.foldLeft[Either[IrError, Unit]](Right(())) { (result, diagram) =>
      result.flatMap { _ =>
        for
          table <- operators.get(diagram.tableOperatorId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"diagrams.${diagram.id}.table", "unknown operator")
          )
          rowForm <- forms.get(diagram.rowFormId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"diagrams.${diagram.id}.row_form", "unknown form")
          )
          columnForm <- forms.get(diagram.columnFormId).toRight(
            IrError(RejectionCategory.DomainCodomainMismatch, s"diagrams.${diagram.id}.column_form", "unknown form")
          )
          _ <-
            if table.role == OperatorRoleIr.Table && table.codomain.spaceId == rowForm.spaceId &&
                table.domain.spaceId == columnForm.spaceId
            then Right(())
            else
              Left(
                IrError(
                  RejectionCategory.DomainCodomainMismatch,
                  s"diagrams.${diagram.id}",
                  "table endpoints do not match row and column forms"
                )
              )
          _ <- diagram.measureId match
            case Some(id) if !measures.contains(id) =>
              Left(IrError(RejectionCategory.DomainCodomainMismatch, s"diagrams.${diagram.id}.measure", "unknown measure"))
            case _ => Right(())
          _ <- validateSingularity(diagram.rowSingularity, capabilities, s"diagrams.${diagram.id}.row_singularity")
          _ <- validateSingularity(diagram.columnSingularity, capabilities, s"diagrams.${diagram.id}.column_singularity")
        yield ()
      }
    }

  private def validateSingularity(
      policy: SingularityPolicyIr,
      capabilities: IrCapabilities,
      path: String
  ): Either[IrError, Unit] =
    val tag =
      policy match
        case SingularityPolicyIr.Reject => "reject"
        case SingularityPolicyIr.RestrictToSupport(_) => "restrict_to_support"
        case SingularityPolicyIr.Regularize(_) => "regularize"
        case SingularityPolicyIr.Quotient(_) => "quotient"
    val parameterValid =
      policy match
        case SingularityPolicyIr.Reject => true
        case SingularityPolicyIr.RestrictToSupport(value) => value.isFinite && value >= 0.0
        case SingularityPolicyIr.Regularize(value) => value.isFinite && value > 0.0
        case SingularityPolicyIr.Quotient(value) => value.isFinite && value >= 0.0
    if !parameterValid then Left(IrError(RejectionCategory.Malformed, path, "invalid singularity parameter"))
    else if !capabilities.singularityPolicies.contains(tag) then
      Left(IrError(RejectionCategory.UnsupportedSingularity, path, s"policy '$tag' is unsupported"))
    else Right(())

  private def validateRelationships(
      relationships: Vector[RelationshipIr],
      operators: Map[String, OperatorIr]
  ): Either[IrError, Unit] =
    relationships.foldLeft[Either[IrError, Unit]](Right(())) { (result, relationship) =>
      result.flatMap { _ =>
        operators.get(relationship.operatorId) match
          case None =>
            Left(IrError(RejectionCategory.DomainCodomainMismatch, s"relationships.${relationship.id}", "unknown operator"))
          case Some(operator) =>
            val mapKinds = Set(
              RelationshipKindIr.SameEntityEvidence,
              RelationshipKindIr.ExactBijection,
              RelationshipKindIr.PartialInjection,
              RelationshipKindIr.IncidenceMap,
              RelationshipKindIr.AggregationMap
            )
            val expectedRole = if mapKinds.contains(relationship.kind) then OperatorRoleIr.RowMap else OperatorRoleIr.RowLink
            if operator.role != expectedRole then
              Left(
                IrError(
                  RejectionCategory.IncompatibleAlignmentKind,
                  s"relationships.${relationship.id}.operator_id",
                  s"${relationship.kind} requires $expectedRole"
                )
              )
            else if relationship.kind == RelationshipKindIr.ProbabilisticCoupling &&
                (relationship.normalization != RelationshipNormalizationIr.UnitMass || relationship.marginals.isEmpty)
            then
              Left(
                IrError(
                  RejectionCategory.IncompatibleAlignmentKind,
                  s"relationships.${relationship.id}",
                  "probabilistic coupling requires unit-mass normalization and marginals"
                )
              )
            else if relationship.kind == RelationshipKindIr.SignedRowLink && relationship.marginals.nonEmpty then
              Left(
                IrError(
                  RejectionCategory.IncompatibleAlignmentKind,
                  s"relationships.${relationship.id}.marginals",
                  "signed row links cannot claim probabilistic marginals"
                )
              )
            else
              relationship.marginals match
                case None => Right(())
                case Some(marginals) =>
                  validatePayload(marginals.left, s"relationships.${relationship.id}.marginals.left")
                    .flatMap(_ => validatePayload(marginals.right, s"relationships.${relationship.id}.marginals.right"))
      }
    }

  private def validateObjectives(objectives: Vector[ObjectiveIr]): Either[IrError, Unit] =
    objectives.find { objective =>
      objective.formula match
        case ObjectiveFormulaIr.PairwiseAssociation =>
          objective.solverFormulation != SolverFormulationIr.SymmetricFeatureEigen
        case ObjectiveFormulaIr.QuadraticDisagreement =>
          objective.solverFormulation != SolverFormulationIr.QuadraticPenalty
        case ObjectiveFormulaIr.HardScoreAgreement =>
          objective.solverFormulation != SolverFormulationIr.LinearEquality
    } match
      case Some(objective) =>
        Left(
          IrError(
            RejectionCategory.Malformed,
            s"objectives.${objective.id}.solver_formulation",
            "solver formulation is incompatible with the nominal objective"
          )
        )
      case None => Right(())
