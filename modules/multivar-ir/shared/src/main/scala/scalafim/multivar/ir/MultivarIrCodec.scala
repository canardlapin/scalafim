package scalafim.multivar.ir

object MultivarIrCodec:
  def encode(document: MultivarIrDocument): String =
    IrJson.render(Encoder.document(document))

  def decode(text: String): Either[IrError, MultivarIrDocument] =
    IrJson.parse(text).flatMap(IrDecoder.document).flatMap(IrValidator.validate(_))

private object Encoder:
  import IrJson.*

  def document(value: MultivarIrDocument): IrJson =
    obj(
      "schema" -> schema(value.schema),
      "spaces" -> arr(value.spaces.map(space)),
      "operators" -> arr(value.operators.map(operator)),
      "forms" -> arr(value.forms.map(form)),
      "measures" -> arr(value.measures.map(measure)),
      "diagrams" -> arr(value.diagrams.map(diagram)),
      "relationships" -> arr(value.relationships.map(relationship)),
      "objectives" -> arr(value.objectives.map(objective))
    )

  private def schema(value: SchemaHeader): IrJson =
    obj(
      "name" -> Str(value.name),
      "version" -> obj("major" -> Num(value.version.major), "minor" -> Num(value.version.minor)),
      "unknown_fields" -> Str(tag(value.unknownFields))
    )

  private def space(value: SpaceIr): IrJson =
    obj("id" -> Str(value.id), "role" -> Str(tag(value.role)), "dimension" -> Num(value.dimension))

  private def coordinate(value: CoordinateIr): IrJson =
    obj("space_id" -> Str(value.spaceId), "variance" -> Str(tag(value.variance)))

  private def operator(value: OperatorIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "role" -> Str(tag(value.role)),
      "domain" -> coordinate(value.domain),
      "codomain" -> coordinate(value.codomain),
      "representation" -> Str(value.representation),
      "payload" -> payload(value.payload),
      "value_identity" -> Str(value.valueIdentity),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def payload(value: PayloadIr): IrJson =
    value match
      case PayloadIr.InlineDense(rows, columns, values, sha256) =>
        obj(
          "kind" -> Str("inline_dense"),
          "rows" -> Num(rows),
          "columns" -> Num(columns),
          "values" -> arr(values.map(Num.apply)),
          "sha256" -> Str(sha256)
        )
      case PayloadIr.InlineSparse(rows, columns, rowIndices, columnIndices, values, sha256) =>
        obj(
          "kind" -> Str("inline_sparse"),
          "rows" -> Num(rows),
          "columns" -> Num(columns),
          "row_indices" -> arr(rowIndices.map(value => Num(value.toDouble))),
          "column_indices" -> arr(columnIndices.map(value => Num(value.toDouble))),
          "values" -> arr(values.map(Num.apply)),
          "sha256" -> Str(sha256)
        )
      case PayloadIr.External(uri, mediaType, rows, columns, sha256) =>
        obj(
          "kind" -> Str("external"),
          "uri" -> Str(uri),
          "media_type" -> Str(mediaType),
          "rows" -> Num(rows),
          "columns" -> Num(columns),
          "sha256" -> Str(sha256)
        )

  private def form(value: FormIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "role" -> Str(tag(value.role)),
      "space_id" -> Str(value.spaceId),
      "operator_id" -> Str(value.operatorId),
      "structure" -> Str(tag(value.structure)),
      "positivity" -> Str(tag(value.positivity)),
      "evidence" -> Str(tag(value.evidence)),
      "scale_semantics" -> scaleSemantics(value.scaleSemantics),
      "certificates" -> arr(value.certificates.map(certificate))
    )

  private def scaleSemantics(value: ScaleSemanticsIr): IrJson =
    value match
      case ScaleSemanticsIr.AbsoluteMetric => obj("kind" -> Str("absolute_metric"))
      case ScaleSemanticsIr.NormalizedMetric => obj("kind" -> Str("normalized_metric"))
      case ScaleSemanticsIr.ShapeMetric(gaugeId) =>
        obj("kind" -> Str("shape_metric"), "gauge_id" -> Str(gaugeId))

  private def certificate(value: CertificateIr): IrJson =
    obj(
      "property" -> Str(value.property),
      "value_identity" -> Str(value.valueIdentity),
      "tolerance" -> obj(
        "absolute" -> Num(value.tolerance.absolute),
        "relative" -> Num(value.tolerance.relative)
      ),
      "norm" -> Str(value.norm),
      "method" -> Str(value.method),
      "precision" -> Str(value.precision),
      "backend" -> Str(value.backend),
      "regularization" -> value.regularization.fold[IrJson](Null)(Str.apply),
      "residual" -> value.residual.fold[IrJson](Null)(Num.apply)
    )

  private def measure(value: MeasureIr): IrJson =
    val normalization =
      value.normalization match
        case MeasureNormalizationIr.UnitMass(originalMass) =>
          obj("kind" -> Str("unit_mass"), "original_mass" -> Num(originalMass))
    obj(
      "id" -> Str(value.id),
      "space_id" -> Str(value.spaceId),
      "weights" -> payload(value.weights),
      "normalization" -> normalization,
      "value_identity" -> Str(value.valueIdentity)
    )

  private def diagram(value: DiagramIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "table_operator_id" -> Str(value.tableOperatorId),
      "row_form_id" -> Str(value.rowFormId),
      "column_form_id" -> Str(value.columnFormId),
      "measure_id" -> value.measureId.fold[IrJson](Null)(Str.apply),
      "centering" -> centering(value.centering),
      "row_singularity" -> singularity(value.rowSingularity),
      "column_singularity" -> singularity(value.columnSingularity),
      "missingness" -> missingness(value.missingness),
      "normalization" -> Str(value.normalization),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def centering(value: CenteringIr): IrJson =
    value match
      case CenteringIr.None => obj("kind" -> Str("none"))
      case CenteringIr.ByMeasure(measureId) =>
        obj("kind" -> Str("by_measure"), "measure_id" -> Str(measureId))
      case CenteringIr.Orthogonal(rowFormId, derivedMeasureId) =>
        obj(
          "kind" -> Str("orthogonal"),
          "row_form_id" -> Str(rowFormId),
          "derived_measure_id" -> derivedMeasureId.fold[IrJson](Null)(Str.apply)
        )
      case CenteringIr.AlreadyCentered(measureId, evidence) =>
        obj(
          "kind" -> Str("already_centered"),
          "measure_id" -> Str(measureId),
          "certificate" -> certificate(evidence)
        )

  private def singularity(value: SingularityPolicyIr): IrJson =
    value match
      case SingularityPolicyIr.Reject => obj("kind" -> Str("reject"))
      case SingularityPolicyIr.RestrictToSupport(threshold) =>
        obj("kind" -> Str("restrict_to_support"), "threshold" -> Num(threshold))
      case SingularityPolicyIr.Regularize(amount) =>
        obj("kind" -> Str("regularize"), "amount" -> Num(amount))
      case SingularityPolicyIr.Quotient(threshold) =>
        obj("kind" -> Str("quotient"), "threshold" -> Num(threshold))

  private def missingness(value: MissingnessIr): IrJson =
    value match
      case MissingnessIr.Complete => obj("kind" -> Str("complete"))
      case MissingnessIr.MissingCells(maskPayloadId) =>
        obj("kind" -> Str("missing_cells"), "mask_payload_id" -> Str(maskPayloadId))

  private def relationship(value: RelationshipIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "kind" -> Str(tag(value.kind)),
      "operator_id" -> Str(value.operatorId),
      "support" -> support(value.support),
      "normalization" -> Str(tag(value.normalization)),
      "marginals" -> value.marginals.fold[IrJson](Null)(marginals),
      "origin" -> Str(tag(value.origin)),
      "provenance" -> arr(value.provenance.map(provenance))
    )

  private def support(value: RelationshipSupportIr): IrJson =
    obj(
      "matched_mass" -> Num(value.matchedMass),
      "unmatched_source_mass" -> Num(value.unmatchedSourceMass),
      "unmatched_target_mass" -> Num(value.unmatchedTargetMass),
      "uncertain_mass" -> Num(value.uncertainMass),
      "excluded_mass" -> Num(value.excludedMass),
      "structural_zero_count" -> Num(value.structuralZeroCount),
      "cardinality" -> Str(tag(value.cardinality)),
      "duplicate_keys" -> arr(value.duplicateKeys.map(Str.apply)),
      "every_source_represented" -> Bool(value.everySourceRepresented),
      "every_target_represented" -> Bool(value.everyTargetRepresented)
    )

  private def marginals(value: MarginalsIr): IrJson =
    obj(
      "left" -> payload(value.left),
      "right" -> payload(value.right),
      "total_mass" -> Num(value.totalMass)
    )

  private def objective(value: ObjectiveIr): IrJson =
    obj(
      "id" -> Str(value.id),
      "formula" -> Str(tag(value.formula)),
      "rendered_formula" -> Str(value.renderedFormula),
      "constraints" -> arr(value.constraints.map(Str.apply)),
      "normalization" -> Str(tag(value.normalization)),
      "solver_formulation" -> Str(tag(value.solverFormulation)),
      "solved_to_reported" -> solvedToReported(value.solvedToReported)
    )

  private def solvedToReported(value: SolvedToReportedIr): IrJson =
    value match
      case SolvedToReportedIr.Identical => obj("kind" -> Str("identical"))
      case SolvedToReportedIr.Scale(factor) => obj("kind" -> Str("scale"), "factor" -> Num(factor))
      case SolvedToReportedIr.Transform(description) =>
        obj("kind" -> Str("transform"), "description" -> Str(description))

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

  private def obj(fields: (String, IrJson)*): IrJson = Obj(fields.toVector)
  private def arr(values: Vector[IrJson]): IrJson = Arr(values)

  private def tag(value: Product): String =
    val name = value.productPrefix
    val out = new StringBuilder
    name.zipWithIndex.foreach { case (character, index) =>
      if character.isUpper && index > 0 then out.append('_')
      out.append(character.toLower)
    }
    out.result()

private object IrDecoder:
  import IrJson.*

  def document(value: IrJson): Either[IrError, MultivarIrDocument] =
    for
      fields <- objectFields(
        value,
        "$",
        Set("schema", "spaces", "operators", "forms", "measures", "diagrams", "relationships", "objectives")
      )
      schemaValue <- required(fields, "schema", "$", schema)
      spaces <- required(fields, "spaces", "$", vector(_, "$.spaces", space))
      operators <- required(fields, "operators", "$", vector(_, "$.operators", operator))
      forms <- required(fields, "forms", "$", vector(_, "$.forms", form))
      measures <- required(fields, "measures", "$", vector(_, "$.measures", measure))
      diagrams <- required(fields, "diagrams", "$", vector(_, "$.diagrams", diagram))
      relationships <- required(fields, "relationships", "$", vector(_, "$.relationships", relationship))
      objectives <- required(fields, "objectives", "$", vector(_, "$.objectives", objective))
    yield MultivarIrDocument(schemaValue, spaces, operators, forms, measures, diagrams, relationships, objectives)

  private def schema(value: IrJson): Either[IrError, SchemaHeader] =
    for
      fields <- objectFields(value, "$.schema", Set("name", "version", "unknown_fields"))
      name <- required(fields, "name", "$.schema", string(_, "$.schema.name"))
      versionValue <- required(fields, "version", "$.schema", version)
      unknown <- required(
        fields,
        "unknown_fields",
        "$.schema",
        enumValue(_, "$.schema.unknown_fields", Vector(UnknownFieldPolicy.Reject))
      )
    yield SchemaHeader(name, versionValue, unknown)

  private def version(value: IrJson): Either[IrError, SchemaVersion] =
    for
      fields <- objectFields(value, "$.schema.version", Set("major", "minor"))
      major <- required(fields, "major", "$.schema.version", integer(_, "$.schema.version.major"))
      minor <- required(fields, "minor", "$.schema.version", integer(_, "$.schema.version.minor"))
    yield SchemaVersion(major, minor)

  private def space(value: IrJson): Either[IrError, SpaceIr] =
    for
      fields <- objectFields(value, "space", Set("id", "role", "dimension"))
      id <- required(fields, "id", "space", string(_, "space.id"))
      role <- required(fields, "role", "space", enumValue(_, "space.role", SpaceRoleIr.values.toVector))
      dimension <- required(fields, "dimension", "space", integer(_, "space.dimension"))
    yield SpaceIr(id, role, dimension)

  private def coordinate(value: IrJson, path: String): Either[IrError, CoordinateIr] =
    for
      fields <- objectFields(value, path, Set("space_id", "variance"))
      spaceId <- required(fields, "space_id", path, string(_, s"$path.space_id"))
      variance <- required(fields, "variance", path, enumValue(_, s"$path.variance", VarianceIr.values.toVector))
    yield CoordinateIr(spaceId, variance)

  private def operator(value: IrJson): Either[IrError, OperatorIr] =
    for
      fields <- objectFields(
        value,
        "operator",
        Set("id", "role", "domain", "codomain", "representation", "payload", "value_identity", "provenance")
      )
      id <- required(fields, "id", "operator", string(_, "operator.id"))
      role <- required(fields, "role", "operator", enumValue(_, "operator.role", OperatorRoleIr.values.toVector))
      domain <- required(fields, "domain", "operator", coordinate(_, "operator.domain"))
      codomain <- required(fields, "codomain", "operator", coordinate(_, "operator.codomain"))
      representation <- required(fields, "representation", "operator", string(_, "operator.representation"))
      payloadValue <- required(fields, "payload", "operator", payload)
      valueIdentity <- required(fields, "value_identity", "operator", string(_, "operator.value_identity"))
      provenanceValue <- required(fields, "provenance", "operator", vector(_, "operator.provenance", provenance))
    yield OperatorIr(id, role, domain, codomain, representation, payloadValue, valueIdentity, provenanceValue)

  private def payload(value: IrJson): Either[IrError, PayloadIr] =
    for
      kind <- discriminator(value, "payload")
      result <- kind match
        case "inline_dense" =>
          for
            fields <- objectFields(value, "payload", Set("kind", "rows", "columns", "values", "sha256"))
            rows <- required(fields, "rows", "payload", integer(_, "payload.rows"))
            columns <- required(fields, "columns", "payload", integer(_, "payload.columns"))
            values <- required(fields, "values", "payload", vector(_, "payload.values", number(_, "payload.values[]")))
            sha256 <- required(fields, "sha256", "payload", string(_, "payload.sha256"))
          yield PayloadIr.InlineDense(rows, columns, values, sha256)
        case "inline_sparse" =>
          for
            fields <- objectFields(
              value,
              "payload",
              Set("kind", "rows", "columns", "row_indices", "column_indices", "values", "sha256")
            )
            rows <- required(fields, "rows", "payload", integer(_, "payload.rows"))
            columns <- required(fields, "columns", "payload", integer(_, "payload.columns"))
            rowIndices <- required(fields, "row_indices", "payload", vector(_, "payload.row_indices", integer(_, "payload.row_indices[]")))
            columnIndices <- required(
              fields,
              "column_indices",
              "payload",
              vector(_, "payload.column_indices", integer(_, "payload.column_indices[]"))
            )
            values <- required(fields, "values", "payload", vector(_, "payload.values", number(_, "payload.values[]")))
            sha256 <- required(fields, "sha256", "payload", string(_, "payload.sha256"))
          yield PayloadIr.InlineSparse(rows, columns, rowIndices, columnIndices, values, sha256)
        case "external" =>
          for
            fields <- objectFields(value, "payload", Set("kind", "uri", "media_type", "rows", "columns", "sha256"))
            uri <- required(fields, "uri", "payload", string(_, "payload.uri"))
            mediaType <- required(fields, "media_type", "payload", string(_, "payload.media_type"))
            rows <- required(fields, "rows", "payload", integer(_, "payload.rows"))
            columns <- required(fields, "columns", "payload", integer(_, "payload.columns"))
            sha256 <- required(fields, "sha256", "payload", string(_, "payload.sha256"))
          yield PayloadIr.External(uri, mediaType, rows, columns, sha256)
        case other => invalidTag("payload.kind", other)
    yield result

  private def form(value: IrJson): Either[IrError, FormIr] =
    for
      fields <- objectFields(
        value,
        "form",
        Set("id", "role", "space_id", "operator_id", "structure", "positivity", "evidence", "scale_semantics", "certificates")
      )
      id <- required(fields, "id", "form", string(_, "form.id"))
      role <- required(fields, "role", "form", enumValue(_, "form.role", FormRoleIr.values.toVector))
      spaceId <- required(fields, "space_id", "form", string(_, "form.space_id"))
      operatorId <- required(fields, "operator_id", "form", string(_, "form.operator_id"))
      structure <- required(fields, "structure", "form", enumValue(_, "form.structure", FormStructureIr.values.toVector))
      positivity <- required(fields, "positivity", "form", enumValue(_, "form.positivity", PositivityIr.values.toVector))
      evidence <- required(fields, "evidence", "form", enumValue(_, "form.evidence", EvidenceStatusIr.values.toVector))
      scale <- required(fields, "scale_semantics", "form", scaleSemantics)
      certificates <- required(fields, "certificates", "form", vector(_, "form.certificates", certificate))
    yield FormIr(id, role, spaceId, operatorId, structure, positivity, evidence, scale, certificates)

  private def scaleSemantics(value: IrJson): Either[IrError, ScaleSemanticsIr] =
    for
      kind <- discriminator(value, "form.scale_semantics")
      result <- kind match
        case "absolute_metric" =>
          objectFields(value, "form.scale_semantics", Set("kind")).map(_ => ScaleSemanticsIr.AbsoluteMetric)
        case "normalized_metric" =>
          objectFields(value, "form.scale_semantics", Set("kind")).map(_ => ScaleSemanticsIr.NormalizedMetric)
        case "shape_metric" =>
          objectFields(value, "form.scale_semantics", Set("kind", "gauge_id")).flatMap { fields =>
            required(fields, "gauge_id", "form.scale_semantics", string(_, "form.scale_semantics.gauge_id"))
              .map(ScaleSemanticsIr.ShapeMetric.apply)
          }
        case other => invalidTag("form.scale_semantics.kind", other)
    yield result

  private def certificate(value: IrJson): Either[IrError, CertificateIr] =
    for
      fields <- objectFields(
        value,
        "certificate",
        Set("property", "value_identity", "tolerance", "norm", "method", "precision", "backend", "regularization", "residual")
      )
      property <- required(fields, "property", "certificate", string(_, "certificate.property"))
      valueIdentity <- required(fields, "value_identity", "certificate", string(_, "certificate.value_identity"))
      tolerance <- required(fields, "tolerance", "certificate", tolerance)
      norm <- required(fields, "norm", "certificate", string(_, "certificate.norm"))
      method <- required(fields, "method", "certificate", string(_, "certificate.method"))
      precision <- required(fields, "precision", "certificate", string(_, "certificate.precision"))
      backend <- required(fields, "backend", "certificate", string(_, "certificate.backend"))
      regularization <- optional(fields, "regularization", "certificate", string(_, "certificate.regularization"))
      residual <- optional(fields, "residual", "certificate", number(_, "certificate.residual"))
    yield CertificateIr(property, valueIdentity, tolerance, norm, method, precision, backend, regularization, residual)

  private def tolerance(value: IrJson): Either[IrError, ToleranceIr] =
    for
      fields <- objectFields(value, "certificate.tolerance", Set("absolute", "relative"))
      absolute <- required(fields, "absolute", "certificate.tolerance", number(_, "certificate.tolerance.absolute"))
      relative <- required(fields, "relative", "certificate.tolerance", number(_, "certificate.tolerance.relative"))
    yield ToleranceIr(absolute, relative)

  private def measure(value: IrJson): Either[IrError, MeasureIr] =
    for
      fields <- objectFields(value, "measure", Set("id", "space_id", "weights", "normalization", "value_identity"))
      id <- required(fields, "id", "measure", string(_, "measure.id"))
      spaceId <- required(fields, "space_id", "measure", string(_, "measure.space_id"))
      weights <- required(fields, "weights", "measure", payload)
      normalization <- required(fields, "normalization", "measure", measureNormalization)
      valueIdentity <- required(fields, "value_identity", "measure", string(_, "measure.value_identity"))
    yield MeasureIr(id, spaceId, weights, normalization, valueIdentity)

  private def measureNormalization(value: IrJson): Either[IrError, MeasureNormalizationIr] =
    for
      kind <- discriminator(value, "measure.normalization")
      result <- kind match
        case "unit_mass" =>
          for
            fields <- objectFields(value, "measure.normalization", Set("kind", "original_mass"))
            mass <- required(fields, "original_mass", "measure.normalization", number(_, "measure.normalization.original_mass"))
          yield MeasureNormalizationIr.UnitMass(mass)
        case other => invalidTag("measure.normalization.kind", other)
    yield result

  private def diagram(value: IrJson): Either[IrError, DiagramIr] =
    for
      fields <- objectFields(
        value,
        "diagram",
        Set(
          "id",
          "table_operator_id",
          "row_form_id",
          "column_form_id",
          "measure_id",
          "centering",
          "row_singularity",
          "column_singularity",
          "missingness",
          "normalization",
          "provenance"
        )
      )
      id <- required(fields, "id", "diagram", string(_, "diagram.id"))
      table <- required(fields, "table_operator_id", "diagram", string(_, "diagram.table_operator_id"))
      rowForm <- required(fields, "row_form_id", "diagram", string(_, "diagram.row_form_id"))
      columnForm <- required(fields, "column_form_id", "diagram", string(_, "diagram.column_form_id"))
      measureId <- optional(fields, "measure_id", "diagram", string(_, "diagram.measure_id"))
      centeringValue <- required(fields, "centering", "diagram", centering)
      rowSingularity <- required(fields, "row_singularity", "diagram", singularity)
      columnSingularity <- required(fields, "column_singularity", "diagram", singularity)
      missingnessValue <- required(fields, "missingness", "diagram", missingness)
      normalization <- required(fields, "normalization", "diagram", string(_, "diagram.normalization"))
      provenanceValue <- required(fields, "provenance", "diagram", vector(_, "diagram.provenance", provenance))
    yield DiagramIr(
      id,
      table,
      rowForm,
      columnForm,
      measureId,
      centeringValue,
      rowSingularity,
      columnSingularity,
      missingnessValue,
      normalization,
      provenanceValue
    )

  private def centering(value: IrJson): Either[IrError, CenteringIr] =
    for
      kind <- discriminator(value, "centering")
      result <- kind match
        case "none" => objectFields(value, "centering", Set("kind")).map(_ => CenteringIr.None)
        case "by_measure" =>
          objectFields(value, "centering", Set("kind", "measure_id")).flatMap { fields =>
            required(fields, "measure_id", "centering", string(_, "centering.measure_id")).map(CenteringIr.ByMeasure.apply)
          }
        case "orthogonal" =>
          for
            fields <- objectFields(value, "centering", Set("kind", "row_form_id", "derived_measure_id"))
            form <- required(fields, "row_form_id", "centering", string(_, "centering.row_form_id"))
            measure <- optional(fields, "derived_measure_id", "centering", string(_, "centering.derived_measure_id"))
          yield CenteringIr.Orthogonal(form, measure)
        case "already_centered" =>
          for
            fields <- objectFields(value, "centering", Set("kind", "measure_id", "certificate"))
            measure <- required(fields, "measure_id", "centering", string(_, "centering.measure_id"))
            evidence <- required(fields, "certificate", "centering", certificate)
          yield CenteringIr.AlreadyCentered(measure, evidence)
        case other => invalidTag("centering.kind", other)
    yield result

  private def singularity(value: IrJson): Either[IrError, SingularityPolicyIr] =
    for
      kind <- discriminator(value, "singularity")
      result <- kind match
        case "reject" => objectFields(value, "singularity", Set("kind")).map(_ => SingularityPolicyIr.Reject)
        case "restrict_to_support" =>
          objectFields(value, "singularity", Set("kind", "threshold")).flatMap { fields =>
            required(fields, "threshold", "singularity", number(_, "singularity.threshold"))
              .map(SingularityPolicyIr.RestrictToSupport.apply)
          }
        case "regularize" =>
          objectFields(value, "singularity", Set("kind", "amount")).flatMap { fields =>
            required(fields, "amount", "singularity", number(_, "singularity.amount"))
              .map(SingularityPolicyIr.Regularize.apply)
          }
        case "quotient" =>
          objectFields(value, "singularity", Set("kind", "threshold")).flatMap { fields =>
            required(fields, "threshold", "singularity", number(_, "singularity.threshold"))
              .map(SingularityPolicyIr.Quotient.apply)
          }
        case other => invalidTag("singularity.kind", other)
    yield result

  private def missingness(value: IrJson): Either[IrError, MissingnessIr] =
    for
      kind <- discriminator(value, "missingness")
      result <- kind match
        case "complete" => objectFields(value, "missingness", Set("kind")).map(_ => MissingnessIr.Complete)
        case "missing_cells" =>
          objectFields(value, "missingness", Set("kind", "mask_payload_id")).flatMap { fields =>
            required(fields, "mask_payload_id", "missingness", string(_, "missingness.mask_payload_id"))
              .map(MissingnessIr.MissingCells.apply)
          }
        case other => invalidTag("missingness.kind", other)
    yield result

  private def relationship(value: IrJson): Either[IrError, RelationshipIr] =
    for
      fields <- objectFields(
        value,
        "relationship",
        Set("id", "kind", "operator_id", "support", "normalization", "marginals", "origin", "provenance")
      )
      id <- required(fields, "id", "relationship", string(_, "relationship.id"))
      kind <- required(fields, "kind", "relationship", enumValue(_, "relationship.kind", RelationshipKindIr.values.toVector))
      operatorId <- required(fields, "operator_id", "relationship", string(_, "relationship.operator_id"))
      supportValue <- required(fields, "support", "relationship", support)
      normalization <- required(
        fields,
        "normalization",
        "relationship",
        enumValue(_, "relationship.normalization", RelationshipNormalizationIr.values.toVector)
      )
      marginalsValue <- optional(fields, "marginals", "relationship", marginals)
      origin <- required(fields, "origin", "relationship", enumValue(_, "relationship.origin", AlignmentOriginIr.values.toVector))
      provenanceValue <- required(fields, "provenance", "relationship", vector(_, "relationship.provenance", provenance))
    yield RelationshipIr(id, kind, operatorId, supportValue, normalization, marginalsValue, origin, provenanceValue)

  private def support(value: IrJson): Either[IrError, RelationshipSupportIr] =
    for
      fields <- objectFields(
        value,
        "support",
        Set(
          "matched_mass",
          "unmatched_source_mass",
          "unmatched_target_mass",
          "uncertain_mass",
          "excluded_mass",
          "structural_zero_count",
          "cardinality",
          "duplicate_keys",
          "every_source_represented",
          "every_target_represented"
        )
      )
      matched <- required(fields, "matched_mass", "support", number(_, "support.matched_mass"))
      unmatchedSource <- required(fields, "unmatched_source_mass", "support", number(_, "support.unmatched_source_mass"))
      unmatchedTarget <- required(fields, "unmatched_target_mass", "support", number(_, "support.unmatched_target_mass"))
      uncertain <- required(fields, "uncertain_mass", "support", number(_, "support.uncertain_mass"))
      excluded <- required(fields, "excluded_mass", "support", number(_, "support.excluded_mass"))
      zeros <- required(fields, "structural_zero_count", "support", integer(_, "support.structural_zero_count"))
      cardinality <- required(fields, "cardinality", "support", enumValue(_, "support.cardinality", CardinalityIr.values.toVector))
      duplicates <- required(fields, "duplicate_keys", "support", vector(_, "support.duplicate_keys", string(_, "support.duplicate_keys[]")))
      everySource <- required(fields, "every_source_represented", "support", boolean(_, "support.every_source_represented"))
      everyTarget <- required(fields, "every_target_represented", "support", boolean(_, "support.every_target_represented"))
    yield RelationshipSupportIr(
      matched,
      unmatchedSource,
      unmatchedTarget,
      uncertain,
      excluded,
      zeros,
      cardinality,
      duplicates,
      everySource,
      everyTarget
    )

  private def marginals(value: IrJson): Either[IrError, MarginalsIr] =
    for
      fields <- objectFields(value, "marginals", Set("left", "right", "total_mass"))
      left <- required(fields, "left", "marginals", payload)
      right <- required(fields, "right", "marginals", payload)
      total <- required(fields, "total_mass", "marginals", number(_, "marginals.total_mass"))
    yield MarginalsIr(left, right, total)

  private def objective(value: IrJson): Either[IrError, ObjectiveIr] =
    for
      fields <- objectFields(
        value,
        "objective",
        Set("id", "formula", "rendered_formula", "constraints", "normalization", "solver_formulation", "solved_to_reported")
      )
      id <- required(fields, "id", "objective", string(_, "objective.id"))
      formula <- required(fields, "formula", "objective", enumValue(_, "objective.formula", ObjectiveFormulaIr.values.toVector))
      rendered <- required(fields, "rendered_formula", "objective", string(_, "objective.rendered_formula"))
      constraints <- required(fields, "constraints", "objective", vector(_, "objective.constraints", string(_, "objective.constraints[]")))
      normalization <- required(
        fields,
        "normalization",
        "objective",
        enumValue(_, "objective.normalization", ObjectiveNormalizationIr.values.toVector)
      )
      formulation <- required(
        fields,
        "solver_formulation",
        "objective",
        enumValue(_, "objective.solver_formulation", SolverFormulationIr.values.toVector)
      )
      relationship <- required(fields, "solved_to_reported", "objective", solvedToReported)
    yield ObjectiveIr(id, formula, rendered, constraints, normalization, formulation, relationship)

  private def solvedToReported(value: IrJson): Either[IrError, SolvedToReportedIr] =
    for
      kind <- discriminator(value, "solved_to_reported")
      result <- kind match
        case "identical" => objectFields(value, "solved_to_reported", Set("kind")).map(_ => SolvedToReportedIr.Identical)
        case "scale" =>
          objectFields(value, "solved_to_reported", Set("kind", "factor")).flatMap { fields =>
            required(fields, "factor", "solved_to_reported", number(_, "solved_to_reported.factor"))
              .map(SolvedToReportedIr.Scale.apply)
          }
        case "transform" =>
          objectFields(value, "solved_to_reported", Set("kind", "description")).flatMap { fields =>
            required(fields, "description", "solved_to_reported", string(_, "solved_to_reported.description"))
              .map(SolvedToReportedIr.Transform.apply)
          }
        case other => invalidTag("solved_to_reported.kind", other)
    yield result

  private def provenance(value: IrJson): Either[IrError, ProvenanceEventIr] =
    for
      kind <- discriminator(value, "provenance")
      result <- kind match
        case "source" =>
          objectFields(value, "provenance", Set("kind", "label")).flatMap { fields =>
            required(fields, "label", "provenance", string(_, "provenance.label")).map(ProvenanceEventIr.Source.apply)
          }
        case "adapted" =>
          objectFields(value, "provenance", Set("kind", "adapter")).flatMap { fields =>
            required(fields, "adapter", "provenance", string(_, "provenance.adapter")).map(ProvenanceEventIr.Adapted.apply)
          }
        case "derived" =>
          for
            fields <- objectFields(value, "provenance", Set("kind", "operation", "inputs"))
            operation <- required(fields, "operation", "provenance", string(_, "provenance.operation"))
            inputs <- required(fields, "inputs", "provenance", vector(_, "provenance.inputs", string(_, "provenance.inputs[]")))
          yield ProvenanceEventIr.Derived(operation, inputs)
        case "certified" =>
          for
            fields <- objectFields(value, "provenance", Set("kind", "property", "method"))
            property <- required(fields, "property", "provenance", string(_, "provenance.property"))
            method <- required(fields, "method", "provenance", string(_, "provenance.method"))
          yield ProvenanceEventIr.Certified(property, method)
        case "unsafe_assumption" =>
          for
            fields <- objectFields(value, "provenance", Set("kind", "property", "reason"))
            property <- required(fields, "property", "provenance", string(_, "provenance.property"))
            reason <- required(fields, "reason", "provenance", string(_, "provenance.reason"))
          yield ProvenanceEventIr.UnsafeAssumption(property, reason)
        case other => invalidTag("provenance.kind", other)
    yield result

  private def objectFields(
      value: IrJson,
      path: String,
      allowed: Set[String]
  ): Either[IrError, Map[String, IrJson]] =
    value match
      case Obj(fields) =>
        fields.find(field => !allowed.contains(field._1)) match
          case Some((name, _)) =>
            Left(IrError(RejectionCategory.UnknownField, s"$path.$name", "unknown fields are rejected by schema 0.1"))
          case None => Right(fields.toMap)
      case _ => Left(IrError(RejectionCategory.Malformed, path, "expected object"))

  private def required[A](
      fields: Map[String, IrJson],
      name: String,
      path: String,
      decode: IrJson => Either[IrError, A]
  ): Either[IrError, A] =
    fields.get(name).toRight(IrError(RejectionCategory.Malformed, s"$path.$name", "required field is missing")).flatMap(decode)

  private def optional[A](
      fields: Map[String, IrJson],
      name: String,
      path: String,
      decode: IrJson => Either[IrError, A]
  ): Either[IrError, Option[A]] =
    fields.get(name) match
      case None | Some(Null) => Right(None)
      case Some(value) => decode(value).map(Some(_))

  private def discriminator(value: IrJson, path: String): Either[IrError, String] =
    value match
      case Obj(fields) =>
        fields.toMap.get("kind").toRight(IrError(RejectionCategory.Malformed, s"$path.kind", "required field is missing"))
          .flatMap(string(_, s"$path.kind"))
      case _ => Left(IrError(RejectionCategory.Malformed, path, "expected object"))

  private def vector[A](
      value: IrJson,
      path: String,
      decode: IrJson => Either[IrError, A]
  ): Either[IrError, Vector[A]] =
    value match
      case Arr(values) =>
        values.zipWithIndex.foldLeft[Either[IrError, Vector[A]]](Right(Vector.empty)) { case (result, (item, index)) =>
          result.flatMap(values => decode(item).map(values :+ _).left.map(error => error.copy(path = s"$path[$index].${error.path}")))
        }
      case _ => Left(IrError(RejectionCategory.Malformed, path, "expected array"))

  private def string(value: IrJson, path: String): Either[IrError, String] =
    value match
      case Str(text) => Right(text)
      case _ => Left(IrError(RejectionCategory.Malformed, path, "expected string"))

  private def number(value: IrJson, path: String): Either[IrError, Double] =
    value match
      case Num(number) if number.isFinite => Right(number)
      case _ => Left(IrError(RejectionCategory.Malformed, path, "expected finite number"))

  private def integer(value: IrJson, path: String): Either[IrError, Int] =
    number(value, path).flatMap { number =>
      if number == Math.rint(number) && number >= Int.MinValue && number <= Int.MaxValue then Right(number.toInt)
      else Left(IrError(RejectionCategory.Malformed, path, "expected 32-bit integer"))
    }

  private def boolean(value: IrJson, path: String): Either[IrError, Boolean] =
    value match
      case Bool(flag) => Right(flag)
      case _ => Left(IrError(RejectionCategory.Malformed, path, "expected boolean"))

  private def enumValue[A <: Product](
      value: IrJson,
      path: String,
      values: Vector[A]
  ): Either[IrError, A] =
    string(value, path).flatMap { encoded =>
      values.find(item => tag(item) == encoded).toRight(
        IrError(RejectionCategory.Malformed, path, s"unknown enum tag '$encoded'")
      )
    }

  private def invalidTag[A](path: String, value: String): Either[IrError, A] =
    Left(IrError(RejectionCategory.Malformed, path, s"unknown tag '$value'"))

  private def tag(value: Product): String =
    val name = value.productPrefix
    val out = new StringBuilder
    name.zipWithIndex.foreach { case (character, index) =>
      if character.isUpper && index > 0 then out.append('_')
      out.append(character.toLower)
    }
    out.result()
