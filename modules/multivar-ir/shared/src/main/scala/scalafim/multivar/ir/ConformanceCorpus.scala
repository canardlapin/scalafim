package scalafim.multivar.ir

object ConformanceCorpus:
  private def accepted[A](value: Either[IrError, A]): A =
    value.fold(error => throw new IllegalStateException(error.message), identity)

  private val identity2 = accepted(PayloadIrFactory.inlineDense(2, 2, Vector(1.0, 0.0, 0.0, 1.0)))
  private val quarter2 = accepted(PayloadIrFactory.inlineDense(2, 2, Vector(0.25, 0.25, 0.25, 0.25)))
  private val weights2 = accepted(PayloadIrFactory.inlineDense(2, 1, Vector(0.5, 0.5)))
  private val external2 = accepted(
    PayloadIrFactory.external(
      "https://example.org/payloads/kernel.bin",
      "application/vnd.scalafim.f64-row-major",
      2,
      2,
      "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"
    )
  )

  private val context = CertificateIr(
    "spd",
    "identity-2",
    ToleranceIr(1e-10, 1e-8),
    "frobenius",
    "portable-spectral-check",
    "float64",
    "scalafim-linalg",
    None,
    Some(0.0)
  )

  val validDocument: MultivarIrDocument =
    MultivarIrDocument(
      SchemaHeader("scalafim-multivar-ir", SchemaVersion.v0_1, UnknownFieldPolicy.Reject),
      spaces = Vector(
        SpaceIr("observations", SpaceRoleIr.Samples, 2),
        SpaceIr("features", SpaceRoleIr.Observed, 2)
      ),
      operators = Vector(
        OperatorIr(
          "table-x",
          OperatorRoleIr.Table,
          CoordinateIr("features", VarianceIr.Dual),
          CoordinateIr("observations", VarianceIr.Primal),
          "dense",
          identity2,
          "table-x-v1",
          Vector(ProvenanceEventIr.Source("fixture"))
        ),
        OperatorIr(
          "row-metric-op",
          OperatorRoleIr.LinearMap,
          CoordinateIr("observations", VarianceIr.Primal),
          CoordinateIr("observations", VarianceIr.Dual),
          "diagonal",
          identity2,
          "identity-2",
          Vector(ProvenanceEventIr.Certified("spd", "portable-spectral-check"))
        ),
        OperatorIr(
          "column-metric-op",
          OperatorRoleIr.LinearMap,
          CoordinateIr("features", VarianceIr.Primal),
          CoordinateIr("features", VarianceIr.Dual),
          "diagonal",
          identity2,
          "identity-2",
          Vector(ProvenanceEventIr.Certified("spd", "portable-spectral-check"))
        ),
        OperatorIr(
          "coupling-op",
          OperatorRoleIr.RowLink,
          CoordinateIr("observations", VarianceIr.Primal),
          CoordinateIr("observations", VarianceIr.Dual),
          "dense",
          quarter2,
          "coupling-v1",
          Vector(ProvenanceEventIr.Source("externally-supplied-coupling"))
        ),
        OperatorIr(
          "external-kernel",
          OperatorRoleIr.LinearMap,
          CoordinateIr("features", VarianceIr.Dual),
          CoordinateIr("features", VarianceIr.Primal),
          "external",
          external2,
          "external-kernel-v1",
          Vector(
            ProvenanceEventIr.UnsafeAssumption(
              "external-payload-availability",
              "fixture resolver is supplied by the consuming binding"
            )
          )
        )
      ),
      forms = Vector(
        FormIr(
          "row-metric",
          FormRoleIr.Metric,
          "observations",
          "row-metric-op",
          FormStructureIr.Symmetric,
          PositivityIr.Spd,
          EvidenceStatusIr.Certified,
          ScaleSemanticsIr.AbsoluteMetric,
          Vector(context)
        ),
        FormIr(
          "column-metric",
          FormRoleIr.Metric,
          "features",
          "column-metric-op",
          FormStructureIr.Symmetric,
          PositivityIr.Spd,
          EvidenceStatusIr.Certified,
          ScaleSemanticsIr.ShapeMetric("shared-shape-fit-gauge-1"),
          Vector(context)
        )
      ),
      measures = Vector(
        MeasureIr(
          "row-measure",
          "observations",
          weights2,
          MeasureNormalizationIr.UnitMass(2.0),
          "row-measure-v1"
        )
      ),
      diagrams = Vector(
        DiagramIr(
          "diagram-1",
          "table-x",
          "row-metric",
          "column-metric",
          Some("row-measure"),
          CenteringIr.ByMeasure("row-measure"),
          SingularityPolicyIr.Reject,
          SingularityPolicyIr.Reject,
          MissingnessIr.Complete,
          "absolute_metric",
          Vector(ProvenanceEventIr.Source("fixture-diagram"))
        )
      ),
      relationships = Vector(
        RelationshipIr(
          "coupling-1",
          RelationshipKindIr.ProbabilisticCoupling,
          "coupling-op",
          RelationshipSupportIr(
            1.0,
            0.0,
            0.0,
            1.0,
            0.0,
            0,
            CardinalityIr.ManyToMany,
            Vector.empty,
            everySourceRepresented = true,
            everyTargetRepresented = true
          ),
          RelationshipNormalizationIr.UnitMass,
          Some(MarginalsIr(weights2, weights2, 1.0)),
          AlignmentOriginIr.ExternallySupplied,
          Vector(ProvenanceEventIr.Source("fixture-coupling"))
        )
      ),
      objectives = Vector(
        ObjectiveIr(
          "association-1",
          ObjectiveFormulaIr.PairwiseAssociation,
          "maximize t* L t",
          Vector("v* R v = I"),
          ObjectiveNormalizationIr.DirectSumMetricOrthonormal,
          SolverFormulationIr.SymmetricFeatureEigen,
          SolvedToReportedIr.Identical
        )
      )
    )

  val validJson: String = MultivarIrCodec.encode(validDocument)

  val tamperedPayloadJson: String =
    validJson.replace(
      "\"values\":[1,0,0,1],\"sha256\":\"a22a44164bd5ee3523c2e80df0080973f5e2dadfb4b653e511da7c64040e69c4\"",
      "\"values\":[2,0,0,1],\"sha256\":\"a22a44164bd5ee3523c2e80df0080973f5e2dadfb4b653e511da7c64040e69c4\""
    )

  val unknownFieldJson: String =
    validJson.replace("{\"schema\":", "{\"future_field\":true,\"schema\":")

  val invalidDomainDocument: MultivarIrDocument =
    validDocument.copy(
      operators = validDocument.operators.updated(
        0,
        validDocument.operators(0).copy(codomain = CoordinateIr("features", VarianceIr.Primal))
      )
    )

  val uncertifiedPositivityDocument: MultivarIrDocument =
    validDocument.copy(forms = validDocument.forms.updated(0, validDocument.forms(0).copy(certificates = Vector.empty)))

  val unsupportedSingularityDocument: MultivarIrDocument =
    validDocument.copy(
      diagrams = validDocument.diagrams.updated(
        0,
        validDocument.diagrams(0).copy(rowSingularity = SingularityPolicyIr.Quotient(1e-10))
      )
    )

  val incompatibleAlignmentDocument: MultivarIrDocument =
    validDocument.copy(
      relationships = validDocument.relationships.updated(
        0,
        validDocument.relationships(0).copy(kind = RelationshipKindIr.ExactBijection)
      )
    )

  val invalidSchemaDocument: MultivarIrDocument =
    validDocument.copy(schema = validDocument.schema.copy(version = SchemaVersion(1, 0)))
