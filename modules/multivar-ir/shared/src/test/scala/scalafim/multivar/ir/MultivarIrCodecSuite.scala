package scalafim.multivar.ir

import scalafim.linalg.DoubleMatrix
import scalafim.multivar.*

class MultivarIrCodecSuite extends munit.FunSuite:
  private def accepted[A](value: Either[IrError, A]): A =
    value.fold(error => fail(error.message), result => result)

  private def acceptedSemantic[A](value: Either[SemanticError, A]): A =
    value.fold(error => fail(error.message), result => result)

  private def acceptedAlignment[A](value: Either[AlignmentError, A]): A =
    value.fold(error => fail(error.message), result => result)

  private def ref(id: String, role: SpaceRole, dimension: Int): SpaceRef =
    SpaceRef(MvSpace(SpaceId.unsafe(id), role, Dimension.unsafe(dimension)))

  private def valueIdentity(id: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(id))

  test("portable SHA-256 payload identity matches an independent known vector") {
    val payload = accepted(PayloadIrFactory.inlineDense(2, 2, Vector(1.0, 0.0, 0.0, 1.0)))
    assertEquals(payload.sha256, "a22a44164bd5ee3523c2e80df0080973f5e2dadfb4b653e511da7c64040e69c4")
  }

  test("encode-decode preserves the complete language-neutral semantic identity") {
    val encoded = MultivarIrCodec.encode(ConformanceCorpus.validDocument)
    val decoded = accepted(MultivarIrCodec.decode(encoded))
    assertEquals(decoded, ConformanceCorpus.validDocument)
    assertEquals(MultivarIrCodec.encode(decoded), encoded)
    assertEquals(decoded.schema.version, SchemaVersion.v0_1)
    assertEquals(decoded.operators.head.domain.variance, VarianceIr.Dual)
    assertEquals(decoded.forms.head.positivity, PositivityIr.Spd)
    assertEquals(decoded.forms(1).scaleSemantics, ScaleSemanticsIr.ShapeMetric("shared-shape-fit-gauge-1"))
    assertEquals(decoded.relationships.head.support.uncertainMass, 1.0)
    assertEquals(decoded.objectives.head.formula, ObjectiveFormulaIr.PairwiseAssociation)
  }

  test("external payload hashes and unsafe assumptions survive round-trip") {
    val decoded = accepted(MultivarIrCodec.decode(ConformanceCorpus.validJson))
    val external = decoded.operators.find(_.id == "external-kernel").getOrElse(fail("missing external operator"))
    assert(external.payload.isInstanceOf[PayloadIr.External])
    assertEquals(external.payload.sha256, "b" * 64)
    assert(
      external.provenance.exists(_.isInstanceOf[ProvenanceEventIr.UnsafeAssumption]),
      external.provenance.toString
    )
  }

  test("inline payload tampering is rejected before semantic construction") {
    val rejection = MultivarIrCodec.decode(ConformanceCorpus.tamperedPayloadJson).left.toOption.get
    assertEquals(rejection.category, RejectionCategory.PayloadTampered)
  }

  test("all bindings receive the same domain and positivity rejection categories") {
    val domain = IrValidator.validate(ConformanceCorpus.invalidDomainDocument).left.toOption.get
    val positivity = IrValidator.validate(ConformanceCorpus.uncertifiedPositivityDocument).left.toOption.get
    assertEquals(domain.category, RejectionCategory.DomainCodomainMismatch)
    assertEquals(positivity.category, RejectionCategory.UncertifiedPositivity)
  }

  test("unsupported singularity and incompatible alignment kinds have stable categories") {
    val singularity = IrValidator
      .validate(ConformanceCorpus.unsupportedSingularityDocument, IrCapabilities(Set("reject")))
      .left
      .toOption
      .get
    val alignment = IrValidator.validate(ConformanceCorpus.incompatibleAlignmentDocument).left.toOption.get
    assertEquals(singularity.category, RejectionCategory.UnsupportedSingularity)
    assertEquals(alignment.category, RejectionCategory.IncompatibleAlignmentKind)
  }

  test("schema 0.1 rejects unknown fields and incompatible versions explicitly") {
    val unknown = MultivarIrCodec.decode(ConformanceCorpus.unknownFieldJson).left.toOption.get
    val version = IrValidator.validate(ConformanceCorpus.invalidSchemaDocument).left.toOption.get
    assertEquals(unknown.category, RejectionCategory.UnknownField)
    assertEquals(version.category, RejectionCategory.SchemaVersionMismatch)
  }

  test("semantic lowering preserves nominal orientation, relation kind, marginals, and provenance") {
    val left = ref("ir.left", SpaceRole.Samples, 2)
    val right = ref("ir.right", SpaceRole.Samples, 2)
    val coupling = acceptedAlignment(
      NonnegativeCoupling.fromMatrix(
        left.evidence,
        right.evidence,
        DoubleMatrix.fromRows(Vector(Vector(0.5, 0.0), Vector(0.0, 0.5))),
        RelationshipNormalization.UnitMass,
        valueIdentity("ir.coupling"),
        SemanticProvenance(
          Vector(SemanticProvenanceEvent.UnsafeAssumption("fixture-link", "external audit"))
        )
      )
    )
    val operator = accepted(
      SemanticIr.operator("ir-coupling-op", OperatorRoleIr.RowLink, coupling.rowLink.operator)
    )
    val relationship = accepted(
      SemanticIr.relationship(
        "ir-coupling",
        coupling.rowLink.descriptor,
        operator.id,
        coupling.rowLink.provenance
      )
    )

    assertEquals(operator.domain, CoordinateIr(right.descriptor.id.value, VarianceIr.Primal))
    assertEquals(operator.codomain, CoordinateIr(left.descriptor.id.value, VarianceIr.Dual))
    assertEquals(relationship.kind, RelationshipKindIr.NonnegativeCoupling)
    assert(relationship.marginals.nonEmpty)
    assert(relationship.provenance.exists(_.isInstanceOf[ProvenanceEventIr.UnsafeAssumption]))
  }

  test("inline sparse fixtures round-trip and objective tags reject method substitution") {
    val sparse = accepted(
      PayloadIrFactory.inlineSparse(
        3,
        2,
        Vector(0, 2),
        Vector(1, 0),
        Vector(0.5, -1.0)
      )
    )
    val operator = OperatorIr(
      "sparse-map",
      OperatorRoleIr.LinearMap,
      CoordinateIr("features", VarianceIr.Primal),
      CoordinateIr("observations", VarianceIr.Primal),
      "sparse",
      sparse,
      "sparse-map-v1",
      Vector.empty
    )
    val document = MultivarIrDocument.empty.copy(
      spaces = Vector(
        SpaceIr("features", SpaceRoleIr.Observed, 2),
        SpaceIr("observations", SpaceRoleIr.Samples, 3)
      ),
      operators = Vector(operator)
    )
    assertEquals(accepted(MultivarIrCodec.decode(MultivarIrCodec.encode(document))), document)

    val wrongSolver = ConformanceCorpus.validDocument.copy(
      objectives = ConformanceCorpus.validDocument.objectives.map(
        _.copy(solverFormulation = SolverFormulationIr.QuadraticPenalty)
      )
    )
    assertEquals(IrValidator.validate(wrongSolver).left.toOption.get.category, RejectionCategory.Malformed)
  }
