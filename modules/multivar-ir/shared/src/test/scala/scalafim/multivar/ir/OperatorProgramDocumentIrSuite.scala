package scalafim.multivar.ir

class OperatorProgramDocumentIrSuite extends munit.FunSuite:

  private val strict = ToleranceIr(1e-10, 1e-8)

  test("v0.2 operator programs round-trip with derivations, rewrite proofs, frames, and fit semantics"):
    val encoded = OperatorProgramDocumentIrCodec.encode(validDocument)
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(encoded))

    assertEquals(decoded, validDocument)
    assertEquals(OperatorProgramDocumentIrCodec.encode(decoded), encoded)
    assertEquals(decoded.programs.head.objective, ProgramObjectiveIr.GeneralizedRayleigh("weights", "between", "within"))
    assertEquals(decoded.rewrites.head.remainingEquivalence, decoded.programs.head.result.equivalence)
    assertEquals(decoded.fits.head.frames.head.scoreIdentities, Vector("scores"))

  test("v0.1 constitution fixtures retain their exact codec and rejection behavior"):
    val encoded = MultivarIrCodec.encode(ConformanceCorpus.validDocument)
    assertEquals(accepted(MultivarIrCodec.decode(encoded)), ConformanceCorpus.validDocument)
    assertEquals(MultivarIrCodec.decode(ConformanceCorpus.tamperedPayloadJson).left.toOption.get.category, RejectionCategory.PayloadTampered)
    assertEquals(MultivarIrCodec.decode(ConformanceCorpus.unknownFieldJson).left.toOption.get.category, RejectionCategory.UnknownField)

  test("operator validation rejects role and orientation conflation"):
    val invalidRole = validDocument.copy(
      operators = validDocument.operators.map:
        case value if value.valueIdentity == "weights" => value.copy(role = ProgramOperatorRoleIr.Score)
        case value => value
    )
    assertEquals(rejection(invalidRole).category, RejectionCategory.DomainCodomainMismatch)

    val invalidOrientation = validDocument.copy(
      operators = validDocument.operators.map:
        case value if value.valueIdentity == "between" =>
          value.copy(derivation = ProgramOperatorDerivationIr.SecondOrder("table", "table", "row-link"))
        case value => value
    )
    assertEquals(rejection(invalidOrientation).category, RejectionCategory.DomainCodomainMismatch)

  test("certification cannot be upgraded silently and ratio denominators require SPD evidence"):
    val silentUpgrade = validDocument.copy(
      operators = validDocument.operators.map:
        case value if value.valueIdentity == "within" =>
          value.copy(evidence = ProgramOperatorEvidenceIr(EvidenceStatusIr.Certified, Vector.empty))
        case value => value
    )
    assertEquals(rejection(silentUpgrade).category, RejectionCategory.UncertifiedPositivity)

    val unsupportedRatio = validDocument.copy(
      operators = validDocument.operators.map:
        case value if value.valueIdentity == "within" =>
          value.copy(evidence = ProgramOperatorEvidenceIr(EvidenceStatusIr.Unchecked, Vector.empty))
        case value => value
    )
    assertEquals(rejection(unsupportedRatio).category, RejectionCategory.UncertifiedPositivity)

  test("rewrites require a bound proof and explicit derived provenance"):
    val broken = validDocument.copy(
      rewrites = validDocument.rewrites.map(_.copy(provenance = Vector(ProvenanceEventIr.Source("rewrite"))))
    )
    assertEquals(rejection(broken).category, RejectionCategory.Malformed)

  test("v0.2 rejects unknown fields instead of dropping future semantics"):
    val encoded = OperatorProgramDocumentIrCodec.encode(validDocument)
    val mutated = encoded.replaceFirst("\"schema\":", "\"future\":true,\"schema\":")
    assertEquals(OperatorProgramDocumentIrCodec.decode(mutated).left.toOption.get.category, RejectionCategory.UnknownField)

  test("directed coefficient operators round-trip and require dual-to-dual observed ports"):
    val coefficient = op(
      "coefficient",
      CoordinateIr("features", VarianceIr.Dual),
      CoordinateIr("features", VarianceIr.Dual),
      ProgramOperatorRoleIr.Coefficient
    )
    val document = validDocument.copy(operators = validDocument.operators :+ coefficient)
    val encoded = OperatorProgramDocumentIrCodec.encode(document)
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(encoded))

    assertEquals(decoded.operators.last.role, ProgramOperatorRoleIr.Coefficient)
    val invalid = document.copy(
      operators = document.operators.updated(
        document.operators.length - 1,
        coefficient.copy(codomain = CoordinateIr("features", VarianceIr.Primal))
      )
    )
    assertEquals(rejection(invalid).category, RejectionCategory.DomainCodomainMismatch)

  private def validDocument: OperatorProgramDocumentIr =
    val spaces = Vector(
      SpaceIr("trials", SpaceRoleIr.Samples, 3),
      SpaceIr("features", SpaceRoleIr.Observed, 2),
      SpaceIr("components", SpaceRoleIr.Latent, 1)
    )
    val table = op(
      "table",
      CoordinateIr("features", VarianceIr.Dual),
      CoordinateIr("trials", VarianceIr.Primal),
      ProgramOperatorRoleIr.Table
    )
    val relationship = op(
      "row-link",
      CoordinateIr("trials", VarianceIr.Primal),
      CoordinateIr("trials", VarianceIr.Dual),
      ProgramOperatorRoleIr.RowLink
    )
    val between = op(
      "between",
      CoordinateIr("features", VarianceIr.Dual),
      CoordinateIr("features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Scatter,
      derivation = ProgramOperatorDerivationIr.SecondOrder("table", "row-link", "table")
    )
    val within = op(
      "within",
      CoordinateIr("features", VarianceIr.Dual),
      CoordinateIr("features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Covariance,
      evidence = certified("within", "spd"),
      derivation = ProgramOperatorDerivationIr.SecondOrder("table", "row-link", "table")
    )
    val cometric = op(
      "cometric",
      CoordinateIr("features", VarianceIr.Dual),
      CoordinateIr("features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Cometric,
      evidence = certified("cometric", "spd")
    )
    val weights = op(
      "weights",
      CoordinateIr("components", VarianceIr.Primal),
      CoordinateIr("features", VarianceIr.Dual),
      ProgramOperatorRoleIr.Frame
    )
    val scores = op(
      "scores",
      CoordinateIr("components", VarianceIr.Primal),
      CoordinateIr("trials", VarianceIr.Primal),
      ProgramOperatorRoleIr.Score,
      derivation = ProgramOperatorDerivationIr.Scores("weights", "table")
    )
    val axes = op(
      "axes",
      CoordinateIr("components", VarianceIr.Primal),
      CoordinateIr("features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Axis,
      derivation = ProgramOperatorDerivationIr.Axes("weights", "cometric")
    )
    val whitened = op(
      "whitened-between",
      CoordinateIr("features", VarianceIr.Dual),
      CoordinateIr("features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Covariance,
      derivation = ProgramOperatorDerivationIr.Lowered("generalized-to-standard-eigen", Vector("between", "within"))
    )
    val result = ProgramResultContractIr(
      ProgramEquivalenceIr.Subspace(strict, strict),
      ProgramRepresentativeIr.OrderedSpectrumThenSign,
      ProgramSolverGuaranteeIr.GlobalSpectralOptimum
    )
    val parameter = ProgramFrameParameterIr("weights", "features", "components", ProgramParameterizationIr.Identity)
    val original = OperatorProgramV2Ir(
      "lda-original",
      Vector(parameter),
      ProgramObjectiveIr.GeneralizedRayleigh("weights", "between", "within"),
      Vector(ProgramNormalizationV2Ir("weights", "within")),
      Vector.empty,
      Vector.empty,
      result,
      Vector(ProvenanceEventIr.Source("lda-rayleigh"))
    )
    val lowered = OperatorProgramV2Ir(
      "lda-lowered",
      Vector(parameter),
      ProgramObjectiveIr.MaximizeTrace("weights", "whitened-between"),
      Vector(ProgramNormalizationV2Ir("weights", "within")),
      Vector.empty,
      Vector.empty,
      result,
      Vector(ProvenanceEventIr.Derived("lower-program", Vector("lda-original")))
    )
    val rewrite = ProgramRewriteIr(
      "lda-whitening-proof",
      original.id,
      lowered.id,
      ProgramRewriteRuleIr.GeneralizedToStandardEigen,
      Vector("between", "within"),
      Vector("whitened-between"),
      certificate("lda-whitening-proof", "rewrite", Some(0.0)),
      result.equivalence,
      Vector(ProvenanceEventIr.Derived("generalized-to-standard-eigen", Vector("between", "within")))
    )
    val fit = ProgramFitIr(
      original.id,
      Vector(FunctionalFrameIr("weights", "weights", Some("cometric"), Vector("scores"), Some("axes"))),
      2.0,
      1,
      Vector(Vector(0)),
      Vector(certificate("lda-fit", "converged", Some(1e-12))),
      ProgramSolverGuaranteeIr.GlobalSpectralOptimum,
      result.equivalence,
      Vector(ProvenanceEventIr.Derived("fit", Vector(original.id)))
    )
    OperatorProgramDocumentIr(
      OperatorProgramDocumentIr.schemaV02,
      spaces,
      Vector(table, relationship, between, within, cometric, weights, scores, axes, whitened),
      Vector(original, lowered),
      Vector(rewrite),
      Vector(fit)
    )

  private def op(
      identity: String,
      domain: CoordinateIr,
      codomain: CoordinateIr,
      role: ProgramOperatorRoleIr,
      evidence: ProgramOperatorEvidenceIr = ProgramOperatorEvidenceIr(EvidenceStatusIr.Unchecked, Vector.empty),
      derivation: ProgramOperatorDerivationIr = ProgramOperatorDerivationIr.Source
  ): ProgramOpIr =
    ProgramOpIr(
      identity,
      domain,
      codomain,
      role,
      evidence,
      ProgramRepresentationIr.Dense,
      ProgramGaugeIr.Ungauged,
      derivation,
      identity,
      Vector(ProvenanceEventIr.Source(identity))
    )

  private def certified(identity: String, property: String): ProgramOperatorEvidenceIr =
    ProgramOperatorEvidenceIr(EvidenceStatusIr.Certified, Vector(certificate(identity, property, Some(0.0))))

  private def certificate(identity: String, property: String, residual: Option[Double]): CertificateIr =
    CertificateIr(property, identity, strict, "frobenius", "fixture", "float64", "gale", None, residual)

  private def accepted[A](value: Either[IrError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def rejection(value: OperatorProgramDocumentIr): IrError =
    OperatorProgramIrValidator.validate(value).left.toOption.getOrElse(fail("document was unexpectedly accepted"))
