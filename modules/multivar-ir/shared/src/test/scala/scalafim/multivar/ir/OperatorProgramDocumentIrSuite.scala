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

  test("quadratic pullback rewrites preserve exact proof and evidence-bearing output identities"):
    val base = validDocument
    val rewrite = base.rewrites.head.copy(rule = ProgramRewriteRuleIr.QuadraticPullback)
    val document = base.copy(rewrites = Vector(rewrite))
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.rewrites.head.rule, ProgramRewriteRuleIr.QuadraticPullback)
    assertEquals(decoded.rewrites.head.proof.property, "rewrite")
    assert(decoded.rewrites.head.outputOperators.nonEmpty)

  test("v0.2 rejects unknown fields instead of dropping future semantics"):
    val encoded = OperatorProgramDocumentIrCodec.encode(validDocument)
    val mutated = encoded.replaceFirst("\"schema\":", "\"future\":true,\"schema\":")
    assertEquals(OperatorProgramDocumentIrCodec.decode(mutated).left.toOption.get.category, RejectionCategory.UnknownField)

  test("multi-parameter targets and public penalty-versus-constraint intent round-trip"):
    val base = validDocument
    val second = base.programs.head.parameters.head.copy(id = "weights-peer")
    val target = ProgramTargetIr(
      "weights",
      ProgramTargetCapabilityIr.Smooth,
      "parameter:frame/parameter:frame/product/aligned-score-difference",
      None,
      additionalParameterIds = Vector("weights-peer"),
      additionalOperatorIdentities = Vector("table"),
      equivariance = ProgramFrameSymmetryIr.Orthogonal
    )
    val penalty = ProgramPenaltyV2Ir(
      target,
      ProgramFunctionalIr.Huber(1.0),
      0.25,
      ProgramFrameSymmetryIr.SignedPermutation
    )
    val constraint = ProgramConstraintV2Ir(
      target,
      ProgramFeasibleSetIr.NormBall(2.0),
      ProgramFrameSymmetryIr.Orthogonal
    )
    val program = base.programs.head.copy(
      parameters = base.programs.head.parameters :+ second,
      normalizations = base.programs.head.normalizations :+ ProgramNormalizationV2Ir("weights-peer", "within"),
      penalties = Vector(penalty),
      constraints = Vector(constraint)
    )
    val document = base.copy(programs = Vector(program), rewrites = Vector.empty, fits = Vector.empty)
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.programs.head.penalties.head.target.parameterIds, Vector("weights", "weights-peer"))
    assertEquals(decoded.programs.head.penalties.head, penalty)
    assertEquals(decoded.programs.head.constraints.head, constraint)

  test("group, sparse-group, and monotone coordinate semantics round-trip"):
    val base = validDocument
    val target = ProgramTargetIr(
      "weights",
      ProgramTargetCapabilityIr.Linear,
      "feature-chart:genes",
      Some("table")
    )
    val groups = ProgramPenaltyV2Ir(
      target,
      ProgramFunctionalIr.GroupL2("gene-groups"),
      0.5,
      ProgramFrameSymmetryIr.Orthogonal
    )
    val sparseGroup = ProgramPenaltyV2Ir(
      target,
      ProgramFunctionalIr.SparseGroup(0.25, "gene-groups"),
      0.75,
      ProgramFrameSymmetryIr.SignedPermutation
    )
    val monotone = ProgramConstraintV2Ir(
      target,
      ProgramFeasibleSetIr.Monotone("genomic-order"),
      ProgramFrameSymmetryIr.Permutation
    )
    val program = base.programs.head.copy(penalties = Vector(groups, sparseGroup), constraints = Vector(monotone))
    val document = base.copy(programs = Vector(program), rewrites = Vector.empty, fits = Vector.empty)
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.programs.head.penalties, Vector(groups, sparseGroup))
    assertEquals(decoded.programs.head.constraints, Vector(monotone))

  test("shared-basis parameterizations and redundant gauge semantics round-trip"):
    val base = validDocument
    val parameter = base.programs.head.parameters.head.copy(
      parameterization = ProgramParameterizationIr.SharedBasis("shared-basis", injective = false)
    )
    val result = base.programs.head.result.copy(
      redundantCoordinates = true,
      parameterGauges = Vector("GeneralLinear")
    )
    val program = base.programs.head.copy(parameters = Vector(parameter), result = result)
    val document = base.copy(programs = Vector(program), rewrites = Vector.empty, fits = Vector.empty)
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.programs.head.parameters.head.parameterization, parameter.parameterization)
    assert(decoded.programs.head.result.redundantCoordinates)
    assertEquals(decoded.programs.head.result.parameterGauges, Vector("GeneralLinear"))

  test("operator policies round-trip separately from parameter ridge terms"):
    val base = validDocument
    val ridge = ProgramPenaltyV2Ir(
      ProgramTargetIr("weights", ProgramTargetCapabilityIr.Linear, "identity", Some("weights")),
      ProgramFunctionalIr.SquaredNorm("cometric"),
      0.2,
      ProgramFrameSymmetryIr.Orthogonal
    )
    val program = base.programs.head.copy(penalties = Vector(ridge))
    val joint = ProgramOperatorPolicyIr(
      "joint-selection",
      ProgramOperatorPolicyKindIr.JointBlockShrinkage,
      Vector("between", "within", "cometric"),
      Vector("whitened-between"),
      ProgramPolicySelectionIr.FoldSelected("select-joint-alpha", Vector(0.0, 0.25, 0.5)),
      ProgramScaleMatchingIr.MatchTrace,
      ProgramPolicyScopeIr.JointSystem,
      Vector(
        ProgramPreservationClaimIr.PsdPreserved,
        ProgramPreservationClaimIr.BlockAdjointsPreserved,
        ProgramPreservationClaimIr.SharedGaugePreserved
      ),
      Vector(ProvenanceEventIr.Derived("joint-shrinkage", Vector("between", "within", "cometric")))
    )
    val unsafe = ProgramOperatorPolicyIr(
      "blockwise-alternative",
      ProgramOperatorPolicyKindIr.BlockwiseShrinkage,
      Vector("between", "within"),
      Vector("between", "within"),
      ProgramPolicySelectionIr.Fixed(0.1),
      ProgramScaleMatchingIr.None,
      ProgramPolicyScopeIr.BlockwiseUnsafe,
      Vector(
        ProgramPreservationClaimIr.BlockAdjointsPreserved,
        ProgramPreservationClaimIr.EvidenceDowngraded("joint PSD and gauge are not established")
      ),
      Vector(ProvenanceEventIr.Derived("blockwise-shrinkage", Vector("between", "within")))
    )
    val document = base.copy(
      programs = Vector(program, base.programs(1)),
      operatorPolicies = Vector(joint, unsafe)
    )
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.operatorPolicies, Vector(joint, unsafe))
    assertEquals(decoded.programs.head.penalties, Vector(ridge))
    assertEquals(decoded.operatorPolicies.head.selection, joint.selection)
    assertNotEquals(decoded.operatorPolicies.head.kind.toString, decoded.programs.head.penalties.head.functional.toString)

  test("unsafe block policies require a visible evidence downgrade"):
    val invalid = ProgramOperatorPolicyIr(
      "hidden-downgrade",
      ProgramOperatorPolicyKindIr.BlockwiseShrinkage,
      Vector("between", "within"),
      Vector("between", "within"),
      ProgramPolicySelectionIr.Fixed(0.25),
      ProgramScaleMatchingIr.None,
      ProgramPolicyScopeIr.BlockwiseUnsafe,
      Vector(ProgramPreservationClaimIr.BlockAdjointsPreserved),
      Vector(ProvenanceEventIr.Derived("blockwise-shrinkage", Vector("between", "within")))
    )

    assertEquals(rejection(validDocument.copy(operatorPolicies = Vector(invalid))).category, RejectionCategory.Malformed)

  test("composed nonsmooth lowerings round-trip explicit auxiliary equations and solver capabilities"):
    val base = validDocument
    val target = ProgramTargetIr(
      "weights",
      ProgramTargetCapabilityIr.Linear,
      "graph-incidence",
      Some("between")
    )
    val penalty = ProgramPenaltyV2Ir(
      target,
      ProgramFunctionalIr.TotalVariation,
      0.25,
      ProgramFrameSymmetryIr.SignedPermutation
    )
    val program = base.programs.head.copy(penalties = Vector(penalty))
    val lowering = ProgramCompositeLoweringIr(
      "tv-primal-dual",
      program.id,
      ProgramLoweredTermIr.Penalty(0),
      "between",
      ProgramSplitMethodIr.PrimalDual,
      Vector(ProgramSplitMethodIr.PrimalDual, ProgramSplitMethodIr.Admm),
      ProgramAuxiliaryConstraintIr(
        "graph-differences",
        target,
        ProgramAuxiliaryEquationIr.TargetCopy
      ),
      Vector(ProvenanceEventIr.Derived("composite-primaldual-split", Vector("between")))
    )
    val document = base.copy(
      programs = Vector(program, base.programs(1)),
      compositeLowerings = Vector(lowering)
    )
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.compositeLowerings, Vector(lowering))
    assertEquals(decoded.compositeLowerings.head.auxiliary.target, decoded.programs.head.penalties.head.target)

  test("composed lowering rejects a selected method absent from its capabilities"):
    val base = validDocument
    val target = ProgramTargetIr("weights", ProgramTargetCapabilityIr.Linear, "general-map", Some("between"))
    val penalty = ProgramPenaltyV2Ir(target, ProgramFunctionalIr.L1, 0.5, ProgramFrameSymmetryIr.SignedPermutation)
    val program = base.programs.head.copy(penalties = Vector(penalty))
    val invalid = ProgramCompositeLoweringIr(
      "missing-admm",
      program.id,
      ProgramLoweredTermIr.Penalty(0),
      "between",
      ProgramSplitMethodIr.Admm,
      Vector(ProgramSplitMethodIr.PrimalDual),
      ProgramAuxiliaryConstraintIr("z", target, ProgramAuxiliaryEquationIr.TargetCopy),
      Vector(ProvenanceEventIr.Derived("composite-admm-split", Vector("between")))
    )
    val document = base.copy(
      programs = Vector(program, base.programs(1)),
      compositeLowerings = Vector(invalid)
    )

    assertEquals(rejection(document).category, RejectionCategory.Malformed)

  test("local, coordinatewise, heuristic, and unresolved solver guarantees remain distinct"):
    val guarantees = Vector(
      ProgramSolverGuaranteeIr.CoordinatewiseStationary,
      ProgramSolverGuaranteeIr.LocallyOptimal,
      ProgramSolverGuaranteeIr.HeuristicFeasible,
      ProgramSolverGuaranteeIr.Unresolved
    )
    guarantees.foreach: guarantee =>
      val base = validDocument
      val programs = base.programs.map(program => program.copy(result = program.result.copy(guarantee = guarantee)))
      val fits = base.fits.map(_.copy(solverGuarantee = guarantee))
      val document = base.copy(programs = programs, fits = fits)
      val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

      assertEquals(decoded.programs.head.result.guarantee, guarantee)
      assertEquals(decoded.fits.head.solverGuarantee, guarantee)

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

  test("typed constraint maps round-trip and require primal-to-primal ports"):
    val constraint = op(
      "constraint",
      CoordinateIr("trials", VarianceIr.Primal),
      CoordinateIr("trials", VarianceIr.Primal),
      ProgramOperatorRoleIr.ConstraintMap
    )
    val document = validDocument.copy(operators = validDocument.operators :+ constraint)
    val decoded = accepted(OperatorProgramDocumentIrCodec.decode(OperatorProgramDocumentIrCodec.encode(document)))

    assertEquals(decoded.operators.last.role, ProgramOperatorRoleIr.ConstraintMap)
    val invalid = document.copy(
      operators = document.operators.updated(
        document.operators.length - 1,
        constraint.copy(domain = CoordinateIr("trials", VarianceIr.Dual))
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
