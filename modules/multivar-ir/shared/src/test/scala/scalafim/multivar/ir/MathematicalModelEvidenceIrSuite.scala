package scalafim.multivar.ir

class MathematicalModelEvidenceIrSuite extends munit.FunSuite:
  test("a theorem-complete GLRM evidence envelope round-trips without losing identities"):
    val document = MathematicalModelEvidenceDocumentIr(
      MathematicalModelEvidenceDocumentIr.schemaV10,
      Vector(stationaryGlrm)
    )
    val encoded = MathematicalModelEvidenceIrCodec.encode(document)
    assertEquals(MathematicalModelEvidenceIrCodec.decode(encoded), Right(document))
    assert(encoded.contains("\"observed_count\":6"))
    assert(encoded.contains("\"stationary\""))

  test("all six model families and estimands survive the shared codec"):
    val pairs = Vector(
      ModelFamilyEvidenceIr.AnchorRegularizedFrame -> ModelEstimandEvidenceIr.AnchorCoefficientRefinement,
      ModelFamilyEvidenceIr.ExactSpectralFrame -> ModelEstimandEvidenceIr.GeneralizedSpectralSubspace,
      ModelFamilyEvidenceIr.JointSparseFunctionalFactorization -> ModelEstimandEvidenceIr.JointStructuredFactors,
      ModelFamilyEvidenceIr.GeneralizedLowRankModel -> ModelEstimandEvidenceIr.GeneralizedLatentRepresentation,
      ModelFamilyEvidenceIr.ConvexifiedLowRankMatrix -> ModelEstimandEvidenceIr.ConvexLowRankMatrix,
      ModelFamilyEvidenceIr.StructuredMultiblockFactorization -> ModelEstimandEvidenceIr.SharedBlockLatentRepresentation
    )
    val document = MathematicalModelEvidenceDocumentIr(
      MathematicalModelEvidenceDocumentIr.schemaV10,
      pairs.zipWithIndex.map:
        case (pair, index) => unresolved(pair._1, pair._2, index)
    )
    val decoded = MathematicalModelEvidenceIrCodec.decode(MathematicalModelEvidenceIrCodec.encode(document))
    assertEquals(decoded, Right(document))

  test("stationarity requires every assumption of one supporting theorem"):
    val incomplete = stationaryGlrm.copy(assumptions = stationaryGlrm.assumptions.dropRight(1))
    assertInvalid(incomplete, "requires every assumption")

  test("estimand and family cannot be cross-wired even when all dimensions agree"):
    val invalid = stationaryGlrm.copy(estimand = ModelEstimandEvidenceIr.GeneralizedSpectralSubspace)
    assertInvalid(invalid, "belongs to")

  test("a nonconvex GLRM cannot serialize an exact-global achieved guarantee"):
    val invalid = stationaryGlrm.copy(
      achievedGuarantee = AchievedGuaranteeEvidenceIr.ExactGlobal("certificate.global"),
      certificateIdentities = Vector("certificate.global")
    )
    assertInvalid(invalid, "not admitted")

  test("an achieved guarantee must reference a retained certificate identity"):
    val invalid = stationaryGlrm.copy(certificateIdentities = Vector("certificate.other"))
    assertInvalid(invalid, "absent from certificate identities")

  test("GLRM and multiblock records require explicit losses and mask semantics"):
    assertInvalid(stationaryGlrm.copy(losses = Vector.empty), "requires explicit entry losses")
    val censored = stationaryGlrm.copy(
      mask = ObservationMaskEvidenceIr.Censored(
        "mask.censored",
        4,
        "likelihood.interval",
        MissingnessTargetEvidenceIr.MnarSensitivity("selection-model.v1")
      )
    )
    assertEquals(validate(censored), Right(()))

  test("reproducibility receipts reject unexplained, unstable, or lossy metadata"):
    assertInvalid(
      stationaryGlrm.copy(reproducibility = stationaryGlrm.reproducibility.copy(dependencies = Vector.empty)),
      "must be non-empty"
    )
    assertInvalid(
      stationaryGlrm.copy(reproducibility = stationaryGlrm.reproducibility.copy(conditionEstimate = 0.9)),
      "at least one"
    )
    assertInvalid(
      stationaryGlrm.copy(reproducibility = stationaryGlrm.reproducibility.copy(seed = 9007199254740992L)),
      "exactly representable"
    )

  test("unknown fields and future schema versions are rejected explicitly"):
    val document = MathematicalModelEvidenceDocumentIr(
      MathematicalModelEvidenceDocumentIr.schemaV10,
      Vector(stationaryGlrm)
    )
    val encoded = MathematicalModelEvidenceIrCodec.encode(document)
    val unknown = encoded.replaceFirst("\"schema\":", "\"surprise\":true,\"schema\":")
    assert(MathematicalModelEvidenceIrCodec.decode(unknown) match
      case Left(IrError(RejectionCategory.UnknownField, "$.surprise", _)) => true
      case _ => false
    )
    val future = encoded.replace(MathematicalModelEvidenceDocumentIr.schemaV10, "scalafim-mathematical-model-evidence-ir/1.1")
    assert(MathematicalModelEvidenceIrCodec.decode(future) match
      case Left(IrError(RejectionCategory.SchemaVersionMismatch, "$.schema", _)) => true
      case _ => false
    )

  test("invalid quantitative guarantees and solver receipts fail before publication"):
    assertInvalid(
      stationaryGlrm.copy(achievedGuarantee = AchievedGuaranteeEvidenceIr.Stationary(Double.NaN, "certificate.stationary")),
      "finite and non-negative"
    )
    assertInvalid(
      stationaryGlrm.copy(solver = stationaryGlrm.solver.copy(iterationCount = -1)),
      "must be non-negative"
    )

  private val stationaryGlrm: MathematicalModelEvidenceIr =
    val theorem = "palm-generalized-low-rank-model"
    MathematicalModelEvidenceIr(
      id = "evidence.glrm.stationary",
      contractId = "multivar.generalized-low-rank-model.v1",
      family = ModelFamilyEvidenceIr.GeneralizedLowRankModel,
      estimand = ModelEstimandEvidenceIr.GeneralizedLatentRepresentation,
      operatorProgramSchema = OperatorProgramDocumentIr.schemaV02,
      programId = "program.glrm.mixed",
      dataIdentity = "data.glrm.fixture",
      mask = ObservationMaskEvidenceIr.Explicit(
        "mask.glrm.fixture",
        6,
        MissingnessTargetEvidenceIr.FixedMask
      ),
      losses = Vector(
        LossBindingEvidenceIr("domain.real", EntryLossEvidenceIr.HalfSquared),
        LossBindingEvidenceIr("domain.binary", EntryLossEvidenceIr.BernoulliLogistic),
        LossBindingEvidenceIr("domain.ordinal", EntryLossEvidenceIr.CumulativeOrdinal(3, "order.ordinal"))
      ),
      geometries = Vector(
        GeometryBindingEvidenceIr(
          GeometryRoleEvidenceIr.RowNormalization,
          "operator.row-identity",
          "certificate.row-spd"
        )
      ),
      penalties = Vector(
        PenaltyBindingEvidenceIr(
          PenaltyOwnerEvidenceIr.RowFactor,
          PenaltyFunctionalEvidenceIr.L1,
          0.2,
          None
        ),
        PenaltyBindingEvidenceIr(
          PenaltyOwnerEvidenceIr.ColumnFactor,
          PenaltyFunctionalEvidenceIr.SquaredSmoothness,
          0.5,
          Some("operator.graph")
        )
      ),
      assumptions = Vector(
        assumption(theorem, "convex-entry-loss-by-block"),
        assumption(theorem, "block-lipschitz-gradient"),
        assumption(theorem, "proper-factor-penalties"),
        assumption(theorem, "bounded-iterates")
      ),
      solver = SolverReceiptEvidenceIr(
        SolverFamilyEvidenceIr.Palm,
        "scalafim-multivar/0.1",
        "policy.palm.exact",
        "trace.palm.fixture",
        ToleranceIr(1e-10, 1e-8),
        17
      ),
      achievedGuarantee = AchievedGuaranteeEvidenceIr.Stationary(4e-9, "certificate.stationary"),
      certificateIdentities = Vector("certificate.stationary", "certificate.row-spd"),
      reproducibility = ReproducibilityReceiptIr(
        "generator.glrm.fixture",
        4409L,
        Vector(
          DependencyVersionEvidenceIr("scala", "3.4.2"),
          DependencyVersionEvidenceIr("gale", "0.1.0")
        ),
        12.5,
        ToleranceIr(1e-10, 1e-8),
        "result.glrm.fixture"
      )
    )

  private def unresolved(
      family: ModelFamilyEvidenceIr,
      estimand: ModelEstimandEvidenceIr,
      index: Int
  ): MathematicalModelEvidenceIr =
    val losses =
      if family == ModelFamilyEvidenceIr.GeneralizedLowRankModel ||
          family == ModelFamilyEvidenceIr.StructuredMultiblockFactorization
      then Vector(LossBindingEvidenceIr(s"domain.$index", EntryLossEvidenceIr.HalfSquared))
      else Vector.empty
    MathematicalModelEvidenceIr(
      id = s"evidence.unresolved.$index",
      contractId = family.contract.id.value,
      family = family,
      estimand = estimand,
      operatorProgramSchema = OperatorProgramDocumentIr.schemaV02,
      programId = s"program.$index",
      dataIdentity = s"data.$index",
      mask = ObservationMaskEvidenceIr.Complete(s"observations.$index"),
      losses = losses,
      geometries = Vector(GeometryBindingEvidenceIr(GeometryRoleEvidenceIr.RowNormalization, s"geometry.$index", s"certificate.geometry.$index")),
      penalties = Vector.empty,
      assumptions = Vector.empty,
      solver = SolverReceiptEvidenceIr(
        SolverFamilyEvidenceIr.AlternatingBlockCoordinate,
        "scalafim-multivar/0.1",
        s"policy.$index",
        s"trace.$index",
        ToleranceIr(1e-9, 1e-8),
        5
      ),
      achievedGuarantee = AchievedGuaranteeEvidenceIr.Unresolved("iteration limit"),
      certificateIdentities = Vector(s"certificate.geometry.$index"),
      reproducibility = ReproducibilityReceiptIr(
        s"generator.$index",
        index.toLong,
        Vector(DependencyVersionEvidenceIr("scala", "3.4.2")),
        1.0,
        ToleranceIr(1e-9, 1e-8),
        s"result.$index"
      )
    )

  private def assumption(theorem: String, id: String): TheoremAssumptionEvidenceIr =
    TheoremAssumptionEvidenceIr(theorem, id, s"witness.$id")

  private def validate(model: MathematicalModelEvidenceIr): Either[IrError, Unit] =
    MathematicalModelEvidenceIrValidator
      .validate(MathematicalModelEvidenceDocumentIr(MathematicalModelEvidenceDocumentIr.schemaV10, Vector(model)))
      .map(_ => ())

  private def assertInvalid(model: MathematicalModelEvidenceIr, clue: String): Unit =
    validate(model) match
      case Left(error) => assert(error.detail.contains(clue), s"${error.message} did not contain '$clue'")
      case Right(_) => fail(s"expected invalid model containing '$clue'")
