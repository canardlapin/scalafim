package scalafim.multivar.ir

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*
import scalafim.multivar.solver.*
import scalafim.multivar.lifecycle.*
import scalafim.multivar.capability.*
import scalafim.multivar.family.spectral.*
import scalafim.multivar.family.paired.*
import scalafim.multivar.family.canonical.*
import scalafim.multivar.family.cpca.*
import scalafim.multivar.family.sparse.*
import scalafim.multivar.family.glrm.*
import scalafim.multivar.family.multiblock.*
import scalafim.multivar.family.kernel.*
import scalafim.multivar.workflow.*
import scalafim.multivar.validation.*

class OperatorPlanIrSuite extends munit.FunSuite:

  private val strict = ToleranceIr(1e-10, 1e-8)

  test("realized operator plans round-trip without serializing estimator or solver dispatch"):
    val input = accepted(SampleByFeatureInput.of("study", 4, 3, MultivarSourceRef.DatasetSelection("train-fold")))
    val roi = accepted(RoiPlan.of("cortex", Vector(0, 2), Some("Cortex")))
    val rois = accepted(RoiPlanSet.of("analysis", Vector(roi), input.featureCount))
    val request = accepted(
      MultivarPlan.of(
        "semantic-plan",
        input,
        rois,
        MultivarEstimator.Pca(ComponentCount.unsafe(1)),
        MultivarExecutionPlan.distributedReadyRoi
      )
    )
    val plan = accepted(
      OperatorPlanIr.from(
        request,
        programDocument,
        Vector(OperatorPlanBindingIr("cortex", Vector("gpca-cortex")))
      )
    )

    val encoded = OperatorPlanIrCodec.encode(plan)
    val decoded = accepted(OperatorPlanIrCodec.decode(encoded))

    assertEquals(decoded, plan)
    assertEquals(OperatorPlanIrCodec.encode(decoded), encoded)
    assertEquals(decoded.bindings.head.programIds, Vector("gpca-cortex"))
    assert(!encoded.contains("estimator"))
    assert(!encoded.contains("solver_backend"))
    assert(!encoded.contains("gmd_backend"))

  test("plan validation rejects unbound programs, invalid ROI columns, and unknown fields"):
    val plan = OperatorPlanIr(
      OperatorPlanIr.schemaV01,
      "semantic-plan",
      OperatorPlanInputIr("study", 4, 3, OperatorPlanSourceIr.InMemory),
      Vector(OperatorPlanRoiIr("cortex", Vector(0, 2), None)),
      OperatorPlanExecutionIr(OperatorPlanExecutionModeIr.Local, OperatorPlanPartitionAxisIr.Roi, false),
      Vector(OperatorPlanBindingIr("cortex", Vector("gpca-cortex"))),
      programDocument
    )
    val unbound = plan.copy(bindings = Vector(OperatorPlanBindingIr("cortex", Vector.empty)))
    val invalidRoi = plan.copy(rois = Vector(OperatorPlanRoiIr("cortex", Vector(3), None)))
    val unknown = OperatorPlanIrCodec.encode(plan).replaceFirst("\"schema\":", "\"future\":true,\"schema\":")

    assertEquals(OperatorPlanIrValidator.validate(unbound).left.toOption.get.category, RejectionCategory.Malformed)
    assertEquals(OperatorPlanIrValidator.validate(invalidRoi).left.toOption.get.category, RejectionCategory.Malformed)
    assertEquals(OperatorPlanIrCodec.decode(unknown).left.toOption.get.category, RejectionCategory.UnknownField)

  private def programDocument: OperatorProgramDocumentIr =
    val spaces = Vector(
      SpaceIr("roi-features", SpaceRoleIr.Observed, 2),
      SpaceIr("components", SpaceRoleIr.Latent, 1)
    )
    val covariance = operator(
      "covariance",
      CoordinateIr("roi-features", VarianceIr.Dual),
      CoordinateIr("roi-features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Covariance,
      ProgramOperatorEvidenceIr(EvidenceStatusIr.Unchecked, Vector.empty)
    )
    val cometric = operator(
      "cometric",
      CoordinateIr("roi-features", VarianceIr.Dual),
      CoordinateIr("roi-features", VarianceIr.Primal),
      ProgramOperatorRoleIr.Cometric,
      certified("cometric", "spd")
    )
    val parameter = ProgramFrameParameterIr(
      "weights",
      "roi-features",
      "components",
      ProgramParameterizationIr.Identity
    )
    val result = ProgramResultContractIr(
      ProgramEquivalenceIr.Subspace(strict, strict),
      ProgramRepresentativeIr.OrderedSpectrumThenSign,
      ProgramSolverGuaranteeIr.GlobalSpectralOptimum
    )
    val program = OperatorProgramV2Ir(
      "gpca-cortex",
      Vector(parameter),
      ProgramObjectiveIr.MaximizeTrace("weights", "covariance"),
      Vector(ProgramNormalizationV2Ir("weights", "cometric")),
      Vector.empty,
      Vector.empty,
      result,
      Vector(ProvenanceEventIr.Source("gpca-cortex"))
    )
    OperatorProgramDocumentIr(
      OperatorProgramDocumentIr.schemaV02,
      spaces,
      Vector(covariance, cometric),
      Vector(program),
      Vector.empty,
      Vector.empty
    )

  private def operator(
      identity: String,
      domain: CoordinateIr,
      codomain: CoordinateIr,
      role: ProgramOperatorRoleIr,
      evidence: ProgramOperatorEvidenceIr
  ): ProgramOpIr =
    ProgramOpIr(
      identity,
      domain,
      codomain,
      role,
      evidence,
      ProgramRepresentationIr.Dense,
      ProgramGaugeIr.Ungauged,
      ProgramOperatorDerivationIr.Source,
      identity,
      Vector(ProvenanceEventIr.Source(identity))
    )

  private def certified(identity: String, property: String): ProgramOperatorEvidenceIr =
    ProgramOperatorEvidenceIr(
      EvidenceStatusIr.Certified,
      Vector(CertificateIr(property, identity, strict, "frobenius", "fixture", "float64", "gale", None, Some(0.0)))
    )

  private def accepted[A](value: Either[?, A]): A =
    value.fold(error => fail(error.toString), identity)
