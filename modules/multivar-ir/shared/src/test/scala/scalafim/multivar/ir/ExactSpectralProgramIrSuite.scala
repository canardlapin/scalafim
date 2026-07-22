package scalafim.multivar.ir

import gale.linalg.DMat
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

class ExactSpectralProgramIrSuite extends munit.FunSuite:

  test("requested and lowered quadratic programs, proof, and attained guarantee round-trip together"):
    val prepared = DynamicGpcaProblem
      .from(
        MatrixView.dense(
          GaleNumerics.matrixFromRows(
            Vector(
              Vector(2.0, 0.0, 1.0),
              Vector(0.0, 2.0, 1.0),
              Vector(-2.0, 0.0, -1.0),
              Vector(0.0, -2.0, -1.0)
            )
          )
        ),
        MvSpace.of("ir-exact-rows", SpaceRole.Samples, 4).toOption.get,
        MvSpace.of("ir-exact-features", SpaceRole.Observed, 3).toOption.get,
        MetricSpec.identity(4).toOption.get,
        MetricSpec.identity(3).toOption.get,
        identity("ir-exact-source"),
        SemanticProvenance.source("ir-exact")
      )
      .toOption
      .get
    val problem = prepared.value
    val targetOperator = Op
      .fromDense(
        DMat.eye(problem.featureSpace.dimension),
        CoordinateEvidence.dual(problem.featureSpace),
        CoordinateEvidence.primal(problem.featureSpace),
        OperatorRoleWitness.cross,
        identity("ir-exact-target")
      )
      .toOption
      .get
    val parameterId = ParameterId.unsafe(s"${problem.featureSpace.id.value}.quadratic-gpca-frame")
    val target = TargetExpression.linear(parameterId, "ir-exact-target", targetOperator).toOption.get
    val penalty = PenaltyTerm(
      target,
      FunctionalKind.SquaredNorm(problem.featureMetric.valueIdentity),
      PenaltyWeight.unsafe(0.1)
    )
    val lowering = QuadraticPullback
      .lower(
        penalty,
        targetOperator,
        problem.featureMetric,
        QuadraticFamily.GraphSmoothness,
        QuadraticPlacement.ObjectiveRidge
      )
      .toOption
      .get
    val effective = QuadraticPullback.effective(problem.covariance, lowering)
    val fit = ExactSpectralPrograms
      .gpcaQuadratic(problem, lowering, ComponentCount.unsafe(2))
      .fold(error => fail(error.message), current => current)
    val originalId = "ir-exact-requested"
    val loweredId = "ir-exact-lowered"
    val rewriteId = "ir-exact-rewrite"
    val original = ProgramSemanticIr.program(originalId, fit.requestedProgram)
    val lowered = ProgramSemanticIr.program(loweredId, fit.loweredProgram)
    val rewrite = ProgramSemanticIr.exactSpectralRewrite(rewriteId, originalId, loweredId, fit)
    val programFit = ProgramSemanticIr.programFit(loweredId, fit.programFit)
    val operators = Vector(
      ProgramSemanticIr.operator("ir-exact-covariance", problem.covariance, ProgramOperatorDerivationIr.Source),
      ProgramSemanticIr.operator("ir-exact-cometric", problem.featureCometric, ProgramOperatorDerivationIr.Source),
      ProgramSemanticIr.operator("ir-exact-target", targetOperator, ProgramOperatorDerivationIr.Source),
      ProgramSemanticIr.operator("ir-exact-metric", problem.featureMetric, ProgramOperatorDerivationIr.Source),
      ProgramSemanticIr.operator(
        "ir-exact-pullback",
        lowering.pulledBack,
        ProgramOperatorDerivationIr.Lowered(
          lowering.proof.rule,
          Vector(targetOperator.valueIdentity.stableKey, problem.featureMetric.valueIdentity.stableKey)
        )
      ),
      ProgramSemanticIr.operator(
        "ir-exact-effective",
        effective,
        ProgramOperatorDerivationIr.Lowered(
          "objective-quadratic",
          Vector(problem.covariance.valueIdentity.stableKey, lowering.pulledBack.valueIdentity.stableKey)
        )
      ),
      ProgramSemanticIr.operator(
        "ir-exact-frame",
        fit.functionalFrame.weights,
        ProgramOperatorDerivationIr.Lowered(
          "global-spectral-solve",
          Vector(effective.valueIdentity.stableKey, problem.featureCometric.valueIdentity.stableKey)
        )
      )
    )
    val document = OperatorProgramDocumentIr(
      OperatorProgramDocumentIr.schemaV02,
      Vector(
        SemanticIr.space(problem.featureSpace.descriptor),
        SemanticIr.space(fit.functionalFrame.weights.domain.descriptor.space)
      ),
      operators,
      Vector(original, lowered),
      Vector(rewrite),
      Vector(programFit)
    )

    val encoded = OperatorProgramDocumentIrCodec.encode(document)
    val decoded = OperatorProgramDocumentIrCodec
      .decode(encoded)
      .fold(error => fail(error.message), current => current)

    assertEquals(decoded, document)
    assertEquals(decoded.rewrites.head.rule, ProgramRewriteRuleIr.QuadraticPullback)
    assertEquals(decoded.rewrites.head.proof.valueIdentity, rewriteId)
    assertEquals(decoded.fits.head.solverGuarantee, ProgramSolverGuaranteeIr.GlobalSpectralOptimum)
    assertEquals(decoded.fits.head.residualCertificates.head.property, "converged")
    assertEquals(decoded.programs.head.penalties.length, 1)
    assertEquals(decoded.programs(1).penalties, Vector.empty)

  private def identity(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))
