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

class OperatorProgramIrSuite extends munit.FunSuite:

  test("operator programs lower to a stable descriptor-only JSON seam"):
    val feature = SpaceRef(MvSpace(SpaceId.unsafe("ir-feature"), SpaceRole.Observed, Dimension.unsafe(2)))
    val component = SpaceRef(MvSpace(SpaceId.unsafe("ir-component"), SpaceRole.Latent, Dimension.unsafe(1)))
    type F = feature.Id
    type K = component.Id
    val variable = acceptedProgram(FrameVariable.from(ParameterId.unsafe("weights"), feature.evidence, component.evidence))
    val parameterization = FrameParameterization.identity(variable)
    val value = acceptedSemantic(
      Op.fromDense(
        matrix(Vector(Vector(2.0, 0.0), Vector(0.0, 1.0))),
        CoordinateEvidence.dual(feature.evidence),
        CoordinateEvidence.primal(feature.evidence),
        OperatorRoleWitness.covariance,
        id("ir-component-value")
      )
    )
    val normalization = FrameNormalization(variable, certifiedCovariance(feature.evidence, DMat.eye(2), "ir-normalization"))
    val penalty = PenaltyTerm(
      TargetExpression.frame(variable.id),
      FunctionalKind.L1,
      PenaltyWeight.unsafe(0.25)
    )
    val program = acceptedProgram(
      OperatorProgram.from(
        Vector(parameterization),
        BaseObjective.MaximizeTrace(SelfCompressionExpression(variable, value)),
        Vector(normalization),
        penalties = Vector(penalty),
        provenance = SemanticProvenance.source("ir-program")
      )
    )

    val ir = OperatorProgramIr.from(program)
    val json = OperatorProgramIrCodec.encode(ir)

    assertEquals(ir.schema, OperatorProgramIr.schemaV01)
    assertEquals(ir.parameters.map(_.id), Vector("weights"))
    assertEquals(ir.parameters.map(_.parameterization), Vector("identity"))
    assertEquals(ir.objective, "maximize-trace")
    assertEquals(ir.penalties.map(_.functional), Vector("l1"))
    assertEquals(ir.result.equivalence, "frame:signed-permutation")
    val typed = ProgramSemanticIr.program("ir-program-v2", program)
    val typedValue = ProgramSemanticIr.operator(
      "ir-component-value-v2",
      value,
      ProgramOperatorDerivationIr.Source
    )
    assertEquals(typed.objective, ProgramObjectiveIr.MaximizeTrace("weights", "ir-component-value"))
    assertEquals(typed.result.equivalence, ProgramEquivalenceIr.Frame(ProgramFrameSymmetryIr.SignedPermutation, ToleranceIr(1e-10, 1e-8)))
    assertEquals(typedValue.role, ProgramOperatorRoleIr.Covariance)
    assertEquals(typedValue.evidence.status, EvidenceStatusIr.Unchecked)
    assertEquals(
      json,
      "{" +
        "\"schema\":\"scalafim-operator-program-ir/0.1\"," +
        "\"parameters\":[{" +
        "\"id\":\"weights\"," +
        "\"feature_space\":{\"id\":\"ir-feature\",\"role\":\"observed\",\"dimension\":2}," +
        "\"component_space\":{\"id\":\"ir-component\",\"role\":\"latent\",\"dimension\":1}," +
        "\"parameterization\":\"identity\",\"operator_identities\":[]}]," +
        "\"objective\":\"maximize-trace\"," +
        "\"normalizations\":[{\"parameter_id\":\"weights\",\"operator_identity\":\"ir-normalization\"}]," +
        "\"penalties\":[{\"parameter_id\":\"weights\",\"target\":\"frame\",\"capability\":\"linear\"," +
        "\"functional\":\"l1\",\"weight\":0.25,\"symmetry\":\"signed-permutation\"}]," +
        "\"constraints\":[]," +
        "\"result\":{\"equivalence\":\"frame:signed-permutation\",\"representative\":\"ordered-spectrum-then-sign\"," +
        "\"guarantee\":\"stationary-point\"}}"
    )

  private def certifiedCovariance[S <: SemanticSpace](
      space: SpaceEvidence[S],
      value: DMat,
      name: String
  ): OpCovariance[S, CertifiedSpd] =
    val linear = acceptedSemantic(
      Lin.fromDenseMatrix(value, CoordinateEvidence.dual(space), CoordinateEvidence.primal(space), id(name))
    )
    acceptedSemantic(
      Op.certifiedSpd(
        Op.fromLin(linear, OperatorRoleWitness.covariance),
        acceptedSemantic(FormCertificates.spd(linear))
      )
    )

  private def acceptedProgram[A](result: Either[ProgramError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def acceptedSemantic[A](result: Either[SemanticError, A]): A =
    result.fold(error => fail(error.message), identity)

  private def id(name: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(name))

  private def matrix(rows: Vector[Vector[Double]]): DMat =
    GaleNumerics.matrixFromRows(rows)
