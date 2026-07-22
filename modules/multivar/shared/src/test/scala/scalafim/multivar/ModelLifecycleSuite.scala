package scalafim.multivar

import gale.linalg.DMat
import scala.compiletime.testing.typeCheckErrors

class ModelLifecycleSuite extends munit.FunSuite:

  private val prepared = DynamicGpcaProblem
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
      MvSpace.of("lifecycle-rows", SpaceRole.Samples, 4).toOption.get,
      MvSpace.of("lifecycle-features", SpaceRole.Observed, 3).toOption.get,
      MetricSpec.identity(4).toOption.get,
      MetricSpec.identity(3).toOption.get,
      ValueIdentity.source(ValueId.unsafe("lifecycle-source")),
      SemanticProvenance.source("lifecycle")
    )
    .toOption
    .get
  private val gpca = prepared.value

  test("family contracts check runtime catalog values against enum-case singleton types"):
    assert(FamilyContract.from[ExactSpectralFamily](MathematicalContractCatalog.exactSpectralFrame).isRight)
    FamilyContract
      .from[ExactSpectralFamily](MathematicalContractCatalog.generalizedLowRankModel)
      .left
      .toOption match
      case Some(ModelLifecycleError.ContractFamilyMismatch(expected, actual)) =>
        assertEquals(expected, MathematicalModelFamily.ExactSpectralFrame)
        assertEquals(actual, MathematicalModelFamily.GeneralizedLowRankModel)
      case other => fail(s"expected a typed family mismatch, got $other")

  test("family-indexed identities and stages reject cross-family substitution at compile time"):
    val identityErrors = typeCheckErrors("""
      val spectral = null.asInstanceOf[
        scalafim.multivar.ProgramId[scalafim.multivar.MathematicalModelFamily.ExactSpectralFrame.type]
      ]
      val glrm: scalafim.multivar.ProgramId[
        scalafim.multivar.MathematicalModelFamily.GeneralizedLowRankModel.type
      ] = spectral
    """)
    val stageErrors = typeCheckErrors("""
      import scalafim.multivar.*
      def accept(
          program: ExactSpectralModelProgram,
          compiled: CompiledModel[ExactSpectralFamily, ExactSpectralModelProgram]
      ): Unit = ()
      val program = null.asInstanceOf[ExactSpectralModelProgram]
      val foreign = null.asInstanceOf[
        CompiledModel[
          MathematicalModelFamily.GeneralizedLowRankModel.type,
          ModelProgram[MathematicalModelFamily.GeneralizedLowRankModel.type]
        ]
      ]
      accept(program, foreign)
    """)

    assert(identityErrors.nonEmpty)
    assert(stageErrors.nonEmpty)

  test("a fitted model cannot be constructed outside its bound execution factory"):
    val errors = typeCheckErrors("""
      import scalafim.multivar.*
      new FittedModel[ExactSpectralFamily, ExactSpectralModelProgram, OperatorFitBundle](
        null,
        null,
        null,
        null,
        null,
        null
      )
    """)

    assert(errors.nonEmpty)

  test("exact spectral adaptation binds declared, compiled, receipt, evidence, and payload identities"):
    val exact = exactFit(QuadraticFamily.GraphSmoothness)
    val declared = accepted(ExactSpectralLifecycle.declare(exact.requestedProgram))
    val method = exact.programFit.solverAttestation.certificate.context.method
    val compiled = accepted(
      ExactSpectralLifecycle.compile(declared, exact.loweredProgram, exact.proof, method)
    )
    val payload = accepted(ExactSpectralLifecycle.bundle(exact))
    val dataIdentity = exact.programFit.achievedGuarantee.semanticEvidence.bindings.data
    val scope = TrainingScopeId.standalone(dataIdentity)
    val fitted = accepted(ExactSpectralLifecycle.solve(compiled, payload, scope))

    assert(declared.operatorProgram eq exact.requestedProgram)
    assertEquals(declared.id.valueIdentity, exact.requestedProgram.valueIdentity)
    assertEquals(compiled.executionProgramIdentity, exact.loweredProgram.valueIdentity)
    assertEquals(fitted.binding.program.valueIdentity, declared.id.valueIdentity)
    assertEquals(fitted.binding.compiled.valueIdentity, compiled.id.valueIdentity)
    assertEquals(fitted.binding.compiledProgram, exact.loweredProgram.valueIdentity)
    assertEquals(fitted.binding.data, dataIdentity)
    assertEquals(fitted.binding.scope.valueIdentity, scope.valueIdentity)
    assertEquals(
      fitted.binding.result.valueIdentity,
      exact.programFit.achievedGuarantee.semanticEvidence.bindings.result
    )
    assert(fitted.payload.programFit eq exact.programFit)
    assertEquals(fitted.solver.achieved, exact.programFit.achievedGuarantee)
    assertEquals(
      fitted.solver.certificates.values,
      Vector(exact.programFit.solverAttestation.certificate)
    )
    fitted.solver.receipt match
      case receipt: ExactSpectralReceipt =>
        assertEquals(receipt.retainedRank, exact.programFit.identifiability.retainedRank)
        assertEqualsDouble(receipt.residual, exact.programFit.identifiability.residual, 0.0)
        assertEquals(receipt.spectralClusters, exact.programFit.identifiability.spectralClusters)
      case other => fail(s"expected an exact-spectral receipt, got $other")

  test("the convenience adapter preserves existing spectral numerical results"):
    val exact = exactFit(QuadraticFamily.Ridge)
    val fitted = accepted(exact.toLifecycleFit)
    val originalWeights = exact.functionalFrame.weights.valueIdentity

    assertEquals(fitted.payload.parameterFrames.map(_.sourceIdentity), Vector(originalWeights))
    assertEquals(fitted.payload.programFit.objectiveValue, exact.programFit.objectiveValue)
    assertEquals(fitted.payload.programFit.identifiability, exact.programFit.identifiability)

  test("a payload from another compiled spectral program fails at the binding boundary"):
    val first = exactFit(QuadraticFamily.GraphSmoothness)
    val second = exactFit(QuadraticFamily.Ridge)
    val declared = accepted(ExactSpectralLifecycle.declare(first.requestedProgram))
    val compiled = accepted(
      ExactSpectralLifecycle.compile(
        declared,
        first.loweredProgram,
        first.proof,
        first.programFit.solverAttestation.certificate.context.method
      )
    )
    val foreignPayload = accepted(ExactSpectralLifecycle.bundle(second))
    val foreignData = second.programFit.achievedGuarantee.semanticEvidence.bindings.data

    ExactSpectralLifecycle
      .solve(compiled, foreignPayload, TrainingScopeId.standalone(foreignData))
      .left
      .toOption match
      case Some(_: ModelLifecycleError.BindingMismatch) => ()
      case other => fail(s"expected a binding mismatch, got $other")

  test("post-fit behavior exists only through an explicit capability"):
    type SpectralFit = FittedModel[ExactSpectralFamily, ExactSpectralModelProgram, OperatorFitBundle]
    given CanTransform[SpectralFit, String] with
      type Output = Option[OperatorSnapshot]
      def transform(
          fit: SpectralFit,
          label: String
      ): Either[ModelLifecycleError, Option[OperatorSnapshot]] =
        Right(fit.payload.operator(label))

    import CanTransform.*
    val fitted = accepted(exactFit(QuadraticFamily.Ridge).toLifecycleFit)
    val label = fitted.payload.parameterFrames.head.label

    assertEquals(accepted(fitted.transformWith(label)), fitted.payload.operator(label))
    val absentCapability = typeCheckErrors("""
      import scalafim.multivar.*
      import scalafim.multivar.CanEncode.*
      type SpectralFit = FittedModel[ExactSpectralFamily, ExactSpectralModelProgram, OperatorFitBundle]
      val fit = null.asInstanceOf[SpectralFit]
      fit.encodeWith(1)
    """)
    assert(absentCapability.nonEmpty)

  private def exactFit(
      family: QuadraticFamily
  ): ExactSpectralProgramFit[prepared.features.Id, ? <: SemanticSpace] =
    ExactSpectralPrograms
      .gpcaQuadratic(
        gpca,
        lowering(family),
        ComponentCount.unsafe(2)
      )
      .fold(error => fail(error.message), identity)

  private def lowering(family: QuadraticFamily): QuadraticLowering[prepared.features.Id] =
    val identity = Op
      .fromDense(
        DMat.eye(gpca.featureSpace.dimension),
        CoordinateEvidence.dual(gpca.featureSpace),
        CoordinateEvidence.primal(gpca.featureSpace),
        OperatorRoleWitness.cross,
        ValueIdentity.source(ValueId.unsafe(s"lifecycle-${family.toString}-target"))
      )
      .toOption
      .get
    val parameter = ParameterId.unsafe(s"${gpca.featureSpace.id.value}.quadratic-gpca-frame")
    val target = TargetExpression.linear(parameter, s"${family.toString}-target", identity).toOption.get
    val term = PenaltyTerm(
      target,
      FunctionalKind.SquaredNorm(gpca.featureMetric.valueIdentity),
      PenaltyWeight.unsafe(0.1)
    )
    QuadraticPullback
      .lower(term, identity, gpca.featureMetric, family, QuadraticPlacement.ObjectiveRidge)
      .toOption
      .get

  private def accepted[A](value: Either[ModelLifecycleError, A]): A =
    value.fold(error => fail(error.message), identity)
