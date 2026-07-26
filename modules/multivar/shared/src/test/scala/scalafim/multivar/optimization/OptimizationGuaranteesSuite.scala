package scalafim.multivar
package optimization

import scalafim.multivar.core.*
import scalafim.multivar.contract.*
import scalafim.multivar.optimization.*

import scala.compiletime.testing.typeCheckErrors

class OptimizationGuaranteesSuite extends munit.FunSuite:

  test("phantom proof constants cannot be interchanged and reject invalid values"):
    val smooth = PositiveProofConstant.smoothness(2.0).toOption.get
    val strong = PositiveProofConstant.strongConvexity(0.5).toOption.get

    assertEquals(smooth.doubleValue, 2.0)
    assertEquals(strong.doubleValue, 0.5)
    assert(PositiveProofConstant.operatorNorm(Double.PositiveInfinity).isLeft)
    assert(NonNegativeProofBound.objectiveGap(-1.0).isLeft)
    assert(
      typeCheckErrors(
        """
        import scalafim.multivar.core.*
        import scalafim.multivar.contract.*
        import scalafim.multivar.optimization.*
        val smooth: SmoothnessConstant = PositiveProofConstant.smoothness(1.0).toOption.get
        val invalid: StrongConvexityModulus = smooth
        """
      ).nonEmpty
    )

  test("identity bindings make complete and observed masks explicit"):
    val fixture = anchorFixture(ObservationMaskIdentity.Observed(id("mask")))

    assertEquals(fixture.bindings.mask, ObservationMaskIdentity.Observed(id("mask")))
    assert(
      OptimizationIdentityBindings
        .from(
          fixture.contract.id,
          fixture.bindings.program,
          fixture.bindings.data,
          ObservationMaskIdentity.Complete,
          Vector.empty,
          fixture.bindings.parameters,
          fixture.bindings.result
        )
        .isLeft
    )

  test("PSD runtime witnesses reuse and identity-check the extant certificate algebra"):
    val fixture = exactFixture()
    val psd = Certificate.unsafe[PsdProperty](
      fixture.operator,
      CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
      CertificateContext.portableFloat64
    ).runtime
    val wrong = Certificate.unsafe[PsdProperty](
      id("foreign-operator"),
      CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
      CertificateContext.portableFloat64
    ).runtime

    assert(PsdOperatorWitness.from(fixture.bindings, fixture.operator, psd, assumption("symmetric-value-operator")).isRight)
    assert(PsdOperatorWitness.from(fixture.bindings, fixture.operator, wrong, assumption("symmetric-value-operator")).isLeft)

  test("a numerical Converged status cannot manufacture a global claim"):
    val fixture = exactFixture()
    val evidence = semanticEvidence(fixture.bindings, NumericalTermination.Converged)
    val result = OptimizationGuaranteeAdmission.admit(
      fixture.contract,
      OptimizationClaimClass.ExactGlobal,
      OptimizationAssumptions.empty(fixture.bindings),
      Set.empty,
      evidence
    )

    assert(result.left.exists(_.isInstanceOf[OptimizationGuaranteeError.MissingGlobalOptimalityWitness]))

  test("exact global admission requires the contract theorem, its witnesses, and an admitted oracle"):
    val fixture = exactFixture()
    val assumptions = exactAssumptions(fixture)
    val evidence = semanticEvidence(fixture.bindings, NumericalTermination.Converged)
    val witness = GlobalOptimalityWitness
      .from(
        fixture.bindings,
        theorem("symmetric-generalized-spectrum"),
        Set(
          assumption("symmetric-value-operator"),
          assumption("spd-normalization"),
          assumption("certified-spectrum")
        ),
        OracleFamily.Analytic
      )
      .toOption
      .get
    val result = OptimizationGuaranteeAdmission
      .admit(
        fixture.contract,
        OptimizationClaimClass.ExactGlobal,
        assumptions,
        Set(
          OptimizationProofObligation.PositiveSemidefinite(fixture.operator),
          OptimizationProofObligation.NormBounded(fixture.operator)
        ),
        evidence,
        Some(witness)
      )
      .toOption
      .get

    assertEquals(result.claimClass, OptimizationClaimClass.ExactGlobal)
    assertEquals(result.semanticEvidence.bindings.contract, MathematicalContractCatalog.exactSpectralFrame.id)

  test("compiler-style admission reports every absent proof obligation"):
    val fixture = anchorFixture()
    val evidence = semanticEvidence(
      fixture.bindings,
      NumericalTermination.Converged,
      objectiveGap = Some(NonNegativeProofBound.objectiveGap(1e-4).toOption.get)
    )
    val result = OptimizationGuaranteeAdmission.admit(
      fixture.contract,
      OptimizationClaimClass.EpsilonGlobal,
      OptimizationAssumptions.empty(fixture.bindings),
      Set(
        OptimizationProofObligation.Smooth(fixture.bindings.program),
        OptimizationProofObligation.ExactProximal(fixture.operator)
      ),
      evidence
    )

    result match
      case Left(OptimizationGuaranteeError.MissingProofObligations(missing)) =>
        assert(missing.contains(OptimizationProofObligation.ProperClosedConvex(fixture.bindings.program)))
        assert(missing.contains(OptimizationProofObligation.Smooth(fixture.bindings.program)))
        assert(missing.contains(OptimizationProofObligation.ExactProximal(fixture.operator)))
      case other => fail(s"expected missing obligations, got $other")

  test("coercivity, norm, exact-oracle, and controlled-inexactness obligations are identity-bound"):
    val fixture = anchorFixture()
    val coercivity = NullspaceCoercivityWitness
      .from(
        fixture.bindings,
        fixture.bindings.program,
        fixture.bindings.data,
        PositiveProofConstant.nullspaceCoercivity(0.25).toOption.get,
        assumption("coercive-on-nullspace")
      )
      .toOption
      .get
    val norm = OperatorNormWitness
      .from(
        fixture.bindings,
        fixture.operator,
        PositiveProofConstant.operatorNorm(3.0).toOption.get,
        assumption("certified-norm")
      )
      .toOption
      .get
    val exact = ExactOracleLawWitness
      .from(
        fixture.bindings,
        fixture.operator,
        ExactOracleKind.Proximal,
        assumption("exact-proximal")
      )
      .toOption
      .get
    val inexact = ControlledInexactnessWitness
      .from(
        fixture.bindings,
        fixture.operator,
        NonNegativeProofBound.inexactness(1e-7).toOption.get,
        assumption("controlled-inexactness")
      )
      .toOption
      .get
    val assumptions = OptimizationAssumptions
      .from(
        fixture.bindings,
        nullspaceCoercivity = Vector(coercivity),
        normBounds = Vector(norm),
        exactOracleLaws = Vector(exact),
        controlledInexactness = Vector(inexact)
      )
      .toOption
      .get
    val obligations = Set(
      OptimizationProofObligation.CoerciveOnNullspace(fixture.bindings.program, fixture.bindings.data),
      OptimizationProofObligation.NormBounded(fixture.operator),
      OptimizationProofObligation.ExactProximal(fixture.operator),
      OptimizationProofObligation.ControlledInexactness(fixture.operator)
    )

    assertEquals(assumptions.unsatisfied(obligations), Set.empty)

  test("epsilon-global and unique-minimizer claims retain their quantitative bounds"):
    val fixture = anchorFixture()
    val assumptions = anchorAssumptions(fixture)
    val gap = NonNegativeProofBound.objectiveGap(0.02).toOption.get
    val evidence = semanticEvidence(
      fixture.bindings,
      NumericalTermination.Converged,
      objectiveGap = Some(gap)
    )
    val witness = anchorGlobalWitness(fixture, assumptions)
    val obligations = Set(
      OptimizationProofObligation.Smooth(fixture.bindings.program),
      OptimizationProofObligation.ExactProximal(fixture.operator)
    )
    val epsilon = OptimizationGuaranteeAdmission
      .admit(
        fixture.contract,
        OptimizationClaimClass.EpsilonGlobal,
        assumptions,
        obligations,
        evidence,
        Some(witness)
      )
      .toOption
      .get
    val unique = OptimizationGuaranteeAdmission
      .admit(
        fixture.contract,
        OptimizationClaimClass.UniqueMinimizerWithinBound,
        assumptions,
        obligations,
        evidence,
        Some(witness)
      )
      .toOption
      .get

    epsilon match
      case AchievedOptimizationGuarantee.EpsilonGlobal(bound, _) =>
        assertEqualsDouble(bound.doubleValue, 0.02, 1e-15)
      case other => fail(s"expected epsilon-global evidence, got $other")
    unique match
      case AchievedOptimizationGuarantee.UniqueMinimizerWithinBound(distance, _) =>
        assertEqualsDouble(distance.doubleValue, 0.2, 1e-15)
      case other => fail(s"expected a distance bound, got $other")

  test("stationary, coordinatewise-stationary, feasible, and unresolved remain distinct"):
    val contract = MathematicalContractCatalog.jointSparseFunctionalFactorization
    val bindings = bindingsFor(contract, Vector(ParameterId.unsafe("left"), ParameterId.unsafe("right")))
    val residual = NonNegativeProofBound.residual(1e-6).toOption.get
    val evidence = semanticEvidence(
      bindings,
      NumericalTermination.Converged,
      stationarity = Some(residual),
      blockStationarity = bindings.parameters.map(_ -> residual),
      feasibility = Some(residual)
    )
    val assumptions = OptimizationAssumptions.empty(bindings)

    val stationary = admit(contract, OptimizationClaimClass.Stationary, assumptions, evidence)
    val coordinatewise = admit(contract, OptimizationClaimClass.CoordinatewiseStationary, assumptions, evidence)
    val feasible = admit(contract, OptimizationClaimClass.Feasible, assumptions, evidence)
    val unresolved = admit(contract, OptimizationClaimClass.Unresolved, assumptions, evidence)

    assert(stationary.isInstanceOf[AchievedOptimizationGuarantee.Stationary])
    assert(coordinatewise.isInstanceOf[AchievedOptimizationGuarantee.CoordinatewiseStationary])
    assert(feasible.isInstanceOf[AchievedOptimizationGuarantee.FeasibleOnly])
    assert(unresolved.isInstanceOf[AchievedOptimizationGuarantee.Unresolved])

  test("coordinatewise stationarity must cover every bound parameter block"):
    val contract = MathematicalContractCatalog.jointSparseFunctionalFactorization
    val bindings = bindingsFor(contract, Vector(ParameterId.unsafe("left"), ParameterId.unsafe("right")))
    val residual = NonNegativeProofBound.residual(1e-6).toOption.get
    val evidence = semanticEvidence(
      bindings,
      NumericalTermination.Converged,
      blockStationarity = Vector(bindings.parameters.head -> residual)
    )
    val result = OptimizationGuaranteeAdmission.admit(
      contract,
      OptimizationClaimClass.CoordinatewiseStationary,
      OptimizationAssumptions.empty(bindings),
      Set.empty,
      evidence
    )

    assert(result.left.exists(_.isInstanceOf[OptimizationGuaranteeError.IncompleteBlockStationarity]))

  test("semantic certificates reject foreign result and operator identities"):
    val fixture = anchorFixture()
    val foreign = Certificate.unsafe[ConvergenceProperty](
      id("foreign-result"),
      CertificateClaim.Converged(1, 0.0, 1.0),
      CertificateContext.portableFloat64
    ).runtime

    assert(
      SemanticOptimizationEvidence
        .from(
          fixture.bindings,
          NumericalTermination.Converged,
          numericalCertificates = Vector(foreign)
        )
        .isLeft
    )

  private final case class ProofFixture(
      contract: MathematicalModelContract,
      bindings: OptimizationIdentityBindings,
      operator: ValueIdentity
  )

  private def anchorFixture(
      mask: ObservationMaskIdentity = ObservationMaskIdentity.Complete
  ): ProofFixture =
    val contract = MathematicalContractCatalog.anchorRegularizedFrame
    val operator = id("anchor-proximal")
    val bindings = bindingsFor(contract, Vector(ParameterId.unsafe("anchor-frame")), mask, operator)
    ProofFixture(contract, bindings, operator)

  private def exactFixture(): ProofFixture =
    val contract = MathematicalContractCatalog.exactSpectralFrame
    val operator = id("spectral-operator")
    val bindings = bindingsFor(contract, Vector(ParameterId.unsafe("spectral-frame")), operator = operator)
    ProofFixture(contract, bindings, operator)

  private def bindingsFor(
      contract: MathematicalModelContract,
      parameters: Vector[ParameterId],
      mask: ObservationMaskIdentity = ObservationMaskIdentity.Complete,
      operator: ValueIdentity = id("joint-operator")
  ): OptimizationIdentityBindings =
    OptimizationIdentityBindings
      .from(
        contract.id,
        id(s"${contract.family}-program"),
        id(s"${contract.family}-data"),
        mask,
        Vector(operator),
        parameters,
        id(s"${contract.family}-result")
      )
      .toOption
      .get

  private def exactAssumptions(fixture: ProofFixture): OptimizationAssumptions =
    val certificate = Certificate.unsafe[PsdProperty](
      fixture.operator,
      CertificateClaim.PositiveSemidefinite(0.0, 0.0, 1.0),
      CertificateContext.portableFloat64
    ).runtime
    val psd = PsdOperatorWitness
      .from(fixture.bindings, fixture.operator, certificate, assumption("symmetric-value-operator"))
      .toOption
      .get
    val norm = OperatorNormWitness
      .from(
        fixture.bindings,
        fixture.operator,
        PositiveProofConstant.operatorNorm(1.0).toOption.get,
        assumption("spd-normalization")
      )
      .toOption
      .get
    val exact = ExactOracleLawWitness
      .from(
        fixture.bindings,
        fixture.operator,
        ExactOracleKind.Projection,
        assumption("certified-spectrum")
      )
      .toOption
      .get
    OptimizationAssumptions
      .from(fixture.bindings, positiveSemidefinite = Vector(psd), normBounds = Vector(norm), exactOracleLaws = Vector(exact))
      .toOption
      .get

  private def anchorAssumptions(fixture: ProofFixture): OptimizationAssumptions =
    val convex = ProperClosedConvexWitness
      .from(fixture.bindings, fixture.bindings.program, assumption("proper-closed-convex-penalty"))
      .toOption
      .get
    val smooth = SmoothnessWitness
      .from(
        fixture.bindings,
        fixture.bindings.program,
        PositiveProofConstant.smoothness(1.0).toOption.get,
        assumption("strongly-convex-anchor")
      )
      .toOption
      .get
    val strong = StrongConvexityWitness
      .from(
        fixture.bindings,
        fixture.bindings.program,
        PositiveProofConstant.strongConvexity(1.0).toOption.get,
        assumption("strongly-convex-anchor")
      )
      .toOption
      .get
    val exact = ExactOracleLawWitness
      .from(
        fixture.bindings,
        fixture.operator,
        ExactOracleKind.Proximal,
        assumption("certified-prox-or-splitting")
      )
      .toOption
      .get
    OptimizationAssumptions
      .from(
        fixture.bindings,
        properClosedConvex = Vector(convex),
        smoothness = Vector(smooth),
        strongConvexity = Vector(strong),
        exactOracleLaws = Vector(exact)
      )
      .toOption
      .get

  private def anchorGlobalWitness(
      fixture: ProofFixture,
      assumptions: OptimizationAssumptions
  ): GlobalOptimalityWitness =
    GlobalOptimalityWitness
      .from(
        fixture.bindings,
        theorem("strongly-convex-anchor-composite"),
        assumptions.assumptionReferences,
        OracleFamily.Analytic
      )
      .toOption
      .get

  private def semanticEvidence(
      bindings: OptimizationIdentityBindings,
      termination: NumericalTermination,
      stationarity: Option[CertifiedResidualBound] = None,
      blockStationarity: Vector[(ParameterId, CertifiedResidualBound)] = Vector.empty,
      feasibility: Option[CertifiedResidualBound] = None,
      objectiveGap: Option[CertifiedObjectiveGap] = None
  ): SemanticOptimizationEvidence =
    SemanticOptimizationEvidence
      .from(
        bindings,
        termination,
        stationarity,
        blockStationarity,
        feasibility,
        objectiveGap
      )
      .toOption
      .get

  private def admit(
      contract: MathematicalModelContract,
      claim: OptimizationClaimClass,
      assumptions: OptimizationAssumptions,
      evidence: SemanticOptimizationEvidence
  ): AchievedOptimizationGuarantee =
    OptimizationGuaranteeAdmission
      .admit(contract, claim, assumptions, Set.empty, evidence)
      .toOption
      .get

  private def assumption(value: String): ContractReference[AssumptionReference] =
    ContractReference.assumption(value).toOption.get

  private def theorem(value: String): ContractReference[TheoremReference] =
    ContractReference.theorem(value).toOption.get

  private def id(value: String): ValueIdentity =
    ValueIdentity.source(ValueId.unsafe(value))
