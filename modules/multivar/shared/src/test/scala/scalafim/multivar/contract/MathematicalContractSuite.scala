package scalafim.multivar
package contract

import scalafim.multivar.contract.*

import scala.compiletime.testing.typeCheckErrors

class MathematicalContractSuite extends munit.FunSuite:

  test("contract references are nominal, validated identifiers"):
    assert(ContractReference.model(" ").isLeft)
    assert(ContractReference.formula("contains spaces").isLeft)
    assertEquals(accepted(ContractReference.api("GeneralizedLowRankProgram")).value, "GeneralizedLowRankProgram")

  test("reference roles cannot be interchanged at compile time"):
    val errors = typeCheckErrors("""
      val api = scalafim.multivar.contract.ContractReference.api("api").toOption.get
      val formula = scalafim.multivar.contract.ContractReference.formula("formula").toOption.get
      val ir = scalafim.multivar.contract.ContractReference.ir("ir").toOption.get
      scalafim.multivar.contract.FormulaBinding(api, formula, ir)
    """)
    assert(errors.nonEmpty)

  test("estimands determine their model family and estimation stage"):
    val expected = Vector(
      MultivarEstimand.AnchorCoefficientRefinement ->
        (MathematicalModelFamily.AnchorRegularizedFrame, EstimationStage.PostFitRefinement),
      MultivarEstimand.GeneralizedSpectralSubspace ->
        (MathematicalModelFamily.ExactSpectralFrame, EstimationStage.JointEstimation),
      MultivarEstimand.JointStructuredFactors ->
        (MathematicalModelFamily.JointSparseFunctionalFactorization, EstimationStage.JointEstimation),
      MultivarEstimand.GeneralizedLatentRepresentation ->
        (MathematicalModelFamily.GeneralizedLowRankModel, EstimationStage.JointEstimation),
      MultivarEstimand.ConvexLowRankMatrix ->
        (MathematicalModelFamily.ConvexifiedLowRankMatrix, EstimationStage.ConvexMatrixEstimation),
      MultivarEstimand.SharedBlockLatentRepresentation ->
        (MathematicalModelFamily.StructuredMultiblockFactorization, EstimationStage.JointEstimation)
    )

    expected.foreach:
      case (estimand, (family, stage)) =>
        assertEquals(estimand.family, family)
        assertEquals(family.estimationStage, stage)

  test("anchor refinement and joint sparse-functional fitting cannot be conflated"):
    val result = MathematicalModelContract.from(
      ContractReference.unsafeModel("invalid-anchor-as-joint"),
      MathematicalModelFamily.AnchorRegularizedFrame,
      MultivarEstimand.JointStructuredFactors,
      binding("invalid-anchor-as-joint"),
      FrameSymmetry.SignedPermutation,
      ContractMaturity.Planned,
      Set(OptimizationClaimClass.Feasible),
      Vector.empty,
      Set(OracleFamily.Analytic),
      Vector.empty
    )

    assert(result.left.exists(_.isInstanceOf[MathematicalContractError.EstimandFamilyMismatch]))

  test("nonconvex factorizations reject global claims"):
    val result = MathematicalModelContract.from(
      ContractReference.unsafeModel("invalid-global-glrm"),
      MathematicalModelFamily.GeneralizedLowRankModel,
      MultivarEstimand.GeneralizedLatentRepresentation,
      binding("invalid-global-glrm"),
      FrameSymmetry.Identity,
      ContractMaturity.Planned,
      Set(OptimizationClaimClass.ExactGlobal),
      Vector(
        theorem(
          "invalid-global-theorem",
          Set(OptimizationClaimClass.ExactGlobal)
        )
      ),
      Set(OracleFamily.Analytic),
      Vector.empty
    )

    assert(result.left.exists(_.isInstanceOf[MathematicalContractError.InadmissibleClaim]))

  test("global claims require theorem support and an independent oracle"):
    val noTheorem = MathematicalModelContract.from(
      ContractReference.unsafeModel("global-without-theorem"),
      MathematicalModelFamily.ExactSpectralFrame,
      MultivarEstimand.GeneralizedSpectralSubspace,
      binding("global-without-theorem"),
      FrameSymmetry.Orthogonal,
      ContractMaturity.Planned,
      Set(OptimizationClaimClass.ExactGlobal),
      Vector.empty,
      Set(OracleFamily.Analytic),
      Vector.empty
    )
    val noIndependentOracle = MathematicalModelContract.from(
      ContractReference.unsafeModel("global-without-independent-oracle"),
      MathematicalModelFamily.ExactSpectralFrame,
      MultivarEstimand.GeneralizedSpectralSubspace,
      binding("global-without-independent-oracle"),
      FrameSymmetry.Orthogonal,
      ContractMaturity.Planned,
      Set(OptimizationClaimClass.ExactGlobal),
      Vector(theorem("exact-spectrum", Set(OptimizationClaimClass.ExactGlobal))),
      Set(OracleFamily.Metamorphic, OracleFamily.Adversarial),
      Vector.empty
    )

    assert(noTheorem.left.exists(_.isInstanceOf[MathematicalContractError.MissingTheoremSupport]))
    assert(noIndependentOracle.left.exists(_ == MathematicalContractError.MissingIndependentGlobalOracle))

  test("theorem claims must be a subset of the model contract"):
    val result = MathematicalModelContract.from(
      ContractReference.unsafeModel("theorem-exceeds-contract"),
      MathematicalModelFamily.AnchorRegularizedFrame,
      MultivarEstimand.AnchorCoefficientRefinement,
      binding("theorem-exceeds-contract"),
      FrameSymmetry.SignedPermutation,
      ContractMaturity.Planned,
      Set(OptimizationClaimClass.EpsilonGlobal),
      Vector(
        theorem(
          "overbroad-theorem",
          Set(OptimizationClaimClass.EpsilonGlobal, OptimizationClaimClass.UniqueMinimizerWithinBound)
        )
      ),
      Set(OracleFamily.Differential),
      Vector.empty
    )

    assert(result.left.exists(_.isInstanceOf[MathematicalContractError.TheoremExceedsContract]))

  test("catalog is complete, conservative, and identity-distinct"):
    val contracts = MathematicalContractCatalog.all
    assertEquals(contracts.size, MathematicalModelFamily.values.size)
    assertEquals(contracts.map(_.family).toSet, MathematicalModelFamily.values.toSet)
    assertEquals(contracts.map(_.id.value).distinct.size, contracts.size)
    assertEquals(contracts.map(_.binding.formula.value).distinct.size, contracts.size)
    assertEquals(contracts.map(_.binding.api.value).distinct.size, contracts.size)
    assertEquals(contracts.map(_.binding.ir.value).distinct.size, contracts.size)
    assert(contracts.forall(_.maturity != ContractMaturity.ReleaseVerified))
    assert(contracts.forall(_.oracles.nonEmpty))
    assert(contracts.forall(contract => contract.admissibleClaims.subsetOf(contract.family.admissibleClaims)))

  test("only convex or exact spectral families admit global claims"):
    val globalFamilies = MathematicalModelFamily.values.filter(_.admissibleClaims.exists(_.isGlobal)).toSet
    assertEquals(
      globalFamilies,
      Set(
        MathematicalModelFamily.AnchorRegularizedFrame,
        MathematicalModelFamily.ExactSpectralFrame,
        MathematicalModelFamily.ConvexifiedLowRankMatrix
      )
    )

  test("every resolved claim names the evidence needed to attain it"):
    OptimizationClaimClass.values.foreach: claim =>
      if claim == OptimizationClaimClass.Unresolved then assertEquals(claim.requiredEvidence, Set.empty)
      else assert(claim.requiredEvidence.nonEmpty)

  test("program requests cover every resolvable claim and cannot request unresolved"):
    assertEquals(
      RequestedOptimizationClaim.values.map(_.claimClass).toSet,
      OptimizationClaimClass.values.toSet - OptimizationClaimClass.Unresolved
    )

  test("unsupported cases carry durable ids and explanations"):
    val cases = MathematicalContractCatalog.all.flatMap(_.unsupportedCases)
    assert(cases.nonEmpty)
    assertEquals(cases.map(_.id.value).distinct.size, cases.size)
    assert(cases.forall(_.explanation.nonEmpty))
    assert(
      UnsupportedModelCase
        .from(ContractReference.unsafeUnsupportedCase("empty-explanation"), "  ")
        .left
        .exists(_.isInstanceOf[MathematicalContractError.EmptyUnsupportedExplanation])
    )

  private def binding(id: String): FormulaBinding =
    FormulaBinding(
      ContractReference.unsafeFormula(s"$id-formula"),
      ContractReference.unsafeApi(s"$id-api"),
      ContractReference.unsafeIr(s"$id-ir")
    )

  private def theorem(id: String, claims: Set[OptimizationClaimClass]): TheoremContract =
    accepted(
      TheoremContract.from(
        ContractReference.unsafeTheorem(id),
        Vector(ContractReference.unsafeAssumption(s"$id-assumption")),
        claims
      )
    )

  private def accepted[A](value: Either[MathematicalContractError, A]): A =
    value.fold(error => fail(error.message), identity)
