package scalafim.multivar

sealed trait ContractReferenceRole
sealed trait ModelContractReference extends ContractReferenceRole
sealed trait FormulaReference extends ContractReferenceRole
sealed trait ApiReference extends ContractReferenceRole
sealed trait IrReference extends ContractReferenceRole
sealed trait TheoremReference extends ContractReferenceRole
sealed trait AssumptionReference extends ContractReferenceRole
sealed trait UnsupportedCaseReference extends ContractReferenceRole

/** Nominal identifier for one role in the mathematical contract.
  *
  * The phantom `Role` prevents formula, API, IR, theorem, and assumption identifiers
  * from being interchanged accidentally while preserving a language-neutral string form.
  */
opaque type ContractReference[Role <: ContractReferenceRole] = String

object ContractReference:
  def model(value: String): Either[MathematicalContractError, ContractReference[ModelContractReference]] =
    from("model contract id", value)

  def formula(value: String): Either[MathematicalContractError, ContractReference[FormulaReference]] =
    from("formula id", value)

  def api(value: String): Either[MathematicalContractError, ContractReference[ApiReference]] =
    from("API id", value)

  def ir(value: String): Either[MathematicalContractError, ContractReference[IrReference]] =
    from("IR id", value)

  def theorem(value: String): Either[MathematicalContractError, ContractReference[TheoremReference]] =
    from("theorem id", value)

  def assumption(value: String): Either[MathematicalContractError, ContractReference[AssumptionReference]] =
    from("assumption id", value)

  def unsupportedCase(
      value: String
  ): Either[MathematicalContractError, ContractReference[UnsupportedCaseReference]] =
    from("unsupported-case id", value)

  private[multivar] def unsafeModel(value: String): ContractReference[ModelContractReference] =
    unsafe(model(value))

  private[multivar] def unsafeFormula(value: String): ContractReference[FormulaReference] =
    unsafe(formula(value))

  private[multivar] def unsafeApi(value: String): ContractReference[ApiReference] =
    unsafe(api(value))

  private[multivar] def unsafeIr(value: String): ContractReference[IrReference] =
    unsafe(ir(value))

  private[multivar] def unsafeTheorem(value: String): ContractReference[TheoremReference] =
    unsafe(theorem(value))

  private[multivar] def unsafeAssumption(value: String): ContractReference[AssumptionReference] =
    unsafe(assumption(value))

  private[multivar] def unsafeUnsupportedCase(value: String): ContractReference[UnsupportedCaseReference] =
    unsafe(unsupportedCase(value))

  extension [Role <: ContractReferenceRole](reference: ContractReference[Role])
    inline def value: String = reference

  private def from[Role <: ContractReferenceRole](
      role: String,
      value: String
  ): Either[MathematicalContractError, ContractReference[Role]] =
    Identifier
      .validate(role, value)
      .left
      .map(error => MathematicalContractError.InvalidReference(role, value, error.message))

  private def unsafe[Role <: ContractReferenceRole](
      value: Either[MathematicalContractError, ContractReference[Role]]
  ): ContractReference[Role] =
    value.fold(error => throw new IllegalArgumentException(error.message), identity)

enum EstimationStage:
  case PostFitRefinement
  case JointEstimation
  case ConvexMatrixEstimation

enum OptimizationClaimClass:
  case ExactGlobal
  case EpsilonGlobal
  case UniqueMinimizerWithinBound
  case Stationary
  case CoordinatewiseStationary
  case Feasible
  case Unresolved

  def isGlobal: Boolean =
    this match
      case ExactGlobal | EpsilonGlobal | UniqueMinimizerWithinBound => true
      case Stationary | CoordinatewiseStationary | Feasible | Unresolved => false

  def requiredEvidence: Set[ClaimEvidenceRequirement] =
    this match
      case ExactGlobal => Set(ClaimEvidenceRequirement.GlobalOptimalityWitness)
      case EpsilonGlobal => Set(ClaimEvidenceRequirement.ObjectiveGapBound)
      case UniqueMinimizerWithinBound => Set(ClaimEvidenceRequirement.DistanceToMinimizerBound)
      case Stationary => Set(ClaimEvidenceRequirement.StationarityResidual)
      case CoordinatewiseStationary => Set(ClaimEvidenceRequirement.BlockStationarityResiduals)
      case Feasible => Set(ClaimEvidenceRequirement.FeasibilityResidual)
      case Unresolved => Set.empty

/** Optimization result class requested by a declared program.
  *
  * `Unresolved` is deliberately absent: it is a possible achieved outcome,
  * never a meaningful request. The explicit conversion keeps prospective
  * intent distinct from retrospective solver evidence.
  */
enum RequestedOptimizationClaim(val claimClass: OptimizationClaimClass):
  case ExactGlobal extends RequestedOptimizationClaim(OptimizationClaimClass.ExactGlobal)
  case EpsilonGlobal extends RequestedOptimizationClaim(OptimizationClaimClass.EpsilonGlobal)
  case UniqueMinimizerWithinBound
      extends RequestedOptimizationClaim(OptimizationClaimClass.UniqueMinimizerWithinBound)
  case Stationary extends RequestedOptimizationClaim(OptimizationClaimClass.Stationary)
  case CoordinatewiseStationary
      extends RequestedOptimizationClaim(OptimizationClaimClass.CoordinatewiseStationary)
  case Feasible extends RequestedOptimizationClaim(OptimizationClaimClass.Feasible)

enum ClaimEvidenceRequirement:
  case GlobalOptimalityWitness
  case ObjectiveGapBound
  case DistanceToMinimizerBound
  case StationarityResidual
  case BlockStationarityResiduals
  case FeasibilityResidual

enum MathematicalModelFamily:
  case AnchorRegularizedFrame
  case ExactSpectralFrame
  case JointSparseFunctionalFactorization
  case GeneralizedLowRankModel
  case ConvexifiedLowRankMatrix
  case StructuredMultiblockFactorization

  def estimationStage: EstimationStage =
    this match
      case AnchorRegularizedFrame => EstimationStage.PostFitRefinement
      case ConvexifiedLowRankMatrix => EstimationStage.ConvexMatrixEstimation
      case ExactSpectralFrame | JointSparseFunctionalFactorization | GeneralizedLowRankModel |
          StructuredMultiblockFactorization => EstimationStage.JointEstimation

  def admissibleClaims: Set[OptimizationClaimClass] =
    this match
      case AnchorRegularizedFrame =>
        Set(
          OptimizationClaimClass.ExactGlobal,
          OptimizationClaimClass.EpsilonGlobal,
          OptimizationClaimClass.UniqueMinimizerWithinBound,
          OptimizationClaimClass.Feasible,
          OptimizationClaimClass.Unresolved
        )
      case ExactSpectralFrame =>
        Set(
          OptimizationClaimClass.ExactGlobal,
          OptimizationClaimClass.EpsilonGlobal,
          OptimizationClaimClass.Unresolved
        )
      case ConvexifiedLowRankMatrix =>
        Set(
          OptimizationClaimClass.ExactGlobal,
          OptimizationClaimClass.EpsilonGlobal,
          OptimizationClaimClass.UniqueMinimizerWithinBound,
          OptimizationClaimClass.Feasible,
          OptimizationClaimClass.Unresolved
        )
      case JointSparseFunctionalFactorization | GeneralizedLowRankModel |
          StructuredMultiblockFactorization =>
        Set(
          OptimizationClaimClass.Stationary,
          OptimizationClaimClass.CoordinatewiseStationary,
          OptimizationClaimClass.Feasible,
          OptimizationClaimClass.Unresolved
        )

enum MultivarEstimand:
  case AnchorCoefficientRefinement
  case GeneralizedSpectralSubspace
  case JointStructuredFactors
  case GeneralizedLatentRepresentation
  case ConvexLowRankMatrix
  case SharedBlockLatentRepresentation

  def family: MathematicalModelFamily =
    this match
      case AnchorCoefficientRefinement => MathematicalModelFamily.AnchorRegularizedFrame
      case GeneralizedSpectralSubspace => MathematicalModelFamily.ExactSpectralFrame
      case JointStructuredFactors => MathematicalModelFamily.JointSparseFunctionalFactorization
      case GeneralizedLatentRepresentation => MathematicalModelFamily.GeneralizedLowRankModel
      case ConvexLowRankMatrix => MathematicalModelFamily.ConvexifiedLowRankMatrix
      case SharedBlockLatentRepresentation => MathematicalModelFamily.StructuredMultiblockFactorization

enum ContractMaturity:
  case Planned
  case Partial
  case Executable
  case ReleaseVerified

enum OracleFamily:
  case Analytic
  case Differential
  case Metamorphic
  case Property
  case Adversarial
  case Regression
  case Performance

final case class FormulaBinding(
    formula: ContractReference[FormulaReference],
    api: ContractReference[ApiReference],
    ir: ContractReference[IrReference]
)

final class TheoremContract private (
    val id: ContractReference[TheoremReference],
    val assumptions: Vector[ContractReference[AssumptionReference]],
    val supportedClaims: Set[OptimizationClaimClass]
)

object TheoremContract:
  def from(
      id: ContractReference[TheoremReference],
      assumptions: Vector[ContractReference[AssumptionReference]],
      supportedClaims: Set[OptimizationClaimClass]
  ): Either[MathematicalContractError, TheoremContract] =
    if assumptions.isEmpty then Left(MathematicalContractError.EmptyTheoremAssumptions(id))
    else if supportedClaims.isEmpty then Left(MathematicalContractError.EmptyTheoremClaims(id))
    else Right(new TheoremContract(id, assumptions.distinct, supportedClaims))

  private[multivar] def unsafe(
      id: ContractReference[TheoremReference],
      assumptions: Vector[ContractReference[AssumptionReference]],
      supportedClaims: Set[OptimizationClaimClass]
  ): TheoremContract =
    from(id, assumptions, supportedClaims).fold(
      error => throw new IllegalArgumentException(error.message),
      identity
    )

final class UnsupportedModelCase private (
    val id: ContractReference[UnsupportedCaseReference],
    val explanation: String
)

object UnsupportedModelCase:
  def from(
      id: ContractReference[UnsupportedCaseReference],
      explanation: String
  ): Either[MathematicalContractError, UnsupportedModelCase] =
    val clean = explanation.trim
    if clean.isEmpty then Left(MathematicalContractError.EmptyUnsupportedExplanation(id))
    else Right(new UnsupportedModelCase(id, clean))

  private[multivar] def unsafe(
      id: ContractReference[UnsupportedCaseReference],
      explanation: String
  ): UnsupportedModelCase =
    from(id, explanation).fold(error => throw new IllegalArgumentException(error.message), identity)

final class MathematicalModelContract private (
    val id: ContractReference[ModelContractReference],
    val family: MathematicalModelFamily,
    val estimand: MultivarEstimand,
    val binding: FormulaBinding,
    val symmetry: FrameSymmetry,
    val maturity: ContractMaturity,
    val admissibleClaims: Set[OptimizationClaimClass],
    val theorems: Vector[TheoremContract],
    val oracles: Set[OracleFamily],
    val unsupportedCases: Vector[UnsupportedModelCase]
):
  def estimationStage: EstimationStage =
    family.estimationStage

object MathematicalModelContract:
  def from(
      id: ContractReference[ModelContractReference],
      family: MathematicalModelFamily,
      estimand: MultivarEstimand,
      binding: FormulaBinding,
      symmetry: FrameSymmetry,
      maturity: ContractMaturity,
      admissibleClaims: Set[OptimizationClaimClass],
      theorems: Vector[TheoremContract],
      oracles: Set[OracleFamily],
      unsupportedCases: Vector[UnsupportedModelCase]
  ): Either[MathematicalContractError, MathematicalModelContract] =
    if estimand.family != family then
      Left(MathematicalContractError.EstimandFamilyMismatch(family, estimand))
    else if admissibleClaims.isEmpty then
      Left(MathematicalContractError.EmptyAdmissibleClaims(family))
    else
      admissibleClaims.find(claim => !family.admissibleClaims.contains(claim)) match
        case Some(claim) => Left(MathematicalContractError.InadmissibleClaim(family, claim))
        case None =>
          validateTheorems(admissibleClaims, theorems).flatMap: _ =>
            validateOracles(admissibleClaims, oracles).flatMap: _ =>
              validateDistinctTheorems(theorems).flatMap: _ =>
                validateDistinctUnsupportedCases(unsupportedCases).map: _ =>
                  new MathematicalModelContract(
                    id,
                    family,
                    estimand,
                    binding,
                    symmetry,
                    maturity,
                    admissibleClaims,
                    theorems,
                    oracles,
                    unsupportedCases
                  )

  private[multivar] def unsafe(
      id: ContractReference[ModelContractReference],
      family: MathematicalModelFamily,
      estimand: MultivarEstimand,
      binding: FormulaBinding,
      symmetry: FrameSymmetry,
      maturity: ContractMaturity,
      admissibleClaims: Set[OptimizationClaimClass],
      theorems: Vector[TheoremContract],
      oracles: Set[OracleFamily],
      unsupportedCases: Vector[UnsupportedModelCase]
  ): MathematicalModelContract =
    from(
      id,
      family,
      estimand,
      binding,
      symmetry,
      maturity,
      admissibleClaims,
      theorems,
      oracles,
      unsupportedCases
    ).fold(error => throw new IllegalArgumentException(error.message), identity)

  private def validateTheorems(
      claims: Set[OptimizationClaimClass],
      theorems: Vector[TheoremContract]
  ): Either[MathematicalContractError, Unit] =
    theorems.find(theorem => !theorem.supportedClaims.subsetOf(claims)) match
      case Some(theorem) =>
        val unsupported = theorem.supportedClaims.diff(claims).head
        Left(MathematicalContractError.TheoremExceedsContract(theorem.id, unsupported))
      case None =>
        claims.find(claim => claim.isGlobal && !theorems.exists(_.supportedClaims.contains(claim))) match
          case Some(claim) => Left(MathematicalContractError.MissingTheoremSupport(claim))
          case None => Right(())

  private def validateOracles(
      claims: Set[OptimizationClaimClass],
      oracles: Set[OracleFamily]
  ): Either[MathematicalContractError, Unit] =
    if oracles.isEmpty then Left(MathematicalContractError.EmptyOracleSet)
    else if claims.exists(_.isGlobal) &&
        !oracles.contains(OracleFamily.Analytic) &&
        !oracles.contains(OracleFamily.Differential)
    then Left(MathematicalContractError.MissingIndependentGlobalOracle)
    else Right(())

  private def validateDistinctTheorems(
      theorems: Vector[TheoremContract]
  ): Either[MathematicalContractError, Unit] =
    duplicate(theorems.map(_.id.value)) match
      case Some(id) => Left(MathematicalContractError.DuplicateTheorem(id))
      case None => Right(())

  private def validateDistinctUnsupportedCases(
      cases: Vector[UnsupportedModelCase]
  ): Either[MathematicalContractError, Unit] =
    duplicate(cases.map(_.id.value)) match
      case Some(id) => Left(MathematicalContractError.DuplicateUnsupportedCase(id))
      case None => Right(())

  private def duplicate(values: Vector[String]): Option[String] =
    val seen = scala.collection.mutable.HashSet.empty[String]
    values.find(value => !seen.add(value))

enum MathematicalContractError:
  case InvalidReference(role: String, value: String, reason: String)
  case EstimandFamilyMismatch(family: MathematicalModelFamily, estimand: MultivarEstimand)
  case EmptyAdmissibleClaims(family: MathematicalModelFamily)
  case InadmissibleClaim(family: MathematicalModelFamily, claim: OptimizationClaimClass)
  case EmptyTheoremAssumptions(theorem: ContractReference[TheoremReference])
  case EmptyTheoremClaims(theorem: ContractReference[TheoremReference])
  case TheoremExceedsContract(
      theorem: ContractReference[TheoremReference],
      claim: OptimizationClaimClass
  )
  case MissingTheoremSupport(claim: OptimizationClaimClass)
  case EmptyOracleSet
  case MissingIndependentGlobalOracle
  case DuplicateTheorem(id: String)
  case EmptyUnsupportedExplanation(id: ContractReference[UnsupportedCaseReference])
  case DuplicateUnsupportedCase(id: String)

  def message: String =
    this match
      case InvalidReference(role, value, reason) => s"invalid $role '$value': $reason"
      case EstimandFamilyMismatch(family, estimand) =>
        s"estimand $estimand belongs to ${estimand.family}, not $family"
      case EmptyAdmissibleClaims(family) => s"$family must declare at least one admissible claim"
      case InadmissibleClaim(family, claim) => s"$family cannot admit the $claim claim"
      case EmptyTheoremAssumptions(theorem) => s"theorem ${theorem.value} must declare its assumptions"
      case EmptyTheoremClaims(theorem) => s"theorem ${theorem.value} must declare the claims it supports"
      case TheoremExceedsContract(theorem, claim) =>
        s"theorem ${theorem.value} supports $claim, which the model contract does not admit"
      case MissingTheoremSupport(claim) => s"global claim $claim has no supporting theorem"
      case EmptyOracleSet => "a mathematical model contract must declare at least one oracle family"
      case MissingIndependentGlobalOracle =>
        "a global claim requires an analytic or independent differential oracle"
      case DuplicateTheorem(id) => s"theorem $id is declared more than once"
      case EmptyUnsupportedExplanation(id) =>
        s"unsupported case ${id.value} must explain why it is unsupported"
      case DuplicateUnsupportedCase(id) => s"unsupported case $id is declared more than once"

/** Conservative catalog of the mathematical families discussed by the multivar API.
  *
  * Maturity is deliberately not inferred from source presence. Only the release gate may
  * promote a contract to [[ContractMaturity.ReleaseVerified]].
  */
object MathematicalContractCatalog:
  val anchorRegularizedFrame: MathematicalModelContract =
    contract(
      id = "multivar.anchor-regularized-frame.v1",
      family = MathematicalModelFamily.AnchorRegularizedFrame,
      estimand = MultivarEstimand.AnchorCoefficientRefinement,
      formula = "anchor-refinement",
      api = "ConvexFunctionalFrameProblem",
      ir = "operator-program.anchor-refinement.v0.2",
      symmetry = FrameSymmetry.SignedPermutation,
      maturity = ContractMaturity.Partial,
      claims = Set(
        OptimizationClaimClass.EpsilonGlobal,
        OptimizationClaimClass.UniqueMinimizerWithinBound,
        OptimizationClaimClass.Feasible,
        OptimizationClaimClass.Unresolved
      ),
      theorems = Vector(
        theorem(
          "strongly-convex-anchor-composite",
          Vector("proper-closed-convex-penalty", "strongly-convex-anchor", "certified-prox-or-splitting"),
          Set(OptimizationClaimClass.EpsilonGlobal, OptimizationClaimClass.UniqueMinimizerWithinBound)
        )
      ),
      oracles = Set(OracleFamily.Analytic, OracleFamily.Differential, OracleFamily.Metamorphic),
      unsupported = Vector(
        unsupported(
          "not-joint-sparse-pca",
          "Refining an already fitted frame is not the estimand of jointly fitted sparse PCA."
        )
      )
    )

  val exactSpectralFrame: MathematicalModelContract =
    contract(
      id = "multivar.exact-spectral-frame.v1",
      family = MathematicalModelFamily.ExactSpectralFrame,
      estimand = MultivarEstimand.GeneralizedSpectralSubspace,
      formula = "generalized-spectral-frame",
      api = "ExactSpectralPrograms",
      ir = "operator-program.exact-spectral.v0.2",
      symmetry = FrameSymmetry.Orthogonal,
      maturity = ContractMaturity.Partial,
      claims = Set(
        OptimizationClaimClass.ExactGlobal,
        OptimizationClaimClass.EpsilonGlobal,
        OptimizationClaimClass.Unresolved
      ),
      theorems = Vector(
        theorem(
          "symmetric-generalized-spectrum",
          Vector("symmetric-value-operator", "spd-normalization", "certified-spectrum"),
          Set(OptimizationClaimClass.ExactGlobal, OptimizationClaimClass.EpsilonGlobal)
        ),
        theorem(
          "generalized-cross-spectrum",
          Vector(
            "empty-factor-penalties",
            "spd-left-normalization",
            "spd-right-normalization",
            "certified-generalized-cross-svd"
          ),
          Set(OptimizationClaimClass.ExactGlobal, OptimizationClaimClass.EpsilonGlobal)
        )
      ),
      oracles = Set(OracleFamily.Analytic, OracleFamily.Differential, OracleFamily.Adversarial),
      unsupported = Vector(
        unsupported(
          "nonsmooth-penalty-not-spectral",
          "A nonsmooth coordinate penalty generally destroys the exact spectral reduction."
        )
      )
    )

  val jointSparseFunctionalFactorization: MathematicalModelContract =
    contract(
      id = "multivar.joint-sparse-functional-factorization.v1",
      family = MathematicalModelFamily.JointSparseFunctionalFactorization,
      estimand = MultivarEstimand.JointStructuredFactors,
      formula = "joint-sparse-functional-factorization",
      api = "SparseFunctionalFactorization",
      ir = "operator-program.joint-structured-factorization.v1",
      symmetry = FrameSymmetry.SignedPermutation,
      maturity = ContractMaturity.Planned,
      claims = Set(
        OptimizationClaimClass.Stationary,
        OptimizationClaimClass.CoordinatewiseStationary,
        OptimizationClaimClass.Feasible,
        OptimizationClaimClass.Unresolved
      ),
      theorems = Vector(
        theorem(
          "palm-structured-factorization",
          Vector("bounded-level-set", "block-lipschitz-gradient", "exact-or-controlled-prox", "kl-objective"),
          Set(OptimizationClaimClass.Stationary)
        ),
        theorem(
          "block-coordinate-structured-factorization",
          Vector("block-convexity", "bounded-iterates", "exact-or-controlled-block-solves"),
          Set(OptimizationClaimClass.CoordinatewiseStationary)
        )
      ),
      oracles = Set(OracleFamily.Differential, OracleFamily.Metamorphic, OracleFamily.Adversarial),
      unsupported = Vector(
        unsupported(
          "generic-global-factor-optimum",
          "The jointly factored sparse-functional objective is nonconvex and has no generic global guarantee."
        )
      )
    )

  val generalizedLowRankModel: MathematicalModelContract =
    contract(
      id = "multivar.generalized-low-rank-model.v1",
      family = MathematicalModelFamily.GeneralizedLowRankModel,
      estimand = MultivarEstimand.GeneralizedLatentRepresentation,
      formula = "masked-generalized-low-rank-model",
      api = "GeneralizedLowRankProgram",
      ir = "operator-program.glrm.v1",
      symmetry = FrameSymmetry.Identity,
      maturity = ContractMaturity.Executable,
      claims = Set(
        OptimizationClaimClass.Stationary,
        OptimizationClaimClass.CoordinatewiseStationary,
        OptimizationClaimClass.Feasible,
        OptimizationClaimClass.Unresolved
      ),
      theorems = Vector(
        theorem(
          "palm-generalized-low-rank-model",
          Vector("convex-entry-loss-by-block", "block-lipschitz-gradient", "proper-factor-penalties", "bounded-iterates"),
          Set(OptimizationClaimClass.Stationary)
        )
      ),
      oracles = Set(OracleFamily.Analytic, OracleFamily.Differential, OracleFamily.Property),
      unsupported = Vector(
        unsupported(
          "mask-is-not-missingness-proof",
          "An observation mask alone does not justify MAR or MNAR inference."
        ),
        unsupported(
          "nonlinear-encoding-is-not-projection",
          "A latent-code solve under a nonquadratic loss is not a fitted linear projection."
        )
      )
    )

  val convexifiedLowRankMatrix: MathematicalModelContract =
    contract(
      id = "multivar.convexified-low-rank-matrix.v1",
      family = MathematicalModelFamily.ConvexifiedLowRankMatrix,
      estimand = MultivarEstimand.ConvexLowRankMatrix,
      formula = "convex-loss-plus-nuclear-norm",
      api = "ConvexLowRankMatrixProgram",
      ir = "operator-program.convex-low-rank-matrix.v1",
      symmetry = FrameSymmetry.Orthogonal,
      maturity = ContractMaturity.Planned,
      claims = Set(
        OptimizationClaimClass.ExactGlobal,
        OptimizationClaimClass.EpsilonGlobal,
        OptimizationClaimClass.Feasible,
        OptimizationClaimClass.Unresolved
      ),
      theorems = Vector(
        theorem(
          "nuclear-norm-subgradient-certificate",
          Vector("convex-matrix-loss", "sufficient-factor-rank", "certified-nuclear-subgradient"),
          Set(OptimizationClaimClass.ExactGlobal, OptimizationClaimClass.EpsilonGlobal)
        )
      ),
      oracles = Set(OracleFamily.Analytic, OracleFamily.Differential, OracleFamily.Regression),
      unsupported = Vector(
        unsupported(
          "certificate-does-not-generalize-to-all-glrms",
          "The nuclear-norm certificate applies only to the stated convex matrix formulation."
        )
      )
    )

  val structuredMultiblockFactorization: MathematicalModelContract =
    contract(
      id = "multivar.structured-multiblock-factorization.v1",
      family = MathematicalModelFamily.StructuredMultiblockFactorization,
      estimand = MultivarEstimand.SharedBlockLatentRepresentation,
      formula = "aligned-structured-multiblock-factorization",
      api = "StructuredMultiblockProgram",
      ir = "operator-program.structured-multiblock.v1",
      symmetry = FrameSymmetry.SignedPermutation,
      maturity = ContractMaturity.Partial,
      claims = Set(
        OptimizationClaimClass.Stationary,
        OptimizationClaimClass.CoordinatewiseStationary,
        OptimizationClaimClass.Feasible,
        OptimizationClaimClass.Unresolved
      ),
      theorems = Vector(
        theorem(
          "block-coordinate-multiblock-factorization",
          Vector("explicit-row-alignment", "block-convexity", "bounded-iterates", "certified-block-solves"),
          Set(OptimizationClaimClass.CoordinatewiseStationary)
        )
      ),
      oracles = Set(OracleFamily.Differential, OracleFamily.Metamorphic, OracleFamily.Adversarial),
      unsupported = Vector(
        unsupported(
          "unaligned-blocks-require-hub-semantics",
          "Shared-score fitting requires explicit row alignment; unaligned studies use hub or direct-sum semantics."
        )
      )
    )

  val all: Vector[MathematicalModelContract] =
    Vector(
      anchorRegularizedFrame,
      exactSpectralFrame,
      jointSparseFunctionalFactorization,
      generalizedLowRankModel,
      convexifiedLowRankMatrix,
      structuredMultiblockFactorization
    )

  private def contract(
      id: String,
      family: MathematicalModelFamily,
      estimand: MultivarEstimand,
      formula: String,
      api: String,
      ir: String,
      symmetry: FrameSymmetry,
      maturity: ContractMaturity,
      claims: Set[OptimizationClaimClass],
      theorems: Vector[TheoremContract],
      oracles: Set[OracleFamily],
      unsupported: Vector[UnsupportedModelCase]
  ): MathematicalModelContract =
    MathematicalModelContract.unsafe(
      ContractReference.unsafeModel(id),
      family,
      estimand,
      FormulaBinding(
        ContractReference.unsafeFormula(formula),
        ContractReference.unsafeApi(api),
        ContractReference.unsafeIr(ir)
      ),
      symmetry,
      maturity,
      claims,
      theorems,
      oracles,
      unsupported
    )

  private def theorem(
      id: String,
      assumptions: Vector[String],
      claims: Set[OptimizationClaimClass]
  ): TheoremContract =
    TheoremContract.unsafe(
      ContractReference.unsafeTheorem(id),
      assumptions.map(ContractReference.unsafeAssumption),
      claims
    )

  private def unsupported(id: String, explanation: String): UnsupportedModelCase =
    UnsupportedModelCase.unsafe(ContractReference.unsafeUnsupportedCase(id), explanation)
